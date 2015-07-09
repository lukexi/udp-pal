{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

import           Linear
import Control.Concurrent.STM
import qualified Data.Map               as Map
import           Halive.Concurrent
import           Control.Concurrent

import           Network.UDP.Pal
import Control.Monad.State
import           Control.Exception

import Control.Lens

import Types
import Data.Binary
import Data.Time

{- 
TODO:

[x] Have the Transceiver hold a TVar of the last time it received a message
    Users of the Transceiver can check this against the current time to decide if
    they want to mark it as dead.

The server will check each client (maybe in findClient, even?) periodically and
kill the Transceiver if it is too old.
The server can then use its own broadcast channel to broadcast a reliable message saying that the
client is dead, which the app threads can pickup and use to clean up the client.

-}

createServer :: forall u r. (Binary u, Binary r) => IO (TChan (AppPacket u r))
createServer = do
  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  let finallyClose = flip finally (close (bsSocket incomingSocket))

  broadcastChan <- newBroadcastTChanIO

  clients <- newMVar mempty
  let findClient fromAddr =
        modifyMVar clients $ \currentClients ->
          case Map.lookup fromAddr currentClients of
            Just client -> return (currentClients, client)
            Nothing     -> do
              client <- newServerToClientThread fromAddr
              return (Map.insert fromAddr client currentClients, client)
      newServerToClientThread fromAddr = do
        toClientSock <- connectedSocketToAddr fromAddr

        broadcastsToClient <- atomically $ dupTChan broadcastChan
        transceiver        <- createTransceiver "Server" (Right toClientSock)
        -- Send all verified packets to all clients
        -- TODO filter out each client's own messages
        -- TODO kill these with the client
        threadID1 <- streamInto broadcastChan                   (atomically . readTChan $ (tcVerifiedPackets transceiver))
        -- Send all broadcasts to this client
        threadID2 <- streamInto (tcOutgoingPackets transceiver) (atomically . readTChan $ broadcastsToClient)
        -- Add these threads to the transceiver's thread list so they can be killed along with it
        return $ transceiver { tcThreads = tcThreads transceiver ++ [threadID1, threadID2] }
  
  -- Launch the client monitor thread to kill clients when they haven't sent a message lately
  _ <- forkIO' . void . forever $ do
    now <- getCurrentTime
    expired <- modifyMVar clients $ \currentClients -> do
      messageTimes <- atomically $ traverse 
        (fmap (diffUTCTime now) . readTVar . tcLastMessageTime)
        currentClients 
      let expiredKeys = Map.filter (> 1) messageTimes
          expired   = Map.intersection currentClients expiredKeys
          remaining = Map.difference   currentClients expiredKeys
      return (remaining, expired)
    forM_ (Map.toList expired) $ \(key, client) -> do
      putStrLn ("GOODBYE " ++ show key)
      killTransceiver client

    -- Check for dead clients once per second
    threadDelay 1000000

  -- Launch the server router thread to route packets to their client's receivers
  _ <- forkIO' . finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket

    -- Find the client receiver who should process this message
    transceiver <- findClient fromAddr

    -- Pass the decoded packet into the client's Transceiver's incomingRawPackets channel
    let packet = decode' newMessage :: WirePacket u r
    atomically $ writeTChan (tcIncomingRawPackets transceiver) packet

  return broadcastChan

main :: IO ()
main = do

  broadcastChan <- createServer

  -- Launch the server's main thread to run physics simulations
  packetsFromClients <- atomically $ dupTChan broadcastChan
  void . flip runStateT emptyAppState . forever $ do
    -- Process new packets
    packets <- liftIO . atomically $ exhaustChan packetsFromClients
    forM_ packets $ \case
      Reliable   (CreateObject newCubeID pose color) -> do
        cubePoses  . at newCubeID ?= pose
        cubeColors . at newCubeID ?= color
      Reliable _ -> return ()
      Unreliable _poses -> do
        return ()

    -- Run physics
    cubePoses . traversed . posOrientation *= axisAngle (V3 0 1 0) 0.1
    cubes <- use $ cubePoses . to Map.toList
    let poses = map (\(cubeID, pose) -> ObjectPose cubeID pose) cubes
    --liftIO . putStrLn $ "Sending: " ++ show poses
    liftIO . atomically $ writeTChan broadcastChan (Unreliable poses)

    liftIO $ threadDelay (1000000 `div` 60)
