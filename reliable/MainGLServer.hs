{-# LANGUAGE LambdaCase                #-}

module MainGLServer where

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

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

packetSize :: Int
packetSize = 4096

launchServer :: IO ThreadId
launchServer = forkIO' $ do
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
        (incomingRawPackets, verifiedPackets, outgoingPackets) <- createReceiver "Server" (Right toClientSock)
        -- Send all verified packets to all clients
        -- TODO filter out each client's own messages
        -- TODO kill these with the client
        _threadID1 <- streamInto broadcastChan (atomically . readTChan $ verifiedPackets)
        -- Send all broadcasts to this client
        _threadID2 <- streamInto outgoingPackets (atomically . readTChan $ broadcastsToClient)
        return incomingRawPackets 
  -- Launch the server router thread to route packets to their client's receivers
  _ <- forkIO' . finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket

    -- Find the client receiver who should process this message
    incomingRawPackets <- findClient fromAddr

    -- Pass the decoded packet into the client's Receiver's incomingRawPackets channel
    let packet = decode' newMessage :: Packet ObjectPose ObjectOp
    atomically $ writeTChan incomingRawPackets packet

  -- Launch the server's main thread to run physics simulations
  packetsFromClients <- atomically $ dupTChan broadcastChan
  void . flip runStateT mempty . forever $ do
    -- Process new packets
    packets <- liftIO . atomically $ exhaustChan packetsFromClients
    forM_ packets $ \case
      Reliable   (CreateObject newCubeID pose) -> do
        id . at newCubeID ?= pose
      Unreliable _poses -> do
        return ()

    -- Run physics
    id . traversed . posOrientation *= axisAngle (V3 0 1 0) 0.1
    cubes <- use $ id . to Map.toList
    let poses = map (\(cubeID, pose) -> ObjectPose cubeID pose) cubes
    --liftIO . putStrLn $ "Sending: " ++ show poses
    liftIO . atomically $ writeTChan broadcastChan (Unreliable poses)

    liftIO $ threadDelay (1000000 `div` 60)
