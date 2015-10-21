{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE BangPatterns              #-}
module Network.UDP.Pal.Reliable.Server where

import qualified Data.Map.Strict               as Map
import           Control.Concurrent.STM
import           Halive.Concurrent
import           Control.Concurrent

import           Network.UDP.Pal.Types
import           Network.UDP.Pal.Socket
import           Network.UDP.Pal.Binary
import           Network.UDP.Pal.Reliable.Types
import           Network.UDP.Pal.Reliable.Transceiver
import           Control.Monad.State.Strict
import           Control.Exception

import           Data.Binary
import           Network.Socket
import           Network.Info
import           Data.List

isPrivateNetIP :: IPv4 -> Bool
isPrivateNetIP ip = or $ (`isPrefixOf` show ip) <$> ["10.", "172.", "192."]

findPrivateNetIP :: IO String
findPrivateNetIP = do
  interfaces <- getNetworkInterfaces
  let desired = filter (isPrivateNetIP . ipv4) interfaces
  case desired of
    (x:_) -> return . show . ipv4 $ x
    _ -> do
      putStrLn ("Couldn't find private net ip (returning 127.0.0.1) in: " ++ show interfaces)
      return "127.0.0.1"

data Server r = Server
  { svrSockAddr       :: SockAddr
  , svrReceive        :: IO [(SockAddr, AppPacket r)] -- ^ Get packets from clients
  , svrBroadcast      :: AppPacket r -> IO ()         -- ^ Send a packet to all clients
  , svrGetDisconnects :: IO [SockAddr]                -- ^ Hear about disconnections
  }

-- | Creates a server than listens for incoming messages from clients
-- and broadcasts them to all listening clients. Returns a channel that
-- can broadcast to all listening clients.

createServer :: forall r. (Binary r) 
             => HostName 
             -> PortNumber 
             -> PacketSize 
             -> IO (Server r)
createServer serverName serverPort packetSize = do
  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  let finallyClose = flip finally (close (bsSocket incomingSocket))

  disconnectionsChan  <- newTChanIO
  broadcastChan       <- newBroadcastTChanIO
  
  reliableStateChan   <- atomically $ dupTChan broadcastChan
  reliableStateAccum  <- newTVarIO mempty
  reliableStateSeqNum <- newTVarIO 0

  -- Create a process to collect all reliable messages, 
  -- so we can catch new clients up with everything that's happend.
  _ <- forkIO' . forever . atomically $ do
    (_fromAddr, newItem) <- readTChan reliableStateChan
    case newItem of
      Reliable relItem -> do
        seqNum        <- readTVar reliableStateSeqNum
        reliableState <- readTVar reliableStateAccum
        writeTVar reliableStateSeqNum $! (succ seqNum)
        writeTVar reliableStateAccum  $! (Map.insert seqNum relItem reliableState)
      _ -> return ()

  clients <- newMVar mempty
  let findClient fromAddr =
        modifyMVar clients $ \currentClients ->
          case Map.lookup fromAddr currentClients of
            Just client -> return (currentClients, client)
            Nothing     -> do
              client <- newServerToClientThread fromAddr
              let !newClients = Map.insert fromAddr client currentClients
              return $! (newClients, client)
      newServerToClientThread fromAddr = do
        toClientSock                <- connectedSocketToAddr fromAddr

        -- Duplicate the broadcast chan so the client can receive broadcasts,
        -- and create a copy of the reliable state sent thus far to catch the client up with.
        (broadcastsToClient, reliableState) <- atomically $ do
          (,) <$> dupTChan broadcastChan
              <*> readTVar reliableStateAccum
        
        transceiver@Transceiver{..} <- createTransceiver "Server" (Right toClientSock) reliableState

        -- Send verified packets to all clients, tagged with this clients address
        incomingThread <- forkIO' . forever . atomically $ 
          writeTChan broadcastChan =<< (fromAddr,) <$> readTChan tcVerifiedPackets
        -- debug helper to purge the verified queue. stops the memory leak....
        -- incomingThread <- forkIO' . forever $ (atomically (readTChan tcVerifiedPackets)) 
        
        -- Send all broadcasts to this client, except messages we sent ourselves
        outgoingThread <- forkIO' . forever . atomically $ 
          readTChan broadcastsToClient >>= \case
            (bcastFrom, msg) 
              | bcastFrom == fromAddr -> return ()
              | otherwise             -> writeTChan tcOutgoingPackets msg

        -- When the transceiver detects a gap of over a second in messages, kill its threads
        -- and remove the client from the list of clients.
        addWatchdog transceiver $ do
          mapM_ killThread [incomingThread, outgoingThread]
          close (unConnectedSocket toClientSock)
          modifyMVar_ clients ((return $!) . Map.delete fromAddr)
          atomically $ writeTChan disconnectionsChan fromAddr
        
        return transceiver

  -- Launch the server router thread to route incoming packets to 
  -- their client's receivers, launching them if they don't exist
  _ <- forkIO' . finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket
      `catch` \(e :: SomeException) -> putStr "receiveFromRaw: " >> print e >> return undefined

    -- Find the client receiver who should process this message
    transceiver <- findClient fromAddr

    -- Pass the decoded packet into the client's Transceiver's incomingRawPackets channel
    -- We don't mind that this is lazily evaluated on the other side of the channel, as that 
    -- distributes the decoding load to the client thread rather than this single incoming thread.
    atomically . writeTChan (tcIncomingRawPackets transceiver) $ (decode' newMessage :: WirePacket r)

  -- Get our own address so we can filter messages we ourselves sent
  ourAddr <- addrAddress <$> addressInfo (Just serverName) (Just (show serverPort))

  -- Get another duplicate to return to the Server-user to get messages from the clients.
  packetsFromClients <- atomically $ dupTChan broadcastChan
  let receiveFromClients = filter ((/= ourAddr) . fst) <$> liftIO (atomically (exhaustChan packetsFromClients))
      broadcastToClients = liftIO . atomically . writeTChan broadcastChan . ((ourAddr,) $!)

  return Server
    { svrSockAddr       = ourAddr
    , svrReceive        = receiveFromClients
    , svrBroadcast      = broadcastToClients
    , svrGetDisconnects = atomically (exhaustChan disconnectionsChan)
    }
