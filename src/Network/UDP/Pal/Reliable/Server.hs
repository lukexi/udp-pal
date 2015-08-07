{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}

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

-- | Creates a server than listens for incoming messages from clients
-- and broadcasts them to all listening clients. Returns a channel that
-- can broadcast to all listening clients.

createServer :: forall r m. (Binary r, MonadIO m) 
             => HostName 
             -> PortNumber 
             -> PacketSize 
             -> IO (m [(SockAddr, AppPacket r)], -- Get packets from clients
                    AppPacket r -> m (),         -- Send a packet to all clients
                    TChan SockAddr)              -- Hear about disconnections
createServer serverName serverPort packetSize = do
  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  let finallyClose = flip finally (close (bsSocket incomingSocket))

  broadcastChan <- newBroadcastTChanIO
  disconnectionsChan <- newTChanIO

  (reliableStateChan, reliableStateAccum, reliableStateSeqNum) <- atomically $ 
    (,,) <$> dupTChan broadcastChan
         <*> newTVar mempty
         <*> newTVar 0
  -- Create a process to collect all reliable messages, 
  -- so we can catch new clients up with everything that's happend.
  _ <- forkIO' . forever . atomically $ do
    (_fromAddr, newItem) <- readTChan reliableStateChan
    case newItem of
      Reliable relItem -> do
        seqNum        <- readTVar reliableStateSeqNum
        reliableState <- readTVar reliableStateAccum
        writeTVar reliableStateSeqNum (succ seqNum)
        writeTVar reliableStateAccum (Map.insert seqNum relItem reliableState)
      _ -> return ()

  clients <- newMVar mempty
  let findClient fromAddr =
        modifyMVar clients $ \currentClients ->
          case Map.lookup fromAddr currentClients of
            Just client -> return (currentClients, client)
            Nothing     -> do
              client <- newServerToClientThread fromAddr
              return (Map.insert fromAddr client currentClients, client)
      newServerToClientThread fromAddr = do
        toClientSock                <- connectedSocketToAddr fromAddr

        -- Duplicate the broadcast chan so the client can receive broadcasts,
        -- and create a copy of the reliable state sent thus far to catch the client up with.
        (broadcastsToClient, reliableState) <- atomically $ do
          (,) <$> dupTChan broadcastChan
              <*> readTVar reliableStateAccum
        
        transceiver@Transceiver{..} <- createTransceiver "Server" (Right toClientSock) reliableState

        -- Send verified packets to all clients, tagged with this clients address
        incomingThread <- streamInto broadcastChan ((fromAddr,) <$> atomically (readTChan tcVerifiedPackets))
        -- Send all broadcasts to this client, except messages we sent ourselves
        outgoingThread <- streamIntoMaybe tcOutgoingPackets 
          ((\(bcastFrom, msg) -> 
            if bcastFrom == fromAddr then Nothing else Just msg) <$> atomically (readTChan broadcastsToClient))

        -- When the transceiver detects a gap of over a second in messages, kill its threads
        -- and remove the client from the list of clients.
        addWatchdog transceiver $ do
          mapM_ killThread [incomingThread, outgoingThread]
          close (unConnectedSocket toClientSock)
          modifyMVar_ clients (return . Map.delete fromAddr)
          atomically $ writeTChan disconnectionsChan fromAddr
        
        return transceiver

  -- Launch the server router thread to route packets to their client's receivers
  _ <- forkIO' . finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket

    -- Find the client receiver who should process this message
    transceiver <- findClient fromAddr

    -- Pass the decoded packet into the client's Transceiver's incomingRawPackets channel
    let packet = decode' newMessage :: WirePacket r
    atomically $ writeTChan (tcIncomingRawPackets transceiver) packet

  ourAddr <- addrAddress <$> addressInfo (Just serverName) (Just (show serverPort))
  packetsFromClients <- atomically $ dupTChan broadcastChan
  let broadcastToClients    = liftIO . atomically . writeTChan broadcastChan . (ourAddr,)
      getPacketsFromClients = filter ((/= ourAddr) . fst) <$> liftIO (atomically (exhaustChan packetsFromClients))

  return (getPacketsFromClients, broadcastToClients, disconnectionsChan)

