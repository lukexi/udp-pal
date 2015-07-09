{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
module Network.UDP.Pal.Reliable.Server where

import qualified Data.Map               as Map
import           Control.Concurrent.STM
import           Halive.Concurrent
import           Control.Concurrent

import           Network.UDP.Pal.Types
import           Network.UDP.Pal.Socket
import           Network.UDP.Pal.Binary
import           Network.UDP.Pal.Reliable.Types
import           Network.UDP.Pal.Reliable.Transceiver
import           Control.Monad.State
import           Control.Exception

import           Data.Binary


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

-- | Creates a server than listens for incoming messages from clients
-- and broadcasts them to all listening clients. Returns a channel that
-- can broadcast to all listening clients.

createServer :: forall u r. (Binary u, Binary r) 
             => HostName 
             -> PortNumber 
             -> PacketSize 
             -> IO (TChan (SockAddr, AppPacket u r), TChan SockAddr)
createServer serverName serverPort packetSize = do
  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  let finallyClose = flip finally (close (bsSocket incomingSocket))

  broadcastChan <- newBroadcastTChanIO
  disconnectionsChan <- newTChanIO

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

        broadcastsToClient          <- atomically $ dupTChan broadcastChan
        transceiver@Transceiver{..} <- createTransceiver "Server" (Right toClientSock)

        -- Send verified packets to all clients
        incomingThread <- streamInto broadcastChan ((fromAddr,) <$> atomically (readTChan tcVerifiedPackets))
        -- Send all broadcasts to this client
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
    let packet = decode' newMessage :: WirePacket u r
    atomically $ writeTChan (tcIncomingRawPackets transceiver) packet

  return (broadcastChan, disconnectionsChan)