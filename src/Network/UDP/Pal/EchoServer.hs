{-# LANGUAGE RecordWildCards #-}
module Network.UDP.Pal.EchoServer where
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString        (ByteString)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Halive.Concurrent
import           Network.Socket         hiding (recv, recvFrom, send, sendTo)
import           Network.UDP.Pal.Binary
import           Network.UDP.Pal.Socket
import           Network.UDP.Pal.Types

echoServer :: HostName -> PortNumber -> Int -> IO ()
echoServer serverName serverPort packetSize = void . forkIO' $ do

  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  clients        <- newMVar mempty
  broadcastChan  <- newBroadcastTChanIO

  flip finally (close (bsSocket incomingSocket)) . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket
    putStrLn $ "Received from: " ++ show fromAddr
      ++ ": " ++ (decode' newMessage :: String)

    -- Launch a thread to speak to the client if they're new
    registerClient broadcastChan clients fromAddr

    -- Broadcast the message to all clients
    atomically $ writeTChan broadcastChan newMessage

registerClient :: TChan ByteString -> MVar (Set SockAddr) -> SockAddr -> IO ()
registerClient broadcastChan clients fromAddr =
  modifyMVar_ clients $ \currentClients ->
    if Set.member fromAddr currentClients
    then return currentClients
    else do
      messageChan <- atomically $ dupTChan broadcastChan
      _ <- newClientThread fromAddr messageChan clients
      return $ Set.insert fromAddr currentClients

-- | Creates a new socket to the client's address, and creates a TChan that's
-- continuously listened to on a new thread and passed along to the new socket
newClientThread :: SockAddr -> TChan ByteString -> MVar (Set SockAddr) -> IO ThreadId
newClientThread clientAddr messageChan clients = forkIO' $ do
  -- Create a connected socket to send messages to the client
  toClientSock <- connectedSocketToAddr clientAddr

  -- Create a display name
  (hostName, serviceName) <- getSockAddrAddress clientAddr
  let displayName = "->" ++ hostName ++ ":" ++ serviceName

  -- Have the thread close the socket and remove the client
  -- from the broadcast queue when an exception occurs
  let finisher e = do
        putStrLn $ displayName ++ " removing self due to " ++ show (e::SomeException)
        close (unConnectedSocket toClientSock)
        modifyMVar_ clients $ return . Set.delete clientAddr
        throwIO e

  handle finisher . forever $ do
    message <- atomically $ readTChan messageChan
    putStrLn $ "Sending to " ++ displayName ++ ": " ++ (decode' message :: String)
    _bytesSent <- sendConn toClientSock message
    return ()
