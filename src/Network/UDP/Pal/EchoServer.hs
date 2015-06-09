{-# LANGUAGE RecordWildCards #-}
module Network.UDP.Pal.EchoServer where
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Network.UDP.Pal.Socket
import           Network.UDP.Pal.Server

echoServer :: HostName -> PortNumber -> Int -> IO ()
echoServer serverName serverPort packetSize = void . forkIO $ do

  server        <- makeServer serverName serverPort packetSize
  clients       <- newMVar mempty
  broadcastChan <- newBroadcastTChanIO

  forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw server
    putStrLn $ "Received from: " ++ show fromAddr ++ ": " ++ (decode' newMessage :: String)

    -- Launch a thread to speak to the client if they're new,
    -- and broadcast the message to all clients
    modifyMVar_ clients $ \currentClients -> 
      if Set.member fromAddr currentClients
      then return currentClients
      else do
        messageChan <- atomically $ dupTChan broadcastChan
        _ <- newClientThread fromAddr messageChan clients
        return $ Set.insert fromAddr currentClients

    -- Broadcast the message to all clients
    atomically $ writeTChan broadcastChan newMessage

-- | Creates a new socket to the client's address, and creates a Chan that's
-- continuously listened to on a new thread and passed along to the new socket
newClientThread :: SockAddr -> TChan ByteString -> MVar (Set SockAddr) -> IO ThreadId
newClientThread clientAddr messageChan clients = forkIO $ do
  toClientSock <- connectedSocketToAddr clientAddr

  -- Have the thread close the socket and remove the client
  -- from the broadcast queue when an exception occurs
  (hostName, serviceName) <- getSockAddrAddress clientAddr
  let displayName = "->" ++ hostName ++ ":" ++ serviceName
      finisher e = do
        putStrLn $ displayName ++ " removing self due to " ++ show (e::SomeException)
        close toClientSock
        modifyMVar_ clients $ return . Set.delete clientAddr
        throwIO e

  handle finisher . forever $ do    
    message <- atomically $ readTChan messageChan
    _bytesSent <- send toClientSock message
    return ()