module Network.UDP.Pal.Server where
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Network.UDP.Pal.Socket


type ClientMap = Map SockAddr (Chan ByteString)


makeServer :: HostName -> PortNumber -> IO ()
makeServer serverName serverPort = void . forkIO $ do

  serverSock <- boundSocket (Just serverName) serverPort

  clientsMVar <- newMVar Map.empty

  forever $ do
    -- putStrLn . ("Clients now: " ++) . show . Map.keys =<< readMVar clientsMVar
    -- putStrLn $ "Server awaiting message..."

    -- Use recvFrom so we can see who the message came from
    (newMessage, fromAddr) <- recvFrom serverSock 4096

    -- let got = decode' newMessage :: String
    -- putStrLn $ "Server got: " ++ show got ++ " from " ++ show fromAddr

    -- Launch a thread to speak to the client if they're new,
    -- and broadcast the message to all clients
    modifyMVar_ clientsMVar $ \clients -> do
      -- Check if the client exists yet
      newClients <- case Map.lookup fromAddr clients of
        Just _ -> return clients
        Nothing            -> do
          clientChannel <- newClientThread clientsMVar fromAddr
          return $ Map.insert fromAddr clientChannel clients

      -- Broadcast the message to all clients
      forM_ newClients $ \clientChannel ->
        writeChan clientChannel newMessage
      return newClients


-- | Creates a new socket to the client's address, and creates a Chan that's
-- continuously listened to on a new thread and passed along to the new socket
newClientThread :: MVar ClientMap -> SockAddr -> IO (Chan ByteString)
newClientThread clientsMVar clientSockAddr = do
  
  clientChannel <- newChan

  -- Start a new thread to broadcast messages to the client
  _ <- forkIO $ do

    toClientSock            <- connectedSocketToAddr clientSockAddr

    -- Have the thread close the socket and remove the client
    -- from the broadcast queue when an exception occurs
    (hostName, serviceName) <- getSockAddrAddress clientSockAddr
    let displayName = "->" ++ hostName ++ ":" ++ serviceName
        finisher e = do
          putStrLn $ displayName ++ " removing self due to " ++ show (e::SomeException)
          close toClientSock
          modifyMVar_ clientsMVar $ return . Map.delete clientSockAddr
          throwIO e

    handle finisher . forever $ do
      
      -- putStrLn $ displayName ++ " awaiting message..."
      
      message <- readChan clientChannel
      
      -- putStrLn $ displayName ++ " sending: " ++ (decode' message)
      _bytesSent <- send toClientSock message

      -- putStrLn $ displayName ++ " sent " ++ show _bytesSent ++ " bytes"
      return ()
  return clientChannel