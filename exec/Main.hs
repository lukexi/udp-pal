import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.Binary
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as L
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString

type ClientMap = Map SockAddr (Chan ByteString)

launchClientChannel :: MVar ClientMap -> SockAddr -> IO (Chan ByteString)
launchClientChannel clientsMVar clientSockAddr = do

  -- Get the client's name and port number
  (hostName, serviceName) <- getSockAddrAddress clientSockAddr

  let displayName = "->" ++ hostName ++ ":" ++ serviceName
  clientChannel <- newChan

  _ <- forkIO $ do
    toClientSock <- socketToAddress hostName serviceName

    let removeClient = do
          putStrLn $ displayName ++ " removing self..."
          close toClientSock
          withMVar clientsMVar $ return . Map.delete clientSockAddr

    let showException e = putStrLn $ displayName ++ " " ++ show (e::SomeException)
        handleException = handle (\e -> showException e >> removeClient >> throwIO e)
    handleException . forever $ do
      
      putStrLn $ displayName ++ " awaiting message..."
      
      message <- readChan clientChannel
      
      putStrLn $ displayName ++ " sending: " ++ (decode' message)
      _bytesSent <- send toClientSock message

      putStrLn $ displayName ++ " sent " ++ show _bytesSent ++ " bytes"
  return clientChannel

launchServer :: IO ()
launchServer = void . forkIO $ do
  serverSock <- listenSocket serverPort

  clientsMVar <- newMVar Map.empty

  forever $ do
    putStrLn . ("Clients now: " ++) . show . Map.keys =<< readMVar clientsMVar
    putStrLn $ "Server awaiting message..."
    (stuff, fromAddr) <- recvFrom serverSock 4096
    let got = decode' stuff :: String
    putStrLn $ "Server got: " ++ show got ++ " from " ++ show fromAddr
    
    modifyMVar_ clientsMVar $ \clients -> do
      -- Check if the client exists yet
      newClients <- case Map.lookup fromAddr clients of
        Just _ -> return clients
        Nothing            -> do
          clientChannel <- launchClientChannel clientsMVar fromAddr
          return $ Map.insert fromAddr clientChannel clients
      
      -- Broadcast the message to all clients
      forM_ newClients $ \clientChannel ->
        writeChan clientChannel stuff
      return newClients

launchClient :: IO ThreadId
launchClient = forkIO $ do

  let hints = Just $ defaultHints { addrFlags = [AI_PASSIVE] }
  (addrInfo:_) <- getAddrInfo hints (Just "127.0.0.1") Nothing
  clientSock <- socket AF_INET Datagram defaultProtocol
  bind clientSock (addrAddress addrInfo)

  clientPort <- socketPort clientSock

  let displayName = "127.0.0.1:" ++ show clientPort
  putStrLn $ "Launched client: " ++ displayName

  (serverAddrInfo:_) <- getAddrInfo Nothing (Just serverName) (Just serverPort)

  _bytesSent <- sendBinaryTo clientSock (addrAddress serverAddrInfo) "HELLO THERE"

  (`finally` close clientSock) . forever $ do
    -- (response, _) <- recvBinaryFrom clientSock
    response <- recvBinary clientSock
    putStrLn $ "<-" ++ displayName ++ " received: " ++ (response :: String)

main :: IO ()
main = do
  putStrLn "Launching Server..."
  launchServer
  threadDelaySec 1
  putStrLn "Launching client..."
  _client1 <- launchClient

  putStrLn "Waiting 5 seconds..."
  threadDelaySec 5

  putStrLn "Killing Client..."
  killThread _client1

  threadDelaySec 1
  putStrLn "Launching another client..."
  _client2 <- launchClient

  threadDelaySec 1
  putStrLn "Done."

serverPort :: String
serverPort = "3000"

serverName :: String
serverName = "127.0.0.1"

-- Connect a socket to a remote address
socketToAddress :: HostName -> ServiceName -> IO Socket
socketToAddress toAddress toPort = do
  (addrInfo:_) <- getAddrInfo Nothing (Just toAddress) (Just toPort)
  s <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Connect once so we can use send rather than sendTo
  connect s (addrAddress addrInfo)
  return s

-- | Create a socket than can be listened to
listenSocket :: ServiceName -> IO Socket
listenSocket listenPort = do
  -- Configure for accepting connections
  let hints = Just $ defaultHints { addrFlags = [AI_PASSIVE] }
  (serverAddrInfo:_) <- getAddrInfo hints Nothing (Just listenPort)
  sock <- socket (addrFamily serverAddrInfo) Datagram defaultProtocol
  bind sock (addrAddress serverAddrInfo)
  return sock

-- | Send a 'Binary' value to a socket
sendBinary :: (MonadIO m, Binary a) => Socket -> a -> m Int
sendBinary s = liftIO . send s . encode'

sendBinaryTo :: (MonadIO m, Binary a) => Socket -> SockAddr -> a -> m Int
sendBinaryTo s addr d = liftIO $ sendTo s (encode' d) addr

recvBinary :: (MonadIO m, Binary a) => Socket -> m a
recvBinary s = liftIO (decode' <$> recv s 4096)

-- recvBinaryFrom :: (MonadIO m, Binary a) => Socket -> m a
recvBinaryFrom s = do
  (d, fromAddr) <- recvFrom s 4096
  return (decode' d, fromAddr)

-- | Encode a value to a strict bytestring
encode' :: Binary a => a -> ByteString
encode' = L.toStrict . encode

-- | Decode a value from a strict bytestring
decode' :: Binary c => ByteString -> c
decode' = decode . L.fromStrict


getSockAddrAddress :: SockAddr -> IO (HostName, ServiceName)
getSockAddrAddress sockAddr = do
  (Just hostName0, Just serviceName) <- getNameInfo [] True True sockAddr

  -- Override localhost as 127.0.0.1 to fix a "connection refused" exception due to IPV6
  let replaceHostName "localhost" = "127.0.0.1"
      replaceHostName other       = other
      hostName                    = replaceHostName hostName0

  return (hostName, serviceName)

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (*1000000)
