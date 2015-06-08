import           Control.Concurrent
import           Control.Concurrent.STM
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



type ClientMap = Map SockAddr (TChan ByteString)


launchClientChannel :: MVar ClientMap -> SockAddr -> IO (TChan ByteString)
launchClientChannel clientsMVar clientSockAddr = do

  -- Get the client's name and port number
  (hostName, serviceName) <- getSockAddrAddress clientSockAddr

  let displayName = "->" ++ hostName ++ ":" ++ serviceName
  -- clientChannel <- newChan
  clientChannel <- newTChanIO

  putStrLn $ displayName ++ " launching client channel"

  _ <- forkIO $ do
    toClientSock <- socketToAddress hostName serviceName

    let removeClient = do
          putStrLn $ displayName ++ " removing self..."
          sClose toClientSock
          withMVar clientsMVar $ return . Map.delete clientSockAddr

    let showException e = putStrLn $ displayName ++ " " ++ show (e::SomeException)
    forever . handle (\e -> showException e >> removeClient >> throwIO e) $ do
      
    -- (`finally` removeClient) . forever $ do
      putStrLn $ displayName ++ " awaiting message..."
      -- message <- readChan clientChannel
      message <- atomically $ readTChan clientChannel
      putStrLn $ displayName ++ " sending: " ++ (decode' message)
      _bytesSent <- send toClientSock message

      putStrLn $ displayName ++ " sent " ++ show _bytesSent ++ " bytes"
  return clientChannel

launchServer :: IO ()
launchServer = void . forkIO $ do
  serverSock <- listenSocket serverPort

  clientsMVar <- newMVar Map.empty

  putStrLn "i'm a server"

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
      putStrLn $ "Clients now: " ++ show (Map.keys newClients)
      -- Broadcast the message to all clients
      forM_ newClients $ \clientChannel ->
        -- writeChan clientChannel stuff
        atomically $ writeTChan clientChannel stuff
      return newClients

launchClient :: IO ThreadId
launchClient = forkIO $ asClient $ \clientSock -> do

  _bytesSent <- sendBinary clientSock "hiiiii"
  putStrLn (show _bytesSent)

  -- receiveSock <- listenSocket clientPort

  putStrLn "I'm a client"
  response <- recvBinary clientSock
  putStrLn $ "CLIENT GOT::::::::::::: " ++ (response :: String)

main :: IO ()
main = do
  putStrLn "Launching Server..."
  launchServer
  threadDelaySec 1
  putStrLn "Launching client..."
  _client1 <- launchClient

  putStrLn "Waiting 5 seconds..."
  threadDelaySec 5

  -- putStrLn "Killing Client..."
  -- killThread client1

  threadDelaySec 1
  putStrLn "Launching another client..."
  _client2 <- launchClient

  threadDelaySec 1
  putStrLn "Done."

serverPort :: String
serverPort = "3000"

serverAddr :: String
serverAddr = "127.0.0.1"

asClient :: (Socket -> IO c) -> IO c
asClient = withSocketsDo . bracket (socketToAddress serverAddr serverPort) 
  (\s -> putStrLn "Closing socket" >> sClose s)

-- asServer :: (Socket -> IO c) -> IO c
-- asServer = withSocketsDo . bracket (listenSocket serverPort) sClose

-- Connect a socket to a remote address
socketToAddress :: HostName -> ServiceName -> IO Socket
socketToAddress toAddress toPort = do
  (serverAddrInfo:_) <- getAddrInfo Nothing (Just toAddress) (Just toPort)
  s <- socket (addrFamily serverAddrInfo) Datagram defaultProtocol
  -- Connect once so we can use send rather than sendTo
  connect s (addrAddress serverAddrInfo)
  return s

-- | Create a socket than can be listened to
listenSocket :: ServiceName -> IO Socket
listenSocket listenPort = do
  -- Configure for accepting connections
  let hints = Just $ defaultHints { addrFlags = [AI_PASSIVE] }
  (serverAddrInfo:_) <- getAddrInfo hints Nothing (Just listenPort)
  sock <- socket (addrFamily serverAddrInfo) Datagram defaultProtocol
  bindSocket sock (addrAddress serverAddrInfo)
  return sock

-- | Send a 'Binary' value to a socket
sendBinary :: (MonadIO m, Binary a) => Socket -> a -> m Int
sendBinary s = liftIO . send s . encode'

recvBinary :: (MonadIO m, Binary a) => Socket -> m a
recvBinary s = liftIO (decode' <$> recv s 4096)

-- | Encode a value to a strict bytestring
encode' :: Binary a => a -> ByteString
encode' = L.toStrict . encode

-- | Decode a value from a strict bytestring
decode' :: Binary c => ByteString -> c
decode' = decode . L.fromStrict


getSockAddrAddress :: SockAddr -> IO (HostName, ServiceName)
getSockAddrAddress sockAddr = do
  (Just hostName, Just serviceName) <- getNameInfo [] True True sockAddr
  return (hostName, serviceName)

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (*1000000)
