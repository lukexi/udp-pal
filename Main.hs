import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad
import           Data.Map (Map)
import qualified Data.Map as Map

type ClientMap = Map SockAddr (Chan ByteString)

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (*1000000)

launchClientChannel :: MVar ClientMap -> SockAddr -> IO (Chan ByteString)
launchClientChannel clientsMVar clientSockAddr = do

  (Just hostName, Just serviceName) <- getNameInfo [] True True clientSockAddr
  let displayName = show hostName ++ " : " ++ show serviceName
  clientChannel <- newChan

  putStrLn $ "Launching client channel to " ++ displayName
  
  forkIO $ do 
    toClientSock <- socketToAddress hostName clientPort

    let removeClient = do
          putStrLn $ "Removing client " ++ displayName
          sClose toClientSock
          withMVar clientsMVar $ return . Map.delete clientSockAddr

    (`finally` removeClient) . forever $ do
      message <- readChan clientChannel
      putStrLn $ "Server Broadcasting " ++ (decode' message) ++ " to " ++ displayName
      send toClientSock message
  return clientChannel

launchServer :: IO ()
launchServer = void . forkIO $ do
  serverSock <- listenSocket serverPort

  clientsMVar <- newMVar Map.empty

  putStrLn "i'm a server"

  forever $ do
    (stuff, fromAddr) <- recvFrom serverSock 4096
    let got = decode' stuff :: String
    putStrLn $ "Server got: " ++ show got ++ " from " ++ show fromAddr

    withMVar clientsMVar $ \clients -> do
      newClients <- case Map.lookup fromAddr clients of
        Just _ -> return clients
        Nothing            -> do
          clientChannel <- launchClientChannel clientsMVar fromAddr 
          return $ Map.insert fromAddr clientChannel clients
      forM newClients $ \clientChannel -> 
        writeChan clientChannel stuff
      return newClients

launchClient :: IO ThreadId
launchClient = forkIO $ asClient $ \sendSock -> do

  sendB sendSock "hiiiii"

  receiveSock <- listenSocket clientPort

  putStrLn "I'm a client"
  response <- recv receiveSock 4096
  let got = decode' response :: String
  putStrLn $ "Client got " ++ show got

main :: IO ()
main = do
  putStrLn "Launching Server..."
  launchServer
  threadDelaySec 1
  putStrLn "Launching client..."
  client <- launchClient

  threadDelaySec 1

  putStrLn "Killing Client..."
  killThread client

  threadDelaySec 1
  putStrLn "Launching another client..."
  _client2 <- launchClient

  threadDelaySec 1
  putStrLn "Done."

serverPort :: String
serverPort = "3000"

clientPort :: String
clientPort = "3001"

serverAddr :: String
serverAddr = "192.168.0.14"

asClient :: (Socket -> IO c) -> IO c
asClient = withSocketsDo . bracket (socketToAddress serverAddr serverPort) (\s -> putStrLn "Closing socket" >> sClose s)

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
sendB :: (MonadIO m, Binary a) => Socket -> a -> m Int
sendB s = liftIO . send s . encode'

-- | Encode a value to a strict bytestring
encode' :: Binary a => a -> ByteString
encode' = L.toStrict . encode

-- | Decode a value from a strict bytestring
decode' :: Binary c => ByteString -> c
decode' = decode . L.fromStrict