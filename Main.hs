import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (*1000000)

launchServer :: IO ()
launchServer = void . forkIO $ do
  serverSock <- listenSocket serverPort

  putStrLn "i'm a server"

  forever $ do
    (stuff, fromAddr) <- recvFrom serverSock 4096
    let got = decode' stuff :: String
    putStrLn $ "got stuff: " ++ show got ++ " from " ++ show fromAddr

  

launchClient :: IO ThreadId
launchClient = forkIO $ asClient $ \clientSock -> do
  -- clientSock <- socketToAddress serverAddr serverPort

  sendB clientSock ""

  putStrLn "I'm a client"

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
  client <- launchClient

  threadDelaySec 1
  putStrLn "Done."

serverPort :: String
serverPort = "3000"

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