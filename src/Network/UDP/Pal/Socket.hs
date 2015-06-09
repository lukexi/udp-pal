module Network.UDP.Pal.Socket where
import Control.Monad.Trans
import Data.Binary
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import qualified Data.ByteString.Lazy      as L
import Control.Concurrent
import Data.ByteString (ByteString)



-- Connect a socket connected to a single remote address
-- so 'send' and 'recv' work
connectedSocket :: HostName -> ServiceName -> IO Socket
connectedSocket toAddress toPort = do
  addrInfo <- addressInfo (Just toAddress) (Just toPort)
  s <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Connect once so we can use send rather than sendTo
  connect s (addrAddress addrInfo)
  return s

-- | Create a socket bound to our IP and the given port
-- that can send to and receive from anywhere
boundSocket :: PortNumber -> IO Socket
boundSocket listenPort = do
  -- Create a socket
  addrInfo <- addressInfo Nothing (Just (show listenPort))
  sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Bind it to the complete address
  bind sock (addrAddress addrInfo)
  return sock


addressInfo :: Maybe HostName -> Maybe ServiceName -> IO AddrInfo
-- getAddrInfo always returns a non-empty list, or throws an exception
addressInfo address port = head <$> getAddrInfo hints address port
  where 
    -- AI_PASSIVE means to get our current IP if none provided
    hints = Just $ defaultHints { addrFlags = [AI_PASSIVE], addrFamily=AF_INET }



-- | Send a 'Binary' value to a socket
sendBinary :: (MonadIO m, Binary a) => Socket -> a -> m Int
sendBinary s = liftIO . send s . encode'

sendBinaryTo :: (MonadIO m, Binary a) => Socket -> SockAddr -> a -> m Int
sendBinaryTo s addr d = liftIO $ sendTo s (encode' d) addr

recvBinary :: (MonadIO m, Binary a) => Socket -> m a
recvBinary s = liftIO (decode' <$> recv s 4096)

recvBinaryFrom :: (MonadIO m, Binary a) => Socket -> m (a, SockAddr)
recvBinaryFrom s = liftIO $ do
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
  let 
      -- replaceHostName "localhost" = "127.0.0.1"
      replaceHostName other       = other
      hostName                    = replaceHostName hostName0

  return (hostName, serviceName)

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (*1000000)