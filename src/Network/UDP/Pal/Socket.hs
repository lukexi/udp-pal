module Network.UDP.Pal.Socket where
import Control.Monad.Trans
import Data.Binary
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import Control.Concurrent
import Network.UDP.Pal.Binary

-- | Create a socket bound to our IP and the given port
-- that can send to and receive from anywhere.
-- I believe passing Just an IP will allow public IP binding, need to verify
-- NOTE I gave up on 'connect()'/'send()' in favor of 'bind()'/'sendto()' 
-- as I couldn't get it to talk across computers yet
-- UPDATE this turned out to just be the Windows firewall. I should use connect and send again.
boundSocket :: Maybe HostName -> PortNumber -> IO Socket
boundSocket maybeHostName listenPort = do
  -- Create a socket
  addrInfo <- addressInfo maybeHostName (Just (show listenPort))
  sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Bind it to the complete address
  bind sock (addrAddress addrInfo)
  return sock


-- | Send a 'Binary' value to a socket/address
sendBinaryTo :: (MonadIO m, Binary a) => Socket -> SockAddr -> a -> m Int
sendBinaryTo s addr d = liftIO $ sendTo s (encode' d) addr

-- | Receive a 'Binary' value from a socket along with the address it originated from
recvBinaryFrom :: (MonadIO m, Binary a) => Socket -> Int -> m (a, SockAddr)
recvBinaryFrom s packetSize = liftIO $ do
  (d, fromAddr) <- recvFrom s packetSize
  return (decode' d, fromAddr)


-- | Get the AF_INET address for a hostname/servicename combo
-- (getAddrInfo always returns a non-empty list, or throws an exception)
addressInfo :: Maybe HostName -> Maybe ServiceName -> IO AddrInfo
addressInfo address port = head <$> getAddrInfo hints address port
  where 
    -- AI_PASSIVE means to get our current IP if none provided
    hints = Just $ defaultHints { addrFlags = [AI_PASSIVE], addrFamily = AF_INET }

getSockAddrAddress :: SockAddr -> IO (HostName, ServiceName)
getSockAddrAddress sockAddr = do
  (Just hostName, Just serviceName) <- getNameInfo [] True True sockAddr
  return (hostName, serviceName)

threadDelaySec :: Float -> IO ()
threadDelaySec = threadDelay . floor . (*1000000)