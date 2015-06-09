module Network.UDP.Pal.Socket where
import Control.Monad.Trans
import Data.Binary
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import qualified Data.ByteString.Lazy      as L
import Control.Concurrent
import Data.ByteString (ByteString)

-- | Create a socket bound to our IP and the given port
-- that can send to and receive from anywhere.
-- I believe passing Just an IP will allow public IP binding, need to verify
-- NOTE I gave up on 'connect()'/'send()' in favor of 'bind()'/'sendto()' 
-- as I couldn't get it to talk across computers yet
boundSocket :: Maybe HostName -> PortNumber -> IO Socket
boundSocket maybeHostName listenPort = do
  -- Create a socket
  addrInfo <- addressInfo maybeHostName (Just (show listenPort))
  sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Bind it to the complete address
  bind sock (addrAddress addrInfo)
  return sock


addressInfo :: Maybe HostName -> Maybe ServiceName -> IO AddrInfo
-- getAddrInfo always returns a non-empty list, or throws an exception
addressInfo address port = head <$> getAddrInfo hints address port
  where 
    -- AI_PASSIVE means to get our current IP if none provided
    hints = Just $ defaultHints { addrFlags = [AI_PASSIVE], addrFamily = AF_INET }
    -- hints = case address of
    --   Just _  -> Just $ defaultHints { addrFamily = AF_INET }
    --   Nothing -> Just $ defaultHints { addrFlags = [AI_PASSIVE], addrFamily = AF_INET }



-- | Send a 'Binary' value to a socket/address
sendBinaryTo :: (MonadIO m, Binary a) => Socket -> SockAddr -> a -> m Int
sendBinaryTo s addr d = liftIO $ sendTo s (encode' d) addr

recvBinaryFrom :: (MonadIO m, Binary a) => Socket -> Int -> m (a, SockAddr)
recvBinaryFrom s packetSize = liftIO $ do
  (d, fromAddr) <- recvFrom s packetSize
  return (decode' d, fromAddr)

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

threadDelaySec :: Float -> IO ()
threadDelaySec = threadDelay . floor . (*1000000)