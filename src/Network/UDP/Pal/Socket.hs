{-# LANGUAGE RecordWildCards #-}
module Network.UDP.Pal.Socket 
  ( module Network.UDP.Pal.Socket
  , close -- From Network.Socket
  ) where

import           Control.Concurrent
import           Control.Monad.Trans
import           Data.Binary
import           Data.ByteString           (ByteString)
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Network.UDP.Pal.Binary
import           Network.UDP.Pal.Types

-- | Create a socket bound to our IP and the given port
-- that can send to and receive from anywhere.
-- I believe passing Just an IP will allow public IP binding, need to verify
boundSocket :: Maybe HostName -> PortNumber -> PacketSize -> IO BoundSocket
boundSocket maybeHostName listenPort packetSize = do
  -- Create a socket
  addrInfo <- addressInfo maybeHostName (Just (show listenPort))
  sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Bind it to the complete address
  bind sock (addrAddress addrInfo)
  
  return (BoundSocket sock packetSize)

-- We don't 'connect' as we want to receive from whatever address
-- the server decides to use to talk to us.
-- We can't use two sockets, since we want to let the server use our
-- from address to send us messages.
-- Nothing uses any IP, and 0 gets a random port to receive from

socketWithDest :: HostName -> PortNumber -> PacketSize -> IO SocketWithDest
socketWithDest destName destPort packetSize = do
  boundSock      <- boundSocket Nothing aNY_PORT packetSize
  -- Get the address for the server's receive port
  serverAddrInfo <- addressInfo (Just destName) (Just (show destPort))

  return SocketWithDest
    { swdBoundSocket = boundSock
    , swdDestination = serverAddrInfo
    }

sendBinary :: (MonadIO m, Binary a) => SocketWithDest -> a -> m Int
sendBinary SocketWithDest{..} =
  sendBinaryTo (bsSocket swdBoundSocket) (addrAddress swdDestination)

sendRaw :: MonadIO m => SocketWithDest -> ByteString -> m Int
sendRaw SocketWithDest{..} bytestring =
  liftIO $ sendTo (bsSocket swdBoundSocket) bytestring (addrAddress swdDestination)

receiveFromDecoded :: (MonadIO m, Binary a) => BoundSocket -> m (a, SockAddr)
receiveFromDecoded BoundSocket{..} = recvBinaryFrom bsSocket bsPacketSize

receiveFromRaw :: (MonadIO m) => BoundSocket -> m (ByteString, SockAddr)
receiveFromRaw BoundSocket{..} = liftIO $ recvFrom bsSocket bsPacketSize


-- | Get the AF_INET address for a hostname/servicename combo
-- (getAddrInfo always returns a non-empty list, or throws an exception)
addressInfo :: Maybe HostName -> Maybe ServiceName -> IO AddrInfo
addressInfo address port = head <$> getAddrInfo hints address port
  where
    -- AI_PASSIVE means to get our current IP if none provided
    hints = Just $ defaultHints { addrFlags = [AI_PASSIVE], addrFamily = AF_INET }





-- | Create a socket than can only send to and recv from the given
-- sockAddr, usually as obtained via recvFrom. You could also create
-- a SockAddr using addressInfo.
connectedSocketToAddr :: MonadIO m => SockAddr -> m ConnectedSocket
connectedSocketToAddr sockAddr = liftIO $ do
  -- Create a socket
  sock <- socket AF_INET Datagram defaultProtocol
  -- Connect it to the address for send/recv
  connect sock sockAddr
  return (ConnectedSocket sock)



-- | Send a 'Binary' value to a connected socket
sendBinaryConn :: (MonadIO m, Binary a) => ConnectedSocket -> a -> m Int
sendBinaryConn s d = sendConn s (encode' d)

sendConn :: (MonadIO m) => ConnectedSocket -> ByteString -> m Int
sendConn (ConnectedSocket s) d = liftIO $ send s d

-- | Receive a 'Binary' value from a socket along with the address it originated from
recvBinaryFrom :: (MonadIO m, Binary a) => Socket -> Int -> m (a, SockAddr)
recvBinaryFrom s packetSize = liftIO $ do
  (d, fromAddr) <- recvFrom s packetSize
  return (decode' d, fromAddr)

-- | Send a 'Binary' value to a socket/address
sendBinaryTo :: (MonadIO m, Binary a) => Socket -> SockAddr -> a -> m Int
sendBinaryTo s addr d = liftIO $ sendTo s (encode' d) addr


getSockAddrAddress :: SockAddr -> IO (HostName, ServiceName)
getSockAddrAddress sockAddr = do
  (Just hostName, Just serviceName) <- getNameInfo [] True True sockAddr
  return (hostName, serviceName)

threadDelaySec :: Float -> IO ()
threadDelaySec = threadDelay . floor . (*1000000)
