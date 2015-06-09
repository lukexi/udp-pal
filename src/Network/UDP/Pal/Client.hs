{-# LANGUAGE RecordWildCards #-}
module Network.UDP.Pal.Client where
import           Data.ByteString           (ByteString)

import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Network.UDP.Pal.Socket
import           Control.Monad.Trans
import           Data.Binary

data Client = Client
  { clientSocket      :: Socket
  , clientDestination :: AddrInfo
  } deriving Show

makeClient :: HostName -> PortNumber -> IO Client
makeClient destName destPort = do
  -- We don't 'connect' as we want to receive from whatever address
  -- the server decides to use to talk to us.
  -- Nothing uses any IP, and 0 gets a random port to receive from
  clientSock <- boundSocket Nothing 0

  -- Get the address for the server's receive port
  serverAddrInfo <- addressInfo (Just destName) (Just (show destPort))

  return Client { clientSocket = clientSock, clientDestination = serverAddrInfo }

sendEncoded :: (MonadIO m, Binary a) => Client -> a -> m Int
sendEncoded Client{..} = sendBinaryTo clientSocket (addrAddress clientDestination)

sendRaw :: MonadIO m => Client -> ByteString -> m Int
sendRaw Client{..} bytestring = liftIO $ sendTo clientSocket bytestring (addrAddress clientDestination)

receiveDecoded :: (MonadIO m, Binary a) => Client -> m a
receiveDecoded Client{..} = recvBinary clientSocket