{-# LANGUAGE RecordWildCards #-}
module Network.UDP.Pal.Server where
import           Data.ByteString           (ByteString)
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Network.UDP.Pal.Socket

data Server = Server
  { serverSocket :: Socket
  , serverPacketSize :: Int 
  } deriving Show

receiveFromRaw :: Server -> IO (ByteString, SockAddr)
receiveFromRaw Server{..} = recvFrom serverSocket serverPacketSize

makeServer :: HostName -> PortNumber -> Int -> IO Server
makeServer serverName serverPort packetSize = do
  serverSock <- boundSocket (Just serverName) serverPort
  return Server { serverSocket = serverSock, serverPacketSize = packetSize }


