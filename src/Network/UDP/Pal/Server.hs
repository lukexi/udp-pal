{-# LANGUAGE RecordWildCards #-}
module Network.UDP.Pal.Server where
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
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


