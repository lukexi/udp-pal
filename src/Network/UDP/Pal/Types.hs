module Network.UDP.Pal.Types where

import           Network.Socket            hiding (recv, recvFrom, send, sendTo)

data BoundSocket = BoundSocket
  { bsSocket :: Socket
  , bsPacketSize :: Int 
  } deriving Show

data SocketWithDest = SocketWithDest
  { swdBoundSocket :: BoundSocket
  , swdDestination :: AddrInfo
  } deriving Show


newtype ConnectedSocket = ConnectedSocket { unConnectedSocket :: Socket }