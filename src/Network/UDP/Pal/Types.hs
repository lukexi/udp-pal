module Network.UDP.Pal.Types 
  ( module Network.UDP.Pal.Types
  , HostName
  , PortNumber
  , SockAddr(..)
  ) where

import           Network.Socket

data BoundSocket = BoundSocket
  { bsSocket     :: Socket
  , bsPacketSize :: Int
  } deriving Show

data SocketWithDest = SocketWithDest
  { swdBoundSocket :: BoundSocket
  , swdDestination :: AddrInfo
  } deriving Show

newtype ConnectedSocket = ConnectedSocket 
  { unConnectedSocket :: Socket 
  } deriving Show

type PacketSize = Int