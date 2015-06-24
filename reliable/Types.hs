{-# LANGUAGE DeriveGeneric, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Types where
import Data.Map (Map)
import GHC.Generics
import Data.Binary
import Control.Lens

-- Each packet is tagged with a monotonic sequence number.
-- On each transmission, we send all packets in order, over and over,
-- until we receive acknowledgement of their receipt.

newtype SeqNum = SeqNum 
  { unSeqNum :: Int } deriving (Eq, Show, Ord, Num, Binary)
newtype BundleNum = BundleNum 
  { unBundleNum :: Int } deriving (Eq, Show, Ord, Num, Binary)


data Packet u r = UnreliablePacket  BundleNum u
                | ReliablePacket    SeqNum    r
                | ReliablePacketAck SeqNum
                deriving (Show, Generic)
instance (Binary u, Binary r) => Binary (Packet u r)


-- The data necessary to track the retransmission of 
-- unacknowledged reliable packets.
data Connection u r = Connection
  { _connNextSeqNum :: SeqNum
  , _connUnacked    :: Map SeqNum r
  }

makeLenses ''Connection

newConnection :: (Binary u, Binary r) => Connection u r
newConnection = Connection 0 mempty
