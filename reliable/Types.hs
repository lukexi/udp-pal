{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types where
import           Control.Lens
import           Data.Binary
import           Data.Map     (Map)
import           GHC.Generics

-- Each packet is tagged with a monotonic sequence number.
-- On each transmission, we send all packets in order, over and over,
-- until we receive acknowledgement of their receipt.

newtype SeqNum = SeqNum
  { unSeqNum :: Int } deriving (Eq, Show, Ord, Num, Enum, Binary)
newtype BundleNum = BundleNum
  { unBundleNum :: Int } deriving (Eq, Show, Ord, Num, Enum, Binary)


data Packet u r = UnreliablePacket  BundleNum u
                | ReliablePacket    SeqNum    r
                | ReliablePacketAck SeqNum
                deriving (Show, Generic)
instance (Binary u, Binary r) => Binary (Packet u r)


-- The data necessary to track the retransmission of
-- unacknowledged reliable packets.
data Connection u r = Connection
  { _connNextSeqNumTo   :: SeqNum
  , _connNextSeqNumFrom :: SeqNum
  , _connUnacked        :: Map SeqNum r
  }

makeLenses ''Connection

newConnection :: (Binary u, Binary r) => Connection u r
newConnection = Connection 0 0 mempty
