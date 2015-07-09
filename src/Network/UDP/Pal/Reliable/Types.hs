{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.UDP.Pal.Reliable.Types where
import           Control.Lens
import           Data.Binary
import           Data.Map     (Map)
import           GHC.Generics
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time
import Network.UDP.Pal.Types

-- Each packet is tagged with a monotonic sequence number.
-- On each transmission, we send all packets in order, over and over,
-- until we receive acknowledgement of their receipt.

newtype SeqNum = SeqNum
  { unSeqNum :: Int } deriving (Eq, Show, Ord, Num, Enum, Binary)
newtype BundleNum = BundleNum
  { unBundleNum :: Int } deriving (Eq, Show, Ord, Num, Enum, Binary)

-- These are the packets that are actually serialized over the wire
data WirePacket u r = UnreliablePacket  BundleNum u
                    | ReliablePacket    SeqNum    r
                    | ReliablePacketAck SeqNum
                    deriving (Show, Generic)
instance (Binary u, Binary r) => Binary (WirePacket u r)

-- These are packets submitted to and received from the Transceiver
data AppPacket u r = Unreliable [u] | Reliable r deriving Show

-- The data necessary to track the retransmission of
-- unacknowledged reliable packets.
data TransceiverState u r = TransceiverState
  { _connNextSeqNumTo   :: SeqNum
  , _connNextSeqNumFrom :: SeqNum
  , _connNextBundleNum  :: BundleNum
  , _connUnacked        :: Map SeqNum r
  , _connBundles        :: Map BundleNum [u]
  }

makeLenses ''TransceiverState

newTransceiverState :: (Binary u, Binary r) => TransceiverState u r
newTransceiverState = TransceiverState 0 0 0 mempty mempty




data Transceiver u r = Transceiver 
  { tcIncomingRawPackets :: TChan (WirePacket u r)
  , tcVerifiedPackets    :: TChan (AppPacket  u r)
  , tcOutgoingPackets    :: TChan (AppPacket  u r)
  , tcLastMessageTime    :: TVar UTCTime
  , tcSocket             :: Either SocketWithDest ConnectedSocket
  , tcThreads            :: [ThreadId]
  }
