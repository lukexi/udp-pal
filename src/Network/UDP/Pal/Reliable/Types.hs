{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.UDP.Pal.Reliable.Types where
import           Control.Lens
import           Data.Binary
import           Data.Map.Strict     (Map)
import           GHC.Generics
import           Control.Concurrent.STM
import           Data.Time

-- Each packet is tagged with a monotonic sequence number.
-- On each transmission, we send all packets in order, over and over,
-- until we receive acknowledgement of their receipt.

newtype SeqNum = SeqNum
  { unSeqNum :: Int } 
  deriving (Eq, Show, Ord, Num, Enum, Binary)
newtype BundleNum = BundleNum
  { unBundleNum :: Int } 
  deriving (Eq, Show, Ord, Num, Enum, Binary)

-- These are the packets that are actually serialized over the wire
data WirePacket r = UnreliablePacket BundleNum r
                    | ReliablePacket    SeqNum r
                    | ReliablePacketAck SeqNum
                    | KeepAlive
                    deriving (Show, Generic)
instance (Binary r) => Binary (WirePacket r)

-- These are packets submitted to and received from the Transceiver
data AppPacket r = Unreliable [r] | Reliable r deriving Show

-- The data necessary to track the retransmission of
-- unacknowledged reliable packets.
data TransceiverState r = TransceiverState
  { _connNextSeqNumTo   :: SeqNum
  , _connNextSeqNumFrom :: SeqNum
  , _connNextBundleNum  :: BundleNum
  , _connUnacked        :: Map SeqNum r
  , _connBundles        :: Map BundleNum [r]
  }

makeLenses ''TransceiverState

newTransceiverState :: Binary r => TransceiverState r
newTransceiverState = TransceiverState 0 0 0 mempty mempty

data Transceiver r = Transceiver 
  { tcIncomingRawPackets :: TChan (WirePacket r)
  , tcVerifiedPackets    :: TChan (AppPacket  r)
  , tcOutgoingPackets    :: TChan (AppPacket  r)
  , tcLastMessageTime    :: TVar UTCTime
  , tcShutdown           :: IO ()
  }
