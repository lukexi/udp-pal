{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}

module ReliableUDP where


import Data.Binary
import Control.Monad.State
import Control.Lens

import           Data.Map (Map)
import qualified Data.Map as Map
import Types

queueReliable :: (MonadIO m, MonadState (Connection u r) m, Binary r) => r -> m (Map SeqNum r)
queueReliable payload = do
  seqNum <- connNextSeqNum <<+= 1
  connUnacked . at seqNum ?= payload
  use connUnacked

-- | Discard all packets less than the acknowledged sequence number
-- Server should send acks with every message to the client to ensure redundancy.
receiveAck :: MonadState (Connection u r) m => SeqNum -> m ()
receiveAck seqNum = do
  unacked <- use connUnacked
  let (_smaller, larger) = Map.split seqNum unacked
  connUnacked .= larger
