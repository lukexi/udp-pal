{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.UDP.Pal.Reliable.ReliableUDP where

import Data.Binary
import Control.Monad.State
import Control.Lens

import           Data.Map (Map)
import qualified Data.Map as Map
import           Network.UDP.Pal.Reliable.Types
import           Network.UDP.Pal.Socket
import           Network.UDP.Pal.Types

import           Data.Monoid

-----------
-- Reliable
-----------
queueReliable :: (MonadIO m, MonadState (TransceiverState u r) m, Binary r) => r -> m (Map SeqNum r)
queueReliable payload = do
  seqNum <- connNextSeqNumTo <<%= succ
  connUnacked . at seqNum ?= payload
  use connUnacked

-- | Discard all packets less than the acknowledged sequence number
-- Server should send acks with every message to the client to ensure redundancy.
receiveAck :: MonadState (TransceiverState u r) m => SeqNum -> m ()
receiveAck seqNum = do
  unacked <- use connUnacked
  let (_smaller, larger) = Map.split seqNum unacked
  connUnacked .= larger


sendReliable :: forall a b m. (Binary a, Binary b, MonadIO m, MonadState (TransceiverState a b) m)
             => SocketWithDest -> b -> m ()
sendReliable sock message = do
  reliablePackets <- queueReliable message
  forM_ (Map.toList reliablePackets) $ \(seqNum, payload) ->
    sendBinary sock (ReliablePacket seqNum payload :: WirePacket a b)

sendReliableConn :: forall a b m. (Binary a, Binary b, MonadIO m, MonadState (TransceiverState a b) m)
                 => ConnectedSocket -> b -> m ()
sendReliableConn sock message = do
  reliablePackets <- queueReliable message
  forM_ (Map.toList reliablePackets) $ \(seqNum, payload) ->
    sendBinaryConn sock (ReliablePacket seqNum payload :: WirePacket a b)

-- | Given the seqNum of a newly-received reliable packet,
-- checkes if the seqNum is the successor of the last one we received.
-- If so, records the new seqNum and calls the given action; otherwise, does nothing.
collectReliablePacket :: MonadState (TransceiverState u r) m
                      => SeqNum -> m () -> m ()
collectReliablePacket seqNum action = do
  nextSeqNumFrom <- use connNextSeqNumFrom
  when (seqNum == nextSeqNumFrom) $ do
    connNextSeqNumFrom %= succ
    action

-------------
-- Unreliable
-------------
collectUnreliablePacket :: MonadState (TransceiverState u r) m
                        => BundleNum
                        -> u
                        -> m (Maybe [u])
collectUnreliablePacket bundleNum payload = do
  -- Ignore bundle pieces that are older than our oldest buffered bundle
  bundles <- use connBundles
  let isOlder = bundles ^? to Map.minViewWithKey . traverse . _1 . _1 . to (bundleNum <)
  if (isOlder == Just True) 
    then return Nothing
    else do
      newBundles <- connBundles <%= Map.insertWith (<>) bundleNum [payload]
      if Map.size newBundles < 5
        then return Nothing
        else do
          let ((_oldestBundleNum, oldestBundle), bundlesMinusOldest) = Map.deleteFindMin newBundles
          connBundles .= bundlesMinusOldest
          return (Just oldestBundle)
