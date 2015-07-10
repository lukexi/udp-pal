{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.UDP.Pal.Reliable.ReliableUDP where

import Data.Binary
import Control.Monad.State
import Control.Lens

import qualified Data.Map as Map
import           Network.UDP.Pal.Reliable.Types
import           Network.UDP.Pal.Socket
import           Network.UDP.Pal.Types

import           Data.Monoid

-----------
-- Reliable
-----------
queueReliable :: (MonadIO m, MonadState (TransceiverState u r) m, Binary r) => r -> m ()
queueReliable payload = do
  seqNum <- connNextSeqNumTo <<%= succ
  connUnacked . at seqNum ?= payload

-- | Discard all packets less than the acknowledged sequence number
-- Server should send acks with every message to the client to ensure redundancy.
receiveAck :: MonadState (TransceiverState u r) m => SeqNum -> m ()
receiveAck seqNum = do
  unacked <- use connUnacked
  let (_smaller, larger) = Map.split seqNum unacked
  connUnacked .= larger

sendReliable :: forall u r m. (Binary u, Binary r, MonadIO m, MonadState (TransceiverState u r) m)
             => Either SocketWithDest ConnectedSocket -> r -> m ()
sendReliable sock message = do
  queueReliable message
  purgeReliable sock

purgeReliable :: forall u r m. (Binary u, Binary r, MonadIO m, MonadState (TransceiverState u r) m)
             => Either SocketWithDest ConnectedSocket -> m ()
purgeReliable sock = do
  reliablePackets <- use connUnacked
  forM_ (Map.toList reliablePackets) $ \(seqNum, payload) ->
    sendBinaryE sock (ReliablePacket seqNum payload :: WirePacket u r)

sendBinaryE :: (MonadIO m, Binary r) => Either SocketWithDest ConnectedSocket -> r -> m Int
sendBinaryE sock = either sendBinary sendBinaryConn sock

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
