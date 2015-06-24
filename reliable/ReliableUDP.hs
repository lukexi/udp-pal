{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReliableUDP where

import Data.Binary
import Control.Monad.State
import Control.Lens

import           Data.Map (Map)
import qualified Data.Map as Map
import Types
import Network.UDP.Pal
import Control.Concurrent
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Maybe

-----------
-- Reliable
-----------
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


sendReliable :: forall a b m. (Binary a, Binary b, MonadIO m, MonadState (Connection a b) m) 
             => SocketWithDest -> b -> m ()
sendReliable client message = do
  reliablePackets <- queueReliable message
  forM_ (Map.toList reliablePackets) $ \(seqNum, payload) ->
    sendEncoded client ((ReliablePacket seqNum payload) :: Packet a b)


-------------
-- Unreliable
-------------

newtype UnreliableCollector a = UnreliableCollector
  { urcCollection :: MVar (Map BundleNum [a])
  }

-- | Continuously reads from the given channel and merges
-- each received packet into a collection MVar
makeCollector :: MonadIO m => m (UnreliableCollector a)
makeCollector = liftIO $ UnreliableCollector <$> newMVar Map.empty

collectUnreliablePacket :: MonadIO m => UnreliableCollector a -> BundleNum -> a -> m ()
collectUnreliablePacket collector bundleNum payload = liftIO $
  modifyMVar_ (urcCollection collector) $ 
    return . Map.insertWith (<>) bundleNum [payload]

-- | Called by the app when it's ready for the packets in a given bundle number
-- As we're unreliable, some or all of these may be missing, so the app must be 
-- designed around that.
-- Returns a list of all packets received with the given bundle number,
-- and discards any older messages than that
extractBundle :: MonadIO m => UnreliableCollector a -> BundleNum -> m [a]
extractBundle collector bundleNum = liftIO $ do
  modifyMVar (urcCollection collector) $ \c -> do
    let (_lower, result, higher) = Map.splitLookup bundleNum c
    return (higher, fromMaybe [] result)




