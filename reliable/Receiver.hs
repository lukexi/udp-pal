{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Receiver where
import           Control.Concurrent.STM
import           Network.UDP.Pal
import           Types

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.State
import           Halive.Concurrent
import           ReliableUDP

streamInto :: TChan a -> IO a -> IO ()
streamInto channel action =
  -- FIXME find a reliable way to kill this thread
  void . forkIO . forever $
    atomically . writeTChan channel =<< action

exhaustChan :: TChan a -> STM [a]
exhaustChan chan = go mempty
  where
    go !accum = tryReadTChan chan >>= \case
      Just msg -> go (accum ++ [msg])
      Nothing -> return accum

createReceiver :: String
               -> Either SocketWithDest ConnectedSocket
               -> IO
                  (TChan (Packet   ObjectPose r),
                   TChan (Outgoing ObjectPose r),
                   TChan (Outgoing ObjectPose ObjectOp))
createReceiver name eitherSocket = do
  incomingRawPackets  <- newTChanIO
  verifiedPackets     <- newTChanIO
  outgoingPackets     <- newTChanIO

  let conn = newConnection :: Connection ObjectPose ObjectOp
      sendReliablePacket    = either sendReliable sendReliableConn eitherSocket
      sendUnreliablePacket  = either sendBinary   sendBinaryConn   eitherSocket

  _threadID <- forkIO' . void . flip runStateT conn . forever $ do
    liftIO $ putStrLn $ name ++ " awaiting packet"

    (outgoing, incoming) <- liftIO . atomically $ do
      outgoing <- exhaustChan outgoingPackets
      packet   <- exhaustChan incomingRawPackets
      case (outgoing, packet) of
        ([], []) -> retry
        (someOutgoing, someIncoming) -> return (someOutgoing, someIncoming)

    forM_ (outgoing :: [Outgoing ObjectPose ObjectOp]) $ \case
      Reliable reliablePacket -> sendReliablePacket reliablePacket
      Unreliable unreliableBundle -> do
        bundleNum <- connNextBundleNum <<%= succ
        liftIO $ forM_ unreliableBundle $ \piece ->
          sendUnreliablePacket (UnreliablePacket bundleNum piece :: Packet ObjectPose ObjectOp)

    forM_ incoming $ \case
      UnreliablePacket bundleNum payload -> do
        maybeBundle <- collectUnreliablePacket bundleNum payload
        case maybeBundle of
          Nothing -> return ()
          Just bundle -> liftIO . atomically . writeTChan verifiedPackets $ Unreliable bundle
      ReliablePacket seqNum payload ->
        collectReliablePacket seqNum $ liftIO $ do

          _ <- sendUnreliablePacket (ReliablePacketAck seqNum :: Packet ObjectPose ObjectOp)
          liftIO . atomically . writeTChan verifiedPackets $ Reliable payload
      ReliablePacketAck seqNum ->
        receiveAck seqNum
  return
    ( incomingRawPackets  -- Channel to pipe in raw packets from the socket;
    , verifiedPackets     -- Channel to get out sequenced reliable packets and bundled unreliable packets
    , outgoingPackets     -- Channel to write packets to send along
    )
