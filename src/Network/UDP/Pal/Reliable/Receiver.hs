{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.UDP.Pal.Reliable.Receiver where
import           Control.Concurrent.STM
import           Network.UDP.Pal.Socket
import Network.UDP.Pal.Types
import           Network.UDP.Pal.Reliable.Types

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.State
import           Halive.Concurrent
import           Network.UDP.Pal.Reliable.ReliableUDP
import Data.Binary

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

createReceiver :: forall u r. 
                  (Binary u, Binary r) 
               => String
               -> Either SocketWithDest ConnectedSocket
               -> IO
                  (TChan (Packet   u r),
                   TChan (Outgoing u r),
                   TChan (Outgoing u r))
createReceiver name eitherSocket = do
  verifiedPackets     <- newTChanIO
  outgoingPackets     <- newTChanIO
  incomingRawPackets  <- createReceiverWithChannels name eitherSocket verifiedPackets outgoingPackets
  return
    ( incomingRawPackets  -- Channel to pipe in raw packets from the socket;
    , verifiedPackets     -- Channel to get out sequenced reliable packets and bundled unreliable packets
    , outgoingPackets     -- Channel to write packets to send along
    )

createReceiverWithChannels :: forall u r.  
                             (Binary u, Binary r)
                           => String
                           -> Either SocketWithDest ConnectedSocket
                           -> TChan (Outgoing u r)
                           -> TChan (Outgoing u r)
                           -> IO (TChan (Packet   u r))
createReceiverWithChannels name eitherSocket verifiedPackets outgoingPackets = do
  incomingRawPackets  <- newTChanIO
  
  let conn = newConnection :: Connection u r
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

    forM_ (outgoing :: [Outgoing u r]) $ \case
      Reliable reliablePacket -> sendReliablePacket reliablePacket
      Unreliable unreliableBundle -> do
        bundleNum <- connNextBundleNum <<%= succ
        liftIO $ forM_ unreliableBundle $ \piece ->
          sendUnreliablePacket (UnreliablePacket bundleNum piece :: Packet u r)

    forM_ incoming $ \case
      UnreliablePacket bundleNum payload -> do
        maybeBundle <- collectUnreliablePacket bundleNum payload
        case maybeBundle of
          Nothing -> return ()
          Just bundle -> liftIO . atomically . writeTChan verifiedPackets $ Unreliable bundle
      ReliablePacket seqNum payload ->
        collectReliablePacket seqNum $ liftIO $ do

          _ <- sendUnreliablePacket (ReliablePacketAck seqNum :: Packet u r)
          liftIO . atomically . writeTChan verifiedPackets $ Reliable payload
      ReliablePacketAck seqNum ->
        receiveAck seqNum
  return
    ( incomingRawPackets  -- Channel to pipe in raw packets from the socket;
    )
