{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.UDP.Pal.Reliable.Transceiver where

import           Control.Concurrent.STM
import           Network.UDP.Pal.Socket
import           Network.UDP.Pal.Types
import           Network.UDP.Pal.Reliable.Types

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.State
import           Halive.Concurrent
import           Network.UDP.Pal.Reliable.ReliableUDP
import           Network.Socket (getSocketName)
import           Data.Binary
import           Data.Time
import qualified Data.Map as Map
import           Data.Map (Map)

writeTransceiver :: MonadIO m => Transceiver r -> AppPacket r -> m ()
writeTransceiver transceiver = liftIO . atomically . writeTChan (tcOutgoingPackets transceiver)

interpredNetworkPacketsFromOthers :: (MonadIO m) 
                                  => m [(SockAddr, AppPacket a)]
                                  -> (SockAddr -> a -> m ()) 
                                  -> m ()
interpredNetworkPacketsFromOthers getPacketsFromClients intepretFunc = do
  packetsWithOrigin <- getPacketsFromClients
  forM_ packetsWithOrigin $ \(fromAddr, msg) -> case msg of
    Reliable   message -> intepretFunc fromAddr message
    Unreliable messages -> forM_ messages (intepretFunc fromAddr)

interpretNetworkPackets :: MonadIO m => TChan (AppPacket a) -> (a -> m ()) -> m ()
interpretNetworkPackets verifiedPacketsChan intepretFunc = 
  liftIO (atomically (exhaustChan verifiedPacketsChan)) 
    >>= mapM_ (\case
        Reliable message -> intepretFunc message
        Unreliable messages -> forM_ messages intepretFunc
      )

streamInto :: TChan a -> IO a -> IO ThreadId
streamInto channel action =
  forkIO . forever $
    action >>= atomically . writeTChan channel

streamIntoMaybe :: TChan a -> IO (Maybe a) -> IO ThreadId
streamIntoMaybe channel action =
  forkIO . forever $
    action >>= \case
      Just msg -> atomically . writeTChan channel $ msg
      Nothing  -> return ()

exhaustChan :: TChan a -> STM [a]
exhaustChan chan = go mempty
  where
    go !accum = tryReadTChan chan >>= \case
      Just msg -> go (accum ++ [msg])
      Nothing -> return accum

createTransceiverToAddress :: forall r. (Binary r)
                           => HostName
                           -> PortNumber
                           -> Int
                           -> IO (Transceiver r)
createTransceiverToAddress serverName serverPort packetSize = do
  toServerSock <- socketWithDest serverName serverPort packetSize
  transceiver <- createTransceiver "Client" (Left toServerSock) mempty
  -- Stream received packets into the Transceiver's packetsIn channel
  _receiveThread <- streamInto (tcIncomingRawPackets transceiver) 
    (fst <$> receiveFromDecoded (swdBoundSocket toServerSock) :: IO (WirePacket r))

  displayName  <- show <$> getSocketName (bsSocket (swdBoundSocket toServerSock))
  putStrLn $ "*** Launched client: " ++ displayName

  return transceiver

createTransceiver :: forall r. (Binary r)
                  => String
                  -> Either SocketWithDest ConnectedSocket
                  -> Map SeqNum r
                  -> IO (Transceiver r)
createTransceiver _name eitherSocket initialUnacked = do
  incomingRawPackets  <- newTChanIO
  verifiedPackets     <- newTChanIO
  outgoingPackets     <- newTChanIO
  lastMessageTime     <- newTVarIO =<< getCurrentTime
  
  let conn = (newTransceiverState :: TransceiverState r)
                & connUnacked .~ initialUnacked
                & connNextSeqNumTo .~ 
                    if Map.null initialUnacked then 0 else (succ . fst . Map.findMax) initialUnacked
      sendUnreliablePacket  = either sendBinary   sendBinaryConn   eitherSocket

  -- Start a thread to send periodic keepalive messages to the destination
  keepAliveThread  <- forkIO' . forever $ do
    _ <- sendUnreliablePacket KeepAlive
    threadDelay 100000 -- 0.1 sec


  transceiverThread <- forkIO' . void . flip runStateT conn $ do
    purgeReliable eitherSocket
    forever $ do
      --liftIO $ putStrLn $ name ++ " awaiting packet"
      
      -- Whenever we have new incoming or outgoing packets to process, 
      -- grab them all and continue
      (outgoing, incoming) <- liftIO . atomically $ do
        outgoing <- exhaustChan outgoingPackets
        packet   <- exhaustChan incomingRawPackets
        case (outgoing, packet) of
          ([], []) -> retry
          (someOutgoing, someIncoming) -> return (someOutgoing, someIncoming)
      when (not (null incoming)) $ 
        liftIO . atomically . writeTVar lastMessageTime =<< liftIO getCurrentTime
      
      -- Each outgoing packet can be sent as Reliable or Unreliable.
      forM_ (outgoing :: [AppPacket r]) $ \case
        -- Reliable packets are sent again and again until they are acknowledged 
        -- by receiveAck via a ReliablePacketAck message (below, in 'incoming')
        Reliable reliablePacket -> sendReliable eitherSocket reliablePacket 
        -- Unreliable packets are sent in numbered bundles and buffered slightly
        -- on the receiving end so the bundles can be reconstructed.
        Unreliable unreliableBundle -> do
          bundleNum <- connNextBundleNum <<%= succ
          liftIO $ forM_ unreliableBundle $ \piece ->
            sendUnreliablePacket (UnreliablePacket bundleNum piece :: WirePacket r)
      
      -- Each incoming packet can be a piece of an Unreliable bundle,
      -- a Reliable packet, or an acknowledgement of 
      -- one of our own previously-sent Reliable packets.
      forM_ incoming $ \case
        KeepAlive -> return ()
        -- Unreliable bundles are buffered into bundles. Once we receive the
        -- Nth-greater bundleNum than the lowest bundleNum we're buffering,
        -- we pop the lowest bundle off the queue and hand it to the application as-is.
        -- Packets older than the lowest bundleNumber we're buffering are ignored.
        UnreliablePacket bundleNum payload -> do
          maybeBundle <- collectUnreliablePacket bundleNum payload
          case maybeBundle of
            Nothing -> return ()
            Just bundle -> liftIO . atomically . writeTChan verifiedPackets $ Unreliable bundle
        -- Reliable packets are only processed if they match the next sequence number we're waiting for.
        -- We then send back an acknowledgement of them and pass them along to the application.
        ReliablePacket seqNum payload ->
          collectReliablePacket seqNum $ liftIO $ do
            _ <- sendUnreliablePacket (ReliablePacketAck seqNum :: WirePacket r)
            liftIO . atomically . writeTChan verifiedPackets $ Reliable payload
        -- Until we receive a ReliablePacketAck, we'll keep sending the unacknowledged
        -- seqNums along using sendReliablePacket (above, in 'outgoing')
        ReliablePacketAck seqNum ->
          receiveAck seqNum


  return Transceiver 
    { tcIncomingRawPackets = incomingRawPackets -- Channel to pipe in raw packets from the socket
    , tcVerifiedPackets    = verifiedPackets    -- Channel to get out sequenced reliable packets and bundled unreliable packets
    , tcOutgoingPackets    = outgoingPackets    -- Channel to write packets to send along
    , tcLastMessageTime    = lastMessageTime
    , tcShutdown           = mapM_ killThread [keepAliveThread, transceiverThread]
    }


-- Start a watchdog to make sure we are still receiving messages
addWatchdog :: Transceiver r -> IO () -> IO ()
addWatchdog transceiver finalizer = void . forkIO' $ do
  let loop = do
        isExpired <- (>1) <$> (diffUTCTime <$> getCurrentTime <*> atomically (readTVar (tcLastMessageTime transceiver)))
        if isExpired 
          then do
            tcShutdown transceiver
            finalizer
          else do
            threadDelay 1000000
            loop
  loop
