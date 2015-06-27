{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad.State
import           Network.UDP.Pal
import           ReliableUDP
import           Types

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Halive.Concurrent

import           Data.Binary
import           GHC.Generics
import           Network.Socket (SockAddr)
import           Control.Lens
import           Data.Monoid
import           Data.Map (Map)
import qualified Data.Map as Map


type ObjectID = Int
data ObjectOp
  = CreateObject ObjectID
  | NameObject ObjectID String
  deriving (Show, Generic, Binary)
data ObjectPose
  = ObjectPose ObjectID
  deriving (Show, Generic, Binary)

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

packetSize :: Int
packetSize = 4096

launchServer :: IO ThreadId
launchServer = forkIO' $ do
  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  let finallyClose = flip finally (close (bsSocket incomingSocket))

  clients <- newMVar mempty
  let findClient fromAddr = do
        modifyMVar clients $ \currentClients -> do
          case Map.lookup fromAddr currentClients of
            Just client -> return (currentClients, client)
            Nothing     -> do
              client <- newServerToClientThread fromAddr
              return (Map.insert fromAddr client currentClients, client)
      newServerToClientThread fromAddr = do
        toClientSock <- connectedSocketToAddr fromAddr
        
        incomingPackets <- newTChanIO
        outgoingPackets <- newTChanIO
        verifiedPackets <- newTChanIO
        let ackReliablePacket = (sendBinaryConn toClientSock :: Packet ObjectPose ObjectOp -> IO Int)
        unreliableCollector <- createReceiver
          (fst <$> atomically (readTChan incomingPackets))
          outgoingPackets
          (atomically . writeTChan verifiedPackets)
          ackReliablePacket
          (sendReliableConn toClientSock)

        return incomingPackets



  finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket
    
    incomingPackets <- findClient fromAddr

    let packet = decode' newMessage :: Packet ObjectPose ObjectOp
    liftIO . putStrLn $ "Received from: " ++ show fromAddr
      ++ ": " ++ show packet

    atomically $ writeTChan incomingPackets (packet, fromAddr)






createReceiver :: Monoid a 
               => IO (Packet a2 t)
               -> TChan a1
               -> (t -> IO ())
               -> (Packet ObjectPose ObjectOp -> IO a3)
               -> (a1 -> StateT (Connection ObjectPose ObjectOp) IO a)
               -> IO (UnreliableCollector a2)
createReceiver incomingPackets outgoingPackets returnPacket ackReliablePacket sendReliablePacket = do
  let conn = newConnection :: Connection ObjectPose ObjectOp
  unreliableCollector <- makeCollector
  _threadID <- forkIO' . void . flip runStateT conn . forever $ do
    _ <- readChanAll outgoingPackets sendReliablePacket
    liftIO $ putStrLn "awaiting packett"
    packet <- liftIO incomingPackets
    case packet of
      UnreliablePacket bundleNum payload ->
        collectUnreliablePacket unreliableCollector bundleNum payload
      ReliablePacket seqNum payload ->
        collectReliablePacket seqNum $ liftIO $ do
          _ <- ackReliablePacket (ReliablePacketAck seqNum :: Packet ObjectPose ObjectOp)
          liftIO $ returnPacket payload
      ReliablePacketAck seqNum ->
        receiveAck seqNum
  return unreliableCollector


-- | Given the seqNum of a newly-received reliable packet,
-- checkes if the seqNum is the successor of the last one we received.
-- If so, records the new seqNum and calls the given action; otherwise, does nothing.
collectReliablePacket :: MonadState (Connection u r) m => SeqNum -> m () -> m ()
collectReliablePacket seqNum action = do
  nextSeqNumFrom <- use connNextSeqNumFrom
  when (seqNum == nextSeqNumFrom) $ do
    connNextSeqNumFrom %= succ
    action

main :: IO ()
main = do
  killThreads
  _           <- launchServer

  toServerSock <- socketWithDest serverName serverPort packetSize
  displayName  <- show <$> getSocketName (bsSocket (swdBoundSocket toServerSock))
  putStrLn $ "*** Launched client: " ++ displayName

  incomingPackets <- channelize (receiveFromDecoded (swdBoundSocket toServerSock) :: IO (Packet ObjectPose ObjectOp, SockAddr))
  outgoingPackets <- newTChanIO
  verifiedPackets <- newTChanIO
  let ackReliablePacket = (sendBinary toServerSock :: Packet ObjectPose ObjectOp -> IO Int)
  unreliableCollector <- createReceiver
    (fst <$> atomically (readTChan incomingPackets))
    outgoingPackets
    (atomically . writeTChan verifiedPackets)
    ackReliablePacket
    (sendReliable toServerSock)
    

  forever . flip runStateT 0 $ do

    liftIO . atomically $ writeTChan outgoingPackets (NameObject 0 "hello")
    liftIO . print =<< extractBundle unreliableCollector =<< id <<%= succ

    liftIO . atomically $ writeTChan outgoingPackets (NameObject 1 "sailor")
    liftIO . print =<< extractBundle unreliableCollector =<< id <<%= succ

    liftIO . atomically $ writeTChan outgoingPackets (NameObject 2 "!!!")
    liftIO . print =<< extractBundle unreliableCollector =<< id <<%= succ

    liftIO $ threadDelay 1000000




-- | Converts a blocking action into a channel
channelize :: (MonadIO m) => IO a -> m (TChan a)
channelize action = liftIO $ do
  messageChan <- newTChanIO
  
  _threadID <- forkIO . forever $ 
    atomically . writeTChan messageChan =<< action
  -- FIXME associate the threadID with a finalizer to kill the thread when the channel is GCd

  return messageChan

readChanAll :: (MonadIO m, Monoid b) => TChan a -> (a -> m b) -> m b
readChanAll chan action = go mempty 
  where 
    go !accum = liftIO (atomically (tryReadTChan chan)) >>= \case
      Just msg -> do
        r <- action msg
        go (r <> accum)
      Nothing -> return accum