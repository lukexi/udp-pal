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
-- import           Network.Socket (SockAddr)
import           Control.Lens
-- import           Data.Map (Map)
import qualified Data.Map as Map
-- import System.Mem.Weak

{-

[x] Use a small sliding window for unreliable messages,
    e.g. [0,1,2,3,4] are all being assembled, and when we see 5, kick out 0 to always keep 5 values in the buffer.
    Maybe when we kick it out, we should place it in the verifiedPackets queue, rather than exposing the collector?
    Then we can keep all receiver state in the Connection type.
    (and we don't need the collector in an MVar)

[x] If we use a list for unreliable packets we can send them into the receiver's outgoing channel
    and add our own bundleID, splitting them up into multiple messages inside the receiver.

[x] Have outgoingPackets use Reliable | Unreliable sum type

Also, we should have a way to bypass the collector on the server side and just broadcast
certain unreliable messages straight out - specifically pose data from players - to avoid
adding any latency to them.
(consider how this will interact with physics updates, though; 
it may make things look unsynchronized which might be worse than latency)

Also add keepalive checks for kicking out clients while we're here.

-}

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
        createReceiver "Server" (Right toClientSock)



  finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket
    
    (incomingRawPackets, verifiedPackets, outgoingPackets) <- findClient fromAddr

    let packet = decode' newMessage :: Packet ObjectPose ObjectOp
    atomically $ writeTChan incomingRawPackets packet

    liftIO . putStrLn $ "Received from: " ++ show fromAddr
                        ++ ": " ++ show packet


    print =<< atomically (exhaustChan verifiedPackets)
    -- atomically $ writeTChan outgoingPackets (Reliable (NameObject 42 "Oh hey!"))

data Outgoing u r = Unreliable [u] | Reliable r deriving Show

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
  
  (incomingRawPackets, verifiedPackets, outgoingPackets) <- createReceiver "Client" (Left toServerSock)

  -- Stream received packets into the Receiver's packetsIn channel
  streamInto incomingRawPackets (fst <$> receiveFromDecoded (swdBoundSocket toServerSock) :: IO (Packet ObjectPose ObjectOp))

  forever $ do

    liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 0 "hello"))
    liftIO $ print =<< atomically (exhaustChan verifiedPackets)

    liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 1 "sailor"))
    liftIO $ print =<< atomically (exhaustChan verifiedPackets)

    liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 2 "!!!"))
    liftIO $ print =<< atomically (exhaustChan verifiedPackets)

    liftIO $ threadDelay 1000000

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