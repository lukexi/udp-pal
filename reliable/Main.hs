{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import ReliableUDP
import Network.UDP.Pal
import Control.Monad.State
import Types

import Control.Exception
import qualified Data.Map as Map
import Data.Binary
import Halive.Concurrent
import Control.Concurrent
serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

packetSize :: Int
packetSize = 4096

launchServer :: IO ThreadId
launchServer = forkIO' $ do
  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  flip finally (close (bsSocket incomingSocket)) . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket
    toClientSock <- connectedSocketToAddr fromAddr

    let packet = decode' newMessage :: Packet String String
    putStrLn $ "Received from: " ++ show fromAddr 
      ++ ": " ++ show packet

    case packet of
      ReliablePacket seqNum _payload -> do
        putStrLn $ "Acknowledging: " ++ show seqNum
        sendBinaryConn toClientSock (ReliablePacketAck seqNum :: Packet String String)
      _ -> error "Unrecognized"
    

sendReliable :: forall a b m. (Binary a, Binary b, MonadIO m, MonadState (Connection a b) m) 
             => SocketWithDest -> b -> m ()
sendReliable client message = do
  reliablePackets <- queueReliable message
  forM_ (Map.toList reliablePackets) $ \(seqNum, payload) ->
    sendEncoded client ((ReliablePacket seqNum payload) :: Packet a b)

main :: IO ()
main = do
  killThreads
  _ <- launchServer

  client <- socketWithDest serverName serverPort packetSize
  displayName <- show <$> getSocketName (bsSocket (swdBoundSocket client))
  putStrLn $ "*** Launched client: " ++ displayName

  let conn = newConnection :: Connection String String
  void . flip runStateT conn $ do
    
    sendReliable client "hello"

    (ack, _) <- receiveFromDecoded (swdBoundSocket client)
    let _ = ack :: Packet String String
    case ack of
      ReliablePacket _ _ -> liftIO $ print "reliable"
      UnreliablePacket _ _ -> liftIO $ print "unreliable"
      ReliablePacketAck seqNum -> do
        liftIO $ print seqNum
        receiveAck seqNum
    
    sendReliable client "sailor"

    (ack2, _) <- receiveFromDecoded (swdBoundSocket client)
    let _ = ack2 :: Packet String String
    case ack2 of
      ReliablePacket _ _ -> liftIO $ print "reliable"
      UnreliablePacket _ _ -> liftIO $ print "unreliable"
      ReliablePacketAck seqNum -> do
        liftIO $ print seqNum
        receiveAck seqNum
    
    sendReliable client "!!!"

    (ack3, _) <- receiveFromDecoded (swdBoundSocket client)
    let _ = ack3 :: Packet String String
    case ack3 of
      ReliablePacket _ _ -> liftIO $ print "reliable"
      UnreliablePacket _ _ -> liftIO $ print "unreliable"
      ReliablePacketAck seqNum -> do
        liftIO $ print seqNum
        receiveAck seqNum