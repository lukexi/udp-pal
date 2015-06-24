{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
import ReliableUDP
import Network.UDP.Pal
import Control.Monad.State
import Types

import Control.Exception
import Halive.Concurrent
import Control.Concurrent

import GHC.Generics
import Data.Binary
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
      conn = newConnection :: Connection ObjectPose ObjectOp

  finallyClose . void . flip runStateT conn . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket
    toClientSock <- connectedSocketToAddr fromAddr

    let packet = decode' newMessage :: Packet ObjectPose ObjectOp
    liftIO . putStrLn $ "Received from: " ++ show fromAddr 
      ++ ": " ++ show packet

    case packet of
      UnreliablePacket bundleNum _payload -> do
        liftIO $ print "unreliable"
      ReliablePacket seqNum _payload -> do
        liftIO . putStrLn $ "Acknowledging: " ++ show seqNum
        _ <- sendBinaryConn toClientSock (ReliablePacketAck seqNum :: Packet ObjectPose ObjectOp)
        return ()
      ReliablePacketAck seqNum -> 
        receiveAck seqNum



main :: IO ()
main = do
  killThreads
  _ <- launchServer

  client <- socketWithDest serverName serverPort packetSize
  displayName <- show <$> getSocketName (bsSocket (swdBoundSocket client))
  putStrLn $ "*** Launched client: " ++ displayName

  unreliableCollector <- makeCollector

  let receivePacket = do
        (ack, _) <- receiveFromDecoded (swdBoundSocket client)
        let _ = ack :: Packet ObjectPose ObjectOp
        case ack of
          UnreliablePacket bundleNum payload -> do
            collectUnreliablePacket unreliableCollector bundleNum payload
            liftIO $ putStrLn "collecting unreliable"
          ReliablePacket seqNum _payload -> do
            _ <- sendEncoded client (ReliablePacketAck seqNum :: Packet ObjectPose ObjectOp)
            return ()
          ReliablePacketAck seqNum -> 
            receiveAck seqNum

  let conn = newConnection :: Connection ObjectPose ObjectOp
  void . flip runStateT conn $ do
    
    sendReliable client (NameObject 0 "hello")

    receivePacket
    
    sendReliable client (NameObject 1 "sailor")

    receivePacket
    
    sendReliable client (NameObject 2 "!!!")

    receivePacket