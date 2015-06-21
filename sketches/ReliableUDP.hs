{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}
module ReliableUDP where

import Data.ByteString (ByteString)

import GHC.Generics
import Data.Binary
import Control.Monad.State
import Control.Lens

import           Data.Map (Map)
import qualified Data.Map as Map
import Network.UDP.Pal.Binary

data ReliablePacket = ReliablePacket 
  { rpkSeqNum  :: Int
  , rpkPayload :: ByteString 
  } deriving (Show, Generic, Binary)

data Connection = Connection
  { _connNextSeqNum :: Int
  , _connUnacked    :: Map Int ReliablePacket
  }
makeLenses ''Connection

newConnection :: Connection
newConnection = Connection 0 mempty

sendReliable :: (MonadIO m, MonadState Connection m) => ByteString -> m ()
sendReliable payload = do
  seqNum <- connNextSeqNum <<+= 1
  let packet = ReliablePacket seqNum payload
  connUnacked . at seqNum ?= packet

  unacked <- use connUnacked

  forM_ unacked $ \aPacket -> do
    liftIO . putStrLn $ "Sending " ++ show aPacket
  liftIO $ putStrLn ""

-- | Discard all packets less than the acknowledged sequence number
-- Server should send acks with every message to the client to ensure redundancy.
receiveAck :: MonadState Connection m => Int -> m ()
receiveAck ack = do
  unacked <- use connUnacked
  let (_smaller, larger) = Map.split ack unacked
  connUnacked .= larger

main :: IO ()
main = do

  void . flip runStateT newConnection $ do
    
    sendReliable (encode' "hello"  )
    receiveAck 0
    sendReliable (encode' "sailor" )
    receiveAck 1
    sendReliable (encode' "!!!"    )
