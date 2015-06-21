{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
module UnreliableUDPBundles where

import Data.ByteString (ByteString)
import Control.Lens
import Control.Monad
import           Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent

import GHC.Generics
import Data.Binary
import Data.Monoid
import Data.Maybe
import Network.UDP.Pal.Binary

data UnreliablePacket = UnreliablePacket
  { upkBundleNum :: Int
  , upkPayload :: ByteString
  } deriving (Show, Generic, Binary)


data AppState = AppState
  { _apsNextBundleNum :: Int
  }

makeLenses ''AppState

newAppState :: AppState
newAppState = AppState 0

-- Sending packet bundles

sendUnreliable :: TChan UnreliablePacket -> Int -> ByteString -> IO ()
sendUnreliable chan bundleNum payload = do
  
  let packet = UnreliablePacket bundleNum payload

  atomically $ writeTChan chan packet

-- Receiving packet bundles

-- | Continuously reads from the given channel and merges
-- each received packet into a collection MVar
createReader :: TChan UnreliablePacket -> IO (MVar (Map Int [ByteString]))
createReader chan = do
  collection <- newMVar Map.empty
  _ <- forkIO . forever $ do
    UnreliablePacket{..} <- atomically (readTChan chan)
    -- putStrLn $ "Inserting " ++ show upkBundleNum
    modifyMVar collection $ \c -> 
      return (Map.insertWith (<>) upkBundleNum [upkPayload] c, ())

  return collection

-- | Returns a list of all packets received with the given bundle number,
-- and discards any older messages than that
receiveBundle :: Ord k => MVar (Map k [t]) -> k -> IO [t]
receiveBundle collection bundleNum = do
  modifyMVar collection $ \c -> do
    let (_lower, result, higher) = Map.splitLookup bundleNum c
    return (higher, fromMaybe [] result)

incrBundleNum :: StateT AppState IO Int
incrBundleNum = apsNextBundleNum <<+= 1

main :: IO ()
main = do

  chan <- newTChanIO
  collection <- createReader chan
  void . flip runStateT newAppState $ do
    
    bundleNum <- incrBundleNum
    let bundle = Map.fromList [(x,y) | x <- [0..20], y <- [0..20]] :: Map Int Int
    forM_ (Map.toList bundle) $ \(x,y) -> 
      liftIO $ sendUnreliable chan bundleNum (encode' (x,y))
    
    -- Give the messages time to reach the collector
    liftIO $ threadDelay 100000

    -- Receive one frame of data
    results <- liftIO $ Map.fromList . map decode' <$> receiveBundle collection bundleNum
    liftIO $ print (results :: Map Int Int)
