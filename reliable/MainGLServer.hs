{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

import           Linear
import Control.Concurrent.STM
import qualified Data.Map               as Map
import           Control.Concurrent

import           Network.UDP.Pal
import           Control.Monad.State
import           Control.Lens
import Network.Socket

import Types

main :: IO ()
main = do

  broadcastChan <- createServer serverName serverPort packetSize
  ourAddr <- addrAddress <$> addressInfo (Just serverName) (Just (show serverPort))
  packetsFromClients <- atomically $ dupTChan broadcastChan
  let getPacketsFromClients = map snd . filter ((/= ourAddr) . fst) <$> liftIO (atomically (exhaustChan packetsFromClients))
      broadcastToClients    = liftIO . atomically . writeTChan broadcastChan . (ourAddr,)

  -- Launch the server's main thread to run physics simulations
  
  void . flip runStateT emptyAppState . forever $ do
    -- Process new packets
    packets <- getPacketsFromClients
    forM_ packets $ \case
      Reliable   (CreateObject newCubeID pose color) -> do
        cubePoses  . at newCubeID ?= pose
        cubeColors . at newCubeID ?= color
      Reliable _ -> return ()
      Unreliable _poses -> do
        return ()

    -- Run physics
    cubePoses . traversed . posOrientation *= axisAngle (V3 0 1 0) 0.1
    cubes <- use $ cubePoses . to Map.toList
    let poses = map (\(cubeID, pose) -> ObjectPose cubeID pose) cubes
    --liftIO . putStrLn $ "Sending: " ++ show poses
    broadcastToClients (Unreliable poses)

    liftIO $ threadDelay (1000000 `div` 60)
