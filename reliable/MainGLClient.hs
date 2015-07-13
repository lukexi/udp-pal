{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DeriveDataTypeable        #-}

import           Control.Monad.State
import           Network.UDP.Pal


import           Control.Concurrent.STM
import           Halive.Concurrent
--import           Control.Lens hiding (view)
import           Graphics.UI.GLFW.Pal
import           Linear

import System.Random

import Types
import Render

{-
SERVER LOGIC:
[x] Reliable messages from clients (like 'CreateCube') should be broadcasted  (with a new seqNum) to all except the sender
    Unreliable messages from clients (like 'ObjectPose') should be broadcasted to all except the sender
    Server unreliable sim messages should be broadcasted to all clients
      clients should be ready for updates to non-existent objects and ignore them

[ ] Let's try starting with having the server expire cubes since it has to anyway.
    Can add having the client expire them too (locally, no network message) later.

Client sends hand and head updates unreliably to the server.
Server receives all these and broadcasts every frame to the all clients.
Client sends reliable "create cube" updates to the server.
Server re-broadcasts these to clients, and also starts physics sim
to send unreliable updates and expiration timer to send reliable "DeleteCube" message.

-}



main :: IO ()
main = do
  killThreads

  -------------------
  -- Networking setup
  -------------------
  ourName <- randomName
  ourPose  <- liftIO $ Pose
    <$> (V3 <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) <*> randomRIO (0, -5))
    <*> pure (axisAngle (V3 0 1 0) 0)
  
  ourColor <- randomColor
  let ourConnectMessage = ConnectClient ourName ourPose ourColor

  _transceiver@Transceiver{..} <- createTransceiverToAddress serverName serverPort packetSize

  -- Initial connection to the server
  liftIO . atomically . writeTChan tcOutgoingPackets $ Reliable ourConnectMessage

  ------------------
  -- GL/Window setup
  ------------------
  (window, events, cube) <- initRenderer

  newWorld <- execStateT (interpretReliable ourConnectMessage) emptyAppState

  void . flip runStateT newWorld . whileWindow window $ do
    -------------------------
    -- Process network events
    -------------------------
    liftIO (atomically (exhaustChan tcVerifiedPackets)) >>= mapM_ (\case
        Reliable message -> interpretReliable message
        Unreliable unrel -> forM_ unrel interpretReliable
      )

    --------------------
    -- Process UI events
    --------------------
    processEvents events $ \e -> do
      closeOnEscape window e
      case e of
        MouseButton _ MouseButtonState'Pressed _ -> do

          -- Add a cube at a random location
          newCubeID <- liftIO randomIO
          randomPos <- liftIO $ V3 <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) <*> randomRIO (0, -5)
          let pose = Pose randomPos (axisAngle (V3 0 1 0) 0)
              message = CreateObject newCubeID pose ourColor

          interpretReliable message
          liftIO . atomically . writeTChan tcOutgoingPackets $ Reliable message

        _ -> return ()

    ---------
    -- Render
    ---------
    renderFrame window cube

