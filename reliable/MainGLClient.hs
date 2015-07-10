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
import qualified Data.Map               as Map
--import           Data.Map               (Map)
import           Halive.Concurrent
import           Control.Lens hiding (view)
import           Graphics.UI.GLFW.Pal
import           Graphics.GL.Pal2
import           Graphics.GL
import           Linear

import Data.Maybe
import System.Random
import Data.Data

import Types

{-

NEXT UP:
[x] Add cube coloring
[x] Support multiple clients
[x] Choose random color for each client, add to cube message
[x] Make sure cubes make it across correctly
[x] Add keepalive 
[x] Add "quit commands" (i.e. delete this object)
    Transmit these at the start? Or make these configurable.
[x] Add avatars that travel in a circle at different speeds

[ ] Add tweak to the Transceiver that allows passing in an initial _connUnacked value
    and have the server start all new clients with a full historically-accumulated connUnacked.
    (be sure to handle the copying of the history with the cloning of the broadcastChan in a single transaction!)

-}

{-
SERVER LOGIC:
Reliable messages from clients (like 'CreateCube') should be broadcasted  (with a new seqNum) to all except the sender
Unreliable messages from clients (like 'ObjectPose') should be broadcasted to all except the sender
Server unreliable sim messages should be broadcasted to all clients
  clients should be ready for updates to non-existent objects and ignore them

Let's try starting with having the server expire cubes since it has to anyway.
Can add having the client expire them too (locally, no network message) later.

Try making the above configurable for the server setup we've got 
(i.e. create a server configured to handle messages in the above way)

-}

{-

Client sends hand and head updates unreliably to the server.
Server receives all these and broadcasts every frame to the all clients.
Client sends reliable "create cube" updates to the server.
Server re-broadcasts these to clients, and also starts physics sim
to send unreliable updates and expiration timer to send reliable "DeleteCube" message.

-}



resX, resY :: Int
resX=1024; resY=768



randomName :: IO String
randomName = concat <$> replicateM 3 randomPair
  where
    randomPair = (\(x,y) -> [x,y]) . (pairs !!) <$> randomRIO (0, length pairs - 1)
    pairs = zip "bcdfghjklmnpqrstvwxz" (cycle "aeiouy")

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
    
  ourColor <- liftIO $ V4 <$> randomRIO (0, 1) <*> randomRIO (0, 1) <*> randomRIO (0, 1) <*> pure 1
  let ourConnectMessage = ConnectClient ourName ourPose ourColor

  _transceiver@Transceiver{..} <- createTransceiverToAddress serverName serverPort packetSize
  liftIO . atomically . writeTChan tcOutgoingPackets $ Reliable ourConnectMessage

  ------------------
  -- GL/Window setup
  ------------------
  (window, events) <- createWindow "ReliableCubes" resX resY

  cubeProg   <- createShaderProgram "reliable/poly.vert" "reliable/poly.frag"
  cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )
  cube       <- entity cubeGeo cubeProg 

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE
  glCullFace GL_BACK

  useProgram (program cube)

  newWorld <- execStateT (interpretReliable ourConnectMessage) emptyAppState

  void . flip runStateT newWorld . whileWindow window $ do
    -------------------------
    -- Process network events
    -------------------------
    liftIO (atomically (exhaustChan tcVerifiedPackets)) >>= mapM_ (\case
        Reliable message -> interpretReliable message
        Unreliable unrel -> forM_ unrel interpretUnreliable
      )

    --------------------
    -- Process UI events
    --------------------
    processEvents events $ \e -> case e of
      MouseButton _ MouseButtonState'Pressed _ -> do
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
    let Uniforms{..} = uniforms cube
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
    projection  <- makeProjection window
    let playerPos = V3 0 0 5
        playerOrient = axisAngle (V3 0 1 0) 0
        view = viewMatrix playerPos playerOrient
        projectionView = projection !*! view
    uniformV3 uCamLocation playerPos

    newCubes <- use cubePoses
    newPlayers <- use playerPoses
    withVAO ( vAO cube ) $ do
      forM_ ( Map.toList newCubes ) $ \( objID , pose ) -> do

        let model = mkTransformation (pose ^. posOrientation) (pose ^. posPosition)
        color <- fromMaybe (V4 0 1 0 1) <$> use (cubeColors . at objID)

        drawEntity cube model projectionView color
      forM_ ( Map.toList newPlayers ) $ \( playerID , pose ) -> do

        let model = mkTransformation (pose ^. posOrientation) (pose ^. posPosition)
        color <- fromMaybe (V4 0 1 0 1) <$> use (playerColors . at playerID)

        drawEntity cube model projectionView color

    swapBuffers window

drawEntity :: MonadIO m => Entity Uniforms -> M44 GLfloat -> M44 GLfloat -> Color -> m ()
drawEntity anEntity model projectionView color = do 

  let Uniforms{..} = uniforms anEntity

  uniformM44 uViewProjection projectionView
  uniformM44 uInverseModel (fromMaybe model (inv44 model))
  uniformM44 uModel model
  uniformV4 uDiffuse color

  let vc = vertCount ( geometry anEntity ) 
  glDrawElements GL_TRIANGLES ( vc ) GL_UNSIGNED_INT nullPtr

-- | Get a view matrix for a camera at a given position and orientation
viewMatrix :: (RealFloat a, Conjugate a) => V3 a -> Quaternion a -> M44 a
viewMatrix position orientation = mkTransformation q (rotate q . negate $ position)
    where q = conjugate orientation

-- | Use the aspect ratio from the window to get a proper projection
makeProjection :: (Floating a, MonadIO m) => Window -> m (M44 a)
makeProjection win = do
    (w,h) <- getWindowSize win
    return $ perspective 45 (fromIntegral w / fromIntegral h) 0.01 100


data Uniforms = Uniforms
  { uViewProjection :: UniformLocation (M44 GLfloat)
  , uInverseModel   :: UniformLocation (M44 GLfloat)
  , uModel          :: UniformLocation (M44 GLfloat)
  , uCamLocation    :: UniformLocation (V3 GLfloat)
  , uDiffuse        :: UniformLocation (V4 GLfloat)
  } deriving (Data, Typeable)
