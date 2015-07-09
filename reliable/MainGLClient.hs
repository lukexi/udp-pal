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
[ ] Add keepalive and "quit commands" (i.e. delete this object)
    Transmit these at the start? Or make these configurable.
[ ] Add avatars that travel in a circle at different speeds

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



main :: IO ()
main = do
  killThreads

  -------------------
  -- Networking setup
  -------------------
  transceiver <- createTransceiverToAddress serverName serverPort packetSize
  liftIO . atomically $ writeTChan (tcOutgoingPackets transceiver) (Reliable (ConnectClient "Sup"))

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

  let newWorld = emptyAppState

  ourColor <- liftIO $ V4 <$> randomRIO (0, 1) <*> randomRIO (0, 1) <*> randomRIO (0, 1) <*> pure 1

  void . flip runStateT newWorld . whileWindow window $ do
    -------------------------
    -- Process network events
    -------------------------
    liftIO (atomically (exhaustChan (tcVerifiedPackets transceiver))) >>= mapM_ (\case
        Reliable (CreateObject objID pose color) -> do
          liftIO $ print (CreateObject objID pose color)
          cubePoses . at objID ?= pose
          cubeColors . at objID ?= color
        Reliable (ConnectClient name) -> liftIO $ putStrLn $ "Hello, " ++ name
        Unreliable unrel                   -> forM_ unrel $ \(ObjectPose objID pose) -> do
          --liftIO $ putStrLn $ "Updating pose to: " ++ show (ObjectPose objID pose)
          cubePoses . at objID ?= pose
      )

    --------------------
    -- Process UI events
    --------------------
    processEvents events $ \e -> case e of
      MouseButton _ MouseButtonState'Pressed _ -> do
        newCubeID <- liftIO randomIO
        randomPos <- liftIO $ V3 <$> randomRIO (-1, 1) <*> randomRIO (-1, 1) <*> randomRIO (0, -5)
        let pose = Pose randomPos (axisAngle (V3 0 1 0) 0)
        -- We should use our quasi-free monad thing here.
        cubePoses . at newCubeID ?= pose
        cubeColors . at newCubeID ?= ourColor
        liftIO . atomically $ writeTChan (tcOutgoingPackets transceiver) (Reliable (CreateObject newCubeID pose ourColor))
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
    withVAO ( vAO cube ) $ 
      forM_ ( Map.toList newCubes ) $ \( objID , pose ) -> do

        let model = mkTransformation (pose ^. posOrientation) (pose ^. posPosition)
        color <- fromMaybe (V4 0 1 0 1) <$> use (cubeColors . at objID)

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
