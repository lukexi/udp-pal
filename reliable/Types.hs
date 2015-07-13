{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleContexts          #-}
module Types where
import GHC.Generics (Generic)
import           Graphics.GL
import           Linear
import           Data.Binary

import           Control.Lens
import           Control.Monad.State
import           Data.Map (Map)
import           Network.UDP.Pal
import           System.Random

data Pose = Pose
  { _posPosition    :: V3 GLfloat
  , _posOrientation :: Quaternion GLfloat 
  } deriving (Generic, Binary, Show)
makeLenses ''Pose

type Color = V4 GLfloat

type ObjectID = Int
type PlayerID = String

data ObjectOp
  = CreateObject ObjectID Pose Color
  | ConnectClient PlayerID Pose Color
  | DisconnectClient PlayerID
  deriving (Show, Generic)
instance Binary ObjectOp

data ObjectPose
  = ObjectPose ObjectID Pose
  | PlayerPose PlayerID Pose
  deriving (Show, Generic)
instance Binary ObjectPose


data AppState = AppState 
  { _cubePoses    :: Map ObjectID Pose
  , _cubeColors   :: Map ObjectID Color
  , _playerPoses  :: Map PlayerID Pose
  , _playerColors :: Map PlayerID Color
  , _playerIDs    :: Map SockAddr PlayerID -- Only needed on server
  } deriving (Show)
makeLenses ''AppState

emptyAppState :: AppState
emptyAppState = AppState mempty mempty mempty mempty mempty

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

packetSize :: Int
packetSize = 4096

randomName :: IO String
randomName = concat <$> replicateM 3 randomPair
  where
    randomPair = (\(x,y) -> [x,y]) . (pairs !!) <$> randomRIO (0, length pairs - 1)
    pairs = zip "bcdfghjklmnpqrstvwxz" (cycle "aeiouy")

randomColor :: MonadIO m => m (V4 GLfloat)
randomColor = liftIO $ V4 <$> randomRIO (0, 1) <*> randomRIO (0, 1) <*> randomRIO (0, 1) <*> pure 1

interpretReliable :: (MonadIO m, MonadState AppState m) => ObjectOp -> m ()
interpretReliable (CreateObject objID pose color) = do
  liftIO $ print (CreateObject objID pose color)
  cubePoses . at objID ?= pose
  cubeColors . at objID ?= color
interpretReliable (ConnectClient playerID pose color) = do
  liftIO $ putStrLn $ "Hello, " ++ playerID
  playerPoses  . at playerID ?= pose
  playerColors . at playerID ?= color

interpretReliable (DisconnectClient playerID) = do
  playerPoses  . at playerID .= Nothing
  playerColors . at playerID .= Nothing
  liftIO $ putStrLn $ "Goodbye, " ++ playerID

interpretUnreliable :: (MonadIO m, MonadState AppState m) => ObjectPose -> m ()
interpretUnreliable (ObjectPose objID pose) = do
  --liftIO $ putStrLn $ "Updating pose to: " ++ show (ObjectPose objID pose)
  -- We use traverse here to only set a new value if there's already one there,
  -- to keep unreliable messages from affecting state out of order.
  cubePoses . at objID . traverse .= pose
interpretUnreliable (PlayerPose objID pose) = do
  --liftIO $ putStrLn $ "Updating pose to: " ++ show (ObjectPose objID pose)
  playerPoses . at objID . traverse .= pose
  --liftIO . print =<< use playerPoses