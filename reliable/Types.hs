{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE TemplateHaskell           #-}
module Types where
import GHC.Generics (Generic)
import           Graphics.GL
import           Linear
import Data.Binary

import Control.Lens
import Data.Map (Map)
import           Network.UDP.Pal

data Pose = Pose
  { _posPosition    :: V3 GLfloat
  , _posOrientation :: Quaternion GLfloat 
  } deriving (Generic, Binary, Show)
makeLenses ''Pose

type Color = V4 GLfloat

type ObjectID = Int

data ObjectOp
  = CreateObject ObjectID Pose Color
  | ConnectClient String
  | DisconnectClient String
  deriving (Show, Generic)
instance Binary ObjectOp
data ObjectPose
  = ObjectPose ObjectID Pose
  deriving (Show, Generic)
instance Binary ObjectPose


data AppState = AppState 
  { _cubePoses  :: Map ObjectID Pose
  , _cubeColors :: Map ObjectID Color
  } deriving (Show)
makeLenses ''AppState

emptyAppState :: AppState
emptyAppState = AppState mempty mempty

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

packetSize :: Int
packetSize = 4096