import           Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import System.Random
import Control.Applicative
import Data.List
import Control.Monad
import Control.Monad.Trans
import Text.Show.Pretty

{-

Generate a random frame to simulate jitter,
and choose random bits of its contents to simulate packet loss.

Make sure we reassemble a best-effort frame on the other side.

Introduce a reliable system too for data that we must have,
where the remote end acknowledges the last message it saw.

-}

pprint :: (MonadIO m, Show a) => a -> m ()
pprint = liftIO . putStrLn . ppShow

createFrame = do
  frameNumber <- randomRIO (0,1000::Int)

  positions <- replicateM 10 $ do
    (,) <$> randomRIO (0,99999 :: Int)
        <*> ((,,) <$> randomRIO (0,10::Int)
                  <*> randomRIO (0,10::Int)
                  <*> randomRIO (0,10::Int))
  pprint (frameNumber, positions)
  return (frameNumber, positions)

main = do
  let frameQueue0 = mempty
  (frameNumber0, positions0) <- createFrame

  let frameQueue1 = Map.insert frameNumber0 positions0 frameQueue0

  (frameNumber1, positions1) <- createFrame

  let frameQueue2 = Map.insert frameNumber1 positions1 frameQueue1

  pprint $ frameQueue2