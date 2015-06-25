import           Network.UDP.Pal
import           Shared

main :: IO ()
main = do
  echoServer serverName serverPort 4096
  threadDelaySec 1000000
