import Control.Concurrent

import Network.UDP.Pal

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

main :: IO ()
main = do
  echoServer serverName serverPort 4096
  threadDelaySec 1000000