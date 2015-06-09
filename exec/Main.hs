import Network.UDP.Pal
import Network.UDP.Pal.Util

main :: IO ()
main = do
  putStrLn "***Launching Server..."
  launchServer
  threadDelaySec 1
  putStrLn "***Launching client 1..."
  _client1 <- launchClient

  putStrLn "***Waiting 5 seconds..."
  threadDelaySec 5

  -- putStrLn "***Killing Client..."
  -- killThread _client1

  threadDelaySec 1
  putStrLn "***Launching client 2..."
  _client2 <- launchClient

  threadDelaySec 1
  putStrLn "***Done."


