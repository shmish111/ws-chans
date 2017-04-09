module Main where

import Network.WebSockets.Chan.UnagiSpec as US

main :: IO ()
main = do
    US.spec
    putStrLn "Test suite not yet implemented"
