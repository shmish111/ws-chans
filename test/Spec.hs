module Main where

import           Network.WebSockets.Chan.UnagiSpec as US
import           Test.Framework                    (defaultMain)

main :: IO ()
main = defaultMain US.tests
