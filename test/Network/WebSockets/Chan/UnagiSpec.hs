{-# LANGUAGE OverloadedStrings #-}

module Network.WebSockets.Chan.UnagiSpec where

import           Control.Concurrent             (threadDelay)
import qualified Control.Concurrent.Chan.Unagi  as Unagi
import           Control.Monad                  (forM, forM_, forever)
import           Data.Text                      (Text)
import           Network.HTTP.Types.Status      (status400)
import           Network.Wai                    (Application, responseLBS)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WSH
import           Network.WebSockets             (ServerApp)
import qualified Network.WebSockets             as WS
import           Network.WebSockets.Chan.Unagi
import           Test.QuickCheck                (arbitrary, generate)
import           Test.QuickCheck.Instances      ()

app :: Application
app = WSH.websocketsOr WS.defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      conn <- WS.acceptRequest pending_conn
      forever $ do
        msg <- WS.receiveData conn :: IO Text
        WS.sendBinaryData conn msg
    backupApp :: Application
    backupApp _ respond =
      respond $ responseLBS status400 [] "Not a WebSocket request"

spec = Warp.withApplication (return app) runSpec

-- add some timing and some specific size to the generator
runSpec p = do
  putStrLn $ "listening on " ++ show p
  (ic, oc, cic) <-
    newChans "localhost" p "" :: IO (Unagi.InChan Text, Unagi.OutChan Text, Unagi.InChan Text)
  msgs <- generate arbitrary :: IO [Text]
  forM_ msgs $ Unagi.writeChan ic
  res <- forM msgs (\_ -> Unagi.readChan oc)
  putStrLn $ "got" ++ show (res == msgs) -- do some proper assertion here
  Unagi.writeChan cic ("finished" :: Text)
  return ()
