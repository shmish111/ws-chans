{-# LANGUAGE OverloadedStrings #-}

module Network.WebSockets.Chan.UnagiSpec where

import           Control.Monad                        (forM, forever)
import           Data.Text                            (Text)
import           Network.HTTP.Types.Status            (status400)
import           Network.Wai                          (Application, responseLBS)
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WebSockets       as WSH
import           Network.WebSockets                   (ServerApp)
import qualified Network.WebSockets                   as WS
import           Network.WebSockets.Chan.Unagi        as Unagi
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Property)
import           Test.QuickCheck.Instances            ()
import           Test.QuickCheck.Monadic              (assert, monadicIO, run)

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

sendAndReceiveAction msgs =
  Warp.withApplication
    (return app)
    (\p -> do
       (ic, oc, cic) <-
         Unagi.newChans "localhost" p "" :: IO (Unagi.InChan Text, Unagi.OutChan Text, Unagi.InChan Text)
       Unagi.writeList2Chan ic msgs
       res <- forM msgs (\_ -> Unagi.readChan oc)
       Unagi.writeChan cic ("finished" :: Text)
       return res)

sendAndReceiveProps :: [Text] -> Property
sendAndReceiveProps msgs =
  monadicIO $ do
    run $ sendAndReceiveAction msgs
    assert True

tests = [testProperty "send and receive some text" sendAndReceiveProps]

