module Network.WebSockets.Chan.Unagi where

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.Async      (Concurrently (..),
                                                runConcurrently)
import qualified Control.Concurrent.Async      as A
import           Control.Concurrent.Chan.Unagi (InChan, OutChan)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import           Control.Exception             (SomeException (..), catch,
                                                finally, fromException, throw)
import           Control.Monad                 (forever)
import qualified Network.Socket                as S
import qualified Network.WebSockets            as WS

type WSChan a b = (InChan a, OutChan b, InChan a)

newChans
  :: (WS.WebSocketsData a, WS.WebSocketsData b)
  => String -> Int -> String -> IO (WSChan a b)
newChans host port path = do
  (ic, sc) <- Unagi.newChan
  (rc, oc) <- Unagi.newChan
  (cic, coc) <- Unagi.newChan
  _ <-
    A.async $
    runRetryClientWith 10 host port path WS.defaultConnectionOptions [] $
    clientApp rc sc coc
  return (ic, oc, cic)

receiveData
  :: (WS.WebSocketsData b)
  => WS.Connection -> InChan b -> IO ()
receiveData conn ic =
  forever $ do
    msg <- WS.receiveData conn
    Unagi.writeChan ic msg

sendData
  :: (WS.WebSocketsData a)
  => WS.Connection -> OutChan a -> IO ()
sendData conn oc =
  forever $ do
    msg <- Unagi.readChan oc
    WS.sendBinaryData conn msg

sendClose
  :: (WS.WebSocketsData a)
  => WS.Connection -> OutChan a -> IO ()
sendClose conn coc = do
  msg <- Unagi.readChan coc
  putStrLn "closing client"
  WS.sendClose conn msg

clientApp
  :: (WS.WebSocketsData a, WS.WebSocketsData b)
  => InChan b -> OutChan a -> OutChan a -> WS.Connection -> IO ()
clientApp ic oc coc conn = do
  res <-
    runConcurrently $
    (,,) <$> Concurrently (receiveData conn ic) <*>
    Concurrently (sendData conn oc) <*>
    Concurrently (sendClose conn coc)
  print res
  return ()

runRetryClientWith
  :: Int
  -> String -- ^ Host
  -> Int -- ^ Port
  -> String -- ^ Path
  -> WS.ConnectionOptions -- ^ Options
  -> WS.Headers -- ^ Custom headers to send
  -> WS.ClientApp a -- ^ Client application
  -> IO a
runRetryClientWith retries host port path opts customHeaders app = do
  let hints =
        S.defaultHints {S.addrFamily = S.AF_INET, S.addrSocketType = S.Stream}
        -- Correct host and path.
      fullHost =
        if port == 80
          then host
          else host ++ ":" ++ show port
      path' =
        if null path
          then "/"
          else path
  addrInfos <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.setSocketOption sock S.NoDelay 1
    -- Connect WebSocket and run client
  finally
    ((S.connect sock (S.addrAddress $ head addrInfos) >>
      WS.runClientWithSocket sock fullHost path' opts customHeaders app) `catch`
     handler)
    (S.close sock)
  where
    handler e = do
      print e
      case fe e of
        (Just (WS.CloseRequest _ _)) -> throw e
        _ ->
          if retries > 0
            then runRetryClientWith
                   (retries - 1)
                   host
                   port
                   path
                   opts
                   customHeaders
                   app
            else throw e
    fe :: SomeException -> Maybe WS.ConnectionException
    fe = fromException
