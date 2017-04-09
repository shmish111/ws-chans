# ws-chans

Websockets represent a channel between a client and a server. `ws-chans` carries this concept deeper into your code by setting up an `Control.Concurrent.Chan.Unagi.InChan` and an `Control.Concurrent.Chan.Unagi.OutChan` as an interface to a websocket server. To send a message to the server you simply write a message to the `InChan`. To receive a message from the server you read from the `OutChan`.

The tests are probably the best place to look at some example usage but basically:

```haskell
import           Control.Monad                        (forM, forever)
import           Data.Text                            (Text)
import           Network.WebSockets.Chan.Unagi        as Unagi

example :: IO [Text]
example = do
    (ic, oc, cic) <- Unagi.newChans "localhost" 8080 "" :: IO (Unagi.InChan Text, Unagi.OutChan Text, Unagi.InChan Text)
    Unagi.writeList2Chan ic msgs
    res <- forM msgs (\_ -> Unagi.readChan oc)
    Unagi.writeChan cic ("finished" :: Text)
    return res
```

`newChans` returns a tuple of:
* an `InChan` which you write messages to, these will be sent to the websocket server
* an `OutChan` which you read messages from, these are messages that have come from the websocket server
* an `InChan` for closing the connection. This should have the same type as the first `InChan`. When you write a message to this `InChan` it will tell the server that you wish to close the connection. See the source code and [Network.WebSockets.sendClose](https://hackage.haskell.org/package/websockets-0.10.0.0/docs/Network-WebSockets.html#v:sendClose) for more information on how this works.