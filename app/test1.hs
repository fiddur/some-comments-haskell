{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
import Yesod
import Data.Aeson
import Database.EventStore
import Control.Concurrent.Async

data HelloWorld = HelloWorld
  { eventstore :: Connection }

mkYesod "HelloWorld" [parseRoutes|
/events EventsR GET
|]

instance Yesod HelloWorld

getEventsR :: Handler Value
getEventsR = returnJson $ liftIO $ do
  events <- readStreamEventsForward eventstore "languages" 0 100 False
  wait events

main :: IO ()
main = do
  conn <- connect defaultSettings (Static "127.0.0.1" 1113)
  warp 3000 HelloWorld
    { eventstore = conn }
