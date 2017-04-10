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

mkYesod "HelloWorld" [parseRoutes|
/events EventsR GET
|]

instance Yesod HelloWorld

getEventsR :: Handler Value
getEventsR = returnJson $ liftIO $ do
  -- TODO: Should connect in main and pass the connection...
  eventstore <- connect defaultSettings (Static "127.0.0.1" 1113)
  events <- readStreamEventsForward eventstore "languages" 0 100 False
  wait events

main :: IO ()
main = do
  warp 3000 HelloWorld
