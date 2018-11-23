{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
import Yesod
import Data.Aeson
import Data.Maybe (catMaybes)
import Database.EventStore
import Control.Concurrent.Async

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/events EventsR GET
|]

instance Yesod HelloWorld

getLanguageEvents :: [ResolvedEvent]
getLanguageEvents = do
  -- TODO: Should connect in main and pass the connection...
  eventstore <- connect defaultSettings (Static "127.0.0.1" 1113)
  rs <- readStreamEventsForward eventstore "languages" 0 100 False >>= waitAsync
  case rs of
    ReadSuccess sl -> do
      -- let events = catMaybes $ fmap resolvedEventDataAsJson $ sliceEvents sl
      sliceEvents sl
    e -> error $ "Read failure: " ++ show e

getEventsR :: Handler Value
getEventsR = do
  events <- liftIO getLanguageEvents
  returnJson events


main :: IO ()
main = do
  warp 3000 HelloWorld
