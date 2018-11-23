{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
import Yesod
import Data.Aeson
import Database.EventStore
import Control.Concurrent.Async
-- import Data.ByteString.Lazy.Char8 as Char8

data HelloWorld = HelloWorld
  { eventstore :: Connection }

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
-- /events EventsR GET
-- /sites/#Int/pages/#Text/comments/ GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

-- getEventsR :: Handler Value
-- getEventsR = returnJson $ liftIO $ do
--   -- TODO: should use eventstore from HelloWorld, not make a new conn
--   conn <- connect defaultSettings (Static "127.0.0.1" 1113)
--   events <- readStreamEventsForward conn "languages" 0 100 False
--   wait events


main :: IO ()
main = do
  conn <- connect defaultSettings (Static "127.0.0.1" 1113)

  let js  = object ["isHaskellTheBest" .= True] -- (.=) comes from Data.Aeson module.
      evt = createEvent "programming" Nothing (withJson js)
  
  -- Appends an event to a stream named `languages`.
  as <- sendEvent conn "languages" anyVersion evt

  -- EventStore interactions are fundamentally asynchronous. Nothing requires
  -- you to wait for the completion of an operation, but it's good to know if
  -- something went wrong.
  _ <- waitAsync as

  events <- readStreamEventsForward conn "languages" 0 100 False
  print $ recordedEventNumber $ resolvedEventOriginal $ head $ liftIO $ wait $ events

  warp 3000 HelloWorld
    { eventstore = conn }
