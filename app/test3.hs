{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
import Yesod
import Data.Aeson
import Database.EventStore
import Control.Concurrent.Async

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
