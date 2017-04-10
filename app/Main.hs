{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Database.EventStore
import Data.Aeson

import Yesod
import Data.Aeson
import Data.Maybe (catMaybes)
import Database.EventStore
import Control.Concurrent.Async

{- 

{-# LANGUAGE NoImplicitPrelude         #-}

import ClassyPrelude
import Prelude -- (getLine)

testevent :: IO ()
testevent = do
  con <- connect defaultSettings { s_retry = keepRetrying } (Static "localhost" 1113)

  let js  = object ["toto" .= True]
      evt = createEvent "testing" Nothing (withJson js)

  fork $ forever $ do
    foo <- getLine
    -- (_ :: ByteString) <- getLine
    sendEvent con "strim" anyVersion evt

  fork . forever $ handle (\(_ :: SubscriptionClosed) -> threadDelay 5000000) (mkSub con >>= process)

  waitTillClosed con

  where
    mkSub c = do
      putStrLn "making a subscription"
      sub <- subscribe c "strim" True

      waitConfirmation sub
      putStrLn "sub confirmed"

      return sub

    process sub = do
      putStrLn "processing..."

      forever $ do
        n <- nextEvent sub
        print (recordedEventNumber $ resolvedEventOriginal n)
-}

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
  return events


main :: IO ()
main = do
  warp 3000 HelloWorld
