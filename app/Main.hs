{-# LANGUAGE DataKinds                 #-}
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

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as B

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

getConnection :: IO Connection
getConnection = 
  connect defaultSettings (Static "127.0.0.1" 1113)

myStreamResult :: IO (Async (ReadResult 'RegularStream StreamSlice))
myStreamResult = do
  conn <- getConnection
  readStreamEventsForward conn "languages" 0 100 False

-- myStreamResultLifted =
--   liftIO myStreamResult

myStreamResultAwaited :: IO (ReadResult 'RegularStream StreamSlice)
myStreamResultAwaited = 
  myStreamResult >>= waitAsync

-- myLanguageEvents = do
--   rs <- myStreamResultAwaited
--   case rs of
--     ReadSuccess sl -> do
--       sliceEvents sl
--     e -> error $ "Read failure: " ++ show e

-- myReadEvents = do
--   conn <- connect defaultSettings (Static "127.0.0.1" 1113)
--   events <- readStreamEventsForward conn "languages" 0 100 False >>= waitAsync
--   events

myStreamEvents :: IO [ResolvedEvent]
myStreamEvents = do
  rs <- myStreamResultAwaited
  case rs of
    ReadSuccess sl -> do
      let slice = sliceEvents sl
      return slice
      -- return sliceEvents sl
    e -> return []


myStreamEventsDataHead :: IO ResolvedEvent
myStreamEventsDataHead = do
  evts <- myStreamEvents
  return $ head evts

myStreamEventsJson :: IO [ByteString]
myStreamEventsJson = do
  evts <- myStreamEvents
  return $ fmap (recordedEventData . resolvedEventOriginal) evts 

-- getLanguageEvents :: [ResolvedEvent]
-- getLanguageEvents = do
--   -- TODO: Should connect in main and pass the connection...
--   eventstore <- connect defaultSettings (Static "127.0.0.1" 1113)
--   rs <- readStreamEventsForward eventstore "languages" 0 100 False >>= waitAsync
--   case rs of
--     ReadSuccess sl -> do
--       let events = catMaybes $ fmap resolvedEventDataAsJson $ sliceEvents sl
--       head events
--       -- head sliceEvents sl
--     e -> error $ "Read failure: " ++ show e

getEventsR :: Handler Value
getEventsR = do
  -- events <- liftIO getLanguageEvents
  -- events <- getLanguageEvents
  foo <- liftIO getLine
  returnJson foo

main :: IO ()
main = do
  warp 3000 HelloWorld
