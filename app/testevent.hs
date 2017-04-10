{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoImplicitPrelude #-}

module Main where

import ClassyPrelude

import Database.EventStore
import Data.Aeson

main :: IO ()
main = do
  con <- connect defaultSettings { s_retry = keepRetrying } (Static "localhost" 1113)

  let js  = object ["toto" .= True]
      evt = createEvent "testing" Nothing (withJson js)

  fork $ forever $ do
    (_ :: ByteString) <- getLine
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
        
