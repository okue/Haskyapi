#! /usr/bin/env stack runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
  let req' = setRequestMethod "POST" "http://example.com"
      req  = setRequestBodyLBS (L8.pack "1") .
             setRequestHeaders [("Content-Type","application/json")] $ req'
  res <- httpLBS req
  L8.putStrLn $ getResponseBody res

