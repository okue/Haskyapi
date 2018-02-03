{-# LANGUAGE BangPatterns #-}
module Main where

import Model (migrateAndInit)
import qualified Api.Hapi as Hapi
import Web.Haskyapi.Console.Cli (haskyapi, haskyapiM)

-- main = haskyapiM Hapi.routing migrateAndInit
main = haskyapi Hapi.routing
