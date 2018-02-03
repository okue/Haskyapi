{-# LANGUAGE BangPatterns #-}
module Main where

import Model (migrateAndInit)
import qualified Api.Hapi as Hapi
import Web.Haskyapi.Console.Cli (haskyapi)

main = haskyapi Hapi.routing migrateAndInit
