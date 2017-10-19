{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.Exit

import Web.Haskyapi (runServer, Port)
import Web.Haskyapi.Console.Cli (argparse, Option(..), Mode(..))
import Model (migrateAndInit)

import qualified Api.Hapi as Hapi
import qualified Config.Config as Config

main :: IO ()
main = do
  args <- getArgs
  case argparse args of
    Error x ->
      putStrLn x
    Message x ->
      putStrLn x
    Runserver opt ->
      mainProc opt
    Migrate opt ->
      migrateAndInit

mainProc :: Option -> IO ()
mainProc !opt = do
  let !root = oroot opt
      !port = oport opt
      !_ip  = oip   opt
      !ip   = if _ip == "null" then Config.ip else _ip
      !url  = "http://" ++ ip ++ ":" ++ port ++ "/"
  putStrLn $ "root: "     ++ root
  putStrLn $ "listen on " ++ port
  putStrLn url
  mapM_ (putStrLn . \h -> url ++ h) =<< getfiles root
  runServer (port, root, ip, Config.subdomain, Hapi.routing)

getfiles :: FilePath -> IO [FilePath]
getfiles root =
  filter aux <$> getDirectoryContents root
  where
    aux ('.':_) = False
    aux _ = True

