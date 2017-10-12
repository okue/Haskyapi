{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.Exit

import Web.Haskyapi (runServer, Port)
import Console.Cli (argparse, Option(..))

import qualified Api.Hapi as Hapi
import qualified Config

main :: IO ()
main = do
  args <- getArgs
  case argparse args of
    Left x -> do
      putStrLn "haskyapictl [-p port] [-r root]"
      putStrLn x
    Right opt ->
      mainProc opt

mainProc :: Option -> IO ()
mainProc !opt = do
  let !root = oroot opt
      !port = oport opt
      !ip   = oip   opt
      !url = "http://" ++ ip ++ ":" ++ port ++ "/"
  putStrLn $ "root: "     ++ root
  putStrLn $ "listen on " ++ port
  putStrLn url
  mapM_ (putStrLn . \h -> url ++ h) =<< getfiles root
  runServer (port, root, Config.subdomain, Hapi.routing)

getfiles :: FilePath -> IO [FilePath]
getfiles root =
  filter aux <$> getDirectoryContents root
  where
    aux ('.':_) = False
    aux _ = True

