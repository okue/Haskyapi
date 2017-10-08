#! /usr/bin/env runhaskell
module Main where

import Control.Monad
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.Exit

import Web.Haskyapi (runServer, Port)
import Console.Cli  (argparse, Option(..))

main :: IO ()
main = do
  args <- getArgs
  case argparse args of
    Left x -> do
      putStrLn "haskyapictl [-p port] [-r root]"
      putStrLn x
    Right xs -> do
      let root = oroot xs
          port = oport xs
      putStrLn $ "port: " ++ port ++ " root: " ++ root
      mainProc port root

mainProc :: Port -> FilePath -> IO ()
mainProc port root = do
  let url = "http://localhost:" ++ port ++ "/"
  putStrLn $ "listen on " ++ port
  putStrLn $ url
  files <- getfiles root
  mapM_ (putStrLn . \h -> url ++ h) files
  runServer port root

getfiles :: FilePath -> IO [FilePath]
getfiles root = do
  c <- getDirectoryContents root
  return $ filter aux c
  where
    aux ('.':_) = False
    aux _ = True

