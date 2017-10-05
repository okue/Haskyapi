#! /usr/bin/env runhaskell
module Main where

import Control.Monad
import Web.Haskyapi (runServer, Port)
import System.Environment (getArgs)
import System.Exit
import System.Directory (getCurrentDirectory, getDirectoryContents)

-- import Foreign.C.String (peekCString)
-- import Foreign.Api

main :: IO ()
main = do
  -- putStrLn =<< peekCString (c_moji 1)
  args <- getArgs
  case args of
    "help":_    -> do
      putStrLn "haskyapictl [-p port] [-r root]"
      exitSuccess
    _ -> do
      case argparse args of
        Just (port, root) -> do
          putStrLn $ "port: " ++ port ++ " root: " ++ root
          mainProc port root
        Nothing ->
          putStrLn "Please use -p or -r for port-root-config"

argparse :: [String] -> Maybe (Port, FilePath)
argparse args = aux "8080" "html" args
  where
    aux p r args =
      case args of
        [] -> Just (p, r)
        "-p":port:as -> aux port r as
        "-r":root:as -> aux p root as
        _  -> Nothing

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

