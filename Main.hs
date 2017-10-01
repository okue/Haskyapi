#! /usr/bin/env runhaskell
module Main where

import Control.Monad
import Web.Haskyapi (runServer)
import System.Environment (getArgs)
import System.Exit

-- import Foreign.C.String (peekCString)
-- import Foreign.Api

htmls :: [FilePath]
htmls = ["index.html", "page.html", "markdown-page.md", "img/logo.png"]

main :: IO ()
main = do
  -- putStrLn =<< peekCString (c_moji 1)
  args <- getArgs
  case args of
    "help":_    -> do
      putStrLn "1 => port 2 => root"
      exitSuccess
    _ -> do
      let (port, root) = case args of
                           []          -> ("8080", "html")
                           port:[]     -> ( port , "html")
                           port:root:_ -> ( port ,  root )
      mainProc port root

mainProc port root = do
  let url = "http://localhost:" ++ port ++ "/"
  putStrLn $ "listen on " ++ port
  putStrLn $ url
  mapM_ (putStrLn . \h -> url ++ h) htmls
  runServer port root
