#! /usr/bin/env runhaskell
module Main where

import Control.Monad
import Web.Haskyapi (runServer)
import System.Environment (getArgs)

-- import Foreign.C.String (peekCString)
-- import Foreign.Api

htmls :: [FilePath]
htmls = ["index.html", "manual.html", "page.html"]

main :: IO ()
main = do
  -- putStrLn =<< peekCString (c_moji 1)
  args <- getArgs
  let port = case args of [] -> "8080"; port:_ -> port
      url  = "http://localhost:" ++ port ++ "/"
  putStrLn $ "listen on " ++ port
  putStrLn $ url
  mapM_ (putStrLn . \h -> url ++ h) htmls
  runServer port
