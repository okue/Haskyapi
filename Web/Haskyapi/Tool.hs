{-# LANGUAGE OverloadedStrings #-}
module Web.Haskyapi.Tool (
  getFileExt,
  basename,
) where

import qualified Data.List.Split as L
import qualified Data.List       as L

getFileExt :: String -> String
getFileExt = last . L.splitOn "."

basename :: String -> String
basename = last . L.splitOn "/"
