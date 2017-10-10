{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Web.Haskyapi.Tool (
  getFileExt,
  basename,
) where

import qualified Data.List.Split as L
import qualified Data.List       as L

getFileExt :: String -> Maybe String
getFileExt path =
  let ex = last . L.splitOn "." $ path in
  if | ex == path -> Nothing
     | otherwise  -> Just ex

basename :: String -> String
basename = last . L.splitOn "/"
