{-# LANGUAGE OverloadedStrings #-}
module Config.Config (
  domain,
  subdomain,
  db,
  ip,
) where

import qualified Data.Text as T
import Web.Haskyapi.Header (Domain, SubDomain)
import Config.Parser

parsedFile = do
  f <- readFile "setting.yml"
  return $ sparser "setting.yml" f

slookup k []   = Nothing
slookup k (x:xs)
  | k == key x = Just x
  | otherwise  = slookup k xs

aux def k fun = do
  f <- parsedFile
  case f of
    Left  x -> return def
    Right x ->
      case slookup k x of
        Nothing -> return def
        Just x  -> return $ fun x

subdomain :: IO SubDomain
subdomain = do
  tmp <- aux [] "subdomain" bval
  return $ map (\x -> (key x, aval x)) tmp

domain :: IO Domain
domain = aux "localhost" "domain" aval

ip = aux "0.0.0.0" "ip" aval

db = T.pack <$> aux "app.db" "db" aval
-- db = T.pack "app.db"

main = do
  print =<< domain
  print =<< ip
  print =<< db
  print =<< subdomain
