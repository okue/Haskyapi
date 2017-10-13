{-# LANGUAGE OverloadedStrings #-}
module Config (
  domain,
  subdomain,
  db,
  ip,
) where

import Web.Haskyapi.Header (Domain, SubDomain)
import qualified Data.Text as T

domain :: Domain
domain = "localhost"

subdomain :: SubDomain
subdomain = [
     ("test",     "/test/")
  ]

ip = "0.0.0.0"

db = T.pack "app.db"

