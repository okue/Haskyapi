{-# LANGUAGE OverloadedStrings #-}
module Config (
  domain,
  subdomain,
  db,
) where

import Web.Haskyapi.Header (Domain, SubDomain)
import qualified Data.Text as T

domain :: Domain
domain = "localhost"

subdomain :: SubDomain
subdomain = [
     ("test",     "/test/")
  ]

db = T.pack "app.db"
