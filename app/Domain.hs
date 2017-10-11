module Domain (
  domain,
  subdomain,
) where

import Web.Haskyapi.Header (Domain, SubDomain)

domain :: Domain
domain = "localhost"

subdomain :: SubDomain
subdomain = [
     ("test",     "/test/")
  ]
