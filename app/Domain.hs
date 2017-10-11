module Domain (
  domain,
  subdomain,
)
where

domain :: Domain
domain = "localhost"

subdomain :: SubDomain
subdomain = [
     ("test",     "/test/")
  ]
