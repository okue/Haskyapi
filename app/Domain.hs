module Domain (
  domain,
  subdomain,
)
where

domain :: String
domain = "localhost"

subdomain :: [(String, String)]
subdomain = [
     ("test",     "/test/")
  ]
