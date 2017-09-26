module Api.Hapi (
  routing,
) where

import Web.Haskyapi.Header (Api, ApiFunc, Method(..))

routing :: [Api]
routing = [
            (GET,  "/test",  test GET),
            (POST, "/test",  test POST),
            (GET,  "/test2", test2),
            (POST, "/test2", test2),
            (GET,  "/add",   add),
            (POST, "/add",   add)
          ]

test :: Method -> ApiFunc
test GET  qry = "ok from haskell api.\nThis is GET."
test POST qry = "ok from haskell api.\nThis is POST."

test2 :: ApiFunc
test2 qry = "おーけい"

add :: ApiFunc
add qry =
  let x' = lookup "x" qry
      y' = lookup "y" qry in
  case (x',y') of
    (Nothing,_) -> "No value of x."
    (_,Nothing) -> "No value of y."
    (Just x, Just y) -> show $ (read x) + (read y)
