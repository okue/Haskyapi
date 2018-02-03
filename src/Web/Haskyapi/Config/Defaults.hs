module Web.Haskyapi.Config.Defaults (
  defs
) where

import Data.Map

defs = fromList [
      ("domain", "localhost")
    , ("port", "8080")
    , ("ip", "0.0.0.0")
    , ("db", "app.db")
  ]
