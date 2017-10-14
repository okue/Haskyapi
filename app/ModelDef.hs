{-# LANGUAGE TemplateHaskell #-}
module ModelDef where
import Database.Persist.TH (derivePersistField)

data Size = S | M | L deriving (Show, Read, Eq)
derivePersistField "Size"
