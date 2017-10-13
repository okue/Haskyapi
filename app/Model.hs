{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model (
  lookupMenu,
  lookupCoupon,
  migrateAndInit,
) where

import Data.Maybe (listToMaybe)
import Control.Monad.Reader   (ReaderT)
-- import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import qualified Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Menu
    key   Int
    price Int
    deriving Show
  Coupon
    key    String
    target Int
    off    Int
    deriving Show
  SetMenu
    key Int
    menu [Int]
    deriving Show
|]

lookupTemplate f g =
  runSqlite Config.db . aux $ listToMaybe . map (f . entityVal) <$> g
  where
    aux :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
    aux = id

lookupCoupon :: String -> IO (Maybe Coupon)
lookupCoupon x =
  lookupTemplate id $ selectList [CouponKey ==. x] [LimitTo 1]

lookupMenu :: Int -> IO (Maybe Int)
lookupMenu x =
  lookupTemplate menuPrice $ selectList [MenuKey ==. x] [LimitTo 1]

migrateAndInit :: IO ()
migrateAndInit = runSqlite Config.db $ do
  runMigration migrateAll
  mapM_ insert [
      Menu 101 100,
      Menu 102 130,
      Menu 103 320,
      Menu 104 320,
      Menu 105 380,
      Menu 201 150,
      Menu 202 270,
      Menu 203 320,
      Menu 204 280,
      Menu 301 100,
      Menu 302 220,
      Menu 303 250,
      Menu 304 150,
      Menu 305 240,
      Menu 306 270,
      Menu 307 100,
      Menu 308 150
    ]
  mapM_ insert [
      Coupon "C001" 202 120,
      Coupon "C002" 203 170,
      Coupon "C003" 103 50,
      Coupon "C004" 502 70,
      Coupon "C005" 503 80,
      Coupon "C006" 308 50
    ]

main :: IO ()
main = do
  migrateAndInit
  print =<< lookupMenu 101
  print =<< lookupMenu 201
  print =<< lookupMenu 301
  print =<< lookupMenu 901
  print =<< lookupCoupon "C001"
  print =<< lookupCoupon "C004"
