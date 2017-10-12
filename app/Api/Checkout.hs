{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Api.Checkout (
  checkout,
  SRequest(..),
  SResponse(..),
) where

import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Aeson

import qualified Model
import Web.Haskyapi.Header (
  Api,
  ApiFunc,
  Method(..),
  ContentType(..)
  )

data SRequest = SRequest  { order :: [Int], coupon :: [String] } deriving (Show, Eq)

data SResponse  = SResponse  { ok :: Bool, amount :: Int, items :: [Int], coupon_ :: Maybe [String] }
                | SResponse2 { ok :: Bool, message :: String }
                deriving (Show, Eq)

instance FromJSON SRequest where
  parseJSON (Object v) = do
    odr <- v .: "order"
    SRequest (map read odr) <$> v .: "coupon"

instance ToJSON SResponse where
  toJSON (SResponse x y z Nothing) = object [ "ok" .= x, "amount" .= y, "items" .= map show z ]
  toJSON (SResponse x y z w)       = object [ "ok" .= x, "amount" .= y, "items" .= map show z, "coupon" .= w ]
  toJSON (SResponse2 x y)          = object [ "ok" .= x, "message" .= y ]

instance FromJSON SResponse where
  parseJSON (Object v) = do
    ok <- v .: "ok"
    if | ok        -> SResponse  ok <$> v .: "amount" <*> (map read <$> v .: "items") <*> v .:? "coupon"
       | otherwise -> SResponse2 ok <$> v .: "message"

checkout :: ApiFunc
checkout qry bdy = do
  let xx   = LC.pack bdy
  case (decode xx :: Maybe SRequest) of
    Nothing  -> return "bad"
    Just (SRequest odr cpn) -> do
      yy <- mapM Model.lookupMenu odr
      if | Nothing `elem` yy -> return . LC.unpack . encode $ SResponse2 False "item_not_found"
         | otherwise -> do
             let ss = SResponse True (sum (catMaybes yy)) odr $ case cpn of [] -> Nothing; _ -> Just cpn
             return . LC.unpack $ encode ss

main = do
  -- let i = concat [ "{ \"order\" : ", show (map show [101,201,301]) , ", \"coupon\" : ", show ["C001","C004","C006"] ," }" ]
  let i = concat [ "{ \"order\" : ", show (map show [109,201,301]) , ", \"coupon\" : [] }" ]
  putStr "Input     => "
  putStrLn i
  putStr "Parse     => "
  print (decode (LC.pack i) :: Maybe SRequest)
  x <- checkout [] i
  putStr "Return    => "
  putStrLn x
  putStr "Parse     => "
  print (decode (LC.pack x) :: Maybe SResponse)
