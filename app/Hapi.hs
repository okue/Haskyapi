{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
module Hapi (
  routing,
  SRequest(..),
  SResponse(..),
) where

import Network.HTTP.Simple
import Text.HTML.TagSoup (parseTags, Tag(..))
import qualified Codec.Binary.UTF8.String as B8
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Applicative
import Data.Aeson
import Data.Maybe (catMaybes)
import GHC.Generics

import Debug.Trace (trace)

import qualified Web.Haskyapi.Tool as Tool
import Web.Haskyapi.Header (
  Api,
  ApiFunc,
  Method(..),
  ContentType(..)
  )

-- import Foreign.C.Types
-- import Foreign.C.String
-- foreign import ccall "math.h sin" c_sin :: Double -> Double
-- foreign import ccall "add"  c_add  :: CInt -> CInt -> CInt
-- foreign import ccall "two"  c_two  :: CInt
-- foreign import ccall "moji" c_moji :: CInt -> CString

routing :: [Api]
routing = [
             (GET,  "/test",     test GET,  Cplain)
            ,(POST, "/test",     test POST, Cplain)
            ,(GET,  "/add",      add,       Cplain)
            ,(GET,  "/title",    title,     Cplain)
            ,(POST, "/checkout", checkout,  Cjson)
            ,(GET,  "/checkout", checkout,  Cjson)
          ]

data SRequest = SRequest  { order :: [String], coupon :: [String] } deriving (Show, Generic)

data SResponse  = SResponse  { ok :: Bool, amount :: Int, items :: [String] }
                | SResponse_ { ok :: Bool, amount :: Int, items :: [String], coupon_ :: [String] }
                | SResponse2 { ok :: Bool, message :: String }
                deriving (Show, Eq)

instance FromJSON SRequest

instance ToJSON SResponse where
  toJSON (SResponse  x y z)   = object [ "ok" .= x, "amount" .= y, "items" .= z ]
  toJSON (SResponse_ x y z w) = object [ "ok" .= x, "amount" .= y, "items" .= z, "coupon" .= w ]
  toJSON (SResponse2 x y)     = object [ "ok" .= x, "message" .= y ]

instance FromJSON SResponse where
  parseJSON (Object v) = do
    ok <- v .: "ok"
    if | ok        -> SResponse  ok <$> (v .: "amount") <*> (v .: "items")
       | otherwise -> SResponse2 ok <$> (v .: "message")

hamb    = [("101",100),("102",130),("103",320),("104",320),("105",380)]
side    = [("201",150),("202",270),("203",320),("204",280)]
drink   = [("301",100),("302",220),("303",250),("304",150),("305",240),("306",270),("307",100),("308",150)]
setmenu = [("501",620),("502",620),("503",680)]
cpon    = [("C001",120),("C002",170),("C003",50),("C004",70),("C005",80),("C006",50)]

checkout :: ApiFunc
checkout qry bdy = do
  let xx   = LC.pack bdy
      menu = hamb ++ side ++ drink
  case (decode xx :: Maybe SRequest) of
    Nothing  -> return "bad"
    Just srq -> do
      let yy = map (`lookup` menu) (order srq)
      if | Nothing `elem` yy -> return . LC.unpack . encode $ SResponse2 False "item_not_found"
         | otherwise -> do
             let ss = SResponse True (sum (catMaybes yy)) (order srq)
             return . LC.unpack $ encode ss

title :: ApiFunc
title qry _ =
  let x = lookup "url" qry in
  case x of
    Nothing  -> return "Please query parameter of \"url\" in the form of ?url=https://hoge.com"
    Just url ->
      case Tool.getFileExt url of
        Just "pdf" ->
          return $ Tool.basename url
        _ ->
          B8.decodeString . LC.unpack . findTitle . parseTags . getResponseBody <$> (httpLBS =<< parseRequest url)
  where
    findTitle (TagOpen "title" _ : TagText x : xs) = x
    findTitle (TagOpen "TITLE" _ : TagText x : xs) = x
    findTitle [] = "no title"
    findTitle (x:xs) = findTitle xs

test :: Method -> ApiFunc
test GET  qry _ = return "ok from haskell api.\nThis is GET."
test POST qry _ = return "ok from haskell api.\nThis is POST."

add :: ApiFunc
add qry _ =
  let x' = lookup "x" qry
      y' = lookup "y" qry in
  return $ case (x',y') of
    (Nothing,_) -> "No value of x."
    (_,Nothing) -> "No value of y."
    (Just x, Just y) -> show $ read x + read y

main = do
  x <- checkout [] ""
  putStrLn x
