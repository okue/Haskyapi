{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiWayIf #-}
module Api.Hapi (
  routing,
  SRequest(..),
  SResponse(..),
) where

import qualified Codec.Binary.UTF8.String as B8
import qualified Data.ByteString.Lazy.Char8 as LC
import Text.HTML.TagSoup (parseTags, Tag(..))
import Network.HTTP.Simple
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Aeson

import qualified Model
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
