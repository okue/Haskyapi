{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiWayIf #-}
module Api.Hapi (
  routing,
) where

import qualified Codec.Binary.UTF8.String as B8
import qualified Data.ByteString.Lazy.Char8 as LC
import Text.HTML.TagSoup (parseTags, Tag(..))
import Network.HTTP.Simple
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Aeson

import qualified Web.Haskyapi.Tool as Tool
import qualified Model
import Api.Checkout (checkout)
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
            ,(POST, "/title",    title,     Cplain)
            ,(POST, "/checkout", checkout,  Cjson)
            ,(GET,  "/checkout", checkout,  Cjson)
          ]

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

