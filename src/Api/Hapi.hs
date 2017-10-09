{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Api.Hapi (
  routing,
) where

import Network.HTTP.Simple
import Text.HTML.TagSoup (parseTags, Tag(..))
import qualified Codec.Binary.UTF8.String as B8
import qualified Data.ByteString.Lazy.Char8 as LC

import qualified Web.Haskyapi.Tool as Tool
import Web.Haskyapi.Header (
  Api,
  ApiFunc,
  Method(..),
  )

-- import Foreign.C.Types
-- import Foreign.C.String
-- foreign import ccall "math.h sin" c_sin :: Double -> Double
-- foreign import ccall "add"  c_add  :: CInt -> CInt -> CInt
-- foreign import ccall "two"  c_two  :: CInt
-- foreign import ccall "moji" c_moji :: CInt -> CString


routing :: [Api]
routing = [
            (GET,  "/test",  test GET),
            (POST, "/test",  test POST),
            (GET,  "/test2", test2),
            (POST, "/test2", test2),
            (GET,  "/add",   add),
            (POST, "/add",   add),
            (GET,  "/title", title),
            (POST, "/title", title)
          ]


title :: ApiFunc
title qry =
  let x = lookup "url" qry in
  case x of
    Nothing  -> return "Please query parameter of \"url\" in the form of ?url=https://hoge.com"
    Just url ->
      case Tool.getFileExt url of
        Just "pdf" -> return $ Tool.basename url
        _ -> do
          res <- httpLBS =<< parseRequest url
          return . B8.decodeString . LC.unpack . findTitle . parseTags $ getResponseBody res
  where
    findTitle (TagOpen "title" _ : TagText x : xs) = x
    findTitle (TagOpen "TITLE" _ : TagText x : xs) = x
    findTitle [] = "no title"
    findTitle (x:xs) = findTitle xs


test :: Method -> ApiFunc
test GET  qry = return "ok from haskell api.\nThis is GET."
test POST qry = return "ok from haskell api.\nThis is POST."

test2 :: ApiFunc
test2 qry = return "おーけい"


add :: ApiFunc
add qry =
  let x' = lookup "x" qry
      y' = lookup "y" qry in
  return $ case (x',y') of
    (Nothing,_) -> "No value of x."
    (_,Nothing) -> "No value of y."
    (Just x, Just y) -> show $ read x + read y

