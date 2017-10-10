module Web.Haskyapi.Header (
  parse,
  RqLine(..),
  Header(..),
  Method(..),
  Query,
  Body,
  Endpoint,
  Api,
  ApiFunc,
  ContentType(..),
  toCType,
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.List.Split as S
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Maybe
import Control.Monad

import Debug.Trace (trace)

type Api      = (Method, Endpoint, ApiFunc, ContentType)
type ApiFunc  = Query -> Body -> IO String

type Body     = String
type Query    = [(String, String)]
type Endpoint = String

data Method = GET 
            | POST
            | PUT
            | DELETE
            | PATCH
            | Other
            deriving (Show, Eq)

toMethod :: String -> Method
toMethod "GET"    = GET
toMethod "POST"   = POST
toMethod "PUT"    = PUT
toMethod "DELETE" = DELETE
toMethod "PATCH"  = PATCH
toMethod _        = Other

data RqLine = RqLine {
                method     :: Method,
                target     :: String,
                parameters :: Query
              } deriving (Show, Eq)

data Header = Header {
                hRqLine        :: RqLine,
                hHost          :: Maybe String,
                hUserAgent     :: Maybe String,
                hAccept        :: Maybe String,
                hContentLength :: Maybe String,
                hContentType   :: Maybe String,
                hReferer       :: Maybe String
              } deriving (Eq)

instance Show Header where
  show hdr = L.intercalate "\n" [
      "RequestLine   -> " ++ show (hRqLine hdr),
      "Host          -> " ++ maybe "nothing" show (hHost hdr),
      "UserAgent     -> " ++ maybe "nothing" show (hUserAgent hdr),
      "Accept        -> " ++ maybe "nothing" show (hAccept hdr),
      "ContentLength -> " ++ maybe "nothing" show (hContentLength hdr),
      "ContentType   -> " ++ maybe "nothing" show (hContentType hdr),
      "Referer       -> " ++ maybe "nothing" show (hReferer hdr)
    ]

unitheader :: Header
unitheader = Header {
               hRqLine        = RqLine GET "/" [],
               hHost          = Nothing,
               hUserAgent     = Nothing,
               hAccept        = Nothing,
               hContentLength = Nothing,
               hContentType   = Nothing,
               hReferer       = Nothing
             }

mkqry :: String -> (Endpoint, Query)
mkqry tmp =
  let ep:qry' = S.splitOneOf "?&" tmp
      qry     = map (qsplit "") qry'
  in (ep, qry)

qsplit :: String -> String -> (String, String)
qsplit key "" = ("", "")
qsplit key (c:cs)
  | c == '='  = (key, cs)
  | otherwise = qsplit (key++[c]) cs

-------------------------------------------------------------
-- Example
-------------------------------------------------------------
-- GET / HTTP/1.1
-- Host: hoge.com
-- Connection: keep-alive
-- Pragma: no-cache
-- Cache-Control: no-cache
-- Upgrade-Insecure-Requests: 1
-- User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36
-- Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
-- Accept-Encoding: gzip, deflate
-- Accept-Language: ja,en-US;q=0.8,en;q=0.6,zh-CN;q=0.4,zh;q=0.2
-- Cookie: _ga=GA1.2.1220732232.1506737218
-------------------------------------------------------------

parseRqLine :: String -> Header -> Header
parseRqLine str hdr =
  -- Assume: Request Line is always in the valid form.
  --         GET /hoge/index.html HTTP/1.1
  let mtd':tmp:_ = words str
      (ep,qry) = mkqry tmp
      mtd = RqLine {method=toMethod mtd', target=ep, parameters=qry}
  in hdr { hRqLine = mtd }

parse :: [String] -> Header
parse []     = unitheader
parse (x:xs) =
  -- In HTTP protocol, the first line must be Request Line
  let firstheader = parseRqLine x unitheader in
  foldl str2h firstheader xs
    where
      str2h hdr "" = hdr
      str2h hdr str =
        let key:rest = words str in
        case key of
          "Host:"           -> hdr { hHost          = Just (head rest) }
          "User-Agent:"     -> hdr { hUserAgent     = Just (head rest) }
          "Accept:"         -> hdr { hAccept        = Just (head rest) }
          "Content-Length:" -> hdr { hContentLength = Just (head rest) }
          "Content-Type:"   -> hdr { hContentType   = Just (head rest) }
          "Referer:"        -> hdr { hReferer       = Just (head rest) }
          _                 -> hdr

data ContentType = Chtml
                 | Ccss
                 | Cjs
                 | Cjson
                 | Cplain
                 | Cjpeg
                 | Cpng
                 | Cgif
                 | Cpdf
                 | Cmarkdown
                 deriving (Eq)

instance Show ContentType where
  show Cmarkdown = show Chtml
  show Chtml  = "text/html"
  show Ccss   = "text/css"
  show Cjs    = "text/javascript"
  show Cplain = "text/plain"
  show Cjpeg  = "image/jpeg"
  show Cpng   = "image/png"
  show Cgif   = "image/gif"
  show Cpdf   = "application/pdf"
  show Cjson  = "application/json"
  -- show _      = "text/plain"

toCType :: String -> ContentType
toCType "html"  = Chtml
toCType "htm"   = Chtml
toCType "md"    = Cmarkdown
toCType "css"   = Ccss
toCType "js"    = Cjs
toCType "plain" = Cplain
toCType "jpeg"  = Cjpeg
toCType "png"   = Cpng
toCType "gif"   = Cgif
toCType "pdf"   = Cpdf
toCType "txt"   = Cplain
toCType "text"  = Cplain
toCType "json"  = Cjson
toCType  _      = Cplain

