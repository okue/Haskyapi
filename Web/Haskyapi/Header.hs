module Web.Haskyapi.Header (
  parse,
  RqLine(..),
  Header(..),
  Method(..),
  Query,
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

type Api      = (Method, Endpoint, ApiFunc)
type ApiFunc  = Query -> String

type Query    = [(String, String)]
type Endpoint = String

data Method = GET 
            | POST
            | Other
            deriving (Show, Eq)

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
                hContentType   :: Maybe String
              } deriving (Eq)

instance Show Header where
  show hdr =
    let hr  = hRqLine hdr
        hh  = hHost hdr
        hu  = hUserAgent hdr
        ha  = hAccept hdr
        hcl = hContentLength hdr
        hct = hContentType hdr
    in L.intercalate "\n" $ [
        "RequestLine   -> " ++ show hr,
        "Host          -> " ++ maybe "nothing" show hh,
        "UserAgent     -> " ++ maybe "nothing" show hu,
        "Accept        -> " ++ maybe "nothing" show ha,
        "ContentLength -> " ++ maybe "nothing" show hcl,
        "ContentType   -> " ++ maybe "nothing" show hct
      ]

unitheader :: Header
unitheader = Header {
               hRqLine        = RqLine GET "/" [],
               hHost          = Nothing,
               hUserAgent     = Nothing,
               hAccept        = Nothing,
               hContentLength = Nothing,
               hContentType   = Nothing
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

parseRqLine :: String -> Header -> Header
parseRqLine str (Header hr hh hu ha hcl hct) =
  let mtd':tmp:_ = words str
      (ep,qry) = mkqry tmp in
  case mtd' of
    "GET"  -> let mtd = RqLine {method=GET,   target=ep, parameters=qry} in Header mtd hh hu ha hcl hct
    "POST" -> let mtd = RqLine {method=POST,  target=ep, parameters=qry} in Header mtd hh hu ha hcl hct
    _      -> let mtd = RqLine {method=Other, target=ep, parameters=qry} in Header mtd hh hu ha hcl hct

parse :: [String] -> Header
parse []     = unitheader
parse (x:xs) =
  -- In HTTP protocol, the first line must be Request Line
  let firstheader = parseRqLine x unitheader in
  foldl str2h firstheader xs
    where
      str2h hdr "" = hdr
      str2h hdr@(Header hr hh hu ha hcl hct) str =
        let key:rest = words str in
        case key of
          "Host:"           -> Header hr (Just (rest!!0)) hu ha hcl hct
          "User-Agent:"     -> Header hr hh (Just (rest!!0)) ha hcl hct
          "Accept:"         -> Header hr hh hu (Just (rest!!0)) hcl hct
          "Content-Length:" -> Header hr hh hu ha (Just (rest!!0))  hct
          "Content-Type:"   -> Header hr hh hu ha hcl  (Just (rest!!0))
          _                 -> hdr


-- Content Type reference from Qiita
-- http://qiita.com/AkihiroTakamura/items/b93fbe511465f52bffaa

data ContentType = Chtml
                 | Ccss
                 | Cjs
                 | Cplain
                 | Cjpeg
                 | Cpng
                 | Cmarkdown

instance Show ContentType where
  show Chtml  = "text/html"
  show Ccss   = "text/css"
  show Cjs    = "text/javascript"
  show Cplain = "text/plain"
  show Cjpeg  = "image/jpeg"
  show Cpng   = "image/png"
  show Cmarkdown = show Chtml

toCType :: String -> ContentType
toCType "html"  = Chtml
toCType "css"   = Ccss
toCType "js"    = Cjs
toCType "plain" = Cplain
toCType "jpeg"  = Cjpeg
toCType "png"   = Cpng
toCType "md"    = Cmarkdown
toCType  _      = Chtml
