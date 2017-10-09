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
type ApiFunc  = Query -> IO String

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
    in L.intercalate "\n" [
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
      (ep,qry) = mkqry tmp
      mtd = RqLine {method=toMethod mtd', target=ep, parameters=qry}
  in Header mtd hh hu ha hcl hct

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
          "Host:"           -> hdr { hHost          = Just (head rest) }
          "User-Agent:"     -> hdr { hUserAgent     = Just (head rest) }
          "Accept:"         -> hdr { hAccept        = Just (head rest) }
          "Content-Length:" -> hdr { hContentLength = Just (head rest) }
          "Content-Type:"   -> hdr { hContentType   = Just (head rest) }
          _                 -> hdr

data ContentType = Chtml
                 | Ccss
                 | Cjs
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
toCType  _      = Cplain

