{-# LANGUAGE MultiWayIf, OverloadedStrings #-} 
module Web.Haskyapi (
  runServer,
  Port
) where

import qualified Network.Socket as Net hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Net

import qualified Codec.Binary.UTF8.String as B8
import qualified Data.ByteString.Char8    as C
import qualified Text.Markdown     as Md
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Time.LocalTime (utcToLocalTime, TimeZone(..))
import Data.Time.Clock (getCurrentTime)

import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.List.Split   as L
import qualified Data.List         as L
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (chr)

import qualified System.Directory as D
import Text.Printf (printf)
-- import Debug.Trace (trace)

import qualified Web.Haskyapi.Tool as Tool
import Web.Haskyapi.Header

type Port = String

data Status = OK
            | NotFound
            | Moved

instance Show Status where
  show OK       = "200 OK"
  show NotFound = "404 Not Found"
  show Moved    = "301 Moved Permanently"

jst :: TimeZone
jst = TimeZone {
        timeZoneMinutes = 540,
        timeZoneSummerOnly = False,
        timeZoneName = "JST"
      }

runServer :: (Port, FilePath, String, SubDomain, [Api]) -> IO ()
runServer (port,root,ip,subdomain,routing) = do
  soc <- serveSocket ip port
  Net.listen soc 10
  startSocket soc (root,subdomain,routing) `finally` Net.close soc

serveSocket :: String -> Port -> IO Net.Socket
serveSocket ip port = do
  soc  <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  addr <- Net.inet_addr ip
  Net.setSocketOption soc Net.ReusePort (read port)
  Net.bind soc (Net.SockAddrInet (read port) addr)
  return soc

startSocket :: Net.Socket -> (FilePath,SubDomain,[Api]) -> IO ()
startSocket soc cf = forever $ do
  (conn, _addr) <- Net.accept soc
  forkIO $ doResponse conn cf

doResponse :: Net.Socket -> (FilePath, SubDomain, [Api]) -> IO ()
doResponse conn (root', subdomain, routing) = do
  (str, _) <- Net.recvFrom conn 2048

  let bdy =  last . L.splitOn "\r\n" $ C.unpack str
      hdr = parse . L.splitOn "\r\n" $ C.unpack str
      RqLine mtd _trg qry = hRqLine hdr
      trg = unescape _trg
      host = fromMaybe "" $ hHost hdr
      ct = case Tool.getFileExt trg of
             Nothing -> Chtml
             Just ex -> toCType ex
      root = root' ++ dlookup (cutSubdomain host) subdomain

  curTime <- show . utcToLocalTime jst <$> getCurrentTime
  printf "[%s] %s\n" curTime (show hdr)

  case (mtd, trg) of
    (mtd, '/':'a':'p':'i':endpoint) ->
      apiSend OK conn routing endpoint qry mtd bdy
    (GET, "/") -> do
      content <- C.readFile (root ++ "/index.html")
        `catch` \(SomeException e) -> do
          errorlog e "doResponse"
          genFilerPage root "/."
      httpSend OK conn Chtml content
    (GET, path) ->
      case (ct, complement path) of
        (Cmarkdown, Just cpath) -> do
          mdfile <- T.readFile $ root ++ urlDecode cpath
          httpSend OK conn ct . markdown2html $ mdfile
        (_, Just cpath) -> httpSend OK conn ct =<< C.readFile (root ++ urlDecode cpath)
        (_, Nothing)    -> redirect conn (C.pack path)
      `catch` \(SomeException e) -> do
        errorlog e "doResponse"
        httpSend NotFound conn ct =<< genFilerPage root (urlDecode path)
    (POST, _) ->
      httpSend OK conn ct "Please POST request to /api"
    _ ->
      httpSend NotFound conn Chtml "404 Not Found"

-- % url encode -> utf8 -> internal encode
urlDecode :: String -> String
urlDecode = urlDecodeInternal ""
  where
    urlDecodeInternal acc ('%':c1:c2:ss) =
      let c = chr (read ("0x" ++ [c1, c2]) :: Int)
      in urlDecodeInternal (c:acc) ss
    urlDecodeInternal acc (c:ss) =
      urlDecodeInternal (c:acc) ss
    urlDecodeInternal acc [] =
      B8.decodeString $ L.reverse acc

markdown2html :: T.Text -> C.ByteString
markdown2html mdfile =
  let html4md = renderHtml $ Md.markdown Md.def mdfile
      aux b = T.unwords [head4md, "<body class='markdown-body'>",
                         b, "</body>"]
  in C.pack . B8.encodeString . T.unpack . aux $ html4md

unescape :: String -> String
unescape = unwords . L.splitOn "%20"

genFilerPage :: FilePath -> FilePath -> IO C.ByteString
genFilerPage root _path = do
  ls    <- D.getDirectoryContents currentDirectory
  alist <- catMaybes <$> mapM mkAnker (L.sort ls)
  return . C.pack . B8.encodeString $
    head4filertable ++ mkTable (mkTHead path ++ mkTBody alist)
  `catch` \(SomeException e) -> do
    errorlog e  "genFilerPage"
    return "404 Not Found"
  where
    path = unescape _path
    currentDirectory = root ++ path

    mkTable x  = "<table>" ++ x ++ "</table>"
    mkTBody xs = "<tbody>" ++ unlines xs ++ "</tbody>"
    mkTHead x  = "<thead><th>" ++ x ++ "</th></thead>"

    imgTag x = "<img style='max-height: 20;' src='" ++ x ++ "'>"
    directory_icon = imgTag "/.img/icon_directory.jpeg"
    pdf_icon = imgTag "/.img/icon_pdf.jpeg"

    column link title =
      "<tr><td><a href='" ++ link ++ "'>" ++ title ++ "</a></td></tr>"

    mkAnker ".." = return . Just $ column ".." "🔙"
    mkAnker ('.':_name) = return $ Just ""
    mkAnker _name = do
      let name = unescape _name
          filepath = currentDirectory ++ "/" ++ name
      isFile <- D.doesFileExist filepath
      isDirectory <- D.doesDirectoryExist filepath
      return $ if
        | isDirectory ->
            Just $ column (name ++ "/") (directory_icon ++ name)
        | isFile ->
            let title = case Tool.getFileExt name of
                          Just "pdf" -> pdf_icon ++ name
                          _ -> name
            in Just $ column name title
        | otherwise -> Nothing

----------------------------------
-- hoge/     -> hoge/index.html
-- hoge.html -> hoge.html
-- hoge      -> Nothing
----------------------------------
complement :: String -> Maybe String
complement path =
  let bn = Tool.basename path in
  case bn of
    -- path ends with '/'
    "" -> Just $ path ++ "index.html"
    _
      -- basename has a file extension such as ".html"
      | length (L.splitOn "." bn) > 1 -> Just path
      | otherwise -> Nothing

redirect :: Net.Socket -> C.ByteString -> IO ()
redirect conn path = do
  sendHeader conn Moved Chtml 0
  Net.sendMany conn [ "Location: ", path, "/\r\n\r\n" ]
  `catch`
    (\(SomeException e) -> errorlog e "redirect")
  `finally`
    Net.close conn

httpSend :: Status -> Net.Socket -> ContentType -> C.ByteString -> IO ()
httpSend st conn ct msg = do
  sendHeader conn st ct (C.length msg)
  Net.sendMany conn [ "\r\n", msg, "\r\n" ]
  `catch`
    (\(SomeException e) -> errorlog e "send")
  `finally`
    Net.close conn

apiSend :: Status -> Net.Socket -> [Api] -> Endpoint
                  -> Query -> Method -> Body -> IO ()
apiSend st conn routing endpoint qry mtd bdy = do
  let h = "Access-Control-Allow-Origin: *\r\n\r\n"
  c <- case rlookup (mtd, endpoint) routing of
        Nothing -> do
          sendHeader conn st Cplain 0
          return "There is no valid api."
        Just (_,_,func,ct) -> do
          sendHeader conn st ct 0
          fmap (C.pack . B8.encodeString) (func qry bdy)
  Net.sendMany conn [h, c, "\r\n"]
  `catch`
    (\(SomeException e) -> errorlog e "apiSend")
  `finally`
    Net.close conn

cshow :: Show a => a -> C.ByteString
cshow = C.pack . show

sendHeader :: Net.Socket -> Status -> ContentType -> Int -> IO Int
sendHeader conn st ct cl = do
  Net.send conn $ C.unwords ["HTTP/1.1 ", cshow st, "\r\n"]
  if ct `elem` [Cjpeg, Cpng, Cpdf] then
    Net.sendMany conn [
      "Accept-Ranges:bytes\r\n",
      "Content-Type: ", cshow ct, "\r\n" ]
  else
    Net.sendMany conn ["Content-Type: ", cshow ct, "; charset=utf-8\r\n"]
  case cl of
    0 -> return ()
    _ -> Net.sendMany conn ["Content-Length: ", cshow cl, "\r\n"]
  Net.send conn "Server: Haskyapi\r\n"

cutSubdomain :: String -> String
cutSubdomain = head . L.splitOn "."

dlookup :: String -> SubDomain -> String
dlookup d subdomain =
  fromMaybe "" (lookup d subdomain)

rlookup :: (Method, Endpoint) -> [Api] -> Maybe Api
rlookup _ [] = Nothing
rlookup me@(mtd, ep) ((x@(mtd',ep',_,_)):xs)
  | mtd == mtd' && ep == ep' = Just x
  | otherwise                = rlookup me xs

errorlog :: Exception e => e -> String -> IO ()
errorlog err funcName =
  printf "<<%s error>> reason is %s\n" funcName (show err)

head4md = "<head>\
\<link rel='icon' type='image/jpg' href='.icon.ico'>\
\<link rel='stylesheet' href='/css/markdown.css' type='text/css'/>\
\<meta name='viewport' content='width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no'>\
\<style>\
\ .markdown-body {\
\   box-sizing: border-box;\
\   min-width: 200px;\
\   max-width: 980px;\
\   margin: 0 auto;\
\   padding: 45px;\
\ }\
\ @media (max-width: 767px) {\
\   .markdown-body\
\ }\
\</style>\
\</head>"

head4filertable = "<head>\
\<link rel='icon' type='image/jpg' href='.icon.ico'>\
\<style type='text/css'>\
\ table{\
\   width: 100%;\
\   border-collapse:collapse;\
\   margin-left: 5;\
\ }\
\ td,th{\
\   padding:10px;\
\ }\
\ th{\
\   color:#fff;\
\   background:#005ab3;\
\ }\
\ table tr:nth-child(odd){\
\   background:#e6f2ff;\
\ }\
\ td{\
\   border-bottom:2px solid #80bcff;\
\ }\
\</style>\
\</head>"
