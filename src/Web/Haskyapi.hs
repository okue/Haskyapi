{-# LANGUAGE MultiWayIf #-}
module Web.Haskyapi (
  runServer,
  Port
) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Codec.Binary.UTF8.String as B8
import qualified Data.ByteString.Char8    as C
import qualified Data.List.Split   as L
import qualified Data.List         as L
import qualified Text.Markdown     as Md
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
-- import Data.Char (ord)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (utcToLocalTime, TimeZone(..))
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified System.Directory as D
import Text.Printf (printf)

import Debug.Trace (trace)

import qualified Web.Haskyapi.Tool as Tool
import Web.Haskyapi.Header

type Port = String

jst :: TimeZone
jst = TimeZone {
        timeZoneMinutes = 540,
        timeZoneSummerOnly = False,
        timeZoneName = "JST"
      }

runServer :: (Port, FilePath, String, SubDomain, [Api]) -> IO ()
runServer (port,root,ip,subdomain,routing) = do
  soc <- serveSocket ip port
  listen soc 10
  startSocket soc (root,subdomain,routing) `finally` close soc

serveSocket :: String -> Port -> IO Socket
serveSocket ip port = do
  soc  <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr ip
  setSocketOption soc ReusePort (read port)
  bind soc (SockAddrInet (read port) addr)
  return soc

startSocket :: Socket -> (FilePath,SubDomain,[Api]) -> IO ()
startSocket soc cf = forever $ do
  (conn, _addr) <- accept soc
  forkIO $ doResponse conn cf

data Status = OK
            | NotFound
            | Moved

instance Show Status where
  show OK       = "200 OK"
  show NotFound = "404 Not Found"
  show Moved    = "301 Moved Permanently"

htmlhead :: String
htmlhead = unlines [
  "<head>",
  "<link rel='icon' type='image/jpg' href='.icon.ico'>",
  "<link rel='stylesheet' href='/css/markdown.css' type='text/css'/>",
  "<link rel='stylesheet' href='https://raw.githubusercontent.com/sindresorhus/github-markdown-css/gh-pages/github-markdown.css' type='text/css'/>",
  "<meta name='viewport' content='width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no'>",
  "<style>.markdown-body { box-sizing: border-box; min-width: 200px; max-width: 980px; margin: 0 auto; padding: 45px; } @media (max-width: 767px) {.markdown-body}</style>",
  "</head>"
  ]

doResponse :: Socket -> (FilePath,SubDomain,[Api]) -> IO ()
doResponse conn (root', subdomain, routing) = do
  (str, _) <- recvFrom conn 2048
  let bdy =  last . L.splitOn "\r\n" $ C.unpack str
      hdr = parse . L.splitOn "\r\n" $ C.unpack str
      RqLine mtd _trg qry = hRqLine hdr
      trg = unescape _trg
      host = fromMaybe "" $ hHost hdr
      ct = case Tool.getFileExt trg of
             Nothing -> Chtml
             Just ex -> toCType ex
      root = root' ++ dlookup (cutSubdomain host) subdomain
  -- print $ unwords $ map (show . ord) $ C.unpack str

  curTime <- show . utcToLocalTime jst <$> getCurrentTime
  printf "[%s] %s\n" curTime (show hdr)
  case (mtd, trg) of
    (GET, "/") ->
      (sender OK conn Chtml =<< C.readFile (root ++ "/index.html"))
      `catch` \(SomeException e) -> do
        errorlog e "doResponse"
        sender NotFound conn ct =<< genFilerPage root "/."
        -- sender NotFound conn ct $ C.pack "404 Not Found"
    (mtd, '/':'a':'p':'i':endpoint) ->
      apisender OK conn routing endpoint qry mtd bdy
    (GET, path) ->
      case (ct, complement path) of
        (Cmarkdown, Just cpath) -> do
          tmp <- T.readFile $ root ++ cpath
          sender OK conn ct . markdown2html $ tmp
        (_, Just cpath) -> sender OK conn ct =<< C.readFile (root ++ cpath)
        (_, Nothing)    -> redirect conn path
      `catch`
        \(SomeException e) -> do
          errorlog e "doResponse"
          sender NotFound conn ct =<< genFilerPage root path
          -- sender NotFound conn ct $ C.pack "404 Not Found"
    (POST, _) ->
      sender OK conn ct $ C.pack "Please POST request to /api"
    _ ->
      sender NotFound conn Chtml $ C.pack "404 Not Found"

markdown2html file =
  let mdfile = renderHtml $ Md.markdown Md.def file
      aux b = htmlhead ++ "<body class='markdown-body'>" ++ b ++ "</body>"
  in C.pack . B8.encodeString . aux . T.unpack $ mdfile

unescape = unwords . L.splitOn "%20"

head4table = "<head>\
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

errorlog err funcName =
  printf "<<%s error>> reason is %s\n" funcName (show err)

genFilerPage root _path = do
  ls    <- D.getDirectoryContents currentDirectory
  alist <- catMaybes <$> mapM mkAnker (L.sort ls)
  return . C.pack . B8.encodeString $
    head4table ++ mkTable (mkTHead path ++ mkTBody alist)
  `catch`
    \(SomeException e) -> do
      errorlog e  "genFilerPage"
      return $ C.pack "404 Not Found"
  where
    path = unescape _path
    currentDirectory = root ++ path
    mkTable x = "<table>" ++ x ++ "</table>"
    mkTBody xs = "<tbody>" ++ unlines xs ++ "</tbody>"
    mkTHead x = "<thead><th>" ++ x ++ "</th></thead>"
    column link title =
      "<tr><td><a href='" ++ link ++ "'>" ++ title ++ "</a></td></tr>"
    imgTag x = "<img style='max-height: 20;' src='" ++ x ++ "'>"
    directory_icon = imgTag "/.img/icon_directory.jpeg"
    pdf_icon = imgTag "/.img/icon_pdf.jpeg"
    mkAnker ".." = return . Just $ column ".." "🔙"
    mkAnker ('.':_name) = return $ Just ""
    mkAnker _name = do
      let name = unescape _name
          filepath = currentDirectory ++ "/" ++ name
      print (_name, name, root, filepath)
      isFile <- D.doesFileExist filepath
      isDirectory <- D.doesDirectoryExist filepath
      return $ if
        | isDirectory ->
            Just $ column name (directory_icon ++ name)
        | isFile ->
            let title = case Tool.getFileExt name of
                          Just "pdf" -> pdf_icon ++ name
                          _ -> name
            in Just $ column name title
        | otherwise ->
            trace _name Nothing

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

redirect :: Socket -> String -> IO ()
redirect conn path = do
  sendHeader conn Moved Chtml 0
  print path
  sendMany conn $ map C.pack [ "Location: ", path, "/\r\n\r\n" ]
  `catch`
    (\(SomeException e) -> errorlog e "redirect")
  `finally`
    close conn -- >> putStrLn ("close conn " ++ show conn)

sender :: Status -> Socket -> ContentType -> C.ByteString -> IO ()
sender st conn ct msg = do
  sendHeader conn st ct (C.length msg)
  sendMany conn [ C.pack "\r\n", msg, C.pack "\r\n" ]
  `catch`
    (\(SomeException e) -> errorlog e "sender")
  `finally`
    close conn -- >> putStrLn ("close conn " ++ show conn)

apisender :: Status -> Socket -> [Api] -> Endpoint -> Query -> Method -> Body -> IO ()
apisender st conn routing endpoint qry mtd bdy = do
  let h = "Access-Control-Allow-Origin: *\r\n\r\n"
  c <- case rlookup (mtd, endpoint) routing of
        Nothing -> do
          sendHeader conn st Cplain 0
          return "There is no valid api."
        Just (_,_,func,ct) -> do
          sendHeader conn st ct 0
          fmap B8.encodeString (func qry bdy)
  sendMany conn $ map C.pack [ h, c, "\r\n" ]
  `catch`
    (\(SomeException e) -> errorlog e "apisender")
  `finally`
    close conn -- >> putStrLn ("close conn " ++ show conn)

sendHeader :: Socket -> Status -> ContentType -> Int -> IO Int
sendHeader conn st ct cl = do
  send conn $ C.pack $ "HTTP/1.1 " ++ show st ++ "\r\n"
  case filter (== ct) [Cjpeg, Cpng, Cpdf] of
    [] ->
      sendMany conn $ map C.pack [ "Content-Type: ", show ct, "; charset=utf-8\r\n" ]
    _  ->
      sendMany conn $ map C.pack [
        "Accept-Ranges:bytes\r\n",
        "Content-Type: ", show ct, "\r\n" ]
  case cl of
    0 -> return ()
    _ -> sendMany conn $ map C.pack [ "Content-Length: ", show cl, "\r\n" ]
  send conn $ C.pack "Server: Haskyapi\r\n"


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
