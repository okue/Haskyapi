module Web.Haskyapi (
  runServer,
  Port
) where

-- http://hackage.haskell.org/package/network-2.4.0.1/docs/Network-Socket.html 
-- http://qiita.com/asukamirai/items/522cc3c07d7d9ad21dfa 

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Codec.Binary.UTF8.String as B8
import qualified Data.ByteString.Char8    as C
import qualified Data.List.Split   as L
import qualified Data.List         as L
import qualified Text.Markdown     as Md
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import Data.Maybe (fromMaybe)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (utcToLocalTime, TimeZone(..))
import Control.Concurrent
import Control.Exception
import Control.Monad
import Debug.Trace (trace)

import qualified Web.Haskyapi.Tool as Tool
import Web.Haskyapi.Header
import qualified Api.Hapi as Hapi
import qualified Config.Domain as Config

type Port = String

jst :: TimeZone
jst = TimeZone {
        timeZoneMinutes = 540,
        timeZoneSummerOnly = False,
        timeZoneName = "JST"
      }

runServer :: Port -> FilePath -> IO ()
runServer port root = do
  soc <- serveSocket port
  listen soc 5
  startSocket soc root `finally` close soc

serveSocket :: Port -> IO Socket
serveSocket port = do
  soc  <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr "0.0.0.0"
  setSocketOption soc ReusePort (read port)
  bind soc (SockAddrInet (read port) addr)
  return soc

startSocket :: Socket -> FilePath -> IO ()
startSocket soc root = forever $ do
  (conn, addr) <- accept soc
  forkIO $ doResponse conn root

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
  "<link rel=\"stylesheet\" href=\"/css/markdown.css\" type=\"text/css\"/>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no\">",
  "</head>"
  ]

doResponse :: Socket -> FilePath -> IO ()
doResponse conn root' = do
  (str, _) <- recvFrom conn 1024
  let bdy =  last . L.splitOn "\r\n" $ C.unpack str
      hdr = parse . L.splitOn "\r\n" $ C.unpack str
      RqLine mtd trg qry = hRqLine hdr
      host = fromMaybe "" $ hHost hdr
      ct = case Tool.getFileExt trg of
             Nothing -> Chtml
             Just ex -> toCType ex
      root = root' ++ dlookup (cutSubdomain host) Config.subdomain
  putStr "\n"
  print =<< utcToLocalTime jst <$> getCurrentTime
  print hdr
  case (mtd, trg) of
    (GET, "/") ->
      (sender OK conn Chtml =<< C.readFile (root ++ "/index.html"))
      `catch` \(SomeException e) -> do
        print e
        sender NotFound conn ct $ C.pack "404 Not Found"
    (mtd, '/':'a':'p':'i':endpoint) ->
      apisender OK conn endpoint qry mtd bdy
    (GET, path) ->
      case (ct, complement path) of
        (Cmarkdown, Just cpath) -> do
          tmp <- T.readFile $ root ++ cpath
          let mdfile = renderHtml $ Md.markdown Md.def tmp
              aux b  = htmlhead ++ "<body>" ++ b ++ "</body>"
          sender OK conn ct $ C.pack . B8.encodeString . aux . T.unpack $ mdfile
        (_, Just cpath) -> sender OK conn ct =<< C.readFile (root ++ cpath)
        (_, Nothing)    -> redirect conn path
      `catch`
        \(SomeException e) -> do
          print e
          sender NotFound conn ct $ C.pack "404 Not Found"
    (POST, _) ->
      sender OK conn ct $ C.pack "Please POST request to /api"
    _ ->
      sender NotFound conn Chtml $ C.pack "404 Not Found"

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
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

sender :: Status -> Socket -> ContentType -> C.ByteString -> IO ()
sender st conn ct msg = do
  sendHeader conn st ct (C.length msg)
  sendMany conn [ C.pack "\r\n", msg, C.pack "\r\n" ]
  `catch`
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

apisender :: Status -> Socket -> Endpoint -> Query -> Method -> Body -> IO ()
apisender st conn endpoint qry mtd bdy = do
  let h = "Access-Control-Allow-Origin: *\r\n\r\n"
  c <- case rlookup (mtd, endpoint) Hapi.routing of
        Nothing -> do
          sendHeader conn st Cplain 0
          return "There is no valid api."
        Just (_,_,func,ct) -> do
          sendHeader conn st ct 0
          fmap B8.encodeString (func qry bdy)
  sendMany conn $ map C.pack [ h, c, "\r\n" ]
  `catch`
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

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

dlookup :: String -> [(String, String)] -> String
dlookup d dict =
  case lookup d Config.subdomain of
    Just x  -> x
    Nothing -> ""

rlookup :: (Method, Endpoint) -> [Api] -> Maybe Api
rlookup _ [] = Nothing
rlookup me@(mtd, ep) ((x@(mtd',ep',_,_)):xs)
  | mtd == mtd' && ep == ep' = Just x
  | otherwise                = rlookup me xs

