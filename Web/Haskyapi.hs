#! /usr/bin/env runhaskell
module Web.Haskyapi (
  runServer
) where

-- http://hackage.haskell.org/package/network-2.4.0.1/docs/Network-Socket.html
-- http://qiita.com/asukamirai/items/522cc3c07d7d9ad21dfa

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Codec.Binary.UTF8.String as B8
import qualified Data.ByteString.Char8    as C
import qualified Data.List.Split as S
import qualified Data.List       as L
import qualified Text.Markdown     as Md
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (utcToLocalTime, TimeZone(..))
import Control.Concurrent
import Control.Exception
import Control.Monad
import Debug.Trace (trace)

import Web.Haskyapi.Header (
  parse,
  RqLine(..),
  Method(..),
  Header(..),
  Api,
  Endpoint,
  Query,
  ContentType(..),
  toCType,
  )
import qualified Api.Hapi as Hapi

type Port = String

jst :: TimeZone
jst = TimeZone {
        timeZoneMinutes = 540,
        timeZoneSummerOnly = False,
        timeZoneName = "JST"
      }

runServer :: Port -> IO ()
runServer port = do
  soc <- serveSocket port
  listen soc 5
  startSocket soc `finally` close soc

serveSocket :: Port -> IO Socket
serveSocket port = do
  soc  <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr "0.0.0.0"
  setSocketOption soc ReusePort (read port)
  bind soc (SockAddrInet (read port) addr)
  return soc

startSocket :: Socket -> IO ()
startSocket soc = forever $ do
  (conn, addr) <- accept soc
  forkIO $ doResponse conn

data Status = OK
            | NotFound
            | Moved

instance Show Status where
  show OK       = "200 OK"
  show NotFound = "404 Not Found"
  show Moved    = "301 Moved Permanently"

doResponse :: Socket -> IO ()
doResponse conn = do
  (str, _) <- recvFrom conn 1024
  let hdr = parse . S.splitOn "\r\n" $ C.unpack str
      RqLine mtd trg qry = hRqLine hdr
      ct = toCType . last $ S.splitOn "." trg
  putStr "\n"
  print =<< utcToLocalTime jst <$> getCurrentTime
  print hdr
  case (mtd, trg) of
    (GET, "/") -> do
      html <- C.readFile "./html/index.html"
      sender OK conn Chtml html
    (mtd, '/':'a':'p':'i':endpoint) ->
      apisender OK conn Cplain endpoint qry mtd
    (GET, path) -> do
      case (ct, complement path) of
        (Cmarkdown, Just cpath) -> do
          tmp <- T.readFile $ "./html" ++ cpath
          let mdfile = renderHtml $ Md.markdown Md.def tmp
          sender OK conn ct $ C.pack $ T.unpack mdfile
        (_, Just cpath) -> do
          html <- C.readFile $ "./html" ++ cpath
          sender OK conn ct html
        (_, Nothing) ->
          redirect conn path
      `catch`
        \(SomeException e) -> do
          print e
          sender NotFound conn ct $ C.pack "404 Not Found"
    (POST, _) ->
      sender OK conn ct $ C.pack "Please POST request to /api"
    _ ->
      sender NotFound conn Chtml $ C.pack "404 Not Found"

complement :: String -> Maybe String
complement path =
  let basename = last (S.splitOn "/" path) in
  case basename of
    -- path ends with '/'
    "" -> Just $ path ++ "index.html"
    _
      -- basename has a file extension such as ".html"
      | length (S.splitOn "." basename) > 1 -> Just path
      | otherwise -> Nothing

redirect :: Socket -> String -> IO ()
redirect conn path = do
  sendHeader conn Moved Chtml
  print path
  send conn $ C.pack $ "Location: " ++ path ++ "/\r\n"
  send conn $ C.pack "\r\n"
  return ()
  `catch`
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

sender :: Status -> Socket -> ContentType -> C.ByteString -> IO ()
sender st conn ct msg = do
  sendHeader conn st ct
  send conn $ C.pack "\r\n"
  send conn $ msg
  send conn $ C.pack "\r\n"
  return ()
  `catch`
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

apisender :: Status -> Socket -> ContentType -> Endpoint -> Query -> Method -> IO ()
apisender st conn ct endpoint qry mtd = do
  sendHeader conn st ct
  send conn $ C.pack "\r\n"
  case rlookup (mtd, endpoint) Hapi.routing of
    Nothing         -> send conn $ C.pack "There is no valid api."
    Just (_,_,func) -> send conn $ C.pack . B8.encodeString $ func qry
  send conn $ C.pack "\r\n"
  return ()
  `catch`
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

sendHeader :: Socket -> Status -> ContentType -> IO Int
sendHeader conn st ct = do
  send conn $ C.pack $ "HTTP/1.1 "      ++ show st ++ "\r\n"
  send conn $ C.pack $ "Content-Type: " ++ show ct ++ "; charset=utf-8\r\n"
  send conn $ C.pack   "Server: Haskyapi\r\n"

rlookup :: (Method, Endpoint) -> [Api] -> Maybe Api
rlookup _ [] = Nothing
rlookup me@(mtd, ep) ((x@(mtd',ep',_)):xs)
  | mtd == mtd' && ep == ep' = Just x
  | otherwise                = rlookup me xs
