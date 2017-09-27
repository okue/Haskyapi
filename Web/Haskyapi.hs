#! /usr/bin/env runhaskell
module Web.Haskyapi (
  runServer
) where

-- http://hackage.haskell.org/package/network-2.4.0.1/docs/Network-Socket.html
-- http://qiita.com/asukamirai/items/522cc3c07d7d9ad21dfa

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
-- import Codec.Binary.UTF8.String as BUS
import qualified Data.ByteString.Char8 as C
import qualified Data.List.Split as S
import qualified Data.List as L
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
  show OK = "200 OK"
  show NotFound = "404 Not Found"
  show Moved = "301 Moved Permanently"

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
      case complement path of
        Just cpath -> do
          html <- C.readFile $ "./html" ++ cpath
          sender OK conn ct html
        Nothing ->
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
    "" -> Just $ path ++ "index.html"
    _
      | length (S.splitOn "." basename) > 1 -> Just path
      | otherwise -> Nothing

redirect :: Socket -> String -> IO ()
redirect conn path = do
  sendHeader conn Moved Chtml
  print path
  send conn $ C.pack "Location: /vv/\r\n"
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
    Just (_,_,func) -> send conn $ C.pack $ func qry
  send conn $ C.pack "\r\n"
  return ()
  `catch`
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

sendHeader :: Socket -> Status -> ContentType -> IO Int
sendHeader conn st ct = do
  send conn $ C.pack $ "HTTP/1.1 "      ++ show st ++ "\r\n"
  send conn $ C.pack $ "Content-Type: " ++ show ct ++ "\r\n"
  send conn $ C.pack   "Server: Haskyapi\r\n"

rlookup :: (Method, Endpoint) -> [Api] -> Maybe Api
rlookup _ [] = Nothing
rlookup me@(mtd, ep) ((x@(mtd',ep',_)):xs)
  | mtd == mtd' && ep == ep' = Just x
  | otherwise                = rlookup me xs
