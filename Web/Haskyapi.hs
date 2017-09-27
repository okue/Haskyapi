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
            deriving (Show)

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
      apisender OK conn ct endpoint qry mtd
    (GET, path) -> do
      html <- C.readFile $ "./html" ++ path
      sender OK conn ct html
      `catch`
        \(SomeException e) -> do
          print e
          sender NotFound conn ct $ C.pack "404 Not Found"
    (POST, _) ->
      sender OK conn ct $ C.pack "Please POST request to /api"
    _ ->
      sender NotFound conn ct $ C.pack "404 Not Found"

sender :: Status -> Socket -> ContentType -> C.ByteString -> IO ()
sender st conn ct msg = do
  sendHeader st conn ct
  send conn $ msg
  send conn $ C.pack "\r\n"
  return ()
  `catch`
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

apisender :: Status -> Socket -> ContentType -> Endpoint -> Query -> Method -> IO ()
apisender st conn ct endpoint qry mtd = do
  sendHeader st conn ct
  case rlookup (mtd, endpoint) Hapi.routing of
    Nothing         -> send conn $ C.pack "There is no valid api."
    Just (_,_,func) -> send conn $ C.pack $ func qry
  send conn $ C.pack "\r\n"
  return ()
  `catch`
    (\(SomeException e) -> print e)
  `finally`
    close conn >> putStrLn ("close conn " ++ show conn)

sendHeader :: Status -> Socket -> ContentType -> IO Int
sendHeader st conn ct = do
  case st of
    OK       -> send conn $ C.pack "HTTP/1.1 200 OK\r\n"
    NotFound -> send conn $ C.pack "HTTP/1.1 404 Not Found\r\n"
  send conn $ C.pack $ "Content-Type: " ++ show ct ++ "\r\n"
  send conn $ C.pack "Server: Haskyapi\r\n"
  send conn $ C.pack "\r\n"

rlookup :: (Method, Endpoint) -> [Api] -> Maybe Api
rlookup _ [] = Nothing
rlookup me@(mtd, ep) ((x@(mtd',ep',_)):xs)
  | mtd == mtd' && ep == ep' = Just x
  | otherwise                = rlookup me xs
