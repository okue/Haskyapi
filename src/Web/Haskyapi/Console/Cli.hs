{-# LANGUAGE ExistentialQuantification           #-}
{-# LANGUAGE MultiWayIf, RankNTypes, BangPatterns#-}
module Web.Haskyapi.Console.Cli (
  haskyapi,
  argparse,
  Option(..),
  Mode(..),
) where

import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit
import Control.Monad.State
import Control.Applicative ((<$>))
import qualified Data.List.Split as L
import qualified Data.List       as L
import Data.Maybe (fromMaybe)


import qualified Web.Haskyapi.Config.Config as Config
import Web.Haskyapi (runServer, Port)
import Web.Haskyapi.Header (Api)


--------------------------------------------------------
-- command option
--------------------------------------------------------
-- haskyapi --help
-- haskyapi --version
-- haskyapi migrate
-- haskyapi migrate --help
-- haskyapi runserver --help
-- haskyapi runserver --root ... --port ... --ip ...
--------------------------------------------------------

data Mode a = Runserver a
            | Migrate   a
            | Message String
            | Error String
            deriving (Show)

mode2str :: Mode a -> String
mode2str (Runserver _) = "haskyapi runserver\n"
mode2str (Migrate   _) = "haskyapi migrate\n"
mode2str (Message   _) = "message"
mode2str (Error     _) = "error"

instance Functor Mode where
  fmap f (Runserver x) = Runserver (f x)
  fmap f (Migrate x)   = Migrate (f x)
  fmap f (Message x)   = Message x
  fmap _ (Error x)     = Error x

data Arg = Arg {
             key   :: [String],
             def   :: String,
             name  :: String,
             description :: String
         }

type A = (String, String)

data Option = OptRunserver {
                oport :: String,
                oroot :: String,
                oip   :: String
              }
            | OptMigrate
            deriving (Show)

argConfMigrate :: [Arg]
argConfMigrate = [
     Arg ["-h", "--help"] "...."      "help" "Help"
  ]

argConfRunserver :: [Arg]
argConfRunserver = [
     Arg ["-p", "--port"] "8080"      "port" "Port number"
    ,Arg ["-r", "--root"] "html"      "root" "Root directory"
    -- This part has to be Fixed ----------------
    ,Arg ["-i", "--ip"  ] "null"      "ip"   "IP"
    ---------------------------------------------
    ,Arg ["-h", "--help"] "...."      "help" "Help"
  ]

verMessage = "haskyapi version 0.0.0.1"

argparse :: [String] -> Mode Option
argparse args =
  case checkmode args of
    Error x      -> Error x
    Message x    -> Message x
    Runserver xs -> argparseMain argConfRunserver xs Runserver
    Migrate   xs -> argparseMain argConfMigrate   xs Migrate
  where
    checkmode :: [String] -> Mode [String]
    checkmode ("runserver":xs) = Runserver xs
    checkmode ("migrate"  :xs) = Migrate xs
    checkmode ("--version":_)  = Message verMessage
    checkmode ("-v":_)         = Message verMessage
    checkmode ("--help":_)     = Message mkHelpAll
    checkmode ("-h":_)         = Message mkHelpAll
    checkmode _                = Error "invalid mode!!"
    mkHelpAll :: String
    mkHelpAll = mkHelp [("haskyapi migrate\n", argConfMigrate), ("haskyapi runserver\n", argConfRunserver)]
    mkHelp lst = concat $ foldl (\l (x,y) -> [x, aux y] ++ l ) [] lst
      where
        aux = unlines . map aux'
        -- Arg's Example => Arg ["-p", "--port"] "8080" "port" "Port number"
        aux' (Arg forms defa nm desc) = L.intercalate "\t" [ "\t" ++ eqleng 6 nm,
                                                             eqleng 11 (unwords forms),
                                                             "defaulut = " ++ eqleng 8 defa,
                                                             desc
                                                           ]
        eqleng th xs =
          let !l = length xs in
          if | length xs <= th -> xs ++ replicate (th-l) ' '
             | otherwise       -> xs
    argparseMain :: [Arg] -> [String] -> ([A] -> Mode [A]) -> Mode Option
    argparseMain argconf xs mode = a2optMain $ execState (argparse' xs) (mode [])
      where
        argparse' :: [String] -> State (Mode [A]) ()
        argparse' [] = return ()
        argparse' ("-h":_)        = modify $ \x -> Message $ mkHelp [(mode2str (mode []), argconf)]
        argparse' ("--help":_)    = modify $ \x -> Message $ mkHelp [(mode2str (mode []), argconf)]
        argparse' [x]             = modify $ \_ -> Error ("error near " ++ x)
        argparse' (ag:x:xs) =
          case filter (elem ag . key) argconf of
            [a] -> modify (fmap ((name a, x):)) >> argparse' xs
            _   -> modify $ \x -> Error ("invalid argument " ++ ag)
        a2optMain :: Mode [A] -> Mode Option
        a2optMain (Runserver as) = Runserver (a2opt as)
        a2optMain (Migrate   as) = Migrate OptMigrate
        a2optMain (Message x)    = Message x
        a2optMain (Error x)      = Error x
        a2opt :: [A] -> Option
        a2opt as = execState (aux argconf) (OptRunserver "" "" "")
          where
            aux :: [Arg] -> State Option ()
            aux [] = return ()
            aux (acf:acfs) =
              let nm = name acf
                  a  = fromMaybe (def acf) $ lookup (name acf) as in
              if | nm == "port" -> modify (\x -> x { oport = a }) >> aux acfs
                 | nm == "root" -> modify (\x -> x { oroot = a }) >> aux acfs
                 | nm == "ip"   -> modify (\x -> x { oip   = a }) >> aux acfs
                 | otherwise    -> aux acfs


-- Main command which users' applications call
haskyapi :: [Api] -> IO () -> IO ()
haskyapi routing migrate = do
  args <- getArgs
  case argparse args of
    Error x ->
      putStrLn x
    Message x ->
      putStrLn x
    Runserver opt ->
      mainProc opt
    Migrate opt ->
      migrate
  where
    mainProc :: Option -> IO ()
    mainProc !opt = do
      cip <- Config.ip
      csd <- Config.subdomain
      let !root = oroot opt
          !port = oport opt
          !_ip  = oip   opt
          !ip   = if _ip == "null" then cip else _ip
          !url  = "http://" ++ ip ++ ":" ++ port ++ "/"
      putStrLn $ "root: "     ++ root
      putStrLn $ "listen on " ++ port
      putStrLn url
      mapM_ (putStrLn . \h -> url ++ h) =<< getfiles root
      runServer (port, root, ip, csd, routing)
      where
        getfiles :: FilePath -> IO [FilePath]
        getfiles root =
          filter aux <$> getDirectoryContents root
          where
            aux ('.':_) = False
            aux _ = True


main :: IO ()
main = do
  print $ argparse ["runserver", "--root", "html"]
  print $ argparse ["migrate", "--help"]
  let Message a = argparse ["runserver", "--help"]
  putStrLn a
  print $ argparse ["migrate"]
  print $ argparse ["-v"]
