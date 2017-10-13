{-# LANGUAGE ExistentialQuantification           #-}
{-# LANGUAGE MultiWayIf, RankNTypes, BangPatterns#-}
module Web.Haskyapi.Console.Cli (
  argparse,
  Option(..),
  Mode(..),
) where

import System.Environment (getArgs)
import Control.Monad.State
import Control.Applicative ((<$>))
import qualified Data.List.Split as L
import qualified Data.List       as L
import Data.Maybe (fromMaybe)

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
    ,Arg ["-i", "--ip"  ] "localhost" "ip"   "IP"
    ,Arg ["-h", "--help"] "...."      "help" "Help"
  ]

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
    checkmode ("--version":_)  = Message "0.0.0.1"
    checkmode ("-v":_)         = Message "0.0.0.1"
    checkmode ("--help":_)     = Message $ mkHelpAll
    checkmode ("-h":_)         = Message $ mkHelpAll
    checkmode _                = Error "invalid mode!!"
    mkHelpAll :: String
    mkHelpAll = concat ["haskyapi runserver\n", aux argConfMigrate, "haskyapi migrate\n", aux argConfRunserver]
      where
        aux = unlines . map aux'
        aux' (Arg forms defa nm desc) = L.intercalate "\t" [ "\t" ++ eqleng 6 nm, eqleng 11 (unwords forms), "defaulut = " ++ eqleng 8 defa, desc]
        eqleng th xs =
          let !l = length xs in
          if | length xs <= th -> xs ++ replicate (th-l) ' '
             | otherwise       -> xs
    argparseMain :: [Arg] -> [String] -> ([A] -> Mode [A]) -> Mode Option
    argparseMain argconf xs mode = a2optMain $ execState (argparse' xs) (mode [])
      where
        mkHelp :: String
        mkHelp = unlines $ map aux argconf
          where
            aux (Arg forms defa nm desc) = unwords [nm, unwords forms, "defaulut =", defa, desc]
        argparse' :: [String] -> State (Mode [A]) ()
        argparse' [] = return ()
        argparse' ("-h":_)        = modify $ \x -> Message mkHelp
        argparse' ("--help":_)    = modify $ \x -> Message mkHelp
        argparse' [x]          = modify $ \_ -> Error ("error near " ++ x)
        argparse' (ag:x:xs) =
          case filter (elem ag . key) argconf of
            [a] -> modify (fmap ((name a, x):)) >> argparse' xs
            _   -> modify $ \x -> Error ("invalid argument " ++ ag)
        a2optMain :: Mode [A] -> Mode Option
        a2optMain (Runserver as) = Runserver (a2opt as)
        a2optMain (Migrate   as) = Migrate OptMigrate
        a2optMain _ = Error "error"
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


main :: IO ()
main = do
  print $ argparse ["runserver", "--root", "html"]
  print $ argparse ["migrate", "--a"]
  print $ argparse ["migrate", "--help"]
  print $ argparse ["migrate"]
  print $ argparse ["-v"]

