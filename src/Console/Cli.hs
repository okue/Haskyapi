{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Console.Cli (
  argparse,
  Option(..)
) where

import System.Environment (getArgs)
import Control.Monad.State
import qualified Data.List.Split as L
import qualified Data.List       as L
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

data Option = Option {
  oport :: String,
  oroot :: String,
  oip   :: String
} deriving (Show)

initOptin = Option "" "" ""

data Arg = forall a. Show a => Arg {
             key   :: [String],
             def   :: String,
             name  :: String,
             func  :: Option -> a,
             description :: String
         }

type A = (String, String)

a2opt :: [A] -> Option
a2opt as = execState (aux argConfs) $ initOptin
  where
    aux :: [Arg] -> State Option ()
    aux [] = return ()
    aux (acf:acfs) =
      case lookup (name acf) as of
        Nothing | name acf == "port" -> modify (\x -> x { oport = def acf }) >> aux acfs
        Just a  | name acf == "port" -> modify (\x -> x { oport = a }) >> aux acfs
        Nothing | name acf == "root" -> modify (\x -> x { oroot = def acf }) >> aux acfs
        Just a  | name acf == "root" -> modify (\x -> x { oroot = a }) >> aux acfs
        Nothing | name acf == "ip"   -> modify (\x -> x { oip   = def acf }) >> aux acfs
        Just a  | name acf == "ip"   -> modify (\x -> x { oip   = a }) >> aux acfs
        _ -> aux acfs

argConfs :: [Arg]
argConfs = [
     Arg ["-p", "--port"] "8080"      "port" oport "Port number"
    ,Arg ["-r", "--root"] "html"      "root" oroot "Root directory"
    ,Arg ["-h", "--help"] "...."      "help" id    "Help"
    ,Arg ["-i", "--ip"  ] "localhost" "ip"   oip   "IP"
  ]

mkHelp :: String
mkHelp = unlines $ map aux argConfs
  where
    aux (Arg forms defa nm _ desc) =
      L.intercalate " " [nm, (L.intercalate " " forms), "defaulut =", defa, desc]

startState :: Either String [A]
startState = Right []

argparse :: [String] -> Either String Option
argparse args = fmap a2opt $ execState (argparse' args) startState
  where
    argparse' :: [String] -> State (Either String [A]) ()
    argparse' [] = return ()
    argparse' ("-h":_)     = modify $ \x -> Left mkHelp
    argparse' ("--help":_) = modify $ \x -> Left mkHelp
    argparse' (x:[])       = modify $ \_ -> Left ("error near " ++ x)
    argparse' (ag:x:xs) =
      case filter (\a -> any (ag ==) (key a)) argConfs of
        [a] -> do
          modify $ fmap ((name a, x):)
          argparse' xs
        _   ->
          modify $ \x -> Left ("invalid argument " ++ ag)

main :: IO ()
main = do
  args <- getArgs
  print $ argparse args
