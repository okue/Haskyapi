{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Console.Cli (
  argparse,
  Option(..)
) where

import System.Environment (getArgs)
import Control.Monad.State
import Control.Applicative ((<$>))
import qualified Data.List.Split as L
import qualified Data.List       as L
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

data Arg = forall a. Show a => Arg {
             key   :: [String],
             def   :: String,
             name  :: String,
             func  :: Option -> a,
             description :: String
         }

type A = (String, String)

data Option = Option {
  oport :: String,
  oroot :: String,
  oip   :: String
} deriving (Show)

initOptin = Option "" "" ""

argConfs :: [Arg]
argConfs = [
     Arg ["-p", "--port"] "8080"      "port" oport "Port number"
    ,Arg ["-r", "--root"] "html"      "root" oroot "Root directory"
    ,Arg ["-i", "--ip"  ] "localhost" "ip"   oip   "IP"
    ,Arg ["-h", "--help"] "...."      "help" id    "Help"
  ]

a2opt :: [A] -> Option
a2opt as = execState (aux argConfs) initOptin
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

mkHelp :: String
mkHelp = unlines $ map aux argConfs
  where
    aux (Arg forms defa nm _ desc) =
      unwords [nm, unwords forms, "defaulut =", defa, desc]

startState :: Either String [A]
startState = Right []

argparse :: [String] -> Either String Option
argparse args = a2opt <$> execState (argparse' args) startState
  where
    argparse' :: [String] -> State (Either String [A]) ()
    argparse' [] = return ()
    argparse' ("-h":_)     = modify $ \x -> Left mkHelp
    argparse' ("--help":_) = modify $ \x -> Left mkHelp
    argparse' [x]          = modify $ \_ -> Left ("error near " ++ x)
    argparse' (ag:x:xs) =
      case filter (elem ag . key) argConfs of
        [a] -> modify (fmap ((name a, x):)) >> argparse' xs
        _   -> modify $ \x -> Left ("invalid argument " ++ ag)

main :: IO ()
main = do
  args <- getArgs
  print $ argparse args

