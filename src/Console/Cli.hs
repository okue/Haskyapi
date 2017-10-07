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

data Option = Option {
  oport :: String,
  oroot :: String
} deriving (Show)

data Arg = forall a. Arg {
             key   :: [String],
             def   :: String,
             name  :: String,
             func  :: Option -> a,
             description :: String
         }

type A = (String, String)

a2opt as =
  let port = fromMaybe "" $ lookup "port" as
      root = fromMaybe "" $ lookup "root" as
  in Option port root

argConfs :: [Arg]
argConfs = [
     Arg ["-p", "--port"] "8080" "port" oport "Port number"
    ,Arg ["-r", "--root"] "html" "root" oroot "Root directory"
    ,Arg ["-h", "--help"] "...." "help" id    "Help"
  ]

validate :: [A] -> [A]
validate as = map aux argConfs
  where
    aux aconf =
      case lookup (name aconf) as of
        Nothing -> (name aconf, def aconf)
        Just x  -> (name aconf, x)

mkHelp :: String
mkHelp = unlines $ map aux argConfs
  where
    aux (Arg forms defa nm _ desc) =
      L.intercalate " " [nm, (L.intercalate " " forms), "defaulut =", defa, desc]

startState :: Either String [A]
startState = Right []

argparse :: [String] -> Either String Option
argparse args = fmap a2opt . fmap validate $ execState (argparse' argConfs args) startState
  where
    argparse' :: [Arg] -> [String] -> State (Either String [A]) ()
    argparse' aconf []     = return ()
    argparse' aconf ("-h":_)     = modify $ \x -> Left mkHelp
    argparse' aconf ("--help":_) = modify $ \x -> Left mkHelp
    argparse' aconf (x:[]) = modify $ \_ -> Left x
    argparse' aconf (ag:x:xs) =
      case filter (\a -> any (ag ==) (key a)) aconf of
        [a] -> do
          modify $ fmap ((name a, x):)
          argparse' aconf xs
        _   ->
          modify $ \x -> Left ("invalid argument " ++ ag)

main :: IO ()
main = do
  args <- getArgs
  print $ argparse args
