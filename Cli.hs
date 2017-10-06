import System.Environment (getArgs)
import Debug.Trace (trace)
import Control.Monad.State

data Arg = Arg {
             key   :: [String],
             def   :: String,
             name  :: String,
             description :: String
         }
         deriving (Show)
data AStatus = AOK
             | AError String
             deriving (Show)

aconf :: [Arg]
aconf = [
     Arg ["-p", "--port"] "8080" "port" "Port number"
    ,Arg ["-r", "--root"] "html" "root" "Root directory"
  ]

type A = (String, String)
type AState = (Bool, [A])
startState = (False, [])
argparse :: [Arg] -> [String] -> State AState [A]
argparse ac [] = do
  (_, as) <- get
  return as
argparse ac (x:[]) = argparse ac [] -- Error
argparse ac (ag:x:xs) = do
  (on, as) <- get
  case filter (\a -> any (ag ==) (key a)) ac of
    [a] -> do
      put (on, (name a,x):as)
      return as
    _ ->
      return as
  argparse ac xs

main :: IO ()
main = do
  args <- getArgs
  print $ evalState (argparse aconf args) startState
