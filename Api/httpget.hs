{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import Text.HTML.TagSoup (parseTags, Tag(..))
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
  res <- httpLBS =<< parseRequest "https://example.com"
  print . findTitle . parseTags $ (getResponseBody res)
  -- L8.putStrLn $ (getResponseBody res)

findTitle ((TagOpen "title" _):(TagText x):xs) = x
findTitle ((TagOpen "TITLE" _):(TagText x):xs) = x
findTitle [] = "no title"
findTitle (x:xs) = findTitle xs
