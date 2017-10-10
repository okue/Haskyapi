{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

sample l = "{ \"order\" : " ++ l ++ " }"

s1 = sample . show $ map show [103, 202, 302]
s2 = sample . show $ map show [107, 202, 302]

main :: IO ()
main = do
  let req' = setRequestMethod "POST" "http://localhost:8080/api/checkout"
      req  = setRequestBodyLBS (L8.pack s1) req'
  res <- httpLBS req
  print $ getResponseHeader "Content-Type" res
  L8.putStrLn $ getResponseBody res

