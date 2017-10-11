{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad
import Data.Aeson
import Data.Maybe (catMaybes)
import GHC.Generics

import Api.Hapi


(f >< g) (a,b) = (f a, g b)

tests = map ((sample . show . map show) >< id) [
      ([103, 202, 302],      SResponse  True 810  [ "103", "202", "302" ] [])
    , ([107, 202, 302],      SResponse2 False "item_not_found")
    , ([103, 202, 302, 104], SResponse  True 1130 [ "103", "202", "302", "104" ] [])
    , ([103, 202, 302, 304], SResponse  True 960  [ "103", "202", "302", "304" ] [])
  ]
  where
    sample l = concat [ "{ \"order\" : ", l, " }" ]


main :: IO ()
main =
  forM_ tests $ \(x, y) -> do
    res <- post2check x
    let q = getResponseBody res
    putStr "\nInput order  ==> "
    putStrLn x
    putStr "Content-Type ==> "
    print $ getResponseHeader "Content-Type" res
    putStr "ResponseBody ==> "
    L8.putStr q
    putStr "Answer is    ==> "
    L8.putStrLn (encode y)
    case decode q of
      Nothing -> putStrLn "ERROR: invalid json"
      Just  w
        | w == y    -> putStrLn "OK!!"
        | otherwise -> putStrLn "BAD!!"

post2check :: String -> IO (Response L8.ByteString)
post2check smpl = do
  let req' = setRequestMethod "POST" "http://localhost:8080/api/checkout"
      req  = setRequestBodyLBS (L8.pack smpl) req'
  httpLBS req

