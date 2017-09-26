{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Api.Model

where
import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Person
    name String
    age Int Maybe
    deriving Show
  BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main = runSqlite "./haskyapi.db" $ do
  runMigration migrateAll

  johnId <- insert $ Person "John Doe" $ Just 35
  janeId <- insert $ Person "Jane Doe" Nothing

  insert $ BlogPost "My fr1st p0st" johnId
  insert $ BlogPost "One more for good measure" johnId

  oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
  liftIO $ print (oneJohnPost :: [Entity BlogPost])
  john <- get johnId
  liftIO $ print (john :: Maybe Person)
  delete janeId
  deleteWhere [BlogPostAuthorId ==. johnId]
