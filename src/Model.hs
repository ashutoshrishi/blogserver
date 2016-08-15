{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model (
    Post(..), runDb, doMigration,
    -- * interface
    insertPost, getNPosts, getPostBySlug, getPostById
    ) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Database.Persist.Postgresql

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)

import Data.Text
import Data.Time.Clock (UTCTime)
import qualified Data.List as List


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post json
    title Text
    content Text
    slug String
    created UTCTime default=now()
    UniqueSlug slug
    deriving Show
|]
    

runDb :: SqlPersistM a -> IO a
runDb query = do
    let connStr = "host=localhost user=rishi dbname=rishi" 
    runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
        liftIO $ runSqlPersistMPool query pool


doMigration :: IO ()
doMigration = runDb (runMigration migrateAll)

-- INTERFACE

insertPost :: Post -> IO (Key Post)
insertPost post = runDb (insert post)


getNPosts :: Int -> IO [Entity Post]
getNPosts n = runDb $
    selectList ([] :: [Filter Post]) [LimitTo n, Desc PostCreated]


getPostBySlug :: String -> IO (Maybe (Entity Post))
getPostBySlug s = runDb $ getBy $ UniqueSlug s


getPostById :: Integer -> IO (Maybe (Entity Post))
getPostById i = runDb $ do
    posts <- selectList [PostId ==. toKey i] []
    if List.null posts
    then return Nothing
    else return $ Just (List.head posts)


-- * Utilities
toKey :: ToBackendKey SqlBackend a => Integer -> Key a
toKey i = toSqlKey (fromIntegral (i :: Integer))

-- main :: IO ()
-- main = runDb $ do
--     time <- liftIO getCurrentTime
--     postId <- insert $ Post "Bar" "Second Random content" "bar" time 
--     post <- get postId
--     liftIO $ print post    

    
    
