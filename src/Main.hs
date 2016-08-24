{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Present a simple REST API to fetch and add new blog posts.
Copyright   : (c) Ashutosh Rishi Ranjan, 2016
Maintainer  : ashutoshrishi92@gmail.com
Stability   : experimental
-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (gets, evalStateT)
import Data.Aeson (object, (.=), Value (Null))
import Model
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.HTTP.Types.Status (notFound404)
import Web.Scotty
import Types

-- import Migration


main :: IO ()
main = do
    env <- getEnvironment
    evalStateT server (ServerState env)



-- | Logging middleware.
loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout


-- * Main runners

-- | Actual scotty server, runs the scotty app which listens on routes.
server :: Server ()
server = do
    env <- gets environment
    opts <- liftIO $ getOptions env
    liftIO $ scottyOpts opts (application env)


application :: Environment -> ScottyM ()
application env = do
    middleware (loggingM env)
    middleware simpleCors
    get "/" (text "Hello, World!")
    get "/posts" getPostsA
    get "/post/slug/:slug" getPostBySlugA
    get "/post/id/:id" getPostByIdA
    notFound notFoundA


-- * Actions

-- | Action for route "/posts".
getPostsA :: ActionM ()
getPostsA = do
    ens <- liftIO $ getNPosts 10
    json $ object ["posts" .= ens]


getPostBySlugA :: ActionM ()
getPostBySlugA = do
    slug <- param "slug"
    maybeEnt <- liftIO $ getPostBySlug slug
    maybe notFoundA json maybeEnt


getPostByIdA :: ActionM ()
getPostByIdA = do
    postId <- param "id"
    maybeEnt <- liftIO $ getPostById (read postId :: Integer)
    maybe notFoundA json maybeEnt


notFoundA :: ActionM ()
notFoundA = do
    status notFound404
    json Null
