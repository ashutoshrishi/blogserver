{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Present a simple REST API to fetch and add new blog posts.
Copyright   : (c) Ashutosh Rishi Ranjan, 2016
Maintainer  : ashutoshrishi92@gmail.com
Stability   : experimental
-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (gets, StateT, evalStateT)
import           Data.Aeson (object, (.=), Value (Null))
import           Model
import           Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (simpleCors)
import           Network.Wai.Handler.Warp (defaultSettings)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.HTTP.Types.Status (notFound404)
import           System.Environment (lookupEnv)
import           Web.Scotty
-- import Migration


main :: IO ()
main = do
    env <- getEnvironment
    evalStateT server (ServerState env)


-- |State monad holding the configuration of a running server.
data ServerState = ServerState
    { environment :: Environment }

-- |Hold the server state over the standard IO monad.
type Server = StateT ServerState IO


-- |Represents different environment variables that can be set.
data Environment = Development | Production
    deriving (Show, Eq, Read)


getEnvironment :: IO Environment
getEnvironment = maybe Development read <$> lookupEnv "SCOTTY_ENV"




-- | Derive Scotty options from the server configuration
getOptions :: Environment -> IO Options
getOptions e = do
    let v = case e of
                Development -> 1
                Production -> 0
    -- For now we are going with default Warp settings
    return $ Options v defaultSettings



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
