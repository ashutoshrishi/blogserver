{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (toJSON, object, (.=), Value (Null))
import           Data.Monoid (mconcat)
import           Data.Text
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import           Model
import           Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (simpleCors)
import           Network.Wai.Handler.Warp (defaultSettings)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.HTTP.Types.Status (notFound404)
import           System.Environment (lookupEnv)
import           Web.Scotty


main :: IO () 
main = getConfig >>= runServer


-- * Configuration

data Config = Config
    { environment :: Environment }

data Environment = Development | Production
    deriving (Show, Eq, Read)

getConfig :: IO Config 
getConfig = getEnvironment >>= return . Config
    

getEnvironment :: IO Environment
getEnvironment = (maybe Development read) <$> lookupEnv "SCOTTY_ENV"
        
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

runServer :: Config -> IO ()
runServer c = do
    let env = environment c
    opts <- getOptions env
    scottyOpts opts (application env)


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
    id <- param "id"
    maybeEnt <- liftIO $ getPostById (read id :: Integer)
    maybe notFoundA json maybeEnt
    


notFoundA :: ActionM ()
notFoundA = do
    status notFound404
    json Null 
