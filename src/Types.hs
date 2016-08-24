{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Server, ServerState(..), initServerState
    -- * Configuration types
    , Environment(..), getEnvironment, getOptions, Source(..)
    , getConfig, parseCmdLine
    , logMsg

    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, modify)
import Network.Wai.Handler.Warp (defaultSettings)
import System.Environment (lookupEnv, getArgs)
import Web.Scotty (Options(..))
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BL
import           System.Console.GetOpt



-- |State monad holding the configuration of a running server.
data ServerState = ServerState
    { environment     :: Environment
    , postSources     :: [Source]
    , optRunMigration :: Bool
    }


initServerState :: ServerState
initServerState = ServerState
    { environment = Development
    , postSources = []
    , optRunMigration = False
    }


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


-- |All sources that can contain markdown posts.
data Source = Disk FilePath


instance Show Source where
    show (Disk f) = "DIR: " ++ f


data ConfigFile = ConfigFile
    { sources :: [Source] }


instance FromJSON ConfigFile where
    parseJSON (Object v) = do
        ss <- v .: "sources"
        diskSources <- ss .:? "disk" .!= ([] :: [FilePath])
        return $ ConfigFile (Disk <$> diskSources)

    parseJSON c = typeMismatch "ConfigFile" c


-----------------------------------------------------------------------------
-- Config Parsing                                                          --
-----------------------------------------------------------------------------


getConfig :: FilePath -> Server ()
getConfig f = do
    maybeConf <- parseConfigFile f
    case maybeConf of
        -- update config
        Just conf -> do
            liftIO $ putStrLn $ "Adding sources: " ++ show (sources conf)
            modify (\s -> s { postSources = sources conf ++ postSources s })
        -- keep config
        Nothing -> return ()



parseConfigFile :: FilePath -> Server (Maybe ConfigFile)
parseConfigFile f = do
    confStr <- liftIO $ BL.readFile f
    return $ decode confStr


-----------------------------------------------------------------------------
-- Command Line Options                                                    --
-----------------------------------------------------------------------------

optDescs :: [OptDescr (ServerState -> ServerState)]
optDescs =
    [ Option "m" ["migrate"]
      ( NoArg (\s -> s { optRunMigration = True }) )
      "Run post migrations from specified sources."
    ]


parseCmdLine :: Server ()
parseCmdLine = do
    argv <- liftIO getArgs
    case getOpt Permute optDescs argv of
        (os, _, []) -> mapM_ modify os
        (_, _, errs) ->
            do liftIO $ mapM_ putStrLn errs
               error "Error parsing command line."



logMsg :: String -> Server ()
logMsg = liftIO . putStrLn
