module Migration ( migrate ) where

import Control.Monad.IO.Class (liftIO)
import PostParser
import Model
import Data.Maybe (catMaybes)
import System.Directory (listDirectory)
import System.IO.Error (catchIOError)
import Types
import Control.Monad.Trans.State.Lazy (gets)


migrate :: Server ()
migrate = do
    sources <- gets postSources
    logMsg $ "Attempting migration on sources: " ++ show sources
    liftIO $ mapM_ migrateFromSource sources


migrateFromSource :: Source -> IO ()
migrateFromSource source = do
    ps <- parseSource source
    results <- mapM insertPost ps
    mapM_ (\(p, r) ->
               case r of
                   Nothing ->
                       putStrLn $ "xxx Post already exists: " ++ postSlug p
                   Just _ ->
                       putStrLn $ "+++ Inserted: " ++ postSlug p
          ) (zip ps results)


parseSource :: Source -> IO [Post]
parseSource (Disk dir) = do
    allFiles <- catchIOError (listDirectory dir) (\_ -> return [])
    catMaybes <$> mapM (parsePostFile . (dir ++)) allFiles
