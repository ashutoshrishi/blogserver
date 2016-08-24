module Migration ( migrate ) where

import PostParser
import Model
import Data.Maybe (catMaybes)
import System.Directory (listDirectory)
import System.IO.Error (catchIOError)


-- |All sources that can contain markdown posts.
data Source = Disk FilePath



migrate :: IO ()
migrate = do
    ps <- parseSource (Disk "_posts/")
    results <- mapM insertPost ps
    mapM_ (\(p, r) ->
               case r of
                   Nothing ->
                       putStrLn $ "xxx Post already exists: " ++ postSlug p
                   Just _ -> putStrLn $ "+++ Inserted: " ++ postSlug p
          ) (zip ps results)


parseSource :: Source -> IO [Post]
parseSource (Disk dir) = do
    allFiles <- catchIOError (listDirectory dir) (\_ -> return [])
    catMaybes <$> mapM (parsePostFile . (dir ++)) allFiles
