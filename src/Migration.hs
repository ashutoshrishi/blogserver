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
    posts <- parseSource (Disk "_posts")
    insertPosts posts


parseSource :: Source -> IO [Post]
parseSource (Disk dir) = do
    allFiles <- catchIOError (listDirectory dir) (\_ -> return [])
    catMaybes <$> mapM parsePostFile allFiles
    


insertPosts :: [Post] -> IO ()
insertPosts = mapM_ insertPost 



