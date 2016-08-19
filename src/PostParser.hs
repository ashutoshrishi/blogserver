{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PostParser
Description : Different conversion mechansisms to prepare a JSON blog post.
Copyright   : (c) Ashutosh Rishi Ranjan, 2016
Maintainer  : ashutoshrishi92@gmail.com
Stability   : experimental

This module presents different interfaces which can be used to transform a
blog post inputted in one form to a JSON form which can be served through the
API.

o Markdown with Metadata to Blog Post

-}

module PostParser (markdownToPost) where


import Text.Parsec
import Text.Parsec.Text
import Data.Text.IO as TextIO
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Applicative ((<$>))
import Model (Post(..))
import Data.List as List


-- | Transform a blog post written in markdown to the `Model.Post` type.
markdownToPost :: FilePath -> IO (Maybe Post)
markdownToPost markfile = do
    postMd <- TextIO.readFile markfile
    time <- liftIO getCurrentTime
    case parse markdownParser markfile postMd of
        Left err -> print err >> return Nothing
        Right p -> return $ makePostFromMarkdown time p
        
    

-----------------------------------------------------------------------------
-- Custom Markdown parser                                                  --
-----------------------------------------------------------------------------

-- * Types to represent a blog post in a markdown file.

data MarkdownPost = MarkdownPost Header Body

instance Show MarkdownPost where
    show (MarkdownPost header body) =
        "--\n" ++ show header ++ "\n--\n" ++ show body

type Header = [HeaderField]

type HeaderField = (String, String)

type Body = T.Text


-- | The Parsec parser
markdownParser :: Parser MarkdownPost
markdownParser = do
    let divider = many1 (char '-') <* spaces
    divider
    header <- markdownHeader
    divider
    body <- markdownBody
    return $ MarkdownPost header body 


markdownHeader :: Parser Header
markdownHeader = many1 mdHeaderField


mdHeaderField :: Parser HeaderField
mdHeaderField = do
    key <- many1 letter <* char ':' <* spaces
    value <- many1 (noneOf "\n") <* newline
    return (key, value)


markdownBody :: Parser Body
markdownBody = T.pack <$> many1 anyChar <* eof


-- | Convert the parsed MarkdownPost type into a `Model.Post`. Takes the
-- default post creation time.
makePostFromMarkdown :: UTCTime -> MarkdownPost -> Maybe Post
makePostFromMarkdown time (MarkdownPost header body) = do
    t <- T.pack <$> List.lookup "title" header
    s <- List.lookup "slug" header
    return $ Post t body s time

