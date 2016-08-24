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

module PostParser ( markdownToPost, ParsedPost, PostType
                  , parsePostFile ) where


import Text.Parsec
import Text.Parsec.Text
import Data.Text.IO as TextIO
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Model (Post(..))
import Data.List as List
import System.FilePath (takeExtension)


-- | Determine the parsing action for a given post file `f` and apply it.
parsePostFile :: FilePath -> IO (Maybe Post)
parsePostFile f
    | takeExtension f == ".markdown" = markdownToPost f
    | otherwise = do
          print $ "Cannot determine post type for " ++ f
          return Nothing


data PostType = MarkdownPost


postExtension :: PostType -> String
postExtension MarkdownPost = ".markdown"



-----------------------------------------------------------------------------
-- Intermediate Types for a simple text Post structure                     --
-----------------------------------------------------------------------------

data ParsedPost = ParsedPost Header Body

type Header = [HeaderField]

type HeaderField = (String, String)

type Body = T.Text


-- | Convert the parsed ParsedPost type into a `Model.Post`. Takes the
-- default post creation time.
makePostFromParsed :: UTCTime -> ParsedPost -> Maybe Post
makePostFromParsed time (ParsedPost header body) = do
    t <- T.pack <$> List.lookup "title" header
    s <- List.lookup "slug" header
    return $ Post t body s time


-----------------------------------------------------------------------------
-- Different Parsers                                                       --
-----------------------------------------------------------------------------


-- | Transform a blog post written in markdown to the `Model.Post` type.
markdownToPost :: FilePath -> IO (Maybe Post)
markdownToPost markfile = do
    postMd <- TextIO.readFile markfile
    time <- liftIO getCurrentTime
    case parse markdownParser markfile postMd of
        Left err -> print err >> return Nothing
        Right p -> return $ makePostFromParsed time p


-----------------------------------------------------------------------------
-- Custom Markdown parser                                                  --
-----------------------------------------------------------------------------

-- | The Parsec parser
markdownParser :: Parser ParsedPost
markdownParser = do
    let divider = many1 (char '-') <* spaces
    divider
    header <- markdownHeader
    divider
    body <- markdownBody
    return $ ParsedPost header body


markdownHeader :: Parser Header
markdownHeader = many1 mdHeaderField


mdHeaderField :: Parser HeaderField
mdHeaderField = do
    key <- many1 letter <* char ':' <* spaces
    value <- many1 (noneOf "\n") <* newline
    return (key, value)


markdownBody :: Parser Body
markdownBody = T.pack <$> many1 anyChar <* eof
