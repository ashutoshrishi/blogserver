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

module PostParser ( markdownToPost, ParsedPost, parsePostFile ) where


import           Control.Monad.IO.Class (liftIO)
import           Data.List as List
import Data.Char (toLower)
import qualified Data.Text as T
import           Data.Text.IO as TextIO
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format
import           Model (Post(..))
import           System.FilePath (takeExtension, takeBaseName)
import           Text.Parsec
import           Text.Parsec.Text


-- | Determine the parsing action for a given post file `f` and apply it.
parsePostFile :: FilePath -> IO (Maybe Post)
parsePostFile f
    | takeExtension f == ".markdown" = withPostFile f markdownToPost
    | otherwise = do
          print $ "Cannot determine post type for " ++ f
          return Nothing


-- | Bracket pattern to run an `action` on a post file `pf`, after parsing the
-- filename for initial header fields (created date and slug), which is passed
-- along to the action.
withPostFile :: FilePath -> ([HeaderField] -> FilePath -> IO a) -> IO a
withPostFile pf action = do
    let pre = parseFilePath pf
    action pre pf

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
    let makeTime = parseTimeM True defaultTimeLocale "%F"
    t <- T.pack <$> List.lookup "title" header
    s <- List.map toLower <$> List.lookup "slug" header
    c <- case List.lookup "created" header of
             Just date -> makeTime date
             Nothing -> return time
    return $ Post t body s c



-- | Slug and creation date information, parsed from the title.
parseFilePath :: FilePath -> [HeaderField]
parseFilePath f =
    [ ("slug", drop 11 nm)
    , ("created", take 10 nm)
    ]
  where
    nm = takeBaseName f

-----------------------------------------------------------------------------
-- Different Parsers                                                       --
-----------------------------------------------------------------------------


-- | Transform a blog post file written in markdown to the `Model.Post`
-- type. Also takes an initial list of ParsedPost header fields which may have
-- been derived from elsewhere. These are added to the ones parsed from the
-- post file header.
markdownToPost :: [HeaderField] -> FilePath -> IO (Maybe Post)
markdownToPost pre markfile = do
    postMd <- TextIO.readFile markfile
    time <- liftIO getCurrentTime
    case parse markdownParser markfile postMd of
        Left err -> print err >> return Nothing
        Right (ParsedPost hds body) -> do
            let final = ParsedPost (hds ++ pre) body
            return $ makePostFromParsed time final


-----------------------------------------------------------------------------
-- Custom Markdown parser                                                  --
-----------------------------------------------------------------------------

-- | The Parsec parser
markdownParser :: Parser ParsedPost
markdownParser = do
    let divider = many1 (char '-') <* spaces
    _ <- divider
    header <- markdownHeader
    _ <- divider
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
