{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Text.Feed.Import (parseFeedFromFile)
import Text.Feed.Types (Feed(AtomFeed, RSSFeed))
import qualified Text.Atom.Feed as AF
import Text.RSS.Syntax as R2S

import Data.Time.Format (parseTimeM, defaultTimeLocale, rfc822DateFormat)
import Data.Time.Clock (UTCTime)
import Data.Text (pack, unpack, Text)
import qualified Data.Text.IO as TI
import Data.Maybe (fromJust)
import Control.Monad (forM)
import Data.List (sortBy)

import Paths_deruta

parseTimeStringISO8601 :: String -> Maybe UTCTime
parseTimeStringISO8601 = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

parseTimeStringRFC822 :: String -> Maybe UTCTime
parseTimeStringRFC822 = parseTimeM True defaultTimeLocale rfc822DateFormat

data CommonItem = CommonItem { title :: Text, url :: Text, author :: Text, published :: UTCTime } deriving Show

fromAtomEntry :: AF.Entry -> CommonItem
fromAtomEntry entry = CommonItem
    { title = (pack . AF.txtToString . AF.entryTitle) entry
    , url = (AF.linkHref . head . AF.entryLinks) entry
    , author = (AF.personName . head . AF.entryAuthors) entry
    , published = (fromJust . parseTimeStringISO8601 . unpack . fromJust . AF.entryPublished) entry
    }

fromRss2Entry :: R2S.RSSItem -> CommonItem
fromRss2Entry item = CommonItem
    { title = (fromJust . R2S.rssItemTitle) item
    , url = (fromJust . R2S.rssItemLink) item
    , author = (fromJust . R2S.rssItemLink) item
    , published = (fromJust . parseTimeStringRFC822 . unpack . fromJust . R2S.rssItemPubDate) item
    }

main :: IO ()
main = do
    feeds <- mapM (\f -> parseFeedFromFile =<< getDataFileName f) ["files/genya0407-atom.xml", "files/genya0407-rss.xml"]
    let commonItems = composeFeeds feeds
    forM (sortBy (\l r -> (published l) `compare` (published r)) commonItems) $ \item -> do
        TI.putStr . title $ item
        TI.putStr "("
        putStr . show . published $ item
        TI.putStrLn ")"
    return ()

composeFeeds :: [Feed] -> [CommonItem]
composeFeeds [] = []
composeFeeds ((AtomFeed feed):rest) = (map fromAtomEntry (AF.feedEntries feed)) ++ (composeFeeds rest)
composeFeeds ((RSSFeed feed):rest) = (map fromRss2Entry ((R2S.rssItems . R2S.rssChannel) feed)) ++ (composeFeeds rest)
composeFeeds _ = []
