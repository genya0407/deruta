{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Text.Feed.Import (parseFeedFromFile)
import Text.Feed.Types (Feed(AtomFeed, RSSFeed))
import qualified Text.Atom.Feed as AF
import Text.RSS.Syntax as R2S

import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM)
import Data.Text (unpack)
import Data.Maybe (fromMaybe)

import Paths_deruta

data CommonItem = CommonItem { title :: String, url :: String, author :: String, published :: String } deriving Show

fromAtomEntry :: AF.Entry -> CommonItem
fromAtomEntry entry = CommonItem
    { title = (AF.txtToString . AF.entryTitle) entry
    , url = (unpack . AF.linkHref . head . AF.entryLinks) entry
    , author = (unpack . AF.personName . head . AF.entryAuthors) entry
    , published = case (AF.entryPublished entry) of
        Just dateText -> unpack dateText
        Nothing -> (unpack . AF.entryUpdated) entry
    }

fromRss2Entry :: R2S.RSSItem -> CommonItem
fromRss2Entry item = CommonItem
    { title = (unpack . fromMaybe "" . R2S.rssItemTitle) item
    , url = (unpack . fromMaybe "" . R2S.rssItemLink) item
    , author = (unpack . fromMaybe "" . R2S.rssItemLink) item
    , published = (unpack . fromMaybe "" . R2S.rssItemPubDate) item
    }

main :: IO ()
main = do
    feeds <- mapM (\f -> parseFeedFromFile =<< getDataFileName f) ["files/genya0407-atom.xml", "files/genya0407-rss.xml"]
    let commonItems = composeFeeds feeds
    mapM (putStrLn . title) commonItems
    return ()

composeFeeds :: [Feed] -> [CommonItem]
composeFeeds [] = []
composeFeeds ((AtomFeed feed):rest) = (map fromAtomEntry (AF.feedEntries feed)) ++ (composeFeeds rest)
composeFeeds ((RSSFeed feed):rest) = (map fromRss2Entry ((R2S.rssItems . R2S.rssChannel) feed)) ++ (composeFeeds rest)
composeFeeds _ = []