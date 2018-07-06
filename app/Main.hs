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
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import Control.Monad (forM)
import Data.List (sortBy)

import Paths_deruta

parseTimeStringISO8601 :: String -> Maybe UTCTime
parseTimeStringISO8601 = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

parseTimeStringRFC822 :: String -> Maybe UTCTime
parseTimeStringRFC822 = parseTimeM True defaultTimeLocale rfc822DateFormat

data CommonItem = CommonItem { itemTitle :: Text, itemUrl :: Text, itemAuthor :: Text, itemPublished :: UTCTime } deriving Show

fromAtomEntry :: AF.Entry -> Maybe CommonItem
fromAtomEntry entry = do
    let title = (pack . AF.txtToString . AF.entryTitle) entry
    url <- ((fmap AF.linkHref) . listToMaybe . AF.entryLinks) entry
    author <- ((fmap AF.personName) . listToMaybe . AF.entryAuthors) entry
    publishedText <- AF.entryPublished entry
    published <- (parseTimeStringISO8601 . unpack) publishedText

    return CommonItem { itemTitle = title , itemUrl = url, itemAuthor = author , itemPublished = published }

fromRss2Entry :: R2S.RSSItem -> Maybe CommonItem
fromRss2Entry item = do
    title <- R2S.rssItemTitle item
    url <- R2S.rssItemLink item
    author <- R2S.rssItemLink item
    publishedText <- R2S.rssItemPubDate item
    published <- parseTimeStringRFC822 . unpack $ publishedText

    return CommonItem { itemTitle = title , itemUrl = url, itemAuthor = author , itemPublished = published }

composeFeeds :: [Feed] -> [CommonItem]
composeFeeds [] = []
composeFeeds ((AtomFeed feed):rest) = (catMaybes . map fromAtomEntry . AF.feedEntries $ feed) ++ (composeFeeds rest)
composeFeeds ((RSSFeed feed):rest) = (catMaybes . map fromRss2Entry . R2S.rssItems . R2S.rssChannel $ feed) ++ (composeFeeds rest)
composeFeeds _ = []

main :: IO ()
main = do
    feeds <- mapM (\f -> parseFeedFromFile =<< getDataFileName f) ["files/genya0407-atom.xml", "files/genya0407-rss.xml"]
    let commonItems = composeFeeds feeds
    forM (sortBy (\l r -> (itemPublished l) `compare` (itemPublished r)) commonItems) $ \item -> do
        TI.putStr . itemTitle $ item
        TI.putStr "("
        putStr . show . itemPublished $ item
        TI.putStrLn ")"
    return ()
