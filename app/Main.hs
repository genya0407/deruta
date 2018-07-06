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

data CommonItem = CommonItem { title :: Text, url :: Text, author :: Text, published :: UTCTime } deriving Show

fromAtomEntry :: AF.Entry -> Maybe CommonItem
fromAtomEntry entry = do
    let t = (pack . AF.txtToString . AF.entryTitle) entry
    u <- ((fmap AF.linkHref) . listToMaybe . AF.entryLinks) entry
    a <- ((fmap AF.personName) . listToMaybe . AF.entryAuthors) entry
    pText <- AF.entryPublished entry
    p <- (parseTimeStringISO8601 . unpack) pText
    return CommonItem { title = t , url = u, author = a , published = p }

fromRss2Entry :: R2S.RSSItem -> Maybe CommonItem
fromRss2Entry item = do
    t <- R2S.rssItemTitle item
    u <- R2S.rssItemLink item
    a <- R2S.rssItemLink item
    pText <- R2S.rssItemPubDate item
    p <- parseTimeStringRFC822 . unpack $ pText

    return CommonItem { title = t, url = u, author = a, published = p }

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
composeFeeds ((AtomFeed feed):rest) = (catMaybes . map fromAtomEntry . AF.feedEntries $ feed) ++ (composeFeeds rest)
composeFeeds ((RSSFeed feed):rest) = (catMaybes . map fromRss2Entry . R2S.rssItems . R2S.rssChannel $ feed) ++ (composeFeeds rest)
composeFeeds _ = []
