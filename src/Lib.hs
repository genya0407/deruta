{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

module Lib ( createAtomFeed, createRss2Feed, composeFeeds ) where

import Text.Feed.Types (Feed(AtomFeed, RSSFeed))
import Text.Atom.Feed.Export (xmlFeed)
import qualified Text.Atom.Feed as AF
import Text.RSS.Syntax as R2S

import Data.Time.Format (parseTimeM, defaultTimeLocale, rfc822DateFormat, formatTime)
import Data.Time.Clock (UTCTime)
import Data.Text (pack, unpack, Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Text.Lazy.IO as LTI
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import Control.Monad (forM)
import Data.List (sortBy)
import qualified Text.XML as C
import Data.XML.Types as XML
import qualified Data.Text.Lazy as Lazy
import qualified Text.HTML.TagSoup as TS
import GHC.Exts (sortWith)

data CommonItem = CommonItem { itemTitle :: Text, itemUrl :: Text, itemPublished :: UTCTime, itemContent :: Text } deriving Show

parseTimeStringISO8601 :: String -> Maybe UTCTime
parseTimeStringISO8601 = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

formatTimeISO8601 :: UTCTime -> String
formatTimeISO8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

parseTimeStringRFC822 :: String -> Maybe UTCTime
parseTimeStringRFC822 = parseTimeM True defaultTimeLocale rfc822DateFormat

formatTimeRFC822 :: UTCTime -> String
formatTimeRFC822 = formatTime defaultTimeLocale rfc822DateFormat

fromEntryContentToText :: AF.EntryContent -> Text
fromEntryContentToText (AF.TextContent content) = content
fromEntryContentToText (AF.HTMLContent content) = content
fromEntryContentToText _ = error "Unexpected content"

fromAtomEntry :: AF.Entry -> Maybe CommonItem
fromAtomEntry entry = do
    let title = (pack . AF.txtToString . AF.entryTitle) entry
    url <- ((fmap AF.linkHref) . listToMaybe . AF.entryLinks) entry
    publishedText <- AF.entryPublished entry
    published <- (parseTimeStringISO8601 . unpack) publishedText
    content <- fmap fromEntryContentToText $ AF.entryContent entry

    return CommonItem { itemTitle = title , itemUrl = url , itemPublished = published, itemContent = content }

fromRss2Entry :: R2S.RSSItem -> Maybe CommonItem
fromRss2Entry item = do
    title <- R2S.rssItemTitle item
    url <- R2S.rssItemLink item
    publishedText <- R2S.rssItemPubDate item
    published <- parseTimeStringRFC822 . unpack $ publishedText
    content <- R2S.rssItemDescription item

    return CommonItem { itemTitle = title , itemUrl = url , itemPublished = published, itemContent = content }

composeFeeds :: [Feed] -> [CommonItem]
composeFeeds = reverse . sortWith itemPublished . _composeFeeds

_composeFeeds :: [Feed] -> [CommonItem]
_composeFeeds [] = []
_composeFeeds ((AtomFeed feed):rest) = (catMaybes . map fromAtomEntry . AF.feedEntries $ feed) ++ (composeFeeds rest)
_composeFeeds ((RSSFeed feed):rest) = (catMaybes . map fromRss2Entry . R2S.rssItems . R2S.rssChannel $ feed) ++ (composeFeeds rest)
_composeFeeds _ = []

toAtomEntry :: CommonItem -> AF.Entry
toAtomEntry (CommonItem
    { itemTitle = title
    , itemUrl = url
    , itemPublished = published
    , itemContent = content }) = (AF.nullEntry url (AF.TextString title) (pack $ formatTimeISO8601 published))
        { AF.entryLinks = [AF.nullLink url]
        , AF.entryContent = Just (AF.TextContent . T.take 100 . TS.innerText . TS.parseTags $ content)
        }

createAtomFeed :: Text -> Text -> [CommonItem] -> AF.Feed
createAtomFeed feedId title commonItems = feed
    where
        lastUpdated = itemPublished . head $ commonItems
        feed = (AF.nullFeed feedId (AF.TextString title) (pack $ formatTimeISO8601 lastUpdated))
            { AF.feedEntries = map toAtomEntry $ commonItems
            }

toRss2Item :: CommonItem -> R2S.RSSItem
toRss2Item (CommonItem
    { itemTitle = title
    , itemUrl = url
    , itemPublished = published
    , itemContent = content }) = (R2S.nullItem title)
        { R2S.rssItemLink = Just url
        , R2S.rssItemDescription = Just . T.take 100 . TS.innerText . TS.parseTags $ content
        , R2S.rssItemPubDate = Just . pack . formatTimeRFC822 $ published
        }

createRss2Feed :: Text -> Text -> [CommonItem] -> R2S.RSS
createRss2Feed url title commonItems = (R2S.nullRSS title url)
    { R2S.rssChannel = channel }
        where
            lastUpdated = itemPublished . head $ commonItems
            channel = (R2S.nullChannel title url)
                { R2S.rssTitle = title
                , R2S.rssLastUpdate = Just . pack . formatTimeRFC822 $ lastUpdated
                , R2S.rssItems = map toRss2Item commonItems
                }
