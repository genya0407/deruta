{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

module Main where

import Lib (createAtomFeed, createRss2Feed, composeFeeds)

import Text.Feed.Import (parseFeedFromFile)
import Text.Atom.Feed.Export (xmlFeed)
import qualified Text.Atom.Feed as AF
import Text.RSS.Export (xmlRSS)
import qualified Text.RSS.Syntax as R2S

import qualified Data.Text.Lazy.IO as LTI
import qualified Text.XML as C
import Data.XML.Types as XML
import qualified Data.Text.Lazy as Lazy
import Data.Maybe (fromJust)
import Control.Monad.IO.Class

import Web.Scotty

import Paths_deruta

elementToDoc :: XML.Element -> Maybe C.Document
elementToDoc el =
  either (const Nothing) Just $ C.fromXMLDocument $ XML.Document (Prologue [] Nothing []) el []

renderAtomFeed :: AF.Feed -> Maybe Lazy.Text
renderAtomFeed = fmap (C.renderText C.def) . elementToDoc . xmlFeed

renderRss2Feed :: R2S.RSS -> Maybe Lazy.Text
renderRss2Feed = fmap (C.renderText C.def) . elementToDoc . xmlRSS

main :: IO ()
main = scotty 3000 $ do
    get "/feed" $ do
        feeds <- liftIO $ mapM (\f -> parseFeedFromFile =<< getDataFileName f) ["files/genya0407-atom.xml", "files/genya0407-rss.xml"]
        let newAtomFeedId = "http://example.com"
        let newAtomFeedTitle = "Example feed"
        let composedAtomFeed = createAtomFeed newAtomFeedId newAtomFeedTitle . composeFeeds $ feeds
        let feed_xml_text = fromJust $ renderAtomFeed composedAtomFeed
        text feed_xml_text
        setHeader "Content-Type" "text/xml"
    get "/rss" $ do
        feeds <- liftIO $ mapM (\f -> parseFeedFromFile =<< getDataFileName f) ["files/genya0407-atom.xml", "files/genya0407-rss.xml"]
        let newAtomFeedId = "http://example.com"
        let newAtomFeedTitle = "Example RSS"
        let composedRss2Feed = createRss2Feed newAtomFeedId newAtomFeedTitle . composeFeeds $ feeds
        let feed_xml_text = fromJust $ renderRss2Feed composedRss2Feed
        text feed_xml_text
        setHeader "Content-Type" "text/xml"
