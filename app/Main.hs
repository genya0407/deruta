{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

module Main where

import Lib (createAtomFeed, composeFeeds)

import Text.Feed.Import (parseFeedFromFile)
import Text.Atom.Feed.Export (xmlFeed)
import qualified Text.Atom.Feed as AF

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

renderFeed :: AF.Feed -> Maybe Lazy.Text
renderFeed = fmap (C.renderText C.def) . elementToDoc . xmlFeed

main :: IO ()
main = scotty 3000 $ do
    get "/feed" $ do
        feeds <- liftIO $ mapM (\f -> parseFeedFromFile =<< getDataFileName f) ["files/genya0407-atom.xml", "files/genya0407-rss.xml"]
        let newAtomFeedId = "http://example.com"
        let newAtomFeedTitle = "Example feed"
        let composedAtomFeed = createAtomFeed newAtomFeedId newAtomFeedTitle . composeFeeds $ feeds
        let feed_xml_text = fromJust $ renderFeed composedAtomFeed
        text feed_xml_text
        setHeader "Content-Type" "text/xml"
