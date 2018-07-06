{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib (createAtomFeed, createRss2Feed, composeFeeds)

import Text.Feed.Types (Feed)
import Text.Feed.Import (parseFeedSource)
import Text.Atom.Feed.Export (xmlFeed)
import qualified Text.Atom.Feed as AF
import Text.RSS.Export (xmlRSS)
import qualified Text.RSS.Syntax as R2S

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTI
import qualified Text.XML as C
import Data.XML.Types as XML
import qualified Data.Text.Lazy as Lazy
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Simple
import GHC.Generics
import Web.Scotty
import qualified Data.Yaml as Y

import Paths_deruta

elementToDoc :: XML.Element -> Maybe C.Document
elementToDoc el =
  either (const Nothing) Just $ C.fromXMLDocument $ XML.Document (Prologue [] Nothing []) el []

renderAtomFeed :: AF.Feed -> Maybe Lazy.Text
renderAtomFeed = fmap (C.renderText C.def) . elementToDoc . xmlFeed

renderRss2Feed :: R2S.RSS -> Maybe Lazy.Text
renderRss2Feed = fmap (C.renderText C.def) . elementToDoc . xmlRSS

fetchFeeds :: [String] -> IO [Feed]
fetchFeeds links = forM links $ \link -> fmap (fromJust . parseFeedSource . getResponseBody) $ httpLBS =<< parseRequest link

data Config = Config { sources :: [String], feedId :: T.Text, feedTitle :: T.Text } deriving Generic
instance Y.FromJSON Config

main :: IO ()
main = do
    (config :: Config) <- Y.decodeFileThrow "./config.yaml"
    scotty 3000 $ do
        get "/feed" $ do
            feeds <- liftIO $ fetchFeeds . sources $ config
            let composedAtomFeed = createAtomFeed (feedId config) (feedTitle config) . composeFeeds $ feeds
            text . fromJust . renderAtomFeed $ composedAtomFeed
            setHeader "Content-Type" "text/xml"
        get "/rss" $ do
            feeds <- liftIO $ fetchFeeds . sources $ config
            let composedRss2Feed = createRss2Feed (feedId config) (feedTitle config) . composeFeeds $ feeds
            text . fromJust . renderRss2Feed $ composedRss2Feed
            setHeader "Content-Type" "text/xml"
