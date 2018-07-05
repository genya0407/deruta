module Main where

import Text.Feed.Import (parseFeedFromFile)

import Paths_deruta

main :: IO ()
main = do
    contents <- parseFeedFromFile =<< getDataFileName "files/feed.xml"
    print contents