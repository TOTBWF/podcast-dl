{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Lib
    ( someFunc
    ) where

import Control.Lens

import Data.Maybe

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Network.HTTP.Simple
import Text.XML.Stream.Parse
import Text.RSS.Conduit.Parse.Simple
import Text.RSS.Types
import Text.RSS.Lens
import URI.ByteString
import qualified Data.ByteString.Char8 as B

requestRSS :: (MonadThrow m, MonadResource m) => String -> m RssDocument'
requestRSS url = do
  req <- parseRequest url
  rss <- runConduit $ httpSource req getResponseBody .| parseBytes def .| rssDocument
  case rss of
    Just rss -> return rss
    Nothing -> fail "Unspecified RSS Parse Failure!"

-- downloadPodcasts :: (MonadThrow m, MonadResource m) => RssDocument '[] -> m ()
-- downloadPodcasts rss = do
--   let urls = rss ^.. channelItemsL . itemEnclosureL . enclosureUrlL
--   --     -- TODO: Set up a worker pool to download all of the episodes in parallel
--   _h
--   where
--     downloadPodcast :: (MonadUnliftIO m, MonadThrow m) => RssItem '[] -> m ()
--     downloadPodcast item = do
--       let title = item ^.. itemTitleL
--       req <- traverse (withRssURI (parseRequest . B.unpack . serializeURIRef')) $ item ^.. itemEnclosureL . enclosureUrlL
--       _h


someFunc :: IO ()
someFunc = putStrLn "someFunc"
