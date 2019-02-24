-- |
-- Module      :  Podcast.Feed
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Podcast.Feed
  ( Feed(..)
  , feedTitle
  , feedCollapsed
  , feedEpisodes
  , downloadFeed
  , renderFeed
  ) where

import Control.Lens
import Control.Monad.Trans.Resource

import Data.Conduit
import Data.Conduit.Combinators
import Network.HTTP.Simple
import Text.XML.Stream.Parse
import Text.RSS.Conduit.Parse.Simple

import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Text.RSS.Types
import Text.RSS.Lens

import qualified Brick.Widgets.List as L
import Brick.Types (Widget)
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , txt
  , withAttr
  )

import Podcast.Types
import Podcast.Episode

toVecOf :: (Getting (Endo (Vector a))) s a -> s -> Vector a
toVecOf l = foldrOf l Vec.cons Vec.empty

parseFeed :: RssDocument '[] -> Feed
parseFeed doc = Feed
  { _feedTitle = doc ^. channelTitleL
  , _feedCollapsed = True
  , _feedEpisodes = toVecOf (channelItemsL . to (parseEpisode (doc ^. channelTitleL))) doc
  }

downloadFeed :: (MonadThrow m, MonadResource m) => String -> m Feed
downloadFeed url = do
  req <- parseRequest url
  rss <- runConduit $ httpSource req getResponseBody .| parseBytes def .| rssDocument
  case rss of
    Just rss -> return $ parseFeed rss
    Nothing -> fail "Unspecified RSS Parse Failure!"

renderFeed :: Bool -> Feed -> Widget ()
renderFeed sel f =
  let rendered = if f ^. feedCollapsed
        then (txt $ "+ " <> (f ^. feedTitle))
        else (txt $ "- " <> (f ^. feedTitle))
  in if sel then withAttr L.listSelectedAttr rendered else rendered
