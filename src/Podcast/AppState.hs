-- |
-- Module      :  Podcast.AppState
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Podcast.AppState
  ( AppState(..)
  , HasFeeds(..)
  , HasChannel(..)
  , HasNotification(..)
  , initializeState
  , episodes
  ) where

import Control.Lens
import Control.Monad.Trans.Resource
import UnliftIO.Async

import qualified Data.Vector as Vec
import Data.Text (Text)

import Podcast.Types
import Podcast.Config
import Podcast.Feed

import Brick.BChan (BChan)
import qualified Brick.Widgets.List as L

initializeState :: (MonadThrow m, MonadUnliftIO m) => BChan Event -> m AppState
initializeState chan = runResourceT $ do
  cfg <- loadConfig
  rss <- mapConcurrently (fmap FeedEntry . downloadFeed) (cfg ^. feedUrls)
  return $ AppState
    { _appStateFeeds = (L.list () (Vec.fromList rss) 1)
    , _appStateChannel = chan
    , _appStateNotification = ""
    , _appStateConfig = cfg
    }

episodes :: Traversal' AppState Episode
episodes = feeds . L.listElementsL . each . _EpisodeEntry
