-- |
-- Module      :  Podcast.Entry
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
module Podcast.Entry
  ( Entry(..)
  , _FeedEntry
  , _EpisodeEntry
  , renderEntry
  ) where

import Brick.Types (Widget)

import Podcast.Types
import Podcast.Feed
import Podcast.Episode

renderEntry :: Bool -> Entry -> Widget Name
renderEntry sel (FeedEntry f) = renderFeed sel f
renderEntry sel (EpisodeEntry e) = renderEpisode sel e
