-- |
-- Module      :  Podcast.Types
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Podcast.Types
  ( Name
  , Episode(..)
  , showName
  , episodeTitle
  , episodeUrl
  , codecType
  , progress
  , episodeDescription
  , descriptionHidden
  , Progress(..)
  , _NotStarted
  , _InProgress
  , _Complete
  , Feed(..)
  , feedTitle
  , feedCollapsed
  , feedEpisodes
  , Entry(..)
  , _FeedEntry
  , _EpisodeEntry
  , Event(..)
  , Config(..)
  , outputDir
  , feedUrls
  , AppState(..)
  , HasFeeds(..)
  , HasChannel(..)
  , HasNotification(..)
  , HasConfig(..)
  ) where

import Control.Lens

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Text (Text)

import qualified Brick.BChan as BC
import qualified Brick.Widgets.List as L

type Name = ()

data Episode = Episode
  { _showName :: Text
  , _episodeTitle :: Text
  , _episodeUrl :: Text
  , _episodeDescription :: Text
  , _descriptionHidden :: Bool
  , _codecType :: Text
  , _progress :: Progress
  }

data Progress = NotStarted | InProgress Char | Complete

makePrisms ''Progress

makeLenses ''Episode

data Feed = Feed
  { _feedTitle :: Text
  , _feedCollapsed :: Bool
  , _feedEpisodes :: Vector Episode
  }

makeLenses ''Feed

data Entry
  = FeedEntry Feed
  | EpisodeEntry Episode

makePrisms ''Entry

data Event
  = CompletionEvent Text
  | NotificationEvent Text
  | TickEvent

data Config = Config
  { _outputDir :: FilePath
  , _feedUrls :: [String]
  }
makeLenses ''Config

data AppState = AppState
  { _appStateFeeds :: L.List Name Entry
  , _appStateChannel :: BC.BChan Event
  , _appStateNotification :: Text
  , _appStateConfig :: Config
  }

makeFields ''AppState
