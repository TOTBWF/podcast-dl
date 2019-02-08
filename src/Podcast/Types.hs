-- |
-- Module      :  Podcast.Types
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
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
  , action
  , episodeDescription
  , descriptionHidden
  , Feed(..)
  , feedTitle
  , episodesHidden
  , episodes
  , Entry(..)
  , _FeedEntry
  , _EpisodeEntry
  , Action(..)
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
data Action = Download | Move

data Episode = Episode
  { _showName :: Text
  , _episodeTitle :: Text
  , _episodeUrl :: Text
  , _episodeDescription :: Text
  , _descriptionHidden :: Bool
  , _codecType :: Text
  , _action :: Action
  , _progress :: Float
  }

makeLenses ''Episode

data Feed = Feed
  { _feedTitle :: Text
  , _episodesHidden :: Bool
  , _episodes :: Vector Episode
  }

makeLenses ''Feed

data Entry
  = FeedEntry Feed
  | EpisodeEntry Episode

makePrisms ''Entry


data Event
  = ProgressEvent Action Text Float
  | NotificationEvent String

data Config = Config
  { _outputDir :: FilePath
  , _feedUrls :: [String]
  }
makeLenses ''Config

data AppState = AppState
  { _appStateFeeds :: L.List Name Entry
  , _appStateChannel :: BC.BChan Event
  , _appStateNotification :: String
  , _appStateConfig :: Config
  }

makeFields ''AppState
