-- |
-- Module      :  Podcast.Types
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Control.DeepSeq

import GHC.Generics

import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Text (Text)

import URI.ByteString
import Text.RSS.Types

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
  deriving stock (Generic)
  deriving anyclass (NFData)

data Progress = NotStarted | InProgress | Complete
  deriving stock (Generic)
  deriving anyclass (NFData)

makePrisms ''Progress

makeLenses ''Episode

data Feed = Feed
  { _feedTitle :: Text
  , _feedCollapsed :: Bool
  , _feedEpisodes :: Vector Episode
  }
  deriving stock (Generic)
  -- deriving anyclass (NFData)

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

instance NFData Scheme
instance NFData Host
instance NFData Port
instance NFData UserInfo
instance NFData Authority
instance NFData Query

instance NFData (URIRef a) where
  rnf u@URI{} = (uriScheme u) `deepseq` (uriAuthority u) `deepseq` (uriPath u) `deepseq` (uriFragment u) `deepseq` ()
  rnf u@RelativeRef{} = (rrAuthority u) `deepseq` (rrPath u) `deepseq` (rrQuery u) `deepseq` (rrFragment u) `deepseq` ()

instance NFData RssURI where
  rnf (RssURI u) = rnf u

instance NFData RssCategory
instance NFData RssEnclosure
instance NFData RssSource
instance NFData RssGuid
instance NFData CloudProtocol
instance NFData RssCloud
instance NFData RssImage
instance NFData RssTextInput
instance NFData Hour
instance NFData Day

instance NFData (RssItemExtensions '[]) where
  rnf _ = ()
instance NFData (RssChannelExtensions '[]) where
  rnf _ = ()

instance NFData (RssItem '[])
instance NFData (RssDocument '[])
