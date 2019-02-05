-- |
-- Module      :  Entry
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Entry
  ( Entry(..)
  , parseEntry
  , renderEntry
  ) where

import Control.Lens

import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import URI.ByteString

import Text.RSS.Lens
import Text.RSS.Types

import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import Brick.Types (Widget)
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , txt
  , vLimit
  , hLimit
  , padLeft
  , vBox
  , withAttr
  )

import Episode

data Entry
  = Feed { _title :: Text, _hidden :: Bool, _episodes :: Vector Entry }
  | EpisodeEntry { _episode :: Episode }
  deriving (Show)

makeLenses ''Entry

toVecOf :: (Getting (Endo (Vector a))) s a -> s -> Vector a
toVecOf l = foldrOf l Vec.cons Vec.empty

parseEntry :: RssDocument '[] -> Entry
parseEntry doc = Feed
  { _title = doc ^. channelTitleL
  , _hidden = True
  , _episodes = toVecOf (channelItemsL . to (EpisodeEntry . parseEpisode (doc ^. channelTitleL))) doc
  }

renderEntry :: Bool -> Entry -> Widget ()
renderEntry sel (Feed title h _) =
  let rendered = if h then (txt $ "+ " <> title) else (txt $ "- " <> title)
  in if sel then withAttr L.listSelectedAttr rendered else rendered
renderEntry sel (EpisodeEntry e) = renderEpisode sel e
