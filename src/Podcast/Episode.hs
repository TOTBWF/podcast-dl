-- |
-- Module      :  Podcast.Episode
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Podcast.Episode
  ( Episode(..)
  , showName
  , episodeTitle
  , episodeUrl
  , codecType
  , progress
  , episodeDescription
  , descriptionHidden
  , parseEpisode
  , downloadEpisode
  , renderEpisode
  , episodeInProgressAttr
  , episodeCompleteAttr
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Concurrent (forkIO, killThread)
import UnliftIO.Async
import UnliftIO.Chan
import UnliftIO.IO as UIO

import Data.Functor
import Data.Conduit
import Data.Conduit.Combinators
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.GIO as GIO
import Data.Conduit.Process
import Data.IORef

import Data.Text (Text)
import qualified Data.Text as T
import URI.ByteString
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as B (length)
import qualified Data.ByteString.Char8 as B8

import Text.RSS.Lens
import Text.RSS.Types

import Network.HTTP.Types (hContentLength)
import Network.HTTP.Simple
import System.IO
import System.Directory
import System.FilePath.Posix
import System.Process

import Brick.BChan (BChan)
import Brick.AttrMap (AttrName)
import qualified Brick.BChan as BC
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P
import qualified Brick.Types as T
import Brick.Types (Widget)
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , txt
  , txtWrap
  , str
  , padLeft
  , withAttr
  )

import Podcast.Types
import Podcast.Config
import Podcast.Event

parseEpisode :: Text -> RssItem '[] -> Episode
parseEpisode nm item = Episode
  { _showName = nm
  , _episodeTitle = item ^. itemTitleL . to (T.filter (/= '/'))
  , _episodeUrl = item ^?! itemEnclosureL . enclosureUrlL . to (withRssURI (T.pack . B8.unpack . serializeURIRef'))
  , _episodeDescription = item ^. itemDescriptionL
  , _descriptionHidden = True
  , _codecType = item ^?! itemEnclosureL . enclosureTypeL
  , _progress = NotStarted
  }

downloadEpisode :: (MonadThrow m, MonadResource m, MonadUnliftIO m, MonadEvent m, MonadConfig m) => Episode -> m ()
downloadEpisode e = UIO.withFile "/dev/null" WriteMode $ \devNull -> do
  req <- parseRequest $ e ^. episodeUrl . to T.unpack
  let fname = (e ^. episodeTitle . to T.unpack) ++ ".mp3"
  out <- fmap (</> fname) $ askConfig outputDir
  ((ffmpegIn, close), ffmpegOut, UseProvidedHandle, handle) <- streamingProcess $ cmd { std_err = UseHandle devNull }
  runConcurrently $
    Concurrently (runConduit $ httpSource req getResponseBody .| ffmpegIn >> close) *>
    Concurrently (runConduit $ ffmpegOut .| GIO.sinkFile out >> (broadcast $ CompletionEvent $ e ^. episodeTitle)) *>
    Concurrently (void $ waitForStreamingProcess handle)
    where
      cmd = proc "ffmpeg"
        ["-i", "-"
        , "-acodec", "mp3"
        , "-ab", "192k"
        , "-f", "mp3"
        , "-metadata", "title=" ++ (T.unpack $ e ^. episodeTitle)
        , "-metadata", "artist=" ++ (T.unpack $ e ^. showName) , "-metadata", "album=" ++ (T.unpack $ e ^. showName)
        , "-"
        ]

episodeInProgressAttr :: AttrName
episodeInProgressAttr = "episode" <> "in-progress"

episodeCompleteAttr :: AttrName
episodeCompleteAttr = "episode" <> "complete"

renderEpisode :: Bool -> Episode -> Widget Name
renderEpisode sel e =
  let rendered = padLeft (T.Pad 4) $ (txt $ e ^. episodeTitle) <+> (str " " <+> (renderProgress $ e ^. progress))
      desc =
        if (e ^. descriptionHidden)
        then rendered
        else rendered <=> (padLeft (T.Pad 8) $ txtWrap $ e ^. episodeDescription)
  in if sel then withAttr L.listSelectedAttr desc else desc

renderProgress :: Progress -> Widget Name
renderProgress NotStarted = txt ""
renderProgress InProgress = withAttr episodeInProgressAttr $ txt "⇩"
renderProgress Complete = withAttr episodeCompleteAttr $ txt "✓"
