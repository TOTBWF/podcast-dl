-- |
-- Module      :  Podcast.Episode
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE LambdaCase #-}
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
  , action
  , episodeDescription
  , descriptionHidden
  , parseEpisode
  , renderEpisode
  , downloadEpisode
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import UnliftIO.Async
import UnliftIO.IO as UIO

import Data.Functor
import Data.Conduit
import Data.Conduit.Combinators
import qualified Data.Conduit.Combinators as Conduit
import Data.Conduit.Process
import Data.IORef

import Data.Text (Text)
import qualified Data.Text as T
import URI.ByteString
import qualified Data.ByteString as B (length)
import qualified Data.ByteString.Char8 as B

import Text.RSS.Lens
import Text.RSS.Types

import Network.HTTP.Types (hContentLength)
import Network.HTTP.Simple
import qualified GI.Gio.Interfaces.File as GIO
import qualified GI.Gio.Objects.Cancellable as GIO
import qualified GI.Gio.Flags as GIO (FileCopyFlags(..))
import System.IO
import System.Directory
import System.FilePath.Posix
import System.Process

import Brick.BChan (BChan)
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
  , _episodeUrl = item ^?! itemEnclosureL . enclosureUrlL . to (withRssURI (T.pack . B.unpack . serializeURIRef'))
  , _episodeDescription = item ^. itemDescriptionL
  , _descriptionHidden = True
  , _codecType = item ^?! itemEnclosureL . enclosureTypeL
  , _progress = 0
  , _action = Download
  }

downloadEpisode :: (MonadThrow m, MonadResource m, MonadUnliftIO m, MonadEvent m, MonadConfig m) => Episode -> m ()
downloadEpisode e = UIO.withFile "/dev/null" WriteMode $ \devNull -> do
  req <- parseRequest $ e ^. episodeUrl . to T.unpack
  let fname = (e ^. episodeTitle . to T.unpack) ++ ".mp3"
  out <- fmap (</> fname) $ askConfig outputDir
  let cmd = proc "ffmpeg"
        ["-i", "-"
        , "-acodec", "mp3"
        , "-ac", "2"
        , "-ab", "192k"
        , "-f", "mp3"
        , "-metadata", "title=" ++ (T.unpack $ e ^. episodeTitle)
        , "-metadata", "artist=" ++ (T.unpack $ e ^. showName) , "-metadata", "album=" ++ (T.unpack $ e ^. showName)
        , "-"
        ]
  ((ffmpegIn, close), ffmpegOut, UseProvidedHandle, handle) <- streamingProcess $ cmd { std_err = UseHandle devNull }
  runConcurrently $
    Concurrently (runConduit $ httpSource req updateProgress .| ffmpegIn >> close) *>
    Concurrently (runConduit $ ffmpegOut .| sinkSystemTempFile fname >>= (\tmp -> gMoveFile tmp out)) *>
    Concurrently (void $ waitForStreamingProcess handle)
    where
      gMoveFile :: (MonadIO m, MonadEvent m) => FilePath -> FilePath -> m ()
      gMoveFile i o = do
        fi <- GIO.fileNewForPath i
        fo <- GIO.fileNewForPath o
        chan <- askChannel
        let broadcastMoveProgress c t = BC.writeBChan chan $ ProgressEvent Move (e ^. episodeTitle) $ (realToFrac c) / (realToFrac t)
        GIO.fileMove fi fo [GIO.FileCopyFlagsOverwrite] GIO.noCancellable (Just broadcastMoveProgress)

      updateProgress res = do
        let (Just cl) = fmap (read . B.unpack) $ lookup hContentLength (getResponseHeaders res)
        tref <- liftIO $ newIORef 0
        let loop = await >>= \case
              Nothing -> return ()
              Just chunk -> do
                  let len = realToFrac $ B.length chunk
                  total <- liftIO $ readIORef tref
                  broadcast $ ProgressEvent Download (e ^. episodeTitle) ((total + len)/cl)
                  liftIO $ writeIORef tref (total + len)
                  yield chunk
                  loop
        getResponseBody res .| loop

renderEpisode :: Bool -> Episode -> Widget Name
renderEpisode sel e =
  let act = case e ^. action of Download -> txt " downloading: "; Move -> txt " moving: "
      pr =
        if (e ^. progress == 0)
        then str ""
        else act <+> (str $ (show ((e ^. progress) * 100)) ++ "%")
      rendered = padLeft (T.Pad 4) $ (txt $ e ^. episodeTitle) <+> pr
      desc =
        if (e ^. descriptionHidden)
        then rendered
        else rendered <=> (padLeft (T.Pad 8) $ txtWrap $ e ^. episodeDescription)
  in if sel then withAttr L.listSelectedAttr desc else desc
