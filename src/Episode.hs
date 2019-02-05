-- |
-- Module      :  Episode
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Episode
  ( Episode(..)
  , showName
  , episodeUrl
  , codecType
  , parseEpisode
  , renderEpisode
  , downloadEpisode
  ) where

import Control.Lens
import Data.Functor

import UnliftIO.Async
import UnliftIO.IO as UIO
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.Combinators
import qualified Data.Conduit.Combinators as Conduit
import Data.Conduit.Process

import Data.Text (Text)
import qualified Data.Text as T
import URI.ByteString
import qualified Data.ByteString.Char8 as B

import Text.RSS.Lens
import Text.RSS.Types

import Network.HTTP.Simple
import System.IO
import System.Process

import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import Brick.Types (Widget)
import Brick.Widgets.Core
  ( (<+>)
  , txt
  , padLeft
  , withAttr
  )

data Episode = Episode
  { _showName :: Text
  , _episodeTitle :: Text
  , _episodeUrl :: Text
  , _codecType :: Text
  }
  deriving (Show)

makeLenses ''Episode

parseEpisode :: Text -> RssItem '[] -> Episode
parseEpisode nm item = Episode
  { _showName = nm
  , _episodeTitle = item ^. itemTitleL . to (T.filter (/= '/'))
  , _episodeUrl = item ^?! itemEnclosureL . enclosureUrlL . to (withRssURI (T.pack . B.unpack . serializeURIRef'))
  , _codecType = item ^?! itemEnclosureL . enclosureTypeL
  }

downloadEpisode :: (MonadThrow m, MonadResource m, MonadUnliftIO m) => Episode -> m ()
downloadEpisode e = UIO.withFile "/dev/null" WriteMode $ \devNull -> do
  req <- parseRequest $ e ^. episodeUrl . to T.unpack
  let output = (e ^. episodeTitle . to T.unpack) ++ ".mp3"
  let cmd = proc "ffmpeg" ["-i", "-", "-acodec", "mp3", "-ac", "2", "-ab", "192k", "-f", "mp3", "-"]
  ((ffmpegIn, close), ffmpegOut, UseProvidedHandle, handle) <- streamingProcess $ cmd { std_err = UseHandle devNull }
  runConcurrently $
    Concurrently (runConduit $ httpSource req getResponseBody .| ffmpegIn >> close) *>
    Concurrently (runConduit $ ffmpegOut .| sinkIOHandle (openBinaryFile output WriteMode)) *>
    Concurrently (void $ waitForStreamingProcess handle)

renderEpisode :: Bool -> Episode -> Widget ()
renderEpisode sel e =
  let rendered = padLeft (T.Pad 4) (txt $ e ^. episodeTitle) <+> (txt $ " <" <> (e ^. codecType) <> ">")
  in if sel then withAttr L.listSelectedAttr rendered else rendered
