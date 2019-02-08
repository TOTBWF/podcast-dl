-- |
-- Module      :  Podcast.Config
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Podcast.Config
  ( loadConfig
  , MonadConfig(..)
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.IO.Class

import Data.Conduit

import Data.Yaml

import System.Directory

import Podcast.Types


loadConfig :: (MonadIO m) => m Config
loadConfig = do
  config <- liftIO $ getXdgDirectory XdgConfig "podcast-dl/config.yaml"
  yaml <- decodeFileThrow config
  parseConfig yaml

parseConfig :: (MonadIO m) => Object -> m Config
parseConfig = parseMonad $ \v -> Config <$> (v .: "output-dir") <*> (v .: "urls")

class (Monad m) => MonadConfig m where
  askConfig :: Lens' Config a -> m a
  default askConfig :: (MonadConfig m1, MonadTrans t, t m1 ~ m) => Lens' Config a -> m a
  askConfig l = lift $ askConfig l

instance (Monad m, HasConfig env Config) => MonadConfig (ReaderT env m) where
  askConfig l = view (config . l)

instance (MonadConfig m) => MonadConfig (ConduitT i o m)
