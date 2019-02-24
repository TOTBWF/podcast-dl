-- |
-- Module      :  Podcast.Event
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Podcast.Event
  ( Event(..)
  , MonadEvent(..)
  , notify
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans

import Data.Conduit

import Data.Text (Text)

import Brick.BChan (BChan)
import qualified Brick.BChan as BC

import Podcast.Types

class (Monad m) => MonadEvent m where
  askChannel :: m (BChan Event)
  default askChannel :: (MonadEvent m1, MonadTrans t, t m1 ~ m) => m (BChan Event)
  askChannel = lift $ askChannel

  broadcast :: Event -> m ()
  default broadcast :: (MonadEvent m1, MonadTrans t, t m1 ~ m) => Event -> m ()
  broadcast = lift . broadcast

instance (MonadIO m, HasChannel e (BChan Event)) => MonadEvent (ReaderT e m) where
  askChannel = view channel
  broadcast e = do
    chan <- view channel
    liftIO $ BC.writeBChan chan e

instance (MonadEvent m) => MonadEvent (ConduitT i o m)

notify :: (MonadEvent m) => Text -> m ()
notify = broadcast . NotificationEvent
