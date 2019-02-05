-- |
-- Module      :  RSSFeeds
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE DataKinds #-}
module RSSFeeds
  ( requestRSS
  ) where

import Control.Lens

import Data.Maybe

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Combinators hiding (print)
import qualified Data.Conduit.Combinators as Conduit
import Network.HTTP.Simple
import Text.XML.Stream.Parse
import Text.RSS.Conduit.Parse.Simple
import Text.RSS.Types
import Text.RSS.Lens
import URI.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import System.IO hiding (print)

requestRSS :: (MonadThrow m, MonadResource m) => String -> m (RssDocument '[])
requestRSS url = do
  req <- parseRequest url
  rss <- runConduit $ httpSource req getResponseBody .| parseBytes def .| rssDocument
  case rss of
    Just rss -> return rss
    Nothing -> fail "Unspecified RSS Parse Failure!"
