-- |
-- Module      :  Brick.Widgets.List.Extensions
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE FlexibleContexts #-}
module Brick.Widgets.List.Extensions
  ( listSelectedElementL
  ) where

import Prelude hiding (splitAt)

import Data.Semigroup (sconcat)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable

import Data.Maybe (fromMaybe)

import Control.Lens
import Brick.Widgets.List

listSelectedElementL :: (TraversableWithIndex Int t) => Traversal' (GenericList n t e) e
listSelectedElementL f l =
  case l ^. listSelectedL of
    Just sel -> fmap (\t -> l & listElementsL .~ t) $ itraverse (\i e -> if (i == sel) then f e else pure e) (l ^. listElementsL)
    Nothing -> traverse pure l
