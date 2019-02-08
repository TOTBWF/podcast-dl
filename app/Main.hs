{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Functor
import Data.Monoid

import qualified Graphics.Vty as V
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Text.RSS.Lens
import Text.RSS.Types
import qualified Data.Text as T

import Brick.Main (App(..))
import qualified Brick.Main as M
import qualified Brick.AttrMap as A
import qualified Brick.BChan as BC
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import Brick.Types
  ( Widget
  , BrickEvent(..)
  , EventM
  )
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , str
  , vLimit
  , hLimit
  , padLeft
  , vBox
  , withAttr
  )
import Brick.Util
  ( bg
  , fg
  , on
  )

import Podcast.Types
import Podcast.AppState
import Podcast.Config
import Podcast.Event
import Podcast.Entry
import Podcast.Feed
import Podcast.Episode

drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = B.borderWithLabel (str $ "[" ++ (s ^. notification) ++ "]") $ L.renderList renderEntry True (s ^. feeds)

toggleEpisodes :: Int -> Feed -> L.List Name Entry -> L.List Name Entry
toggleEpisodes i f l =
  case (f ^. episodesHidden) of
    True -> foldr (L.listInsert (i + 1) . EpisodeEntry) l (f ^. episodes)
    False -> foldr (\_ l -> L.listRemove (i + 1) l) l (f ^. episodes)

toggleSubItems :: L.List Name Entry -> L.List Name Entry
toggleSubItems l = do
  case L.listSelectedElement l of
    Just (i, (FeedEntry f)) -> toggleEpisodes i f l & L.listModify (over (_FeedEntry . episodesHidden) not)
    Just (i, (EpisodeEntry e)) -> L.listModify (over (_EpisodeEntry . descriptionHidden) not) l
    _ -> l

collapseSubItems :: L.List Name Entry -> L.List Name Entry
collapseSubItems l = do
  case L.listSelectedElement l of
    Just (i, (FeedEntry f)) | f ^. episodesHidden == False -> toggleEpisodes i f l & L.listModify (over (_FeedEntry . episodesHidden) not)
    Just (i, (EpisodeEntry e)) -> collapseSubItems (L.listMoveUp l)
    _ -> l

handleDownload :: (MonadIO m) => AppState -> m ()
handleDownload s = do
  case L.listSelectedElement (s ^. feeds) of
    Just (i, (EpisodeEntry e)) -> void $ liftIO $ forkIO $ runResourceT $ runReaderT (downloadEpisode e) s
    _ -> return ()

appEvent :: AppState -> BrickEvent Name Event -> EventM Name (T.Next AppState)
appEvent s = \case
    AppEvent (NotificationEvent n) -> M.continue $ s & notification .~ n
    AppEvent (ProgressEvent act ttl pct) ->
      M.continue $ s
        & feeds . L.listElementsL . each
        . _EpisodeEntry . filtered (views episodeTitle (== ttl))
        . lensProduct action progress .~ (act, pct)
    VtyEvent (V.EvKey V.KEsc []) -> M.halt s
    VtyEvent (V.EvKey (V.KChar 'd') []) -> do
      handleDownload s
      M.continue s
    VtyEvent (V.EvKey (V.KChar '-') []) -> M.continue $ s & feeds %~ collapseSubItems
    VtyEvent (V.EvKey V.KEnter []) -> M.continue $ s & over feeds toggleSubItems
    (VtyEvent ev) -> do
      l <- L.handleListEventVi L.handleListEvent ev (s ^. feeds)
      M.continue $ s & feeds .~ l

attrs :: A.AttrMap
attrs = A.attrMap V.defAttr
    [ (L.listAttr, fg V.brightBlack)
    , (L.listSelectedAttr, fg V.white)
    ]

app :: App AppState Event Name
app = App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appStartEvent = return
  , M.appHandleEvent = appEvent
  , M.appAttrMap = const attrs
  }

main :: IO ()
main = do
  chan <- liftIO $ BC.newBChan 10
  s <- initializeState chan
  void $ M.customMain (V.mkVty V.defaultConfig) (Just chan) app s
