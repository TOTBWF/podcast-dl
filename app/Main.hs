{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Functor
import Data.Monoid

import qualified Graphics.Vty as V
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Control.Lens
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Text.RSS.Lens
import Text.RSS.Types
import qualified Data.Text as T

import Brick.Main (App(..))
import qualified Brick.Main as M
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import Brick.Types
  ( Widget
  , BrickEvent
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

import RSSFeeds
import Episode
import Entry


data AppState = AppState
  { _feeds :: L.List () Entry
  }
  deriving (Show)

makeLenses ''Entry
makeLenses ''AppState

drawUI :: AppState -> [Widget ()]
drawUI s = [ui]
  where
    ui = B.borderWithLabel (str "[Feeds]") $ L.renderList renderEntry True (s ^. feeds)

toggleEpisodes :: Int -> Bool -> Vector Entry -> L.List () Entry -> L.List () Entry
toggleEpisodes i True ep l = foldr (L.listInsert (i + 1)) l ep
toggleEpisodes i False ep l = foldr (\_ l -> L.listRemove (i + 1) l) l ep

handleEnter :: (MonadIO m) => L.List () Entry -> m (L.List () Entry)
handleEnter l =
  case L.listSelectedElement l of
    Just (i, (Feed doc h ep)) -> return $ toggleEpisodes i h ep l & L.listModify (over hidden not)
    Just (i, (EpisodeEntry e)) -> do
      liftIO $ forkIO $ runResourceT $ downloadEpisode e
      return l
    _ -> return l

appEvent :: AppState -> BrickEvent () e -> EventM () (T.Next AppState)
appEvent s (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt s
    V.EvKey V.KEnter [] -> do
      l' <- handleEnter $ s ^. feeds
      M.continue $ s & feeds .~ l'
    ev -> do
      l <- L.handleListEvent ev (s ^. feeds)
      M.continue $ s & feeds .~ l
appEvent l _ = M.continue l

attrs :: A.AttrMap
attrs = A.attrMap V.defAttr
    [ (L.listAttr, fg V.brightBlack)
    , (L.listSelectedAttr, fg V.white)
    ]

app :: App AppState e ()
app = App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appStartEvent = return
  , M.appHandleEvent = appEvent
  , M.appAttrMap = const attrs
  }

urls :: [String]
urls =
  [ "https://feeds.soundcloud.com/users/soundcloud:users:211911700/sounds.rss"
  , "http://feeds.gimletmedia.com/hearreplyall"
  ]

main :: IO ()
main = do
  rss <- runResourceT $ traverse (fmap parseEntry . requestRSS) urls
  void $ M.defaultMain app $ AppState { _feeds = (L.list () (Vec.fromList rss) 1) }
