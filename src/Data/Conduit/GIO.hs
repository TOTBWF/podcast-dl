-- |
-- Module      :  Data.Conduit.GIO
-- Copyright   :  (c) Reed Mullanix 2019
-- License     :  BSD-style
-- Maintainer  :  reedmullanix@gmail.com
--
{-# LANGUAGE LambdaCase #-}
module Data.Conduit.GIO
  ( sinkFile
  , runMainLoop
  , allocateMainLoop
  ) where

import Data.Functor (void)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Concurrent (forkIO)

import UnliftIO.Exception
import UnliftIO.MVar

import Data.Conduit
import Data.ByteString (ByteString)

import qualified GI.GLib.Constants as GLib
import qualified GI.Gio.Interfaces.File as GIO
import qualified GI.Gio.Objects.OutputStream as GIO
import qualified GI.Gio.Objects.Cancellable as GIO
import qualified GI.Gio.Flags as GIO (FileCreateFlags(..))
import qualified GI.GLib.Structs.MainLoop as GLib
import GI.GLib.Structs.MainLoop (MainLoop)
import qualified GI.GObject.Objects as GObj


sinkHandle :: (MonadIO m) => GIO.File -> ConduitT ByteString o m ()
sinkHandle f = do
  mvar <- newEmptyMVar
  GIO.fileCreateAsync f [GIO.FileCreateFlagsReplaceDestination] GLib.PRIORITY_DEFAULT GIO.noCancellable $ Just $ \_ res -> do
    s <- GIO.fileCreateFinish f res
    writeLoop mvar s
  awaitForever (putMVar mvar . Chunk)
  putMVar mvar Flush
  where
    writeLoop :: (GIO.IsOutputStream s, GObj.IsObject s) => MVar (Flush ByteString) -> s -> IO ()
    writeLoop mvar s = takeMVar mvar >>= \case
      Chunk bytes -> GIO.outputStreamWriteAsync s (Just bytes) GLib.PRIORITY_DEFAULT GIO.noCancellable $ Just $ \_ res -> do
        _ <- GIO.outputStreamWriteFinish s res
        writeLoop mvar s
      Flush -> GIO.outputStreamCloseAsync s GLib.PRIORITY_DEFAULT GIO.noCancellable $ Just $ \_ res -> do
        GIO.outputStreamCloseFinish s res

sinkFile :: (MonadResource m) => FilePath -> ConduitT ByteString o m ()
sinkFile fp = GIO.fileNewForPath fp >>= sinkHandle

withSinkFile :: (MonadUnliftIO m, MonadIO n) => FilePath -> (ConduitT ByteString o n () -> m a) -> m a
withSinkFile fp inner = GIO.fileNewForPath fp >>= inner . sinkHandle

allocateMainLoop :: (MonadResource m) => m (ReleaseKey, MainLoop)
allocateMainLoop =
  let allocLoop = do
        loop <- GLib.mainLoopNew Nothing False
        forkIO $ GLib.mainLoopRun loop
        return loop
      deallocLoop loop = do
        GLib.mainLoopQuit loop
        GLib.mainLoopUnref loop
  in allocate allocLoop deallocLoop

-- | Runs GLib's main loop on a separate thread, and automatically performs cleanup
runMainLoop :: (MonadResource m) => m ()
runMainLoop = void $ allocateMainLoop
