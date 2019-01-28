{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_MusicComposer (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/borisqa/Documents/Study/2019-Retakes/MusicComposer/.cabal-sandbox/bin"
libdir     = "/home/borisqa/Documents/Study/2019-Retakes/MusicComposer/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/MusicComposer-0.1.0.0"
dynlibdir  = "/home/borisqa/Documents/Study/2019-Retakes/MusicComposer/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/borisqa/Documents/Study/2019-Retakes/MusicComposer/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/MusicComposer-0.1.0.0"
libexecdir = "/home/borisqa/Documents/Study/2019-Retakes/MusicComposer/.cabal-sandbox/libexec"
sysconfdir = "/home/borisqa/Documents/Study/2019-Retakes/MusicComposer/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MusicComposer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MusicComposer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "MusicComposer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "MusicComposer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MusicComposer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MusicComposer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
