{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_editor (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/david/CS4032_PROJECT/simple_text_editor/editor/.stack-work/install/x86_64-linux/lts-7.10/8.0.1/bin"
libdir     = "/home/david/CS4032_PROJECT/simple_text_editor/editor/.stack-work/install/x86_64-linux/lts-7.10/8.0.1/lib/x86_64-linux-ghc-8.0.1/editor-0.1.0.0"
datadir    = "/home/david/CS4032_PROJECT/simple_text_editor/editor/.stack-work/install/x86_64-linux/lts-7.10/8.0.1/share/x86_64-linux-ghc-8.0.1/editor-0.1.0.0"
libexecdir = "/home/david/CS4032_PROJECT/simple_text_editor/editor/.stack-work/install/x86_64-linux/lts-7.10/8.0.1/libexec"
sysconfdir = "/home/david/CS4032_PROJECT/simple_text_editor/editor/.stack-work/install/x86_64-linux/lts-7.10/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "editor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "editor_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "editor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "editor_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "editor_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
