{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_digitRec (
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

bindir     = "C:\\dev\\Projects\\Haskell\\digitRec\\digitRec\\.stack-work\\install\\69d820b8\\bin"
libdir     = "C:\\dev\\Projects\\Haskell\\digitRec\\digitRec\\.stack-work\\install\\69d820b8\\lib\\x86_64-windows-ghc-8.2.2\\digitRec-0.1.0.0-KIUGafAaQdN9vU4iORD3Fv-digitRec"
dynlibdir  = "C:\\dev\\Projects\\Haskell\\digitRec\\digitRec\\.stack-work\\install\\69d820b8\\lib\\x86_64-windows-ghc-8.2.2"
datadir    = "C:\\dev\\Projects\\Haskell\\digitRec\\digitRec\\.stack-work\\install\\69d820b8\\share\\x86_64-windows-ghc-8.2.2\\digitRec-0.1.0.0"
libexecdir = "C:\\dev\\Projects\\Haskell\\digitRec\\digitRec\\.stack-work\\install\\69d820b8\\libexec\\x86_64-windows-ghc-8.2.2\\digitRec-0.1.0.0"
sysconfdir = "C:\\dev\\Projects\\Haskell\\digitRec\\digitRec\\.stack-work\\install\\69d820b8\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "digitRec_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "digitRec_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "digitRec_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "digitRec_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "digitRec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "digitRec_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
