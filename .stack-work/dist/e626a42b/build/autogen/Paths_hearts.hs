{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hearts (
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

bindir     = "C:\\Users\\USER\\Documents\\A_Monash\\Semester 2\\FIT2102\\Assignment 2\\assignment2\\.stack-work\\install\\3d867836\\bin"
libdir     = "C:\\Users\\USER\\Documents\\A_Monash\\Semester 2\\FIT2102\\Assignment 2\\assignment2\\.stack-work\\install\\3d867836\\lib\\x86_64-windows-ghc-8.6.4\\hearts-0.1.0.0-KO99k9iedMh5kLxWwUwYKT"
dynlibdir  = "C:\\Users\\USER\\Documents\\A_Monash\\Semester 2\\FIT2102\\Assignment 2\\assignment2\\.stack-work\\install\\3d867836\\lib\\x86_64-windows-ghc-8.6.4"
datadir    = "C:\\Users\\USER\\Documents\\A_Monash\\Semester 2\\FIT2102\\Assignment 2\\assignment2\\.stack-work\\install\\3d867836\\share\\x86_64-windows-ghc-8.6.4\\hearts-0.1.0.0"
libexecdir = "C:\\Users\\USER\\Documents\\A_Monash\\Semester 2\\FIT2102\\Assignment 2\\assignment2\\.stack-work\\install\\3d867836\\libexec\\x86_64-windows-ghc-8.6.4\\hearts-0.1.0.0"
sysconfdir = "C:\\Users\\USER\\Documents\\A_Monash\\Semester 2\\FIT2102\\Assignment 2\\assignment2\\.stack-work\\install\\3d867836\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hearts_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hearts_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hearts_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hearts_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hearts_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hearts_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
