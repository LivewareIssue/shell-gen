{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_shell_gen (
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/tombates/.cabal/bin"
libdir     = "/Users/tombates/.cabal/lib/x86_64-osx-ghc-8.10.7/shell-gen-0.1.0.0-inplace-sg-lib"
dynlibdir  = "/Users/tombates/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/tombates/.cabal/share/x86_64-osx-ghc-8.10.7/shell-gen-0.1.0.0"
libexecdir = "/Users/tombates/.cabal/libexec/x86_64-osx-ghc-8.10.7/shell-gen-0.1.0.0"
sysconfdir = "/Users/tombates/.cabal/etc"

getBinDir     = catchIO (getEnv "shell_gen_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "shell_gen_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "shell_gen_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "shell_gen_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shell_gen_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shell_gen_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir ++ fname
  | otherwise                  = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
