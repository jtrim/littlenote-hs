module Paths_littlenote_hs (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jesse/Library/Haskell/bin"
libdir     = "/Users/jesse/Library/Haskell/ghc-7.8.3-x86_64/lib/littlenote-hs-0.0.1"
datadir    = "/Users/jesse/Library/Haskell/share/ghc-7.8.3-x86_64/littlenote-hs-0.0.1"
libexecdir = "/Users/jesse/Library/Haskell/libexec"
sysconfdir = "/Users/jesse/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "littlenote_hs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "littlenote_hs_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "littlenote_hs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "littlenote_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "littlenote_hs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
