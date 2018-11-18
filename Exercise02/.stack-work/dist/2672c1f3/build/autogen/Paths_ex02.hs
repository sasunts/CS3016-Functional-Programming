module Paths_ex02 (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\vahes\\gits\\CS3016-Functional-Programming\\Exercise02\\.stack-work\\install\\a64c1a55\\bin"
libdir     = "C:\\Users\\vahes\\gits\\CS3016-Functional-Programming\\Exercise02\\.stack-work\\install\\a64c1a55\\lib\\x86_64-windows-ghc-7.10.3\\ex02-0.1.0.0-89yKVDAmSzjD9UwyfxdGo4"
datadir    = "C:\\Users\\vahes\\gits\\CS3016-Functional-Programming\\Exercise02\\.stack-work\\install\\a64c1a55\\share\\x86_64-windows-ghc-7.10.3\\ex02-0.1.0.0"
libexecdir = "C:\\Users\\vahes\\gits\\CS3016-Functional-Programming\\Exercise02\\.stack-work\\install\\a64c1a55\\libexec"
sysconfdir = "C:\\Users\\vahes\\gits\\CS3016-Functional-Programming\\Exercise02\\.stack-work\\install\\a64c1a55\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ex02_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ex02_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ex02_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ex02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ex02_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
