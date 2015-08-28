module Paths_math_graphics (
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

bindir     = "/home/ross/programming/haskell-stack/math-prelude/.stack-work/install/x86_64-linux/nightly-2015-07-11/7.10.1/bin"
libdir     = "/home/ross/programming/haskell-stack/math-prelude/.stack-work/install/x86_64-linux/nightly-2015-07-11/7.10.1/lib/x86_64-linux-ghc-7.10.1/mathg_AuUe5HYrrH34wvGK09tU5A"
datadir    = "/home/ross/programming/haskell-stack/math-prelude/.stack-work/install/x86_64-linux/nightly-2015-07-11/7.10.1/share/x86_64-linux-ghc-7.10.1/math-graphics-0.1.0.0"
libexecdir = "/home/ross/.cabal/libexec"
sysconfdir = "/home/ross/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "math_graphics_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "math_graphics_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "math_graphics_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "math_graphics_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "math_graphics_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
