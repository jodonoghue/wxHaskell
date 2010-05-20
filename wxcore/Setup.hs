import Data.List                          (foldl', intercalate, nub)
import Data.Maybe                         (fromJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Distribution.Simple.Setup          (ConfigFlags)
import System.Cmd                         (system)
import System.FilePath.Posix              ((</>), (<.>))
import System.Directory                   (createDirectoryIfMissing)
import System.Process                     (readProcess)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

sourceDirectory  :: FilePath
eiffelDirectory  :: FilePath
includeDirectory :: FilePath
wxcoreDirectory  :: FilePath

sourceDirectory  = "src"
eiffelDirectory  = sourceDirectory </> "eiffel"
includeDirectory = sourceDirectory </> "include"
wxcoreDirectory  = sourceDirectory </> "haskell/Graphics/UI/WXCore"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

wxcoreIncludeFile :: FilePath
wxcoreIncludeFile = includeDirectory </> "wxc.h"

eiffelFiles :: [FilePath]
eiffelFiles =
    map ((<.> "e") . (eiffelDirectory </>)) names
  where
    names = ["wxc_defs", "wx_defs", "stc"]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Comment out type signature because of a Cabal API change from 1.6 to 1.7
myConfHook (pkg0, pbi) flags = do
    createDirectoryIfMissing True wxcoreDirectory
    system $ "wxdirect -t --wxc " ++ sourceDirectory ++ " -o " ++ wxcoreDirectory ++ " " ++ wxcoreIncludeFile
    system $ "wxdirect -i --wxc " ++ sourceDirectory ++ " -o " ++ wxcoreDirectory ++ " " ++ wxcoreIncludeFile
    system $ "wxdirect -c --wxc " ++ sourceDirectory ++ " -o " ++ wxcoreDirectory ++ " " ++ wxcoreIncludeFile
    system $ "wxdirect -d --wxc " ++ sourceDirectory ++ " -o " ++ wxcoreDirectory ++ " " ++ intercalate " " eiffelFiles

    wx <- fmap parseWxConfig (readProcess "wx-config" ["--libs", "--cppflags"] "")
    lbi <- confHook simpleUserHooks (pkg0, pbi) flags

    let lpd   = localPkgDescr lbi
    let lib   = fromJust (library lpd)
    let libbi = libBuildInfo lib

    let libbi' = libbi
          { extraLibDirs = extraLibDirs libbi ++ extraLibDirs wx
          , extraLibs    = extraLibs    libbi ++ extraLibs    wx
          , ldOptions    = ldOptions    libbi ++ ldOptions    wx ++ ["-lstdc++"]
          , frameworks   = frameworks   libbi ++ frameworks   wx
          , includeDirs  = includeDirs  libbi ++ includeDirs  wx
          , ccOptions    = ccOptions    libbi ++ ccOptions    wx ++ ["-DwxcREFUSE_MEDIACTRL"]
          }

    let lib' = lib { libBuildInfo = libbi' }
    let lpd' = lpd { library = Just lib' }

    return $ lbi { localPkgDescr = lpd' }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseWxConfig :: String -> BuildInfo
parseWxConfig s =
    helper emptyBuildInfo (words s)
  where
    helper b ("-framework":w:ws) = helper (b { frameworks = w : frameworks b }) ws
    helper b (w:ws)              = helper (f b w) ws
    helper b []                  = b
    f b w =
        case w of
          ('-':'L':v) -> b { extraLibDirs = v : extraLibDirs b }
          ('-':'l':v) -> b { extraLibs    = v : extraLibs b }
          ('-':'I':v) -> b { includeDirs  = v : includeDirs b }
          ('-':'D':_) -> b { ccOptions    = w : ccOptions b }
          _           -> b
