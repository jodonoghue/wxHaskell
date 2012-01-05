import Data.List                          (foldl', intercalate, nub)
import Data.Maybe                         (fromJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Distribution.Simple.Setup          (ConfigFlags)
import System.Cmd                         (system)
import System.FilePath.Posix              ((</>), (<.>))
import System.Directory                   (createDirectoryIfMissing)
import System.Info                        (os)
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

    ver <- fmap (head . drop 1 . splitBy (== '.')) (readProcess "wx-config" ["--version"] "")
    let extra_wx_libs = if os == "mingw32"
                        then [ "-lwxmsw28ud_media", "-lwxmsw28ud_richtext", "-lwxmsw28ud_aui"
                             , "-lwxmsw28ud_xrc", "-lstdc++" ]
                        else [ "-lstdc++" ]
        wx_cfg_parms = if os == "mingw32"
                       then [ "--unicode", "--libs", "gl,stc", "--cppflags" ]
                       else [ "--libs", "std,gl,stc,xrc,richtext,aui,media", "--cppflags" ]
    wx  <- fmap parseWxConfig (readProcess "wx-config" wx_cfg_parms "")
    lbi <- confHook simpleUserHooks (pkg0, pbi) flags

    let lpd   = localPkgDescr lbi
    let lib   = fromJust (library lpd)
    let libbi = libBuildInfo lib

    let libbi' = libbi
          { extraLibDirs = extraLibDirs libbi ++ extraLibDirs wx
          , extraLibs    = extraLibs    libbi ++ extraLibs    wx
          , ldOptions    = ldOptions    libbi ++ ldOptions    wx ++ extra_wx_libs
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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy pred l = case dropWhile pred l of
                   [] -> []
                   l' -> x : splitBy pred xs
                       where (x, xs) = break pred l'