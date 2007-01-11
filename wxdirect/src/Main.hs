-----------------------------------------------------------------------------------------
{-| Module      :  Main
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    The program @wxDirect@ generates the @Graphcic.UI.WXH.WXC@ module automatically
    from the @wxc.h@ and @ewxc_glue.h@ header files. It is highly dependent on the
    format of these header files but generates a very reasonable haskell interface
    on top the basic C interface.

    The generation process can easily be tuned by editing the string lists in this file.
-}
-----------------------------------------------------------------------------------------
module Main where

import List( isPrefixOf )

import System( getEnv )
import System.Environment( getArgs )
import System.Console.GetOpt

import CompileClasses   ( compileClasses)
import CompileHeader    ( compileHeader )
import CompileDefs      ( compileDefs )
import CompileClassTypes( compileClassTypes )
import CompileClassInfo ( compileClassInfo )
import CompileSTC       ( compileSTC )

import Classes( getWxcDir, setWxcDir )

{-----------------------------------------------------------------------------------------
  Main & options
-----------------------------------------------------------------------------------------}
main
  = do mode <- compileOpts
       case mode of
         ModeHelp
          -> showHelp
         ModeClasses outputDir inputFiles verbose
          -> compileClasses verbose moduleRootWxCore moduleClassTypesName moduleClassesName
                             (outputDir ++ moduleClassesName) inputFiles
         ModeClassTypes outputDir inputFiles verbose
          -> compileClassTypes verbose moduleRootWxCore moduleClassTypesName
                                (outputDir ++ moduleClassTypesName ++ ".hs") inputFiles
         ModeDefs outputDir inputFiles verbose
          -> compileDefs verbose moduleRootWxCore moduleDefsName
                             (outputDir ++ moduleDefsName ++ ".hs") inputFiles
         ModeClassInfo outputDir verbose
          -> compileClassInfo verbose moduleRootWxCore moduleClassesName moduleClassTypesName moduleClassInfoName
                             (outputDir ++ moduleClassInfoName ++ ".hs")

         ModeCHeader outputDir inputFiles verbose
          -> compileHeader verbose (outputDir ++ "wxc_glue.h") inputFiles
         ModeSTC outputDir inputFiles verbose
          -> compileSTC verbose outputDir inputFiles
       -- putStrLn "done."

moduleClassesShortName= "Classes"
moduleClassTypesName  = "WxcClassTypes"
moduleClassesName     = "WxcClasses"
moduleClassInfoName   = "WxcClassInfo"
moduleDefsName        = "WxcDefs"
moduleRootWxCore      = "Graphics.UI.WXCore."
moduleRootWx          = "Graphics.UI.WX."

moduleRootDir moduleRoot
  = map dotToSlash moduleRoot
  where
    dotToSlash c  | c == '.'  = '/'
                  | otherwise = c


defaultOutputDirWxh
  = "../wxcore/src/" ++ moduleRootDir moduleRootWxCore

getDefaultFiles
  = do hs <- getDefaultHeaderFiles
       es <- getDefaultEiffelFiles
       return (hs++es)

getDefaultEiffelFiles :: IO [FilePath]
getDefaultEiffelFiles
  = do wxcdir <- getWxcDir
       return [wxcdir ++ "/include/wxc_defs.e"
              ,wxcdir ++ "/ewxw/eiffel/spec/r_2_4/wx_defs.e"]


getDefaultHeaderFiles :: IO [FilePath]
getDefaultHeaderFiles
  = do wxcdir <- getWxcDir
       return [wxcdir ++ "/include/wxc.h"
              -- , wxcdir ++ "/ewxw/eiffel/ewxw_glue.h"
              -- , wxcdir ++ "/ewxw/eiffel/wxc_glue.h"
              ]

getDefaultSTCHeaderFile :: IO [FilePath]
getDefaultSTCHeaderFile
  = do wxcdir <- getWxcDir
       return [wxcdir ++ "/wxSTC-D3/stc.h"]

getDefaultOutputDirWxc
  = do wxcdir <- getWxcDir
       return (wxcdir ++ "/include/")

{-----------------------------------------------------------------------------------------
  Options
-----------------------------------------------------------------------------------------}
data Flag
 = Verbose | Output FilePath | Target Target | Help | WxcDir FilePath


data Target
  = TDefs | TClasses | TClassTypes | THeader | TClassInfo | TSTC

data Mode
  = ModeHelp
  | ModeClasses   { outputDir :: FilePath, inputFiles :: [FilePath], verbose :: Bool }
  | ModeClassTypes{ outputDir :: FilePath, inputFiles :: [FilePath], verbose :: Bool }
  | ModeDefs      { outputDir :: FilePath, inputFiles :: [FilePath], verbose :: Bool }
  | ModeClassInfo { outputDir :: FilePath, verbose :: Bool }
  | ModeCHeader { outputDir :: FilePath, inputFiles :: [FilePath], verbose :: Bool }
  | ModeSTC { outputDir :: FilePath, inputFiles :: [FilePath], verbose :: Bool }


isHelp Help         = True
isHelp _            = False

isVerbose Verbose   = True
isVerbose _         = False

isOutput (Output _) = True
isOutput _          = False

isTarget (Target _) = True
isTarget _          = False

options :: [OptDescr Flag]
options =
 [ Option ['d'] ["definitions"] (NoArg (Target TDefs))    "generate constant definitions from .e files"
 , Option ['c'] ["classes"]     (NoArg (Target TClasses)) "generate class method definitions from .h files"
 , Option ['t'] ["classtypes"]   (NoArg (Target TClassTypes)) "generate class type definitions from .h files"
 , Option ['i'] ["classinfo"]   (NoArg (Target TClassInfo)) "generate class info definitions"
 , Option ['h'] ["header"]      (NoArg (Target THeader))  "generate typed C header file -- development use only"
 , Option ['s'] ["stc"]         (NoArg (Target TSTC)) "generate wxSTC wrapper from .h file"
 , Option ['v'] ["verbose"]     (NoArg Verbose)           "verbose: show ignored definitions"
 , Option ['o'] ["output"]      (ReqArg Output "DIR")     "optional output directory"
 , Option ['w'] ["wxc"]         (ReqArg WxcDir "DIR")     "optional 'wxc' directory (=../wxc)"
 , Option ['?'] ["help"]        (NoArg Help)              "show this information"
 ]

compileOpts :: IO Mode
compileOpts
  = do args <- getArgs
       case (getOpt Permute options args) of
        (flags,files,[])
          -> do extractWxcDir (reverse flags)
                if (any isHelp flags)
                 then return ModeHelp
                 else case filter isTarget flags of
                   []     -> invokeError ["you need to specify a target: methods, definitions or classes.\n"]
                   [Target TDefs]    -> do defaultEiffelFiles <- getDefaultEiffelFiles
                                           inputFiles <- getInputFiles ".e" defaultEiffelFiles files
                                           outputDir  <- getOutputDir flags defaultOutputDirWxh
                                           return (ModeDefs outputDir inputFiles (any isVerbose flags))
                   [Target TClassInfo]
                                     -> do outputDir  <- getOutputDir flags defaultOutputDirWxh
                                           return (ModeClassInfo outputDir (any isVerbose flags))
                   [Target TClasses] -> do defaultHeaderFiles <- getDefaultHeaderFiles
                                           inputFiles <- getInputFiles ".h" defaultHeaderFiles files
                                           outputDir  <- getOutputDir flags defaultOutputDirWxh
                                           return (ModeClasses outputDir inputFiles (any isVerbose flags))
                   [Target TClassTypes] ->
                                        do defaultHeaderFiles <- getDefaultHeaderFiles
                                           inputFiles <- getInputFiles ".h" defaultHeaderFiles files
                                           outputDir  <- getOutputDir flags defaultOutputDirWxh
                                           return (ModeClassTypes outputDir inputFiles (any isVerbose flags))
                   [Target THeader]
                                     -> do defaultHeaderFiles <- getDefaultHeaderFiles
                                           inputFiles <- getInputFiles ".h" defaultHeaderFiles files
                                           defdir     <- getDefaultOutputDirWxc
                                           outputDir  <- getOutputDir flags defdir
                                           return (ModeCHeader outputDir inputFiles (any isVerbose flags))
                   [Target TSTC]
                                     -> do defaultSTCHeaderFile <- getDefaultSTCHeaderFile
                                           inputFiles <- getInputFiles ".h" defaultSTCHeaderFile files
                                           defdir     <- getDefaultOutputDirWxc
                                           outputDir  <- getOutputDir flags defdir
                                           return (ModeSTC outputDir inputFiles (any isVerbose flags))
                   other -> invokeError ["invalid, or multiple, targets specification.\n"]
        (_,_,errs)
           -> invokeError errs
  where
    getOutputDir flags defaultOutputDir
      = case filter isOutput flags of
          []            -> do putStrLn ("warning: using default output directory:\n  " ++ defaultOutputDir ++ "\n")
                              return defaultOutputDir
          [Output dir]  -> case reverse dir of
                             []       -> return ""
                             ('/':cs) -> return dir
                             ('\\':cs)-> return dir
                             other    -> return (dir ++ "/")
          other         -> invokeError ["invalid, or multiple, output directories"]

    getInputFiles ext defaultFiles files
      = case filter (hasExt ext) files of
          [] -> do putStrLn (unlines (["warning: using default input files:"] ++ map ("  "++) defaultFiles))
                   return defaultFiles
          fs -> return fs

    hasExt ext file
      = let (rext,rbase) = span (/='.') (reverse file)
        in (not (null rbase) && (ext == ("." ++ reverse rext)))

    -- wxcdir is set via a global variable (yes, I know, it is an ugly hack :-)
    extractWxcDir flags
      = case flags of
          (WxcDir dir :fs)  -> setWxcDir dir
          (other      :fs)  -> extractWxcDir fs
          []                -> return ()



showHelp :: IO ()
showHelp
  = do msg <- helpMessage
       putStrLn msg

invokeError :: [String] -> IO a
invokeError errs
  = do msg <- helpMessage
       ioError (userError (concat errs ++ "\n" ++ msg))

helpMessage :: IO String
helpMessage
  = do defaultFiles <- getDefaultFiles
       return  (usageInfo header options ++
                "\ndefault input files:\n" ++
                unlines (map ("  "++) defaultFiles))
  where header = "usage: wxDirect -[dcti] [other options] [header-files..] [eiffel-files..]"