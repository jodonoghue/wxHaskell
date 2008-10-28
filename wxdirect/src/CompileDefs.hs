-----------------------------------------------------------------------------------------
{-| Module      :  CompileDefs
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Module that compiles constant definitions to Haskell.
-}
-----------------------------------------------------------------------------------------
module CompileDefs( compileDefs ) where

import Data.List( sortBy, sort )

import Types
import HaskellNames
import ParseEiffel( parseEiffel )


{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileDefs :: Bool -> String -> String -> FilePath -> [FilePath] -> IO ()
compileDefs verbose moduleRoot moduleName outputFile inputFiles
  = do defss     <- mapM parseEiffel inputFiles
       let defs      = concat defss
           (haskellExports,haskellDefs)  = unzip (map toHaskellDef defs)

           defCount = length defs

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Types"
                                , "      BitFlag"
                                , "      -- * Constants"
                                ]
                              , map (exportComma++) haskellExports
                              , [ "    ) where"
                                , ""
                                , "-- | A flag can be combined with other flags to a bit mask."
                                , "type BitFlag = Int"
                                , ""
                                ]
                              ]
       prologue  <- getPrologue moduleName "constant"
                                    (show defCount ++ " constant definitions.") inputFiles

       putStrLn ("generating: " ++ outputFile)
       writeFile outputFile (unlines (prologue ++ export ++ haskellDefs))
       putStrLn ("generated " ++ show defCount ++ " constant definitions")
       putStrLn "ok."

cmpDef def1 def2
  = compare (defName def1) (defName def2)

exportComma  = exportSpaces ++ ","
exportSpaces = "     "


{-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------}
toHaskellDef :: Def -> (String,String)
toHaskellDef def
  = (haskellUnderscoreName (defName def)
    ,haskellUnderscoreName (defName def) ++ " :: " ++ haskellDefType def ++ "\n" ++
     haskellUnderscoreName (defName def) ++ " = " ++ haskellDefValue def ++ "\n"
    )

haskellDefValue def
  = showNum (defValue def)
  where
    showNum x     | x >= 0    = show x
                  | otherwise = "(" ++ show x ++ ")"


haskellDefType def
  = case defType def of
      DefInt    -> "Int"
      DefMask   -> "BitFlag"
