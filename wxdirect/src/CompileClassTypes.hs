-----------------------------------------------------------------------------------------
{-| Module      :  CompileClassTypes
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Module that compiles class types to a Haskell module for safe casting
-}
-----------------------------------------------------------------------------------------
module CompileClassTypes( compileClassTypes ) where

import Char( toLower )
import List( sortBy, sort )

import Types
import HaskellNames
import Classes


{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileClassTypes :: Bool -> String -> String -> String -> FilePath -> IO ()
compileClassTypes verbose moduleRoot moduleClassesName moduleName outputFile
  = do let classNames  = sortBy cmpName objectClassNames
           (haskellExports,haskellDefs)  = unzip (map toHaskellClassType classNames)

           defCount = length classNames

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Class Info"
                                , "      ClassType, classInfo, instanceOf"
                                , "      -- * Safe casts"
                                , "    , safeCast, ifInstanceOf, whenInstanceOf, whenValidInstanceOf"
                                , "      -- * Class Types"
                                ]
                              , map (exportComma++) haskellExports
                              , [ "    ) where"
                                , ""
                                , "import System.IO.Unsafe( unsafePerformIO )"
                                , "import " ++ moduleRoot ++ "WxcTypes"
                                , "import " ++ moduleRoot ++ moduleClassesName
                                , ""
                                , "-- | The type of a class."
                                , "data ClassType a = ClassType (ClassInfo ())"
                                , ""
                                , "-- | Return the 'ClassInfo' belonging to a class type. (Do not delete this object, it is statically allocated)"
                                , "{-# NOINLINE classInfo #-}"
                                , "classInfo :: ClassType a -> ClassInfo ()"
                                , "classInfo (ClassType info) = info"
                                , ""
                                , "-- | Test if an object is of a certain kind. (Returns also 'True' when the object is null.)"
                                , "{-# NOINLINE instanceOf #-}"
                                , "instanceOf :: WxObject b -> ClassType a -> Bool"
                                , "instanceOf obj (ClassType classInfo) "
                                , "  = if (obj==objectNull)"
                                , "     then True"
                                , "     else unsafePerformIO (objectIsKindOf obj classInfo)"
                                , ""
                                , "-- | A safe object cast. Returns 'Nothing' if the object is of the wrong type. Note that a null object can always be cast."
                                , "safeCast :: WxObject b -> ClassType (WxObject a) -> Maybe (WxObject a)"
                                , "safeCast obj classType"
                                , "  | instanceOf obj classType = Just (objectCast obj)"
                                , "  | otherwise                = Nothing"
                                , ""
                                , "-- | Perform an action when the object has the right type /and/ is not null."
                                , "whenValidInstanceOf :: WxObject a -> ClassType (WxObject b) -> (WxObject b -> IO ()) -> IO ()"
                                , "whenValidInstanceOf obj classType f"
                                , "  = whenInstanceOf obj classType $ \\object ->"
                                , "    if (object==objectNull) then return () else f object"
                                , ""
                                , "-- | Perform an action when the object has the right kind. Note that a null object has always the right kind."
                                , "whenInstanceOf :: WxObject a -> ClassType (WxObject b) -> (WxObject b -> IO ()) -> IO ()"
                                , "whenInstanceOf obj classType f"
                                , "  = ifInstanceOf obj classType f (return ())"
                                , ""
                                , "-- | Perform an action when the object has the right kind. Perform the default action if the kind is not correct. Note that a null object has always the right kind."
                                , "ifInstanceOf :: WxObject a -> ClassType (WxObject b) -> (WxObject b -> c) -> c -> c"
                                , "ifInstanceOf obj classType yes no"
                                , "  = case safeCast obj classType of"
                                , "      Just object -> yes object"
                                , "      Nothing     -> no"
                                , ""
                                ]
                              ]
       prologue  <- getPrologue moduleName "class type"
                                    (show defCount ++ " class type definitions.") []

       putStrLn ("generating: " ++ outputFile)
       writeFile outputFile (unlines (prologue ++ export ++ haskellDefs))
       putStrLn ("generated " ++ show defCount ++ " class type definitions")
       putStrLn "ok."

cmpName s1 s2
  = compare (map toLower (haskellTypeName s1)) (map toLower (haskellTypeName s2))

cmpDef def1 def2
  = compare (defName def1) (defName def2)

exportComma  = exportSpaces ++ ","
exportSpaces = "     "


{-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------}
toHaskellClassType :: String -> (String,String)
toHaskellClassType className
  = (classTypeDeclName
    ,"{-# NOINLINE " ++ classTypeDeclName ++ " #-}\n" ++
     classTypeDeclName ++ " :: ClassType (" ++ classTypeName ++ " ())\n" ++
     classTypeDeclName ++ " = ClassType (unsafePerformIO (classInfoFindClass " ++ classTypeString ++ "))\n\n"
    )
  where
    classTypeDeclName = haskellDeclName ("class" ++ classTypeName)
    classTypeName     = haskellTypeName className
    classTypeString   = "\"" ++ className ++ "\""