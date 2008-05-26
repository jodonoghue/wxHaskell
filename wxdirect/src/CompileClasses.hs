-----------------------------------------------------------------------------------------
{-| Module      :  CompileClasses
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Module that compiles method definitions to Haskell, together
    with a proper marshaling wrapper.
-}
-----------------------------------------------------------------------------------------
module CompileClasses( compileClasses, haskellTypeArg, haskellTypePar ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified MultiSet

import Data.Time( getCurrentTime)
import Data.Char( toUpper, isUpper, toLower ) --toLower, toUpper, isSpace, isLower, isUpper )
import Data.List( isPrefixOf, sort, sortBy, intersperse, zipWith4 )

import Types
import HaskellNames
import Classes( isClassName, haskellClassDefs, objectClassNames, ClassInfo(..), classInfo, classIsManaged )
import ParseC( parseC )
import DeriveTypes( deriveTypes, classifyName, Name(..), Method(..), ClassName, MethodName, PropertyName )

{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileClasses :: Bool -> String -> String -> String -> FilePath -> [FilePath] -> IO ()
compileClasses showIgnore moduleRoot moduleClassTypesName moduleName outputFile inputFiles
  = do declss  <- mapM parseC inputFiles
       time    <- getCurrentTime
       let splitter        = 'M'
           (decls1,decls2) = let isLower decl = (haskellDeclName (declName decl) < [toLower splitter])
                             in span isLower (deriveTypes showIgnore (sortBy cmpDecl (concat declss)))

           postfix1       = "A" ++ [toEnum (fromEnum splitter -1)]
           postfix2       = [splitter] ++ "Z"

           module1        = moduleRoot ++ moduleName ++ postfix1
           module2        = moduleRoot ++ moduleName ++ postfix2

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Version"
                                , "      version" ++ moduleName
                                , "      -- * Re-export" 
                                , "    , module " ++ module1
                                , "    , module " ++ module2
                                , "    , module " ++ moduleRoot ++ moduleClassTypesName
                                , "    ) where"
                                , ""
                                , "import " ++ module1
                                , "import " ++ module2
                                , "import " ++ moduleRoot ++ moduleClassTypesName
                                , ""
                                , "version" ++ moduleName ++ " :: String"
                                , "version" ++ moduleName ++ "  = \"" ++ show time ++ "\""
                                , ""
                                ]
                              ]

       (m1,c1) <- compileClassesFile showIgnore moduleRoot moduleClassTypesName
                                    (moduleName ++ postfix1) (outputFile ++ postfix1) inputFiles decls1 time
       (m2,c2) <- compileClassesFile showIgnore moduleRoot moduleClassTypesName
                                    (moduleName ++ postfix2) (outputFile ++ postfix2) inputFiles decls2 time
       let methodCount = m1 + m2
           classCount  = c1 + c2

       prologue <- getPrologue moduleName "class"
                               (show methodCount ++ " methods for " ++ show classCount ++ " classes.")
                               inputFiles
       
       let output  = unlines (prologue ++ export)
       putStrLn ("generating: " ++ outputFile ++ ".hs")
       writeFile (outputFile ++ ".hs") output
       putStrLn ("generated " ++ show methodCount ++ " total methods for " ++ show classCount ++ " total classes.")
       putStrLn ("ok.")


compileClassesFile showIgnore moduleRoot moduleClassTypesName moduleName outputFile inputFiles decls time
  = do let foreignDecls = map foreignDecl decls
           haskellDecls = map haskellDecl decls
           typeDecls    = map haskellTypeDecl decls

           marshalDecls = concat (zipWith3 (\t h f -> [t,h,f,""]) typeDecls haskellDecls foreignDecls)

           (exportsClass,classDecls)          = haskellClassDefs

           (exportsStatic,exportsClassClasses,classCount) = exportDefs decls exportsClass []

           methodCount  = length decls
           ghcoptions   = ["{-# OPTIONS -fglasgow-exts -#include \"wxc.h\" #-}"]

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Version"
                                , "      version" ++ moduleName
                                , "      -- * Global" ]
                              , exportsStatic
                                , [ "      -- * Classes" ]
                              , exportsClassClasses
                              , [ "    ) where"
                                , ""
                                , "import System.IO.Unsafe( unsafePerformIO )"
                                , "import " ++ moduleRoot ++ "WxcTypes"
                                , "import " ++ moduleRoot ++ moduleClassTypesName
                                , ""
                                , "version" ++ moduleName ++ " :: String"
                                , "version" ++ moduleName ++ "  = \"" ++ show time ++ "\""
                                , ""
                                ]
                              ]

       prologue <- getPrologue moduleName "class"
                               (show methodCount ++ " methods for " ++ show classCount ++ " classes.")
                               inputFiles
       let output  = unlines (ghcoptions ++ prologue ++ export {- ++ classDecls -} ++ marshalDecls)

       putStrLn ("generating: " ++ outputFile ++ ".hs")
       writeFile (outputFile ++ ".hs") output
       putStrLn ("generated " ++ show methodCount ++ " methods for " ++ show classCount ++ " classes.")
       return (methodCount,classCount)



cmpDecl decl1 decl2
  = compare (haskellDeclName (declName decl1)) (haskellDeclName (declName decl2))


exportComma  = exportSpaces ++ ","
exportSpaces = "     "


{-----------------------------------------------------------------------------------------
   Create export definitions
-----------------------------------------------------------------------------------------}
exportDefs :: [Decl] -> [(ClassName,[String])] -> [(ClassName,[String])] -> ([String],[String],Int)
exportDefs decls classExports shortExports
  = let classMap   = Map.fromListWith (++) (classExports ++ [("Events",[]),("Null",[]),("Misc.",[])])
        methodMap  = Map.map sort (Map.fromListWith (++) (map exportDef decls))
        shortMap   = Map.map sort (Map.fromListWith (++) shortExports)
        exportMap  = Map.mapWithKey (addMethods methodMap shortMap) classMap
        eventEntry = case Map.lookup "Events" exportMap of
                       Just entry  -> [("Events",entry)]
                       Nothing     -> []
        miscEntry  = case Map.lookup "Misc." exportMap of
                       Just entry  -> [("Misc.",entry)]
                       Nothing     -> []
        nullEntry  = case Map.lookup "Null" exportMap of
                       Just entry  -> [("Null",entry)]
                       Nothing     -> []

        staticExps = map todef (nullEntry ++ eventEntry ++ miscEntry)
        classExps  = map todef (Map.toAscList (Map.delete "Null" (Map.delete "Misc." (Map.delete "Events" exportMap))))

    in  (concat staticExps
        ,concat classExps
        ,length (filter (not . null) classExps)
        )
  where
    addMethods methodMap shortMap className classDecls
      | null decls = []
      | otherwise  = [heading 2 className] ++ decls {- ++ commaSep classDecls -} 
      where
        decls =
          (case Map.lookup className shortMap of
             Nothing    -> []
             Just decls -> [heading 3 "Short methods"] ++ (commaSep decls)) ++
          (case Map.lookup className methodMap of
             Nothing    -> []
             Just decls -> {- (if (null shortExports || elem className ["Events","Misc.","Null"])
                             then []
                             else [heading 3 "Methods"])
                           ++ -} (commaSep decls))


    todef (classname,decls)
      = decls

    exportDef decl
      = (case classifyName (declName decl) of
           Name name     | isPrefixOf "expEVT_" name  -> "Events"
                         | isPrefixOf "Null_"   name  -> "Null"
                         | otherwise                  -> "Misc."
           Create name   -> haskellTypeName name
           Method name _ -> haskellTypeName name
        , [haskellDeclName (declName decl)])

    commaSep xs
      = map (exportComma++) xs

    heading i name
      = exportSpaces ++ "-- " ++ replicate i '*' ++ " " ++ name

{-
properties decls
  = unlines
  $ map (\(propname,classes) -> propname ++ ": " ++ concat (intersperse ", " classes))
  $ Map.toAscList
  $ Map.fromListWith (++)
  $ (concatMap property decls)

property decl
  = case (classifyName (declName decl),declArgs decl) of
      (Method name (Get propname), [Arg _ (Object objname)])          | objname==name && noclass (declRet decl)-> [(propname ++ "Get", [name])]
      (Method name (Set propname), [Arg _ (Object objname),Arg _ tp]) | objname == name && noclass tp -> [(propname ++ "Set", [name])]
      other                       -> []
  where
    noclass (Object name)   = not (isClassName name || isBuiltin name)
    noclass _               = True
-}
{-
properties decls
  = unlines
  $ map (\(methodname,classes) -> methodname ++ ": " ++ concat (intersperse ", " classes))
  $ filter (\(methodname,classes) -> length classes > 1)
  $ Map.toAscList
  $ Map.fromListWith (++)
  $ (concatMap pmethod decls)

pmethod decl
  = case (classifyName (declName decl)) of
      (Method name _)             -> [(declName decl, [name])]
      other                       -> []
-}

{-----------------------------------------------------------------------------------------
   Short cut names  (unused)
-----------------------------------------------------------------------------------------}
validShortNames :: [Decl] -> Set.Set String
validShortNames decls
  = Set.fromList
  $ map fst
  $ filter ((==1).snd)
  $ MultiSet.toOccurList
  $ MultiSet.fromList
  $ filter (any isUpper)
  $ filter (not.isBuiltin.headToUpper)
  $ filter (not.isClassName.headToUpper)
  $ filter (not.isPrefixOf "wx")
  $ filter ((>1).length)
  $ map shortName decls
  where
    headToUpper []      = []
    headToUpper (c:cs)  = toUpper c : cs

shortName :: Decl -> String
shortName decl
  = snd (shortNameEx decl)

shortNameEx :: Decl -> (String,String)
shortNameEx decl
  = case classifyName (declName decl) of
      Method cname (Normal name) -> (cname,haskellDeclName name)
      Method cname (Set name)    -> (cname,haskellDeclName  ("Set"++name))
      Method cname (Get name)    -> (cname,haskellDeclName  ("Get"++name))
      other                      -> ("","")

shortDecl :: Set.Set String -> Decl -> [((ClassName,[String]), [String])]
shortDecl validShorts decl    | Set.member sname validShorts
  = [( (cname,[sname])
     , [haskellTypeSignature sname decl
       ,sname ++ " = " ++ haskellDeclName (declName decl)
       ,""]
     )
    ]
  where
    (cname,sname) = shortNameEx decl

shortDecl validShorts decl
  = []

{-----------------------------------------------------------------------------------------
   Compile "xxx_Delete" methods to "objectDelete" to accomodate managed objects.
-----------------------------------------------------------------------------------------}
isDeleteMethod :: Decl -> Bool
isDeleteMethod decl
  = case (declRet decl, declArgs decl, classifyName (declName decl)) of
      (Void,[Arg [_] (Object selfName)],Method cname (Normal mname))
         -> (mname == "Delete" || mname=="SafeDelete") 
            && (selfName == cname) 
            && (cname `elem` objectClassNames || classIsManaged cname)
      _  -> False


{-----------------------------------------------------------------------------------------
   Make the "this" pointer the last argument
-----------------------------------------------------------------------------------------}
-- 2003-7-2: We disable this argument swapping and make it the first argument.
haskellThisArgument :: Decl -> (Maybe Arg,[Arg])
haskellThisArgument decl
  = (Nothing, declArgs decl)

haskellThisArgs :: Decl -> [(Bool,Arg)]
haskellThisArgs decl
  = case classifyName (declName decl) of
      Method name _  | not (null args) && (argType (head args) == Object name)
                     -> [(True,head args)] ++ [(False,arg) | arg <- tail args]
      other          -> [(False,arg) | arg <- args]
  where
    args = declArgs decl

haskellSwapThis :: Decl -> [Arg]
haskellSwapThis decl
  = case haskellThisArgument decl of
      (Just this,args)  -> args ++ [this]
      (Nothing,args)    -> args

{-----------------------------------------------------------------------------------------
   Translate a declaration to a haskell marshalling wrapper
-----------------------------------------------------------------------------------------}
haskellDecl :: Decl -> String
haskellDecl decl | isDeleteMethod decl
  = haskellDeclName (declName decl) ++ nlStart
    -- ++ "traceDelete \"" ++ (declName decl) ++ "\" . " 
    ++ objDelete (classInfo (case classifyName (declName decl) of
                                Method cname _ -> cname
                                _              -> "wxObject"))


haskellDecl decl
  = methodName ++ " " ++ haskellArgs (haskellSwapThis decl) ++ nlStart
    ++ haskellToCResult decl (declRet decl) (
           haskellToCArgsIO methodName (haskellThisArgs decl)
        ++ foreignName (declName decl) ++ " " ++ haskellToCArgs decl (declArgs decl)
       )
  where
    methodName = haskellDeclName (declName decl)

nl
  = "\n    "
nlStart
  = "\n  = "
pparens txt
  = "(" ++ txt ++ ")"

haskellArgs args
  = concatMap (\arg -> haskellName (argName arg) ++ " ") args


haskellToCResult decl tp call
  = unsafeIO $
    case tp of
      Fun f  -> traceWarning "function as result" decl $ call
      EventId -> "withIntResult $" ++ nl ++ call
      Int _ -> "withIntResult $" ++ nl ++ call
      Bool  -> "withBoolResult $" ++ nl ++ call
      Char  -> "withCharResult $" ++ nl ++ call
      {-
      Object obj | isBuiltin obj
            -> "withManaged" ++ haskellTypeName obj ++ "Result $" ++ nl ++ call
      Object obj | obj == "wxTreeItemId"
            -> "with" ++ haskellTypeName obj ++ "Result $" ++ nl ++ call
      Object obj
            -> "withObjectResult $" ++ nl ++ call
      -}
      Object obj -> withResult (classInfo obj)  ++ " $" ++ nl ++ call
      String _ -> "withWStringResult $ \\buffer -> " ++ nl ++ call ++ " buffer"    -- always last argument!
      Point CDouble -> "withPointDoubleResult $ \\px py -> " ++ nl ++ call ++ " px py"       -- always last argument!
      Point _  -> "withPointResult $ \\px py -> " ++ nl ++ call ++ " px py"       -- always last argument!
      Vector CDouble -> "withVectorDoubleResult $ \\pdx pdy -> " ++ nl ++ call ++ " pdx pdy"       -- always last argument!
      Vector _ -> "withVectorResult $ \\pdx pdy -> " ++ nl ++ call ++ " pdx pdy"       -- always last argument!
      Size CDouble -> "withSizeDoubleResult $ \\pw ph -> " ++ nl ++ call ++ " pw ph"       -- always last argument!
      Size _   -> "withSizeResult $ \\pw ph -> " ++ nl ++ call ++ " pw ph"       -- always last argument!
      Rect CDouble -> "withRectDoubleResult $ \\px py pw ph -> " ++ nl ++ call ++ "px py pw ph"       -- always last argument!
      Rect _   -> "withRectResult $ \\px py pw ph -> " ++ nl ++ call ++ "px py pw ph"       -- always last argument!
      
      -- RefObject name  -> "withRef" ++ haskellTypeName name ++ " $ \\pref -> " ++ nl ++ call ++ " pref"  -- always last argument!
      RefObject name -> case withRef (classInfo name) of
                          ""     -> errorMsgDecl decl "illegal reference object" 
                          action -> action ++ " $ \\pref -> " ++ nl ++ call ++ " pref"  -- always last argument!
      ArrayInt _    -> "withArrayIntResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      ArrayString _ -> "withArrayWStringResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      ArrayObject name _ -> "withArrayObjectResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      other -> call
  where
    unsafeIO body
      = case tp of
          EventId  -> "unsafePerformIO $" ++ nl ++ body
          other    | isPrefixOf "Null_" (declName decl)  -> "unsafePerformIO $" ++ nl ++ body
                   | otherwise -> body


haskellToCArgsIO methodName args
  = concatMap (\(isSelf,arg) -> haskellToCArgIO methodName isSelf arg) args

haskellToCArgIO methodName isSelf arg
  = case argType arg of
      String _    -> "withCWString " ++ haskellName (argName arg)
                      ++ " $ \\" ++ haskellCStringName (argName arg) ++ " -> " ++ nl
      {-
      Object obj  | isBuiltin obj
                  -> "withManaged" ++ haskellTypeName obj ++ " " ++ haskellName (argName arg)
                      ++ " $ \\" ++ haskellCManagedName (argName arg) ++ " -> " ++ nl
      Object obj | obj == "wxTreeItemId"
                  -> "with" ++ haskellTypeName obj ++ " " ++ haskellName (argName arg)
                      ++ " $ \\" ++ haskellCManagedName (argName arg) ++ " -> " ++ nl
      -}
      ArrayString _
                  -> "withArrayWString " ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellArrayLenName (argName arg) ++ " " ++ haskellArrayName (argName arg)
                     ++ " -> " ++ nl
      ArrayObject tp _
                  -> "withArrayObject " ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellArrayLenName (argName arg) ++ " " ++ haskellArrayName (argName arg)
                     ++ " -> " ++ nl
      ArrayInt _
                  -> "withArrayInt " ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellArrayLenName (argName arg) ++ " " ++ haskellArrayName (argName arg)
                     ++ " -> " ++ nl
      {-
      Object obj  -> (if isSelf then "withObjectRef " else "withObjectPtr ") ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellCObjectName (argName arg) ++ " -> " ++ nl
      -}
      Object obj  -> (if isSelf then withSelf (classInfo obj) ("\"" ++ methodName ++ "\"") 
                                else withPtr (classInfo obj)) ++ " "
                     ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellCObjectName (argName arg) ++ " -> " ++ nl
      other       -> ""

haskellToCArgs decl args
  = concatMap (\arg -> haskellToCArg decl arg ++ "  ") args

haskellToCArg decl arg
  = case argType arg of
      RefObject name -> traceError "reference object as argument" decl $ name
      EventId        -> traceError "event id as argument" decl $ name
      Int _ -> pparens ("toCInt " ++ name)
      Char  -> pparens ("toCWchar " ++ name)
      Bool  -> pparens ("toCBool " ++ name)
      Fun f -> pparens ("toCFunPtr " ++ name)

      String _   -> haskellCStringName (argName arg)
      {-
      Object obj | isBuiltin obj -> haskellCManagedName (argName arg)
      Object obj | obj == "wxTreeItemId" -> haskellCManagedName (argName arg)
      -}
      Object obj -> haskellCObjectName (argName arg)
      Point CDouble -> pparens ("pointX " ++ name) ++ " " ++ pparens( "pointY " ++ name)
      Point _  -> pparens ("toCIntPointX " ++ name) ++ " " ++ pparens( "toCIntPointY " ++ name)
      Vector CDouble -> pparens ("vecX " ++ name) ++ " " ++ pparens( "vecY " ++ name)
      Vector _ -> pparens ("toCIntVectorX " ++ name) ++ " " ++ pparens( "toCIntVectorY " ++ name)
      Size CDouble -> pparens ("sizeW " ++ name) ++ " " ++ pparens( "sizeH " ++ name)
      Size _   -> pparens ("toCIntSizeW " ++ name) ++ " " ++ pparens( "toCIntSizeH " ++ name)
      Rect CDouble -> pparens ("rectLeft " ++ name) ++ " " ++ pparens( "rectTop " ++ name)
                   ++ pparens ("rectWidth " ++ name) ++ " " ++ pparens( "rectHeight " ++ name)
      Rect _   -> pparens ("toCIntRectX " ++ name) ++ " " ++ pparens( "toCIntRectY " ++ name)
                  ++ pparens ("toCIntRectW " ++ name) ++ " " ++ pparens( "toCIntRectH " ++ name)
      ColorRGB _ ->    pparens ("toCCharColorRed " ++ name) ++ " " 
                    ++ pparens ("toCCharColorGreen " ++ name) ++ " "
                    ++ pparens ("toCCharColorBlue " ++ name) 

      ArrayString _     -> haskellArrayLenName name ++ " " ++ haskellArrayName name
      ArrayObject tp _  -> haskellArrayLenName name ++ " " ++ haskellArrayName name
      ArrayInt _        -> haskellArrayLenName name ++ " " ++ haskellArrayName name

      other -> name
  where
    name = haskellName (argName arg)


haskellCStringName name
  = "cstr_" ++ haskellName name

{-
haskellCManagedName name
  = "cobject_" ++ haskellName name
-}

haskellArrayName name
  = "carr_" ++ haskellName name

haskellArrayLenName name
  = "carrlen_" ++ haskellName name

haskellCObjectName name
  = "cobj_" ++ haskellName name

{-----------------------------------------------------------------------------------------
   Translate a declaration to a haskell type declaration
-----------------------------------------------------------------------------------------}
-- | Generate a full haskell type declarations
haskellTypeDecl :: Decl -> String
haskellTypeDecl decl
  = haskellHaddockComment decl ++ "\n" ++
    haskellTypeSignature (haskellDeclName (declName decl)) decl

-- | Generate a haddock comment
haskellHaddockComment :: Decl -> String
haskellHaddockComment decl
  | null (declComment decl) =  "-- | usage: (@" ++ callExpr ++ "@)."
  | otherwise               =  "{- | " ++ declComment decl ++ " -}"
  where
    callExpr = case haskellThisArgument decl of
                 (Just this,args) -> haskellArgName (argName this) ++ " # " ++ haskellDeclName (declName decl) ++ callArgs args
                 (Nothing,args)   -> haskellDeclName (declName decl) ++ callArgs args
    callArgs args
             = concatMap (\arg -> " " ++ haskellArgName (argName arg)) args

-- | Generate a haskell type signature
haskellTypeSignature :: String -> Decl -> String
haskellTypeSignature name decl
  = haskellRetType decl $
    name ++ " :: "  ++ haskellTypeArgs decl (haskellSwapThis decl)


haskellTypeArgs decl args
  = concatMap (\(i,arg) -> haskellTypeArg decl i arg ++ " -> ") (zip [1..] args)


haskellRetType decl typedecl
  = case declRet decl of
      EventId   -> "{-# NOINLINE " ++ haskellDeclName (declName decl) ++ " #-}\n" ++ typedecl ++ " EventId"
      tp        | isPrefixOf "Null_" (declName decl)
                -> typedecl ++ haskellType 0 tp
                | otherwise
                -> typedecl ++ " IO " ++ haskellTypePar 0 tp



-- type def. for clarity
haskellTypeArg decl i (Arg ["id"] (Int _))     = "Id"
haskellTypeArg decl i (Arg ["_id"] (Int _))    = "Id"
haskellTypeArg decl i (Arg ["_stl"] (Int _))   = "Style"
haskellTypeArg decl i arg
  = haskellType i (argType arg)

haskellTypePar i tp
  = parenType (haskellType i) tp

haskellType i tp
  = case tp of
      Bool   -> "Bool"
      Int _  -> "Int"
      Void   -> "()"
      Char   -> "Char"
      Double -> "Double"
      Float  -> "Float"
      Ptr Void  -> "Ptr " ++ typeVar i
      Ptr t  -> "Ptr " ++ foreignTypePar i t
      -- special
      Vector CDouble -> "(Vector2 Double)"
      Vector _ -> "Vector"
      Point CDouble  -> "(Point2 Double)"
      Point _  -> "Point"
      Size CDouble -> "(Size2D Double)"
      Size _   -> "Size"
      ColorRGB _ -> "Color"
      String _ -> "String"
      ArrayString _ -> "[String]"
      ArrayInt _    -> "[Int]"
      ArrayObject name _ -> "[" ++ haskellTypeName name ++ typeVar i ++ "]"
      Rect CDouble   -> "(Rect2D Double)"
      Rect _   -> "Rect"
      {-
      RefObject "wxColour"  -> "Color"
      Object    "wxColour"  -> "Color"
      RefObject "wxTreeItemId"  -> "TreeItem"
      Object    "wxTreeItemId"  -> "TreeItem"
      Object    "wxString"      -> "String"
      -}
      Fun f  -> "FunPtr " ++ pparens f
      RefObject name  -> classTypeName (classInfo name) (typeVar i) -- haskellTypeName name ++ typeVar i
      Object name     -> classTypeName (classInfo name) (typeVar i) -- haskellTypeName name ++ typeVar i
      other           -> error ("Non exaustive pattern: CompileClasses.haskellType: " ++ show tp)

{-----------------------------------------------------------------------------------------
   Translate a declaration to a foreign import declaration
-----------------------------------------------------------------------------------------}
foreignDecl :: Decl -> String
foreignDecl decl | isDeleteMethod decl
  = ""

foreignDecl decl
  = "foreign import ccall \"" ++ declName decl ++ "\" "
      ++ foreignName (declName decl) ++ " :: "
      ++ foreignArgs decl (declArgs decl) ++ foreignResultType (declRet decl)

foreignName name
  | isPrefixOf "wx" name  && elem '_' name  = name
  | otherwise                               = "wx_" ++ name

foreignArgs :: Decl -> [Arg] -> String
foreignArgs decl args
  = concatMap (\(i,arg) -> foreignArg decl i arg ++ " -> ") (zip [1..] args)

foreignArg decl i arg
  = case argType arg of
      RefObject name -> traceError "RefObject in argument" decl $ foreignType i (RefObject name)
      Void           -> traceError "void type in argument" decl $ foreignType i Void
      tp             -> foreignType i tp

foreignResultType tp
  = case tp of
      ArrayInt _    -> "Ptr CInt -> IO CInt"
      ArrayString _ -> "Ptr (Ptr CWchar) -> IO CInt"
      ArrayObject name _ -> "Ptr " ++ foreignTypePar 0 (Object name) ++ " -> IO CInt"
      String _ -> "Ptr CWchar -> IO CInt"
      Point CDouble -> "Ptr Double -> Ptr Double -> IO ()"
      Point _  -> "Ptr CInt -> Ptr CInt -> IO ()"
      Vector CDouble -> "Ptr Double -> Ptr Double -> IO ()"
      Vector _ -> "Ptr CInt -> Ptr CInt -> IO ()"
      Size CDouble -> "Ptr Double -> Ptr Double -> IO ()"
      Size _   -> "Ptr CInt -> Ptr CInt -> IO ()"
      Rect CDouble -> "Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()"
      Rect _    -> "Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()"
      -- RefObject "wxColour"  -> "ColourPtr () -> IO ()"
      RefObject name        -> foreignType 0 tp ++ " -> IO ()"
      EventId -> "IO CInt"
      other   -> "IO " ++ foreignTypePar 0 tp

foreignTypePar i tp
  = parenType (foreignType i) tp

foreignType i tp
  = case tp of
      Bool   -> "CBool"
      Int _  -> "CInt"
      Void   -> "()"
      Char   -> "CWchar"
      Double -> "Double"
      Float  -> "Float"
      Ptr Void  -> "Ptr " ++ typeVar i
      Ptr t     -> "Ptr " ++ foreignTypePar i t
      -- special
      String _ -> "CWString"
      Point CDouble  -> "Double -> Double"
      Point _  -> "CInt -> CInt"
      Vector CDouble  -> "Double -> Double"
      Vector _ -> "CInt -> CInt"
      Size CDouble  -> "Double -> Double"
      Size _   -> "CInt -> CInt"
      ColorRGB _ -> "CChar -> CChar -> CChar"
      Rect CDouble -> "Double -> Double -> Double -> Double"
      Rect _   -> "CInt -> CInt -> CInt -> CInt"
      Fun f    -> "Ptr " ++ pparens f
      ArrayObject name _ -> "CInt -> Ptr " ++ foreignTypePar i (Object name)
      ArrayString _      -> "CInt -> Ptr (Ptr CWchar)"
      ArrayInt _         -> "CInt -> Ptr CInt"
      {-
      RefObject "wxColour"  -> "ColourPtr ()"
      Object    "wxColour"  -> "ColourPtr ()"
      -}
      {-
      RefObject name -> "Ptr (T" ++ haskellUnBuiltinTypeName name ++ typeVar i ++ ")"
      Object name    -> "Ptr (T" ++ haskellUnBuiltinTypeName name ++ typeVar i ++ ")"
      -}
      RefObject name -> "Ptr (T" ++ haskellTypeName name ++ typeVar i ++ ")"
      Object name    -> "Ptr (T" ++ haskellTypeName name ++ typeVar i ++ ")"

parenType f tp
  = parenFun tp (f tp)
  where
    parenFun tp
      = case tp of
          Ptr _       -> pparens
          Object _    -> pparens
          RefObject _ -> pparens
          other       -> id


typeVar i = " " ++ typeVars !! i
typeVars  = "()" : [[toEnum (fromEnum 'a' + x)] | x <- [0..]]
