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
module CompileClasses( compileClasses, compileClassesShort, haskellTypeArg, haskellTypePar ) where

import qualified Set
import qualified Map
import qualified MultiSet

import Time( getClockTime)
import Char( toUpper, isUpper ) --toLower, toUpper, isSpace, isLower, isUpper )
import List( isPrefixOf, sort, sortBy, intersperse, zipWith4 )

import Types
import HaskellNames
import Classes( isClassName, haskellClassDefs )
import ParseC( parseC )
import DeriveTypes( deriveTypes, classifyName, Name(..), Method(..), ClassName, MethodName, PropertyName )

{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileClasses :: Bool -> String -> String -> FilePath -> [FilePath] -> IO ()
compileClasses showIgnore moduleRoot moduleName outputFile inputFiles
  = do declss  <- mapM parseC inputFiles
       time    <- getClockTime
       let decls        = deriveTypes showIgnore (sortBy cmpDecl (concat declss))

           foreignDecls = map foreignDecl decls
           haskellDecls = map haskellDecl decls
           typeDecls    = map haskellTypeDecl decls

           marshalDecls = concat (zipWith3 (\t h f -> [t,h,f,""]) typeDecls haskellDecls foreignDecls)

           (exportsClass,classDecls)          = haskellClassDefs

           (exportsStatic,exportsClassClasses) = exportDefs decls exportsClass []

           methodCount  = length decls
           classCount   = length exportsClass
           ghcoptions   = ["{-# OPTIONS -fglasgow-exts -#include \"wxc.h\" #-}"]

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Version"
                                , "      moduleVersion"
                                , "      -- * Classes" ]
                              , exportsClassClasses
                              , [ "      -- * Global" ]
                              , exportsStatic
                              , [ "    ) where"
                                , ""
                                , "import System.IO.Unsafe( unsafePerformIO )"
                                , "import " ++ moduleRoot ++ "WxcTypes"
                                , ""
                                , "moduleVersion :: String"
                                , "moduleVersion  = \"" ++ show time ++ "\""
                                , ""
                                ]
                              ]

       prologue <- getPrologue moduleName "class"
                               (show methodCount ++ " methods in " ++ show classCount ++ " classes.")
                               inputFiles
       let output  = unlines (ghcoptions ++ prologue ++ export ++ classDecls ++ marshalDecls)

       putStrLn ("generating: " ++ outputFile)
       writeFile outputFile output
       putStrLn ("generated " ++ show methodCount ++ " methods in " ++ show classCount ++ " classes.")
       putStrLn ("ok.")

{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileClassesShort :: Bool -> String -> String -> String -> FilePath -> [FilePath] -> IO ()
compileClassesShort showIgnore moduleRoot moduleClassesName moduleName outputFile inputFiles
  = do declss  <- mapM parseC inputFiles
       time    <- getClockTime
       let decls        = deriveTypes showIgnore (sortBy cmpDecl (concat declss))

           shortNames   = validShortNames decls
           (exportsShort,shortDecls)  = unzip (concatMap (shortDecl shortNames) decls)

           (exportsClass,classDecls)  = haskellClassDefs

           (exportsStatic,exportsClassClasses) = exportDefs decls exportsClass exportsShort

           methodCount  = length decls
           classCount   = length exportsClass
           shortCount   = length exportsShort

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Version"
                                , "      moduleVersion"
                                , "      -- * Classes" ]
                              , exportsClassClasses
                              , [ "      -- * Global" ]
                              , exportsStatic
                              , [ "    ) where"
                                , ""
                                , "import " ++ moduleRoot ++ "Types"
                                , "import " ++ moduleRoot ++ moduleClassesName ++ " hiding (moduleVersion)"
                                , ""
                                , "moduleVersion :: String"
                                , "moduleVersion  = \"" ++ show time ++ "\""
                                , ""
                                ]
                              ]

       prologue <- getPrologue moduleName "short class"
                               (show methodCount ++ " methods in " ++ show classCount ++ " classes with " ++ show shortCount ++ " short method names.")
                               inputFiles
       let output  = unlines (prologue ++ export ++ concat shortDecls)

       putStrLn ("generating: " ++ outputFile)
       writeFile outputFile output
       putStrLn ("generated " ++ show shortCount ++ " short methods.")
       putStrLn ("ok.\n")






cmpDecl decl1 decl2
  = compare (haskellDeclName (declName decl1)) (haskellDeclName (declName decl2))


exportComma  = exportSpaces ++ ","
exportSpaces = "     "


{-----------------------------------------------------------------------------------------
   Create export definitions
-----------------------------------------------------------------------------------------}
exportDefs :: [Decl] -> [(ClassName,[String])] -> [(ClassName,[String])] -> ([String],[String])
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

    in  (concatMap todef (nullEntry ++ eventEntry ++ miscEntry)
        ,concatMap todef (Map.toAscList (Map.delete "Null" (Map.delete "Misc." (Map.delete "Events" exportMap))))
        )
  where
    addMethods methodMap shortMap className classDecls
      = [heading 2 className] ++ commaSep classDecls ++
        (case Map.lookup className shortMap of
           Nothing    -> []
           Just decls -> [heading 3 "Short methods"] ++ (commaSep decls)) ++
        (case Map.lookup className methodMap of
           Nothing    -> []
           Just decls -> (if (null shortExports || elem className ["Events","Misc.","Null"])
                           then []
                           else [heading 3 "Methods"])
                         ++ (commaSep decls))


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
    noclass (Object name)   = not (isClassName name || isManaged name)
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
  $ filter (not.isManaged.headToUpper)
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
   Make the "this" pointer the last argument
-----------------------------------------------------------------------------------------}
-- 2003-7-2: We disable this argument swapping and make it the first argument.
haskellThisArgument :: Decl -> (Maybe Arg,[Arg])
haskellThisArgument decl
  = (Nothing, declArgs decl)

{-
  = case classifyName (declName decl) of
      Method name _  | not (null args) && (argType (head args) == Object name)
                     -> (Just (head args), tail args)
      other          -> (Nothing, args)
  where
    args = declArgs decl
-}

haskellSwapThis :: Decl -> [Arg]
haskellSwapThis decl
  = case haskellThisArgument decl of
      (Just this,args)  -> args ++ [this]
      (Nothing,args)    -> args

{-----------------------------------------------------------------------------------------
   Translate a declaration to a haskell marshalling wrapper
-----------------------------------------------------------------------------------------}
haskellDecl :: Decl -> String
haskellDecl decl
  = haskellDeclName (declName decl) ++ " " ++ haskellArgs (haskellSwapThis decl) ++ nlStart
    ++ haskellToCResult decl (declRet decl) (
           haskellToCArgsIO (declArgs decl)
        ++ foreignName (declName decl) ++ " " ++ haskellToCArgs decl (declArgs decl)
       )

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
      Object obj | isManaged obj
            -> "withManaged" ++ haskellTypeName obj ++ "Result $" ++ nl ++ call
      Object obj | obj == "wxTreeItemId"
            -> "with" ++ haskellTypeName obj ++ "Result $" ++ nl ++ call
      String _ -> "withStringResult $ \\buffer -> " ++ nl ++ call ++ " buffer"    -- always last argument!
      Point _  -> "withPointResult $ \\px py -> " ++ nl ++ call ++ " px py"       -- always last argument!
      Vector _ -> "withVectorResult $ \\pdx pdy -> " ++ nl ++ call ++ " pdx pdy"       -- always last argument!
      Size _   -> "withSizeResult $ \\pw ph -> " ++ nl ++ call ++ " pw ph"       -- always last argument!
      Rect _   -> "withRectResult $ \\px py pw ph -> " ++ nl ++ call ++ "px py pw ph"       -- always last argument!
      RefObject name  -> "withRef" ++ haskellTypeName name ++ " $ \\pref -> " ++ nl ++ call ++ " pref"  -- always last argument!
      ArrayInt _    -> "withArrayIntResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      ArrayString _ -> "withArrayStringResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      ArrayObject name _ -> "withArrayObjectResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      other -> call
  where
    unsafeIO body
      = case tp of
          EventId  -> "unsafePerformIO $" ++ nl ++ body
          other    | isPrefixOf "Null_" (declName decl)  -> "unsafePerformIO $" ++ nl ++ body
                   | otherwise -> body


haskellToCArgsIO args
  = concatMap (\arg -> haskellToCArgIO arg) args

haskellToCArgIO arg
  = case argType arg of
      String _    -> "withCString " ++ haskellName (argName arg)
                      ++ " $ \\" ++ haskellCStringName (argName arg) ++ " -> " ++ nl
      Object obj  | isManaged obj
                  -> "withManaged" ++ haskellTypeName obj ++ " " ++ haskellName (argName arg)
                      ++ " $ \\" ++ haskellCManagedName (argName arg) ++ " -> " ++ nl
      Object obj | obj == "wxTreeItemId"
                  -> "with" ++ haskellTypeName obj ++ " " ++ haskellName (argName arg)
                      ++ " $ \\" ++ haskellCManagedName (argName arg) ++ " -> " ++ nl
      ArrayString _
                  -> "withArrayString " ++ haskellName (argName arg)
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
      other       -> ""

haskellToCArgs decl args
  = concatMap (\arg -> haskellToCArg decl arg ++ "  ") args

haskellToCArg decl arg
  = case argType arg of
      RefObject name -> traceError "reference object as argument" decl $ name
      EventId        -> traceError "event id as argument" decl $ name
      Int _ -> pparens ("toCInt " ++ name)
      Char  -> pparens ("toCChar " ++ name)
      Bool  -> pparens ("toCBool " ++ name)
      Fun f -> pparens ("toCFunPtr " ++ name)

      String _   -> haskellCStringName (argName arg)
      Object obj | isManaged obj -> haskellCManagedName (argName arg)
      Object obj | obj == "wxTreeItemId" -> haskellCManagedName (argName arg)
      Point _  -> pparens ("toCIntPointX " ++ name) ++ " " ++ pparens( "toCIntPointY " ++ name)
      Vector _ -> pparens ("toCIntVectorX " ++ name) ++ " " ++ pparens( "toCIntVectorY " ++ name)
      Size _   -> pparens ("toCIntSizeW " ++ name) ++ " " ++ pparens( "toCIntSizeH " ++ name)
      Rect _   -> pparens ("toCIntRectX " ++ name) ++ " " ++ pparens( "toCIntRectY " ++ name)
                  ++ pparens ("toCIntRectW " ++ name) ++ " " ++ pparens( "toCIntRectH " ++ name)

      ArrayString _     -> haskellArrayLenName name ++ " " ++ haskellArrayName name
      ArrayObject tp _  -> haskellArrayLenName name ++ " " ++ haskellArrayName name
      ArrayInt _        -> haskellArrayLenName name ++ " " ++ haskellArrayName name

      other -> name
  where
    name = haskellName (argName arg)


haskellCStringName name
  = "cstr_" ++ haskellName name

haskellCManagedName name
  = "cobject_" ++ haskellName name

haskellArrayName name
  = "carr_" ++ haskellName name

haskellArrayLenName name
  = "carrlen_" ++ haskellName name

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
      Vector _ -> "Vector"
      Point _  -> "Point"
      Size _   -> "Size"
      String _ -> "String"
      ArrayString _ -> "[String]"
      ArrayInt _    -> "[Int]"
      ArrayObject name _ -> "[" ++ haskellTypeName name ++ typeVar i ++ "]"
      Rect _   -> "Rect"
      RefObject "wxColour"  -> "Color"
      Object    "wxColour"  -> "Color"
      RefObject "wxTreeItemId"  -> "TreeItem"
      Object    "wxTreeItemId"  -> "TreeItem"
      Fun f  -> "FunPtr " ++ pparens f
      RefObject name  -> haskellTypeName name ++ typeVar i
      Object name     -> haskellTypeName name ++ typeVar i
      other           -> error ("Non exaustive pattern: CompileClasses.haskellType: " ++ show tp)

{-----------------------------------------------------------------------------------------
   Translate a declaration to a foreign import declaration
-----------------------------------------------------------------------------------------}
foreignDecl :: Decl -> String
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
      ArrayString _ -> "Ptr (Ptr CChar) -> IO CInt"
      ArrayObject name _ -> "Ptr " ++ foreignTypePar 0 (Object name) ++ " -> IO CInt"
      String _ -> "Ptr CChar -> IO CInt"
      Point _  -> "Ptr CInt -> Ptr CInt -> IO ()"
      Vector _ -> "Ptr CInt -> Ptr CInt -> IO ()"
      Size _   -> "Ptr CInt -> Ptr CInt -> IO ()"
      Rect _   -> "Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()"
      RefObject "wxColour"  -> "ColourObject () -> IO ()"
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
      Char   -> "CChar"
      Double -> "Double"
      Float  -> "Float"
      Ptr Void  -> "Ptr " ++ typeVar i
      Ptr t     -> "Ptr " ++ foreignTypePar i t
      -- special
      String _ -> "CString"
      Point _  -> "CInt -> CInt"
      Vector _ -> "CInt -> CInt"
      Size _   -> "CInt -> CInt"
      Rect _   -> "CInt -> CInt -> CInt -> CInt"
      Fun f    -> "Ptr " ++ pparens f
      ArrayObject name _ -> "CInt -> Ptr " ++ foreignTypePar i (Object name)
      ArrayString _      -> "CInt -> Ptr (Ptr CChar)"
      ArrayInt _         -> "CInt -> Ptr CInt"
      RefObject "wxColour"  -> "ColourObject ()"
      Object    "wxColour"  -> "ColourObject ()"
      RefObject name -> haskellUnManagedTypeName name ++ typeVar i
      Object name    -> haskellUnManagedTypeName name ++ typeVar i


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