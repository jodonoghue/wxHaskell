-----------------------------------------------------------------------------------------
{-| Module      :  Classes
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Defines most of the classes in wxWindows.
-}
-----------------------------------------------------------------------------------------
module Classes( isClassName, isManaged, haskellClassDefs
              , objectClassNames, classNames
              , classExtends
              , getWxcDir, setWxcDir
              ) where

import System( getEnv )
import Char( isUpper )
import List( sort, sortBy )
import qualified Set
import qualified Map
import HaskellNames( haskellTypeName, isManaged )
import Types

-- to parse a class hierarchy
import Text.ParserCombinators.Parsec

-- unsafe hack :-(
import System.IO.Unsafe( unsafePerformIO )
import Data.IORef



-- urk, ugly hack to make "classes" function pure.
{-# NOINLINE wxcdir #-}
wxcdir :: IORef String
wxcdir
  = unsafePerformIO $
    do newIORef ("../wxc")

getWxcDir :: IO String
getWxcDir
  = readIORef wxcdir

setWxcDir :: String -> IO ()
setWxcDir dir
  = writeIORef wxcdir dir

{-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------}
ignoreClasses :: Set.Set String
ignoreClasses
  = Set.fromList [ "wxFile", "wxDir"  ]

classes :: [Class]
classes
  = unsafePerformIO $
    do {-
       xs <- parseClassHierarchy "ClassHierarchy.txt"
       ys <- parseClassHierarchy "ClassHierarchyExtra.txt"
       -}
       -- urk, ugly hack.
       wxcdir <- getWxcDir
       cs <- parseClassDefs [wxcdir ++ "/include/ewxw/wxc_glue.h", wxcdir ++ "/include/wxc.h"]
       -- writeFile "wxclasses.def" (showClasses cs)
       return cs
       -- return (mergeClasses xs ys) -- (mergeClasses zs (mergeClasses ys xs))


mergeClasses xs ys
  = foldr (\c cs -> mergeClass c cs) xs ys

mergeClass cls []   = [cls]
mergeClass cls1@(Class name1 subs1)  (cls2@(Class name2 subs2) : cs)
  | name1 == name2  = Class name2 (mergeClasses subs1 subs2) : cs
  | otherwise       = cls2:mergeClass cls1 cs


{-
    [ final "AcceleratorEntry"
    , final "BusyData"
    , final "BusyInfo"
    , final "BusyCursor"
    , final "CalendarDateAttr"
    , final "Caret"
    , final "Colour"
    , final "ConfigBase"
    , final "Condition"
    , final "CriticalSection"
    , final "DataFormat"
    , Class "DataObject"
      [ Class "DataObjectSimple"
        [ final "CustomDataObject"
        , final "FileDataObject"
        , final "TextDataObject"
        , final "BitmapDataObject"
        ]
      , final "DataObjectComposite"
      ]
    , final "DateTime"
    , final "DateSpan"
    , final "DialUpManager"
    , final "Dir"
    , final "DirTraverser"
    , final "DropSource"
    , Class "DropTarget"
      [ final "ELJDropTarget"
      , Class "FileDropTarget" [final "ELJFileDropTarget"]
      , Class "TextDropTarget" [final "ELJTextDropTarget"]
      ]
    , final "ELJDragDataObject"
    , final "File"
    , final "FileType"
    , final "FFile"
    , final "FileName"
    , final "FontEnumerator"
    , final "FontMapper"
    , final "GridCellAttr"
    , Class "GridCellEditor"
      [ final "GridCellBoolEditor"
      , final "GridCellChoiceEditor"
      , final "GridCellFloatEditor"
      , final "GridCellNumberEditor"
      , final "GridCellTextEditor"
      ]
    , final "HtmlHelpController"
    , Class "Locale" [final "ELJLocale"]
    , Class "Log"
      [ final "ELJLog"
      , final "LogStdErr"
      , final "LogStream"
      , final "LogTextCtrl"
      , final "LogWindow"
      , final "LogGui"
      , final "LogNull"
      , final "LogChain"
      , final "LogPassThrough"
      ]
    , final "Mutex"
    , final "MutexLocker"
    , final "Node"
    , Class "Object"
      [ final "AcceleratorTable"
      , final "BitmapHandler"
      , final "ClassInfo"
      , final "ClientData"
      , final "Clipboard"
      , Class "Client" [final "ELJClient"]
      , final "Closure"
      , final "ColourData"
      , Class "Connection" [final "ELJConnection"]
      , Class "DC"
        [ final "MemoryDC"
        , final "MetafileDC"
        , final "PostscriptDC"
        , final "PrinterDC"
        , final "ScreenDC"
        , Class "WindowDC" [final "ClientDC", final "PaintDC" ]
        ]
      , final "EncodingConverter"
      , Class "Event"
        [ final "ActivateEvent"
        , final "CalendarEvent"
        , final "CalculateLayoutEvent"
        , final "CloseEvent"
        , Class "CommandEvent"
          [ final "DialUpEvent"
          , Class "NotifyEvent"
            [ final "NotebookEvent"
            , final "SpinEvent"
            , final "TreeEvent"
            , final "WizardEvent"
            ]
          , final "ScrollEvent"
          , final "TabEvent"
          , final "UpdateUIEvent"
        ]
        , final "DropFilesEvent"
        , final "EraseEvent"
        , final "FindDialogEvent"
        , final "FocusEvent"
        , final "KeyEvent"
        , final "IconizeEvent"
        , final "IdleEvent"
        , final "InitDialogEvent"
        , final "JoystickEvent"
        , final "ListEvent"
        , final "MaximizeEvent"
        , final "MenuEvent"
        , final "MouseCaptureChangedEvent"
        , final "MouseEvent"
        , final "MoveEvent"
        , final "NavigationKeyEvent"
        , final "PaintEvent"
        , final "PaletteChangedEvent"
        , final "ProcessEvent"
        , final "QueryLayoutInfoEvent"
        , final "QueryNewPaletteEvent"
        , final "ScrollWinEvent"
        , final "SizeEvent"
        , final "ShowEvent"
        , final "SocketEvent"
        , final "SashEvent"
        , final "SetCursorEvent"
        , final "SysColourChangedEvent"
        , final "TimerEvent"
        , final "WindowCreateEvent"
        , final "WindowDestroyEvent"
        ]
      , Class "EvtHandler"
        [ final "Menu"
        , final "MenuBar"
        , final "Process"
        , Class "Validator"
          [ final "GenericValidator"
          , Class "TextValidator" [final "ELJTextValidator"]
          ]
        , Class "Window"
          [ Class "Control"
            [ final "CalendarCtrl"
            , Class "Button" [final "BitmapButton"]
            , final "CheckBox"
            , Class "Choice" [final "ComboBox"]
            , final "Gauge"
            , final "GenericDirCtrl"
            , Class "ListBox" [final "CheckListBox"]
            , final "ListCtrl"
            , final "ListView"
            , final "Notebook"
            , final "RadioBox"
            , final "RadioButton"
            , final "ScrollBar"
            , final "Slider"
            , final "SpinButton"
            , final "SpinCtrl"
            , final "StaticBitmap"
            , final "StaticBox"
            , final "StaticLine"
            , final "StaticText"
            , final "TabCtrl"
            , final "TextCtrl"
            , final "TreeCtrl"
            , final "ToggleButton"
            , final "ToolBar"
            ]
          , Class "Dialog"
            [ final "ColourDialog"
            , final "DirDialog"
            , final "FileDialog"
            , final "FindReplaceDialog"
            , final "FontDialog"
            , final "MessageDialog"
            , final "MultipleChoiceDialog"
            , final "PageSetupDialog"
            , final "PrintDialog"
            , final "SingleChoiceDialog"
            , final "TextEntryDialog"
            , final "Wizard"
            ]
          , Class "Frame"
            [ final "MDIChildFrame"
            , final "MDIParentFrame"
            , final "MiniFrame"
            , Class "PreviewFrame" [final "ELJPreviewFrame"]
            , final "SplashScreen"
            ]
          , Class "Panel"
            [ Class "PreviewControlBar" [final "ELJPreviewControlBar"]
            , Class "WizardPage" [final "WizardPageSimple"]
            ]
          , Class "SashWindow" [final "SashLayoutWindow"]
          , Class "ScrolledWindow"
            [ final "Grid"
            , final "PreviewCanvas"
            ]
          , final "SplitterWindow"
          , final "StatusBar"
          , final "TipWindow"
          ]
        ]
      , final "FileHistory"
      , final "FontData"
      , Class "GDIObject"
        [ Class "Bitmap"  [final "Icon", final "Cursor"]
        , final "Brush"
        , final "Font"
        , final "Pen"
        ]
      , final "Image"
      , final "IndividualLayoutConstraint"
      , final "LayoutConstraints"
      , final "LayoutAlgorithm"
      , Class "List"
        [ final "BrushList"
        , final "FontList"
        , final "ImageList"
        , final "Palette"
        , final "PenList"
        , final "Region"
        ]
      , final "ListItem"
      , final "Mask"
      , final "MenuItem"
      , final "PageSetupData"
      , final "PageSetupDialogData"
      , final "Printer"
      , Class "Printout" [final "ELJPrintout"]
      , final "PrintPreview"
      , final "PrintData"
      , final "PrintDialogData"
      , final "RegionIterator"
      , Class "Sizer"
        [ Class "GridSizer" [final "FlexGridSizer"]
        , Class "BoxSizer"  [final "StaticBoxSizer"]
        , final "NotebookSizer"
        ]
      , final "StopWatch"
      , final "Timer"
      ]
    , final "MimeTypesManager"
    , final "Scintilla"
    , Class "Server" [final "ELJServer"]
    , Class "StreamBase"
      [ final "InputStream"
      , final "OutputStream"
      ]
    , final "StreamToTextRedirector"
    , final "TextFile"
    , final "TimeSpan"
    , final "TreeItemId"
    , final "Variant"
    -- fake: static stuff
--    , final "DllLoader"
--    , final "ELJSysError"
    , final "SystemSettings"
--    , final "CApp"
    ]
  where
    final name  = Class name []
-}

{-----------------------------------------------------------------------------------------
   Classes
-----------------------------------------------------------------------------------------}
data Class
  = Class String [Class]
  deriving Eq

instance Show Class where
  showsPrec d c
    = showString (showClass 0 c)

showClasses cs
  = unlines (map (showClass 0) cs)

showClass indent (Class name subs)
  = (replicate indent '\t' ++ name ++ concatMap ("\n"++) (map (showClass (indent+1)) subs))

isClassName s
  = Set.member s classNames

objectClassNames :: [String]
objectClassNames
  = case filter isObject classes of
      [classObject] -> filter (/="wxColour") $ flatten classObject
      other         -> []
  where
    flatten (Class name derived)
      = name : concatMap flatten derived

    isObject (Class name derived)
      = (name == "wxObject")


classNames :: Set.Set String
classNames
  = Set.unions (map flatten classes)
  where
    flatten (Class name derived)
      = Set.insert name (Set.unions (map flatten derived))

classExtends :: Map.Map String String
classExtends
  = Map.unions (map (flatten "") classes)
  where
    flatten parent (Class name derived)
      = Map.insert name parent (Map.unions (map (flatten name) derived))


sortClasses :: [Class] -> [Class]
sortClasses cs
  = map sortExtends (sortBy cmp cs)
  where
    cmp (Class name1 _) (Class name2 _) = compare name1 name2

    sortExtends (Class name extends)
      = Class name (sortClasses extends)


haskellClassDefs :: ([(String,[String])],[String])     -- exported, definitions
haskellClassDefs
  = unzip (concatMap (haskellClass []) classes)


haskellClass parents (Class name derived)
  | name == "wxColour" = []   -- handled as a basic type
  | otherwise
    = ( (tname,[tname,inheritName tname,className tname] ++ (if isManaged name then [tname ++ "Object"] else []))
      ,   (if isManaged name
            then ("-- | Pointer to a managed object of type '" ++ tname ++ "'" ++
                  (if null parents then "" else ", derived from '" ++ head parents ++ "'") ++
                  ".\n" ++
                  "type " ++ tname ++ " a  = " ++
                  "Managed " ++ pparens (inheritName tname ++ " a") ++ "\n" ++
                  "-- | Pointer to an (unmanaged) object of type " ++ tname ++ ".\n" ++
                  "type " ++ tname ++ "Object" ++ " a  = " ++ "Object " ++ pparens (inheritName tname ++ " a"))
            else ("-- | Pointer to an object of type '" ++ tname ++ "'" ++
                  (if null parents then "" else ", derived from '" ++ head parents ++ "'") ++
                  ".\n" ++
                  "type " ++ tname ++ " a  = " ++  inheritance)
          ) ++ "\n" ++
        "-- | Inheritance type of the " ++ tname ++ " class.\n" ++
        "type " ++ inheritName tname ++ " a  = " ++ inheritanceType ++ "\n" ++
        "-- | Abstract type of the " ++ tname ++ " class.\n" ++
        "data " ++ className tname ++ " a  = " ++ className tname ++ "\n"
      )
     : concatMap (haskellClass (tname:parents)) derived
  where
    tname         = haskellTypeName name
    className s   = "C" ++ haskellTypeName s
    inheritName s = "T" ++ haskellTypeName s

    explicitInheritance
      = foldl extend (className tname ++ " a") parents
      where
        extend child parent
          = "C"++parent ++ " " ++ pparens child

    inheritanceType
      = (if null parents then id else (\tp -> inheritName (head parents) ++ " " ++ pparens tp))
         (className tname ++ " a")

    inheritance
      = (if null parents then "Object " else (haskellTypeName (head parents) ++ " "))
        ++ pparens (className tname ++ " a")


pparens txt
  = "(" ++ txt ++ ")"


{-----------------------------------------------------------------------------------------
   Read a class hierarchy from file.
   The format consists of all classes on a line,
   with subclassing expressed by putting tabs in front of the class.
   see: http://www.wxwindows.org/classhierarchy.txt
-----------------------------------------------------------------------------------------}
parseClassHierarchy :: FilePath -> IO [Class]
parseClassHierarchy fname
  = do result <- parseFromFile parseClasses (if null fname then "classhierarchy.txt" else fname)
       case result of
         Left err  -> do putStrLn ("parse error in class hierarchy: " ++ show err)
                         return []
         Right cs -> return cs
    `catch` \err ->
     do putStrLn ("exception while parsing: " ++ fname)
        print err
        return []

parseClasses :: Parser [Class]
parseClasses
  = do cs <- pclasses 0
       eof
       return cs

pclasses :: Int -> Parser [Class]
pclasses indent
  = do css <- many (pclass indent)
       return (concat css)
  <?> "classes"

pclass :: Int -> Parser [Class]
pclass indent
  = do try (count indent pindent)
       name <- pclassName
       whiteSpace
       mkClass
            <- (do char '\n'
                   return (\subs -> filterClass (Class name subs))
                <|>
                do name2 <- try $
                             do char '='
                                whiteSpace
                                name2 <- pclassName
                                whiteSpace
                                char '\n'
                                return name2
                   return (\subs -> filterClass (Class name2 subs))
                <|>
                do skipToEndOfLine
                   return (\subs -> []))
       subs <- pclasses (indent+1)
       return (mkClass subs)
  <|>
    do char '\n'
       return []
  <?> "class"


filterClass :: Class -> [Class]
filterClass (Class name subs)
  | not (Set.member name ignoreClasses) = [Class name subs]
filterClass cls
  = []


pindent
  = do{ char '\t'; return ()} <|> do{ count 8 space; return () }
  <?> ""

pclassName
  = many1 alphaNum
  <?> "class name"

skipToEndOfLine
  = do many (noneOf "\n")
       char '\n'

whiteSpace
  = many (oneOf " \t")

{-----------------------------------------------------------------------------------------
  parse class hierarchy from class definitions in a C header files:
  TClassDef(tp)
  TClassDefExtend(tp,parent)
-----------------------------------------------------------------------------------------}
parseClassDefs :: [FilePath] -> IO [Class]
parseClassDefs fnames
  = do putStrLn "reading class definitions from:"
       putStr   (concatMap (\fname -> "  " ++ fname ++ "\n") fnames)
       inputs <- mapM readFile fnames
       let defs    = filter (not . null . fst) (map parseClassDef (lines (concat inputs)))
           extends = Map.fromList defs
           extend name
                   = complete (Class name [])
                   where
                     complete cls@(Class cname ext)
                       = case Map.lookup cname extends of
                           Just ""     -> cls
                           Just parent -> complete (Class parent [cls])
                           Nothing     -> trace ("warning: undefined base class " ++ show cname ++ " in definition of " ++ show name) $
                                          cls
           clss    = map (extend . fst) defs
       return (foldr (\c cs -> mergeClass c cs) [] clss)

parseClassDef :: String -> (String,String)
parseClassDef line
  = case parse pdef "" line of
      Left err  -> ("","")
      Right r   -> r

pdef :: Parser (String,String)
pdef
  = do reserved "TClassDefExtend"
       psymbol "("
       tp <- identifier
       psymbol ","
       ext <- identifier
       psymbol ")"
       return (tp,ext)
  <|>
    do reserved "TClassDef"
       tp <- parens identifier
       return (tp,"")

parens p
  = do{ psymbol "("; x <- p; psymbol ")"; return x }

psymbol s
  = lexeme (string s)

reserved s
  = lexeme (try (string s))

identifier
  = lexeme (many1 alphaNum)

lexeme p
  = do{ x <- p
      ; whiteSpace
      ; return x
      }