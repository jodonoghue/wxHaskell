{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Controls
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

   Defines common GUI controls.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Controls
    ( -- * Containers
        Panel, panel, panelEx, defaultButton
      , Notebook, notebook
      , focusOn
      -- * Controls
      -- ** Button
      , Button, button, buttonEx, smallButton
      , BitmapButton, bitmapButton
      -- ** Text entry
      , Align(..), Wrap(..)
      , TextCtrl, textEntry, textCtrl, textCtrlRich, textCtrlEx
      -- ** CheckBox
      , CheckBox, checkBox
      -- ** Choice
      , Choice, choice
      -- ** ComboBox
      , ComboBox, comboBox
      -- ** ListBox
      , ListBox, SingleListBox, MultiListBox, singleListBox, multiListBox
      -- ** RadioBox
      , RadioBox, radioBox
      -- ** Spin Control
      , SpinCtrl, spinCtrl
      -- ** Slider
      , Slider, hslider, vslider, sliderEx
      -- ** Gauge
      , Gauge, hgauge, vgauge, gaugeEx
      -- ** Tree control
      , TreeCtrl, treeCtrl, treeCtrlEx, treeEvent
      -- ** List control
      , ListCtrl, listCtrl, listCtrlEx, listEvent, columns
      -- ** Static text
      , StaticText, staticText
      -- ** SplitterWindow
      , SplitterWindow, splitterWindow      
      -- ** ImageList
      , ImageList, imageList, imageListFromFiles
    ) where

import Graphics.UI.WXCore hiding (Event)

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Window

-- | Create a 'Panel', a window that is normally used as a container for
-- controls. It has a standard background and maintains standard keyboard
-- navigation (ie. /Tab/ moves through the controls).
panel :: Window a -> [Prop (Panel ())] -> IO (Panel ())
panel parent props
  = panelEx parent (wxTAB_TRAVERSAL .+. wxCLIP_CHILDREN .+. wxNO_FULL_REPAINT_ON_RESIZE) props -- .+. wxNO_FULL_REPAINT_ON_RESIZE) props

-- | Create a 'Panel' with a specific style.
panelEx :: Window a -> Style -> [Prop (Panel ())] -> IO (Panel ())
panelEx parent style props
  = do p <- panelCreate parent idAny rectNull style
       windowSetFocus p
       set p props
       return p

instance Form (Panel a) where
  layout
    = writeAttr "layout" windowSetLayout

defaultButton :: Attr (Panel p) (Button ())
defaultButton
  = newAttr "defaultButton" getter setter
  where
    getter panel
      = do p <- panelGetDefaultItem panel
           return (objectCast p)

    setter panel button
      = do panelSetDefaultItem panel button
           focusOn button

-- | Set the initial focus on this control.
focusOn :: Window a -> IO ()
focusOn w
  = windowSetFocus w


-- | Create a 'Notebook'. Layout is managed with the 'tabs' combinator.
notebook :: Window a -> [Prop (Notebook ())] -> IO (Notebook ())
notebook parent props
  = do nb <- notebookCreate parent idAny rectNull wxCLIP_CHILDREN
       set nb props
       return nb

{--------------------------------------------------------------------------------
  Button
--------------------------------------------------------------------------------}

-- | Create a standard push button.
button :: Window a -> [Prop (Button ())] -> IO (Button ())
button parent props
  = buttonEx parent 0 props

-- | Create a minimially sized push button.
smallButton :: Window a -> [Prop (Button ())] -> IO (Button ())
smallButton parent props
  = buttonEx parent wxBU_EXACTFIT props


-- | Create a standard push button with the given flags.
buttonEx :: Window a -> Int -> [Prop (Button ())] -> IO (Button ())
buttonEx parent flags props
  = do b <- buttonCreate parent idAny "?" rectNull flags
       set b props
       return b

instance Commanding (Button a) where
  command  = newEvent "command" buttonGetOnCommand buttonOnCommand

-- | Create a bitmap button. Use the 'image' attribute to set the
-- bitmap.
bitmapButton :: Window a -> [Prop (BitmapButton ())] -> IO (BitmapButton ())
bitmapButton parent props
  = do bb <- bitmapButtonCreate parent idAny nullBitmap rectNull wxBU_AUTODRAW 
       set bb props
       return bb

instance HasImage (BitmapButton a) where
  image 
    = writeAttr "image" setter
    where
      setter w fname
        = withBitmapFromFile fname (bitmapButtonSetBitmapLabel w)


{--------------------------------------------------------------------------------
  Text entry
--------------------------------------------------------------------------------}
-- | Alignment.
data Align   = AlignLeft | AlignRight | AlignCentre
             deriving Eq

-- | Wrap mode.
data Wrap    = WrapNone   -- ^ No wrapping (and show a horizontal scrollbar).
             | WrapLine   -- ^ Wrap lines that are too long at any position.
             | WrapWord   -- ^ Wrap lines that are too long at word boundaries.
             deriving Eq

instance BitMask Align where
  assocBitMask
    = [(AlignCentre,wxALIGN_CENTRE_HORIZONTAL)
      ,(AlignRight, wxALIGN_RIGHT)
      ,(AlignLeft,  wxALIGN_LEFT)]

instance BitMask Wrap where
  assocBitMask
    = [(WrapNone, wxHSCROLL)
      ,(WrapLine, wxTE_LINEWRAP)
      ,(WrapWord, wxTE_WORDWRAP)]

{-
-- note: you can't seem to set alignment after creation in a text control :-(
class Aligned w where
  align :: Attr w Align

instance Aligned (Entry a) where
  align
    = newAttr "entry-align" getter setter
    where
      getter w
        = do st <- get style w
             return (fromBitMask st)

      setter align w
        = set w [style :~ setBitMask align ]

instance Able (TextCtrl a) where
  enable
    = newAttr "entry-enable" textCtrlIsEditable textCtrlSetEditable
-}

-- | Create a single-line text entry control.
textEntry :: Window a -> Align -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textEntry parent align props
  = textCtrlEx parent (toBitMask align) props

-- | Create a multi-line text control with a certain wrap mode
textCtrl :: Window a -> Wrap -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrl parent wrap props
  = textCtrlEx parent (toBitMask wrap .+. wxTE_MULTILINE) props


-- | Create a multi-line text rich-text control with a certain wrap mode
-- Enables font and color settings on windows, while being equal to 'textCtrl'
-- on other platforms.
textCtrlRich :: Window a -> Wrap -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrlRich parent wrap props
  = textCtrlEx parent (toBitMask wrap .+. wxTE_MULTILINE .+. wxTE_RICH2) props

-- | Create a generic text control given a certain style.
textCtrlEx :: Window a -> Style -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrlEx parent flags props
  = do e <- textCtrlCreate parent idAny "" rectNull flags
       set e props
       return e

instance Commanding (TextCtrl a) where
  command = newEvent "command" textCtrlGetOnTextEnter textCtrlOnTextEnter

{--------------------------------------------------------------------------------
  Static text
--------------------------------------------------------------------------------}
-- | Create static text label, see also 'label'.
staticText :: Window a -> [Prop (StaticText ())] -> IO (StaticText ())
staticText parent props
  = do t <- staticTextCreate parent idAny "" rectNull 0 {- (wxALIGN_LEFT + wxST_NO_AUTORESIZE) -}
       set t props
       return t


{--------------------------------------------------------------------------------
  Check box
--------------------------------------------------------------------------------}
instance Commanding (CheckBox a) where
  command = newEvent "command" checkBoxGetOnCommand checkBoxOnCommand

instance Checkable (CheckBox a) where
  checkable
    = enable

  checked
    = newAttr "checked" checkBoxGetValue checkBoxSetValue

-- | Create a new checkbox.
checkBox :: Window a -> [Prop (CheckBox ())] -> IO (CheckBox ())
checkBox parent props
  = do c <- checkBoxCreate parent idAny (replicate 80 ' ') rectNull 0
       set c [text := " "]
       set c props
       return c

{--------------------------------------------------------------------------------
  Choice
--------------------------------------------------------------------------------}
instance Commanding (Choice ()) where
  command = newEvent "command" choiceGetOnCommand choiceOnCommand

instance Selection (Choice ()) where
  selection
    = newAttr "selection" choiceGetSelection choiceSetSelection


instance Items (Choice a) String where
  itemCount
    = readAttr "itemCount" choiceGetCount

  item i
    = newAttr "item" (\w -> choiceGetString w i) (\w x -> choiceSetString w i x)

  itemAppend w x
    = choiceAppend w x

  itemDelete w i
    = choiceDelete w i


-- | Create a choice item to select a one of a list of strings.
-- It takes a boolean that determines if the list is sorted
-- and a list of labels.
choice :: Window a -> Bool -> [String] -> [Prop (Choice ())] -> IO (Choice ())
choice parent sorted labels props
  = do c <- choiceCreate parent idAny rectNull labels (if sorted then wxCB_SORT else 0)
       set c props
       return c

{--------------------------------------------------------------------------------
  ComboBox
--------------------------------------------------------------------------------}
instance Commanding (ComboBox a) where
  command
    = newEvent "command" comboBoxGetOnCommand comboBoxOnCommand

instance Selection (ComboBox a) where
  selection
    = newAttr "selection" comboBoxGetSelection comboBoxSetSelection


-- implemented by choice
{-
instance Items (ComboBox a) String where
  itemCount
    = readAttr "itemCount" comboBoxGetCount

  item i
    = readAttr "item" (\w -> comboBoxGetString w i) -- (\w x -> comboBoxSetString w i x)

  itemAppend w x
    = comboBoxAppend w x

  itemDelete w i
    = comboBoxDelete w i
-}

-- | Create a new combo box with a list of initial entries and a boolean
-- that is 'True' when the entries should be sorted.
comboBox :: Window a -> Bool -> [String] -> [Prop (ComboBox ())] -> IO (ComboBox ())
comboBox parent sorted labels props
  = do cb <- comboBoxCreate parent idAny "" rectNull labels ((if sorted then wxCB_SORT else 0) .+. wxCB_DROPDOWN)
       set cb props
       return cb


{--------------------------------------------------------------------------------
  ListBox
--------------------------------------------------------------------------------}
instance Commanding (ListBox a) where
  command
    = newEvent "command" listBoxGetOnCommand listBoxOnCommand

instance Items (ListBox a) String where
  itemCount
    = readAttr "itemCount" listBoxGetCount

  item i
    = newAttr "item" (\w -> listBoxGetString w i) (\w x -> listBoxSetString w i x)

  itemAppend w x
    = listBoxAppend w x

  itemDelete w i
    = listBoxDelete w i

-- | Pointer to single selection list boxes, deriving from 'ListBox'.
type SingleListBox a  = ListBox (CSingleListBox a)

-- | Abstract type of the 'SingleListBox' class.
data CSingleListBox a = CSingleListBox

instance Selection (SingleListBox a) where
  selection
    = newAttr "selection" listBoxGetSelection (\w x -> listBoxSetSelection w x True)


-- | Pointer to multiple selection list boxes, deriving from 'ListBox'.
type MultiListBox a   = ListBox (CMultiListBox a)

-- | Abstract type of the 'MultiListBox' class.
data CMultiListBox a  = CMultiListBox

instance Selections (MultiListBox a) where
  selections
    = newAttr "selections" listBoxGetSelectionList setter
    where
      setter w is
        = mapM_ (\i -> listBoxSetSelection w i True) is


-- | Create a single selection list box. Takes a boolean that determines if
-- the entries are sorted and a list of labels.
singleListBox :: Window a -> Bool -> [String] -> [Prop (SingleListBox ())] -> IO (SingleListBox ())
singleListBox parent sorted labels props
  = do lb <- listBoxCreate parent idAny rectNull labels
                (wxLB_SINGLE .+. wxHSCROLL .+. wxLB_NEEDED_SB .+. (if sorted then wxLB_SORT else 0))
       let sl = (objectCast lb :: SingleListBox ())
       set sl props
       return sl

-- | Create a multi selection list box with a boolean that determines if
-- the entries are sorted and a list of labels.
multiListBox :: Window a -> Bool -> [String] -> [Prop (MultiListBox ())] -> IO (MultiListBox ())
multiListBox parent sorted labels props
  = do lb <- listBoxCreate parent idAny rectNull labels
              (wxLB_MULTIPLE .+. wxLB_EXTENDED .+. wxHSCROLL .+. wxLB_NEEDED_SB .+. (if sorted then wxLB_SORT else 0))
       let ml = (objectCast lb :: MultiListBox ())
       set ml props
       return ml  

{--------------------------------------------------------------------------------
  RadioBox
--------------------------------------------------------------------------------}
instance Commanding (RadioBox a) where
  command = newEvent "radioBox-command" radioBoxGetOnCommand radioBoxOnCommand

instance Selection (RadioBox a) where
  selection
    = newAttr "selection" radioBoxGetSelection radioBoxSetSelection

instance Items (RadioBox a) String where
  itemCount
    = readAttr "itemCount" radioBoxNumber

  item i
    = newAttr "item" (\r -> radioBoxGetItemLabel r i) (\r s -> radioBoxSetItemLabel r i s)

  itemAppend
    = error "Controls.itemAppend: you can not append items to a radiobox"

  itemDelete
    = error "Controls.itemDelete: you can not delete items of a radiobox"


-- | Create a new radio button group with an initial orientation and a list of
-- labels. Use 'selection' to get the currently selected item.
radioBox :: Window a -> Orientation -> [String] -> [Prop (RadioBox ())] -> IO (RadioBox ())
radioBox parent direction labels props
  = do r <- radioBoxCreate parent idAny title rectNull labels 1 flags
       set r props
       return r
  where
    title
      = if (containsProp "text" props) then " " else ""

    flags
      = (if (direction==Horizontal) then wxRA_SPECIFY_ROWS else wxRA_SPECIFY_COLS)

{--------------------------------------------------------------------------------
  Gauge
--------------------------------------------------------------------------------}
-- | Create a horizontal gauge with a specified integer range (max value).
-- The 'selection' attribute determines the position of the gauge.
hgauge :: Window a -> Int -> [Prop (Gauge ())] -> IO (Gauge ())
hgauge parent range props
  = gaugeEx parent range (wxHORIZONTAL .+. wxGA_SMOOTH) props

-- | Create a vertical gauge with a specified integer range (max value).
-- The 'selection' attribute determines the position of the gauge.
vgauge :: Window a -> Int -> [Prop (Gauge ())] -> IO (Gauge ())
vgauge parent range props
  = gaugeEx parent range (wxVERTICAL .+. wxGA_SMOOTH) props

-- | Create a gauge control.
-- The 'selection' attribute determines the position of the gauge.
gaugeEx :: Window a -> Int -> Style -> [Prop (Gauge ())] -> IO (Gauge ())
gaugeEx parent range style props
  = do g <- gaugeCreate parent idAny range rectNull style 
       set g props
       return g


instance Selection (Gauge a) where
  selection
    = newAttr "selection" getter setter
    where
      getter g
        = do i  <- gaugeGetValue g
             hi <- gaugeGetRange g
             return (max 0 (min hi i))

      setter g i
        = do hi <- gaugeGetRange g
             gaugeSetValue g (max 0 (min hi i))

{--------------------------------------------------------------------------------
  Slider
--------------------------------------------------------------------------------}
instance Commanding (Slider a) where
  command = newEvent "command" sliderGetOnCommand sliderOnCommand

-- | Create a horizontal slider with a specified minimum and maximum. Set
-- the 'Bool' argument to 'True' to show labels (minimumn, maximum, and
-- current value). The 'selection' attribute gives the current value.
hslider :: Window a -> Bool -> Int -> Int -> [Prop (Slider ())] -> IO (Slider ())
hslider parent showLabels min max props
  = sliderEx parent min max (wxHORIZONTAL .+. (if showLabels then wxSL_LABELS else 0)) props

-- | Create a vertical slider with a specified minimum and maximum. Set
-- the 'Bool' argument to 'True' to show labels (minimumn, maximum, and
-- current value). The 'selection' attribute gives the current value.
vslider :: Window a -> Bool -> Int -> Int -> [Prop (Slider ())] -> IO (Slider ())
vslider parent showLabels min max props
  = sliderEx parent min max (wxVERTICAL .+. (if showLabels then wxSL_LABELS else 0)) props

-- | Create a slider with a specified minimum and maximum. The
-- 'selection' attribute gives the current value.
sliderEx :: Window a -> Int -> Int -> Style -> [Prop (Slider ())] -> IO (Slider ())
sliderEx parent min max style props
  = do s <- sliderCreate parent style min min max rectNull style
       set s props
       return s

instance Selection (Slider a) where
  selection
    = newAttr "selection" getter setter
    where
      getter s
        = do i  <- sliderGetValue s
             lo <- sliderGetMin s
             hi <- sliderGetMax s
             return (max lo (min hi i))

      setter s i
        = do lo <- sliderGetMin s
             hi <- sliderGetMax s
             sliderSetValue s (max lo (min hi i))


{--------------------------------------------------------------------------------
  SpinCtrl
--------------------------------------------------------------------------------}
-- | Create a spin control: a text field with up\/down buttons. The value ('selection')
-- is always between a specified minimum and maximum.
spinCtrl :: Window a -> Int -> Int -> [Prop (SpinCtrl ())] -> IO (SpinCtrl ())
spinCtrl parent lo hi props
  = do sc <- spinCtrlCreate parent idAny "" rectNull wxSP_ARROW_KEYS (min lo hi) (max lo hi) lo
       set sc props
       return sc

instance Selection (SpinCtrl a) where
  selection
    = newAttr "selection" getter setter
    where
      getter sc
        = do i  <- spinCtrlGetValue sc
             lo <- spinCtrlGetMin sc
             hi <- spinCtrlGetMax sc
             return (max lo (min hi i))

      setter sc i
        = do lo <- spinCtrlGetMin sc
             hi <- spinCtrlGetMax sc
             spinCtrlSetValue sc (max lo (min hi i))


{--------------------------------------------------------------------------------
  TreeCtrl
--------------------------------------------------------------------------------}
-- | Tree control events.
treeEvent :: Event (TreeCtrl a) (EventTree -> IO ())
treeEvent 
  = newEvent "treeEvent" treeCtrlGetOnTreeEvent treeCtrlOnTreeEvent 

-- | Create a single-selection tree control with buttons (i.e. + and - signs).
treeCtrl :: Window a -> [Prop (TreeCtrl ())] -> IO (TreeCtrl ())
treeCtrl parent props
  = treeCtrlEx parent (wxTR_HAS_BUTTONS .+. wxCLIP_CHILDREN .+. wxNO_FULL_REPAINT_ON_RESIZE) props

-- | Create a tree control.
treeCtrlEx :: Window a -> Style -> [Prop (TreeCtrl ())] -> IO (TreeCtrl ())
treeCtrlEx parent style props
  = do t <- treeCtrlCreate2 parent idAny rectNull style
       set t props
       return t

{--------------------------------------------------------------------------------
  ListCtrl
--------------------------------------------------------------------------------}
-- | The @columns@ attribute controls the columns in a report-view list control.
columns :: Attr (ListCtrl a) [(String,Align,Int)]
columns
  = newAttr "columns" getter setter
  where
    setter l xs
      = do n <- listCtrlGetColumnCount l
           mapM_ (listCtrlDeleteColumn l) (reverse [0..n-1])
           mapM_ (insertColumn l) (zip [0..] xs)
      where
        insertColumn l (idx,(name,align,width))
          = let alignment = case align of
                              AlignRight -> wxLIST_FORMAT_RIGHT
                              AlignCentre-> wxLIST_FORMAT_CENTER
                              other      -> wxLIST_FORMAT_LEFT
            in listCtrlInsertColumn l idx name alignment width

    getter l 
      = do n <- listCtrlGetColumnCount l
           mapM (getColumn l) [0..n]
      where
        getColumn l idx
          = bracket (listCtrlGetColumn2 l idx)
                    (listItemDelete)
                    (\item -> do name      <- listItemGetText item
                                 alignment <- listItemGetAlign item
                                 width     <- listItemGetWidth item
                                 let align | alignment == wxLIST_FORMAT_RIGHT  = AlignRight
                                           | alignment == wxLIST_FORMAT_CENTER = AlignCentre
                                           | otherwise                         = AlignLeft
                                 return (name,align,width)
                    )
               
-- | List control events.
listEvent :: Event (ListCtrl a) (EventList -> IO ())
listEvent
  = newEvent "listEvent" listCtrlGetOnListEvent listCtrlOnListEvent 

-- | Create a report-style list control.
listCtrl :: Window a -> [Prop (ListCtrl ())] -> IO (ListCtrl ())
listCtrl parent props
  = listCtrlEx parent (wxLC_REPORT .+. wxCLIP_CHILDREN .+. wxNO_FULL_REPAINT_ON_RESIZE) props

-- | Create a list control.
listCtrlEx :: Window a -> Style -> [Prop (ListCtrl ())] -> IO (ListCtrl ())
listCtrlEx parent style props
  = do l <- listCtrlCreate parent idAny rectNull style
       set l props
       return l

{--------------------------------------------------------------------------------
  SplitterWindow
--------------------------------------------------------------------------------}
-- | Create a splitter window.
splitterWindow :: Window a -> [Prop (SplitterWindow ())] -> IO (SplitterWindow ())
splitterWindow parent props
  = do s <- splitterWindowCreate parent idAny rectNull (wxSP_LIVE_UPDATE .+. wxCLIP_CHILDREN .+. wxNO_FULL_REPAINT_ON_RESIZE)
       set s props
       return s

{--------------------------------------------------------------------------------
  ImageList
--------------------------------------------------------------------------------}
-- | Create an empty image list that will contain images of the desired size.
imageList :: Size -> IO (ImageList ())
imageList size
  = imageListCreate size True 10

-- | Create an image list containing the images in the supplied file name list
-- that will be scaled towards the desired size.
imageListFromFiles :: Size -> [FilePath] -> IO (ImageList ())
imageListFromFiles size files
  = do images <- imageListCreate size True (length files)
       imageListAddIconsFromFiles images files
       return images