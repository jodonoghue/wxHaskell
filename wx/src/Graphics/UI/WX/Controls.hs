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
      , processEnter, processTab
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
--
-- * Attributes: 'defaultButton', 'focusOn' 
--
-- * Instances: 'Form' -- 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled',
--             'Textual', 'Literate' 
panel :: Window a -> [Prop (Panel ())] -> IO (Panel ())
panel parent props
  = panelEx parent (wxTAB_TRAVERSAL .+. wxCLIP_CHILDREN .+. noFullRepaintOnResize props) props 
      -- .+. wxNO_FULL_REPAINT_ON_RESIZE) props 

-- | Create a 'Panel' with a specific style.
--
-- * Attributes: 'defaultButton', 'focusOn' 
--
-- * Instances: 'Form' -- 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled',
--             'Textual', 'Literate', 'Reactive', 'Paint' 
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
           buttonSetDefault button
           focusOn button

-- | Set the initial focus on this control.
focusOn :: Window a -> IO ()
focusOn w
  = windowSetFocus w


-- | Create a 'Notebook'. Layout is managed with the 'tabs' combinator.
--
-- * Instances: 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled',
--             'Textual', 'Literate', 'Reactive', 'Paint' 
notebook :: Window a -> [Prop (Notebook ())] -> IO (Notebook ())
notebook parent props
  = do nb <- notebookCreate parent idAny rectNull wxCLIP_CHILDREN
       set nb props
       return nb

{--------------------------------------------------------------------------------
  Button
--------------------------------------------------------------------------------}

-- | Create a standard push button.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
button :: Window a -> [Prop (Button ())] -> IO (Button ())
button parent props
  = buttonEx parent 0 props

-- | Create a minimially sized push button.
--
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
smallButton :: Window a -> [Prop (Button ())] -> IO (Button ())
smallButton parent props
  = buttonEx parent wxBU_EXACTFIT props


-- | Create a standard push button with the given flags.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
buttonEx :: Window a -> Int -> [Prop (Button ())] -> IO (Button ())
buttonEx parent flags props
  = do b <- buttonCreate parent idAny "?" rectNull flags
       set b props
       return b

instance Commanding (Button a) where
  command  = newEvent "command" buttonGetOnCommand buttonOnCommand

-- | Create a bitmap button. Use the 'image' attribute to set the
-- bitmap.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
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
        = do fpath <- getAbsoluteFilePath fname
             withBitmapFromFile fpath (bitmapButtonSetBitmapLabel w)


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
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
textEntry :: Window a -> Align -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textEntry parent align props
  = textCtrlEx parent (toBitMask align) props

-- | Create a multi-line text control with a certain wrap mode
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
textCtrl :: Window a -> Wrap -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrl parent wrap props
  = textCtrlEx parent (toBitMask wrap .+. wxTE_MULTILINE) props


-- | Create a multi-line text rich-text control with a certain wrap mode
-- Enables font and color settings on windows, while being equal to 'textCtrl'
-- on other platforms.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
textCtrlRich :: Window a -> Wrap -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrlRich parent wrap props
  = textCtrlEx parent (toBitMask wrap .+. wxTE_MULTILINE .+. wxTE_RICH2) props

-- | Create a generic text control given a certain style.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
textCtrlEx :: Window a -> Style -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrlEx parent flags props
  = do e <- textCtrlCreate parent idAny "" rectNull flags
       set e props
       return e

instance Commanding (TextCtrl a) where
  command = newEvent "command" textCtrlGetOnTextEnter textCtrlOnTextEnter

-- | Process @enter@ key events, used in a 'comboBox' or 'textCtrl' and
-- catched using a 'on' 'command' handler.
-- (otherwise pressing @Enter@ is either processed 
-- internally by the control or used for navigation between dialog controls). 
processEnter :: Styled w => Attr w Bool
processEnter 
  = newAttr "processEnter" getter setter
  where
    getter w 
      = do s <- get w style
           return (bitsSet wxTE_PROCESS_ENTER s)
    setter w p
      = set w [style :~ \stl -> stl .+. wxTE_PROCESS_ENTER]


-- | Process @tab@ key events, used in a 'comboBox' or 'textCtrl'. 
-- (otherwise pressing @Tab@ is either processed 
-- internally by the control or used for navigation between dialog controls). 
processTab :: Styled w => Attr w Bool
processTab 
  = newAttr "processTab" getter setter
  where
    getter w 
      = do s <- get w style
           return (bitsSet wxTE_PROCESS_TAB s)
    setter w p
      = set w [style :~ \stl -> stl .+. wxTE_PROCESS_TAB]


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
    = enabled

  checked
    = newAttr "checked" checkBoxGetValue checkBoxSetValue

-- | Create a new checkbox.
--
-- * Instances: 'Commanding','Checkable' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
checkBox :: Window a -> [Prop (CheckBox ())] -> IO (CheckBox ())
checkBox parent props
  = do c <- checkBoxCreate parent idAny "" rectNull 0
       set c props
       return c

{--------------------------------------------------------------------------------
  Choice
--------------------------------------------------------------------------------}
instance Selecting (Choice ()) where
  select = newEvent "select" choiceGetOnCommand choiceOnCommand

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
--
-- * Instances: 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
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
    = newEvent "command" comboBoxGetOnTextEnter comboBoxOnTextEnter

instance Selecting (ComboBox a) where
  select
    = newEvent "select" comboBoxGetOnCommand comboBoxOnCommand

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
--
-- * Instances: 'Selecting', 'Commanding','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
-- A 'command' event is triggered when the @enter@ key is pressed and when
-- 'processEnter' has been set to 'True'.
comboBox :: Window a -> Bool -> [String] -> [Prop (ComboBox ())] -> IO (ComboBox ())
comboBox parent sorted labels props
  = do cb <- comboBoxCreate parent idAny "" rectNull labels ((if sorted then wxCB_SORT else 0) .+. wxCB_DROPDOWN)
       set cb props
       return cb


{--------------------------------------------------------------------------------
  ListBox
--------------------------------------------------------------------------------}
instance Selecting (ListBox a) where
  select
    = newEvent "select" listBoxGetOnCommand listBoxOnCommand

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
-- the entries are sorted.
--
-- * Instances: 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
singleListBox :: Window a -> Bool -> [Prop (SingleListBox ())] -> IO (SingleListBox ())
singleListBox parent sorted props
  = do lb <- listBoxCreate parent idAny rectNull []
                (wxLB_SINGLE .+. wxHSCROLL .+. wxLB_NEEDED_SB .+. (if sorted then wxLB_SORT else 0))
       let sl = (objectCast lb :: SingleListBox ())
       set sl props
       return sl

-- | Create a multi selection list box with a boolean that determines if
-- the entries are sorted.
----
-- * Instances: 'Selecting','Selections','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
multiListBox :: Window a -> Bool -> [Prop (MultiListBox ())] -> IO (MultiListBox ())
multiListBox parent sorted props
  = do lb <- listBoxCreate parent idAny rectNull []
              (wxLB_MULTIPLE .+. wxLB_EXTENDED .+. wxHSCROLL .+. wxLB_NEEDED_SB .+. (if sorted then wxLB_SORT else 0))
       let ml = (objectCast lb :: MultiListBox ())
       set ml props
       return ml  

{--------------------------------------------------------------------------------
  RadioBox
--------------------------------------------------------------------------------}
instance Selecting (RadioBox a) where
  select 
    = newEvent "select" radioBoxGetOnCommand radioBoxOnCommand

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
--
-- * Instances: 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
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
--
-- * Instances: 'Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
hgauge :: Window a -> Int -> [Prop (Gauge ())] -> IO (Gauge ())
hgauge parent range props
  = gaugeEx parent range (wxHORIZONTAL .+. wxGA_SMOOTH) props

-- | Create a vertical gauge with a specified integer range (max value).
-- The 'selection' attribute determines the position of the gauge.
--
-- * Instances: 'Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
vgauge :: Window a -> Int -> [Prop (Gauge ())] -> IO (Gauge ())
vgauge parent range props
  = gaugeEx parent range (wxVERTICAL .+. wxGA_SMOOTH) props

-- | Create a gauge control.
-- The 'selection' attribute determines the position of the gauge.
--
-- * Instances: 'Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
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
--
-- * Instances: 'Commanding','Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
hslider :: Window a -> Bool -> Int -> Int -> [Prop (Slider ())] -> IO (Slider ())
hslider parent showLabels min max props
  = sliderEx parent min max (wxHORIZONTAL .+. (if showLabels then wxSL_LABELS else 0)) props

-- | Create a vertical slider with a specified minimum and maximum. Set
-- the 'Bool' argument to 'True' to show labels (minimumn, maximum, and
-- current value). The 'selection' attribute gives the current value.
--
-- * Instances: 'Commanding','Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
vslider :: Window a -> Bool -> Int -> Int -> [Prop (Slider ())] -> IO (Slider ())
vslider parent showLabels min max props
  = sliderEx parent min max (wxVERTICAL .+. (if showLabels then wxSL_LABELS else 0)) props

-- | Create a slider with a specified minimum and maximum. The
-- 'selection' attribute gives the current value.
--
-- * Instances: 'Commanding','Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
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
--
-- * Instances: 'Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
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
--
-- * Attributes: 'treeEvent'
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
treeCtrl :: Window a -> [Prop (TreeCtrl ())] -> IO (TreeCtrl ())
treeCtrl parent props
  = treeCtrlEx parent (wxTR_HAS_BUTTONS .+. wxCLIP_CHILDREN .+. noFullRepaintOnResize props) props

-- | Create a tree control.
--
-- * Attributes: 'treeEvent'
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
treeCtrlEx :: Window a -> Style -> [Prop (TreeCtrl ())] -> IO (TreeCtrl ())
treeCtrlEx parent style props
  = do t <- treeCtrlCreate2 parent idAny rectNull style
       set t props
       return t

{--------------------------------------------------------------------------------
  ListCtrl
--------------------------------------------------------------------------------}
instance Items (ListCtrl a) [String] where
  itemCount
    = readAttr "itemCount" listCtrlGetItemCount

  item i
    = newAttr "item" getter setter
    where
      getter l
        = bracket listItemCreate
                  listItemDelete
                  (\li -> do count <- listCtrlGetColumnCount l
                             mapM (\column -> do listItemSetColumn li (column-1)
                                                 listCtrlGetItem l li
                                                 listItemGetText li) [1..count])
                             
        
      setter l texts
        = do count <- listCtrlGetItemCount l
             when (i == count) (do listCtrlInsertItemWithLabel l i (show i) (-1); return ())
             mapM_ (\(column,txt) -> listCtrlSetItem l i column txt (-1)) (zip [0..] texts)

  itemAppend l texts
    = do count <- listCtrlGetItemCount l
         listCtrlInsertItemWithLabel l count (show count) (-1)
         mapM_ (\(column,txt) -> listCtrlSetItem l count column txt (-1)) (zip [0..] texts)

  itemDelete l i
    = do listCtrlDeleteItem l i
         return ()

  itemsDelete l
    = do listCtrlDeleteAllItems l 
         return ()

-- | The @columns@ attribute controls the columns in a report-view list control.
columns :: Attr (ListCtrl a) [(String,Align,Int)]
columns
  = newAttr "columns" getter setter
  where
    setter l xs
      = do n <- listCtrlGetColumnCount l
           mapM_ (\c -> listCtrlDeleteColumn l 0) (reverse [1..n])
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
--
-- * Attributes: 'listEvent', 'columns'
--
-- * Instances: 'Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
listCtrl :: Window a -> [Prop (ListCtrl ())] -> IO (ListCtrl ())
listCtrl parent props
  = listCtrlEx parent (wxLC_REPORT .+. wxCLIP_CHILDREN .+. noFullRepaintOnResize props) props

-- | Create a list control.
--
-- * Attributes: 'listEvent', 'columns'
--
-- * Instances: 'Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
listCtrlEx :: Window a -> Style -> [Prop (ListCtrl ())] -> IO (ListCtrl ())
listCtrlEx parent style props
  = do l <- listCtrlCreate parent idAny rectNull style
       set l props
       return l

{--------------------------------------------------------------------------------
  SplitterWindow
--------------------------------------------------------------------------------}
-- | Create a splitter window.
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--             
splitterWindow :: Window a -> [Prop (SplitterWindow ())] -> IO (SplitterWindow ())
splitterWindow parent props
  = do s <- splitterWindowCreate parent idAny rectNull 
            (wxSP_LIVE_UPDATE .+. wxCLIP_CHILDREN .+. noFullRepaintOnResize props)
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
       imageListAddIconsFromFiles images size files
       return images
