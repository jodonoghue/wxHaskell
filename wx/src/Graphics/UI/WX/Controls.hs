{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Controls
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Controls
    ( -- * Containers
        Panel, panel, defaultButton
      , Notebook, notebook
      , focusOn
      -- * Controls
      -- ** Button
      , Button, button, smallButton
      -- ** Text entry
      , Align(..), Wrap(..)
      , textEntry, textCtrl, textCtrlRich
      -- ** CheckBox
      , CheckBox, checkBox
      -- ** Choice
      , Choice, choice
      -- ** ComboBox
      , ComboBox, comboBox
      -- ** ListBox
      , ListBox, singleListBox
      -- ** RadioBox
      , RadioBox, radioBox
      -- ** Slider
      , Slider
      -- ** Gauge
      , Gauge
      -- ** Static text
      , staticText
      -- * Primitive
      , buttonEx, panelEx, textCtrlEx
    ) where

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXH
import Graphics.UI.WXH.WxcClasses
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.Events

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
  = panelEx parent (wxTAB_TRAVERSAL .+. wxCLIP_CHILDREN .+. wxNO_FULL_REPAINT_ON_RESIZE) props

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
-- Has only effect on windows.
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
-- | Create static text label, see also 'text'.
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

-- | Create a choice item to select a one of a list of strings.
-- It takes a boolean that determines if the list is sorted
-- and a list of labels with corresponding event handlers when the item is selected.
-- The handlers get the created choice item as their argument. The event
-- handlers are installed with the 'command' event and can be overridden.
choice :: Window a -> Bool -> [(String,IO ())] -> [Prop (Choice ())] -> IO (Choice ())
choice parent sorted labels props
  = do c <- choiceCreate parent idAny rectNull (map fst labels) (if sorted then wxCB_SORT else 0)
       set c [on command := select c]
       set c props
       return c
  where
    select c
      = do i <- choiceGetSelection c
           if (i >= 0 && i < length labels)
            then do s <- choiceGetString c i
                    case lookup s labels of
                      Just io -> io
                      Nothing -> snd (labels !! i)
            else return ()

{--------------------------------------------------------------------------------
  ComboBox
--------------------------------------------------------------------------------}
instance Commanding (ComboBox a) where
  command
    = newEvent "command" comboBoxGetOnCommand comboBoxOnCommand

instance Selection (ComboBox a) where
  selection
    = newAttr "selection" comboBoxGetSelection comboBoxSetSelection

-- | Create a new combo box with a boolean argument that specifies if the entries
-- should be sorted and a list of initial entries.
comboBox :: Window a -> Bool -> [String] -> [Prop (ComboBox ())] -> IO (ComboBox ())
comboBox parent sort labels props
  = do cb <- comboBoxCreate parent idAny "" rectNull labels ((if sort then wxCB_SORT else 0) .+. wxCB_DROPDOWN)
       set cb props
       return cb


{--------------------------------------------------------------------------------
  ListBox
--------------------------------------------------------------------------------}
instance Commanding (ListBox a) where
  command
    = newEvent "command" listBoxGetOnCommand listBoxOnCommand

-- | Pointer to single selection list boxes, deriving from 'ListBox'.
type SingleListBox a  = ListBox (CSingleListBox a)

-- | Abstract type of the 'SingleListBox' class.
data CSingleListBox a = CSingleListBox

-- | Pointer to multiple selection list boxes, deriving from 'ListBox'.
type MultiListBox a   = ListBox (CMultiListBox a)

-- | Abstract type of the 'MultiListBox' class.
data CMultiListBox a  = CMultiListBox


singleListBox :: Window a -> Bool -> [(String,IO ())] -> [Prop (SingleListBox ())] -> IO (SingleListBox ())
singleListBox parent sorted labels props
  = do lb <- listBoxCreate parent idAny rectNull (map fst labels)
                (wxLB_SINGLE .+. wxHSCROLL .+. wxLB_NEEDED_SB .+. (if sorted then wxLB_SORT else 0))
       let sl = (objectCast lb :: SingleListBox ())
       set sl [on command := select sl]
       set sl props
       return sl
  where
    select sl
      = do i <- listBoxGetSelection sl
           if (i >= 0 && i < length labels)
            then snd (labels !! i)
            else return ()

{--------------------------------------------------------------------------------
  RadioBox
--------------------------------------------------------------------------------}
instance Commanding (RadioBox a) where
  command = newEvent "radioBox-command" radioBoxGetOnCommand radioBoxOnCommand

instance Selection (RadioBox a) where
  selection
    = newAttr "selection" radioBoxGetSelection radioBoxSetSelection

-- | Create a new radio button group with an initial orientation and a list of
-- labels and corresponding event handlers when the item is selected. The handlers
-- get the created radio box as their argument. The event
-- handlers are installed with the 'command' event and can be overridden.
radioBox :: Window a -> Orientation -> [(String,IO ())] -> [Prop (RadioBox ())] -> IO (RadioBox ())
radioBox parent direction labels props
  = do r <- radioBoxCreate parent idAny title rectNull (map fst labels) 1 flags
       set r [on command := select r]
       set r props
       return r
  where
    title
      = if (containsProp "text" props) then " " else ""

    flags
      = (if (direction==Horizontal) then wxRA_SPECIFY_ROWS else wxRA_SPECIFY_COLS)

    select r
      = do i <- radioBoxGetSelection r
           if (i >= 0 && i < length labels)
            then snd (labels !! i)
            else return ()


{--------------------------------------------------------------------------------
  Slider
--------------------------------------------------------------------------------}
instance Commanding (Slider a) where
  command = newEvent "command" sliderGetOnCommand sliderOnCommand