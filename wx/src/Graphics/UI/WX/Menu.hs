{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Menu
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Menu
    ( -- * Menu
      -- ** Menu containers
      MenuBar, Menu, menubar, menuPopup, menuList
    -- ** Menu events
    , menu, menuId
      -- ** Menu items
    , MenuItem, menuItem, menuExit, menuHelp, menuAbout
    , menuLine, menuSub
    -- * Status bar
    , StatusField, statusbar, statusFixed, statusField
    ) where

import Char( toUpper )
import List( partition, intersperse )
import System.IO.Unsafe (unsafePerformIO)

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXH
import Graphics.UI.WXH.WxcClasses hiding (Event)
import Graphics.UI.WXH.WxcDefs
import Graphics.UI.WXH.Events
import Graphics.UI.WXH.Frame


import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events


{--------------------------------------------------------------------------------
  Menubar
--------------------------------------------------------------------------------}
-- | Set the menu bar of a frame.
menubar :: WriteAttr (Frame a) [Menu ()]
menubar
  = writeAttr "menubar" setter
  where
    setter frame menus
      = do mb <- menuBarCreate wxMB_DOCKABLE
           mapM_ (append mb) menus
           frameSetMenuBar frame mb
           return ()

    append mb menu
      = do title <- menuGetTitle menu
           menuSetTitle menu ""
           menuBarAppend mb menu title

-- | Show a popup menu for a certain window.
menuPopup :: Menu b -> Point -> Window a -> IO ()
menuPopup menu pt parent
  = do windowPopupMenu parent menu pt
       return ()

{--------------------------------------------------------------------------------
  Menu
--------------------------------------------------------------------------------}
-- | Create a new menu with an initial title.
menuList :: String -> [Prop (Menu ())] -> IO (Menu ())
menuList lab props
  = do m <- menuCreate lab wxMENU_TEAROFF
       set m props
       return m

instance Textual (Menu a) where
  text
    = newAttr "text" menuGetTitle menuSetTitle

{--------------------------------------------------------------------------------
  Menu items
--------------------------------------------------------------------------------}
-- | Create a submenu item.
menuSub :: Menu b -> String -> Menu a -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuSub parent lab m props
  = do id <- idCreate
       menuAppendSub parent id lab m ""
       item <- menuFindItem parent id objectNull
       set item props
       return item


-- | Append a menu item with a label, and help text. The help text is by default
-- shown in the first field of the status bar. The label can contain
-- menu accellerators by using an ampersand. It can also contain keyboard accellerators
-- after a tab (@'\\t'@) character.
--
-- > menuItem "&Open\tCtrl+O" "Opens an existing document" [] menu
--
menuItem :: Menu a -> String -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuItem menu lab hlp props
  = do id <- idCreate
       menuItemId menu id False lab hlp props

-- | Append an /about/ menu item with a label, and help text. On some platforms,
-- the /about/ menu is handled specially.
menuAbout :: Menu a -> String -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuAbout menu lab hlp props
  = menuItemId menu  wxID_ABOUT False lab hlp props

-- | Append an /exit/ menu item with a label, and help text. On some platforms,
-- the /exit/ menu is handled specially
menuExit :: Menu a -> String -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuExit menu lab hlp props
  = menuItemId menu wxID_EXIT False lab hlp props

-- | Append a /help/ menu item with a label, and help text. On some platforms,
-- the /help/ menu is handled specially
menuHelp :: Menu a -> String -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuHelp menu lab hlp props
  = menuItemId menu wxID_HELP False lab hlp props

-- | Append a menu item with a specific id, check ability, label, and help text.
menuItemId :: Menu a -> Int -> Bool -> String -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuItemId menu id checkable lab hlp props
  = do menuAppend menu id lab hlp checkable
       item <- menuFindItem menu id objectNull
       set item props
       return item

instance Able (MenuItem a) where
  enable = newAttr "enable" menuItemIsEnabled menuItemEnable

instance Textual (MenuItem a) where
  text
    = newAttr "label" menuItemGetText menuItemSetText

instance Help (MenuItem a) where
  help  = newAttr "help" menuItemGetHelp menuItemSetHelp

instance Checkable (MenuItem a) where
  checkable = newAttr "checkable" menuItemIsCheckable (\m c -> menuItemSetCheckable m (intFromBool c))
  checked   = newAttr "checked"   menuItemIsChecked menuItemCheck

instance Identity (MenuItem a) where
  identity  = newAttr "identity" menuItemGetId menuItemSetId


-- | Add a menu seperator.
menuLine :: Menu a -> IO ()
menuLine menu
  = menuAppendSeparator menu


{--------------------------------------------------------------------------------
  Events
--------------------------------------------------------------------------------}
-- | React to menu events.
--
-- > do frame <- frame [label := "Demo"]
-- >    file  <- menuList "&File" []
-- >    quit  <- menuItem "Quit\tCtrl+Q" "Quit the application" [] file
-- >    set [menubar := [file]] frame
-- >    set [on (menu quit) := close frame] frame
--
menu :: MenuItem a -> Event (Window w) (IO ())
menu item
  = let id = unsafePerformIO (get item identity)
    in  menuId id

-- | React to a menu event based on identity.
menuId :: Id -> Event (Window w) (IO ())
menuId id
  = newEvent "menu" (\w -> windowGetOnMenuCommand w id) (\w h -> windowOnMenuCommand w id h)

{--------------------------------------------------------------------------------
  Statusbar
--------------------------------------------------------------------------------}
data StatusField  = SF Int (Var (StatusBar ())) (Var Int) (Var String)

-- | Create a fixed width (in pixels) status field.
statusFixed :: Int -> [Prop StatusField] -> IO StatusField
statusFixed pixels props
  = do vsbar <- varCreate objectNull
       vidx  <- varCreate (-1)
       vtext <- varCreate ""
       let sf = SF pixels vsbar vidx vtext
       set sf props
       return sf

-- | Create a variable witdth status field. The argument determines the stretch
-- weight in relation to the other fields. Here is an example of a statusbar
-- with three fields, where the last field is 50 pixels wide, the first takes
-- 66% of the remaining space and the second field 33%.
--
-- > field1 <- statusField 2 []
-- > field2 <- statusField 1 [label := "hi"]
-- > field3 <- statusFixed 50  []
-- > set [statusbar := [field1,field2,field3]] frame
--
statusField :: Int -> [Prop StatusField] -> IO StatusField
statusField weight props
  = statusFixed (-weight) props

instance Textual StatusField where
  text
    = newAttr "text" get set
    where
      get (SF _ vsbar vidx vtext)
        = varGet vtext

      set (SF _ vsbar vidx vtext)  text
        = do varSet vtext text
             idx <- varGet vidx
             if (idx >= 0)
              then do sbar <- varGet vsbar
                      statusBarSetStatusText sbar text idx
              else return ()


-- | Specify the statusbar of a frame.
statusbar :: WriteAttr (Frame a) [StatusField]
statusbar
  = writeAttr "statusbar" set
  where
    set f fields
      = do sb <- statusBarCreateFields f (map width fields)
           mapM_ (setsb sb) (zip [0..] fields )

    width (SF w _ _ _)
      = w

    setsb sb (idx,SF _ vsbar vidx vtext)
      = do varSet vsbar sb
           varSet vidx idx
           text <- varGet vtext
           statusBarSetStatusText sb text idx -- initialize