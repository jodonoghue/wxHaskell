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
      MenuBar, Menu, menubar, menuPopup, menuList, menuHelp
    -- ** Menu events
    , menu, menuId
      -- ** Menu items
    , MenuItem, menuItem, menuQuit, menuAbout
    , menuLine, menuSub
    -- * Status bar
    , StatusField, statusbar, statusField, statusWidth
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

-- | Append a /help/ menu item with an initial title. On some platforms,
-- the /help/ menu is handled specially
menuHelp :: String -> [Prop (Menu ())] -> IO (Menu ())
menuHelp lab props
  = menuList lab props


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
-- > menuItem menu "&Open\tCtrl+O" "Opens an existing document" [] 
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

-- | Append an /quit/ menu item with a label, and help text. On some platforms,
-- the /quit/ menu is handled specially
menuQuit :: Menu a -> String -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuQuit menu lab hlp props
  = menuItemId menu wxID_EXIT False lab hlp props

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

instance Commanding (MenuItem a) where
  command
    = newEvent "command" getter setter
    where
      getter item
        = do id   <- get item identity
             menu <- menuItemGetMenu item
             evtHandlerGetOnMenuCommand menu id

      setter item h
        = do id   <- get item identity
             menu <- menuItemGetMenu item
             evtHandlerOnMenuCommand menu id h
                   

-- | Add a menu seperator.
menuLine :: Menu a -> IO ()
menuLine menu
  = menuAppendSeparator menu


{--------------------------------------------------------------------------------
  Events
--------------------------------------------------------------------------------}
-- | React to menu events.
--
-- > do frame <- frame [text := "Demo"]
-- >    file  <- menuList "&File" []
-- >    quit  <- menuItem file "Quit\tCtrl+Q" "Quit the application" [] 
-- >    set frame [menubar := [file]] 
-- >    set frame [on (menu quit) := close frame] 
--
-- Note that popup menus can also use a 'command' handler on the menu item itself.
menu :: MenuItem a -> Event (Window w) (IO ())
menu item
  = let id = unsafePerformIO (get item identity)
    in  menuId id

-- | React to a menu event based on identity.
menuId :: Id -> Event (Window w) (IO ())
menuId id
  = newEvent "menu" (\w -> evtHandlerGetOnMenuCommand w id) (\w h -> evtHandlerOnMenuCommand w id h)

                 

{--------------------------------------------------------------------------------
  Statusbar
--------------------------------------------------------------------------------}
data StatusField  = SF (Var Int) (Var (StatusBar ())) (Var Int) (Var String)

-- | The status width attribute determines the width of a status bar field.
-- A negative width makes the field strechable. The width than determines
-- the amount of stretch in relation with other fields. The default 
-- status width is @-1@, ie. all fields stretch evenly.
--
-- Here is an example of a statusbar
-- with three fields, where the last field is 50 pixels wide, the first takes
-- 66% of the remaining space and the second field 33%.
--
-- > field1 <- statusField [statusWidth := -2]
-- > field2 <- statusField [label := "hi"]
-- > field3 <- statusField [statusWidth := 50]
-- > set [statusbar := [field1,field2,field3]] frame
--
statusWidth :: Attr StatusField Int
statusWidth 
  = newAttr "statusWidth" getter setter
  where
    getter (SF vwidth _ _ _)
      = varGet vwidth

    setter (SF vwidth _ _ _) w
      = varSet vwidth w

-- | Create a status field.
statusField :: [Prop StatusField] -> IO StatusField
statusField props
  = do vwidth<- varCreate (-1)
       vsbar <- varCreate objectNull
       vidx  <- varCreate (-1)
       vtext <- varCreate ""
       let sf = SF vwidth vsbar vidx vtext
       set sf props
       return sf


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
      = do ws <- mapM (\field -> get field statusWidth) fields
           sb <- statusBarCreateFields f ws
           mapM_ (setsb sb) (zip [0..] fields )

    setsb sb (idx,SF _ vsbar vidx vtext)
      = do varSet vsbar sb
           varSet vidx idx
           text <- varGet vtext
           statusBarSetStatusText sb text idx -- initialize