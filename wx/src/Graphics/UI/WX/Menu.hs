{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Menu
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Defines Menus, toolbars, and statusbars.
    
    The function 'menuPane' is used to create a menu
    that can contain 'menuItem's. Menu items can contain event handlers
    using ('on' 'command'), but they can also be set, using the 'menu'
    function, on a frame or (mdi) window so that the menu command is handled
    in the context of the active window instead of the context of the
    entire application. 

   > do frame <- frame    [text := "Demo"]
   >    file  <- menuPane [text := "&File"]
   >    quit  <- menuItem file "&Quit\tCtrl+Q" [help := "Quit the application"] 
   >    set frame [menuBar        := [file] 
   >              ,on (menu quit) := close frame] 

    Note: /Unfortunately, the 'text' of menu items must be given at creation time
    due to restrictions on GTK./
-}
--------------------s------------------------------------------------------------
module Graphics.UI.WX.Menu
    ( -- * Menu
      -- ** Menu containers
      MenuBar, Menu, menuBar, menuPopup, menuPane, menuHelp
    -- ** Menu events
    , menu, menuId
      -- ** Menu items
    , MenuItem, menuItem, menuCheckItem, menuQuit, menuAbout
    , menuLine, menuSub
    -- * Tool bar
    , ToolBar, toolBar, toolBarEx
    , ToolBarItem, toolMenu, toolItem, toolControl, tool
    -- * Status bar
    , StatusField, statusBar, statusField, statusWidth
    -- * Deprecated
    , menuList, menubar, statusbar
    ) where

import Char( toUpper )
import List( partition, intersperse )
import System.IO.Unsafe (unsafePerformIO)

-- for haddock, we import wxh module selectively
-- import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcClasses hiding (Event)
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.Frame


import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events


{--------------------------------------------------------------------------------
  Menubar
--------------------------------------------------------------------------------}
-- | /Deprecated/: use 'menuBar'.
menubar :: WriteAttr (Frame a) [Menu ()]
menubar
  = menuBar

-- | Set the menu bar of a frame.
menuBar :: WriteAttr (Frame a) [Menu ()]
menuBar
  = writeAttr "menubar" setter
  where
    setter frame menus
      = do mb <- menuBarCreate wxMB_DOCKABLE
           mapM_ (append mb) menus
           frameSetMenuBar frame mb
           -- set delayed menu handlers on the frame
           mapM_ (evtHandlerSetAndResetMenuCommands frame) menus
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
-- | /Deprecated/: use 'menuPane'.
menuList :: [Prop (Menu ())] -> IO (Menu ())
menuList 
  = menuPane 

-- | Create a new menu with a certain title (corresponds with 'text' attribute).
menuPane :: [Prop (Menu ())] -> IO (Menu ())
menuPane props
  = do m <- menuCreate " " wxMENU_TEAROFF
       set m props
       return m

-- | Append a /help/ menu item (@"&Help"@). On some platforms,
-- the /help/ menu is handled specially
menuHelp :: [Prop (Menu ())] -> IO (Menu ())
menuHelp props
  = menuPane ([text := "&Help"] ++ props)

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

-- | Add a menu seperator.
menuLine :: Menu a -> IO ()
menuLine menu
  = menuAppendSeparator menu


-- | Append a menu item. The label can contain
-- menu accellerators by using an ampersand. It can also contain keyboard accellerators
-- after a tab (@'\\t'@) character.
--
-- > menuItem menu "&Open\tCtrl+O" [help := "Opens an existing document"] 
--
menuItem :: Menu a -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuItem menu label props
  = do id <- idCreate
       menuItemId menu id label props

-- | Append a checkable menu item with a certain title (corresponds with 'text' attribute).
menuCheckItem :: Menu a -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuCheckItem menu label props
  = do id <- idCreate
       menuItemEx menu id label True props

-- | Append an /about/ menu item (@"&About..."@). On some platforms,
-- the /about/ menu is handled specially.
menuAbout :: Menu a -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuAbout menu props
  = menuItemId menu wxID_ABOUT "&About..." props

-- | Append an /quit/ menu item (@"&Quit\tCtrl+Q"@). On some platforms,
-- the /quit/ menu is handled specially
menuQuit :: Menu a -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuQuit menu props
  = menuItemId menu wxID_EXIT "&Quit\tCtrl+Q" props

-- | Append a menu item with a specific id and label.
menuItemId :: Menu a -> Id -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuItemId menu id label props
  = menuItemEx menu id label False props

-- | Append a menu item with a specific id, label, and whether it is checkable.
menuItemEx :: Menu a -> Id -> String -> Bool -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuItemEx menu id label isCheck props
  = do menuAppend menu id label "" isCheck
       item <- menuFindItem menu id objectNull
       set item props
       return item

instance Able (MenuItem a) where
  enable = newAttr "enable" menuItemIsEnabled menuItemEnable

instance Textual (MenuItem a) where
  text
    = newAttr "text" menuItemGetText menuItemSetText

instance Help (MenuItem a) where
  help  = newAttr "help" menuItemGetHelp menuItemSetHelp

instance Checkable (MenuItem a) where
  checkable = newAttr "checkable" menuItemIsCheckable (\m c -> menuItemSetCheckable m (intFromBool c))
  checked   = newAttr "checked"   menuItemIsChecked menuItemCheck

instance Identity (MenuItem a) where
  identity  = newAttr "identity" menuItemGetId menuItemSetId

{--------------------------------------------------------------------------------
  Events
--------------------------------------------------------------------------------}
-- | React to menu events.
menu :: MenuItem a -> Event (Window w) (IO ())
menu item
  = let id = unsafePerformIO (get item identity)
    in  menuId id

-- | React to a menu event based on identity.
menuId :: Id -> Event (Window w) (IO ())
menuId id
  = newEvent "menu" (\w -> evtHandlerGetOnMenuCommand w id) (\w h -> evtHandlerOnMenuCommand w id h)
              
{--------------------------------------------------------------------------------
  Menu commands:

  Ok, we would like to set command handlers in two ways:
  1) As an "on command" on the menu item itself. 
  2) With an "on (menu xxx)" on a window. 

  Unfortunately, wxWindows does not support method (1) for menus that are
  part of a menubar and assumes they are set on using (2) on the associated
  frame. We can't tell whether a menu is part of a menubar or popupmenu untill
  the user sets it. Thus we both set the eventhandlers always directly on the
  top level menu (this is good enough for popup menus) and we maintain 
  a list of menu item id's and associated event handler as client data on the
  top level menu. When the menu is set as part of a menubar, we install the
  handlers on the associated frame.
--------------------------------------------------------------------------------}

instance Commanding (MenuItem a) where
  command
    = newEvent "command" menuItemGetOnCommand menuItemOnCommand

menuItemGetOnCommand :: MenuItem a -> IO (IO ())
menuItemGetOnCommand item 
  = do id      <- get item identity
       topmenu <- menuItemGetTopMenu item
       evtHandlerGetOnMenuCommand topmenu id

menuItemOnCommand :: MenuItem a -> IO () -> IO ()
menuItemOnCommand item io
  = do id      <- get item identity
       topmenu <- menuItemGetTopMenu item
       -- always set it on the menu itself (has only effect on popup menus)
       evtHandlerOnMenuCommand topmenu id io
       -- update the haskell event handler list for delayed frame installation
       menuUpdateEvtHandlers topmenu (insert id io)
       -- and set it directly on the frame if already instantiated. 
       frame   <- menuGetFrame topmenu
       when (not (objectIsNull frame)) (evtHandlerOnMenuCommand frame id io)
  where
    insert key val []         = [(key,val)]
    insert key val ((k,v):xs) | key == k  = (key,val):xs
                              | otherwise = (k,v):insert key val xs


-- Get associated frame of a menu in a menubar. Returns NULL for popup and sub menus.
menuGetFrame :: Menu a -> IO (Frame ())
menuGetFrame menu
  = do menubar <- menuGetMenuBar menu
       if (objectIsNull menubar) 
        then return objectNull
        else menuBarGetFrame menubar

-- Get top level menu of a menu item (never null)
menuItemGetTopMenu :: MenuItem a -> IO (Menu ())
menuItemGetTopMenu item
  = do menu <- menuItemGetMenu item
       menuGetTopMenu menu

-- Get the top level menu of a possible sub menu 
menuGetTopMenu :: Menu () -> IO (Menu ())
menuGetTopMenu menu
  = do parent <- menuGetParent menu
       if (objectIsNull parent)
        then return menu
        else menuGetTopMenu parent

-- Set all menu event handlers on a certain window (EvtHandler)
evtHandlerSetAndResetMenuCommands :: EvtHandler a -> Menu b -> IO ()
evtHandlerSetAndResetMenuCommands evtHandler menu
  = do handlers <- menuGetEvtHandlers menu
       menuSetEvtHandlers menu []
       mapM_ (\(id,io) -> evtHandlerOnMenuCommand evtHandler id io) handlers

-- Update the menu event handler list.
menuUpdateEvtHandlers menu f
  = do hs <- menuGetEvtHandlers menu
       menuSetEvtHandlers menu (f hs)

menuGetEvtHandlers :: Menu a -> IO [(Id,IO ())]
menuGetEvtHandlers menu 
  = do mbHandlers <- unsafeEvtHandlerGetClientData menu
       case mbHandlers of
         Nothing -> return []
         Just hs -> return hs

menuSetEvtHandlers :: Menu a -> [(Id,IO ())] -> IO ()
menuSetEvtHandlers menu hs
  = evtHandlerSetClientData menu (return ()) hs 


{--------------------------------------------------------------------------------
  Toolbar
--------------------------------------------------------------------------------}
-- | Create a toolbar window with a divider and text labels.
-- Normally, you can use 'toolMenu' to add tools in the toolbar
-- that behave like normal menu items.
--
-- >  tbar   <- toolBar f []
-- >  toolMenu tbar open  "Open"  "open.png"  []
-- >  toolMenu tbar about "About" "about.png" []
--
toolBar :: Frame a -> [Prop (ToolBar ())] -> IO (ToolBar ())
toolBar parent props
  = toolBarEx parent True True props

-- | Create a toolbar window. The second argument specifies whether text labels
-- should be shown, and the third argument whether a divider line is present
-- above the toolbar.
toolBarEx :: Frame a -> Bool -> Bool -> [Prop (ToolBar ())] -> IO (ToolBar ())
toolBarEx parent showText showDivider props
  = do t <- toolBarCreate parent idAny rectNull 
              ( wxTB_DOCKABLE .+. wxTB_FLAT
                .+. (if showText then wxTB_TEXT else 0)
                .+. (if showDivider then 0 else 0x200 {- wxTB_NODIVIDER -})
              )
       set t props
       frameSetToolBar parent t
       return t

-- | A tool in a toolbar.
data ToolBarItem  = ToolBarItem (ToolBar ()) Id Bool

instance Able ToolBarItem  where
  enable 
    = newAttr "enable" getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = toolBarGetToolEnabled toolbar id

      setter (ToolBarItem toolbar id isToggle) enable
        = toolBarEnableTool toolbar id enable
         

instance Tipped ToolBarItem where
  tooltip 
    = newAttr "tooltip" getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = toolBarGetToolShortHelp toolbar id

      setter (ToolBarItem toolbar id isToggle) txt
        = toolBarSetToolShortHelp toolbar id txt
         
instance Help ToolBarItem  where
  help  
    = newAttr "help" getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = toolBarGetToolLongHelp toolbar id

      setter (ToolBarItem toolbar id isToggle) txt
        = toolBarSetToolLongHelp toolbar id txt
         

instance Checkable ToolBarItem where
  checkable 
    = readAttr "checkable" getter
    where
      getter (ToolBarItem toolbar id isToggle)
        = return isToggle

  checked   
    = newAttr "checked"  getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = toolBarGetToolState toolbar id

      setter (ToolBarItem toolbar id isToggle) toggle
        = toolBarToggleTool toolbar id toggle
         

instance Identity ToolBarItem where
  identity  
    = readAttr "identity" getter
    where
      getter (ToolBarItem toolbar id isToggle)
        = return id


instance Commanding ToolBarItem where
  command
    = newEvent "command" getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = evtHandlerGetOnMenuCommand toolbar id

      setter (ToolBarItem toolbar id isToggle) io
        = evtHandlerOnMenuCommand toolbar id io

-- | React on tool event. (normally handled by 'menu' though, so only use this
-- for orphan toolbar items).
tool :: ToolBarItem -> Event (Window w) (IO ())
tool (ToolBarItem toolbar id isToggle)
  = newEvent "tool" getter setter
  where
    getter w
      = evtHandlerGetOnMenuCommand w id
    setter w io
      = evtHandlerOnMenuCommand w id io

-- | Create a tool bar item based on a menu. Takes a a relevant menu
-- item, a label and an image file (bmp,png,gif,ico,etc.) as arguments. The image
-- file is normally 16x15 pixels.
-- The toolbar item will fire the relevant menu items just as if the menu has been selected.
-- Checkable menus will give a checkable toolbar item. Beware though that checkable tools
-- normally require a specific @on command@ handler to keep them synchronised with the 
-- corresponding menu item.
toolMenu :: ToolBar a -> MenuItem a -> String -> FilePath -> [Prop ToolBarItem] -> IO ToolBarItem
toolMenu toolbar menuitem label bitmapPath props
  = do isToggle <- get menuitem checkable
       id       <- get menuitem identity
       lhelp    <- get menuitem help
       shelp    <- get menuitem help
       withBitmapFromFile bitmapPath $ \bitmap ->
         do toolBarAddTool2 toolbar id label bitmap nullBitmap 
                            (if isToggle then wxITEM_CHECK else wxITEM_NORMAL)
                            shelp lhelp
            let t = ToolBarItem (downcastToolBar toolbar) id isToggle
            set t props
            toolBarRealize toolbar
            return t
       
-- | Create an /orphan/ toolbar item that is unassociated with a menu. Takes a 
-- label, a flag that is 'True' when the item is 'checkable' and a path to an image
-- (bmp,png,gif,ico,etc.) as arguments.
toolItem :: ToolBar a -> String -> Bool -> FilePath -> [Prop ToolBarItem] -> IO ToolBarItem
toolItem toolbar label isCheckable bitmapPath props
  = withBitmapFromFile bitmapPath $ \bitmap ->
    do id <- idCreate
       toolBarAddTool2 toolbar id label bitmap nullBitmap 
                            (if isCheckable then wxITEM_CHECK else wxITEM_NORMAL)
                            "" ""
       let t = ToolBarItem (downcastToolBar toolbar) id isCheckable
       set t props
       toolBarRealize toolbar
       return t

-- | Add an arbitrary control to a toolbar (typically a 'ComboBox'). The control
-- must be created with the toolbar as the parent.
toolControl :: ToolBar a -> Control b -> IO ()
toolControl toolbar control
  = do toolBarAddControl toolbar control
       return ()
   
downcastToolBar :: ToolBar a -> ToolBar ()
downcastToolBar t  = objectCast t

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
-- > field2 <- statusField [text := "hi"]
-- > field3 <- statusField [statusWidth := 50]
-- > set frame [statusBar := [field1,field2,field3]] 
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



-- | /Deprecated/: use 'statusBar'. 
statusbar :: WriteAttr (Frame a) [StatusField]
statusbar
  = statusBar

-- | Specify the statusbar of a frame.
statusBar :: WriteAttr (Frame a) [StatusField]
statusBar
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