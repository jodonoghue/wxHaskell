{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Attributes
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable


   Widgets @w@ can have attributes of type @a@ represented by the type @Attr w a@.
   An attribute can set or read. An example of an attribute is @title@ with type:

   > title :: Attr (Frame a) String

   This means that any object derived from 'Frame' has a 'title' attribute of type 'String'.
   An attribute can be read with the 'get' function:

   > frame # get title           :: IO String

   When an attribute is associated with a value, we call it a /property/ of type @Prop w@.
   Properties are constructed with the (':=') constructor:

   > title := "hello world"      :: Prop (Frame a)

   Properties can be set with the 'set' function:

   > set frame [title := "Hi"]   :: IO ()

   The (':~') constructor is used to transform an attribute with an update function.
   For example, the 'interval' on a timer can be doubled with:

   > set timer [interval :~ (*2)]

   The function 'get', 'set', (':='), and (':~') are polymorphic and work for all widgets, but
   the @title@ attribute just works for frames. Many attributes work for different kind
   of objects and are organised into type classes. Actually, the real type of the
   'title' attribute is:

   > IsFrame w => Attr w String

   and 'Frame' derived objects are part of this class:

   > instance IsFrame (Frame a)


-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Attributes
    (
    -- * Attributes
      Attr, Prop((:=),(:~),(::=),(::~)), ReadAttr, WriteAttr
    , get, set
    , mapAttr, mapAttrW


    -- * Internal
    , newAttr, readAttr, writeAttr, nullAttr, constAttr
    , attrName, propName, containsProp
    ) where

infixr 0 :=,:~,::=,::~

-- | A property of a widget @w@ is an attribute that
-- is already associated with a value. .
data Prop w     = forall a. Attr w a := a         -- ^ Assign a value to an attribute.
                | forall a. Attr w a :~ (a -> a)  -- ^ Apply an update function to an attribute.
                | forall a. Attr w a ::= (w -> a) -- ^ Assign a value to an attribute with the widget as argument.
                | forall a. Attr w a ::~ (w -> a -> a) -- ^ Apply an update function to an attribute with the widget as an argument.


-- | A read-only attribute. Just for documentation purposes.
type ReadAttr w a = Attr w a

-- | A write-only attribute. Just for documentation purposes.
type WriteAttr w a = Attr w a

-- | Widgets @w@ can have attributes of type @a@.
data Attr w a   = Attr String (w -> IO a) (w -> a -> IO ())   -- name, getter, setter


-- | Create a new attribute with a specified name, getter and setter function.
newAttr :: String -> (w -> IO a) -> (w -> a -> IO ()) -> Attr w a
newAttr name getter setter
  = Attr name getter setter

-- | Define a read-only attribute.
readAttr :: String -> (w -> IO a) -> ReadAttr w a
readAttr name getter
  = Attr name getter (\w x -> ioError (userError ("attribute '" ++ name ++ "' is read-only.")))

-- | Define a write-only attribute.
writeAttr :: String -> (w -> a -> IO ()) -> WriteAttr w a
writeAttr name setter
  = Attr name (\w -> ioError (userError ("attribute '" ++ name ++ "' is write-only."))) setter

-- | A dummy attribute.
nullAttr :: String -> WriteAttr w a
nullAttr name
  = writeAttr name (\w x -> return ())

-- | A constant attribute.
constAttr :: String -> a -> Attr w a
constAttr name x
  = newAttr name (\w -> return x) (\w x -> return ())


-- | (@mapAttr get set attr@) maps an attribute of @Attr w a@ to
-- @Attr w b@ where (@get :: a -> b@) is used when the attribute is
-- requested and (@set :: b -> a -> a@) is applied to current
-- value when the attribute is set.
mapAttr :: (a -> b) -> (a -> b -> a) -> Attr w a -> Attr w b
mapAttr get set (Attr name getter setter)
    = Attr name (\w   -> do a <- getter w; return (get a))
                (\w b -> do a <- getter w; setter w (set a b))


-- | (@mapAttrW conv attr@) maps an attribute of @Attr w a@ to
-- @Attr v a@ where (@conv :: v -> w@) is used to convert a widget
-- @v@ into a widget of type @w@.
mapAttrW :: (v -> w) -> Attr w a -> Attr v a
mapAttrW f (Attr name getter setter)
  = Attr name (\v -> getter (f v)) (\v x -> setter (f v) x)


-- | Get the value of an attribute
--
-- > t <- w # get title
--
get :: w -> Attr w a -> IO a
get w (Attr name getter setter)
  = getter w

-- | Set a list of properties.
--
-- > w # set [title := "Hi"]
--
set :: w -> [Prop w] -> IO ()
set w props
  = mapM_ setprop props
  where
    setprop ((Attr name getter setter) := x)
      = setter w x
    setprop ((Attr name getter setter) :~ f)
      = do x <- getter w
           setter w (f x)
    setprop ((Attr name getter setter) ::= f)
      = setter w (f w)
    setprop ((Attr name getter setter) ::~ f)
      = do x <- getter w
           setter w (f w x)


attrName :: Attr w a -> String
attrName (Attr name _ _)
  = name

propName :: Prop w -> String
propName (attr := x)    = attrName attr
propName (attr :~ f)    = attrName attr

containsProp :: String -> [Prop w] -> Bool
containsProp name props
  = any (\p -> propName p == name) props