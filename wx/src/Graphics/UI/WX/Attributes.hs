{-# OPTIONS -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  Attributes
    Copyright   :  (c) Daan Leijen 2003
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable


   Widgets @w@ can have attributes of type @a@ represented by the type 'Attr' @w a@.
   An example of an attribute is 'Graphics.UI.WX.Classes.text' with type:

   > text :: Attr (Window a) String

   This means that any object derived from 'Window' has a 'Graphics.UI.WX.Classes.text' attribute of type 'String'.
   An attribute can be read with the 'get' function:

   > get w title           :: IO String

   When an attribute is associated with a value, we call it a /property/ of type 'Prop' @w@.
   Properties are constructed by assigning a value to an attribute with the (':=') constructor:

   > text := "hello world"      :: Prop (Window a)

   A list of properties can be set with the 'set' function:

   > set w [text := "Hi"]   :: IO ()

   The (':~') constructor is used to transform an attribute value with an update function.
   For example, the 'interval' on a timer can be doubled with the following expression.

   > set timer [interval :~ (*2)]

   The functions 'get', 'set', (':='), and (':~') are polymorphic and work for all widgets, but
   the @text@ attribute just works for windows. Many attributes work for different kind
   of objects and are organised into type classes. Actually, the real type of the
   'Graphics.UI.WX.Classes.text' attribute is:

   > Textual w => Attr w String

   and 'Window' derived objects are part of this class:

   > instance Textual (Window a)

   But also menus and status fields:

   > instance Textual (Menu a)
   > instance Textual (StatusField)

   Sometimes, it is convenient to also get a reference to the object itself when setting
   a property. The operators ('::=') and ('::~') provide this reference.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Attributes
    (
    -- * Attributes
      Attr, Prop((:=),(:~),(::=),(::~)), ReadAttr, WriteAttr
    , get, set
    , mapAttr, mapAttrW


    -- * Internal
    -- ** Attributes
    , newAttr, readAttr, writeAttr, nullAttr, constAttr
    -- ** Reflection
    , attrName, propName, containsProp
    -- ** Reflective attributes
    , reflectiveAttr, getPropValue
    ) where

import Data.Dynamic

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
data Attr w a   = Attr String (Maybe (a -> Dynamic)) (w -> IO a) (w -> a -> IO ())   -- name, getter, setter


-- | Create a /reflective/ attribute with a specified name: value can possibly
-- retrieved using 'getPropValue'. Note: the use of this function is discouraged
-- as it leads to non-compositional code.
reflectiveAttr :: Typeable a => String -> (w -> IO a) -> (w -> a -> IO ()) -> Attr w a
reflectiveAttr name getter setter
  = Attr name (Just toDyn) getter setter

-- | Create a new attribute with a specified name, getter and setter function.
newAttr :: String -> (w -> IO a) -> (w -> a -> IO ()) -> Attr w a
newAttr name getter setter
  = Attr name Nothing getter setter

-- | Define a read-only attribute.
readAttr :: String -> (w -> IO a) -> ReadAttr w a
readAttr name getter
  = newAttr name getter (\w x -> ioError (userError ("attribute '" ++ name ++ "' is read-only.")))

-- | Define a write-only attribute.
writeAttr :: String -> (w -> a -> IO ()) -> WriteAttr w a
writeAttr name setter
  = newAttr name (\w -> ioError (userError ("attribute '" ++ name ++ "' is write-only."))) setter

-- | A dummy attribute.
nullAttr :: String -> WriteAttr w a
nullAttr name
  = writeAttr name (\w x -> return ())

-- | A constant attribute.
constAttr :: Typeable a => String -> a -> Attr w a
constAttr name x
  = newAttr name (\w -> return x) (\w x -> return ())


-- | (@mapAttr get set attr@) maps an attribute of @Attr w a@ to
-- @Attr w b@ where (@get :: a -> b@) is used when the attribute is
-- requested and (@set :: a -> b -> a@) is applied to current
-- value when the attribute is set.
mapAttr :: (a -> b) -> (a -> b -> a) -> Attr w a -> Attr w b
mapAttr get set (Attr name reflect getter setter)
    = Attr name Nothing
                (\w   -> do a <- getter w; return (get a))
                (\w b -> do a <- getter w; setter w (set a b))


-- | (@mapAttrW conv attr@) maps an attribute of @Attr w a@ to
-- @Attr v a@ where (@conv :: v -> w@) is used to convert a widget
-- @v@ into a widget of type @w@.
mapAttrW :: (v -> w) -> Attr w a -> Attr v a
mapAttrW f (Attr name reflect getter setter)
  = Attr name reflect (\v -> getter (f v)) (\v x -> setter (f v) x)


-- | Get the value of an attribute
--
-- > t <- get w text
--
get :: w -> Attr w a -> IO a
get w (Attr name reflect getter setter)
  = getter w

-- | Set a list of properties.
--
-- > set w [text := "Hi"]
--
set :: w -> [Prop w] -> IO ()
set w props
  = mapM_ setprop props
  where
    setprop ((Attr name reflect getter setter) := x)
      = setter w x
    setprop ((Attr name reflect getter setter) :~ f)
      = do x <- getter w
           setter w (f x)
    setprop ((Attr name reflect getter setter) ::= f)
      = setter w (f w)
    setprop ((Attr name reflect getter setter) ::~ f)
      = do x <- getter w
           setter w (f w x)


-- | Retrieve the name of an attribute
attrName :: Attr w a -> String
attrName (Attr name _ _ _)
  = name

-- | Retrieve the name of a property.
propName :: Prop w -> String
propName (attr := x)    = attrName attr
propName (attr :~ f)    = attrName attr
propName (attr ::= f)   = attrName attr
propName (attr ::~ f)   = attrName attr

-- | Is a certain property in a list of properties?
containsProp :: String -> [Prop w] -> Bool
containsProp name props
  = any (\p -> propName p == name) props

-- | Get a value of a reflective property. Only works on attributes
-- created with 'reflectiveAttr' and when the property is set using ':='.
-- Returns 'Nothing' whenever the property is not present or when the types
-- do not match.
getPropValue :: Typeable a => Attr w a -> [Prop w] -> Maybe a
getPropValue (Attr name1 (Just todyn1) _ _) ((Attr name2 (Just todyn2) _ _ := x):props)
  | name1 == name2  = fromDynamic (todyn2 x)
getPropValue attr (prop:props)
  = getPropValue attr props
getPropValue attr []
  = Nothing