-----------------------------------------------------------------------------------------
{-| Module      :  WxcObject
    Copyright   :  (c) Daan Leijen 2003, 2004
    License     :  wxWindows

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

    Basic object type.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.WxcObject(
            -- * Object types
              Object, objectNull, objectIsNull, objectCast, objectIsManaged
            , objectFromPtr, managedObjectFromPtr
            , withObjectPtr, withObjectRef
            , objectFinalize, withObjectResult, withManagedResultPtr 
            ) where

import Control.Exception 
import System.IO.Unsafe( unsafePerformIO )
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

{- note: for GHC 5.04, replace the following two imports by "import Foreign.ForeignPtr" -}
import Foreign.ForeignPtr hiding (newForeignPtr,addForeignPtrFinalizer)
import Foreign.Concurrent

{-----------------------------------------------------------------------------------------
    Objects
-----------------------------------------------------------------------------------------}

{- | An @Object a@ is a pointer to an object of type @a@. The @a@ parameter is used
   to encode the inheritance relation. When the type parameter is unit @()@, it denotes
   an object of exactly that class, when the parameter is a type variable @a@, it
   specifies an object that is at least an instance of that class. For example in 
   wxWindows, we have the following class hierarchy:

   > EvtHandler
   >   |- Window
   >        |- Frame
   >        |- Control
   >            |- Button
   >            |- Radiobox

   In wxHaskell, all the creation functions will return objects of exactly that
   class and use the @()@ type:

   > frameCreate :: Window a -> ... -> IO (Frame ())
   > buttonCreate :: Window a -> ... -> IO (Button ())
   > ...

   In contrast, all the /this/ (or /self/) pointers of methods can take objects
   of any instance of that class and have a type variable, for example:

   > windowSetClientSize :: Window a -> Size -> IO ()
   > controlSetLabel     :: Control a -> String -> IO ()
   > buttonSetDefault    :: Button a -> IO ()

   This means that we can use @windowSetClientSize@ on any window, including
   buttons and frames, but we can only use @controlSetLabel@ on controls, not
   includeing frames. 

   In wxHaskell, this works since a @Frame ()@ is actually a type synonym for
   @Window (CFrame ())@ (where @CFrame@ is an abstract data type). We can thus
   pass a value of type @Frame ()@ to anything that expects some @Window a@.
   For a button this works too, as it is a synonym for @Control (CButton ())@
   which is in turn a synonym for @Window (CControl (CButton ()))@. Note that
   we can\'t pass a frame to something that expects a value of type @Control a@.
   Of course, a @Window a@ is actually a type synonym for @EvtHandler (CWindow a)@.
   If you study the documentation in "Graphics.UI.WXH.WxcClasses" closely, you
   can discover where this chain ends :-).  

   Objects are not automatically deleted. Normally you can use a delete function
   like @windowDelete@ to delete an object. However, almost all objects in the
   wxWindows library are automatically deleted by the library. The only objects
   that should be used with care are resources as bitmaps, fonts and brushes.
-}
data Object a   = Object  !(Ptr a)
                | Managed !(ForeignPtr (Ptr a)) (Ptr a -> IO ())


instance Eq (Object a) where
  obj1 == obj2
    = unsafePerformIO $
      withObjectPtr obj1 $ \p1 ->
      withObjectPtr obj2 $ \p2 ->
      return (p1 == p2)

instance Show (Object a) where
  show obj
    = unsafePerformIO $
      withObjectPtr obj $ \p ->
      return (show p)

-- | A null object. Use with care.
objectNull :: Object a
objectNull
  = Object nullPtr

-- | Is this a managed object.
objectIsManaged :: Object a -> Bool
objectIsManaged obj
  = case obj of
      Managed fp final  -> True
      _                 -> False

-- | Test for null object.
objectIsNull :: Object a -> Bool
objectIsNull obj
  = unsafePerformIO $
    withObjectPtr obj $ \p -> return (p == nullPtr)
      

-- | Cast an object to another type. Use with care.
objectCast :: Object a -> Object b
objectCast obj
  = case obj of
      Object p         -> Object (castPtr p)
      Managed fp final -> Managed (castForeignPtr fp) (\p -> final (castPtr p))


-- | Do something with the object pointer.
withObjectPtr :: Object a -> (Ptr a -> IO b) -> IO b
withObjectPtr obj f
  = case obj of
      Object p         -> f p
      Managed fp final -> withForeignPtr fp $ \pp ->
                          do p <- peek pp
                             f p

-- | Finalize a managed object manually. (no effect on unmanaged objects)
objectFinalize :: Object a -> IO ()
objectFinalize obj
  = case obj of
      Object p  -> return ()
      Managed fp final -> withForeignPtr fp $ \pp ->
                          do p <- peek pp
                             if (p == nullPtr)
                              then return ()
                              else do poke pp nullPtr    -- make NULL.
                                      final p

-- | Extract the object pointer and raise an exception if @NULL@.
withObjectRef :: Object a -> (Ptr a -> IO b) -> IO b
withObjectRef obj f
  = withObjectPtr obj $ \p ->
    if (p == nullPtr)
     then ioError (userError "wxHaskell: method call with NULL object")
     else f p


-- | Create an unmanaged object.
objectFromPtr :: Ptr a -> Object a
objectFromPtr p
  = Object p

-- | Create a managed object with a given finalizer.
managedObjectFromPtr :: (Ptr a -> IO ()) -> Ptr a -> IO (Object a)
managedObjectFromPtr final p
  = do pp <- malloc
       poke pp p
       fp <- newForeignPtr pp (finalizer pp)
       return (Managed fp final)
  where
    finalizer pp
      = do p <- peek pp
           if (p == nullPtr)
            then return ()
            else final p
           free pp


-- | Create an unmanaged object.
withObjectResult :: IO (Ptr a) -> IO (Object a)
withObjectResult io
  = do p <- io
       return (objectFromPtr p)

-- | Create a managed object with a given finalizer.
withManagedResultPtr :: (Ptr a -> IO ()) -> IO (Ptr a) -> IO (Object a)
withManagedResultPtr final io
  = do p <- io
       managedObjectFromPtr final p

