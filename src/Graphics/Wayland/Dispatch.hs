{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Wayland.Dispatch
    ( Server
    , Client
    , SlotConstructor
    , SignalConstructor
    , Object (..)
    , Dispatch (..)
    , MonadDispatch (..)
    , DispatchInterface (..)
    , consName
    , consVer
    , objectFromNewId
    , newObject
    , maybeNewObject
    , regObject
    )
where

import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import Text.Printf

data Server
data Client

newtype Object c i = Object { unObject :: ObjId }

type SlotConstructor c i m = (Object c i -> m (Slots c i m)) -> m (Object c i)
type SignalConstructor c i m = Object c i -> m (Slots c i m)

class Dispatch i c where
    data Slots   c i :: (* -> *) -> *
    data Signals c i :: (* -> *) -> *
    dispatch :: MonadDispatch c m => Slots c i m -> Message -> m ()
    signals  :: MonadDispatch c m => Object c i -> Signals c i m

-- | A monad that supports handling objects.
class MonadIO m => MonadDispatch c m | m -> c where
    -- | Allocates a new object, but does not add any handlers to it.
    allocObject :: m NewId
    -- | Frees an allocated object, removing any handlers.
    freeObject :: Object c i -> m ()
    registerObject :: Object c i -> Slots c i m -> m ()
    sendMessage :: Message -> m ()
    dispatchMessage :: Message -> m ()

-- | Makes it possible to lookup the name and version, as specified in the
-- protocol, of an interface.
class DispatchInterface i where
    interfaceName :: i -> String
    interfaceVersion :: i -> Int

-- | Creates an 'Object' from a 'NewId'.
objectFromNewId :: NewId -> Object c i
objectFromNewId = Object . ObjId . unNewId

consInterface :: SignalConstructor c i m -> i
consInterface _ = undefined

consName :: DispatchInterface i => SignalConstructor c i m -> String
consName = interfaceName . consInterface

consVer :: DispatchInterface i => SignalConstructor c i m -> Int
consVer = interfaceVersion . consInterface

-- | Creates a new object, using the given constructor.
newObject :: MonadDispatch c m
          => (Object c i -> m (Slots c i m))    -- ^ The object constructor.
          -> m (NewId, Object c i)              -- ^ The new object and its Id
newObject f = do
    n <- allocObject
    let o = objectFromNewId n
    f o >>= registerObject o
    return (n, o)

-- | Creates a new object if a constructor is given.
maybeNewObject :: MonadDispatch c m
          => Maybe (Object c i -> m (Slots c i m)) -- ^ The object constructor.
          -> m (Maybe NewId, Maybe (Object c i))   -- ^ The new object and its Id
maybeNewObject Nothing     = return (Nothing, Nothing)
maybeNewObject (Just cons) = liftM (Just *** Just) (newObject cons)

regObject :: (MonadDispatch c m)
          => Maybe (String, Int)
          -> NewId
          -> forall i . DispatchInterface i
          => (Object c i -> m (Slots c i m))    -- ^ The object constructor.
          -> m (Object c i)                     -- ^ The new object.
regObject i n f = do
    let o = objectFromNewId n
    case i of
         Nothing                -> return ()
         Just (expName, expVer) -> do
             let actName = interfaceName    $ iface o
                 actVer  = interfaceVersion $ iface o
             unless (actName == expName && actVer >= expVer)
                 . fail
                 $ printf "Interface (%s, %i) does not match expected interface (%s, %i)\n"
                 actName actVer expName expVer
    f o >>= registerObject o >> return o
    where
        iface :: Object c i -> i
        iface = undefined
