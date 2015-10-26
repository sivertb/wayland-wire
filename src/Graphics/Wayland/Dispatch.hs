{-|
Module      : Graphics.Wayland.Dispatch
Description : Dispatches incomming and outgoing messages to/from objects
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental
-}

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
    , SObject
    , CObject
    , Dispatchable (..)
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
import Data.Word
import Control.Monad.Error
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import qualified Graphics.Wayland.Protocol as P
import Text.Printf

data Server
data Client

-- | An object with phantom types side @c@ ('Server' or 'Client') and interface @i@.
newtype Object c i = Object { unObject :: ObjId } deriving (Show, Eq, Ord)

type SObject = Object Server
type CObject = Object Client

type SlotConstructor c i m = (Object c i -> m (Slots c i m)) -> m (Object c i)
type SignalConstructor c i m = Object c i -> m (Slots c i m)

-- | A class defining operations to send and received calls to a 'Object' with
-- side @c@ and interface @i@.
class Dispatchable c i where
    -- | The slots (incomming calls) for this interface.
    data Slots   c i :: (* -> *) -> *
    -- | The signals (outgoing calls) for this interface.
    data Signals c i :: (* -> *) -> *
    -- | Dispatches a message to the correct slot.
    dispatch  :: MonadDispatch c m => Slots c i m -> Message -> m ()
    -- | Returns the signals of an object.
    signals   :: MonadDispatch c m => Object c i -> Signals c i m
    -- | Returns the argument types for a specified slot.
    slotTypes :: Slots c i m -> OpCode -> Maybe [P.Type]

-- | A monad used to deliever messages to the correct object, as well as
-- handling allocation and tracking of objects.
class MonadIO m => MonadDispatch c m | m -> c where
    -- | Allocates a new object, but does not add any handlers to it.
    allocObject :: m NewId
    -- | Frees an allocated object, removing any handlers.
    freeObject :: ObjId -> m ()
    -- | Registers handlers for an object's slots.
    registerObject :: Dispatchable c i => Object c i -> Slots c i m -> m ()
    -- | Sends a message.
    sendMessage :: Message -> m ()
    -- | Dispatches a message to the correct object and slot.
    dispatchMessage :: Message -> m ()

-- | Makes it possible to lookup the name and version, as specified in the
-- protocol, of an interface.
class DispatchInterface i where
    interfaceName :: i -> String
    interfaceVersion :: i -> Word32
    interfaceInfo :: i -> P.Interface

-- | Creates an 'Object' from a 'NewId'.
objectFromNewId :: NewId -> Object c i
objectFromNewId = Object . ObjId . unNewId

consInterface :: SignalConstructor c i m -> i
consInterface _ = undefined

-- | Returns the name of the interface associated with a 'SignalConstructor'.
consName :: DispatchInterface i => SignalConstructor c i m -> String
consName = interfaceName . consInterface

-- | Returns the version of the interface associated with a 'SignalConstructor'.
consVer :: DispatchInterface i => SignalConstructor c i m -> Word32
consVer = interfaceVersion . consInterface

-- | Creates a new object, using the given constructor.
newObject :: (Dispatchable c i, MonadDispatch c m)
          => SignalConstructor c i m -- ^ The object constructor.
          -> m (NewId, Object c i)   -- ^ The new object and its Id
newObject f = do
    n <- allocObject
    let o = objectFromNewId n
    f o >>= registerObject o
    return (n, o)

-- | Creates a new object if a constructor is given.
maybeNewObject :: (Dispatchable c i, MonadDispatch c m)
          => Maybe (SignalConstructor c i m)
          -> m (Maybe NewId, Maybe (Object c i))   -- ^ The new object and its Id
maybeNewObject Nothing     = return (Nothing, Nothing)
maybeNewObject (Just cons) = liftM (Just *** Just) (newObject cons)

-- | Registers a new 'Object' using the given constructor.
regObject :: MonadDispatch c m
          => Maybe (String, Word32)                         -- ^ An interface name and version the constructor must match,
                                                            -- or 'Nothing' if this has already been enforced
                                                            -- by the type system.
          -> NewId                                          -- ^ The object's ID.
          -> forall i . (DispatchInterface i, Dispatchable c i)
          => SignalConstructor c i m                        -- ^ The constructor.
          -> m (Object c i)                                 -- ^ The new object.
regObject checkVers newId cons = do
    let obj = objectFromNewId newId
    case checkVers of
         Nothing                -> return ()
         Just (expName, expVer) -> do
             let iface :: Object c i -> i
                 iface = undefined
                 actName = interfaceName    $ iface obj
                 actVer  = interfaceVersion $ iface obj
             unless (actName == expName && actVer >= expVer)
                 . fail
                 $ printf "Interface (%s, %i) does not match expected interface (%s, %i)\n"
                 actName actVer expName expVer
    slots <- cons obj
    registerObject obj slots
    return obj
