{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Wayland.Dispatch
    ( Side (..)
    , RecordType (..)
    , Object (..)
    , Slots
    , Signals
    , Dispatch (..)
    , MonadDispatch (..)
    , DispatchInterface (..)
    , objectFromNewId
    , newObject
    , checkCons
    )
where

import Control.Monad
import Control.Monad.Error
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import Text.Printf

data Side = Server | Client deriving (Eq, Show)
data RecordType = Slot | Signal deriving (Eq, Show)

newtype Object (c :: Side) i = Object { unObject :: ObjId }

type family Constructor (r :: RecordType) c i m where
    Constructor Slot   c i m = (Object c i -> m (Slots c i m)) -> m (Object c i)
    Constructor Signal c i m = Object c i -> m (Slots c i m)

type family RetType (r :: RecordType) a where
    RetType Signal a = a
    RetType Slot   a = ()

data family Requests (r :: RecordType) (c :: Side) i :: (* -> *) -> *
data family Events   (r :: RecordType) (c :: Side) i :: (* -> *) -> *

type family Slots (c :: Side) where
    Slots Server = Requests Slot Server
    Slots Client = Events   Slot Client

type family Signals (c :: Side) where
    Signals Client = Requests Signal Client
    Signals Server = Events   Signal Server

class Dispatch i c where
    dispatch :: MonadDispatch c m => Slots c i m -> Message -> m ()
    signals :: MonadDispatch c m => Object c i -> Signals c i m

class MonadIO m => MonadDispatch c m | m -> c where
    allocObject :: m (Object c i)
    freeObject :: Object c i -> m ()
    registerObject :: Object c i -> Slots c i m -> m ()
    sendMessage :: Message -> m ()
    dispatchMessage :: Message -> m ()

class DispatchInterface i where
    interfaceName :: i -> String
    interfaceVersion :: i -> Int

objectFromNewId :: NewId -> Object c i
objectFromNewId = Object . ObjId . unNewId

newObject :: MonadDispatch c m => (Object c i -> m (Slots c i m)) -> m (Object c i)
newObject f = do
    o <- allocObject
    f o >>= registerObject o
    return o

-- | Checks that the interface of a constructor matches the expected interface.
checkCons :: forall e r c i m
          .  (Error e, DispatchInterface i, MonadError e m)
          => String                 -- ^ The expected interface name.
          -> Int                    -- ^ The expected interface version.
          -> Constructor r c i m    -- ^ The interface constructor.
          -> m ()
checkCons name ver cons =
    unless (interfaceName iface == name && interfaceVersion iface >= ver)
        . throwError . strMsg
        $ printf "Interface (%s, %i) does not match expected interface (%s, %i)\n"
            (interfaceName iface) (interfaceVersion iface) name ver
    where
        getIface :: Constructor r c i m -> i
        getIface = undefined
        iface = getIface cons
