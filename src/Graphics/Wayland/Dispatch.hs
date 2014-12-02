{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graphics.Wayland.Dispatch
    ( Client
    , Server
    , Object (..)
    , Slots
    , Signals
    , Dispatch (..)
    , MonadDispatch (..)
    , DispatchInterface (..)
    , objectFromNewId
    , newObject
    , checkNew
    )
where

import Control.Monad
import Control.Monad.IO.Class
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import Text.Printf

data Client
data Server

newtype Object c i = Object { unObject :: ObjId }

data family Slots c i :: (* -> *)-> *
data family Signals c i :: (* -> *) -> *

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

checkNew :: (DispatchInterface i, Monad m)
         => String
         -> Int
         -> ((Object c i -> m (Slots c i m)) -> m (Object c i))
         -> m ()
checkNew name ver cons =
    unless (interfaceName iface == name && interfaceVersion iface >= ver) $
        fail $ printf "Interface (%s, %i) does not match expected interface (%s, %i)\n"
            (interfaceName iface) (interfaceVersion iface) name ver
    where
        getIface :: ((Object c i -> m (Slots c i m)) -> m (Object c i)) -> i
        getIface = undefined
        iface = getIface cons
