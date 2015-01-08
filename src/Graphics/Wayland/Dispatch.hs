{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Wayland.Dispatch
    ( Side (..)
    , RecordType (..)
    , Requests
    , Events
    , Constructor
    , P
    , UnCons
    , RetType
    , Object (..)
    , Slots
    , Signals
    , Dispatch (..)
    , MonadDispatch (..)
    , DispatchInterface (..)
    , objectFromNewId
    , newObject
    , regObject
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

-- | The type of an object constructor.
type family Constructor (r :: RecordType) c i m where
    Constructor Slot   c i m = (Object c i -> m (Slots c i m)) -> m (Object c i)
    Constructor Signal c i m = Object c i -> m (Slots c i m)

-- | A helper type family to prove that 'Constructor' is injective.
type family UnCons cons where
    UnCons ((Object c i -> m x) -> m (Object c i)) = P Slot   c i m
    UnCons ( Object c i -> m x                   ) = P Signal c i m

-- | Defines an empty data type used as the proof for 'UnCons'.
data P (r :: RecordType) (c :: Side) i (m :: * -> *)

-- | Changes a return type to '()' for Slots.
type family RetType (r :: RecordType) a where
    RetType Signal a = a
    RetType Slot   a = ()

-- | The requests of an interface.
data family Requests (r :: RecordType) (c :: Side) i :: (* -> *) -> *
 
-- | The events of an interface.
data family Events   (r :: RecordType) (c :: Side) i :: (* -> *) -> *

-- | Slots are functions that can be called on the object, and the user has to
-- add handlers for them. On the server side the 'Slots' map to the 'Requests',
-- that is called by client code, while on the client side it is the 'Signals',
-- called by server code.
type family Slots (c :: Side) where
    Slots Server = Requests Slot Server
    Slots Client = Events   Slot Client

-- | The signals are the outbound interface of the objects. On the server side
-- these map to the 'Events', and on the client side to the 'Requests'. A user
-- can use the 'signals' function to get 'Signals' she can call.
type family Signals (c :: Side) where
    Signals Client = Requests Signal Client
    Signals Server = Events   Signal Server

class Dispatch i c where
    dispatch :: MonadDispatch c m => Slots c i m -> Message -> m ()
    signals  :: MonadDispatch c m => Object c i -> Signals c i m

-- | A monad that supports handling objects.
class MonadIO m => MonadDispatch c m | m -> c where
    -- | Allocates a new object, but does not add any handlers to it.
    allocObject :: m (Object c i)
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

-- | Creates a new object, using the given constructor.
newObject :: MonadDispatch c m
          => (Object c i -> m (Slots c i m))    -- ^ The object constructor.
          -> m (Object c i)                     -- ^ The new object.
newObject f = do
    o <- allocObject
    f o >>= registerObject o
    return o

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

-- | Checks that the interface of a constructor matches the expected interface.
checkCons :: ( Error e
             , DispatchInterface i
             , MonadError e m
             , UnCons (Constructor r c i m) ~ P r c i m
             )
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
        getIface :: UnCons (Constructor r c i m) ~ P r c i m
                 => Constructor r c i m
                 -> i
        getIface = undefined
        iface = getIface cons
