{-|
Module      : Graphics.Wayland.W
Description : A transformer monad implementing the 'MonadDispatch' class.
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

A transformer monad that can be used to add support for calling and receiving
calls to Wayland objects.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Graphics.Wayland.W
    ( W
    , ObjectError (..)
    , WC
    , WS
    , ObjectManager
    , newObjectManager
    , messageLookup
    , runW
    , AllocLimits ()
    )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set.Diet as D
import qualified Data.Map as M
import Graphics.Wayland.Dispatch
import Graphics.Wayland.Protocol
import Graphics.Wayland.Types
import Graphics.Wayland.Wire
import Prelude

-- | A class used for finding the minimum and maximum allowed object ID.
class AllocLimits c where
    allocLimits :: c -> D.Interval ObjId

instance AllocLimits Server where
    allocLimits _ = D.Interval 0xff000000 0xffffffff

instance AllocLimits Client where
    allocLimits _ = D.Interval 0x00000001 0xfeffffff

-- | The different errors.
data ObjectError =
    -- | A message sent to the given object was invalid.
    ErrMethod ObjId String
    -- | A message was sent to a non-existing object.
  | ErrObject ObjId
    -- | Any other error.
  | ErrUser   String
  deriving (Eq, Show)

-- | Keeps track of objects and their handlers.
data ObjectManager c m =
    ObjectManager { regObjs  :: M.Map ObjId (OpCode -> Maybe [Type], Message -> W c m ())
                  , freeObjs :: D.Diet ObjId
                  }

-- | Returns a new 'ObjectManager'.
newObjectManager :: AllocLimits c => ObjectManager c m
newObjectManager =
    let mgr = ObjectManager { regObjs  = M.empty
                            , freeObjs = D.singletonI (interval mgr)
                            }
    in mgr
    where
        interval :: AllocLimits c => ObjectManager c m -> D.Interval ObjId
        interval mgr = allocLimits (c mgr)

        c :: ObjectManager c m -> c
        c = undefined

-- | Returns a function that can look up the correct handler given an object
-- and an opcode.
messageLookup :: ObjectManager c m -> MessageLookup
messageLookup objMgr obj op = M.lookup obj (regObjs objMgr) >>= ($ op) . fst

type WC = W Client
type WS = W Server

newtype W c m a = W { runW' :: ExceptT ObjectError (StateT (ObjectManager c m) m) a }
    deriving
    ( MonadState (ObjectManager c m)
    , MonadError ObjectError
    , Monad
    , Functor
    , Applicative
    )

runW :: W c m a -> ObjectManager c m -> m (Either ObjectError a, ObjectManager c m)
runW w mgr = runStateT (runExceptT (runW' w)) mgr

instance MonadTrans (W c) where
    lift = W . lift . lift

instance (Monad m, MonadSend m) => MonadSend (W c m) where
    sendMessage = lift . sendMessage

instance (Functor m, Monad m, MonadSend m) => MonadObject c (W c m) where
    allocObject = do
        mv <- gets (D.minView . freeObjs)
        case mv of
             Nothing           -> throwError $ ErrUser "No more free IDs!"
             Just (a, newObjs) -> newFromObj a <$ modify (\s -> s { freeObjs = newObjs })

    freeObject (Object objId) = do
        alreadyFree <- D.member objId <$> gets freeObjs
        when alreadyFree . throwError $ ErrUser "Trying to free an ID that's already free"
        modify (\s -> s { freeObjs = D.insert objId (freeObjs s) } )

    registerObject obj slots = do
        exists <- gets (M.member (unObject obj) . regObjs)
        when exists . throwError . ErrUser $ "Trying to register " ++ show obj ++ " which already exists"
        modify (\s -> s { regObjs = M.insert (unObject obj) (slotTypes slots, dispatch slots) (regObjs s) })

    unregisterObject obj = do
        exists <- gets (M.member (unObject obj) . regObjs)
        unless exists . throwError . ErrUser $ "Trying to unregister " ++ show obj ++ " that does not exist"
        modify (\s -> s { regObjs = M.delete (unObject obj) (regObjs s) })

    dispatchMessage msg = do
        handler <- gets (fmap snd . M.lookup (msgObj msg) . regObjs)
        case handler of
             Nothing -> throwError . ErrObject $ msgObj msg
             Just h  -> h msg

    protocolError obj err = throwError $ ErrMethod obj err
