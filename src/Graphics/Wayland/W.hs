{-|
Module      : Graphics.Wayland.W
Description : A transformer monad implementing 'MonadSend' and 'MonadObject'.
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

A simple transformer monad that can be used to add support for calling and
receiving calls to Wayland objects.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Wayland.W
    ( WC
    , WS
    , W
    , runW
    , ObjectError (..)
    )
where

import Control.Monad.Except
import Control.Monad.State
import Graphics.Wayland.Dispatch
import Graphics.Wayland.ObjectManager
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message

type WC = W Client
type WS = W Server

-- | The different errors.
data ObjectError =
    -- | A message sent to the given object was invalid.
    ErrMethod ObjId String
    -- | A message was sent to a non-existing object.
  | ErrObject ObjId
    -- | Any other error.
  | ErrUser   String
  deriving (Eq, Show)

newtype W c m a = W { runW' :: ExceptT ObjectError (StateT (ObjectManager c (W c m)) m) a }
    deriving
    ( MonadState (ObjectManager c (W c m))
    , MonadError ObjectError
    , Monad
    , MonadIO
    , Functor
    , Applicative
    )

runW :: W c m a -> ObjectManager c (W c m) -> m (Either ObjectError a, ObjectManager c (W c m))
runW w = runStateT (runExceptT (runW' w))

instance MonadTrans (W c) where
    lift = W . lift . lift

instance (Monad m, MonadSend m) => MonadSend (W c m) where
    sendMessage = lift . sendMessage

instance (Functor m, Monad m, MonadSend m) => MonadObject c (W c m) where
    allocObject = do
        mv <- gets alloc
        case mv of
             Nothing       -> throwError $ ErrUser "No more free IDs!"
             Just (a, mgr) -> a <$ put mgr

    registerObject obj slots = modify (insert obj slots)

    dispatchMessage msg = do
        handler <- gets (lookupHandler $ msgObj msg)
        case handler of
             Nothing -> throwError . ErrObject $ msgObj msg
             Just h  -> h msg

    protocolError obj err = throwError $ ErrMethod obj err
