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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Wayland.W
    ( W
    , WError (..)
    , WC
    , WS
    , runW
    , recvAndDispatch
    , AllocLimits ()
    )
where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
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

-- | The error type of the 'W' monad.
data WError =
    WErrIO    IOError
  | WErrProto String
  | WErrUser  String
  deriving (Eq, Show)

-- | Holds the 'W' monad's state.
data WState m = WState { regObjs  :: M.Map ObjId (OpCode -> Maybe [Type], Message -> m ())
                       , freeObjs :: D.Diet ObjId
                       }

-- | A wayland transformer monad parametrized over:
--
-- * @c@ - Where this monad runs. Either 'Server' or 'Client'.
--
-- * @m@ - The inner monad.
newtype W c m a = W { runW' :: ExceptT WError (ReaderT Socket (StateT (WState (W c m)) m)) a }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadError WError
             , MonadReader Socket
             , MonadState (WState (W c m))
             )

type WC = W Client
type WS = W Server

-- | Returns the inital state to use when running the 'W' monad.
initialState :: D.Interval ObjId -> WState m
initialState i = WState { regObjs  = M.empty
                        , freeObjs = D.singletonI i
                        }

-- | Returns the allocation limits of a 'W' value.
wLimits :: AllocLimits c => W c m a -> D.Interval ObjId
wLimits = allocLimits . c
    where
        c :: W c m a -> c
        c = undefined

-- | Returns the allocation limits in a 'W' monad.
wmLimits :: (AllocLimits c, Monad m) => W c m (D.Interval ObjId)
wmLimits = m >> return (wLimits m)
    where
        m = return ()

-- | Runs a 'W' computation.
runW :: (AllocLimits c, Monad m) => Socket -> W c m a -> m (Either WError a)
runW s w = evalStateT (runReaderT (runExceptT (runW' w)) s) $ initialState (wLimits w)

-- | Receives and dispatches a message from the 'W' monad's socket.
recvAndDispatch :: (AllocLimits c, Functor m, MonadIO m) => W c m ()
recvAndDispatch = ask >>= recv >>= dispatchMessage

instance (AllocLimits c, Functor m, MonadIO m) => MonadDispatch c (W c m) where
    allocObject = do
        mv <- gets (D.minView . freeObjs)
        case mv of
             Nothing           -> throwError $ WErrUser "No more free IDs!"
             Just (a, newObjs) -> newFromObj a <$ modify (\s -> s { freeObjs = newObjs })

    freeObject objId = do
        validId     <- D.overlapping (D.point objId) <$> wmLimits
        alreadyFree <- D.member objId <$> gets freeObjs
        unless validId     . throwError $ WErrUser "Trying to free an ID outside the valid ID range"
        when   alreadyFree . throwError $ WErrUser "Trying to free an ID that's already free"
        modify (\s -> s { freeObjs = D.insert objId (freeObjs s) } )

    registerObject obj slots = do
        exists <- gets (M.member (unObject obj) . regObjs)
        when exists . throwError . WErrUser $ "Trying to register " ++ show obj ++ " which already exists"
        modify (\s -> s { regObjs = M.insert (unObject obj) (slotTypes slots, dispatch slots) (regObjs s) })

    sendMessage msg = do
        sock <- ask
        send sock msg

    dispatchMessage msg = do
        handler <- gets (fmap snd . M.lookup (msgObj msg) . regObjs)
        case handler of
             Nothing -> throwError . WErrProto $ "Could not find a dispatcher for object " ++ show (msgObj msg)
             Just h  -> h msg

instance (Functor m, MonadIO m) => SocketError (W c m) where
    sockErr = throwError . WErrIO

instance (Functor m, MonadIO m) => SocketLookup (W c m) where
    msgLookup = do
        m <- gets regObjs
        return $ \obj op -> M.lookup obj m >>= (\f -> f op) . fst
