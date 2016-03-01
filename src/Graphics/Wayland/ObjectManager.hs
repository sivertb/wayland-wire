{-|
Module      : Graphics.Wayland.ObjectManager
Description : A data structure that keeps track of objects,
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Wayland.ObjectManager
    ( ObjectManager
    , AllocLimits ()
    , newObjectManager
    , messageLookup
    , lookupInterface
    , lookupHandler
    , getObjects
    , alloc
    , free
    , insert
    , delete
    , member
    )
where

import Control.Applicative
import Control.Arrow
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

-- | Keeps track of objects and their handlers.
data ObjectManager c m =
    ObjectManager { regObjs  :: M.Map ObjId (OpCode -> Maybe [Type], Message -> m (), Interface)
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
messageLookup objMgr obj op = M.lookup obj (regObjs objMgr) >>= ($ op) . (\(a, _, _) -> a)

-- | Finds the 'Interface' of an 'ObjId', or 'Nothing' if the object does not exist.
lookupInterface :: ObjId -> ObjectManager c m -> Maybe Interface
lookupInterface obj = fmap (\(_, _, i) -> i) . M.lookup obj . regObjs

lookupHandler :: ObjId -> ObjectManager c m -> Maybe (Message -> m ())
lookupHandler obj = fmap (\(_, h, _) -> h) . M.lookup obj . regObjs

-- | Returns a list of all the objects and their 'Interface'.
getObjects :: ObjectManager c m -> [(ObjId, Interface)]
getObjects = map (second (\(_, _, i) -> i)) . M.toList . regObjs

alloc :: ObjectManager c m -> Maybe (NewId, ObjectManager c m)
alloc mgr = (newFromObj *** (\f -> mgr { freeObjs = f })) <$> D.minView (freeObjs mgr)

free :: Object c i -> ObjectManager c m -> ObjectManager c m
free (Object objId) mgr = mgr { freeObjs = D.insert objId (freeObjs mgr) }

insert :: (DispatchInterface i, Dispatchable c i, MonadSend m, MonadObject c m)
       => Object c i
       -> Slots c i m
       -> ObjectManager c m
       -> ObjectManager c m
insert obj slots mgr =
    mgr { regObjs = M.insert (unObject obj) (slotTypes slots, dispatch slots, interfaceInfo $ objInterface obj) (regObjs mgr) }
    where
        objInterface :: Object c i -> i
        objInterface = undefined

delete :: Object c i
       -> ObjectManager c m
       -> ObjectManager c m
delete (Object objId) mgr = mgr { regObjs = M.delete objId (regObjs mgr) }

member :: Object c i -> ObjectManager c m -> Bool
member (Object objId) = M.member objId . regObjs
