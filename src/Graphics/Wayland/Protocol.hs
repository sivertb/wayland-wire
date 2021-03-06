{-|
Module      : Graphics.Wayland.Protocol
Description : Haskell representation and XML parser for the Wayland XML
              protocol format.
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

This module contains Haskell types used to represent a Wayland protocol
definition. It also contains XML picklers for HXT.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Graphics.Wayland.Protocol
    ( Protocol (..)
    , Interface (..)
    , Request (..)
    , Event (..)
    , Argument (..)
    , Type (..)
    , Enum' (..)
    , Entry (..)
    , Description (..)
    )
where

import Data.Word
import Text.XML.HXT.Core hiding (mkName)
import Language.Haskell.TH.Syntax (mkName)
import Language.Haskell.TH.Lift (deriveLiftMany)

#if !MIN_VERSION_template_haskell(2,10,0)
import Language.Haskell.TH.Lib (litE, integerL)
import Language.Haskell.TH.Lift (Lift (..))

instance Lift Word32 where
    lift = litE . integerL . toInteger
#endif

data Protocol =
    Protocol { protoName       :: String
             , protoCopyright  :: String
             , protoInterfaces :: [Interface]
             }
             deriving (Eq, Show)

data Description =
    Description { descSummary :: String
                , description :: String
                }
                deriving (Eq, Show)

instance XmlPickler Protocol where
    xpickle =
        xpElem "protocol" $
        xpWrap ( uncurry3 Protocol
               , \(Protocol a b c) -> (a, b, c)
               ) $
        xpTriple (xpAttr "name" xpText)
                 (xpElem "copyright" xpText)
                 (xpList1 xpickle)

data Interface =
    Interface { ifaceDescription :: Maybe Description
              , ifaceName        :: String
              , ifaceVersion     :: Word32
              , ifaceRequests    :: [Request]
              , ifaceEvents      :: [Event]
              , ifaceEnums       :: [Enum']
              }
              deriving (Eq, Show)

data IfaceHelper =
    IfaceRequest Request
  | IfaceEvent   Event
  | IfaceEnum    Enum'

instance XmlPickler IfaceHelper where
    xpickle =
        xpAlt toIndex
              [ xpWrap ( IfaceRequest, \(IfaceRequest a) -> a) xpickle
              , xpWrap ( IfaceEvent  , \(IfaceEvent a)   -> a) xpickle
              , xpWrap ( IfaceEnum   , \(IfaceEnum a)    -> a) xpickle
              ]
        where
            toIndex (IfaceRequest _) = 0
            toIndex (IfaceEvent   _) = 1
            toIndex (IfaceEnum    _) = 2

fromHelper :: [IfaceHelper] -> ([Request], [Event], [Enum'])
fromHelper = helper ([], [], [])
    where
        helper (as, bs, cs) []     = (reverse as, reverse bs, reverse cs)
        helper (as, bs, cs) (d:ds) =
            case d of
                 IfaceRequest a -> helper (a:as, bs, cs) ds
                 IfaceEvent   b -> helper (as, b:bs, cs) ds
                 IfaceEnum    c -> helper (as, bs, c:cs) ds

toHelper :: [Request] -> [Event] -> [Enum'] -> [IfaceHelper]
toHelper as bs cs =
    map IfaceRequest as ++
    map IfaceEvent   bs ++
    map IfaceEnum    cs

instance XmlPickler Interface where
    xpickle =
        xpElem "interface" $
        xpWrap ( \(a, b, c, d) -> uncurry3 (Interface a b c) $ fromHelper d
               , \(Interface a b c d e f) -> let g = toHelper d e f in (a, b, c, g)
               ) $
        xp4Tuple xpDesc
                 (xpAttr "name" xpText)
                 (xpAttr "version" xpPrim)
                 (xpList1 xpickle)

data Request =
    Request { reqName        :: String
            , reqDescription :: Maybe Description
            , reqSince       :: Maybe Int
            , reqArgs        :: [Argument]
            , reqType        :: Maybe String
            }
            deriving (Eq, Show)

instance XmlPickler Request where
    xpickle =
        xpElem "request" $
        xpWrap ( \(a, b, c, d, e) -> Request a b c d e
               , \(Request a b c d e) -> (a, b, c, d, e)
               ) $
        xp5Tuple (xpAttr "name" xpText)
                 xpDesc
                 xpSince
                 (xpList xpickle)
                 (xpOption $ xpAttr "type" xpText)

data Event =
    Event { eventName        :: String
          , eventDescription :: Maybe Description
          , eventSince       :: Maybe Int
          , eventArgs        :: [Argument]
          }
          deriving (Eq, Show)

instance XmlPickler Event where
    xpickle =
        xpElem "event" $
        xpWrap ( uncurry4 Event
               , \(Event a b c d) -> (a, b, c, d)
               ) $
        xp4Tuple (xpAttr "name" xpText)
                 xpDesc
                 xpSince
                 (xpList xpickle)

data Argument =
    Argument { argName        :: String
             , argDescription :: Maybe Description
             , argType        :: Type
             , argSummary     :: Maybe String
             }
             deriving (Eq, Show)

instance XmlPickler Argument where
    xpickle =
        xpElem "arg" $
        xpWrap ( uncurry4 Argument
               , \(Argument a b c d) -> (a, b, c, d)
               ) $
        xp4Tuple (xpAttr "name" xpText)
                 xpDesc
                 xpType
                 xpSummary

data Type =
    TypeSigned
  | TypeUnsigned
  | TypeFixed
  | TypeFd
  | TypeArray
  | TypeString Bool
  | TypeObject Bool (Maybe String)
  | TypeNew    Bool (Maybe String)
  | TypeEnum   String
    deriving (Eq, Show)

xpType :: PU Type
xpType =
    xpWrapMaybe ( toType
                , fromType
                ) $
    xp4Tuple (xpAttr "type" xpText)
             (xpOption $ xpAttr "interface" xpText)
             (xpOption $ xpAttr "enum" xpText)
             (xpDefault False $ xpAttr "allow-null" xpBool)
    where

        toType (typeName, interface, enum, allowNull) =
            case typeName of
                 "int"      -> Just $ maybe TypeSigned TypeEnum enum
                 "uint"     -> Just $ maybe TypeUnsigned TypeEnum enum
                 "fixed"    -> Just TypeFixed
                 "fd"       -> Just TypeFd
                 "array"    -> Just TypeArray
                 "string"   -> Just $ TypeString allowNull
                 "object"   -> Just $ TypeObject allowNull interface
                 "new_id"   -> Just $ TypeNew allowNull interface
                 _          -> Nothing

        fromType (TypeSigned         ) = ("int", Nothing, Nothing, False)
        fromType (TypeUnsigned       ) = ("uint", Nothing, Nothing, False)
        fromType (TypeFixed          ) = ("fixed", Nothing, Nothing, False)
        fromType (TypeFd             ) = ("fd", Nothing, Nothing, False)
        fromType (TypeArray          ) = ("array", Nothing, Nothing, False)
        fromType (TypeString an      ) = ("array", Nothing, Nothing, an)
        fromType (TypeObject an iface) = ("object", iface, Nothing, an)
        fromType (TypeNew    an iface) = ("new_id", iface, Nothing, an)
        fromType (TypeEnum   enum    ) = ("uint", Nothing, Just enum, False)

data Enum' =
    Enum' { enumName        :: String
          , enumSince       :: Maybe Int
          , enumDescription :: Maybe Description
          , enumValues      :: [Entry]
          , enumBitfield    :: Bool
          }
          deriving (Eq, Show)

instance XmlPickler Enum' where
    xpickle =
        xpElem "enum" $
        xpWrap ( \(a, b, c, d, e) -> Enum' a b c d e
               , \(Enum' a b c d e) -> (a, b, c, d, e)
               ) $
        xp5Tuple (xpAttr "name" xpText)
                 xpSince
                 xpDesc
                 (xpList xpickle)
                 (xpDefault False $ xpAttr "bitfield" xpBool)

data Entry =
    Entry { entryName        :: String
          , entryValue       :: Word32
          , entrySince       :: Maybe Int
          , entrySummary     :: Maybe String
          , entryDescription :: Maybe Description
          }
          deriving (Eq, Show)

instance XmlPickler Entry where
    xpickle =
        xpElem "entry" $
        xpWrap ( \(n, v, s, su, d) -> Entry n v s su d
               , \(Entry n v s su d) -> (n, v, s, su, d)
               ) $
        xp5Tuple (xpAttr "name" xpText)
                 (xpAttr "value" xpPrim)
                 xpSince
                 xpSummary
                 xpDesc

xpBool :: PU Bool
xpBool = xpWrap (toBool, fromBool) xpText
    where
        toBool     = (== "true")
        fromBool b = if b then "true" else "false"

xpDesc :: PU (Maybe Description)
xpDesc =
    xpOption $
    xpElem "description" $
    xpWrap ( uncurry Description
           , \(Description a b) -> (a, b)
           ) $
    xpPair (xpAttr "summary" xpText)
           xpText0

xpSummary :: PU (Maybe String)
xpSummary = xpOption $ xpAttr "summary" xpText

xpSince :: PU (Maybe Int)
xpSince = xpOption $ xpAttr "since" xpPrim


$( deriveLiftMany
   $ map mkName
   [ "Protocol"
   , "Interface"
   , "Request"
   , "Event"
   , "Argument"
   , "Type"
   , "Enum'"
   , "Entry"
   , "Description"
   ])
