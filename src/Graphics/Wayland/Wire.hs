{-|
Module      : Graphics.Wayland.Wire
Description : Abstraction over Unix sockets.
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

This module handles the socket side, allowing a 'Message' to be sent or
received on Unix sockets.
-}

module Graphics.Wayland.Wire
    ( -- * Message
      Message (..)
    , MsgArg (..)
    -- * Fixed
    , Fixed
    , fixedToDouble
    , doubleToFixed
    -- * Socket
    , Socket
    , MessageLookup
    , listen
    , connect
    , accept
    , close
    , send
    , recv
    -- * Encode / decode message
    , toMessage
    , fromMessage
    , Encodable
    , Decodable
    , ArgType
    -- * Debug
    , MessageType (..)
    , ppMsg
    )
where

import Graphics.Wayland.Wire.Debug
import Graphics.Wayland.Wire.Encode
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Socket
