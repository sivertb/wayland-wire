{-|
Module      : Graphics.Wayland.Wire
Description : Abstraction over Unix sockets.
Copyright   : (C) Sivert Berg, 2014-2015
License     : MIT
Maintainer  : code@trev.is
Stability   : Experimental

This module handles the socket side, allowing a 'Message' to be sent or
received on Unix sockets.
-}

module Graphics.Wayland.Wire
    ( -- * Message
      Message (..)
    , MsgArg (..)
    -- * Socket
    , Socket
    , SocketError (..)
    , SocketLookup (..)
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
    )
where

import Graphics.Wayland.Wire.Encode
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Socket
