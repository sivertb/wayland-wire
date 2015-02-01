module Graphics.Wayland.Wire
    ( -- * Message
      Message (..)
    -- * Socket
    , Socket
    , SocketClass (..)
    , listen
    , connect
    , accept
    , close
    , send
    , recv
    -- * Encode / decode message
    , toMessage
    , fromMessage
    )
where

import Graphics.Wayland.Wire.Encode
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Socket
