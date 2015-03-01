{-|
Module      : Graphics.Wayland
Description : Haskell implementation of the Wayland wire protocol
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

Main module that pulls in all other required modules.
-}
module Graphics.Wayland
    (
      module Graphics.Wayland.Dispatch
    , module Graphics.Wayland.Protocol
    , module Graphics.Wayland.Types
    , module Graphics.Wayland.Wire
    , module Graphics.Wayland.W

    -- * Scanner
    , Side (..)
    , generateFromXml
    )
where

import Graphics.Wayland.Dispatch
import Graphics.Wayland.Protocol
import Graphics.Wayland.TH
import Graphics.Wayland.Types
import Graphics.Wayland.W
import Graphics.Wayland.Wire
