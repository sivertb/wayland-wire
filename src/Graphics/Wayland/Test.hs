{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Wayland.Test
where

import Graphics.Wayland.TH
import Language.Haskell.TH


$(generateFromXml Server "/usr/share/wayland/wayland.xml")
