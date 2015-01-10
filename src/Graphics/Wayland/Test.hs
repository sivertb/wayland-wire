{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Wayland.Test
where

import Graphics.Wayland.TH

$(generateFromXml Client "/usr/share/wayland/wayland.xml")
