{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Api.Server
where

import Graphics.Wayland.TH

$(generateFromXml Server "tests/test.xml")
