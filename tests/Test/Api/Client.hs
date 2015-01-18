{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Api.Client
where

import Graphics.Wayland.TH

$(generateFromXml Client "tests/test.xml")
