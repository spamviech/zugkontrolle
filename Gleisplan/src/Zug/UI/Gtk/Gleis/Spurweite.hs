{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Zug.UI.Gtk.Gleis.Spurweite (Spurweite(..), abstand, beschränkung, radiusBegrenzung) where

import Data.Proxy (Proxy())

import Zug.Enums (Zugtyp(..))

class Spurweite (z :: Zugtyp) where
    spurweite :: Proxy z -> Double

instance Spurweite 'Märklin where
    spurweite :: Proxy 'Märklin -> Double
    spurweite = const 16.5

instance Spurweite 'Lego where
    spurweite :: Proxy 'Lego -> Double
    spurweite = const 38

abstand :: (Spurweite z) => Proxy z -> Double
abstand proxy = spurweite proxy / 3

beschränkung :: (Spurweite z) => Proxy z -> Double
beschränkung proxy = spurweite proxy + 2 * abstand proxy

-- Märklin verwendet mittleren Kurvenradius
-- http://www.modellbau-wiki.de/wiki/Gleisradius
radiusBegrenzung :: (Spurweite z) => Double -> Proxy z -> Double
radiusBegrenzung radius proxy = radius + 0.5 * spurweite proxy + abstand proxy
