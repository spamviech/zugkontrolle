{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}

module Zug.UI.Gtk.Gleis.Gerade (zeichneGerade, anchorPointsGerade, widthGerade, heightGerade) where

import Data.Int (Int32)
import Data.Proxy (Proxy())
import qualified GI.Cairo.Render as Cairo

import Zug.Enums (Zugtyp())
import Zug.UI.Gtk.Gleis.Anchor (AnchorPoint(..), AnchorPointMap, withAnchorName)
import Zug.UI.Gtk.Gleis.Spurweite (Spurweite(), beschränkung, abstand)

-- | Pfad zum Zeichnen einer Gerade angegebener Länge.
zeichneGerade :: (Spurweite z) => Double -> Proxy z -> Cairo.Render ()
zeichneGerade länge proxy = do
    -- Beschränkungen
    Cairo.moveTo 0 0
    Cairo.lineTo 0 $ beschränkung proxy
    Cairo.moveTo länge 0
    Cairo.lineTo länge $ beschränkung proxy
    -- Gleis
    Cairo.moveTo 0 gleisOben
    Cairo.lineTo länge gleisOben
    Cairo.moveTo 0 gleisUnten
    Cairo.lineTo länge gleisUnten
    where
        gleisOben :: Double
        gleisOben = abstand proxy

        gleisUnten :: Double
        gleisUnten = beschränkung proxy - abstand proxy

-- | Anchor Points einer Gerade angegebener Länge.
anchorPointsGerade :: (Spurweite z) => Double -> Proxy z -> AnchorPointMap
anchorPointsGerade länge proxy =
    withAnchorName
        "Gerade"
        [ AnchorPoint
          { anchorX = 0, anchorY = 0.5 * beschränkung proxy, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint
          { anchorX = länge, anchorY = 0.5 * beschränkung proxy, anchorVX = -1, anchorVY = 0 }]

-- | Breite einer Gerade mit angegebener Länge.
widthGerade :: forall (z :: Zugtyp). Double -> Proxy z -> Int32
widthGerade länge = const $ ceiling länge

-- | Höhe einer Gerade beliebiger Länge.
heightGerade :: (Spurweite z) => Proxy z -> Int32
heightGerade = ceiling . beschränkung
