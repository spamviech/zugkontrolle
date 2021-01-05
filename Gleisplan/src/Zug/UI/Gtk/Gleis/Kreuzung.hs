{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Zug.UI.Gtk.Gleis.Kreuzung
  ( zeichneKreuzung
  , anchorPointsKreuzung
  , widthKreuzung
  , heightKreuzung
  , KreuzungsArt(..)
  , KurveFehler(..)
  , KurveErgebnis(..)
  ) where

import Control.Monad (when)
import Data.Binary (Binary())
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.Proxy (Proxy())
import GHC.Generics (Generic())
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Matrix (Matrix(Matrix))

import Zug.UI.Gtk.Gleis.Anchor (AnchorPoint(..), AnchorPointMap, withAnchorName)
import Zug.UI.Gtk.Gleis.Gerade (zeichneGerade)
import Zug.UI.Gtk.Gleis.Kurve
       (KurvenBeschränkung(KeineBeschränkungen), KurveFehler(..), KurveErgebnis(..)
      , beideErgebnisse, widthKurve, heightKurve, zeichneKurve)
import Zug.UI.Gtk.Gleis.Spurweite (Spurweite(), beschränkung)

infixl 6 -#

{-# INLINE (-#) #-}

-- | Identisch zu '-'. Definiert, damit Section-Syntax funktioniert (keine unary negation).
(-#) :: (Num a) => a -> a -> a
(-#) = (-)

widthKreuzung :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis Int32
widthKreuzung länge radius winkelBogenmaß proxy =
    max (ceiling länge) <$> widthKurve radius winkelBogenmaß proxy

heightKreuzung :: (Spurweite z) => Double -> Double -> Proxy z -> KurveErgebnis Int32
heightKreuzung radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) . (-# ceiling (beschränkung proxy)) . (2 *)
    <$> heightKurve radius winkelBogenmaß proxy

data KreuzungsArt
    = MitKurve
    | OhneKurve
    deriving (Eq, Show, Generic)

instance Binary KreuzungsArt

zeichneKreuzung :: (Spurweite z)
                => Double
                -> Double
                -> Double
                -> KreuzungsArt
                -> Proxy z
                -> KurveErgebnis (Cairo.Render ())
zeichneKreuzung länge radius winkelBogenmaß kreuzungsArt proxy =
    beideErgebnisse
        (widthKreuzung länge radius winkelBogenmaß proxy)
        (heightKurve radius winkelBogenmaß proxy)
    <&> \(width, height) -> do
        let startHeight :: Double
            startHeight = max 0 $ fromIntegral height - beschränkung proxy
            halfWidth :: Double
            halfWidth = 0.5 * fromIntegral width
            halfHeight :: Double
            halfHeight = 0.5 * fromIntegral height
        Cairo.translate 0 startHeight
        zeichneGerade länge proxy
        when (kreuzungsArt == MitKurve)
            $ zeichneKurve radius winkelBogenmaß KeineBeschränkungen proxy
        Cairo.translate 0 $ -startHeight
        Cairo.stroke
        Cairo.translate halfWidth halfHeight
        Cairo.rotate winkelBogenmaß
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth) (-halfHeight)
        Cairo.translate 0 startHeight
        zeichneGerade länge proxy
        when (kreuzungsArt == MitKurve)
            $ zeichneKurve radius winkelBogenmaß KeineBeschränkungen proxy

anchorPointsKreuzung
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis AnchorPointMap
anchorPointsKreuzung länge radius winkelBogenmaß proxy =
    beideErgebnisse
        (widthKreuzung länge radius winkelBogenmaß proxy)
        (heightKurve radius winkelBogenmaß proxy)
    <&> \(fromIntegral -> width, fromIntegral -> height)
    -> let halfHeight :: Double
           halfHeight = 0.5 * height
       in withAnchorName
              "Kreuzung"
              [ AnchorPoint { anchorX = 0, anchorY = halfHeight, anchorVX = -1, anchorVY = 0 }
              , AnchorPoint { anchorX = länge, anchorY = halfHeight, anchorVX = 1, anchorVY = 0 }
              , AnchorPoint
                { anchorX = radius * sin winkelBogenmaß
                , anchorY = halfHeight + radius * (1 - cos winkelBogenmaß)
                , anchorVX = cos winkelBogenmaß
                , anchorVY = sin winkelBogenmaß
                }
              , AnchorPoint
                { anchorX = width - radius * sin winkelBogenmaß
                , anchorY = halfHeight - radius * (1 - cos winkelBogenmaß)
                , anchorVX = -cos winkelBogenmaß
                , anchorVY = -sin winkelBogenmaß
                }]
