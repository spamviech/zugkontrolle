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
  , KurveError(..)
  ) where

import Control.Monad (when)
import Data.Binary (Binary())
import Data.Int (Int32)
import Data.Proxy (Proxy())
import GHC.Generics (Generic())
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Matrix (Matrix(Matrix))

import Zug.UI.Gtk.Gleis.Anchor (AnchorPoint(..), AnchorPointMap, withAnchorName)
import Zug.UI.Gtk.Gleis.Gerade (zeichneGerade)
import Zug.UI.Gtk.Gleis.Kurve (KurvenBeschränkung(KeineBeschränkungen), KurveError(..), widthKurve
                             , heightKurve, zeichneKurve)
import Zug.UI.Gtk.Gleis.Spurweite (Spurweite(), beschränkung)

infixl 6 -#

{-# INLINE (-#) #-}

-- | Identisch zu '-'. Definiert, damit Section-Syntax funktioniert (keine unary negation).
(-#) :: (Num a) => a -> a -> a
(-#) = (-)

widthKreuzung :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Either KurveError Int32
widthKreuzung länge radius winkelBogenmaß proxy =
    max (ceiling länge) <$> widthKurve radius winkelBogenmaß proxy

heightKreuzung :: (Spurweite z) => Double -> Double -> Proxy z -> Either KurveError Int32
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
                -> Either KurveError (Cairo.Render ())
zeichneKreuzung länge radius winkelBogenmaß kreuzungsArt proxy =
    case ( widthKreuzung länge radius winkelBogenmaß proxy
         , heightKurve radius winkelBogenmaß proxy
         ) of
        (Right width, Right height) -> Right $ do
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
            where
                startHeight :: Double
                startHeight = max 0 $ fromIntegral height - beschränkung proxy

                halfWidth :: Double
                halfWidth = 0.5 * fromIntegral width

                halfHeight :: Double
                halfHeight = 0.5 * fromIntegral height
        (Left f0, Left f1) -> Left $ f0 <> f1
        (Left fehler, Right _height) -> Left fehler
        (Right _width, Left fehler) -> Left fehler

anchorPointsKreuzung
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Either KurveError AnchorPointMap
anchorPointsKreuzung länge radius winkelBogenmaß proxy =
    case ( widthKreuzung länge radius winkelBogenmaß proxy
         , heightKurve radius winkelBogenmaß proxy
         ) of
        (Right (fromIntegral -> width), Right (fromIntegral -> height)) -> Right
            $ withAnchorName
                "Kreuzung"
                [ AnchorPoint { anchorX = 0, anchorY = halfHeight, anchorVX = -1, anchorVY = 0 }
                , AnchorPoint
                  { anchorX = länge, anchorY = halfHeight, anchorVX = 1, anchorVY = 0 }
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
            where
                halfHeight :: Double
                halfHeight = 0.5 * height
        (Left f0, Left f1) -> Left $ f0 <> f1
        (Left fehler, Right _height) -> Left fehler
        (Right _width, Left fehler) -> Left fehler
