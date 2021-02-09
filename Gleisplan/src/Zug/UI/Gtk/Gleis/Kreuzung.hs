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
  ) where

import Control.Monad (when)
import Data.Int (Int32)
import Data.Proxy (Proxy())
import Flat (Flat())
import GHC.Generics (Generic())
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Matrix (Matrix(Matrix))

import Zug.UI.Gtk.Gleis.Anchor
       (AnchorPoint(..), AnchorPosition(..), AnchorDirection(..), AnchorPointMap, withAnchorName)
import Zug.UI.Gtk.Gleis.Gerade (zeichneGerade)
import Zug.UI.Gtk.Gleis.Kurve
       (KurvenBeschränkung(KeineBeschränkungen), widthKurve, heightKurve, zeichneKurve)
import Zug.UI.Gtk.Gleis.Spurweite (Spurweite(), beschränkung)

widthKreuzung :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthKreuzung länge radius winkelBogenmaß proxy =
    max (ceiling länge) $ widthKurve radius winkelBogenmaß proxy

heightKreuzung :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightKreuzung radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy)
    $ 2 * heightKurve radius winkelBogenmaß proxy - ceiling (beschränkung proxy)

data KreuzungsArt
    = MitKurve
    | OhneKurve
    deriving (Eq, Show, Generic)

instance Flat KreuzungsArt

zeichneKreuzung
    :: (Spurweite z) => Double -> Double -> Double -> KreuzungsArt -> Proxy z -> Cairo.Render ()
zeichneKreuzung länge radius winkelBogenmaß kreuzungsArt proxy = do
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
        width :: Int32
        width = widthKreuzung länge radius winkelBogenmaß proxy

        height :: Int32
        height = heightKreuzung radius winkelBogenmaß proxy

        halfWidth :: Double
        halfWidth = 0.5 * fromIntegral width

        halfHeight :: Double
        halfHeight = 0.5 * fromIntegral height

        startHeight :: Double
        startHeight = halfHeight - 0.5 * beschränkung proxy

anchorPointsKreuzung :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsKreuzung länge radius winkelBogenmaß proxy =
    withAnchorName
        "Kreuzung"
        [ AnchorPoint
              AnchorPosition { anchorX = 0, anchorY = halfHeight }
              AnchorDirection { anchorDX = -1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition { anchorX = länge, anchorY = halfHeight }
              AnchorDirection { anchorDX = 1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = halfHeight + radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = sin winkelBogenmaß }
        , AnchorPoint
              AnchorPosition
              { anchorX = width - radius * sin winkelBogenmaß
              , anchorY = halfHeight - radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = -cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }]
    where
        width :: Double
        width = fromIntegral $ widthKreuzung länge radius winkelBogenmaß proxy

        height :: Double
        height = fromIntegral $ heightKreuzung radius winkelBogenmaß proxy

        halfHeight :: Double
        halfHeight = 0.5 * height
