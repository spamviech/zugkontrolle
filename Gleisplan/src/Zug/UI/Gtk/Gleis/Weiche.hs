{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Zug.UI.Gtk.Gleis.Weiche
  (   -- * Gerade
    widthWeiche
  , heightWeiche
    -- ** Rechts
  , zeichneWeicheRechts
  , anchorPointsWeicheRechts
    -- ** Links
  , zeichneWeicheLinks
  , anchorPointsWeicheLinks
    -- * Dreiwegeweiche
  , widthDreiwegeweiche
  , heightDreiwegeweiche
  , zeichneDreiwegeweiche
  , anchorPointsDreiwegeweiche
    -- * Kurve
  , widthKurvenWeiche
  , heightKurvenWeiche
    -- ** Rechts
  , zeichneKurvenWeicheRechts
  , anchorPointsKurvenWeicheRechts
    -- ** Links
  , zeichneKurvenWeicheLinks
  , anchorPointsKurvenWeicheLinks
    -- * S-Kurve
  , widthSKurveWeiche
  , heightSKurveWeiche
    -- ** Rechts
  , zeichneSKurveWeicheRechts
  , anchorPointsSKurveWeicheRechts
    -- ** Links
  , zeichneSKurveWeicheLinks
  , anchorPointsSKurveWeicheLinks
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy())
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Matrix (Matrix(Matrix))

import Zug.UI.Gtk.Gleis.Anchor
       (AnchorPoint(..), AnchorPosition(..), AnchorDirection(..), AnchorPointMap, withAnchorName)
import Zug.UI.Gtk.Gleis.Gerade (zeichneGerade)
import Zug.UI.Gtk.Gleis.Kurve (KurvenBeschränkung(AlleBeschränkungen, EndBeschränkung)
                             , widthKurve, heightKurve, zeichneKurve)
import Zug.UI.Gtk.Gleis.Spurweite (abstand, Spurweite(), beschränkung)

widthWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthWeiche länge radius winkelBogenmaß proxy =
    max (ceiling länge) $ widthKurve radius winkelBogenmaß proxy

heightWeiche :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightWeiche = heightKurve

-- | Pfad zum Zeichnen einer Weiche mit angegebener Länge und Rechts-Kurve mit Kurvenradius und Winkel im Bogenmaß.
zeichneWeicheRechts :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Cairo.Render ()
zeichneWeicheRechts länge radius winkel proxy = do
    zeichneGerade länge proxy
    Cairo.stroke
    zeichneKurve radius winkel EndBeschränkung proxy

anchorPointsWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsWeicheRechts länge radius winkelBogenmaß proxy =
    withAnchorName
        "WeicheRechts"
        [ AnchorPoint
              AnchorPosition { anchorX = 0, anchorY = 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = -1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition { anchorX = länge, anchorY = 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = 1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = 0.5 * beschränkung proxy + radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = sin winkelBogenmaß }]

zeichneWeicheLinks :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Cairo.Render ()
zeichneWeicheLinks länge radius winkelBogenmaß proxy = do
    Cairo.translate halfWidth halfHeight
    Cairo.transform $ Matrix 1 0 0 (-1) 0 0
    Cairo.translate (-halfWidth) (-halfHeight)
    zeichneWeicheRechts länge radius winkelBogenmaß proxy
    where
        halfWidth :: Double
        halfWidth = 0.5 * fromIntegral (widthWeiche länge radius winkelBogenmaß proxy)

        halfHeight :: Double
        halfHeight = 0.5 * fromIntegral (heightWeiche radius winkelBogenmaß proxy)

anchorPointsWeicheLinks :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsWeicheLinks länge radius winkelBogenmaß proxy =
    withAnchorName
        "WeicheLinks"
        [ AnchorPoint
              AnchorPosition { anchorX = 0, anchorY = height - 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = -1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition { anchorX = länge, anchorY = height - 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = 1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = height - 0.5 * beschränkung proxy - radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }]
    where
        height :: Double
        height = fromIntegral $ heightWeiche radius winkelBogenmaß proxy

widthDreiwegeweiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthDreiwegeweiche länge radius winkelBogenmaß proxy =
    max (ceiling länge) $ widthKurve radius winkelBogenmaß proxy

heightDreiwegeweiche :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightDreiwegeweiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy)
    $ 2 * heightKurve radius winkelBogenmaß proxy - ceiling (beschränkung proxy)

zeichneDreiwegeweiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Cairo.Render ()
zeichneDreiwegeweiche länge radius winkelBogenmaß proxy = do
    Cairo.translate 0 startHeight
    zeichneWeicheRechts länge radius winkelBogenmaß proxy
    Cairo.translate 0 $ -startHeight
    Cairo.stroke
    Cairo.translate halfWidth halfHeight
    Cairo.transform $ Matrix 1 0 0 (-1) 0 0
    Cairo.translate (-halfWidth) (-halfHeight)
    Cairo.translate 0 startHeight
    zeichneKurve radius winkelBogenmaß EndBeschränkung proxy
    where
        width :: Int32
        width = widthDreiwegeweiche länge radius winkelBogenmaß proxy

        height :: Int32
        height = heightDreiwegeweiche radius winkelBogenmaß proxy

        halfWidth :: Double
        halfWidth = 0.5 * fromIntegral width

        halfHeight :: Double
        halfHeight = 0.5 * fromIntegral height

        startHeight :: Double
        startHeight = halfHeight - 0.5 * beschränkung proxy

anchorPointsDreiwegeweiche
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsDreiwegeweiche länge radius winkelBogenmaß proxy =
    withAnchorName
        "Dreiwegeweiche"
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
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = halfHeight - radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }]
    where
        height :: Double
        height = fromIntegral $ heightDreiwegeweiche radius winkelBogenmaß proxy

        halfHeight :: Double
        halfHeight = 0.5 * height

widthKurvenWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthKurvenWeiche länge radius winkelBogenmaß proxy =
    ceiling länge + widthKurve radius winkelBogenmaß proxy

heightKurvenWeiche :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightKurvenWeiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) $ heightKurve radius winkelBogenmaß proxy

-- | Pfad zum Zeichnen einer Kurven-Weiche mit angegebener Länge und Rechts-Kurve mit Kurvenradius und Winkel im Bogenmaß.
--
-- Beide Kurven haben den gleichen Radius und Winkel, die äußere Kurve beginnt erst nach /länge/.
zeichneKurvenWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Cairo.Render ()
zeichneKurvenWeicheRechts länge radius winkel proxy = do
    zeichneKurve radius winkel AlleBeschränkungen proxy
    Cairo.stroke
    -- Gerade vor äußerer Kurve
    Cairo.moveTo 0 gleisOben
    Cairo.lineTo länge gleisOben
    Cairo.moveTo 0 gleisUnten
    Cairo.lineTo länge gleisUnten
    Cairo.stroke
    Cairo.translate länge 0
    zeichneKurve radius winkel EndBeschränkung proxy
    where
        gleisOben :: Double
        gleisOben = abstand proxy

        gleisUnten :: Double
        gleisUnten = beschränkung proxy - abstand proxy

anchorPointsKurvenWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsKurvenWeicheRechts länge radius winkelBogenmaß proxy =
    withAnchorName
        "KurvenWeicheRechts"
        [ AnchorPoint
              AnchorPosition { anchorX = 0, anchorY = 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = -1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = 0.5 * beschränkung proxy + radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = sin winkelBogenmaß }
        , AnchorPoint
              AnchorPosition
              { anchorX = länge + radius * sin winkelBogenmaß
              , anchorY = 0.5 * beschränkung proxy + radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = sin winkelBogenmaß }]

zeichneKurvenWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Cairo.Render ()
zeichneKurvenWeicheLinks länge radius winkelBogenmaß proxy = do
    Cairo.translate halfWidth halfHeight
    Cairo.transform $ Matrix 1 0 0 (-1) 0 0
    Cairo.translate (-halfWidth) (-halfHeight)
    zeichneKurvenWeicheRechts länge radius winkelBogenmaß proxy
    where
        halfWidth :: Double
        halfWidth = 0.5 * fromIntegral (widthKurvenWeiche länge radius winkelBogenmaß proxy)

        halfHeight = 0.5 * fromIntegral (heightKurvenWeiche radius winkelBogenmaß proxy)

anchorPointsKurvenWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsKurvenWeicheLinks länge radius winkelBogenmaß proxy =
    withAnchorName
        "KurvenWeicheLinks"
        [ AnchorPoint
              AnchorPosition { anchorX = 0, anchorY = height - 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = -1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = height - 0.5 * beschränkung proxy - radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }
        , AnchorPoint
              AnchorPosition
              { anchorX = länge + radius * sin winkelBogenmaß
              , anchorY = height - 0.5 * beschränkung proxy - radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }]
    where
        height :: Double
        height = fromIntegral $ heightKurvenWeiche radius winkelBogenmaß proxy

widthSKurveWeiche :: (Spurweite z) => Double -> Double -> Double -> Double -> Proxy z -> Int32
widthSKurveWeiche länge radius winkelBogenmaß sWinkelBogenmaß proxy =
    -- TODO
    max (ceiling länge) $ widthKurve radius winkelBogenmaß proxy

heightSKurveWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
heightSKurveWeiche radius winkelBogenmaß sWinkelBogenmaß = heightKurve radius winkelBogenmaß

-- | Pfad zum Zeichnen einer Weiche mit angegebener Länge und Rechts-Kurve mit Kurvenradius und Winkel im Bogenmaß.
zeichneSKurveWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Double -> Proxy z -> Cairo.Render ()
zeichneSKurveWeicheRechts länge radius winkel sWinkelBogenmaß proxy = do
    zeichneGerade länge proxy
    Cairo.stroke
    zeichneKurve radius winkel EndBeschränkung proxy

anchorPointsSKurveWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsSKurveWeicheRechts länge radius winkelBogenmaß sWinkelBogenmaß proxy =
    withAnchorName
        "WeicheRechts"
        [ AnchorPoint
              AnchorPosition { anchorX = 0, anchorY = 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = -1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition { anchorX = länge, anchorY = 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = 1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = 0.5 * beschränkung proxy + radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = sin winkelBogenmaß }]

zeichneSKurveWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Double -> Proxy z -> Cairo.Render ()
zeichneSKurveWeicheLinks länge radius winkelBogenmaß sWinkelBogenmaß proxy = do
    Cairo.translate halfWidth halfHeight
    Cairo.transform $ Matrix 1 0 0 (-1) 0 0
    Cairo.translate (-halfWidth) (-halfHeight)
    zeichneWeicheRechts länge radius winkelBogenmaß proxy
    where
        halfWidth :: Double
        halfWidth = 0.5 * fromIntegral (widthWeiche länge radius winkelBogenmaß proxy)

        halfHeight :: Double
        halfHeight = 0.5 * fromIntegral (heightWeiche radius winkelBogenmaß proxy)

anchorPointsSKurveWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsSKurveWeicheLinks länge radius winkelBogenmaß sWinkelBogenmaß proxy =
    withAnchorName
        "WeicheLinks"
        [ AnchorPoint
              AnchorPosition { anchorX = 0, anchorY = height - 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = -1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition { anchorX = länge, anchorY = height - 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = 1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = height - 0.5 * beschränkung proxy - radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }]
    where
        height :: Double
        height = fromIntegral $ heightWeiche radius winkelBogenmaß proxy
