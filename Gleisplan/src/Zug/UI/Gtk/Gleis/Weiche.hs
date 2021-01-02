{-# LANGUAGE OverloadedStrings #-}

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
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy())
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Matrix (Matrix(Matrix))

import Zug.UI.Gtk.Gleis.Anchor (AnchorPoint(..), AnchorPointMap, withAnchorName)
import Zug.UI.Gtk.Gleis.Gerade (zeichneGerade)
import Zug.UI.Gtk.Gleis.Kurve (KurvenBeschränkung(AlleBeschränkungen, EndBeschränkung)
                             , widthKurve, heightKurve, zeichneKurve)
import Zug.UI.Gtk.Gleis.Spurweite (abstand, Spurweite(), beschränkung)

widthWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthWeiche länge radius winkelBogenmaß proxy =
    max (ceiling länge) $ widthKurve radius winkelBogenmaß proxy

heightWeiche :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightWeiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) $ heightKurve radius winkelBogenmaß proxy

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
          { anchorX = 0, anchorY = 0.5 * beschränkung proxy, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint
          { anchorX = länge, anchorY = 0.5 * beschränkung proxy, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint
          { anchorX = radius * sin winkelBogenmaß
          , anchorY = radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = sin winkelBogenmaß
          }]

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
          { anchorX = 0
          , anchorY = height - 0.5 * beschränkung proxy
          , anchorVX = -1
          , anchorVY = 0
          }
        , AnchorPoint
          { anchorX = länge
          , anchorY = height - 0.5 * beschränkung proxy
          , anchorVX = -1
          , anchorVY = 0
          }
        , AnchorPoint
          { anchorX = radius * sin winkelBogenmaß
          , anchorY = height - radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = -sin winkelBogenmaß
          }]
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
        startHeight :: Double
        startHeight =
            max 0 $ fromIntegral (heightKurve radius winkelBogenmaß proxy) - beschränkung proxy

        halfWidth :: Double
        halfWidth = 0.5 * fromIntegral (widthDreiwegeweiche länge radius winkelBogenmaß proxy)

        halfHeight :: Double
        halfHeight = 0.5 * fromIntegral (heightDreiwegeweiche radius winkelBogenmaß proxy)

anchorPointsDreiwegeweiche
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsDreiwegeweiche länge radius winkelBogenmaß proxy =
    withAnchorName
        "Dreiwegeweiche"
        [ AnchorPoint { anchorX = 0, anchorY = halfHeight, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint { anchorX = länge, anchorY = halfHeight, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint
          { anchorX = radius * sin winkelBogenmaß
          , anchorY = startHeight + radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = sin winkelBogenmaß
          }
        , AnchorPoint
          { anchorX = radius * sin winkelBogenmaß
          , anchorY = startHeight - radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = -sin winkelBogenmaß
          }]
    where
        height :: Double
        height = fromIntegral $ heightDreiwegeweiche radius winkelBogenmaß proxy

        halfHeight :: Double
        halfHeight = 0.5 * height

        startHeight :: Double
        startHeight = max 0 $ height - beschränkung proxy

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
          { anchorX = 0, anchorY = 0.5 * beschränkung proxy, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint
          { anchorX = radius * sin winkelBogenmaß
          , anchorY = radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = sin winkelBogenmaß
          }
        , AnchorPoint
          { anchorX = länge + radius * sin winkelBogenmaß
          , anchorY = radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = sin winkelBogenmaß
          }]

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

        halfHeight :: Double
        halfHeight = 0.5 * fromIntegral (heightKurvenWeiche radius winkelBogenmaß proxy)

anchorPointsKurvenWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsKurvenWeicheLinks länge radius winkelBogenmaß proxy =
    withAnchorName
        "KurvenWeicheLinks"
        [ AnchorPoint
          { anchorX = 0
          , anchorY = height - 0.5 * beschränkung proxy
          , anchorVX = -1
          , anchorVY = 0
          }
        , AnchorPoint
          { anchorX = radius * sin winkelBogenmaß
          , anchorY = height - radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = -sin winkelBogenmaß
          }
        , AnchorPoint
          { anchorX = länge + radius * sin winkelBogenmaß
          , anchorY = height - radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = -sin winkelBogenmaß
          }]
    where
        height :: Double
        height = fromIntegral $ heightWeiche radius winkelBogenmaß proxy
