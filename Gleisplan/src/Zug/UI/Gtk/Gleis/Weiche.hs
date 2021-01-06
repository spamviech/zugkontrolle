{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Zug.UI.Gtk.Gleis.Weiche
  (   -- * Gerade
    widthWeiche
  , heightWeiche
  , KurveFehler(..)
  , KurveErgebnis(..)
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

import Zug.UI.Gtk.Gleis.Anchor
       (AnchorPoint(..), AnchorPosition(..), AnchorDirection(..), AnchorPointMap, withAnchorName)
import Zug.UI.Gtk.Gleis.Gerade (zeichneGerade)
import Zug.UI.Gtk.Gleis.Kurve
       (KurvenBeschränkung(AlleBeschränkungen, EndBeschränkung), KurveFehler(..)
      , KurveErgebnis(..), widthKurve, heightKurve, zeichneKurve)
import Zug.UI.Gtk.Gleis.Spurweite (abstand, Spurweite(), beschränkung)

infixl 6 -#

{-# INLINE (-#) #-}

-- | Identisch zu '-'. Definiert, damit Section-Syntax funktioniert (keine unary negation).
(-#) :: (Num a) => a -> a -> a
(-#) = (-)

widthWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis Int32
widthWeiche länge radius winkelBogenmaß proxy =
    max (ceiling länge) <$> widthKurve radius winkelBogenmaß proxy

heightWeiche :: (Spurweite z) => Double -> Double -> Proxy z -> KurveErgebnis Int32
heightWeiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) <$> heightKurve radius winkelBogenmaß proxy

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

zeichneWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis (Cairo.Render ())
zeichneWeicheLinks länge radius winkelBogenmaß proxy = do
    halfWidth <- (0.5 *) . fromIntegral <$> widthWeiche länge radius winkelBogenmaß proxy
    halfHeight <- (0.5 *) . fromIntegral <$> heightWeiche radius winkelBogenmaß proxy
    pure $ do
        Cairo.translate halfWidth halfHeight
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth) (-halfHeight)
        zeichneWeicheRechts länge radius winkelBogenmaß proxy

anchorPointsWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis AnchorPointMap
anchorPointsWeicheLinks länge radius winkelBogenmaß proxy = do
    height <- fromIntegral <$> heightWeiche radius winkelBogenmaß proxy
    pure
        $ withAnchorName
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
                  , anchorY =
                        height - 0.5 * beschränkung proxy - radius * (1 - cos winkelBogenmaß)
                  }
                  AnchorDirection
                  { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }]

widthDreiwegeweiche
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis Int32
widthDreiwegeweiche länge radius winkelBogenmaß proxy =
    max (ceiling länge) <$> widthKurve radius winkelBogenmaß proxy

heightDreiwegeweiche :: (Spurweite z) => Double -> Double -> Proxy z -> KurveErgebnis Int32
heightDreiwegeweiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) . (-# ceiling (beschränkung proxy)) . (2 *)
    <$> heightKurve radius winkelBogenmaß proxy

zeichneDreiwegeweiche
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis (Cairo.Render ())
zeichneDreiwegeweiche länge radius winkelBogenmaß proxy = do
    width <- widthDreiwegeweiche länge radius winkelBogenmaß proxy
    height <- heightDreiwegeweiche radius winkelBogenmaß proxy
    let halfWidth :: Double
        halfWidth = 0.5 * fromIntegral width
        halfHeight :: Double
        halfHeight = 0.5 * fromIntegral height
        startHeight :: Double
        startHeight = halfHeight - 0.5 * beschränkung proxy
    pure $ do
        Cairo.translate 0 startHeight
        zeichneWeicheRechts länge radius winkelBogenmaß proxy
        Cairo.translate 0 $ -startHeight
        Cairo.stroke
        Cairo.translate halfWidth halfHeight
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth) (-halfHeight)
        Cairo.translate 0 startHeight
        zeichneKurve radius winkelBogenmaß EndBeschränkung proxy

anchorPointsDreiwegeweiche
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis AnchorPointMap
anchorPointsDreiwegeweiche länge radius winkelBogenmaß proxy = do
    height <- fromIntegral <$> heightDreiwegeweiche radius winkelBogenmaß proxy
    let halfHeight :: Double
        halfHeight = 0.5 * height
    pure
        $ withAnchorName
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
                  AnchorDirection
                  { anchorDX = cos winkelBogenmaß, anchorDY = sin winkelBogenmaß }
            , AnchorPoint
                  AnchorPosition
                  { anchorX = radius * sin winkelBogenmaß
                  , anchorY = halfHeight - radius * (1 - cos winkelBogenmaß)
                  }
                  AnchorDirection
                  { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }]

widthKurvenWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis Int32
widthKurvenWeiche länge radius winkelBogenmaß proxy =
    (ceiling länge +) <$> widthKurve radius winkelBogenmaß proxy

heightKurvenWeiche :: (Spurweite z) => Double -> Double -> Proxy z -> KurveErgebnis Int32
heightKurvenWeiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) <$> heightKurve radius winkelBogenmaß proxy

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
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis (Cairo.Render ())
zeichneKurvenWeicheLinks länge radius winkelBogenmaß proxy = do
    halfWidth <- (0.5 *) . fromIntegral <$> widthKurvenWeiche länge radius winkelBogenmaß proxy
    halfHeight <- (0.5 *) . fromIntegral <$> heightKurvenWeiche radius winkelBogenmaß proxy
    pure $ do
        Cairo.translate halfWidth halfHeight
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth) (-halfHeight)
        zeichneKurvenWeicheRechts länge radius winkelBogenmaß proxy

anchorPointsKurvenWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> KurveErgebnis AnchorPointMap
anchorPointsKurvenWeicheLinks länge radius winkelBogenmaß proxy = do
    height <- fromIntegral <$> heightKurvenWeiche radius winkelBogenmaß proxy
    pure
        $ withAnchorName
            "KurvenWeicheLinks"
            [ AnchorPoint
                  AnchorPosition { anchorX = 0, anchorY = height - 0.5 * beschränkung proxy }
                  AnchorDirection { anchorDX = -1, anchorDY = 0 }
            , AnchorPoint
                  AnchorPosition
                  { anchorX = radius * sin winkelBogenmaß
                  , anchorY =
                        height - 0.5 * beschränkung proxy - radius * (1 - cos winkelBogenmaß)
                  }
                  AnchorDirection
                  { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }
            , AnchorPoint
                  AnchorPosition
                  { anchorX = länge + radius * sin winkelBogenmaß
                  , anchorY =
                        height - 0.5 * beschränkung proxy - radius * (1 - cos winkelBogenmaß)
                  }
                  AnchorDirection
                  { anchorDX = cos winkelBogenmaß, anchorDY = -sin winkelBogenmaß }]
