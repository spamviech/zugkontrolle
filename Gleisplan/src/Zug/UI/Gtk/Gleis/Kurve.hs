{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Zug.UI.Gtk.Gleis.Kurve
  ( zeichneKurve
  , anchorPointsKurve
  , widthKurve
  , heightKurve
  , KurvenBeschränkung(..)
  , KurveFehler(..)
  , KurveErgebnis(..)
  ) where

import Control.Monad (when)
import Data.Int (Int32)
import Data.Proxy (Proxy())
import qualified GI.Cairo.Render as Cairo

import Zug.UI.Gtk.Gleis.Anchor
       (AnchorPoint(..), AnchorPosition(..), AnchorDirection(..), AnchorPointMap, withAnchorName)
import Zug.UI.Gtk.Gleis.Spurweite (Spurweite(spurweite), radiusBegrenzung, beschränkung, abstand)

newtype KurveFehler = NichtUnterstützeWinkel Double
    deriving (Show, Eq)

-- | 'Either'-Like Datentyp.
-- Die 'Applicative'-Instanz sammelt alle Fehler, weshalb es keine 'Monad'en-Instanz geben kann.
data KurveErgebnis a
    = KurveErgebnis a
    | KurveFehler KurveFehler
    deriving (Show, Eq, Functor)

instance Applicative KurveErgebnis where
    pure :: a -> KurveErgebnis a
    pure = KurveErgebnis

    (<*>) :: KurveErgebnis (a -> b) -> KurveErgebnis a -> KurveErgebnis b
    (KurveErgebnis f) <*> (KurveErgebnis a) = KurveErgebnis $ f a
    (KurveFehler fehlerF) <*> _kurveErgebnis = KurveFehler fehlerF
    _kurveErgebnis <*> (KurveFehler fehlerA) = KurveFehler fehlerA

instance Monad KurveErgebnis where
    (>>=) :: KurveErgebnis a -> (a -> KurveErgebnis b) -> KurveErgebnis b
    (KurveErgebnis a) >>= f = f a
    (KurveFehler fehler) >>= _f = KurveFehler fehler

widthKurve :: (Spurweite z) => Double -> Double -> Proxy z -> KurveErgebnis Int32
widthKurve radius winkelBogenmaß proxy
    | (winkelBogenmaß < 0) || (winkelBogenmaß > 0.5 * pi) =
        KurveFehler $ NichtUnterstützeWinkel winkelBogenmaß
    | otherwise = KurveErgebnis $ ceiling $ radiusBegrenzung radius proxy * sin winkelBogenmaß

heightKurve :: (Spurweite z) => Double -> Double -> Proxy z -> KurveErgebnis Int32
heightKurve radius winkelBogenmaß proxy
    | (winkelBogenmaß < 0) || (winkelBogenmaß > 0.5 * pi) =
        KurveFehler $ NichtUnterstützeWinkel winkelBogenmaß
    | otherwise =
        KurveErgebnis
        $ ceiling
        $ radiusBegrenzung radius proxy * (1 - cos winkelBogenmaß)
        + beschränkung proxy * cos winkelBogenmaß

data KurvenBeschränkung
    = KeineBeschränkungen
    | AnfangsBeschränkung
    | EndBeschränkung
    | AlleBeschränkungen
    deriving (Eq, Show)

anfangsBeschränkung :: KurvenBeschränkung -> Bool
anfangsBeschränkung KeineBeschränkungen = False
anfangsBeschränkung AnfangsBeschränkung = True
anfangsBeschränkung EndBeschränkung = False
anfangsBeschränkung AlleBeschränkungen = True

endBeschränkung :: KurvenBeschränkung -> Bool
endBeschränkung KeineBeschränkungen = False
endBeschränkung AnfangsBeschränkung = False
endBeschränkung EndBeschränkung = True
endBeschränkung AlleBeschränkungen = True

-- | Pfad zum Zeichnen einer 'Kurve' mit angegebenen Kurvenradius und Winkel im Bogenmaß.
zeichneKurve
    :: (Spurweite z) => Double -> Double -> KurvenBeschränkung -> Proxy z -> Cairo.Render ()
zeichneKurve radius winkel kurvenBeschränkung proxy = do
    -- Beschränkungen
    when (anfangsBeschränkung kurvenBeschränkung) $ do
        Cairo.moveTo 0 0
        Cairo.lineTo 0 $ beschränkung proxy
        Cairo.stroke
    when (endBeschränkung kurvenBeschränkung) $ do
        Cairo.moveTo begrenzungX0 begrenzungY0
        Cairo.lineTo begrenzungX1 begrenzungY1
        Cairo.stroke
    -- Gleis
    Cairo.arc 0 bogenZentrumY radiusAußen anfangsWinkel (anfangsWinkel + winkel)
    Cairo.stroke
    Cairo.arc 0 bogenZentrumY radiusInnen anfangsWinkel (anfangsWinkel + winkel)
    where
        begrenzungX0 :: Double
        begrenzungX0 = radiusBegrenzungAußen * sin winkel

        begrenzungY0 :: Double
        begrenzungY0 = radiusBegrenzungAußen * (1 - cos winkel)

        begrenzungX1 :: Double
        begrenzungX1 = begrenzungX0 - beschränkung proxy * sin winkel

        begrenzungY1 :: Double
        begrenzungY1 = begrenzungY0 + beschränkung proxy * cos winkel

        bogenZentrumY :: Double
        bogenZentrumY = abstand proxy + radiusAußen

        anfangsWinkel :: Double
        anfangsWinkel = 3 * pi / 2

        radiusInnen :: Double
        radiusInnen = radius - 0.5 * spurweite proxy

        radiusAußen :: Double
        radiusAußen = radius + 0.5 * spurweite proxy

        radiusBegrenzungAußen :: Double
        radiusBegrenzungAußen = radiusAußen + abstand proxy

anchorPointsKurve :: (Spurweite z) => Double -> Double -> Proxy z -> AnchorPointMap
anchorPointsKurve radius winkelBogenmaß proxy =
    withAnchorName
        "Kurve"
        [ AnchorPoint
              AnchorPosition { anchorX = 0, anchorY = 0.5 * beschränkung proxy }
              AnchorDirection { anchorDX = -1, anchorDY = 0 }
        , AnchorPoint
              AnchorPosition
              { anchorX = radius * sin winkelBogenmaß
              , anchorY = 0.5 * beschränkung proxy + radius * (1 - cos winkelBogenmaß)
              }
              AnchorDirection { anchorDX = cos winkelBogenmaß, anchorDY = sin winkelBogenmaß }]
