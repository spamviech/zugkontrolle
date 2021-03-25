//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::gerade::*;
use crate::gleis::types::*;

/// Definition einer Kurven-Weiche
#[derive(Debug, Clone)]
pub struct KurvenWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub direction: WeichenRichtung,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum KurvenWeicheAnchors {
    Anfang,
    Innen,
    Aussen,
}
/*
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
*/
