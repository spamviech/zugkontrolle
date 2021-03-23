//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use crate::zug::gleis::types::*;

/// Definition einer Weiche
#[derive(Debug, Clone)]
pub struct Weiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub direction: WeichenRichtung,
}
#[derive(Debug, Clone, Copy)]
pub enum WeichenRichtung {
    Links,
    Rechts,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum WeicheAnchors {
    Anfang,
    Gerade,
    Kurve,
}
/*
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
*/
