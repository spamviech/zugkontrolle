//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use crate::gleis::types::*;

/// Definition einer Dreiwege-Weiche
#[derive(Debug, Clone)]
pub struct DreiwegeWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum DreiwegeWeicheAnchors {
    Anfang,
    Gerade,
    Links,
    Rechts,
}
/*
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
*/
