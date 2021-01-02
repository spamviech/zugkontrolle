{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

{-
ideas for rewrite with gtk4
    start from scratch (UI), so legacy-based mistakes might vanish
look at Gtk.renderLine (cairo-render/-connector should still work)
Use `GLib.addIdle GLib.PRIORITY_DEFAULT $ gtkAction` to call Gtk from a forked thread
    (sync e.g. with TMVar for a blocking version)
    https://github.com/haskell-gi/haskell-gi/wiki/Using-threads-in-Gdk-and-Gtk--programs
Gtk.Application has to be used instead of Gtk.main
    https://hackage.haskell.org/package/gi-gtk-4.0.2/docs/GI-Gtk-Objects-Application.html
    startup/activate-signals are in gi-gio
    https://hackage.haskell.org/package/gi-gio-2.0.27/docs/GI-Gio-Objects-Application.html#v:applicationSetResourceBasePath
AspectFrame doesn't draw a frame around the child, so might be useful here
    doesn't pass scaling information to DrawingArea
    usefulness questionable anyway due to rotation requirement
Assistant should work again
-}
module Zug.UI.Gtk.Gleis.Widget
  ( -- * Gleis Widgets
    Gleis()
    -- ** Definition
  , GleisDefinition(..)
  , WeichenArt(..)
  , WeichenRichtungAllgemein(..)
  , alsDreiweg
  , WeichenRichtung(..)
  , KreuzungsArt(..)
    -- ** Anker-Punkte
  , AnchorName(..)
  , AnchorPoint(..)
    -- * Methoden
  , gleisNew
  , gleisGetSize
    --   , gleisGetWidth
    --   , gleisGetHeight
    -- * Anzeige
  , GleisAnzeige()
  , Position(..)
    -- ** Konstruktor
  , gleisAnzeigeNew
    -- ** Gleis platzieren
  , gleisPut
  , gleisRemove
  , gleisAnzeigePutLabel
  , gleisAnzeigeRemoveLabel
  , gleisAnzeigeScale
  ) where

import Control.Concurrent.STM
       (STM, atomically, TVar, newTVarIO, readTVarIO, readTVar, writeTVar, modifyTVar', TMVar
      , newEmptyTMVarIO, putTMVar, takeTMVar, readTMVar, tryReadTMVar, tryPutTMVar)
import Control.Monad (foldM, when, void, forM_)
import Control.Monad.RWS.Strict
       (RWST(), MonadReader(ask), MonadState(state), MonadWriter(tell), execRWST)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.HashMap.Strict (HashMap())
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable(hashWithSalt))
import Data.Int (Int32)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy(Proxy))
import Data.RTree (RTree)
import qualified Data.RTree as RTree
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic())
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import GI.Cairo.Render.Matrix (Matrix(Matrix))
import qualified GI.Graphene as Graphene
import qualified GI.Gsk as Gsk
import qualified GI.Gtk as Gtk
import Numeric.Natural (Natural)

import Zug.Enums (Zugtyp(..))
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget))

-- import Zug.UI.Gtk.Hilfsfunktionen (fixedPutWidgetNew)
-- | 'Gtk.Widget' von einem Gleis.
-- Die Größe wird nur über 'gleisScale', 'gleisSetWidth' und 'gleisSetHeight' verändert.
data Gleis (z :: Zugtyp) =
    Gleis
    { drawingArea :: Gtk.DrawingArea
    , width :: Int32
    , height :: Int32
    , anchorPoints :: HashMap AnchorName AnchorPoint
    , tmvarAnchorPoints :: TMVar (TVar AnchorPointRTree)
    }
    deriving (Eq)

instance MitWidget (Gleis z) where
    erhalteWidget :: (MonadIO m) => Gleis z -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . drawingArea

instance Hashable (Gleis z) where
    hashWithSalt :: Int -> Gleis z -> Int
    hashWithSalt salt Gleis {width, height, anchorPoints} =
        hashWithSalt salt (width, height, anchorPoints)

-- | Speichern aller AnchorPoints
type AnchorPointRTree = RTree (NonEmpty (AnchorName, AnchorPoint, Gtk.DrawingArea))

-- | Namen um ausgezeichnete Punkte eines 'Gleis'es anzusprechen.
newtype AnchorName = AnchorName { anchor :: Text }
    deriving (Show, Eq, Hashable)

-- | Position und ausgehender Vektor eines AnchorPoint.
data AnchorPoint =
    AnchorPoint { anchorX :: Double, anchorY :: Double, anchorVX :: Double, anchorVY :: Double }
    deriving (Eq, Show, Generic)

instance Hashable AnchorPoint

mbb :: AnchorPoint -> RTree.MBB
mbb AnchorPoint {anchorX, anchorY} =
    RTree.mbb (anchorX - epsilon) (anchorY - epsilon) (anchorX + epsilon) (anchorY + epsilon)
    where
        epsilon :: Double
        epsilon = 0.5

-- | Erhalte die Breite und Höhe eines 'Gleis'es (unscaled).
gleisGetSize :: Gleis z -> (Int32, Int32)
gleisGetSize Gleis {width, height} = (width, height)

-- | Create a new 'Gtk.DrawingArea' with a fixed size set up with the specified 'Cairo.Render' /draw/ path.
--
-- 'Cairo.setLineWidth' 1 is called before the /draw/ action is executed.
-- After the action 'Cairo.stroke' is executed.
createGleisWidget :: forall m z.
                  (MonadIO m)
                  => (Proxy z -> Int32)
                  -> (Proxy z -> Int32)
                  -> (Proxy z -> HashMap AnchorName AnchorPoint)
                  -> (Proxy z -> Cairo.Render ())
                  -> m (Gleis z)
createGleisWidget widthFn heightFn anchorPointsFn draw = do
    (tvarScale, tvarAngle, tmvarAnchorPoints)
        <- liftIO $ (,,) <$> newTVarIO 1 <*> newTVarIO 0 <*> newEmptyTMVarIO
    let proxy :: Proxy z
        proxy = Proxy
        width :: Int32
        width = widthFn proxy
        height :: Int32
        height = heightFn proxy
        anchorPoints :: HashMap AnchorName AnchorPoint
        anchorPoints = anchorPointsFn proxy
    drawingArea <- Gtk.drawingAreaNew
    Gtk.widgetSetHexpand drawingArea False
    Gtk.widgetSetHalign drawingArea Gtk.AlignStart
    Gtk.widgetSetVexpand drawingArea False
    Gtk.widgetSetValign drawingArea Gtk.AlignStart
    let gleis = Gleis { drawingArea, width, height, anchorPoints, tmvarAnchorPoints }
    Gtk.drawingAreaSetContentHeight drawingArea height
    Gtk.drawingAreaSetContentWidth drawingArea width
    Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ \_drawingArea context newWidth newHeight
        -> void $ flip Cairo.renderWithContext context $ do
            (scale, angle, knownAnchorPoints) <- liftIO
                $ atomically
                $ (,,) <$> readTVar tvarScale
                <*> readTVar tvarAngle
                <*> (tryReadTMVar tmvarAnchorPoints >>= maybe (pure RTree.empty) readTVar)
            Cairo.save
            let halfWidth = 0.5 * fromIntegral newWidth
                halfHeight = 0.5 * fromIntegral newHeight
            Cairo.translate halfWidth halfHeight
            Cairo.rotate angle
            Cairo.scale scale scale
            Cairo.translate (-0.5 * fromIntegral width) (-0.5 * fromIntegral height)
            Cairo.setLineWidth 1
            Cairo.newPath
            draw Proxy
            Cairo.stroke
            Cairo.restore
            -- container coordinates
            Cairo.save
            Cairo.setLineWidth 1
            forM_ anchorPoints
                $ \anchorPoint@AnchorPoint {anchorX, anchorY, anchorVX, anchorVY} -> do
                    Cairo.moveTo anchorX anchorY
                    let intersections = mbb anchorPoint `RTree.intersect` knownAnchorPoints
                        (r, g, b) =
                            if length intersections > 1
                                then (0, 1, 0)
                                else (0, 0, 1)
                        len = anchorVX * anchorVX + anchorVY * anchorVY
                    Cairo.setSourceRGB r g b
                    Cairo.relLineTo (-5 * anchorVX / len) (-5 * anchorVY / len)
                    Cairo.stroke
            Cairo.restore
            pure True
    pure gleis

class Spurweite (z :: Zugtyp) where
    spurweite :: Proxy z -> Double

instance Spurweite 'Märklin where
    spurweite :: Proxy 'Märklin -> Double
    spurweite = const 16.5

instance Spurweite 'Lego where
    spurweite :: Proxy 'Lego -> Double
    spurweite = const 38

abstand :: (Spurweite z) => Proxy z -> Double
abstand gleis = spurweite gleis / 3

beschränkung :: (Spurweite z) => Proxy z -> Double
beschränkung gleis = spurweite gleis + 2 * abstand gleis

-- | Erzeuge eine neues gerades 'Gleis' der angegebenen Länge.
geradeNew :: (MonadIO m, Spurweite z) => Double -> m (Gleis z)
geradeNew länge =
    createGleisWidget
        (const $ ceiling länge)
        (ceiling . beschränkung)
        (anchorPointsGerade länge)
    $ zeichneGerade länge

-- | Pfad zum Zeichnen einer Geraden der angegebenen Länge.
zeichneGerade :: (Spurweite z) => Double -> Proxy z -> Cairo.Render ()
zeichneGerade länge proxy = do
    -- Beschränkungen
    Cairo.moveTo 0 0
    Cairo.lineTo 0 $ beschränkung proxy
    Cairo.moveTo länge 0
    Cairo.lineTo länge $ beschränkung proxy
    -- Gleis
    Cairo.moveTo 0 gleisOben
    Cairo.lineTo länge gleisOben
    Cairo.moveTo 0 gleisUnten
    Cairo.lineTo länge gleisUnten
    where
        gleisOben :: Double
        gleisOben = abstand proxy

        gleisUnten :: Double
        gleisUnten = beschränkung proxy - abstand proxy

withAnchorName :: Text -> [AnchorPoint] -> HashMap AnchorName AnchorPoint
withAnchorName anchorBaseName =
    fst . foldl' (\(n, acc) anchorPoint
                  -> ( succ n
                     , HashMap.insert
                           AnchorName { anchor = anchorBaseName <> Text.pack (show n) }
                           anchorPoint
                           acc
                     )) (0, HashMap.empty)

anchorPointsGerade
    :: forall z. (Spurweite z) => Double -> Proxy z -> HashMap AnchorName AnchorPoint
anchorPointsGerade länge proxy =
    withAnchorName
        "Gerade"
        [ AnchorPoint
          { anchorX = 0, anchorY = 0.5 * beschränkung proxy, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint
          { anchorX = länge, anchorY = 0.5 * beschränkung proxy, anchorVX = -1, anchorVY = 0 }]

-- Märklin verwendet mittleren Kurvenradius
-- http://www.modellbau-wiki.de/wiki/Gleisradius
radiusBegrenzung :: (Spurweite z) => Double -> Proxy z -> Double
radiusBegrenzung radius proxy = radius + 0.5 * spurweite proxy + abstand proxy

widthKurve :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
widthKurve radius winkelBogenmaß proxy
    | winkelBogenmaß <= 0.5 * pi = ceiling $ radiusBegrenzung radius proxy * sin winkelBogenmaß
    | otherwise = error "Nur Kurven mit Winkel <= pi/2 (90°) sind unterstützt."

heightKurve :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightKurve radius winkelBogenmaß proxy
    | winkelBogenmaß <= 0.5 * pi =
        ceiling
        $ radiusBegrenzung radius proxy * (1 - cos winkelBogenmaß)
        + beschränkung proxy * cos winkelBogenmaß
    | otherwise = error "Nur Kurven mit Winkel <= pi/2 (90°) sind unterstützt."

-- | Erzeuge eine neue Kurve mit angegebenen Radius und Winkel im Gradmaß.
kurveNew :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> m (Gleis z)
kurveNew radius winkel =
    createGleisWidget
        (widthKurve radius winkelBogenmaß)
        (heightKurve radius winkelBogenmaß)
        (anchorPointsKurve radius winkelBogenmaß)
    $ zeichneKurve radius winkelBogenmaß AlleBeschränkungen
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

data KurvenBeschränkung
    = KeineBeschränkungen
    | AnfangsBeschränkung
    | EndBeschränkung
    | AlleBeschränkungen

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

anchorPointsKurve :: (Spurweite z) => Double -> Double -> Proxy z -> HashMap AnchorName AnchorPoint
anchorPointsKurve radius winkelBogenmaß proxy =
    withAnchorName
        "Kurve"
        [ AnchorPoint
          { anchorX = 0, anchorY = 0.5 * beschränkung proxy, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint
          { anchorX = radius * sin winkelBogenmaß
          , anchorY = radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = sin winkelBogenmaß
          }]

widthWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthWeiche länge radius winkelBogenmaß proxy =
    max (ceiling länge) $ widthKurve radius winkelBogenmaß proxy

heightWeiche :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightWeiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) $ heightKurve radius winkelBogenmaß proxy

weicheRechtsNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
weicheRechtsNew länge radius winkel =
    createGleisWidget
        (widthWeiche länge radius winkelBogenmaß)
        (heightWeiche radius winkelBogenmaß)
        (anchorPointsWeicheRechts länge radius winkelBogenmaß)
    $ zeichneWeicheRechts länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Pfad zum Zeichnen einer Weiche mit angegebener Länge und Rechts-Kurve mit Kurvenradius und Winkel im Bogenmaß.
zeichneWeicheRechts :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Cairo.Render ()
zeichneWeicheRechts länge radius winkel proxy = do
    zeichneGerade länge proxy
    Cairo.stroke
    zeichneKurve radius winkel EndBeschränkung proxy

anchorPointsWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> HashMap AnchorName AnchorPoint
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

weicheLinksNew :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
weicheLinksNew länge radius winkel =
    createGleisWidget
        (widthWeiche länge radius winkelBogenmaß)
        (heightWeiche radius winkelBogenmaß)
        (anchorPointsWeicheLinks länge radius winkelBogenmaß)
    $ \proxy -> do
        Cairo.translate (halfWidth proxy) (halfHeight proxy)
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth proxy) (-halfHeight proxy)
        zeichneWeicheRechts länge radius winkelBogenmaß proxy
    where
        halfWidth :: Proxy z -> Double
        halfWidth proxy = 0.5 * fromIntegral (widthWeiche länge radius winkelBogenmaß proxy)

        halfHeight :: Proxy z -> Double
        halfHeight proxy = 0.5 * fromIntegral (heightWeiche radius winkelBogenmaß proxy)

        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

anchorPointsWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> HashMap AnchorName AnchorPoint
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

dreiwegeweicheNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
dreiwegeweicheNew länge radius winkel =
    createGleisWidget
        (widthDreiwegeweiche länge radius winkelBogenmaß)
        (heightDreiwegeweiche radius winkelBogenmaß)
        (anchorPointsDreiwegeweiche länge radius winkelBogenmaß)
    $ \proxy -> do
        Cairo.translate 0 $ startHeight proxy
        zeichneWeicheRechts länge radius winkelBogenmaß proxy
        Cairo.translate 0 $ -startHeight proxy
        Cairo.stroke
        Cairo.translate (halfWidth proxy) (halfHeight proxy)
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth proxy) (-halfHeight proxy)
        Cairo.translate 0 $ startHeight proxy
        zeichneKurve radius winkelBogenmaß EndBeschränkung proxy
    where
        startHeight :: Proxy z -> Double
        startHeight proxy =
            max 0 $ fromIntegral (heightKurve radius winkelBogenmaß proxy) - beschränkung proxy

        halfWidth :: Proxy z -> Double
        halfWidth proxy =
            0.5 * fromIntegral (widthDreiwegeweiche länge radius winkelBogenmaß proxy)

        halfHeight :: Proxy z -> Double
        halfHeight proxy = 0.5 * fromIntegral (heightDreiwegeweiche radius winkelBogenmaß proxy)

        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

anchorPointsDreiwegeweiche
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> HashMap AnchorName AnchorPoint
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

kurvenWeicheRechtsNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
kurvenWeicheRechtsNew länge radius winkel =
    createGleisWidget
        (widthKurvenWeiche länge radius winkelBogenmaß)
        (heightKurvenWeiche radius winkelBogenmaß)
        (anchorPointsKurvenWeicheRechts länge radius winkelBogenmaß)
    $ zeichneKurvenWeicheRechts länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

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
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> HashMap AnchorName AnchorPoint
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

kurvenWeicheLinksNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
kurvenWeicheLinksNew länge radius winkel =
    createGleisWidget
        (widthKurvenWeiche länge radius winkelBogenmaß)
        (heightKurvenWeiche radius winkelBogenmaß)
        (anchorPointsKurvenWeicheLinks länge radius winkelBogenmaß)
    $ \proxy -> do
        Cairo.translate (halfWidth proxy) (halfHeight proxy)
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth proxy) (-halfHeight proxy)
        zeichneKurvenWeicheRechts länge radius winkelBogenmaß proxy
    where
        halfWidth :: Proxy z -> Double
        halfWidth proxy =
            0.5 * fromIntegral (widthKurvenWeiche länge radius winkelBogenmaß proxy)

        halfHeight :: Proxy z -> Double
        halfHeight proxy = 0.5 * fromIntegral (heightKurvenWeiche radius winkelBogenmaß proxy)

        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

anchorPointsKurvenWeicheLinks
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> HashMap AnchorName AnchorPoint
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

widthKreuzung :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthKreuzung länge radius winkelBogenmaß proxy =
    max (ceiling länge) $ widthKurve radius winkelBogenmaß proxy

heightKreuzung :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightKreuzung radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy)
    $ 2 * heightKurve radius winkelBogenmaß proxy - ceiling (beschränkung proxy)

kreuzungNew :: forall m z.
            (MonadIO m, Spurweite z)
            => Double
            -> Double
            -> Double
            -> KreuzungsArt
            -> m (Gleis z)
kreuzungNew länge radius winkel kreuzungsArt =
    createGleisWidget
        (widthKreuzung länge radius winkelBogenmaß)
        (heightKreuzung radius winkelBogenmaß)
        (anchorPointsKreuzung länge radius winkelBogenmaß)
    $ \proxy -> do
        Cairo.translate 0 $ startHeight proxy
        zeichneGerade länge proxy
        when (kreuzungsArt == MitKurve)
            $ zeichneKurve radius winkelBogenmaß KeineBeschränkungen proxy
        Cairo.translate 0 $ -startHeight proxy
        Cairo.stroke
        Cairo.translate (halfWidth proxy) (halfHeight proxy)
        Cairo.rotate winkelBogenmaß
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth proxy) (-halfHeight proxy)
        Cairo.translate 0 $ startHeight proxy
        zeichneGerade länge proxy
        when (kreuzungsArt == MitKurve)
            $ zeichneKurve radius winkelBogenmaß KeineBeschränkungen proxy
    where
        startHeight :: Proxy z -> Double
        startHeight proxy =
            max 0 $ fromIntegral (heightKurve radius winkelBogenmaß proxy) - beschränkung proxy

        halfWidth :: Proxy z -> Double
        halfWidth proxy = 0.5 * fromIntegral (widthKreuzung länge radius winkelBogenmaß proxy)

        halfHeight :: Proxy z -> Double
        halfHeight proxy = 0.5 * fromIntegral (heightKreuzung radius winkelBogenmaß proxy)

        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

anchorPointsKreuzung
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> HashMap AnchorName AnchorPoint
anchorPointsKreuzung länge radius winkelBogenmaß proxy =
    withAnchorName
        "Kreuzung"
        [ AnchorPoint { anchorX = 0, anchorY = halfHeight, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint { anchorX = länge, anchorY = halfHeight, anchorVX = -1, anchorVY = 0 }
        , AnchorPoint
          { anchorX = radius * sin winkelBogenmaß
          , anchorY = startHeight + radius * (1 - cos winkelBogenmaß)
          , anchorVX = cos winkelBogenmaß
          , anchorVY = sin winkelBogenmaß
          }
        , AnchorPoint
          { anchorX = width - radius * sin winkelBogenmaß
          , anchorY = startHeight - radius * (1 - cos winkelBogenmaß)
          , anchorVX = -cos winkelBogenmaß
          , anchorVY = -sin winkelBogenmaß
          }]
    where
        width :: Double
        width = fromIntegral $ widthKreuzung länge radius winkelBogenmaß proxy

        height :: Double
        height = fromIntegral $ heightKreuzung radius winkelBogenmaß proxy

        halfHeight :: Double
        halfHeight = 0.5 * height

        startHeight :: Double
        startHeight = max 0 $ height - beschränkung proxy

-- | Erstelle ein neues 'Gleis'.
gleisNew :: (MonadIO m, Spurweite z) => GleisDefinition z -> m (Gleis z)
gleisNew Gerade {länge} = geradeNew länge
gleisNew Kurve {radius, winkel} = kurveNew radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Normal {geradeRichtung = Links}} =
    weicheLinksNew länge radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Normal {geradeRichtung = Rechts}} =
    weicheRechtsNew länge radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Normal {geradeRichtung = Dreiwege}} =
    dreiwegeweicheNew länge radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Gebogen {gebogeneRichtung = Links}} =
    kurvenWeicheLinksNew länge radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Gebogen {gebogeneRichtung = Rechts}} =
    kurvenWeicheRechtsNew länge radius winkel
gleisNew
    Kreuzung {länge, radius, winkel, kreuzungsArt} = kreuzungNew länge radius winkel kreuzungsArt

-- | Notwendige Größen zur Charakterisierung eines 'Gleis'es.
--
-- Alle Längenangaben sind in mm (= Pixel mit scale 1).
-- Winkel sind im Bogenmaß (z.B. 90° ist rechter Winkel).
data GleisDefinition (z :: Zugtyp)
    = Gerade { länge :: Double }
    | Kurve { radius :: Double, winkel :: Double }
    | Weiche { länge :: Double, radius :: Double, winkel :: Double, richtung :: WeichenRichtung }
    | Kreuzung
      { länge :: Double, radius :: Double, winkel :: Double, kreuzungsArt :: KreuzungsArt }

data WeichenArt
    = WeicheZweiweg
    | WeicheDreiweg

data WeichenRichtungAllgemein (a :: WeichenArt) where
    Links :: WeichenRichtungAllgemein a
    Rechts :: WeichenRichtungAllgemein a
    Dreiwege :: WeichenRichtungAllgemein 'WeicheDreiweg

alsDreiweg :: WeichenRichtungAllgemein a -> WeichenRichtungAllgemein 'WeicheDreiweg
alsDreiweg Links = Links
alsDreiweg Rechts = Rechts
alsDreiweg Dreiwege = Dreiwege

data WeichenRichtung
    = Normal { geradeRichtung :: WeichenRichtungAllgemein 'WeicheDreiweg }
    | Gebogen { gebogeneRichtung :: WeichenRichtungAllgemein 'WeicheZweiweg }

data KreuzungsArt
    = MitKurve
    | OhneKurve
    deriving (Eq, Show)

-- | Alle 'AnchorPoint's einer 'GleisDefinition'.
getAnchorPoints :: forall z. (Spurweite z) => GleisDefinition z -> HashMap AnchorName AnchorPoint
getAnchorPoints = \case
    Gerade {länge} -> anchorPointsGerade länge proxy
    Kurve {radius, winkel = ((pi / 180 *) -> winkelBogenmaß)}
        -> anchorPointsKurve radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Normal Rechts}
        -> anchorPointsWeicheRechts länge radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Normal Links}
        -> anchorPointsWeicheLinks länge radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Normal Dreiwege}
        -> anchorPointsDreiwegeweiche länge radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Gebogen Links}
        -> anchorPointsKurvenWeicheRechts länge radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Gebogen Rechts}
        -> anchorPointsKurvenWeicheLinks länge radius winkelBogenmaß proxy
    Kreuzung {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß)}
        -> anchorPointsKreuzung länge radius winkelBogenmaß proxy
    where
        proxy :: Proxy z
        proxy = Proxy

-- | Postion auf einer 'Gleisanzeige'.
--
-- /x/-Koordinate wächst nach rechts.
-- /y/-Koordinate wächst nach unten.
-- /winkel/ werden im Gradmaß übergeben und beschreiben eine Rotation im Uhrzeigersinn.
data Position = Position { x :: Double, y :: Double, winkel :: Double }
    deriving (Eq, Ord)

data GleisAnzeige (z :: Zugtyp) =
    GleisAnzeige
    { fixed :: Gtk.Fixed
    , tvarScale :: TVar Double
    , tvarGleise :: TVar (HashMap (Gleis z) Position)
    , tvarLabel :: TVar [(Gtk.Label, Position)]
    , tvarAnchorPoints :: TVar AnchorPointRTree
    }

instance MitWidget (GleisAnzeige z) where
    erhalteWidget :: (MonadIO m) => GleisAnzeige z -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . fixed

gleisAnzeigeNew :: (MonadIO m) => m (GleisAnzeige z)
gleisAnzeigeNew =
    liftIO
    $ GleisAnzeige <$> Gtk.fixedNew
    <*> newTVarIO 1
    <*> newTVarIO HashMap.empty
    <*> newTVarIO []
    <*> newTVarIO RTree.empty

fixedSetChildTransformation
    :: (MonadIO m, Gtk.IsWidget w) => Gtk.Fixed -> w -> Position -> Double -> m ()
fixedSetChildTransformation fixed child Position {x, y, winkel} scale = liftIO $ do
    -- FIXME debug message since it refuses to work atm
    -- putStrLn $ "(" ++ show x ++ ", " ++ show y ++ ": " ++ show winkel ++ ") * " ++ show scale
    let fScale = realToFrac scale
        fWinkel = realToFrac winkel
        scaledX = realToFrac $ scale * x
        scaledY = realToFrac $ scale * y
    transform0 <- Gsk.transformNew
    point <- Graphene.newZeroPoint
    Graphene.setPointX point scaledX
    Graphene.setPointY point scaledY
    transform1 <- Gsk.transformTranslate transform0 point
    transform2 <- Gsk.transformScale transform1 fScale fScale
    transform3 <- Gsk.transformRotate transform2 fWinkel
    Gtk.fixedSetChildTransform fixed child $ Just transform3

-- TODO split Model and View/Controller?
-- speichere nur [GleisDefinition, Position, Connection] und übergebe diese an View
-- | Bewege ein 'Gleis' zur angestrebten 'Position' einer 'GleisAnzeige'.
--
-- Wenn ein 'Gleis' kein Teil der 'GleisAnzeige' war wird es neu hinzugefügt.
gleisPut :: (MonadIO m) => GleisAnzeige z -> Gleis z -> Position -> m ()
gleisPut
    GleisAnzeige {fixed, tvarScale, tvarGleise, tvarAnchorPoints}
    gleis@Gleis {drawingArea, tmvarAnchorPoints}
    position = liftIO $ do
    (scale, gleise, drawingAreas) <- atomically $ do
        scale <- readTVar tvarScale
        gleise <- readTVar tvarGleise
        writeTVar tvarGleise $! HashMap.insert gleis position gleise
        -- make sure every gleis knows about other AnchorPoints
        tryPutTMVar tmvarAnchorPoints tvarAnchorPoints
        -- TODO reset connected anchor points
        drawingAreas <- _TODO
        pure (scale, gleise, drawingAreas)
    -- Queue re-draw for previously connected gleise
    forM_ drawingAreas Gtk.widgetQueueDraw
    when (isNothing $ HashMap.lookup gleis gleise)
        $ Gtk.widgetInsertAfter drawingArea fixed (Nothing :: Maybe Gtk.Widget)
    fixedSetChildTransformation fixed drawingArea position scale

-- | Bewege /gleisA/ neben /gleisB/, so dass /anchorNameA/ direkt neben /anchorNameB/ liegt.
--
-- Der Rückgabewert signalisiert ob das anfügen erfolgreich wahr. Mögliche Fehlerquellen:
-- * /gleisB/ ist kein Teil der 'GleisAnzeige'
gleisAttach
    :: (MonadIO m) => GleisAnzeige z -> Gleis z -> AnchorName -> Gleis z -> AnchorName -> m Bool
gleisAttach
    gleisAnzeige@GleisAnzeige {tvarGleise}
    gleisA@Gleis {anchorPoints = anchorPointsA}
    anchorNameA
    gleisB@Gleis {anchorPoints = anchorPointsB}
    anchorNameB = liftIO $ do
    (gleise, anchorPointsA, anchorPointsB) <- atomically $ do
        gleise <- readTVar tvarGleise
        let maybePosition = case HashMap.lookup gleisB gleise of
                Nothing -> Nothing
                (Just Position {x = xB, y = yB, winkel = winkelB}) -> case HashMap.lookup
                    anchorNameA
                    anchorPointsB of
                    (Just
                         AnchorPoint { anchorX = anchorXB
                                     , anchorY = anchorYB
                                     , anchorVX = anchorVXB
                                     , anchorVY = anchorVYB}) -> case HashMap.lookup
                        anchorNameA
                        anchorPointsA of
                        (Just AnchorPoint {anchorVX = anchorVXA, anchorVY = anchorVYA}) -> Just
                            Position
                            { x = xB + anchorXB
                            , y = yB + anchorYB
                            , winkel = winkelB
                                  + 180 / pi
                                  * (winkelMitXAchse (-anchorVXB) (-anchorVYB)
                                     - winkelMitXAchse anchorVXA anchorVYA)
                            }
                            where
                                -- Winkel im Bogenmaß zwischen Vektor und x-Achse
                                -- steigt im Uhrzeigersinn
                                winkelMitXAchse :: Double -> Double -> Double
                                winkelMitXAchse vx vy =
                                    if
                                        | vx > 0 && vy < 0 -> 1.5 * pi
                                            + acos (vx / (vx * vx + vy * vy))
                                        | vx < 0 && vy < 0 -> pi + acos (vx / (vx * vx + vy * vy))
                                        | vx < 0 && vy > 0 -> 0.5 * pi
                                            + acos (vx / (vx * vx + vy * vy))
                                        | otherwise -> acos $ vx / (vx * vx + vy * vy)
                        _otherwise -> Nothing
                    _otherwise -> Nothing
        -- TODO mark both anchors
        pure (gleise, anchorPointsA, anchorPointsB)
    -- TODO gleisPut, ohne reset der AnchorPoints
    error "_TODO"   --TODO

-- | Entferne ein 'Gleis' aus der 'GleisAnzeige'.
--
-- 'Gleis'e die kein Teil der 'GleisAnzeige' sind werden stillschweigend ignoriert.
gleisRemove :: (MonadIO m) => GleisAnzeige z -> Gleis z -> m ()
gleisRemove
    GleisAnzeige {fixed, tvarGleise, tvarAnchorPoints}
    gleis@Gleis {drawingArea, tmvarAnchorPoints} = liftIO $ do
    (gleise, drawingAreas) <- atomically $ do
        gleise <- readTVar tvarGleise
        writeTVar tvarGleise $! HashMap.delete gleis gleise
        -- TODO get connected anchorPoints
        drawingAreas <- _TODO
        pure (gleise, drawingAreas)
    when (isJust $ HashMap.lookup gleis gleise) $ Gtk.fixedRemove fixed drawingArea
    forM_ drawingAreas Gtk.widgetQueueDraw

-- | Bewege ein 'Gtk.Label' zur angestrebten 'Position' einer 'GleisAnzeige'.
--
-- Wenn ein 'Gtk.Label' kein Teil der 'GleisAnzeige' war wird es neu hinzugefügt.
gleisAnzeigePutLabel :: (MonadIO m) => GleisAnzeige z -> Gtk.Label -> Position -> m ()
gleisAnzeigePutLabel GleisAnzeige {fixed, tvarScale, tvarLabel} label position = liftIO $ do
    (scale, bekannteLabel) <- atomically $ (,) <$> readTVar tvarScale <*> readTVar tvarLabel
    restLabel <- case partition ((== label) . fst) bekannteLabel of
        ([], alleLabel) -> do
            -- Gtk.widgetInsertAfter label fixed (Nothing :: Maybe Gtk.Widget)
            Gtk.fixedPut fixed label 0 0
            pure alleLabel
        (_labelVorkommen, andereLabel) -> pure andereLabel
    atomically $ writeTVar tvarLabel $ (label, position) : restLabel
    fixedSetChildTransformation fixed label position scale

-- | Entferne ein 'Gtk.Label' aus der 'GleisAnzeige'.
--
-- 'Gtk.Label' die kein Teil der 'GleisAnzeige' sind werden stillschweigend ignoriert.
gleisAnzeigeRemoveLabel :: (MonadIO m) => GleisAnzeige z -> Gtk.Label -> m ()
gleisAnzeigeRemoveLabel GleisAnzeige {fixed, tvarLabel} label = liftIO $ do
    bekannteLabel <- readTVarIO tvarLabel
    case partition ((== label) . fst) bekannteLabel of
        ([], _alleGleise) -> pure ()
        (_gleisVorkommen, andereLabel) -> do
            Gtk.fixedRemove fixed label
            atomically $ writeTVar tvarLabel andereLabel

-- https://blog.gtk.org/2019/05/08/gtk-3-96-0/
-- use 'fixedSetChildTransform' with a 'Gsk.Transform'
-- might completely remove the necessity to scale/rotate drawings myself
-- | Skaliere eine 'GleisAnzeige'.
gleisAnzeigeScale :: (MonadIO m) => GleisAnzeige z -> Double -> m ()
gleisAnzeigeScale GleisAnzeige {fixed, tvarScale, tvarGleise, tvarLabel} scale = liftIO $ do
    atomically $ writeTVar tvarScale scale
    (gleise, bekannteLabel) <- atomically $ (,) <$> readTVar tvarGleise <*> readTVar tvarLabel
    forM_ (HashMap.toList gleise) $ \(Gleis {drawingArea}, position)
        -> fixedSetChildTransformation fixed drawingArea position scale
    forM_ bekannteLabel
        $ \(label, position) -> fixedSetChildTransformation fixed label position scale
