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
{-# LANGUAGE TupleSections #-}
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
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.HashMap.Strict (HashMap())
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable(hashWithSalt))
import Data.Int (Int32)
import Data.List (partition, foldl')
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
import Zug.UI.Gtk.Gleis.Anchor (AnchorPoint(..), AnchorName(..), AnchorPointMap, AnchorPointRTree)
import Zug.UI.Gtk.Gleis.Gerade (zeichneGerade, anchorPointsGerade, widthGerade, heightGerade)
import Zug.UI.Gtk.Gleis.Kreuzung
       (zeichneKreuzung, anchorPointsKreuzung, widthKreuzung, heightKreuzung, KreuzungsArt(..))
import Zug.UI.Gtk.Gleis.Kurve (zeichneKurve, anchorPointsKurve, widthKurve, heightKurve)
import Zug.UI.Gtk.Gleis.Spurweite (Spurweite(..), radiusBegrenzung)
import Zug.UI.Gtk.Gleis.Weiche
       (zeichneWeicheRechts, anchorPointsWeicheRechts, zeichneWeicheLinks, anchorPointsWeicheLinks, widthWeiche
      , heightWeiche, zeichneDreiwegeweiche, anchorPointsDreiwegeweiche, widthDreiwegeweiche, heightDreiwegeweiche)
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget))

-- import Zug.UI.Gtk.Hilfsfunktionen (fixedPutWidgetNew)
-- | 'Gtk.Widget' von einem Gleis.
-- Die Größe wird nur über 'gleisScale', 'gleisSetWidth' und 'gleisSetHeight' verändert.
data Gleis (z :: Zugtyp) =
    Gleis
    { drawingArea :: Gtk.DrawingArea
    , width :: Int32
    , height :: Int32
    , anchorPoints :: AnchorPointMap
    , tmvarParentInformation :: TMVar (TVar AnchorPointRTree, TVar (HashMap (Gleis z) Position))
    }
    deriving (Eq)

instance MitWidget (Gleis z) where
    erhalteWidget :: (MonadIO m) => Gleis z -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . drawingArea

instance Hashable (Gleis z) where
    hashWithSalt :: Int -> Gleis z -> Int
    hashWithSalt salt Gleis {width, height, anchorPoints} =
        hashWithSalt salt (width, height, anchorPoints)

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
                  -> (Proxy z -> AnchorPointMap)
                  -> (Proxy z -> Cairo.Render ())
                  -> m (Gleis z)
createGleisWidget widthFn heightFn anchorPointsFn draw = do
    tmvarParentInformation <- newEmptyTMVarIO
    let proxy :: Proxy z
        proxy = Proxy
        width :: Int32
        width = widthFn proxy
        height :: Int32
        height = heightFn proxy
        anchorPoints :: AnchorPointMap
        anchorPoints = anchorPointsFn proxy
    drawingArea <- Gtk.drawingAreaNew
    Gtk.widgetSetHexpand drawingArea False
    Gtk.widgetSetHalign drawingArea Gtk.AlignStart
    Gtk.widgetSetVexpand drawingArea False
    Gtk.widgetSetValign drawingArea Gtk.AlignStart
    let gleis = Gleis { drawingArea, width, height, anchorPoints, tmvarParentInformation }
    Gtk.drawingAreaSetContentHeight drawingArea height
    Gtk.drawingAreaSetContentWidth drawingArea width
    Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ \_drawingArea context newWidth newHeight
        -> void $ flip Cairo.renderWithContext context $ do
            Cairo.save
            Cairo.setLineWidth 1
            Cairo.newPath
            draw proxy
            Cairo.stroke
            Cairo.restore
            -- mark anchor points
            Cairo.save
            Cairo.setLineWidth 1
            currentParentInformation
                <- liftIO $ atomically $ tryReadTMVar tmvarParentInformation >>= \case
                    (Just (tvarAnchorPoints, tvarGleise)) -> do
                        gleise <- readTVar tvarGleise
                        case HashMap.lookup gleis gleise of
                            (Just position) -> (, position) <$> readTVar tvarAnchorPoints
                            Nothing -> pure Nothing
                    Nothing -> pure Nothing
            currentParentInformation <- liftIO $ atomically $ runMaybeT $ do
                (tvarAnchorPoints, tvarGleise) <- MaybeT $ tryReadTMVar tmvarParentInformation
                position <- MaybeT $ HashMap.lookup gleis <$> readTVar tvarGleise
                fmap (, position) $ lift $ readTVar tvarAnchorPoints
            case currentParentInformation of
                (Just (knownAnchorPoints, position)) -> forM_ anchorPoints
                    $ \AnchorPoint {anchorX, anchorY, anchorVX, anchorVY} -> do
                        Cairo.moveTo anchorX anchorY
                        let intersections = mbb anchorX anchorY `RTree.intersect` knownAnchorPoints
                            (r, g, b) =
                                if length intersections > 1
                                    then (0, 1, 0)
                                    else (0, 0, 1)
                            len = anchorVX * anchorVX + anchorVY * anchorVY
                        Cairo.setSourceRGB r g b
                        Cairo.relLineTo (-5 * anchorVX / len) (-5 * anchorVY / len)
                        Cairo.stroke
                Nothing -> pure ()
            Cairo.restore
            pure True
    pure gleis

-- | Erzeuge eine neues gerades 'Gleis' der angegebenen Länge.
geradeNew :: (MonadIO m, Spurweite z) => Double -> m (Gleis z)
geradeNew länge =
    createGleisWidget (widthGerade länge) heightGerade (anchorPointsGerade länge)
    $ zeichneGerade länge

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

weicheLinksNew :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
weicheLinksNew länge radius winkel =
    createGleisWidget
        (widthWeiche länge radius winkelBogenmaß)
        (heightWeiche radius winkelBogenmaß)
        (anchorPointsWeicheLinks länge radius winkelBogenmaß)
    $ zeichneWeicheLinks länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

dreiwegeweicheNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
dreiwegeweicheNew länge radius winkel =
    createGleisWidget
        (widthDreiwegeweiche länge radius winkelBogenmaß)
        (heightDreiwegeweiche radius winkelBogenmaß)
        (anchorPointsDreiwegeweiche länge radius winkelBogenmaß)
    $ zeichneDreiwegeweiche länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

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

kurvenWeicheLinksNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
kurvenWeicheLinksNew länge radius winkel =
    createGleisWidget
        (widthKurvenWeiche länge radius winkelBogenmaß)
        (heightKurvenWeiche radius winkelBogenmaß)
        (anchorPointsKurvenWeicheLinks länge radius winkelBogenmaß)
    $ zeichneKurvenWeicheLinks länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

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
    $ zeichneKreuzung länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

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

-- | Alle 'AnchorPoint's einer 'GleisDefinition'.
getAnchorPoints :: forall z. (Spurweite z) => GleisDefinition z -> AnchorPointMap
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

-- | Bewege ein 'Gleis' zur angestrebten 'Position' einer 'GleisAnzeige'.
--
-- Wenn ein 'Gleis' kein Teil der 'GleisAnzeige' war wird es neu hinzugefügt.
gleisPut :: (MonadIO m) => GleisAnzeige z -> Gleis z -> Position -> m ()
gleisPut
    GleisAnzeige {fixed, tvarScale, tvarGleise, tvarAnchorPoints}
    gleis@Gleis {drawingArea, tmvarParentInformation}
    position = liftIO $ do
    (scale, isNew, drawingAreas) <- atomically $ do
        scale <- readTVar tvarScale
        gleise <- readTVar tvarGleise
        writeTVar tvarGleise $! HashMap.insert gleis position gleise
        -- make sure every gleis knows about other AnchorPoints
        tryPutTMVar tmvarAnchorPoints tvarAnchorPoints
        -- TODO reset connected anchor points
        drawingAreas <- case HashMap.lookup gleis gleise of
            (Just oldPosition) -> _TODO
            Nothing -> pure []
        -- TODO add new anchor points
        pure (scale, isNothing $ HashMap.lookup gleis gleise, drawingAreas)
    -- Queue re-draw for previously connected gleise
    forM_ drawingAreas Gtk.widgetQueueDraw
    when isNew $ Gtk.widgetInsertAfter drawingArea fixed (Nothing :: Maybe Gtk.Widget)
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
    gleis@Gleis {drawingArea, tmvarParentInformation} = liftIO $ do
    (isChild, drawingAreas) <- atomically $ do
        gleise <- readTVar tvarGleise
        writeTVar tvarGleise $! HashMap.delete gleis gleise
        -- TODO reset connected anchorPoints
        drawingAreas <- _TODO
        pure (isJust $ HashMap.lookup gleis gleise, drawingAreas)
    forM_ drawingAreas Gtk.widgetQueueDraw
    when isChild $ Gtk.fixedRemove fixed drawingArea

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
