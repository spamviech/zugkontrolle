{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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
    addIdle for some reason move to gi-glib
Gtk.Application has to be used instead of Gtk.main
    https://hackage.haskell.org/package/gi-gtk-4.0.2/docs/GI-Gtk-Objects-Application.html
    startup/activate-signals are in gi-gio
    https://hackage.haskell.org/package/gi-gio-2.0.27/docs/GI-Gio-Objects-Application.html#v:applicationSetResourceBasePath
AspectFrame doesn't draw a frame around the child, so might be useful here
    doesn't pass scaling information to DrawingArea
    usefulness questionable anyway due to rotation requirement
Assistant should work again
Scrolling, Drag&Drop use EventController, added using 'Gtk.widgetAddController'
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-Widget.html#g:method:addController
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-EventControllerScroll.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-DragSource.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-DragIcon.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-DropTarget.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-DropTargetAsync.html
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
    -- * Anzeige
  , GleisAnzeige()
  , Position(..)
    -- ** Konstruktor
  , gleisAnzeigeNew
    -- ** Gleis platzieren
  , gleisPut
  , gleisAttach
  , AttachError
  , gleisRemove
  , gleisAnzeigePutLabel
  , gleisAnzeigeRemoveLabel
  , gleisAnzeigeScale
    -- ** Speichern / Laden
  , gleisAnzeigeSave
  , Binary(..)
  ) where

import Control.Concurrent.STM (STM, atomically, TVar, newTVarIO, readTVarIO, readTVar, writeTVar
                             , TMVar, newEmptyTMVarIO, tryReadTMVar, tryPutTMVar, tryTakeTMVar)
import Control.Monad (when, void, forM_, forM)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Bifunctor (first)
import Data.Binary (Binary())
import qualified Data.Binary as Binary
import Data.HashMap.Strict (HashMap())
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable(hashWithSalt))
import Data.Int (Int32)
import Data.List (partition, foldl')
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy(Proxy))
import qualified Data.RTree as RTree
import Data.Text (Text)
import GHC.Generics (Generic())
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Graphene as Graphene
import qualified GI.Gsk as Gsk
import qualified GI.Gtk as Gtk

import Zug.Enums (Zugtyp(..))
import Zug.UI.Gtk.Gleis.Anchor
       (AnchorPoint(..), AnchorPosition(..), AnchorDirection(..), AnchorName(..), AnchorPointMap
      , AnchorPointRTree, mbbSearch, mbbPoint)
import Zug.UI.Gtk.Gleis.Gerade (zeichneGerade, anchorPointsGerade, widthGerade, heightGerade)
import Zug.UI.Gtk.Gleis.Kreuzung
       (zeichneKreuzung, anchorPointsKreuzung, widthKreuzung, heightKreuzung, KreuzungsArt(..))
import Zug.UI.Gtk.Gleis.Kurve (zeichneKurve, anchorPointsKurve, widthKurve, heightKurve
                             , KurvenBeschränkung(AlleBeschränkungen))
import Zug.UI.Gtk.Gleis.Spurweite (Spurweite(..))
import Zug.UI.Gtk.Gleis.Weiche
       (zeichneWeicheRechts, anchorPointsWeicheRechts, zeichneWeicheLinks, anchorPointsWeicheLinks
      , widthWeiche, heightWeiche, zeichneDreiwegeweiche, anchorPointsDreiwegeweiche
      , widthDreiwegeweiche, heightDreiwegeweiche, widthKurvenWeiche, heightKurvenWeiche
      , zeichneKurvenWeicheRechts, anchorPointsKurvenWeicheRechts, zeichneKurvenWeicheLinks
      , anchorPointsKurvenWeicheLinks)
import Zug.UI.Gtk.MitWidget (MitWidget(erhalteWidget))

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
    , definition :: GleisDefinition z
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

translateAnchorPoint :: Position -> AnchorPosition -> (Double, Double)
translateAnchorPoint Position {x, y, winkel} AnchorPosition {anchorX, anchorY} = (fixedX, fixedY)
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi / 180 * winkel

        fixedX :: Double
        fixedX = x + anchorX * cos winkelBogenmaß - anchorY * sin winkelBogenmaß

        fixedY :: Double
        fixedY = y + anchorX * sin winkelBogenmaß + anchorY * cos winkelBogenmaß

intersections :: Position -> AnchorPoint -> AnchorPointRTree -> [Gtk.DrawingArea]
intersections position anchorPoint knownAnchorPoints =
    concatMap NonEmpty.toList
    $ uncurry mbbSearch (translateAnchorPoint position $ anchorPosition anchorPoint)
    `RTree.intersect` knownAnchorPoints

-- | Create a new 'Gtk.DrawingArea' with a fixed size set up with the specified 'Cairo.Render' /draw/ path.
--
-- 'Cairo.setLineWidth' 1 is called before the /draw/ action is executed.
-- After the action 'Cairo.stroke' is executed.
gleisNew :: forall m z. (MonadIO m, Spurweite z) => GleisDefinition z -> m (Gleis z)
gleisNew definition = do
    tmvarParentInformation <- liftIO newEmptyTMVarIO
    drawingArea <- Gtk.drawingAreaNew
    Gtk.widgetSetHexpand drawingArea False
    Gtk.widgetSetHalign drawingArea Gtk.AlignStart
    Gtk.widgetSetVexpand drawingArea False
    Gtk.widgetSetValign drawingArea Gtk.AlignStart
    let gleis =
            Gleis { drawingArea, width, height, anchorPoints, tmvarParentInformation, definition }
    Gtk.drawingAreaSetContentHeight drawingArea height
    Gtk.drawingAreaSetContentWidth drawingArea width
    Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ \_drawingArea context _newWidth _newHeight
        -> void $ flip Cairo.renderWithContext context $ do
            Cairo.save
            Cairo.setLineWidth 1
            Cairo.newPath
            zeichnen
            Cairo.stroke
            Cairo.restore
            -- mark anchor points
            Cairo.save
            Cairo.setLineWidth 1
            currentParentInformation <- liftIO $ atomically $ runMaybeT $ do
                (tvarAnchorPoints, tvarGleise) <- MaybeT $ tryReadTMVar tmvarParentInformation
                position <- MaybeT $ HashMap.lookup gleis <$> readTVar tvarGleise
                fmap (, position) $ lift $ readTVar tvarAnchorPoints
            forM_ anchorPoints
                $ \anchorPoint@AnchorPoint
                { anchorPosition = AnchorPosition {anchorX, anchorY}
                , anchorDirection = AnchorDirection {anchorDX, anchorDY}} -> do
                    Cairo.moveTo anchorX anchorY
                    let r, g, b :: Double
                        (r, g, b) = case currentParentInformation of
                            (Just (knownAnchorPoints, position))
                                | any (/= drawingArea)
                                    $ intersections position anchorPoint knownAnchorPoints
                                    -> (0, 1, 0)
                                | otherwise -> (0, 0, 1)
                            Nothing -> (1, 0, 0)
                        len :: Double
                        len = sqrt $ anchorDX * anchorDX + anchorDY * anchorDY
                    Cairo.setSourceRGB r g b
                    Cairo.relLineTo (-5 * anchorDX / len) (-5 * anchorDY / len)
                    Cairo.stroke
            Cairo.restore
            pure True
    pure gleis
    where
        width :: Int32
        width = getWidth definition

        height :: Int32
        height = getHeight definition

        anchorPoints :: AnchorPointMap
        anchorPoints = getAnchorPoints definition

        zeichnen :: Cairo.Render ()
        zeichnen = getZeichnen definition

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
    deriving (Eq, Show, Generic)

instance Binary (GleisDefinition z)

data WeichenArt
    = WeicheZweiweg
    | WeicheDreiweg
    deriving (Eq, Show)

data WeichenRichtungAllgemein (a :: WeichenArt) where
    Links :: WeichenRichtungAllgemein a
    Rechts :: WeichenRichtungAllgemein a
    Dreiwege :: WeichenRichtungAllgemein 'WeicheDreiweg

deriving instance Eq (WeichenRichtungAllgemein a)

deriving instance Show (WeichenRichtungAllgemein a)

instance Binary (WeichenRichtungAllgemein 'WeicheZweiweg) where
    put :: WeichenRichtungAllgemein 'WeicheZweiweg -> Binary.Put
    put Links = Binary.putWord8 0
    put Rechts = Binary.putWord8 1

    get :: Binary.Get (WeichenRichtungAllgemein 'WeicheZweiweg)
    get = Binary.getWord8 >>= \case
        0 -> pure Links
        1 -> pure Rechts
        2 -> fail "Unbekannte Zweiweg-WeichenRichtung: Dreiwege"
        n -> fail $ "Unbekannte Zweiweg-WeichenRichtung: " <> show n

instance Binary (WeichenRichtungAllgemein 'WeicheDreiweg) where
    put :: WeichenRichtungAllgemein 'WeicheDreiweg -> Binary.Put
    put Links = Binary.putWord8 0
    put Rechts = Binary.putWord8 1
    put Dreiwege = Binary.putWord8 2

    get :: Binary.Get (WeichenRichtungAllgemein 'WeicheDreiweg)
    get = Binary.getWord8 >>= \case
        0 -> pure Links
        1 -> pure Rechts
        2 -> pure Dreiwege
        n -> fail $ "Unbekannte Dreiweg-WeichenRichtung: " <> show n

alsDreiweg :: WeichenRichtungAllgemein a -> WeichenRichtungAllgemein 'WeicheDreiweg
alsDreiweg Links = Links
alsDreiweg Rechts = Rechts
alsDreiweg Dreiwege = Dreiwege

data WeichenRichtung
    = Normal { geradeRichtung :: WeichenRichtungAllgemein 'WeicheDreiweg }
    | Gebogen { gebogeneRichtung :: WeichenRichtungAllgemein 'WeicheZweiweg }
    deriving (Eq, Show, Generic)

instance Binary WeichenRichtung

-- TODO getGleisAttribute :: GleisDefinition z -> GleisAttribute
-- data GleisAttribute = GleisAttribute {width, height, anchorPoints, zeichnen}
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
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Gebogen Rechts}
        -> anchorPointsKurvenWeicheRechts länge radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Gebogen Links}
        -> anchorPointsKurvenWeicheLinks länge radius winkelBogenmaß proxy
    Kreuzung {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß)}
        -> anchorPointsKreuzung länge radius winkelBogenmaß proxy
    where
        proxy :: Proxy z
        proxy = Proxy

-- | Breite des zugehörigen 'Gleis'es einer 'GleisDefinition'.
getWidth :: forall z. (Spurweite z) => GleisDefinition z -> Int32
getWidth = \case
    Gerade {länge} -> widthGerade länge proxy
    Kurve {radius, winkel = ((pi / 180 *) -> winkelBogenmaß)}
        -> widthKurve radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Normal Dreiwege}
        -> widthDreiwegeweiche länge radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Normal _rl}
        -> widthWeiche länge radius winkelBogenmaß proxy
    Weiche {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Gebogen _rl}
        -> widthKurvenWeiche länge radius winkelBogenmaß proxy
    Kreuzung {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß)}
        -> widthKreuzung länge radius winkelBogenmaß proxy
    where
        proxy :: Proxy z
        proxy = Proxy

-- | Höhe des zugehörigen 'Gleis'es einer 'GleisDefinition'.
getHeight :: forall z. (Spurweite z) => GleisDefinition z -> Int32
getHeight = \case
    Gerade {} -> heightGerade proxy
    Kurve {radius, winkel = ((pi / 180 *) -> winkelBogenmaß)}
        -> heightKurve radius winkelBogenmaß proxy
    Weiche {radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Normal Dreiwege}
        -> heightDreiwegeweiche radius winkelBogenmaß proxy
    Weiche {radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Normal _rl}
        -> heightWeiche radius winkelBogenmaß proxy
    Weiche {radius, winkel = ((pi / 180 *) -> winkelBogenmaß), richtung = Gebogen _rl}
        -> heightKurvenWeiche radius winkelBogenmaß proxy
    Kreuzung {radius, winkel = ((pi / 180 *) -> winkelBogenmaß)}
        -> heightKreuzung radius winkelBogenmaß proxy
    where
        proxy :: Proxy z
        proxy = Proxy

-- | Erstelle ein neues 'Gleis'.
getZeichnen :: forall z. (Spurweite z) => GleisDefinition z -> Cairo.Render ()
getZeichnen = \case
    Gerade {länge} -> zeichneGerade länge proxy
    Kurve {radius, winkel = ((pi / 180 *) -> winkelBogenmaß)}
        -> zeichneKurve radius winkelBogenmaß AlleBeschränkungen proxy
    Weiche { länge
           , radius
           , winkel = ((pi / 180 *) -> winkelBogenmaß)
           , richtung = Normal {geradeRichtung = Links}}
        -> zeichneWeicheLinks länge radius winkelBogenmaß proxy
    Weiche { länge
           , radius
           , winkel = ((pi / 180 *) -> winkelBogenmaß)
           , richtung = Normal {geradeRichtung = Rechts}}
        -> zeichneWeicheRechts länge radius winkelBogenmaß proxy
    Weiche { länge
           , radius
           , winkel = ((pi / 180 *) -> winkelBogenmaß)
           , richtung = Normal {geradeRichtung = Dreiwege}}
        -> zeichneDreiwegeweiche länge radius winkelBogenmaß proxy
    Weiche { länge
           , radius
           , winkel = ((pi / 180 *) -> winkelBogenmaß)
           , richtung = Gebogen {gebogeneRichtung = Rechts}}
        -> zeichneKurvenWeicheRechts länge radius winkelBogenmaß proxy
    Weiche { länge
           , radius
           , winkel = ((pi / 180 *) -> winkelBogenmaß)
           , richtung = Gebogen {gebogeneRichtung = Links}}
        -> zeichneKurvenWeicheLinks länge radius winkelBogenmaß proxy
    Kreuzung {länge, radius, winkel = ((pi / 180 *) -> winkelBogenmaß), kreuzungsArt}
        -> zeichneKreuzung länge radius winkelBogenmaß kreuzungsArt proxy
    where
        proxy :: Proxy z
        proxy = Proxy

-- | Postion auf einer 'GleisAnzeige'.
--
-- /x/-Koordinate wächst nach rechts.
-- /y/-Koordinate wächst nach unten.
-- /winkel/ werden im Gradmaß übergeben und beschreiben eine Rotation im Uhrzeigersinn.
data Position = Position { x :: Double, y :: Double, winkel :: Double }
    deriving (Eq, Show, Ord, Generic)

instance Binary Position

-- https://hackage.haskell.org/package/gi-pangocairo-1.0.24/docs/GI-PangoCairo-Functions.html#v:createLayout
-- https://hackage.haskell.org/package/gi-pango-1.0.23/docs/GI-Pango-Objects-Layout.html#v:layoutSetText
data GleisAnzeigeDrawingArea (z :: Zugtyp) =
    GleisAnzeigeDrawingArea
    { fixed :: Gtk.Fixed
    , tvarScale :: TVar Double
    , tvarWindow :: TVar (Double, Double) -- ^ x, y; width, height from size
    , tvarGleise :: TVar (HashMap (GleisDefinition z) Position)
    , tvarLabel :: TVar [(Text, Position)]
    , tvarAnchorPoints :: TVar AnchorPointRTree
    }

-- TODO add possibility to move shown widget portion (from x,y -> to x,y)
-- i.e. using a 'Gtk.ScrolledWindow' without shown scrollbar (Gtk.scrolledWindowSetPolicy)
-- disabling 'Gtk.scrolledWindowSetKineticScrolling' is probably desired
data GleisAnzeige (z :: Zugtyp) =
    GleisAnzeige
    { scrolledWindow :: Gtk.ScrolledWindow
    , hAdjustment :: Gtk.Adjustment
    , vAdjustment :: Gtk.Adjustment
    , fixed :: Gtk.Fixed
    , tvarScale :: TVar Double
    , tvarGleise :: TVar (HashMap (Gleis z) Position)
    , tvarLabel :: TVar [(Gtk.Label, Position)]
    , tvarAnchorPoints :: TVar AnchorPointRTree
    }
    deriving (Eq)

instance MitWidget (GleisAnzeige z) where
    erhalteWidget :: (MonadIO m) => GleisAnzeige z -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . scrolledWindow

gleisAnzeigeNew :: (MonadIO m) => m (GleisAnzeige z)
gleisAnzeigeNew = liftIO $ do
    {-
    hAdjustment <- Gtk.adjustmentNew 0 0 1 0.1 1 1
    vAdjustment <- Gtk.adjustmentNew 0 0 1 0.1 1 1
    viewport <- Gtk.viewportNew (Just hAdjustment) (Just vAdjustment)
    Gtk.viewportSetScrollToFocus viewport True
    fixed <- Gtk.fixedNew
    Gtk.viewportSetChild viewport $ Just fixed
    --}
    --{-
    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetKineticScrolling scrolledWindow False
    Gtk.scrolledWindowSetPolicy scrolledWindow Gtk.PolicyTypeExternal Gtk.PolicyTypeExternal
    hAdjustment <- Gtk.scrolledWindowGetHadjustment scrolledWindow
    Gtk.adjustmentConfigure hAdjustment 0 0 1 0.1 1 1
    vAdjustment <- Gtk.scrolledWindowGetVadjustment scrolledWindow
    Gtk.adjustmentConfigure vAdjustment 0 0 1 0.1 1 1
    fixed <- Gtk.fixedNew
    Gtk.scrolledWindowSetChild scrolledWindow $ Just fixed
    --}
    Gtk.widgetSetMarginTop fixed margin
    Gtk.widgetSetMarginBottom fixed margin
    Gtk.widgetSetMarginStart fixed margin
    Gtk.widgetSetMarginEnd fixed margin
    tvarScale <- newTVarIO 1
    tvarGleise <- newTVarIO HashMap.empty
    tvarLabel <- newTVarIO []
    tvarAnchorPoints <- newTVarIO RTree.empty
    pure
        GleisAnzeige
        { scrolledWindow
        , hAdjustment
        , vAdjustment
        , fixed
        , tvarScale
        , tvarGleise
        , tvarLabel
        , tvarAnchorPoints
        }
    where
        margin :: Int32
        margin = 5

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

-- | Remove current 'AnchorPoint' of the 'Gleis' and, if available, move them to the new location.
-- Returns list of 'Gtk.DrawingArea' with close 'AnchorPoint' to old or new location.
moveAnchors :: TVar AnchorPointRTree
            -> Maybe Position
            -> Maybe Position
            -> Gleis z
            -> STM [Gtk.DrawingArea]
moveAnchors tvarAnchorPoints maybeOldPosition maybeNewPosition Gleis {drawingArea, anchorPoints} = do
    knownAnchorPoints <- readTVar tvarAnchorPoints
    let intersectingDrawingAreas :: Maybe Position -> [Gtk.DrawingArea]
        intersectingDrawingAreas Nothing = []
        intersectingDrawingAreas (Just oldPosition) =
            foldl' (\acc anchorPoint -> filter
                        (/= drawingArea)
                        (intersections oldPosition anchorPoint knownAnchorPoints)
                    <> acc) [] anchorPoints
        otherAnchorPoints :: AnchorPointRTree
        otherAnchorPoints =
            RTree.mapMaybe (NonEmpty.nonEmpty . NonEmpty.filter (/= drawingArea)) knownAnchorPoints
        newAnchorPoints :: AnchorPointRTree
        newAnchorPoints = maybe otherAnchorPoints insertAnchorPoints maybeNewPosition
        insertAnchorPoints :: Position -> AnchorPointRTree
        insertAnchorPoints newPosition =
            foldl' (\acc AnchorPoint {anchorPosition} -> RTree.insertWith
                        (<>)
                        (uncurry mbbPoint $ translateAnchorPoint newPosition anchorPosition)
                        (drawingArea :| [])
                        acc) otherAnchorPoints anchorPoints
        newIntersectingDrawingAreas :: Maybe Position -> [Gtk.DrawingArea]
        newIntersectingDrawingAreas Nothing = []
        newIntersectingDrawingAreas (Just newPosition) =
            foldl' (\acc anchorPoint -> filter
                        (/= drawingArea)
                        (intersections newPosition anchorPoint newAnchorPoints)
                    <> acc) [] anchorPoints
    writeTVar tvarAnchorPoints $! newAnchorPoints
    pure
        $ intersectingDrawingAreas maybeOldPosition <> newIntersectingDrawingAreas maybeNewPosition

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
        -- move Position
        gleise <- readTVar tvarGleise
        writeTVar tvarGleise $! HashMap.insert gleis position gleise
        -- make sure every gleis knows about other AnchorPoints/Gleis-Positions
        tryPutTMVar tmvarParentInformation (tvarAnchorPoints, tvarGleise)
        -- move anchor points
        drawingAreas
            <- moveAnchors tvarAnchorPoints (HashMap.lookup gleis gleise) (Just position) gleis
        pure (scale, isNothing $ HashMap.lookup gleis gleise, drawingAreas)
    -- Queue re-draw for previously connected gleise
    forM_ drawingAreas Gtk.widgetQueueDraw
    when isNew $ Gtk.widgetInsertAfter drawingArea fixed (Nothing :: Maybe Gtk.Widget)
    fixedSetChildTransformation fixed drawingArea position scale

data AttachError
    = GleisBNotFount
    | AnchorBNotFound
    | AnchorANotFount
    deriving (Eq, Show)

-- | Bewege /gleisA/ neben /gleisB/, so dass /anchorNameA/ direkt neben /anchorNameB/ liegt.
--
-- Der Rückgabewert signalisiert ob das anfügen erfolgreich wahr. Mögliche Fehlerquellen:
-- * /gleisB/ ist kein Teil der 'GleisAnzeige'.
-- * /anchorB/ ist kein 'AnchorPoint' von /gleisB/.
-- * /anchorA/ ist kein 'AnchorPoint' von /gleisA/.
gleisAttach :: (MonadIO m)
            => GleisAnzeige z
            -> Gleis z
            -> AnchorName
            -> Gleis z
            -> AnchorName
            -> m (Either AttachError ())
gleisAttach
    gleisAnzeige@GleisAnzeige {tvarGleise}
    gleisA@Gleis {anchorPoints = anchorPointsA}
    anchorNameA
    gleisB@Gleis {anchorPoints = anchorPointsB}
    anchorNameB = liftIO $ runExceptT $ do
    position <- ExceptT $ atomically $ do
        gleise <- readTVar tvarGleise
        let maybePosition = do
                positionB@Position {winkel = winkelB} <- maybe (Left GleisBNotFount) Right
                    $ HashMap.lookup gleisB gleise
                AnchorPoint { anchorPosition = anchorPositionB
                            , anchorDirection =
                              AnchorDirection {anchorDX = anchorDXB, anchorDY = anchorVYB}}
                    <- maybe (Left AnchorBNotFound) Right
                    $ HashMap.lookup anchorNameB anchorPointsB
                AnchorPoint
                    { anchorPosition = AnchorPosition {anchorX = anchorXA, anchorY = anchorYA}
                    , anchorDirection =
                      AnchorDirection {anchorDX = anchorDXA, anchorDY = anchorVYA}}
                    <- maybe (Left AnchorANotFount) Right
                    $ HashMap.lookup anchorNameA anchorPointsA
                let winkelA :: Double
                    winkelA = winkelB + 180 / pi * anchorWinkelDifferenzBogenmaß
                    winkelBBogenmaß :: Double
                    winkelBBogenmaß = pi / 180 * winkelB
                    anchorWinkelDifferenzBogenmaß :: Double
                    anchorWinkelDifferenzBogenmaß =
                        winkelMitXAchse (-anchorDXB) (-anchorVYB)
                        - winkelMitXAchse anchorDXA anchorVYA
                    winkelABogenmaß :: Double
                    winkelABogenmaß = winkelBBogenmaß + anchorWinkelDifferenzBogenmaß
                    translatedAnchorXB, translatedAnchorYB :: Double
                    (translatedAnchorXB, translatedAnchorYB) =
                        translateAnchorPoint positionB anchorPositionB
                pure
                    Position
                    { x = translatedAnchorXB - anchorXA * cos winkelABogenmaß
                          + anchorYA * sin winkelABogenmaß
                    , y = translatedAnchorYB
                          - anchorXA * sin winkelABogenmaß
                          - anchorYA * cos winkelABogenmaß
                    , winkel = winkelA
                    }
            -- Winkel im Bogenmaß zwischen Vektor und x-Achse
            -- steigt im Uhrzeigersinn
            winkelMitXAchse :: Double -> Double -> Double
            winkelMitXAchse vx vy =
                if vy < 0
                    then -acosWinkel
                    else acosWinkel
                where
                    len :: Double
                    len = vx * vx + vy * vy

                    acosWinkel :: Double
                    acosWinkel = acos $ vx / len
        pure maybePosition
    lift $ gleisPut gleisAnzeige gleisA position

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
        tryTakeTMVar tmvarParentInformation
        -- reset anchorPoints
        drawingAreas <- moveAnchors tvarAnchorPoints (HashMap.lookup gleis gleise) Nothing gleis
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
    (gleise, bekannteLabel) <- atomically $ do
        writeTVar tvarScale scale
        (,) <$> readTVar tvarGleise <*> readTVar tvarLabel
    forM_ (HashMap.toList gleise) $ \(Gleis {drawingArea}, position)
        -> fixedSetChildTransformation fixed drawingArea position scale
    forM_ bekannteLabel
        $ \(label, position) -> fixedSetChildTransformation fixed label position scale

gleisAnzeigeSave :: (MonadIO m) => GleisAnzeige z -> FilePath -> m ()
gleisAnzeigeSave GleisAnzeige {tvarScale, tvarGleise, tvarLabel} filePath = liftIO $ do
    (gleise, bekannteLabel, scale)
        <- atomically $ (,,) <$> readTVar tvarGleise <*> readTVar tvarLabel <*> readTVar tvarScale
    texte <- forM bekannteLabel $ \(a, b) -> (, b) <$> Gtk.labelGetLabel a
    Binary.encodeFile filePath (map (first definition) $ HashMap.toList gleise, texte, Just scale)
