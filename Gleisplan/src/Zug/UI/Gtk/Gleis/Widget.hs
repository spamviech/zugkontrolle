{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

{-
ideas for rewrite with gtk4
    start from scratch (UI), so legacy-based mistakes might vanish
look at Gtk.renderLine (cairo-render/-connector should still work)
Use `GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ gtkAction` to call Gtk from a forked thread
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
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-GestureZoom.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-GestureClick.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-GestureDrag.html
Drag & Drop specific EventController; probably only required when multiple widgets are involved
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-DragSource.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-DragIcon.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-DropTarget.html
    https://hackage.haskell.org/package/gi-gtk-4.0.3/docs/GI-Gtk-Objects-DropTargetAsync.html
-}
module Zug.UI.Gtk.Gleis.Widget
  (  -- * GleisAnzeige
    GleisAnzeige
  , Position(..)
  , GleisAnzeigeConfig(..)
  , gleisAnzeigeNew
  , gleisAnzeigeConfig
    -- ** Gleise
  , gleisPut
  , GleisMoveError(..)
  , gleisMove
  , gleisRemove
  , AttachError(..)
  , gleisAttach
  , AttachMoveError(..)
  , gleisAttachMove
    -- *** Definition
  , GleisDefinition(..)
  , getWidth
  , getHeight
  , WeichenArt(..)
  , WeichenRichtungAllgemein(..)
  , alsDreiweg
  , WeichenRichtung(..)
  , KreuzungsArt(..)
    -- *** Anker-Punkte
  , AnchorName(..)
    -- ** Texte
  , textPut
  , TextMoveError(..)
  , textMove
  , textRemove
    -- ** Speichern / Laden
  , gleisAnzeigeSave
  , SaveError()
  , gleisAnzeigeLoad
  , LoadError(..)
  ) where

import Control.Carrier.Error.Either (runError, ErrorC())
import Control.Concurrent.STM (STM, atomically, TVar, newTVarIO, readTVar, writeTVar, modifyTVar')
import Control.Effect.Exception (handle, try)
import Control.Effect.Lift (Lift(), sendIO)
import Control.Effect.Throw (Has(), Throw(), liftEither)
import Control.Monad (void, forM_)
import Data.Bifunctor (first)
import qualified Data.ByteString as Bytestring
import Data.HashMap.Strict (HashMap())
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable())
import Data.Int (Int32)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy(Proxy))
import Data.RTree (RTree)
import qualified Data.RTree as RTree
import Data.Text (Text)
import Flat (Flat(..))
import qualified Flat
import qualified Flat.Decoder as Flat
import qualified Flat.Encoder as Flat
import GHC.Generics (Generic())
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as PangoCairo
import Numeric.Natural (Natural)
import System.IO (IOMode(ReadMode, WriteMode), withBinaryFile)

import Zug.Enums (Zugtyp(..))
import Zug.UI.Gtk.Gleis.Anchor (AnchorPoint(..), AnchorPosition(..), AnchorDirection(..)
                              , AnchorName(..), AnchorPointMap, mbbSearch, mbbPoint)
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

translateAnchorPoint :: Position -> AnchorPosition -> (Double, Double)
translateAnchorPoint Position {x, y, winkel} AnchorPosition {anchorX, anchorY} = (fixedX, fixedY)
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi / 180 * winkel

        fixedX :: Double
        fixedX = x + anchorX * cos winkelBogenmaß - anchorY * sin winkelBogenmaß

        fixedY :: Double
        fixedY = y + anchorX * sin winkelBogenmaß + anchorY * cos winkelBogenmaß

intersections :: Position -> AnchorPoint -> RTree (NonEmpty a) -> [a]
intersections position anchorPoint knownAnchorPoints =
    concatMap NonEmpty.toList
    $ uncurry mbbSearch (translateAnchorPoint position $ anchorPosition anchorPoint)
    `RTree.intersect` knownAnchorPoints

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

instance Flat (GleisDefinition z)

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

-- | Helper type to automatically derive Generic, and transitively Flat instance
data FlatRichtung
    = FlatLinks
    | FlatRechts
    | FlatDreiwege
    deriving (Show, Eq, Generic)

instance Flat FlatRichtung

alsFlat :: WeichenRichtungAllgemein a -> FlatRichtung
alsFlat Links = FlatLinks
alsFlat Rechts = FlatRechts
alsFlat Dreiwege = FlatDreiwege

instance Flat (WeichenRichtungAllgemein 'WeicheZweiweg) where
    encode :: WeichenRichtungAllgemein 'WeicheZweiweg -> Flat.Encoding
    encode = encode . alsFlat

    decode :: Flat.Get (WeichenRichtungAllgemein 'WeicheZweiweg)
    decode = decode >>= \case
        FlatLinks -> pure Links
        FlatRechts -> pure Rechts
        FlatDreiwege -> fail "Unbekannte Zweiweg-WeichenRichtung: Dreiwege"

    size :: WeichenRichtungAllgemein 'WeicheZweiweg -> Flat.NumBits -> Flat.NumBits
    size = size . alsFlat

    -- TODO two bits should be enough
instance Flat (WeichenRichtungAllgemein 'WeicheDreiweg) where
    encode :: WeichenRichtungAllgemein 'WeicheDreiweg -> Flat.Encoding
    encode = encode . alsFlat

    decode :: Flat.Get (WeichenRichtungAllgemein 'WeicheDreiweg)
    decode = decode >>= \case
        FlatLinks -> pure Links
        FlatRechts -> pure Rechts
        FlatDreiwege -> pure Dreiwege

    size :: WeichenRichtungAllgemein 'WeicheDreiweg -> Flat.NumBits -> Flat.NumBits
    size = size . alsFlat

alsDreiweg :: WeichenRichtungAllgemein a -> WeichenRichtungAllgemein 'WeicheDreiweg
alsDreiweg Links = Links
alsDreiweg Rechts = Rechts
alsDreiweg Dreiwege = Dreiwege

data WeichenRichtung
    = Normal { geradeRichtung :: WeichenRichtungAllgemein 'WeicheDreiweg }
    | Gebogen { gebogeneRichtung :: WeichenRichtungAllgemein 'WeicheZweiweg }
    deriving (Eq, Show, Generic)

instance Flat WeichenRichtung

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

instance Flat Position

data GleisAnzeigeConfig = GleisAnzeigeConfig { scale :: Double, x :: Double, y :: Double }
    deriving (Show, Eq, Generic)

instance Flat GleisAnzeigeConfig

newtype GleisId (z :: Zugtyp) = GleisId { gId :: Natural }
    deriving stock Show
    deriving (Eq, Hashable, Flat) via Natural

newtype TextId (z :: Zugtyp) = TextId { tId :: Natural }
    deriving stock Show
    deriving (Eq, Hashable, Flat) via Natural

-- TODO add AnchorDirection?
-- | AnchorPoints einer 'GleisAnzeige', sortiert nach 'Position'.
type AnchorPointRTree (z :: Zugtyp) = RTree (NonEmpty (GleisId z))

-- https://hackage.haskell.org/package/gi-pangocairo-1.0.24/docs/GI-PangoCairo-Functions.html#v:createLayout
-- https://hackage.haskell.org/package/gi-pango-1.0.23/docs/GI-Pango-Objects-Layout.html#v:layoutSetText
-- https://hackage.haskell.org/package/gi-pangocairo-1.0.24/docs/GI-PangoCairo-Functions.html#v:layoutPath
data GleisAnzeige (z :: Zugtyp) =
    GleisAnzeige
    { drawingArea :: Gtk.DrawingArea
    , tvarConfig :: TVar GleisAnzeigeConfig
    , tvarNextGleisId :: TVar (GleisId z)
    , tvarNextTextId :: TVar (TextId z)
    , tvarGleise :: TVar (HashMap (GleisId z) (GleisDefinition z, Position))
    , tvarTexte :: TVar (HashMap (TextId z) (Text, Position))
    , tvarAnchorPoints :: TVar (AnchorPointRTree z)
    }

instance MitWidget (GleisAnzeige z) where
    erhalteWidget :: (Has (Lift IO) sig m) => GleisAnzeige z -> m Gtk.Widget
    erhalteWidget GleisAnzeige {drawingArea} = sendIO $ Gtk.toWidget drawingArea

withSaveRestore :: Cairo.Render a -> Cairo.Render a
withSaveRestore action = Cairo.save *> action <* Cairo.restore

-- TODO add hotkeys to adjust config
-- scrolling & zoom gesture for scale (zoom)
--      maybe only Ctrl+Scroll for zoom, so touch screen works correctly
-- right-click to move
--      GestureClick and right click behave weirdly, might be fixed in newer versions
--      use arrow-keys to move instead
-- allow rotation? e.g. via middle click
-- TODO Ctrl+0/Num0 reset to scale 1
-- TODO Home/Pos1 reset to position 0x0
-- | This function contains Gtk-methods, thus must be run in the Gtk main thread
gleisAnzeigeNew :: forall m sig z. (Has (Lift IO) sig m, Spurweite z) => m (GleisAnzeige z)
gleisAnzeigeNew = sendIO $ do
    drawingArea <- Gtk.drawingAreaNew
    tvarNextGleisId <- newTVarIO $ GleisId 0
    tvarNextTextId <- newTVarIO $ TextId 0
    tvarConfig <- newTVarIO GleisAnzeigeConfig { scale = 1, x = 0, y = 0 }
    tvarGleise <- newTVarIO HashMap.empty
    tvarTexte <- newTVarIO HashMap.empty
    tvarAnchorPoints <- newTVarIO RTree.empty
    -- configure drawing area
    Gtk.drawingAreaSetContentHeight drawingArea height
    Gtk.drawingAreaSetContentWidth drawingArea width
    Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ \_drawingArea context _newWidth _newHeight -> do
        (knownAnchorPoints, gleise, texte, GleisAnzeigeConfig {x, y, scale}) <- atomically
            $ (,,,) <$> readTVar tvarAnchorPoints
            <*> readTVar tvarGleise
            <*> readTVar tvarTexte
            <*> readTVar tvarConfig
        void $ flip Cairo.renderWithContext context $ withSaveRestore $ do
            Cairo.setLineWidth 1
            -- adjust Scale and show window section
            Cairo.scale scale scale
            Cairo.translate (-x) (-y)
            -- zeichne Gleise
            forM_ (HashMap.toList gleise) $ renderGleis knownAnchorPoints
            -- schreibe Texte
            forM_ texte renderText
            pure True
    pure
        GleisAnzeige
        { drawingArea
        , tvarNextGleisId
        , tvarNextTextId
        , tvarConfig
        , tvarGleise
        , tvarTexte
        , tvarAnchorPoints
        }
    where
        width :: Int32
        width = 600

        height :: Int32
        height = 400

        renderGleis
            :: AnchorPointRTree z -> (GleisId z, (GleisDefinition z, Position)) -> Cairo.Render ()
        renderGleis
            knownAnchorPoints
            (gleisId, (gleisDefinition, position@Position {x, y, winkel})) = withSaveRestore $ do
            -- move to position and rotate
            let winkelBogenmaß :: Double
                winkelBogenmaß = pi / 180 * winkel
            Cairo.translate x y
            Cairo.rotate winkelBogenmaß
            -- draw rail
            withSaveRestore $ do
                Cairo.newPath
                getZeichnen gleisDefinition
                Cairo.stroke
            -- mark anchor points
            forM_ (getAnchorPoints gleisDefinition)
                $ \anchorPoint@AnchorPoint
                { anchorPosition = AnchorPosition {anchorX, anchorY}
                , anchorDirection = AnchorDirection {anchorDX, anchorDY}} -> withSaveRestore $ do
                    Cairo.moveTo anchorX anchorY
                    let r, g, b :: Double
                        (r, g, b) =
                            if any (/= gleisId)
                                $ intersections position anchorPoint knownAnchorPoints
                                then (0, 1, 0)
                                else (0, 0, 1)
                        len :: Double
                        len = sqrt $ anchorDX * anchorDX + anchorDY * anchorDY
                    Cairo.setSourceRGB r g b
                    Cairo.relLineTo (-5 * anchorDX / len) (-5 * anchorDY / len)
                    Cairo.stroke

        renderText :: (Text, Position) -> Cairo.Render ()
        renderText (text, Position {x, y, winkel}) = withSaveRestore $ do
            context <- Cairo.getContext
            -- move to position and rotate
            let winkelBogenmaß :: Double
                winkelBogenmaß = pi / 180 * winkel
            Cairo.translate x y
            Cairo.rotate winkelBogenmaß
            -- write text
            Cairo.newPath
            Cairo.moveTo 0 0
            layout <- PangoCairo.createLayout context
            Pango.layoutSetText layout text $ -1
            PangoCairo.layoutPath context layout
            Cairo.stroke

-- | Anpassen der 'GleisAnzeigeConfig' einer 'GleisAnzeige'.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
gleisAnzeigeConfig :: (Has (Lift IO) sig m)
                   => GleisAnzeige z
                   -> (GleisAnzeigeConfig -> GleisAnzeigeConfig)
                   -> m ()
gleisAnzeigeConfig GleisAnzeige {drawingArea, tvarConfig} updateFn = sendIO $ do
    atomically $ modifyTVar' tvarConfig updateFn
    Gtk.widgetQueueDraw drawingArea

-- | Remove current 'AnchorPoint' of the 'Gleis' and, if available, move them to the new location.
-- Returns list of 'Gtk.DrawingArea' with close 'AnchorPoint' to old or new location.
moveAnchors :: forall z.
            TVar (AnchorPointRTree z)
            -> GleisId z
            -> Maybe (Position, AnchorPointMap)
            -> STM ()
moveAnchors tvarAnchorPoints gleisId maybePositionAnchorPoints = do
    knownAnchorPoints <- readTVar tvarAnchorPoints
    let otherAnchorPoints :: AnchorPointRTree z
        otherAnchorPoints =
            RTree.mapMaybe (NonEmpty.nonEmpty . NonEmpty.filter (/= gleisId)) knownAnchorPoints
        newAnchorPoints :: AnchorPointRTree z
        newAnchorPoints = case maybePositionAnchorPoints of
            Nothing -> otherAnchorPoints
            (Just (position, anchorPoints))
                -> foldl' (\acc AnchorPoint {anchorPosition} -> RTree.insertWith
                               (<>)
                               (uncurry mbbPoint $ translateAnchorPoint position anchorPosition)
                               (gleisId :| [])
                               acc) otherAnchorPoints anchorPoints
    writeTVar tvarAnchorPoints $! newAnchorPoints

-- | Füge ein Gleis zur angestrebten 'Position' einer 'GleisAnzeige' hinzu.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
gleisPut :: (Has (Lift IO) sig m, Spurweite z)
         => GleisAnzeige z
         -> GleisDefinition z
         -> Position
         -> m (GleisId z)
gleisPut
    GleisAnzeige {drawingArea, tvarNextGleisId, tvarGleise, tvarAnchorPoints}
    definition
    position = sendIO $ do
    gleisId <- atomically $ do
        -- create new id
        gleisId@GleisId {gId} <- readTVar tvarNextGleisId
        writeTVar tvarNextGleisId $ GleisId $ succ gId
        -- move Position
        gleise <- readTVar tvarGleise
        writeTVar tvarGleise $! HashMap.insert gleisId (definition, position) gleise
        -- move anchor points
        moveAnchors tvarAnchorPoints gleisId $ Just (position, getAnchorPoints definition)
        -- result is newly created id
        pure gleisId
    -- Queue re-draw
    Gtk.widgetQueueDraw drawingArea
    pure gleisId

newtype GleisMoveError (z :: Zugtyp) = InvalidGleisId (GleisId z)
    deriving (Eq, Show)

-- | Bewege das Gleis mit der übergebenen 'GleisId' zur angestrebten 'Position' einer 'GleisAnzeige'.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
gleisMove :: (Has (Throw (GleisMoveError z)) sig m, Has (Lift IO) sig m, Spurweite z)
          => GleisAnzeige z
          -> GleisId z
          -> Position
          -> m ()
gleisMove GleisAnzeige {drawingArea, tvarGleise, tvarAnchorPoints} gleisId position = do
    liftEither =<< (sendIO . atomically) do
        gleise <- readTVar tvarGleise
        case HashMap.lookup gleisId gleise of
            Nothing -> pure $ Left $ InvalidGleisId gleisId
            (Just (definition, _oldPosition)) -> Right <$> do
                -- move Position
                writeTVar tvarGleise $! HashMap.insert gleisId (definition, position) gleise
                -- move anchor points
                moveAnchors tvarAnchorPoints gleisId $ Just (position, getAnchorPoints definition)
    -- Queue re-draw
    sendIO $ Gtk.widgetQueueDraw drawingArea

-- | Entferne ein 'Gleis' aus der 'GleisAnzeige'.
--
-- 'Gleis'e die kein Teil der 'GleisAnzeige' sind werden stillschweigend ignoriert.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
gleisRemove :: (Has (Lift IO) sig m) => GleisAnzeige z -> GleisId z -> m ()
gleisRemove GleisAnzeige {drawingArea, tvarGleise, tvarAnchorPoints} gleisId = sendIO $ do
    atomically $ do
        -- remove from shown rails
        modifyTVar' tvarGleise $ HashMap.delete gleisId
        -- remove anchor points
        moveAnchors tvarAnchorPoints gleisId Nothing
    -- Queue re-draw
    Gtk.widgetQueueDraw drawingArea

-- | Bewege einen 'Text' zur angestrebten 'Position' einer 'GleisAnzeige'.
--
-- Wenn ein 'Text' kein Teil der 'GleisAnzeige' war wird es neu hinzugefügt.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
textPut :: (Has (Lift IO) sig m) => GleisAnzeige z -> Text -> Position -> m (TextId z)
textPut GleisAnzeige {drawingArea, tvarNextTextId, tvarTexte} text position = sendIO $ do
    textId <- atomically $ do
        -- create new id
        textId@TextId {tId} <- readTVar tvarNextTextId
        writeTVar tvarNextTextId $ TextId $ succ tId
        -- move Position
        texte <- readTVar tvarTexte
        writeTVar tvarTexte $! HashMap.insert textId (text, position) texte
        -- result is newly created id
        pure textId
    -- Queue re-draw
    Gtk.widgetQueueDraw drawingArea
    pure textId

-- | Entferne einen 'Text' aus der 'GleisAnzeige'.
--
-- 'Text'e die kein Teil der 'GleisAnzeige' sind werden stillschweigend ignoriert.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
textRemove :: (Has (Lift IO) sig m) => GleisAnzeige z -> TextId z -> m ()
textRemove GleisAnzeige {drawingArea, tvarTexte} textId = sendIO $ do
    -- remove from show texts
    atomically $ modifyTVar' tvarTexte $ HashMap.delete textId
    -- Queue re-draw
    Gtk.widgetQueueDraw drawingArea

newtype TextMoveError (z :: Zugtyp) = InvalidTextId (TextId z)
    deriving (Eq, Show)

-- | Bewege den 'Text' mit der übergebenen 'TextId' zur angestrebten 'Position' einer 'GleisAnzeige'.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
textMove :: (Has (Throw (TextMoveError z)) sig m, Has (Lift IO) sig m, Spurweite z)
         => GleisAnzeige z
         -> TextId z
         -> Position
         -> m ()
textMove GleisAnzeige {drawingArea, tvarTexte} textId position = do
    liftEither =<< (sendIO . atomically) do
        texte <- readTVar tvarTexte
        case HashMap.lookup textId texte of
            Nothing -> pure $ Left $ InvalidTextId textId
            -- move Position
            (Just (text, _oldPosition))
                -> fmap Right $ writeTVar tvarTexte $! HashMap.insert textId (text, position) texte
    -- Queue re-draw
    sendIO $ Gtk.widgetQueueDraw drawingArea

data AttachError (z :: Zugtyp)
    = GleisBNotFount (GleisId z)
    | AnchorBNotFound AnchorName
    | AnchorANotFount AnchorName
    deriving (Eq, Show)

gleisAttachPosition
    :: (Spurweite z)
    => HashMap (GleisId z) (GleisDefinition z, Position)
    -> GleisId z
    -> AnchorName
    -> GleisDefinition z
    -> AnchorName
    -> Either (AttachError z) Position
gleisAttachPosition gleise gleisIdB anchorNameB definitionA anchorNameA = do
    (definitionB, positionB@Position {winkel = winkelB})
        <- maybe (Left $ GleisBNotFount gleisIdB) Right $ HashMap.lookup gleisIdB gleise
    AnchorPoint { anchorPosition = anchorPositionB
                , anchorDirection = AnchorDirection {anchorDX = anchorDXB, anchorDY = anchorVYB}}
        <- maybe (Left $ AnchorBNotFound anchorNameB) Right
        $ HashMap.lookup anchorNameB
        $ getAnchorPoints definitionB
    AnchorPoint { anchorPosition = AnchorPosition {anchorX = anchorXA, anchorY = anchorYA}
                , anchorDirection = AnchorDirection {anchorDX = anchorDXA, anchorDY = anchorVYA}}
        <- maybe (Left $ AnchorANotFount anchorNameA) Right
        $ HashMap.lookup anchorNameA
        $ getAnchorPoints definitionA
    let winkelA :: Double
        winkelA = winkelB + 180 / pi * anchorWinkelDifferenzBogenmaß
        winkelBBogenmaß :: Double
        winkelBBogenmaß = pi / 180 * winkelB
        anchorWinkelDifferenzBogenmaß :: Double
        anchorWinkelDifferenzBogenmaß =
            winkelMitXAchse (-anchorDXB) (-anchorVYB) - winkelMitXAchse anchorDXA anchorVYA
        winkelABogenmaß :: Double
        winkelABogenmaß = winkelBBogenmaß + anchorWinkelDifferenzBogenmaß
        translatedAnchorXB, translatedAnchorYB :: Double
        (translatedAnchorXB, translatedAnchorYB) = translateAnchorPoint positionB anchorPositionB
    pure
        Position
        { x = translatedAnchorXB - anchorXA * cos winkelABogenmaß
              + anchorYA * sin winkelABogenmaß
        , y = translatedAnchorYB
              - anchorXA * sin winkelABogenmaß
              - anchorYA * cos winkelABogenmaß
        , winkel = winkelA
        }
    where
        -- Winkel im Bogenmaß zwischen Vektor und x-Achse
        -- steigt im Uhrzeigersinn
        winkelMitXAchse :: Double -> Double -> Double
        winkelMitXAchse vx vy =
            if vy < 0
                then -acosWinkel
                else acosWinkel
            where
                len :: Double
                len = sqrt $ vx * vx + vy * vy

                acosWinkel :: Double
                acosWinkel = acos $ vx / len

-- | Bewege /gleisA/ neben /gleisB/, so dass /anchorNameA/ direkt neben /anchorNameB/ liegt.
--
-- Der Rückgabewert signalisiert ob das anfügen erfolgreich wahr. Mögliche Fehlerquellen:
-- * /gleisB/ ist kein Teil der 'GleisAnzeige'.
-- * /anchorB/ ist kein 'AnchorPoint' von /gleisB/.
-- * /anchorA/ ist kein 'AnchorPoint' von /gleisA/.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
gleisAttach :: (Has (Throw (AttachError z)) sig m, Has (Lift IO) sig m, Spurweite z)
            => GleisAnzeige z
            -> GleisId z
            -> AnchorName
            -> GleisDefinition z
            -> AnchorName
            -> m (GleisId z)
gleisAttach gleisAnzeige@GleisAnzeige {tvarGleise} gleisIdB anchorNameB definitionA anchorNameA = do
    position <- liftEither =<< (sendIO . atomically) do
        gleise <- readTVar tvarGleise
        pure $ gleisAttachPosition gleise gleisIdB anchorNameB definitionA anchorNameA
    gleisPut gleisAnzeige definitionA position

data AttachMoveError (z :: Zugtyp)
    = AttachError (AttachError z)
    | MoveError (GleisMoveError z)
    | GleisANotFound (GleisId z)

-- | Bewege /gleisA/ neben /gleisB/, so dass /anchorNameA/ direkt neben /anchorNameB/ liegt.
--
-- Der Rückgabewert signalisiert ob das anfügen erfolgreich wahr. Mögliche Fehlerquellen:
-- * /gleisB/ ist kein Teil der 'GleisAnzeige'.
-- * /gleisA/ ist kein Teil der 'GleisAnzeige'.
-- * /anchorB/ ist kein 'AnchorPoint' von /gleisB/.
-- * /anchorA/ ist kein 'AnchorPoint' von /gleisA/.
--
-- This function contains Gtk-methods, thus must be run in the Gtk main thread
gleisAttachMove :: forall m sig z.
                (Has (Throw (AttachMoveError z)) sig m, Has (Lift IO) sig m, Spurweite z)
                => GleisAnzeige z
                -> GleisId z
                -> AnchorName
                -> GleisId z
                -> AnchorName
                -> m ()
gleisAttachMove gleisAnzeige@GleisAnzeige {tvarGleise} gleisIdB anchorNameB gleisIdA anchorNameA = do
    position <- liftEither =<< (sendIO . atomically) do
        gleise <- readTVar tvarGleise
        case HashMap.lookup gleisIdA gleise of
            Nothing -> pure $ Left $ GleisANotFound gleisIdA
            (Just (definitionA, _oldPosition)) -> pure
                $ first AttachError
                $ gleisAttachPosition gleise gleisIdB anchorNameB definitionA anchorNameA
    liftEither . first MoveError
        =<< runError (gleisMove gleisAnzeige gleisIdA position :: ErrorC (GleisMoveError z) m ())

newtype SaveError = SaveIOError IOError

saveVersion :: Natural
saveVersion = 0

-- TODO can encodeFile throw IOError as well?
-- e.g. isAlreadyInUseError, isFullError, isPermissionError
gleisAnzeigeSave
    :: (Has (Throw SaveError) sig m, Has (Lift IO) sig m) => GleisAnzeige z -> FilePath -> m ()
gleisAnzeigeSave GleisAnzeige {tvarConfig, tvarGleise, tvarTexte} filePath = do
    (gleise, texte, config) <- sendIO
        $ atomically
        $ (,,) <$> readTVar tvarGleise <*> readTVar tvarTexte <*> readTVar tvarConfig
    let saveData = (saveVersion, HashMap.elems gleise, HashMap.elems texte, config)
    liftEither . first SaveIOError
        =<< try
            (sendIO $ withBinaryFile filePath WriteMode $ flip Bytestring.hPut $ Flat.flat saveData)

-- TODO classify IOError instead of simply re-throwing?
-- with a fallback UnexpectedIOError
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO-Error.html#g:2
data LoadError
    = LoadIOError IOError
    | DecodingError Flat.DecodeException
    deriving (Show, Eq)

-- |This function contains Gtk-methods, thus must be run in the Gtk main thread
gleisAnzeigeLoad :: forall m sig z.
                 (Has (Throw LoadError) sig m, Has (Lift IO) sig m, Spurweite z)
                 => GleisAnzeige z
                 -> FilePath
                 -> m ()
gleisAnzeigeLoad
    gleisAnzeige@GleisAnzeige
    {drawingArea, tvarGleise, tvarTexte, tvarAnchorPoints, tvarNextGleisId, tvarNextTextId}
    filePath = do
    ( _saveVersion :: Natural
        , gleise :: [(GleisDefinition z, Position)]
        , texte :: [(Text, Position)]
        , config :: GleisAnzeigeConfig
        ) <- liftEither
        =<< (handle (pure . Left . LoadIOError) . fmap (first DecodingError))
            (sendIO $ Flat.unflat <$> withBinaryFile filePath ReadMode Bytestring.hGetContents)
    sendIO $ do
        -- remove all present rails and texts
        atomically $ do
            writeTVar tvarGleise HashMap.empty
            writeTVar tvarTexte HashMap.empty
            writeTVar tvarAnchorPoints RTree.empty
            -- reset ids (not really necessary)
            writeTVar tvarNextGleisId $ GleisId 0
            writeTVar tvarNextTextId $ TextId 0
        -- add saved rails ands texts
        forM_ gleise $ uncurry $ gleisPut gleisAnzeige
        forM_ texte $ uncurry $ textPut gleisAnzeige
        -- restore config
        gleisAnzeigeConfig gleisAnzeige $ const config
        -- make sure loaded rails are shown
        Gtk.widgetQueueDraw drawingArea
