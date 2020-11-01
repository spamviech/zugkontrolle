{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
#endif

module Zug.UI.Gtk.Gleise
  (
#ifdef ZUGKONTROLLEGUI
    -- * Gleis Widgets
    Gleis()
    -- ** Anpassen der Größe
  , gleisScale
  , gleisSetWidth
  , gleisSetHeight
    -- * Konstruktoren
  , geradeNew
  , märklin5106New
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Monad.Trans (MonadIO(..))
import Data.Int (Int32)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

import Zug.Enums (Zugtyp(..))
import Zug.UI.Gtk.Klassen (MitWidget(..))

{-
H0 Spurweite: 16.5mm
Gerade (5106): L180mm
Kurve (5120): 45°, R286mm
Kurve (5100): 30°, R360mm
Kurve (5200): 30°, R437.4mm
Kurve (5206): 24.28°, R427.4mm
Weiche (5202 L/R): L180mm, 24,28°, 427.4mm
Weiche (5140 L/R): 30°, Rin360mm, Rout77.4mm
Kreuzung (5128): L193mm, 30°
Kreuzung (5207): L180mm, 24,28°, R427.4mm
-}
{-
Lego Spurweite: 38mm
-}
-- | 'Gtk.Widget' von einem Gleis.
-- Die Größe wird nur über 'gleisScale', 'gleisSetWidth' und 'gleisSetHeight' verändert.
data Gleis (z :: Zugtyp) =
    Gleis { drawingArea :: Gtk.DrawingArea, width :: Int32, height :: Int32 }

instance MitWidget (Gleis z) where
    erhalteWidget :: (MonadIO m) => Gleis z -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . drawingArea

-- | Skaliere das 'Gleis' mit dem angegebenen Faktor.
gleisScale :: (MonadIO m) => Gleis z -> Double -> m ()
gleisScale Gleis {drawingArea, width, height} scale = do
    let newWidth = ceiling $ scale * (fromIntegral width)
        newHeight = ceiling $ scale * (fromIntegral height)
    Gtk.widgetSetSizeRequest drawingArea newWidth newHeight

-- | Ändere die Breite des 'Gleis'es zum angegebenen Wert.
-- Die Höhe wird bei konstantem Längenverhältnis angepasst.
gleisSetWidth :: (MonadIO m) => Gleis z -> Int32 -> m ()
gleisSetWidth gleis@Gleis {width} newWidth =
    gleisScale gleis $ fromIntegral newWidth / fromIntegral width

-- | Ändere die Höhe des 'Gleis'es zum angegebenen Wert.
-- Die Breite wird bei konstantem Längenverhältnis angepasst.
gleisSetHeight :: (MonadIO m) => Gleis z -> Int32 -> m ()
gleisSetHeight gleis@Gleis {height} newHeight =
    gleisScale gleis $ fromIntegral newHeight / fromIntegral height

-- | Create a new 'Gtk.DrawingArea' with a fixed size set up with the specified 'Cairo.Render' /draw/ path.
--
-- 'Cairo.setLineWidth' 1 is called before the /draw/ action is executed.
-- After the action 'Cairo.stroke' is executed.
gleisNew :: (MonadIO m)
         => (Gleis z -> Int32)
         -> (Gleis z -> Int32)
         -> (Gleis z -> Cairo.Render ())
         -> m (Gleis z)
gleisNew widthFn heightFn draw = do
    drawingArea <- Gtk.drawingAreaNew
    let gleis = Gleis { drawingArea, width, height }
        width = widthFn gleis
        height = heightFn gleis
    Gtk.widgetSetHexpand drawingArea False
    Gtk.widgetSetHalign drawingArea Gtk.AlignStart
    Gtk.widgetSetVexpand drawingArea False
    Gtk.widgetSetValign drawingArea Gtk.AlignStart
    gleisScale gleis 1
    Gtk.onWidgetDraw drawingArea $ Cairo.renderWithContext $ do
        newWidth <- Gtk.widgetGetAllocatedWidth drawingArea
        newHeight <- Gtk.widgetGetAllocatedHeight drawingArea
        let scale =
                min
                    (fromIntegral newWidth / fromIntegral width)
                    (fromIntegral newHeight / fromIntegral height)
        Cairo.save
        Cairo.scale scale scale
        Cairo.setLineWidth 1
        Cairo.newPath
        draw gleis
        Cairo.stroke
        Cairo.restore
        pure True
    pure gleis

class Spurweite z where
    spurweite :: Gleis z -> Double

instance Spurweite 'Märklin where
    spurweite :: Gleis 'Märklin -> Double
    spurweite = const 16.5

instance Spurweite 'Lego where
    spurweite :: Gleis 'Lego -> Double
    spurweite = const 38

abstand :: (Spurweite z) => Gleis z -> Double
abstand gleis = spurweite gleis / 3

geradeHeight :: (Spurweite z) => Gleis z -> Double
geradeHeight gleis = spurweite gleis + 2 * abstand gleis

geradeNew :: (MonadIO m, Spurweite z) => (forall n. Num n => n) -> m (Gleis z)
geradeNew länge = gleisNew (const länge) (ceiling . geradeHeight) $ zeichneGerade länge

märklin5106New :: (MonadIO m) => m (Gleis 'Märklin)
märklin5106New = geradeNew 180

zeichneGerade :: (Spurweite z) => Double -> Gleis z -> Cairo.Render ()
zeichneGerade länge gleis = do
    -- Beschränkungen
    Cairo.moveTo 0 0
    Cairo.lineTo 0 $ geradeHeight gleis
    Cairo.moveTo länge 0
    Cairo.lineTo länge $ geradeHeight gleis
    -- Gleis
    Cairo.moveTo 0 gleisOben
    Cairo.lineTo länge gleisOben
    Cairo.moveTo 0 gleisUnten
    Cairo.lineTo länge gleisUnten
    where
        gleisOben :: Double
        gleisOben = abstand gleis

        gleisUnten :: Double
        gleisUnten = geradeHeight gleis - abstand gleis
#endif
