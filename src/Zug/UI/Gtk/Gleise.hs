{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
#endif

module Zug.UI.Gtk.Gleise
  (
#ifdef ZUGKONTROLLEGUI
    Gleis()
  , geradeNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Monad.Trans (MonadIO())
import Data.Int (Int32)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
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
newtype Gleis (z :: Zugtyp) = Gleis Gtk.DrawingArea

instance MitWidget (Gleis z) where
    erhalteWidget :: (MonadIO m) => Gleis z -> m Gtk.Widget
    erhalteWidget (Gleis drawingArea) = Gtk.toWidget drawingArea

-- | Create a new 'Gtk.DrawingArea' with a fixed size set up with the specified 'Cairo.Render' /draw/ path.
--
-- 'Cairo.setLineWidth' 1 is called before the /draw/ action is executed.
-- After the action 'Cairo.stroke' is executed.
schieneNew :: (MonadIO m) => Int32 -> Int32 -> (Gleis z -> Cairo.Render ()) -> m (Gleis z)
schieneNew width height draw = do
    canvas <- Gtk.drawingAreaNew
    Gtk.widgetSetSizeRequest canvas width height
    Gtk.widgetSetHexpand canvas False
    Gtk.widgetSetHalign canvas Gtk.AlignCenter
    Gtk.widgetSetVexpand canvas False
    Gtk.widgetSetValign canvas Gtk.AlignCenter
    let schiene = Gleis canvas
    Gtk.onWidgetDraw canvas $ Cairo.renderWithContext $ do
        Cairo.save
        Cairo.setLineWidth 1
        Cairo.newPath
        draw schiene
        Cairo.stroke
        Cairo.restore
        pure True
    pure schiene

class Spurweite z where
    spurweite :: Gleis z -> Double

instance Spurweite 'Märklin where
    spurweite :: Gleis 'Märklin -> Double
    spurweite = const 16.5

instance Spurweite 'Lego where
    spurweite :: Gleis 'Lego -> Double
    spurweite = const 38

geradeNew :: (MonadIO m, Spurweite z) => m (Gleis z)
geradeNew = schieneNew geradeWidth geradeHeight zeichneGerade

geradeWidth :: (Num n) => n
geradeWidth = 180

geradeHeight :: (Num n) => n
geradeHeight = 25

zeichneGerade :: (Spurweite z) => Gleis z -> Cairo.Render ()
zeichneGerade gleis = do
    -- Beschränkungen
    Cairo.moveTo 0 0
    Cairo.lineTo 0 geradeHeight
    Cairo.moveTo geradeWidth 0
    Cairo.lineTo geradeWidth geradeHeight
    -- Gleis
    Cairo.moveTo 0 gleisOben
    Cairo.lineTo geradeWidth gleisOben
    Cairo.moveTo 0 gleisUnten
    Cairo.lineTo geradeWidth gleisUnten
    where
        gleisOben :: Double
        gleisOben = 0.5 * (geradeHeight - spurweite gleis)

        gleisUnten :: Double
        gleisUnten = geradeHeight - gleisOben
#endif
