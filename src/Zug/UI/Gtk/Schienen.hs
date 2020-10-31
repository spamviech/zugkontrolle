{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE MonoLocalBinds #-}
#endif

module Zug.UI.Gtk.Schienen
  (
#ifdef ZUGKONTROLLEGUI
    geradeNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Monad.Trans (MonadIO())
import Data.Int (Int32)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Gtk as Gtk

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
-- | Create a new 'Gtk.DrawingArea' with a fixed size (unless changed by careless 'boxPack'),
-- set up with the specified 'Cairo.Render' /draw/ path.
--
-- The path should be constructed in coordinates from 0 to 1 (both directions).
-- 'Cairo.setLineWidth' 0.1 is called in these coordinates.
staticDrawingAreaNew :: (MonadIO m) => Int32 -> Int32 -> Cairo.Render () -> m Gtk.DrawingArea
staticDrawingAreaNew width height draw = do
    canvas <- Gtk.drawingAreaNew
    Gtk.widgetSetSizeRequest canvas width height
    Gtk.widgetSetHexpand canvas False
    Gtk.widgetSetHalign canvas Gtk.AlignCenter
    Gtk.widgetSetVexpand canvas False
    Gtk.widgetSetValign canvas Gtk.AlignCenter
    Gtk.onWidgetDraw canvas $ Cairo.renderWithContext $ do
        Cairo.save
        Cairo.scale (fromIntegral width) (fromIntegral height)
        Cairo.setLineWidth 0.1
        Cairo.newPath
        draw
        Cairo.restore
        pure True
    pure canvas

geradeNew :: (MonadIO m) => m Gtk.DrawingArea
geradeNew = staticDrawingAreaNew geradeWidth geradeHeight zeichneGerade

geradeWidth :: Int32
geradeWidth = 180

geradeHeight :: Int32
geradeHeight = 18

zeichneGerade :: Cairo.Render ()
zeichneGerade = do
    Cairo.rectangle 0 0 1 1
    Cairo.stroke
#endif
