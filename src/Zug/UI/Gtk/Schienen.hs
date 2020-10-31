{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE MonoLocalBinds #-}
#endif

module Zug.UI.Gtk.Schienen
  (
#ifdef ZUGKONTROLLEGUI
    zeichneGerade
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import qualified GI.Cairo.Render as Cairo
import qualified GI.Gtk as Gtk

zeichneGerade :: (Gtk.IsWidget w) => w -> Cairo.Render Bool
zeichneGerade widget = do
    width <- Gtk.widgetGetAllocatedWidth widget
    height <- Gtk.widgetGetAllocatedHeight widget
    Cairo.save
    Cairo.scale (fromIntegral width) (fromIntegral height)
    -- start drawing
    Cairo.newPath
    Cairo.rectangle 0 0 1 1
    Cairo.stroke
    -- end drawing
    Cairo.restore
    pure True
#endif
