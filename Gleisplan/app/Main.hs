{-# LANGUAGE MonoLocalBinds #-}

module Main (main) where

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.Gleis.Demonstration (gleisDemonstrationNew)
import Zug.UI.Gtk.MitWidget (MitWidget(erhalteWidget))

main :: IO ()
main = do
    application <- Gtk.applicationNew Nothing []
    Gio.onApplicationActivate application $ createWindow application
    Gio.applicationRun application Nothing
    pure ()

createWindow :: (Gtk.IsApplication a) => a -> IO ()
createWindow application = do
    appWindow <- Gtk.applicationWindowNew application
    Gtk.applicationAddWindow application appWindow
    Gtk.windowSetDefaultSize appWindow 480 320
    Gtk.windowSetChild appWindow . Just =<< erhalteWidget =<< gleisDemonstrationNew
    Gtk.widgetShow appWindow
