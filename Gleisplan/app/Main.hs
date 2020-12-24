{-# LANGUAGE MonoLocalBinds #-}

module Main (main) where

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.Gleis.Anzeige (gleisAnzeigeNew)

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
    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.windowSetChild appWindow $ Just scrolledWindow
    gleisAnzeige <- gleisAnzeigeNew
    Gtk.scrolledWindowSetChild scrolledWindow $ Just gleisAnzeige
    Gtk.widgetShow appWindow
