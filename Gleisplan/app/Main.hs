{-# LANGUAGE MonoLocalBinds #-}

module Main (main) where

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.Gleise (gleisAnzeigeNew)

main :: IO ()
main = do
    (Just application) <- Gtk.applicationNew Nothing []
    Gio.onApplicationActivate application $ createWindow application
    Gio.applicationRun application Nothing
    pure ()

createWindow :: (Gtk.IsApplication a) => a -> IO ()
createWindow application = do
    appWindow <- Gtk.applicationWindowNew application
    -- Hide decoration, since for some reason Gtk puts another window around it
    -- main window (Application) is really unresponsive for some reasons
    --Gtk.windowSetDecorated appWindow False
    Gtk.applicationAddWindow application appWindow
    Gtk.windowSetDefaultSize appWindow 480 320
    --scrolledWindow <- Gtk.scrolledWindowNew
    scrolledWindow <- Gtk.scrolledWindowNew (Nothing :: Maybe Gtk.Adjustment) (Nothing :: Maybe Gtk.Adjustment)
    --Gtk.windowSetChild appWindow $ Just scrolledWindow
    Gtk.containerAdd appWindow scrolledWindow
    gleisAnzeige <- gleisAnzeigeNew
    --Gtk.scrolledWindowSetChild scrolledWindow $ Just gleisAnzeige
    Gtk.containerAdd scrolledWindow gleisAnzeige
    --Gtk.widgetShow appWindow
    Gtk.widgetShowAll appWindow
