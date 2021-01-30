{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Control.Monad (void)
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.EventController (legacyControllerNew, moveControllerNew)
import Zug.UI.Gtk.Gleis.Demonstration (gleisDemonstrationNew)
import Zug.UI.Gtk.MitWidget (MitWidget(erhalteWidget))

main :: IO ()
main = void $ do
    Gtk.init
    application <- Gtk.applicationNew Nothing []
    Gio.onApplicationActivate application $ createWindow application
    Gio.applicationRun application Nothing

createWindow :: (Gtk.IsApplication a) => a -> IO ()
createWindow application = do
    appWindow <- Gtk.applicationWindowNew application
    Gtk.applicationAddWindow application appWindow
    gleisAnzeige <- gleisDemonstrationNew
    Gtk.windowSetChild appWindow . Just =<< erhalteWidget gleisAnzeige
    Gtk.widgetAddController appWindow =<< legacyControllerNew
    Gtk.widgetAddController appWindow =<< moveControllerNew gleisAnzeige
    Gtk.widgetShow appWindow
