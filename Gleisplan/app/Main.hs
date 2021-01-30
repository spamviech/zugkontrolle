{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Control.Monad (void)
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.EventController (createMoveController)
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
    Gtk.widgetAddController appWindow =<< createMoveController gleisAnzeige
    -- test legacy events
    --      use one EventControllerLegacy to rule them all?
    --      probably better to have one for each action, i.e. a movingController, zoomingController, etc.
    -- key events only happen in appWindow, they are not passed to children
    -- EventControllerKey:
    --      keyReleased event is not able to suppress further event propagation (void return instead of Bool)
    -- EventControllerScroll:
    --      event has no position, probably requires constant storage of EventTypeMotionNotify result
    -- EventControllerZoom:
    --      event has no position, probably requires constant storage of EventTypeMotionNotify result
    -- GestureClick:
    --      no way to get the pressed button, right click is always only unpairedRelease
    legacyController <- Gtk.eventControllerLegacyNew
    Gtk.onEventControllerLegacyEvent legacyController $ \event -> do
        maybeMsg <- Gdk.eventGetEventType event >>= \case
            -- when registered directly on the window, there are no unpaired button release events
            -- the program loses focus, so the release event isn't caught as well
            -- if you move the mouse outside the program region, release is still caught
            -- to pair them, store a (HashMap Button Position)?
            --      this way we always get the distance between press an release
            Gdk.EventTypeButtonPress -> do
                buttonEvent <- Gdk.unsafeCastTo Gdk.ButtonEvent event
                Just . ("pressed button: " <>) . show <$> Gdk.buttonEventGetButton buttonEvent
            Gdk.EventTypeButtonRelease -> do
                buttonEvent <- Gdk.unsafeCastTo Gdk.ButtonEvent event
                Just . ("released button: " <>) . show <$> Gdk.buttonEventGetButton buttonEvent
            Gdk.EventTypeKeyPress -> pure Nothing
            Gdk.EventTypeKeyRelease -> pure Nothing
            Gdk.EventTypeScroll -> pure Nothing
            Gdk.EventTypeMotionNotify -> pure Nothing    -- main occurring event, clutters console
            eventType -> pure $ Just $ show eventType
        case maybeMsg of
            (Just msg) -> do
                putStrLn "----------"
                print =<< Gdk.eventGetPosition event
                putStrLn msg
            Nothing -> pure ()
        pure False
    Gtk.widgetAddController appWindow legacyController
    Gtk.widgetShow appWindow
