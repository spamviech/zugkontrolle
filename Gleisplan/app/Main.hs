{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad (void)
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.Gleis.Demonstration (gleisDemonstrationNew)
import Zug.UI.Gtk.MitWidget (MitWidget(erhalteWidget))

main :: IO ()
main = void $ do
    application <- Gtk.applicationNew Nothing []
    Gio.onApplicationActivate application $ createWindow application
    Gio.applicationRun application Nothing

createWindow :: (Gtk.IsApplication a) => a -> IO ()
createWindow application = do
    appWindow <- Gtk.applicationWindowNew application
    Gtk.applicationAddWindow application appWindow
    Gtk.windowSetChild appWindow . Just =<< erhalteWidget =<< gleisDemonstrationNew
    -- test legacy events
    -- key events only happen in appWindow, they are not passed to children
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
            Gdk.EventTypeKeyPress -> do
                keyEvent <- Gdk.unsafeCastTo Gdk.KeyEvent event
                keyval <- Gdk.keyEventGetKeyval keyEvent
                keycode <- Gdk.keyEventGetKeycode keyEvent
                let knownKey = case fromIntegral keyval of
                        Gdk.KEY_Up -> Just "up"
                        Gdk.KEY_Down -> Just "down"
                        Gdk.KEY_Left -> Just "left"
                        Gdk.KEY_Right -> Just "right"
                        _other -> Nothing
                -- keycode represents state of shift, ctrl, etc.
                pure
                    $ Just
                    $ "pressed key (keyval, keycode): ("
                    <> show keyval
                    <> maybe "" (": " <>) knownKey
                    <> ", "
                    <> show keycode
                    <> ")"
            Gdk.EventTypeKeyRelease -> do
                keyEvent <- Gdk.unsafeCastTo Gdk.KeyEvent event
                fmap (Just . ("released key (keyval, keycode): " <>) . show)
                    $ (,) <$> Gdk.keyEventGetKeyval keyEvent <*> Gdk.keyEventGetKeycode keyEvent
            Gdk.EventTypeScroll -> do
                scrollEvent <- Gdk.unsafeCastTo Gdk.ScrollEvent event
                fmap (Just . ("scroll event (deltas, direction, isStop): " <>) . show)
                    $ (,,) <$> Gdk.scrollEventGetDeltas scrollEvent
                    <*> Gdk.scrollEventGetDirection scrollEvent
                    <*> Gdk.scrollEventIsStop scrollEvent
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
