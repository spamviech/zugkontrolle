{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Concurrent.STM (newTVarIO, atomically, modifyTVar', readTVar, writeTVar)
import Control.Monad (void, unless)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.Gleis.Demonstration (gleisDemonstrationNew)
import Zug.UI.Gtk.Gleis.Widget (GleisAnzeige, gleisAnzeigeConfig, GleisAnzeigeConfig(..))
import Zug.UI.Gtk.MitWidget (MitWidget(erhalteWidget))

main :: IO ()
main = void $ do
    Gtk.init
    application <- Gtk.applicationNew Nothing []
    Gio.onApplicationActivate application $ createWindow application
    Gio.applicationRun application Nothing

data Key
    = KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    deriving (Show, Eq, Generic)

instance Hashable Key

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

-- | key event to move window
-- EventControllerKey:
--      keyReleased event is not able to suppress further event propagation (void return instead of Bool)
createMoveController :: GleisAnzeige z -> IO Gtk.EventControllerLegacy
createMoveController gleisAnzeige = do
    moveController <- Gtk.eventControllerLegacyNew
    tvarPressedKeys <- newTVarIO HashSet.empty
    tvarTime <- newTVarIO 0
    let speed = 50   -- units / (scale * s)
    widget <- erhalteWidget gleisAnzeige
    Gtk.widgetAddTickCallback widget $ \_widget frameClock -> do
        currentTime <- Gdk.frameClockGetFrameTime frameClock    -- µs
        (lastTime, pressedKeys) <- atomically $ do
            lastTime <- readTVar tvarTime
            writeTVar tvarTime $! currentTime
            (lastTime, ) <$> readTVar tvarPressedKeys
        let ifPressed key value
                | HashSet.member key pressedKeys = value
                | otherwise = 0
            delay = fromIntegral $ currentTime - lastTime       -- µs
            change keyPositive keyNegative scale =
                ifPressed keyPositive delta - ifPressed keyNegative delta
                where
                    delta = speed * scale * delay * 0.000001    -- units (as used by DrawingArea)
        unless (null pressedKeys)
            $ gleisAnzeigeConfig gleisAnzeige
            $ \GleisAnzeigeConfig {scale, x, y} -> GleisAnzeigeConfig
            { scale, x = x + change KeyRight KeyLeft scale, y = y + change KeyDown KeyUp scale }
        pure GLib.SOURCE_CONTINUE
    Gtk.onEventControllerLegacyEvent moveController $ \event -> Gdk.eventGetEventType event >>= \case
        Gdk.EventTypeKeyPress -> do
            keyEvent <- Gdk.unsafeCastTo Gdk.KeyEvent event
            keyval <- Gdk.keyEventGetKeyval keyEvent
            -- keycode <- Gdk.keyEventGetKeycode keyEvent
            case fromIntegral keyval of
                Gdk.KEY_Up -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.insert KeyUp
                    pure True
                Gdk.KEY_Down -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.insert KeyDown
                    pure True
                Gdk.KEY_Left -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.insert KeyLeft
                    pure True
                Gdk.KEY_Right -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.insert KeyRight
                    pure True
                Gdk.KEY_Home -> do
                    gleisAnzeigeConfig gleisAnzeige $ \config -> config { x = 0, y = 0 }
                    pure True
                _other -> pure False
        Gdk.EventTypeKeyRelease -> do
            keyEvent <- Gdk.unsafeCastTo Gdk.KeyEvent event
            keyval <- Gdk.keyEventGetKeyval keyEvent
            -- keycode <- Gdk.keyEventGetKeycode keyEvent
            case fromIntegral keyval of
                Gdk.KEY_Up -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.delete KeyUp
                    pure True
                Gdk.KEY_Down -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.delete KeyDown
                    pure True
                Gdk.KEY_Left -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.delete KeyLeft
                    pure True
                Gdk.KEY_Right -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.delete KeyRight
                    pure True
                _other -> pure False
        _otherwise -> pure False
    pure moveController
