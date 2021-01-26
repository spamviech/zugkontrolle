{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Concurrent.STM (newTVarIO, atomically, modifyTVar', readTVar, writeTVar)
import Control.Monad (void)
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

-- TODO allow changing keys?
-- how to display them? Is there a toUnicode/toChar function in Gdk/Gtk?
-- https://hackage.haskell.org/package/gi-gdk-4.0.2/docs/GI-Gdk-Functions.html#g:method:keyvalConvertCase
data Key
    = KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    | KeyHome
    deriving (Show, Eq, Generic)

instance Hashable Key

{-
data Button
    = ButtonPrimary {  }
    | ButtonSecondary
    | ButtonMiddle
    deriving (Show, Eq, Generic)

instance Hashable Button

data MotionPosition = MotionPosition { x :: Double, y :: Double }
    deriving (Show, Eq, Generic)
-}
data Scroll
    = ScrollUp
    | ScrollDown
    | ScrollLeft
    | ScrollRight
    deriving (Show, Eq, Generic)

instance Hashable Scroll

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

data PositionChange
    = PositionReset
    | PositionUnchanged
    | PositionMove { deltaX :: Double -> Double, deltaY :: Double -> Double }

-- | key event to move window
-- EventControllerKey:
--      keyReleased event is not able to suppress further event propagation (void return instead of Bool)
createMoveController :: GleisAnzeige z -> IO Gtk.EventControllerLegacy
createMoveController gleisAnzeige = do
    moveController <- Gtk.eventControllerLegacyNew
    tvarPressedKeys <- newTVarIO HashSet.empty
    tvarScrolls <- newTVarIO HashSet.empty
    tvarLastScrolls <- newTVarIO (HashSet.empty, 0)
    tvarTime <- newTVarIO 0
    tvarSpeed <- newTVarIO 0
    -- all units are given as used by 'Position' (unscaled)
    let acceleration = 1_000        -- units / s^2
        drag = 10                   -- 1 / s
        -- time a scroll event is kept alive
        scrollLifetime = 500_000    -- µs
    widget <- erhalteWidget gleisAnzeige
    Gtk.widgetAddTickCallback widget $ \_widget frameClock -> do
        currentTime <- Gdk.frameClockGetFrameTime frameClock        -- µs
        changes <- atomically $ do
            lastTime <- readTVar tvarTime
            writeTVar tvarTime $! currentTime
            lastSpeed <- readTVar tvarSpeed
            pressedKeys <- readTVar tvarPressedKeys
            currentScrolls <- readTVar tvarScrolls
            (lastScrolls, lastScrollTime) <- readTVar tvarLastScrolls
            -- there is no guaranteed stop event for scrolling
            scrolls <- if null currentScrolls && (lastScrollTime + scrollLifetime < currentTime)
                then do
                    pure lastScrolls
                else do
                    writeTVar tvarScrolls HashSet.empty
                    writeTVar tvarLastScrolls (currentScrolls, currentTime)
                    pure currentScrolls
            if
                | HashSet.member KeyHome pressedKeys -> do
                    writeTVar tvarSpeed 0
                    pure PositionReset
                | null pressedKeys && null scrolls -> do
                    writeTVar tvarSpeed 0
                    pure PositionUnchanged
                | otherwise -> do
                    let delay_µs = fromIntegral $ currentTime - lastTime    -- µs
                        delay_s = 0.000_001 * delay_µs                      -- s
                        currentSpeed =
                            lastSpeed + acceleration * delay_s - drag * lastSpeed * delay_s  -- units / s
                        -- TODO respect direction (reset speed when direction changes?)
                        -- TODO use polar coordinates, so diagonal movement is not faster?
                        change keyPositive scrollPositive keyNegative scrollNegative scale =
                            ifPressedOrScroll keyPositive scrollPositive
                            - ifPressedOrScroll keyNegative scrollNegative
                            where
                                ifPressedOrScroll key scroll
                                    | HashSet.member key pressedKeys
                                        || HashSet.member scroll scrolls =
                                        currentSpeed
                                        * scale
                                        * delay_s    -- scaled units (as used by DrawingArea)
                                    | otherwise = 0
                        deltaX = change KeyRight ScrollRight KeyLeft ScrollLeft
                        deltaY = change KeyDown ScrollDown KeyUp ScrollUp
                    writeTVar tvarSpeed $! currentSpeed
                    pure PositionMove { deltaX, deltaY }
        case changes of
            PositionReset -> gleisAnzeigeConfig gleisAnzeige $ \config -> config { x = 0, y = 0 }
            PositionUnchanged -> pure ()
            PositionMove {deltaX, deltaY}
                -> gleisAnzeigeConfig gleisAnzeige $ \GleisAnzeigeConfig {scale, x, y}
                -> GleisAnzeigeConfig { scale, x = x + deltaX scale, y = y + deltaY scale }
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
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.insert KeyHome
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
                Gdk.KEY_Home -> do
                    atomically $ modifyTVar' tvarPressedKeys $ HashSet.delete KeyHome
                    pure True
                _other -> pure False
        Gdk.EventTypeScroll -> do
            scrollEvent <- Gdk.unsafeCastTo Gdk.ScrollEvent event
            Gdk.scrollEventGetDirection scrollEvent >>= \case
                Gdk.ScrollDirectionUp -> do
                    atomically $ modifyTVar' tvarScrolls $ HashSet.insert ScrollUp
                    pure True
                Gdk.ScrollDirectionDown -> do
                    atomically $ modifyTVar' tvarScrolls $ HashSet.insert ScrollDown
                    pure True
                Gdk.ScrollDirectionLeft -> do
                    atomically $ modifyTVar' tvarScrolls $ HashSet.insert ScrollLeft
                    pure True
                Gdk.ScrollDirectionRight -> do
                    atomically $ modifyTVar' tvarScrolls $ HashSet.insert ScrollRight
                    pure True
                _other -> pure False
        _otherwise -> pure False
    pure moveController
