{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Zug.UI.Gtk.EventController (legacyControllerNew, moveControllerNew) where

import Control.Concurrent.STM (newTVarIO, atomically, modifyTVar', readTVar, writeTVar)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.Gleis.Widget (GleisAnzeige, gleisAnzeigeConfig, GleisAnzeigeConfig(..))
import Zug.UI.Gtk.MitWidget (MitWidget(erhalteWidget))

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

data PositionChange
    = PositionReset
    | PositionMove { deltaX :: Double -> Double, deltaY :: Double -> Double }

data Speed = Speed { vX :: Double, vY :: Double }

-- | test legacy events
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
legacyControllerNew :: IO Gtk.EventControllerLegacy
legacyControllerNew = do
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
    pure legacyController

-- | key event to move window
-- EventControllerKey:
--      keyReleased event is not able to suppress further event propagation (void return instead of Bool)
--
-- Has to be added to the Toplevel window!
moveControllerNew :: GleisAnzeige z -> IO Gtk.EventControllerLegacy
moveControllerNew gleisAnzeige = do
    moveController <- Gtk.eventControllerLegacyNew
    tvarPressedKeys <- newTVarIO HashSet.empty
    tvarScrolls <- newTVarIO HashSet.empty
    tvarLastScrolls <- newTVarIO (HashSet.empty, 0)
    tvarTime <- newTVarIO Nothing
    tvarSpeed <- newTVarIO Speed { vX = 0, vY = 0 }
    -- all units are given as used by 'Position' (unscaled)
    let acceleration = 1000         -- unit / s^2
        drag = 10                   -- 1 / s
        -- time a scroll event is kept alive
        scrollLifetime = 750000     -- µs
    widget <- erhalteWidget gleisAnzeige
    Gtk.widgetAddTickCallback widget $ \_widget frameClock -> do
        currentTime <- Gdk.frameClockGetFrameTime frameClock        -- µs
        changes <- atomically $ do
            maybeLastTime <- readTVar tvarTime
            writeTVar tvarTime $ Just currentTime
            case maybeLastTime of
                Nothing -> pure PositionMove { deltaX = const 0, deltaY = const 0 }
                (Just lastTime) -> do
                    Speed {vX = lastVX, vY = lastVY} <- readTVar tvarSpeed
                    pressedKeys <- readTVar tvarPressedKeys
                    currentScrolls <- readTVar tvarScrolls
                    (lastScrolls, lastScrollTime) <- readTVar tvarLastScrolls
                    -- there is no guaranteed stop event for scrolling
                    scrolls <- if null currentScrolls
                        && (lastScrollTime + scrollLifetime < currentTime)
                        then do
                            pure lastScrolls
                        else do
                            writeTVar tvarScrolls HashSet.empty
                            writeTVar tvarLastScrolls (currentScrolls, currentTime)
                            pure currentScrolls
                    if HashSet.member KeyHome pressedKeys
                        then do
                            writeTVar tvarSpeed Speed { vX = 0, vY = 0 }
                            pure PositionReset
                        else do
                            let delay_µs = fromIntegral $ currentTime - lastTime    -- µs
                                delay_s = 0.000001 * delay_µs                       -- s
                                directionAcceleration
                                    keyPositive
                                    scrollPositive
                                    keyNegative
                                    scrollNegative =
                                    ifPressedOrScroll keyPositive scrollPositive
                                    - ifPressedOrScroll keyNegative scrollNegative
                                    where
                                        ifPressedOrScroll key scroll
                                            | HashSet.member key pressedKeys
                                                || HashSet.member scroll scrolls =
                                                1
                                            | otherwise = 0
                                -- acceleration due to user input
                                unitAX =
                                    directionAcceleration KeyRight ScrollRight KeyLeft ScrollLeft
                                unitAY = directionAcceleration KeyDown ScrollDown KeyUp ScrollUp
                                unitLen = max 1 $ sqrt $ unitAX * unitAX + unitAY * unitAY
                                -- keep acceleration constant size, independent of direction
                                aX = acceleration * unitAX / unitLen   -- unit / s^2
                                aY = acceleration * unitAY / unitLen   -- unit / s^2
                                -- speed after acceleration
                                vX = lastVX + aX * delay_s - drag * lastVX * delay_s
                                vY = lastVY + aY * delay_s - drag * lastVY * delay_s
                                currentSpeed = Speed { vX, vY }
                                deltaX scale = vX * scale * delay_s    -- scaled units (as used by DrawingArea)
                                deltaY scale = vY * scale * delay_s    -- scaled units (as used by DrawingArea)
                            writeTVar tvarSpeed currentSpeed
                            pure PositionMove { deltaX, deltaY }
        case changes of
            PositionReset -> gleisAnzeigeConfig gleisAnzeige $ \config -> config { x = 0, y = 0 }
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
