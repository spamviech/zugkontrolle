{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

{-|
Description : Erzeuge Buttons die nur sensitiv sind, wenn eine Bedingung erfüllt ist.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.FortfahrenWennToggled () where
#else
module Zug.UI.Gtk.FortfahrenWennToggled (
    -- * Datentyp
    FortfahrenWennToggled(),
    -- * Feste Anzahl an 'CheckButton's
    fortfahrenWennToggledNew, aktiviereWennToggled,
    -- * Variable Anzahl an 'CheckButton's
    fortfahrenWennToggledTMVar, tmvarCheckButtons, aktiviereWennToggledTMVar,
    RegistrierterCheckButton(), RCheckButton, registrieren) where

-- Bibliotheken
import qualified Control.Lens as Lens
import Control.Monad (foldM_, forM_, when)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Graphics.UI.Gtk (Widget, Button, CheckButton, set, get, AttrOp(..),
                        toggleButtonActive, widgetSensitive, on, toggled)
import Control.Concurrent.STM (atomically, TMVar, readTMVar)
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Hilfsfunktionen (buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), MitButton(..), MitToggleButton(), MitCheckButton(..))

-- | Fortfahren nur möglich, wenn mindestens ein 'CheckButton' aktiviert ist.
-- Ansonsten wird die Sensitivität des 'Button's deaktiviert.
data FortfahrenWennToggled f t = FortfahrenWennToggled {fortfahren :: Button, checkButtons :: f t}
                                    deriving (Eq)

instance MitWidget (FortfahrenWennToggled f t) where
    erhalteWidget :: FortfahrenWennToggled f t -> Widget
    erhalteWidget = erhalteWidget . fortfahren

instance (Foldable f) => Foldable (FortfahrenWennToggled f) where
    foldMap :: Monoid m => (a -> m) -> FortfahrenWennToggled f a -> m
    foldMap f (FortfahrenWennToggled {checkButtons}) = foldMap f checkButtons

-- | Konstruktor, wenn alle 'CheckButton's beim erzeugen bekannt sind.
fortfahrenWennToggledNew :: (MitCheckButton c) => Text -> IO () -> NonEmpty c -> IO (FortfahrenWennToggled NonEmpty c)
fortfahrenWennToggledNew label action checkButtons = do
    fortfahren <- buttonNewWithEventLabel label action
    let fortfahrenWennToggled = FortfahrenWennToggled {fortfahren, checkButtons}
    forM_ checkButtons $ \c -> on (erhalteCheckButton c) toggled $ aktiviereWennToggled fortfahrenWennToggled
    aktiviereWennToggled fortfahrenWennToggled
    pure fortfahrenWennToggled

-- | Funktion zum manuellen überprüfen
aktiviereWennToggled :: (MitCheckButton c, Foldable f) => FortfahrenWennToggled f c -> IO ()
aktiviereWennToggled
    FortfahrenWennToggled {fortfahren, checkButtons}
        = aktiviereWennToggledAux fortfahren checkButtons

aktiviereWennToggledAux :: (MitCheckButton c, Foldable f) => Button -> f c -> IO ()
aktiviereWennToggledAux button foldable = do
    set button [widgetSensitive := False]
    foldM_ (aktiviereWennToggledCheckButton button) False foldable
        where
            aktiviereWennToggledCheckButton :: (MitCheckButton c) => Button -> Bool -> c -> IO Bool
            aktiviereWennToggledCheckButton _button True    _c  = pure True
            aktiviereWennToggledCheckButton button  False   c   = do
                toggled <- get (erhalteCheckButton c) toggleButtonActive
                when toggled $ set button [widgetSensitive := True]
                pure toggled

-- | Konstruktor, wenn zu überprüfende 'CheckButton's sich während der Laufzeit ändern können.
fortfahrenWennToggledTMVar :: Text -> IO () -> TMVar a -> IO (FortfahrenWennToggled TMVar a)
fortfahrenWennToggledTMVar label action checkButtons = do
    fortfahren <- buttonNewWithEventLabel label action
    set fortfahren [widgetSensitive := False]
    pure FortfahrenWennToggled {fortfahren, checkButtons}

-- | Erhalte 'TVar' in der 'CheckButton's gespeichert werden
tmvarCheckButtons :: FortfahrenWennToggled TMVar a -> TMVar a
tmvarCheckButtons = checkButtons

-- | Funktion zum manuellen überprüfen
aktiviereWennToggledTMVar :: (MitCheckButton c) => FortfahrenWennToggled TMVar a -> Lens.Fold a (RegistrierterCheckButton c) -> IO ()
aktiviereWennToggledTMVar (FortfahrenWennToggled {fortfahren, checkButtons}) traversal = do
    currentCheckButtons <- atomically $ readTMVar checkButtons
    aktiviereWennToggledAux fortfahren $ Lens.toListOf traversal currentCheckButtons

-- | 'MitCheckButton', der vielleicht schon registriert wurde
newtype RegistrierterCheckButton c
    = RegistrierterCheckButton c
        deriving (Show, Eq, MitWidget, MitContainer, MitButton, MitToggleButton, MitCheckButton)
-- | 'RegistrierterCheckButton' spezialisiert auf 'CheckButton'
type RCheckButton = RegistrierterCheckButton CheckButton

-- | Konstruktor für 'RegistrierterCheckButton'
registrieren :: (MitCheckButton c) =>
    c -> FortfahrenWennToggled TMVar a -> Lens.Fold a (RegistrierterCheckButton c) -> IO (RegistrierterCheckButton c)
registrieren c fortfahrenWennToggled fold = do
    on (erhalteCheckButton c) toggled $ aktiviereWennToggledTMVar fortfahrenWennToggled fold
    pure $ RegistrierterCheckButton c
#endif