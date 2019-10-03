{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
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
    fortfahrenWennToggledTMVarNew, tmvarCheckButtons, aktiviereWennToggledTMVar,
    RegistrierterCheckButton(), registrierterCheckButtonNew, MitRegistrierterCheckButton(..)) where

-- Bibliotheken
import qualified Control.Lens as Lens
import Control.Monad (foldM_, forM_)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Graphics.UI.Gtk (Widget, Button, CheckButton, set, get, AttrOp(..),
                        toggleButtonActive, widgetSensitive, on, toggled, checkButtonNew)
import Control.Concurrent.STM (atomically, TMVar, readTMVar)
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Hilfsfunktionen (buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), MitButton(..), MitToggleButton(), MitCheckButton(..))

-- | Fortfahren nur möglich, wenn mindestens ein 'CheckButton' aktiviert ist.
-- Ansonsten wird die Sensitivität des 'Button's deaktiviert.
data FortfahrenWennToggled f a where
    FortfahrenWennToggled :: {
        fortfahren :: Button,
        checkButtons :: NonEmpty a}
            -> FortfahrenWennToggled NonEmpty a
    FortfahrenWennToggledTMVar :: {
        fortfahrenTMVar :: Button,
        tmvarCheckButtons :: TMVar a,
        fold :: Lens.Fold a RegistrierterCheckButton}
            -> FortfahrenWennToggled TMVar a 

instance MitWidget (FortfahrenWennToggled f t) where
    erhalteWidget :: FortfahrenWennToggled f t -> Widget
    erhalteWidget   FortfahrenWennToggled {fortfahren}              = erhalteWidget fortfahren
    erhalteWidget   FortfahrenWennToggledTMVar {fortfahrenTMVar}    = erhalteWidget fortfahrenTMVar

instance Foldable (FortfahrenWennToggled NonEmpty) where
    foldMap :: Monoid m => (a -> m) -> FortfahrenWennToggled NonEmpty a -> m
    foldMap
        funktion
        FortfahrenWennToggled {checkButtons}
            = foldMap funktion checkButtons

-- | Konstruktor, wenn alle 'CheckButton's beim erzeugen bekannt sind.
fortfahrenWennToggledNew :: (MitCheckButton c, MonadIO m) => Text -> IO () -> NonEmpty c -> m (FortfahrenWennToggled NonEmpty c)
fortfahrenWennToggledNew label action checkButtons = liftIO $ do
    fortfahren <- buttonNewWithEventLabel label action
    let fortfahrenWennToggled = FortfahrenWennToggled {fortfahren, checkButtons}
    forM_ checkButtons $ \c -> on (erhalteCheckButton c) toggled $ aktiviereWennToggled fortfahrenWennToggled
    aktiviereWennToggled fortfahrenWennToggled
    pure fortfahrenWennToggled

-- | Funktion zum manuellen überprüfen
aktiviereWennToggled :: (MitCheckButton c, MonadIO m) => FortfahrenWennToggled NonEmpty c -> m ()
aktiviereWennToggled
    FortfahrenWennToggled {fortfahren, checkButtons}
        = aktiviereWennToggledAux fortfahren checkButtons

aktiviereWennToggledAux :: (MitCheckButton c, Foldable f, MonadIO m) => Button -> f c -> m ()
aktiviereWennToggledAux button foldable = liftIO $ foldM_ (aktiviereWennToggledCheckButton button) False foldable
    where
        aktiviereWennToggledCheckButton :: (MitCheckButton c) => Button -> Bool -> c -> IO Bool
        aktiviereWennToggledCheckButton _button True    _c  = pure True
        aktiviereWennToggledCheckButton button  False   c   = do
            toggled <- get (erhalteCheckButton c) toggleButtonActive
            set button [widgetSensitive := toggled]
            pure toggled

-- | Konstruktor, wenn zu überprüfende 'CheckButton's sich während der Laufzeit ändern können.
fortfahrenWennToggledTMVarNew :: (MonadIO m) =>
    Text -> IO () -> Lens.Fold a RegistrierterCheckButton -> TMVar a -> m (FortfahrenWennToggled TMVar a)
fortfahrenWennToggledTMVarNew label action fold tmvarCheckButtons = liftIO $ do
    fortfahrenTMVar <- buttonNewWithEventLabel label action
    set fortfahrenTMVar [widgetSensitive := False]
    pure FortfahrenWennToggledTMVar {fortfahrenTMVar, tmvarCheckButtons, fold}

-- | Funktion zum manuellen überprüfen
aktiviereWennToggledTMVar :: (MonadIO m) => FortfahrenWennToggled TMVar a -> m ()
aktiviereWennToggledTMVar FortfahrenWennToggledTMVar {fortfahrenTMVar, tmvarCheckButtons, fold} = liftIO $ do
    checkButtons <- atomically $ readTMVar tmvarCheckButtons
    aktiviereWennToggledAux fortfahrenTMVar $ Lens.toListOf fold checkButtons

-- | 'MitCheckButton', der vielleicht schon registriert wurde
newtype RegistrierterCheckButton = RegistrierterCheckButton CheckButton
        deriving (Eq, MitWidget, MitContainer, MitButton, MitToggleButton, MitCheckButton)

-- | Konstruktor für neuen 'RegistrierterCheckButton'
registrierterCheckButtonNew :: (MonadIO m) => FortfahrenWennToggled TMVar a -> m RegistrierterCheckButton
registrierterCheckButtonNew fortfahrenWennToggled = liftIO $ do
    checkButton <- checkButtonNew
    on checkButton toggled $ aktiviereWennToggledTMVar fortfahrenWennToggled
    pure $ RegistrierterCheckButton checkButton

-- | Klasse für Typen mit 'RegistrierterCheckButton'
class (MitCheckButton c) => MitRegistrierterCheckButton c where
    erhalteRegistrierterCheckButton :: c -> RegistrierterCheckButton
    mitRegistrierterCheckButton :: (MonadIO m) => (RegistrierterCheckButton -> m a) -> c -> m a
    mitRegistrierterCheckButton funktion = funktion . erhalteRegistrierterCheckButton

instance MitRegistrierterCheckButton RegistrierterCheckButton where
    erhalteRegistrierterCheckButton :: RegistrierterCheckButton -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton = id
#endif