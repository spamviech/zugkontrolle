{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}

{-|
Description : Erzeuge Buttons die nur sensitiv sind, wenn eine Bedingung erfüllt ist.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.FortfahrenWennToggled () where
#else
module Zug.UI.Gtk.FortfahrenWennToggled (
    -- * Feste Anzahl an 'CheckButton's
    FortfahrenWennToggled(), checkButtons,
    fortfahrenWennToggledNew, aktiviereWennToggled,
    -- * Variable Anzahl an 'CheckButton's
    FortfahrenWennToggledVar(), varCheckButtons, foldCheckButtons,
    fortfahrenWennToggledVarNew, aktiviereWennToggledVar,
    -- * Assoziierter 'CheckButton' zu einem 'FortfahrenWennToggled'/'FortfahrenWennToggledTMvar'
    RegistrierterCheckButton, MitRegistrierterCheckButton(..), registrierterCheckButtonNew,
    mitRegistrierterCheckButton, registrierterCheckButton, registrierterCheckButtonToggled) where

-- Bibliotheken
import qualified Control.Lens as Lens
import Control.Monad (foldM_, forM_, forM)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.Language (Sprache())
import Zug.UI.Gtk.Hilfsfunktionen (widgetShowNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), MitButton(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), verwendeSpracheGui)

-- | Fortfahren nur möglich, wenn mindestens ein 'CheckButton' aktiviert ist.
-- Ansonsten wird die Sensitivität des 'Button's deaktiviert.
data FortfahrenWennToggled
    = FortfahrenWennToggled {
        fortfahren :: Gtk.Button,
        checkButtons :: NonEmpty RegistrierterCheckButton}
    deriving (Eq)

instance MitWidget FortfahrenWennToggled  where
    erhalteWidget :: FortfahrenWennToggled -> Gtk.Widget
    erhalteWidget = erhalteWidget . fortfahren

instance MitContainer FortfahrenWennToggled  where
    erhalteContainer :: FortfahrenWennToggled -> Gtk.Container
    erhalteContainer = erhalteContainer . fortfahren

instance MitButton FortfahrenWennToggled  where
    erhalteButton :: FortfahrenWennToggled -> Gtk.Button
    erhalteButton = erhalteButton . fortfahren

-- | Konstruktor, wenn alle 'CheckButton's beim erzeugen bekannt sind.
fortfahrenWennToggledNew :: (SpracheGuiReader r m, MonadIO m) =>
    (Sprache -> Text) -> NonEmpty (Sprache -> Text) -> m FortfahrenWennToggled
fortfahrenWennToggledNew label checkButtonNames = do
    fortfahren <- liftIO Gtk.buttonNew
    verwendeSpracheGui $ \sprache -> Gtk.set fortfahren [Gtk.buttonLabel := label sprache]
    checkButtons <- forM checkButtonNames $ \name -> do
        checkButton <- liftIO $ widgetShowNew Gtk.checkButtonNew
        verwendeSpracheGui $ \sprache -> Gtk.set checkButton [Gtk.buttonLabel := name sprache]
        pure $ RegistrierterCheckButton checkButton
    let fortfahrenWennToggled = FortfahrenWennToggled {fortfahren, checkButtons}
    liftIO $ forM_ checkButtons $ \(RegistrierterCheckButton checkButton) ->
        Gtk.on checkButton Gtk.toggled $ aktiviereWennToggled fortfahrenWennToggled
    aktiviereWennToggled fortfahrenWennToggled
    pure fortfahrenWennToggled

-- | Funktion zum manuellen überprüfen
aktiviereWennToggled :: (MonadIO m) => FortfahrenWennToggled -> m ()
aktiviereWennToggled
    FortfahrenWennToggled {fortfahren, checkButtons}
        = aktiviereWennToggledAux fortfahren checkButtons

aktiviereWennToggledAux :: (Foldable f, MitRegistrierterCheckButton c, MonadIO m) => Gtk.Button -> f c -> m ()
aktiviereWennToggledAux button foldable = liftIO $ foldM_ (aktiviereWennToggledCheckButton button) False foldable
    where
        aktiviereWennToggledCheckButton :: (MitRegistrierterCheckButton c) => Gtk.Button -> Bool -> c -> IO Bool
        aktiviereWennToggledCheckButton _button True    _c  = pure True
        aktiviereWennToggledCheckButton button  False   c   = do
            toggled <- registrierterCheckButtonToggled $ erhalteRegistrierterCheckButton c
            Gtk.set button [Gtk.widgetSensitive := toggled]
            pure toggled

data FortfahrenWennToggledVar a v c
    = FortfahrenWennToggledVar {
        fortfahrenVar :: Gtk.Button,
        varCheckButtons :: v,
        readVar :: v -> IO a,
        foldCheckButtons :: Lens.Fold a c}

instance (Eq v) => Eq (FortfahrenWennToggledVar a v c) where
    (==) :: FortfahrenWennToggledVar a v c -> FortfahrenWennToggledVar a v c -> Bool
    (==)
        FortfahrenWennToggledVar {fortfahrenVar = fortfahrenVar0, varCheckButtons = varCheckButtons0}
        FortfahrenWennToggledVar {fortfahrenVar = fortfahrenVar1, varCheckButtons = varCheckButtons1}
            = (fortfahrenVar0 == fortfahrenVar1) && (varCheckButtons0 == varCheckButtons1)

instance MitWidget (FortfahrenWennToggledVar a v c)  where
    erhalteWidget :: FortfahrenWennToggledVar a v c -> Gtk.Widget
    erhalteWidget = erhalteWidget . fortfahrenVar

instance MitContainer (FortfahrenWennToggledVar a v c)  where
    erhalteContainer :: FortfahrenWennToggledVar a v c -> Gtk.Container
    erhalteContainer = erhalteContainer . fortfahrenVar

instance MitButton (FortfahrenWennToggledVar a v c)  where
    erhalteButton :: FortfahrenWennToggledVar a v c -> Gtk.Button
    erhalteButton = erhalteButton . fortfahrenVar

-- | Konstruktor, wenn zu überprüfende 'CheckButton's sich während der Laufzeit ändern können.
fortfahrenWennToggledVarNew :: (MonadIO m, SpracheGuiReader r m, MitRegistrierterCheckButton c) =>
    (Sprache -> Text) -> Lens.Fold a c -> (v -> IO a) -> v -> m (FortfahrenWennToggledVar a v c)
fortfahrenWennToggledVarNew label foldCheckButtons readVar varCheckButtons = do
    fortfahrenVar <- liftIO $ do
        fortfahrenVar <- Gtk.buttonNew
        Gtk.set fortfahrenVar [Gtk.widgetSensitive := False]
        pure fortfahrenVar
    verwendeSpracheGui $ \sprache -> Gtk.set fortfahrenVar [Gtk.buttonLabel :=  label sprache]
    pure FortfahrenWennToggledVar {fortfahrenVar, readVar, varCheckButtons, foldCheckButtons}

-- | Funktion zum manuellen überprüfen
aktiviereWennToggledVar :: (MonadIO m, MitRegistrierterCheckButton c) => FortfahrenWennToggledVar a v c -> m ()
aktiviereWennToggledVar FortfahrenWennToggledVar {fortfahrenVar, varCheckButtons, readVar, foldCheckButtons}
    = liftIO $ do
        checkButtons <- readVar varCheckButtons
        aktiviereWennToggledAux fortfahrenVar $ Lens.toListOf foldCheckButtons checkButtons

-- | 'CheckButton', welcher mit einem 'ForfahrenWennToggled' assoziiert ist.
newtype RegistrierterCheckButton
    = RegistrierterCheckButton
        Gtk.CheckButton
            deriving (Eq, MitWidget)

-- | Konstruktor für neuen 'RegistrierterCheckButton'
registrierterCheckButtonNew :: (MonadIO m, SpracheGuiReader r m, MitRegistrierterCheckButton c) =>
    (Sprache -> Text) -> FortfahrenWennToggledVar a v c -> m RegistrierterCheckButton
registrierterCheckButtonNew label fortfahrenWennToggled = do
    checkButton <- liftIO $ do
        checkButton <- widgetShowNew $ Gtk.checkButtonNew
        Gtk.on checkButton Gtk.toggled $ aktiviereWennToggledVar fortfahrenWennToggled
        pure checkButton
    verwendeSpracheGui $ \sprache -> Gtk.set checkButton [Gtk.buttonLabel := label sprache]
    pure $ RegistrierterCheckButton checkButton

-- | Überprüfe ob ein 'RegistrierterCheckButtonAuswahl' aktuell gedrückt ist
registrierterCheckButtonToggled :: (MitRegistrierterCheckButton r, MonadIO m) => r -> m Bool
registrierterCheckButtonToggled
    (erhalteRegistrierterCheckButton -> (RegistrierterCheckButton checkButton))
        = liftIO $ Gtk.get checkButton Gtk.toggleButtonActive

-- | Klasse für Typen mit 'RegistrierterCheckButton'
class (MitWidget c) => MitRegistrierterCheckButton c where
    erhalteRegistrierterCheckButton :: c -> RegistrierterCheckButton

-- | Führe eine Aktion mit einem 'MitRegistrierterCheckButton' aus
mitRegistrierterCheckButton :: (MitRegistrierterCheckButton c) => (RegistrierterCheckButton -> b) -> c -> b
mitRegistrierterCheckButton funktion = funktion . erhalteRegistrierterCheckButton
-- | 'Lens.Getter' analog zu 'erhalteRegistrierterCheckButton'
registrierterCheckButton :: (MitRegistrierterCheckButton c) => Lens.Getter c RegistrierterCheckButton
registrierterCheckButton = Lens.to erhalteRegistrierterCheckButton

instance MitRegistrierterCheckButton RegistrierterCheckButton where
    erhalteRegistrierterCheckButton :: RegistrierterCheckButton -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton = id
#endif