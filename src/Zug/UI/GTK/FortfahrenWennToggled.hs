{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    FortfahrenWennToggledTMVar(), tmvarCheckButtons, foldCheckButtons, fortfahrenWennToggledTMVarNew, aktiviereWennToggledTMVar,
    -- * Assoziierter 'CheckButton' zu einem 'FortfahrenWennToggled'/'FortfahrenWennToggledTMvar'
    RegistrierterCheckButton, MitRegistrierterCheckButton(..), registrierterCheckButtonNew,
    mitRegistrierterCheckButton, registrierterCheckButton, registrierterCheckButtonToggled) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, TMVar, readTMVar)
import qualified Control.Lens as Lens
import Control.Monad (foldM_, forM_, forM)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Hilfsfunktionen (buttonNewWithEventLabel,)
import Zug.UI.Gtk.Klassen (MitWidget(..))

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

-- | Konstruktor, wenn alle 'CheckButton's beim erzeugen bekannt sind.
fortfahrenWennToggledNew :: (MonadIO m) =>
    Text -> IO () -> NonEmpty Text -> m FortfahrenWennToggled
fortfahrenWennToggledNew label action checkButtonNames = liftIO $ do
    fortfahren <- buttonNewWithEventLabel label action
    checkButtons <- forM checkButtonNames $ fmap RegistrierterCheckButton . Gtk.checkButtonNewWithLabel
    let fortfahrenWennToggled = FortfahrenWennToggled {fortfahren, checkButtons}
    forM_ checkButtons $ \(RegistrierterCheckButton checkButton) ->
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
            toggled <- registrierterCheckButtonToggled (erhalteRegistrierterCheckButton c)
            Gtk.set button [Gtk.widgetSensitive := toggled]
            pure toggled

data FortfahrenWennToggledTMVar a c
    = FortfahrenWennToggledTMVar {
        fortfahrenTMVar :: Gtk.Button,
        tmvarCheckButtons :: TMVar a,
        foldCheckButtons :: Lens.Fold a c}

instance Eq (FortfahrenWennToggledTMVar a c) where
    (==) :: FortfahrenWennToggledTMVar a c -> FortfahrenWennToggledTMVar a c -> Bool
    (==)
        FortfahrenWennToggledTMVar {fortfahrenTMVar = fortfahrenTMVar0, tmvarCheckButtons = tmvarCheckButtons0}
        FortfahrenWennToggledTMVar {fortfahrenTMVar = fortfahrenTMVar1, tmvarCheckButtons = tmvarCheckButtons1}
            = (fortfahrenTMVar0 == fortfahrenTMVar1) && (tmvarCheckButtons0 == tmvarCheckButtons1)

instance MitWidget (FortfahrenWennToggledTMVar a c)  where
    erhalteWidget :: FortfahrenWennToggledTMVar a c -> Gtk.Widget
    erhalteWidget = erhalteWidget . fortfahrenTMVar

-- | Konstruktor, wenn zu überprüfende 'CheckButton's sich während der Laufzeit ändern können.
fortfahrenWennToggledTMVarNew :: (MonadIO m, MitRegistrierterCheckButton c) =>
    Text -> IO () -> Lens.Fold a c -> TMVar a -> m (FortfahrenWennToggledTMVar a c)
fortfahrenWennToggledTMVarNew label action foldCheckButtons tmvarCheckButtons = liftIO $ do
    fortfahrenTMVar <- buttonNewWithEventLabel label action
    Gtk.set fortfahrenTMVar [Gtk.widgetSensitive := False]
    pure FortfahrenWennToggledTMVar {fortfahrenTMVar, tmvarCheckButtons, foldCheckButtons}

-- | Funktion zum manuellen überprüfen
aktiviereWennToggledTMVar :: (MonadIO m, MitRegistrierterCheckButton c) => FortfahrenWennToggledTMVar a c -> m ()
aktiviereWennToggledTMVar FortfahrenWennToggledTMVar {fortfahrenTMVar, tmvarCheckButtons, foldCheckButtons} = liftIO $ do
    checkButtons <- atomically $ readTMVar tmvarCheckButtons
    aktiviereWennToggledAux fortfahrenTMVar $ Lens.toListOf foldCheckButtons checkButtons

-- | 'CheckButton', welcher mit einem 'ForfahrenWennToggled' assoziiert ist.
newtype RegistrierterCheckButton
    = RegistrierterCheckButton
        Gtk.CheckButton
            deriving (Eq, MitWidget)

-- | Konstruktor für neuen 'RegistrierterCheckButton'
registrierterCheckButtonNew :: (MonadIO m, MitRegistrierterCheckButton c) =>
    Text -> FortfahrenWennToggledTMVar a c -> m RegistrierterCheckButton
registrierterCheckButtonNew label fortfahrenWennToggled = liftIO $ do
    checkButton <- Gtk.checkButtonNewWithLabel label
    Gtk.on checkButton Gtk.toggled $ aktiviereWennToggledTMVar fortfahrenWennToggled
    pure $ RegistrierterCheckButton checkButton

-- | Überprüfe ob ein 'RegistrierterCheckButtonAuswahl' aktuell gedrückt ist
registrierterCheckButtonToggled :: (MonadIO m) => RegistrierterCheckButton -> m Bool
registrierterCheckButtonToggled
    (RegistrierterCheckButton checkButton)
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