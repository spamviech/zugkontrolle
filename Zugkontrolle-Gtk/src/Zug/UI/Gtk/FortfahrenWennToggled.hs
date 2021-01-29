{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description : Erzeuge Buttons die nur sensitiv sind, wenn eine Bedingung erfüllt ist.
-}
module Zug.UI.Gtk.FortfahrenWennToggled
  ( -- * Feste Anzahl an 'CheckButton's
    FortfahrenWennToggled()
  , checkButtons
  , fortfahrenWennToggledNew
  , aktiviereWennToggled
    -- * Variable Anzahl an 'CheckButton's
  , FortfahrenWennToggledVar()
  , varCheckButtons
  , getCheckButtons
  , fortfahrenWennToggledVarNew
  , aktiviereWennToggledVar
    -- * Assoziierter 'CheckButton' zu einem 'FortfahrenWennToggled'/'FortfahrenWennToggledTMvar'
  , RegistrierterCheckButton
  , MitRegistrierterCheckButton(..)
  , registrierterCheckButtonNew
  , mitRegistrierterCheckButton
  , registrierterCheckButtonToggled
  , registrierterCheckButtonSetToggled
  ) where

import Control.Monad (foldM_, forM_, forM)
import Control.Monad.Trans (MonadIO())
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import Zug.Language (Sprache())
import Zug.UI.Gtk.Hilfsfunktionen (widgetShowNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), MitButton(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), verwendeSpracheGui, TVarSprachewechselAktionen)

-- | Fortfahren nur möglich, wenn mindestens ein 'CheckButton' aktiviert ist.
-- Ansonsten wird die Sensitivität des 'Button's deaktiviert.
data FortfahrenWennToggled =
    FortfahrenWennToggled
    { fortfahren :: Gtk.Button
    , checkButtonsRec :: NonEmpty RegistrierterCheckButton
    }
    deriving (Eq)

-- | Erhalte die mit einem 'FortfahrenWennToggled' assoziierten 'RegistrierterCheckButton's.
checkButtons :: FortfahrenWennToggled -> NonEmpty RegistrierterCheckButton
checkButtons = checkButtonsRec

instance MitWidget FortfahrenWennToggled where
    erhalteWidget :: (MonadIO m) => FortfahrenWennToggled -> m Gtk.Widget
    erhalteWidget = erhalteWidget . fortfahren

instance MitContainer FortfahrenWennToggled where
    erhalteContainer :: (MonadIO m) => FortfahrenWennToggled -> m Gtk.Container
    erhalteContainer = erhalteContainer . fortfahren

instance MitButton FortfahrenWennToggled where
    erhalteButton :: (MonadIO m) => FortfahrenWennToggled -> m Gtk.Button
    erhalteButton = erhalteButton . fortfahren

-- | Konstruktor, wenn alle 'CheckButton's beim erzeugen bekannt sind.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Buttons aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fortfahrenWennToggledNew
    :: (SpracheGuiReader r m, MonadIO m)
    => Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> NonEmpty (Sprache -> Text)
    -> m FortfahrenWennToggled
fortfahrenWennToggledNew maybeTVar label checkButtonNames = do
    fortfahren <- Gtk.buttonNew
    verwendeSpracheGui maybeTVar $ \sprache -> Gtk.setButtonLabel fortfahren $ label sprache
    checkButtonsRec <- forM checkButtonNames $ \name -> do
        checkButton <- widgetShowNew Gtk.checkButtonNew
        verwendeSpracheGui maybeTVar $ \sprache -> Gtk.setButtonLabel checkButton $ name sprache
        pure $ RegistrierterCheckButton checkButton
    let fortfahrenWennToggled = FortfahrenWennToggled { fortfahren, checkButtonsRec }
    forM_ checkButtonsRec $ \(RegistrierterCheckButton checkButton)
        -> Gtk.onToggleButtonToggled checkButton $ aktiviereWennToggled fortfahrenWennToggled
    aktiviereWennToggled fortfahrenWennToggled
    pure fortfahrenWennToggled

-- | Funktion zum manuellen überprüfen
aktiviereWennToggled :: (MonadIO m) => FortfahrenWennToggled -> m ()
aktiviereWennToggled FortfahrenWennToggled {fortfahren, checkButtonsRec} =
    aktiviereWennToggledAux fortfahren checkButtonsRec

aktiviereWennToggledAux
    :: (Foldable f, MitRegistrierterCheckButton c, MonadIO m) => Gtk.Button -> f c -> m ()
aktiviereWennToggledAux button foldable = foldM_ aktiviereWennToggledCheckButton False foldable
    where
        aktiviereWennToggledCheckButton
            :: (MitRegistrierterCheckButton c, MonadIO m) => Bool -> c -> m Bool
        aktiviereWennToggledCheckButton True _c = pure True
        aktiviereWennToggledCheckButton False c = do
            toggled <- registrierterCheckButtonToggled $ erhalteRegistrierterCheckButton c
            Gtk.setWidgetSensitive button toggled
            pure toggled

-- | Fortfahren nur möglich, wenn mindestens ein 'CheckButton' aktiviert ist.
-- Ansonsten wird die Sensitivität des 'Button's deaktiviert.
-- Die 'CheckButtons' sind dabei in einer in 'IO' lesbaren Variablen /v/ gespeichert.
data FortfahrenWennToggledVar a v c =
    FortfahrenWennToggledVar
    { fortfahrenVar :: Gtk.Button
    , varCheckButtonsRec :: v
    , readVar :: forall m. (MonadIO m) => v -> m a
    , getCheckButtonsRec :: a -> [c]
    }

-- | Die Variable, in der die 'MitRegistrierterCheckButton' einer 'FortfahrenWennToggledVar' gespeichert werden.
varCheckButtons :: FortfahrenWennToggledVar a v c -> v
varCheckButtons = varCheckButtonsRec

-- | Erhalte eine List der 'MitRegistrierterCheckButton' von dem in der Variable gelesenen Wert.
getCheckButtons :: FortfahrenWennToggledVar a v c -> a -> [c]
getCheckButtons = getCheckButtonsRec

instance (Eq v) => Eq (FortfahrenWennToggledVar a v c) where
    (==) :: FortfahrenWennToggledVar a v c -> FortfahrenWennToggledVar a v c -> Bool
    (==)
        FortfahrenWennToggledVar
        {fortfahrenVar = fortfahrenVar0, varCheckButtonsRec = varCheckButtons0}
        FortfahrenWennToggledVar
        {fortfahrenVar = fortfahrenVar1, varCheckButtonsRec = varCheckButtons1} =
        (fortfahrenVar0 == fortfahrenVar1) && (varCheckButtons0 == varCheckButtons1)

instance MitWidget (FortfahrenWennToggledVar a v c) where
    erhalteWidget :: (MonadIO m) => FortfahrenWennToggledVar a v c -> m Gtk.Widget
    erhalteWidget = erhalteWidget . fortfahrenVar

instance MitContainer (FortfahrenWennToggledVar a v c) where
    erhalteContainer :: (MonadIO m) => FortfahrenWennToggledVar a v c -> m Gtk.Container
    erhalteContainer = erhalteContainer . fortfahrenVar

instance MitButton (FortfahrenWennToggledVar a v c) where
    erhalteButton :: (MonadIO m) => FortfahrenWennToggledVar a v c -> m Gtk.Button
    erhalteButton = erhalteButton . fortfahrenVar

-- | Konstruktor, wenn zu überprüfende 'CheckButton's sich während der Laufzeit ändern können.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Buttons aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fortfahrenWennToggledVarNew
    :: (MonadIO m, SpracheGuiReader r m, MitRegistrierterCheckButton c)
    => Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> (a -> [c])
    -> (forall mm. (MonadIO mm) => v -> mm a)
    -> v
    -> m (FortfahrenWennToggledVar a v c)
fortfahrenWennToggledVarNew maybeTVar label getCheckButtonsRec readVar varCheckButtonsRec = do
    fortfahrenVar <- Gtk.buttonNew
    Gtk.setWidgetSensitive fortfahrenVar False
    verwendeSpracheGui maybeTVar $ \sprache -> Gtk.setButtonLabel fortfahrenVar $ label sprache
    pure
        FortfahrenWennToggledVar { fortfahrenVar, readVar, varCheckButtonsRec, getCheckButtonsRec }

-- | Funktion zum manuellen überprüfen
aktiviereWennToggledVar
    :: (MonadIO m, MitRegistrierterCheckButton c) => FortfahrenWennToggledVar a v c -> m ()
aktiviereWennToggledVar
    FortfahrenWennToggledVar {fortfahrenVar, varCheckButtonsRec, readVar, getCheckButtonsRec} = do
    alleCheckButtons <- readVar varCheckButtonsRec
    aktiviereWennToggledAux fortfahrenVar $ getCheckButtonsRec alleCheckButtons

-- | 'CheckButton', welcher mit einem 'FortfahrenWennToggled' assoziiert ist.
newtype RegistrierterCheckButton = RegistrierterCheckButton Gtk.CheckButton
    deriving (Eq, MitWidget)

-- | Konstruktor für neuen 'RegistrierterCheckButton'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Buttons aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
registrierterCheckButtonNew
    :: (MonadIO m, SpracheGuiReader r m, MitRegistrierterCheckButton c)
    => Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> FortfahrenWennToggledVar a v c
    -> m RegistrierterCheckButton
registrierterCheckButtonNew maybeTVar label fortfahrenWennToggled = do
    checkButton <- widgetShowNew $ Gtk.checkButtonNew
    Gtk.onToggleButtonToggled checkButton $ aktiviereWennToggledVar fortfahrenWennToggled
    verwendeSpracheGui maybeTVar $ \sprache -> Gtk.setButtonLabel checkButton $ label sprache
    pure $ RegistrierterCheckButton checkButton

-- | Überprüfe ob ein 'RegistrierterCheckButtonAuswahl' aktuell gedrückt ist.
registrierterCheckButtonToggled :: (MitRegistrierterCheckButton r, MonadIO m) => r -> m Bool
registrierterCheckButtonToggled
    (erhalteRegistrierterCheckButton -> (RegistrierterCheckButton checkButton)) =
    Gtk.getToggleButtonActive checkButton

-- | Bestimme ob ein 'RegistrierterCheckButtonAuswahl' aktuell gedrückt ist.
registrierterCheckButtonSetToggled
    :: (MitRegistrierterCheckButton r, MonadIO m) => r -> Bool -> m ()
registrierterCheckButtonSetToggled
    (erhalteRegistrierterCheckButton -> (RegistrierterCheckButton checkButton)) =
    Gtk.setToggleButtonActive checkButton

-- | Klasse für Typen mit 'RegistrierterCheckButton'
class (MitWidget c) => MitRegistrierterCheckButton c where
    erhalteRegistrierterCheckButton :: c -> RegistrierterCheckButton

-- | Führe eine Aktion mit einem 'MitRegistrierterCheckButton' aus
mitRegistrierterCheckButton
    :: (MitRegistrierterCheckButton c) => (RegistrierterCheckButton -> b) -> c -> b
mitRegistrierterCheckButton funktion = funktion . erhalteRegistrierterCheckButton

instance MitRegistrierterCheckButton RegistrierterCheckButton where
    erhalteRegistrierterCheckButton :: RegistrierterCheckButton -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton = id
