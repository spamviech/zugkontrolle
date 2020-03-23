{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

{-|
Description : Erzeuge Buttons die nur sensitiv sind, wenn eine Bedingung erfüllt ist.
-}
module Zug.UI.Gtk.FortfahrenWennToggled
  (
#ifdef ZUGKONTROLLEGUI
    -- * Feste Anzahl an 'CheckButton's
    FortfahrenWennToggled()
  , checkButtons
  , fortfahrenWennToggledNew
  , aktiviereWennToggled
    -- * Variable Anzahl an 'CheckButton's
  , FortfahrenWennToggledVar()
  , varCheckButtons
  , foldCheckButtons
  , fortfahrenWennToggledVarNew
  , aktiviereWennToggledVar
    -- * Assoziierter 'CheckButton' zu einem 'FortfahrenWennToggled'/'FortfahrenWennToggledTMvar'
  , RegistrierterCheckButton
  , MitRegistrierterCheckButton(..)
  , registrierterCheckButtonNew
  , mitRegistrierterCheckButton
  , registrierterCheckButton
  , registrierterCheckButtonToggled
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Lens as Lens
import Control.Monad (foldM_, forM_, forM)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Language (Sprache())
import Zug.UI.Gtk.Hilfsfunktionen (widgetShowNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), MitButton(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), verwendeSpracheGui)

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
    erhalteWidget :: FortfahrenWennToggled -> Gtk.Widget
    erhalteWidget = erhalteWidget . fortfahren

instance MitContainer FortfahrenWennToggled where
    erhalteContainer :: FortfahrenWennToggled -> Gtk.Container
    erhalteContainer = erhalteContainer . fortfahren

instance MitButton FortfahrenWennToggled where
    erhalteButton :: FortfahrenWennToggled -> Gtk.Button
    erhalteButton = erhalteButton . fortfahren

-- | Konstruktor, wenn alle 'CheckButton's beim erzeugen bekannt sind.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Buttons aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fortfahrenWennToggledNew
    :: (SpracheGuiReader r m, MonadIO m)
    => Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (Sprache -> Text)
    -> NonEmpty (Sprache -> Text)
    -> m FortfahrenWennToggled
fortfahrenWennToggledNew maybeTVar label checkButtonNames = do
    fortfahren <- liftIO Gtk.buttonNew
    verwendeSpracheGui maybeTVar
        $ \sprache -> Gtk.set fortfahren [Gtk.buttonLabel := label sprache]
    checkButtonsRec <- forM checkButtonNames $ \name -> do
        checkButton <- liftIO $ widgetShowNew Gtk.checkButtonNew
        verwendeSpracheGui maybeTVar
            $ \sprache -> Gtk.set checkButton [Gtk.buttonLabel := name sprache]
        pure $ RegistrierterCheckButton checkButton
    let fortfahrenWennToggled = FortfahrenWennToggled { fortfahren, checkButtonsRec }
    liftIO $ forM_ checkButtonsRec $ \(RegistrierterCheckButton checkButton)
        -> Gtk.on checkButton Gtk.toggled $ aktiviereWennToggled fortfahrenWennToggled
    aktiviereWennToggled fortfahrenWennToggled
    pure fortfahrenWennToggled

-- | Funktion zum manuellen überprüfen
aktiviereWennToggled :: (MonadIO m) => FortfahrenWennToggled -> m ()
aktiviereWennToggled FortfahrenWennToggled {fortfahren, checkButtonsRec} =
    aktiviereWennToggledAux fortfahren checkButtonsRec

aktiviereWennToggledAux
    :: (Foldable f, MitRegistrierterCheckButton c, MonadIO m) => Gtk.Button -> f c -> m ()
aktiviereWennToggledAux button foldable =
    liftIO $ foldM_ (aktiviereWennToggledCheckButton button) False foldable
    where
        aktiviereWennToggledCheckButton
            :: (MitRegistrierterCheckButton c) => Gtk.Button -> Bool -> c -> IO Bool
        aktiviereWennToggledCheckButton _button True _c = pure True
        aktiviereWennToggledCheckButton button False c = do
            toggled <- registrierterCheckButtonToggled $ erhalteRegistrierterCheckButton c
            Gtk.set button [Gtk.widgetSensitive := toggled]
            pure toggled

-- | Fortfahren nur möglich, wenn mindestens ein 'CheckButton' aktiviert ist.
-- Ansonsten wird die Sensitivität des 'Button's deaktiviert.
-- Die 'CheckButtons' sind dabei in einer in 'IO' lesbaren Variablen /v/ gespeichert.
data FortfahrenWennToggledVar a v c =
    FortfahrenWennToggledVar
    { fortfahrenVar :: Gtk.Button
    , varCheckButtonsRec :: v
    , readVar :: v -> IO a
    , foldCheckButtonsRec :: Lens.Fold a c
    }

-- | Die Variable, in der die 'RegistrierterCheckButton' einer 'FortfahrenWennToggledVar' gespeichert werden.
varCheckButtons :: FortfahrenWennToggledVar a v c -> v
varCheckButtons = varCheckButtonsRec

-- | Ein 'Lens.Fold' vom in der Variable gelesenen Wert auf 'MitRegistrierterCheckButton'.
foldCheckButtons :: FortfahrenWennToggledVar a v c -> Lens.Fold a c
foldCheckButtons = foldCheckButtonsRec

instance (Eq v) => Eq (FortfahrenWennToggledVar a v c) where
    (==) :: FortfahrenWennToggledVar a v c -> FortfahrenWennToggledVar a v c -> Bool
    (==)
        FortfahrenWennToggledVar
        {fortfahrenVar = fortfahrenVar0, varCheckButtonsRec = varCheckButtons0}
        FortfahrenWennToggledVar
        {fortfahrenVar = fortfahrenVar1, varCheckButtonsRec = varCheckButtons1} =
        (fortfahrenVar0 == fortfahrenVar1) && (varCheckButtons0 == varCheckButtons1)

instance MitWidget (FortfahrenWennToggledVar a v c) where
    erhalteWidget :: FortfahrenWennToggledVar a v c -> Gtk.Widget
    erhalteWidget = erhalteWidget . fortfahrenVar

instance MitContainer (FortfahrenWennToggledVar a v c) where
    erhalteContainer :: FortfahrenWennToggledVar a v c -> Gtk.Container
    erhalteContainer = erhalteContainer . fortfahrenVar

instance MitButton (FortfahrenWennToggledVar a v c) where
    erhalteButton :: FortfahrenWennToggledVar a v c -> Gtk.Button
    erhalteButton = erhalteButton . fortfahrenVar

-- | Konstruktor, wenn zu überprüfende 'CheckButton's sich während der Laufzeit ändern können.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Buttons aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fortfahrenWennToggledVarNew
    :: (MonadIO m, SpracheGuiReader r m, MitRegistrierterCheckButton c)
    => Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (Sprache -> Text)
    -> Lens.Fold a c
    -> (v -> IO a)
    -> v
    -> m (FortfahrenWennToggledVar a v c)
fortfahrenWennToggledVarNew maybeTVar label foldCheckButtonsRec readVar varCheckButtonsRec = do
    fortfahrenVar <- liftIO $ do
        fortfahrenVar <- Gtk.buttonNew
        Gtk.set fortfahrenVar [Gtk.widgetSensitive := False]
        pure fortfahrenVar
    verwendeSpracheGui maybeTVar
        $ \sprache -> Gtk.set fortfahrenVar [Gtk.buttonLabel := label sprache]
    pure
        FortfahrenWennToggledVar
        { fortfahrenVar
        , readVar
        , varCheckButtonsRec
        , foldCheckButtonsRec
        }

-- | Funktion zum manuellen überprüfen
aktiviereWennToggledVar
    :: (MonadIO m, MitRegistrierterCheckButton c) => FortfahrenWennToggledVar a v c -> m ()
aktiviereWennToggledVar
    FortfahrenWennToggledVar {fortfahrenVar, varCheckButtonsRec, readVar, foldCheckButtonsRec} =
    liftIO $ do
        checkButtons <- readVar varCheckButtonsRec
        aktiviereWennToggledAux fortfahrenVar $ Lens.toListOf foldCheckButtonsRec checkButtons

-- | 'CheckButton', welcher mit einem 'FortfahrenWennToggled' assoziiert ist.
newtype RegistrierterCheckButton = RegistrierterCheckButton Gtk.CheckButton
    deriving (Eq, MitWidget)

-- | Konstruktor für neuen 'RegistrierterCheckButton'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Buttons aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
registrierterCheckButtonNew
    :: (MonadIO m, SpracheGuiReader r m, MitRegistrierterCheckButton c)
    => Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (Sprache -> Text)
    -> FortfahrenWennToggledVar a v c
    -> m RegistrierterCheckButton
registrierterCheckButtonNew maybeTVar label fortfahrenWennToggled = do
    checkButton <- liftIO $ do
        checkButton <- widgetShowNew $ Gtk.checkButtonNew
        Gtk.on checkButton Gtk.toggled $ aktiviereWennToggledVar fortfahrenWennToggled
        pure checkButton
    verwendeSpracheGui maybeTVar
        $ \sprache -> Gtk.set checkButton [Gtk.buttonLabel := label sprache]
    pure $ RegistrierterCheckButton checkButton

-- | Überprüfe ob ein 'RegistrierterCheckButtonAuswahl' aktuell gedrückt ist
registrierterCheckButtonToggled :: (MitRegistrierterCheckButton r, MonadIO m) => r -> m Bool
registrierterCheckButtonToggled
    (erhalteRegistrierterCheckButton -> (RegistrierterCheckButton checkButton)) =
    liftIO $ Gtk.get checkButton Gtk.toggleButtonActive

-- | Klasse für Typen mit 'RegistrierterCheckButton'
class (MitWidget c) => MitRegistrierterCheckButton c where
    erhalteRegistrierterCheckButton :: c -> RegistrierterCheckButton

-- | Führe eine Aktion mit einem 'MitRegistrierterCheckButton' aus
mitRegistrierterCheckButton
    :: (MitRegistrierterCheckButton c) => (RegistrierterCheckButton -> b) -> c -> b
mitRegistrierterCheckButton funktion = funktion . erhalteRegistrierterCheckButton

-- | 'Lens.Getter' analog zu 'erhalteRegistrierterCheckButton'
registrierterCheckButton
    :: (MitRegistrierterCheckButton c) => Lens.Getter c RegistrierterCheckButton
registrierterCheckButton = Lens.to erhalteRegistrierterCheckButton

instance MitRegistrierterCheckButton RegistrierterCheckButton where
    erhalteRegistrierterCheckButton :: RegistrierterCheckButton -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton = id
#endif
    --
