{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

{-|
Description : Erzeuge Buttons die nur sensitiv sind, wenn eine Bedingung erfüllt ist.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.GTK.FortfahrenWennToggled () where
#else
module Zug.UI.GTK.FortfahrenWennToggled (
    -- * Datentyp
    FortfahrenWennToggled(), fortfahrenButton,
    -- * Feste Anzahl an 'CheckButton's
    fortfahrenWennToggledNew, aktiviereWennToggled,
    MitCheckButton(..),
    -- * Variable Anzahl an 'CheckButton's
    fortfahrenWennToggledTMVar, tmvarCheckButtons, aktiviereWennToggledTMVar,
    RegistrierterCheckButton(), RCheckButton, registrieren, erhalteWidget) where

-- Bibliotheken
import Graphics.UI.Gtk (Button, CheckButton, set, get, AttrOp(..), toggleButtonActive, widgetSensitive, on, toggled)
import Control.Concurrent.STM (atomically, TMVar, readTMVar)
import Control.Monad (foldM_, forM_, when)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Lens (Fold, Getter, Field2(..), (^.), toListOf)
import qualified Control.Lens as Lens

-- | Fortfahren nur möglich, wenn mindestens ein 'CheckButton' aktiviert ist. Ansonsten wird die Sensitivität des 'Button's deaktiviert.
data FortfahrenWennToggled f t = FortfahrenWennToggled {fortfahren :: Button, checkButtons :: f t}

-- | Automatisch (de-)aktivierter 'Button'
fortfahrenButton :: Getter (FortfahrenWennToggled f t) Button
fortfahrenButton = Lens.to fortfahren

instance (Foldable f) => Foldable (FortfahrenWennToggled f) where
    foldMap :: Monoid m => (a -> m) -> FortfahrenWennToggled f a -> m
    foldMap f (FortfahrenWennToggled {checkButtons}) = foldMap f checkButtons

-- | Klasse für Typen, die einen ausgezeichneten 'CheckButton' haben
class MitCheckButton c where
    checkButton :: Getter c CheckButton

instance MitCheckButton CheckButton where
    checkButton :: Getter CheckButton CheckButton
    checkButton = Lens.to id

instance (MitCheckButton c) => MitCheckButton (RegistrierterCheckButton c) where
    checkButton :: Getter (RegistrierterCheckButton c) CheckButton
    checkButton = erhalteWidget . checkButton

instance (MitCheckButton c) => MitCheckButton (a, c) where
    checkButton :: Getter (a, c) CheckButton
    checkButton = _2 . checkButton

instance (MitCheckButton c) => MitCheckButton (a, c, b) where
    checkButton :: Getter (a, c, b) CheckButton
    checkButton = _2 . checkButton

-- | Konstruktor, wenn alle 'CheckButton's beim erzeugen bekannt sind.
fortfahrenWennToggledNew :: (MitCheckButton c) => Button -> NonEmpty c -> IO (FortfahrenWennToggled NonEmpty c)
fortfahrenWennToggledNew fortfahren checkButtons = do
    let fortfahrenWennToggled = FortfahrenWennToggled {fortfahren, checkButtons}
    forM_ checkButtons $ \c -> on (c ^. checkButton) toggled $ aktiviereWennToggled fortfahrenWennToggled
    aktiviereWennToggled fortfahrenWennToggled
    pure fortfahrenWennToggled

-- | Funktion zum manuellen überprüfen
aktiviereWennToggled :: (MitCheckButton c, Foldable f) => FortfahrenWennToggled f c -> IO ()
aktiviereWennToggled (FortfahrenWennToggled {fortfahren, checkButtons})
    = aktiviereWennToggledAux fortfahren checkButtons

aktiviereWennToggledAux :: (MitCheckButton c, Foldable f) => Button -> f c -> IO ()
aktiviereWennToggledAux button foldable = do
    set button [widgetSensitive := False]
    foldM_ (aktiviereWennToggledCheckButton button) False foldable
        where
            aktiviereWennToggledCheckButton :: (MitCheckButton c) => Button -> Bool -> c -> IO Bool
            aktiviereWennToggledCheckButton _button True    _c  = pure True
            aktiviereWennToggledCheckButton button  False   c   = do
                toggled <- get (c ^. checkButton) toggleButtonActive
                when toggled $ set button [widgetSensitive := True]
                pure toggled

-- | Konstruktor, wenn zu überprüfende 'CheckButton's sich während der Laufzeit ändern können.
fortfahrenWennToggledTMVar :: Button -> TMVar a -> IO (FortfahrenWennToggled TMVar a)
fortfahrenWennToggledTMVar fortfahren checkButtons = do
    set fortfahren [widgetSensitive := False]
    pure FortfahrenWennToggled {fortfahren, checkButtons}

-- | Erhalte 'TVar' in der 'CheckButton's gespeichert werden
tmvarCheckButtons :: Getter (FortfahrenWennToggled TMVar a) (TMVar a)
tmvarCheckButtons = Lens.to checkButtons

-- | Funktion zum manuellen überprüfen
aktiviereWennToggledTMVar :: (MitCheckButton c) => FortfahrenWennToggled TMVar a -> Fold a (RegistrierterCheckButton c) -> IO ()
aktiviereWennToggledTMVar (FortfahrenWennToggled {fortfahren, checkButtons}) traversal = do
    currentCheckButtons <- atomically $ readTMVar checkButtons
    aktiviereWennToggledAux fortfahren $ toListOf traversal currentCheckButtons

-- | 'MitCheckButton', der vielleicht schon registriert wurde
newtype RegistrierterCheckButton c = RegistrierterCheckButton {registrierterCheckButton :: c}
                                        deriving (Show, Eq)
-- | 'RegistrierterCheckButton' spezialisiert auf 'CheckButton'
type RCheckButton = RegistrierterCheckButton CheckButton

-- | Konstruktor für 'RegistrierterCheckButton'
registrieren :: (MitCheckButton c) => c -> FortfahrenWennToggled TMVar a -> Fold a (RegistrierterCheckButton c) -> IO (RegistrierterCheckButton c)
registrieren c fortfahrenWennToggled fold = do
    on (c ^. checkButton) toggled $ aktiviereWennToggledTMVar fortfahrenWennToggled fold
    pure $ RegistrierterCheckButton c

-- | Erhalte Widget eines 'RegistierterCheckButton', z.B. um es einem 'Container' hinzuzufügen
erhalteWidget :: Getter (RegistrierterCheckButton c) c
erhalteWidget = Lens.to registrierterCheckButton
#endif