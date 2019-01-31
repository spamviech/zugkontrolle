{-# LANGUAGE LambdaCase, NamedFieldPuns, InstanceSigs, RankNTypes, CPP #-}

{-|
Description : Erzeuge Buttons die nur sensitiv sind, wenn eine Bedingung erfüllt ist.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.GTK.FortfahrenWenn () where
#else
module Zug.UI.GTK.FortfahrenWenn (
                                -- * CheckButton
                                FortfahrenWennToggled(), fortfahrenWennToggledNew, aktiviereWennToggled,
                                MitCheckButton(..),
                                -- * Foldable
                                VielleichtRegistrierterCheckButton(), VRCheckButton, unregistriert, erhalteWidget, _Registriert, _Unregistriert,
                                fortfahrenWennToggledEmptyLinkedMVarNew, linkedMVarCheckButtons,
                                FortfahrenWennGefüllt(), fortfahrenWennGefülltEmptyLinkedMVarNew, linkedMVarElemente) where

-- Bibliotheken
import Graphics.UI.Gtk
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (maybe)
import Control.Lens (Traversal', Getter, Prism', Field2(..), (^.), (%%~))
import qualified Control.Lens as Lens
import Control.Monad.Trans (liftIO)
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar

-- | Fortfahren nur möglich, wenn mindestens ein CheckButton aktiviert ist
data FortfahrenWennToggled f t = FortfahrenWennToggled {fortfahren :: Button, checkButtons :: f t}

instance (Foldable f) => Foldable (FortfahrenWennToggled f) where
    foldMap :: Monoid m => (a -> m) -> FortfahrenWennToggled f a -> m
    foldMap f (FortfahrenWennToggled {checkButtons}) = foldMap f checkButtons

class MitCheckButton c where
    checkButton :: Getter c CheckButton

instance MitCheckButton CheckButton where
    checkButton :: Getter CheckButton CheckButton
    checkButton = Lens.to id

instance (MitCheckButton c) => MitCheckButton (VielleichtRegistrierterCheckButton c) where
    checkButton :: Getter (VielleichtRegistrierterCheckButton c) CheckButton
    checkButton = erhalteWidget . checkButton

instance (MitCheckButton c) => MitCheckButton (a, c) where
    checkButton :: Getter (a, c) CheckButton
    checkButton = _2 . checkButton

instance (MitCheckButton c) => MitCheckButton (a, c, b) where
    checkButton :: Getter (a, c, b) CheckButton
    checkButton = _2 . checkButton

-- | Konstruktor, wenn alle CheckButtons beim erzeugen bekannt sind
fortfahrenWennToggledNew :: (MitCheckButton c) => Button -> NonEmpty c -> IO (FortfahrenWennToggled NonEmpty c)
fortfahrenWennToggledNew fortfahren checkButtons = do
    let self = FortfahrenWennToggled {fortfahren, checkButtons}
    mapM_ (\c -> on (c ^. checkButton) toggled $ aktiviereWennToggled self) checkButtons
    aktiviereWennToggled self
    pure self

-- | Funktion zum manuellen überprüfen
aktiviereWennToggled :: (MitCheckButton c, Foldable f) => FortfahrenWennToggled f c -> IO ()
aktiviereWennToggled (FortfahrenWennToggled {fortfahren, checkButtons}) = aktiviereWennToggledAux fortfahren checkButtons

aktiviereWennToggledAux :: (MitCheckButton c, Foldable f) => Button -> f c -> IO ()
aktiviereWennToggledAux button foldable = set button [widgetSensitive := False] >> foldM_ (aktiviereWennToggledAux' button) False foldable
    where
        aktiviereWennToggledAux' :: (MitCheckButton c) => Button -> Bool -> c -> IO Bool
        aktiviereWennToggledAux' _button (True)  _c  = pure True
        aktiviereWennToggledAux' button  (False) c   = get (c ^. checkButton) toggleButtonActive >>= \toggled -> when toggled (set button [widgetSensitive := True]) >> pure toggled

-- | Konstruktor, wenn zu überprüfende CheckButtons sich während der Laufzeit ändern können.
-- 
-- Es wird eine neue 'LinkedMVar' erzeugt, welche bei jedem setzten die Button-Senitivität überprüft.
fortfahrenWennToggledEmptyLinkedMVarNew :: (MitCheckButton c) => Button -> Traversal' a (VielleichtRegistrierterCheckButton c) -> Maybe (LinkedMVar a) -> IO (FortfahrenWennToggled LinkedMVar a)
fortfahrenWennToggledEmptyLinkedMVarNew fortfahren traversal maybeLMVar = do
    checkButtons <- liftIO $ (maybe newEmptyLinkedMVar appendEmptyLinkedMVar maybeLMVar) $ \updateAktion vielleichtUnregistriert -> do
        alleRegistriert <- traversal %%~ registrieren updateAktion $ vielleichtUnregistriert
        aktiviereWennToggledAux fortfahren $ Lens.toListOf (traversal . _Registriert) alleRegistriert
        pure alleRegistriert
    pure FortfahrenWennToggled {fortfahren, checkButtons}
        where
            registrieren :: (MitCheckButton c) => IO () -> VielleichtRegistrierterCheckButton c -> IO (VielleichtRegistrierterCheckButton c)
            registrieren    _updateAktion   reg@(Registriert _c)    = pure reg
            registrieren    updateAktion    (Unregistriert c)       = on (c ^. checkButton) toggled updateAktion >> pure (Registriert c)

linkedMVarCheckButtons :: Getter (FortfahrenWennToggled LinkedMVar a) (LinkedMVar a)
linkedMVarCheckButtons = Lens.to checkButtons

-- ** Either-ähnlicher Datentyp, zum unterscheiden von registrierten und unregistrierten CheckButtons
data VielleichtRegistrierterCheckButton c   = Unregistriert c
                                            | Registriert   c
                                                            deriving (Eq)
type VRCheckButton = VielleichtRegistrierterCheckButton CheckButton

-- Prism wie bei Either
_Registriert :: Prism' (VielleichtRegistrierterCheckButton c) c
_Registriert = Lens.prism Registriert updateRegistriert
    where
        updateRegistriert :: ((VielleichtRegistrierterCheckButton c) -> Either (VielleichtRegistrierterCheckButton c) c)
        updateRegistriert   vielleicht@(Unregistriert _c)   = Left vielleicht
        updateRegistriert   (Registriert c)                 = Right c

_Unregistriert :: Prism' (VielleichtRegistrierterCheckButton c) c
_Unregistriert = Lens.prism Unregistriert updateUnregistriert
    where
        updateUnregistriert :: ((VielleichtRegistrierterCheckButton c) -> Either (VielleichtRegistrierterCheckButton c) c)
        updateUnregistriert   vielleicht@(Registriert _rc)  = Left vielleicht
        updateUnregistriert   (Unregistriert c)             = Right c

-- exportierte Konstruktoren
unregistriert :: c -> VielleichtRegistrierterCheckButton c
unregistriert = Unregistriert

erhalteWidget :: Getter (VielleichtRegistrierterCheckButton c) c
erhalteWidget = Lens.to erhalteWidgetAux
    where
        erhalteWidgetAux :: VielleichtRegistrierterCheckButton c -> c
        erhalteWidgetAux (Unregistriert c) = c
        erhalteWidgetAux (Registriert c)   = c

-- | Fortfahren nur möglich, wenn mindesten ein Element vorhanden ist
data FortfahrenWennGefüllt t a = FortfahrenWennGefüllt {fortfahrenGefüllt :: Button, elemente :: LinkedMVar (t a)}

-- | Konstruktor erzeugt neue 'LinkedMVar', welche bei jedem Setzen die Button-Sensitivität überprüft.
fortfahrenWennGefülltEmptyLinkedMVarNew :: (Foldable t) => Button -> Maybe (LinkedMVar (t a)) -> IO (FortfahrenWennGefüllt t a)
fortfahrenWennGefülltEmptyLinkedMVarNew fortfahrenGefüllt maybeLMVar = do
    elemente <- liftIO $ (maybe newEmptyLinkedMVar appendEmptyLinkedMVar maybeLMVar) $ \_updateAktion fa -> aktiviereWennGefülltAux fortfahrenGefüllt fa >> pure fa
    pure FortfahrenWennGefüllt {fortfahrenGefüllt, elemente}

linkedMVarElemente :: Getter (FortfahrenWennGefüllt t a) (LinkedMVar (t a))
linkedMVarElemente = Lens.to elemente

aktiviereWennGefülltAux :: (Foldable f) => Button -> f a -> IO ()
aktiviereWennGefülltAux button foldable = set button [widgetSensitive := not $ null foldable]
#endif