{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

module Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
  (
#ifdef ZUGKONTROLLEGUI
    WidgetsTyp(..)
  , WidgetsTypReader
  , EventAusführen(..)
  , eventAusführen
  , ohneEvent
  , buttonEntfernenPackNew
  , buttonBearbeitenPackNew
  , MitAktionBearbeiten(..)
  , AktionBearbeitenReader(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, readTVarIO, writeTVar, swapTVar)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.GI.Gtk.Threading as Gtk
import Data.Kind (Type, Constraint)
import qualified GI.Gtk as Gtk

import qualified Zug.Language as Language
import Zug.Objekt (Objekt, ObjektElement(..))
import Zug.UI.Base (ObjektReader, ReaderFamilie, IOStatusAllgemein)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNew, buttonNewWithEventLabel, Packing(PackNatural)
                                 , paddingDefault, Position(End))
import Zug.UI.Gtk.Klassen (MitWidget())
import Zug.UI.Gtk.SpracheGui (MitSpracheGui(), SpracheGuiReader(), TVarSprachewechselAktionen)
import Zug.UI.StatusVar
       (MitStatusVar(), StatusVarReader(erhalteStatusVar), auswertenStatusVarIOStatus)

-- | Klasse für Widgets-Repräsentation von Objekt-Typen.
class (MitWidget s, ObjektElement s) => WidgetsTyp s where
    -- | Constraint, den ein 'MonadReader'-Typ erfüllen muss, damit entferneWidgets möglich ist.
    type ReaderConstraint s :: Type -> Constraint

    -- | Entferne Widgets inklusive aller Hilfswidgets aus den entsprechenden Boxen.
    entferneWidgets :: (MonadIO m, WidgetsTypReader r s m) => s -> m ()

    -- | 'Gtk.Box', in die der Entfernen-Knopf von 'buttonEntfernenPack' gepackt wird.
    -- Hier definiert, damit keine 'MitBox'-Instanz notwendig ist.
    boxButtonEntfernen :: s -> Gtk.Box

    -- | 'Gtk.Box', in die der Bearbeiten-Knopf von 'buttonBearbeitenPack' gepackt wird.
    -- Hier definiert, damit keine 'MitBox'-Instanz notwendig ist.
    --
    -- Wird kein expliziter Wert angegeben ist diese Funktion identisch zu 'boxButtonEntfernen'.
    boxButtonBearbeiten :: s -> Gtk.Box
    boxButtonBearbeiten = boxButtonEntfernen

    -- | Erhalte die 'TVar', die steuert welche Widgets bei 'Zug.UI.Gtk.SpracheGui.sprachwechsel'
    -- angepasst werden.
    tvarSprache :: s -> TVarSprachewechselAktionen

    -- | Erhalte die 'TVar', die steuert ob Events ausgeführt werden.
    tvarEvent :: s -> TVar EventAusführen

-- | 'ReaderConstraint' für einen 'MonadReader'.
type WidgetsTypReader r s m = (MonadReader r m, ReaderConstraint s r)

-- | Soll das zugehörige Event ausgeführt werden?
data EventAusführen
    = EventAusführen
    | EventIgnorieren
    deriving (Eq, Show)

-- | Führe ein Event aus oder ignoriere es.
eventAusführen :: (MonadIO m) => TVar EventAusführen -> m () -> m ()
eventAusführen tvar aktion = liftIO (readTVarIO tvar) >>= \case
    EventAusführen -> aktion
    EventIgnorieren -> pure ()

-- | Führe eine Gtk-Aktion ohne zugehöriges Event aus.
ohneEvent :: TVar EventAusführen -> IO () -> IO ()
ohneEvent tvarEventAusführen aktion = Gtk.postGUIASync $ do
    alterWert <- atomically $ swapTVar tvarEventAusführen EventIgnorieren
    aktion
    when (alterWert == EventAusführen)
        $ atomically
        $ writeTVar tvarEventAusführen EventAusführen

-- | Typen, die eine Aktion zum bearbeiten eines 'Objekt' enthalten.
class MitAktionBearbeiten r where
    aktionBearbeiten :: r -> Objekt -> IO ()

-- | Reader zu 'MitAktionBearbeiten'.
class (MonadReader r m, MitAktionBearbeiten r) => AktionBearbeitenReader r m | m -> r where
    erhalteAktionBearbeiten :: m (Objekt -> IO ())
    erhalteAktionBearbeiten = asks aktionBearbeiten

instance (MonadReader r m, MitAktionBearbeiten r) => AktionBearbeitenReader r m

-- | Neuen Entfernen-Knopf an das Ende der zugehörigen 'Box' hinzufügen.
-- Beim drücken werden 'entferneWidgets' und die übergebene 'IOStatusGui'-Aktion ausgeführt.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus
-- 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonEntfernenPackNew
    :: ( WidgetsTyp w
       , ObjektReader o m
       , ReaderConstraint w (ReaderFamilie o)
       , MitStatusVar (ReaderFamilie o) o
       , MitSpracheGui (ReaderFamilie o)
       , MonadIO m
       )
    => w
    -> IOStatusAllgemein o ()
    -> m Gtk.Button
buttonEntfernenPackNew w entfernenAktion = do
    statusVar <- erhalteStatusVar
    objektReader <- ask
    boxPackWidgetNew (boxButtonEntfernen w) PackNatural paddingDefault End
        $ buttonNewWithEventLabel (Just $ tvarSprache w) Language.entfernen
        $ flip runReaderT objektReader
        $ do
            auswertenStatusVarIOStatus entfernenAktion statusVar
            entferneWidgets w

-- | Neuen Bearbeiten-Knopf an das Ende der zugehörigen 'Box' hinzufügen.
-- Beim drücken wird 'bearbeitenAktion' ausgeführt.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus
-- 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonBearbeitenPackNew
    :: ( WidgetsTyp w
       , ObjektElement (ObjektTyp w)
       , AktionBearbeitenReader r m
       , SpracheGuiReader r m
       , MonadIO m
       )
    => w
    -> m Gtk.Button
buttonBearbeitenPackNew w = do
    aktion <- erhalteAktionBearbeiten
    boxPackWidgetNew (boxButtonBearbeiten w) PackNatural paddingDefault End
        $ buttonNewWithEventLabel (Just $ tvarSprache w) Language.bearbeiten
        $ aktion
        $ zuObjekt
        $ zuObjektTyp w
#endif
--
