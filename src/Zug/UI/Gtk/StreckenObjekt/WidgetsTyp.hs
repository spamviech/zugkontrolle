{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
  ( WidgetsTyp(..)
  , WidgetsTypReader
  , EventAusführen(..)
  , eventAusführen
  , ohneEvent
  , buttonEntfernenPackNew
  ) where

import Control.Concurrent.STM (atomically, TVar, readTVarIO, writeTVar, swapTVar)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(ask), runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Kind (Type, Constraint)
import qualified Graphics.UI.Gtk as Gtk

import Zug.Language (Sprache())
import qualified Zug.Language as Language
import Zug.UI.Base (ReaderFamilie, IOStatusAllgemein)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNew, buttonNewWithEventLabel, Packing(PackNatural)
                                 , paddingDefault, Position(End))
import Zug.UI.Gtk.Klassen (MitWidget())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader())
import Zug.UI.StatusVar (StatusVarReader(erhalteStatusVar), auswertenStatusVarIOStatus)

-- | Klasse für Widgets-Repräsentation von Objekt-Typen.
class (MitWidget s) => WidgetsTyp s where
    -- | Assoziierter 'Objekt'-Typ
    type ObjektTyp s

    -- | Constraint, den ein 'MonadReader'-Typ erfüllen muss, damit entferneWidgets möglich ist.
    type ReaderConstraint s :: Type -> Constraint

    -- | Erhalte den eingebetteten 'ObjektTyp'.
    erhalteObjektTyp :: s -> ObjektTyp s

    -- | Entferne Widgets inklusive aller Hilfswidgets aus den entsprechenden Boxen.
    entferneWidgets :: (MonadIO m, WidgetsTypReader r s m) => s -> m ()

    -- | 'Gtk.Box', in die der Entfernen-Knopf von 'buttonEntfernenPack' gepackt wird.
    -- Hier definiert, damit keine 'MitBox'-Instanz notwendig ist.
    boxButtonEntfernen :: s -> Gtk.Box

    -- | Erhalte die 'TVar', die steuert welche Widgets bei 'Zug.UI.Gtk.SpracheGui.sprachwechsel'
    -- angepasst werden.
    tvarSprache :: s -> TVar (Maybe [Sprache -> IO ()])

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
ohneEvent tvarEventAusführen aktion = Gtk.postGUIAsync $ do
    alterWert <- atomically $ swapTVar tvarEventAusführen EventIgnorieren
    aktion
    when (alterWert == EventAusführen)
        $ atomically
        $ writeTVar tvarEventAusführen EventAusführen

-- | Neuen Entfernen-Knopf an das Ende der zugehörigen 'Box' hinzufügen.
-- Beim drücken werden 'entferneWidgets' und die übergebene 'IOStatusGui'-Aktion ausgeführt.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonEntfernenPackNew
    :: ( WidgetsTyp w
       , MonadReader (ReaderFamilie o) m
       , ReaderConstraint w (ReaderFamilie o)
       , StatusVarReader (ReaderFamilie o) o m
       , SpracheGuiReader (ReaderFamilie o) m
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