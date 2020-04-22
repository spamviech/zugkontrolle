{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
  ( WidgetsTyp(..)
  , EventAusführen(..)
  , eventAusführen
  , ohneEvent
  ) where

import Control.Concurrent.STM (atomically, TVar, readTVarIO, writeTVar, swapTVar)
import Control.Monad (when)
import Control.Monad.Reader.Class (MonadReader())
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Kind (Type, Constraint)
import qualified Graphics.UI.Gtk as Gtk

import Zug.Language (Sprache())
import Zug.UI.Gtk.Klassen (MitWidget())

-- | Klasse für Widgets-Repräsentation von Objekt-Typen.
class (MitWidget s) => WidgetsTyp s where
    -- | Assoziierter 'Objekt'-Typ
    type ObjektTyp s

    -- | Constraint, den ein 'MonadReader'-Typ erfüllen muss, damit entferneWidgets möglich ist.
    type ReaderConstraint s :: Type -> Constraint

    -- | Erhalte den eingebetteten 'ObjektTyp'.
    erhalteObjektTyp :: s -> ObjektTyp s

    -- | Entferne Widgets inklusive aller Hilfswidgets aus den entsprechenden Boxen.
    entferneWidgets :: (MonadIO m, MonadReader r m, ReaderConstraint s r) => s -> m ()

    -- | 'Gtk.Box', in die der Entfernen-Knopf von 'buttonEntfernenPack' gepackt wird.
    -- Hier definiert, damit keine 'MitBox'-Instanz notwendig ist.
    boxButtonEntfernen :: s -> Gtk.Box

    -- | Erhalte die 'TVar', die steuert welche Widgets bei 'Zug.UI.Gtk.SpracheGui.sprachwechsel'
    -- angepasst werden.
    tvarSprache :: s -> TVar (Maybe [Sprache -> IO ()])

    -- | Erhalte die 'TVar', die steuert ob Events ausgeführt werden.
    tvarEvent :: s -> TVar EventAusführen

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