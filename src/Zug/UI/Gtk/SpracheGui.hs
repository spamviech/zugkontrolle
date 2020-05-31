{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description: 'Sprache' mit IO-Aktionen, welche bei einem 'sprachwechsel' ausgeführt werden.
-}
module Zug.UI.Gtk.SpracheGui
  ( SpracheGui()
  , SpracheGuiReader(..)
  , MitSpracheGui(..)
  , spracheGuiNeu
  , sprachwechsel
  , verwendeSpracheGui
  , verwendeSpracheGuiFn
  ) where

import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, modifyTVar, writeTVar)
import Control.Monad (when, foldM)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans (MonadIO(..))

import Zug.Language (Sprache(), MitSprache(..))

-- | 'Sprache' mit IO-Aktionen, welche bei einem 'sprachwechsel' ausgeführt werden.
data SpracheGui =
    SpracheGui { tvarSprache :: TVar Sprache, sprachwechselAktionen :: TVar [AktionOderTVar] }

instance MitSprache SpracheGui where
    leseSprache :: (MonadIO m) => (Sprache -> m a) -> SpracheGui -> m a
    leseSprache f SpracheGui {tvarSprache} = liftIO (readTVarIO tvarSprache) >>= f

-- | Klasse für Typen mit Zugriff auf 'SpracheGui'.
class MitSpracheGui r where
    spracheGui :: (MonadIO m) => r -> m SpracheGui

instance MitSpracheGui SpracheGui where
    spracheGui :: (MonadIO m) => SpracheGui -> m SpracheGui
    spracheGui = pure

-- | Abkürzungen für Funktionen, die ein 'SpracheGui' benötigen.
class (MonadReader r m, MitSpracheGui r) => SpracheGuiReader r m where
    erhalteSpracheGui :: m SpracheGui

instance (MonadReader r m, MitSpracheGui r, MonadIO m) => SpracheGuiReader r m where
    erhalteSpracheGui :: m SpracheGui
    erhalteSpracheGui = ask >>= spracheGui

-- | Erzeuge ein neues 'SpracheGui' ohne 'sprachwechsel'-Aktionen.
spracheGuiNeu :: (MonadIO m) => Sprache -> m SpracheGui
spracheGuiNeu sprache = liftIO $ SpracheGui <$> newTVarIO sprache <*> newTVarIO []

-- | Abkürzung für eine 'Sprache'-abhängige Aktion, oder eine 'TVar' mit 'Maybe' einer Liste davon.
type AktionOderTVar = Either (Sprache -> IO ()) (TVar (Maybe [Sprache -> IO ()]))

-- | Wechsel die 'Sprache' eines 'SpracheGui' und führe alle zugehörigen 'IO'-Aktionen aus.
sprachwechsel :: (MonadIO m) => SpracheGui -> Sprache -> m ()
sprachwechsel SpracheGui {tvarSprache, sprachwechselAktionen} sprache = liftIO $ do
    sprachwechselAktionenAlt <- readTVarIO sprachwechselAktionen
    let ausführenOderLöschen :: [AktionOderTVar] -> AktionOderTVar -> IO [AktionOderTVar]
        ausführenOderLöschen acc rightTVar@(Right tvar) = readTVarIO tvar >>= \case
            -- Führe alle Aktionen in der tvar aus
            (Just aktionen) -> do
                mapM_ ($ sprache) aktionen
                pure $ rightTVar : acc
            -- Lösche deaktivierte Aktionen
            Nothing -> pure acc
        ausführenOderLöschen acc leftAktion@(Left aktion) = do
            -- Führe die Aktion aus
            aktion sprache
            pure $ leftAktion : acc
    sprachwechselAktionenNeu <- foldM ausführenOderLöschen [] sprachwechselAktionenAlt
    atomically $ do
        writeTVar sprachwechselAktionen sprachwechselAktionenNeu
        writeTVar tvarSprache sprache

-- | Führe die übergebene Aktion mit der aktellen 'Sprache' aus.
-- Speichere sie außerdem zum erneuten Aufruf bei einem 'sprachwechsel'.
--
-- Wenn eine 'TVar' übergeben wird gehören alle Aktionen darin zusammen.
-- Sobald ein 'sprachwechsel' durchgeführt wird während die 'TVar' als Wert
-- 'Nothing' hat wird die Aktion gelöscht.
-- Ansonsten werden alle Aktionen darin ausgeführt.
verwendeSpracheGui :: (SpracheGuiReader r m, MonadIO m)
                   => Maybe (TVar (Maybe [Sprache -> IO ()]))
                   -> (Sprache -> IO ())
                   -> m ()
verwendeSpracheGui maybeTVar neueAktion = do
    readerSpracheGui <- erhalteSpracheGui
    verwendeSpracheGuiFn readerSpracheGui maybeTVar neueAktion

-- | Wie 'verwendeSpracheGui' mit explizit übergebenem 'SpracheGui'.
verwendeSpracheGuiFn :: (MonadIO m)
                     => SpracheGui
                     -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                     -> (Sprache -> IO ())
                     -> m ()
verwendeSpracheGuiFn SpracheGui {tvarSprache, sprachwechselAktionen} maybeTVar neueAktion =
    liftIO $ do
        sprache <- readTVarIO tvarSprache
        neueAktion sprache
        case maybeTVar of
            (Just tvar) -> do
                -- füge TVar zu sprachwechselAktionen hinzu, wenn es die erste Aktion ist
                let appendTVar = atomically $ modifyTVar sprachwechselAktionen (Right tvar :)
                readTVarIO tvar >>= flip when appendTVar . maybe False null
                -- füge die neueAktion zur TVar hinzu
                atomically $ modifyTVar tvar $ fmap (neueAktion :)
            -- füge neueAktion als permanente Aktion zu sprachwechselAktionen hinzu
            Nothing -> atomically $ modifyTVar sprachwechselAktionen (Left neueAktion :)
