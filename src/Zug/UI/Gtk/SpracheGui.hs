{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

{-|
Description: Allgemeine Hilfsfunktionen
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.SpracheGui () where
#else
module Zug.UI.Gtk.SpracheGui (
    SpracheGui(), SpracheGuiReader(..), spracheGuiNeu, sprachwechsel, verwendeSpracheGui) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, modifyTVar)
import Control.Monad.Reader.Class (MonadReader())
import Control.Monad.Trans (MonadIO(..))
-- Abhängigkeit von anderen Modulen
import Zug.Language (Sprache(), MitSprache(..))

-- | 'Sprache' mit IO-Aktionen, welche bei einem 'sprachwechsel' ausgeführt werden.
data SpracheGui = SpracheGui {sprache :: Sprache, sprachwechselAktionen :: TVar [Sprache -> IO ()]}

instance MitSprache SpracheGui where
    leseSprache :: (Sprache -> a) -> SpracheGui -> a
    leseSprache f = f . sprache

-- | Abkürzungen für Funktionen, die ein 'SpracheGui' benötigen.
class (MonadReader r m) => SpracheGuiReader r m | m -> r where
    erhalteSpracheGui :: m SpracheGui

-- | Erzeuge ein neues 'SpracheGui' ohne 'sprachwechsel'-Aktionen.
spracheGuiNeu :: (MonadIO m) => Sprache -> m SpracheGui
spracheGuiNeu sprache = liftIO $ SpracheGui sprache <$> newTVarIO []

-- | Wechsel die 'Sprache' eines 'SpracheGui' und führe alle zugehörigen 'IO'-Aktionen aus.
sprachwechsel :: (MonadIO m) => Sprache -> SpracheGui -> m SpracheGui
sprachwechsel
    sprache
    spracheGui@SpracheGui {sprachwechselAktionen}
        = liftIO $ do
            readTVarIO sprachwechselAktionen >>= sequence_ . map ($ sprache)
            pure $ spracheGui {sprache}

-- | Führe die übergebene Aktion mit der aktellen 'Sprache' aus.
-- Speichere sie außerdem zum erneuten Aufruf bei einem 'sprachwechsel.
verwendeSpracheGui :: (SpracheGuiReader r m, MonadIO m) => (Sprache -> IO ()) -> m ()
verwendeSpracheGui neueAktion = do
    SpracheGui {sprache, sprachwechselAktionen} <- erhalteSpracheGui
    liftIO $ do
        neueAktion sprache
        atomically $ modifyTVar sprachwechselAktionen (neueAktion :)
#endif