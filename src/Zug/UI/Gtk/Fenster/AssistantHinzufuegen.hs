{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
#endif

{-|
Description : Erstellen eines Assistant zum Hinzufügen eines 'StreckenObjekt'es.
-}
module Zug.UI.Gtk.Fenster.AssistantHinzufuegen
  (
#ifdef ZUGKONTROLLEGUI
    assistantHinzufügenNew
  , AssistantHinzufügen()
  , hinzufügenErgebnis
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
-- Bibliotheken
import Control.Monad.Trans (MonadIO(..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Graphics.UI.Gtk as Gtk

-- Abhängigkeiten von anderen Modulen
import Zug.Objekt (Objekt)
import Zug.UI.Gtk.Fenster.HinzufuegenSeite (HinzufügenSeite(..), seiteErgebnis)
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.StreckenObjekt (ObjektGui)
import Zug.UI.StatusVar (StatusVarReader(..))

-- | Seiten des Hinzufügen-'Assistant'
data AssistantHinzufügen =
    AssistantHinzufügen
    { window :: Gtk.Window
    , notebook :: Gtk.Notebook
    , indexSeiten :: Map Int HinzufügenSeite
    }
    deriving (Eq)

instance MitWidget AssistantHinzufügen where
    erhalteWidget :: AssistantHinzufügen -> Gtk.Widget
    erhalteWidget = Gtk.toWidget . window

-- | Erhalte das Ergebnis einer 'HinzufügenSeite'.
hinzufügenErgebnis
    :: (StatusVarReader r ObjektGui m, MonadIO m) => AssistantHinzufügen -> m Objekt
hinzufügenErgebnis AssistantHinzufügen {notebook, indexSeiten} = do
    aktuelleSeite <- liftIO $ Gtk.get notebook Gtk.notebookPage
    seiteErgebnis $ indexSeiten Map.! aktuelleSeite

assistantHinzufügenNew :: (MonadIO m) => m AssistantHinzufügen
assistantHinzufügenNew = _undefined --TODO
#endif







































