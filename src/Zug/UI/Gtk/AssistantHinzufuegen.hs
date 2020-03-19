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
module Zug.UI.Gtk.AssistantHinzufuegen
  (
#ifdef ZUGKONTROLLEGUI
    assistantHinzufügenNew
  , AssistantHinzufügen()
  , assistantHinzufügenAuswerten
  , HinzufügenErgebnis(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
-- Bibliotheken
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TVar, TMVar, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Monad.Trans (MonadIO(..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Graphics.UI.Gtk as Gtk

-- Abhängigkeiten von anderen Modulen
import Zug.Enums (Zugtyp())
import Zug.Language (Sprache())
import Zug.Objekt (Objekt)
import Zug.UI.Gtk.AssistantHinzufuegen.HinzufuegenSeite (HinzufügenSeite(..), seiteErgebnis)
import Zug.UI.Gtk.Auswahl (AuswahlWidget, aktuelleAuswahl)
import Zug.UI.Gtk.Fliessend (FließendAuswahlWidget, aktuellerFließendValue)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitWidgetShow, mitWidgetHide, MitWindow())
import Zug.UI.Gtk.StreckenObjekt (ObjektGui)
import Zug.UI.StatusVar (StatusVarReader(..))

-- | Seiten des Hinzufügen-'Assistant'
data AssistantHinzufügen =
    AssistantHinzufügen
    { window :: Gtk.Window
    , notebook :: Gtk.Notebook
    , fließendAuswahl :: FließendAuswahlWidget
    , zugtypAuswahl :: AuswahlWidget Zugtyp
    , indexSeiten :: Map Int HinzufügenSeite
    , tmVarErgebnis :: TMVar HinzufügenErgebnis
    }
    deriving (Eq)

instance MitWidget AssistantHinzufügen where
    erhalteWidget :: AssistantHinzufügen -> Gtk.Widget
    erhalteWidget = Gtk.toWidget . window

-- | Hat der 'AssistantHinzufügen' ein Ergebnis geliefert?
data HinzufügenErgebnis
    = HinzufügenErfolgreich Objekt
    | HinzufügenAbbrechen
    | HinzufügenBeenden

-- | Zeige den 'AssistantHinzufügen' an und warte bis ein 'HinzufügenErgebnis' vorliegt.
--
-- Es wird erwartet, dass diese Funktion in einem eigenen Thread ausgeführt wird.
assistantHinzufügenAuswerten :: (MonadIO m) => AssistantHinzufügen -> m HinzufügenErgebnis
assistantHinzufügenAuswerten AssistantHinzufügen {window, tmVarErgebnis} = liftIO $ do
    Gtk.postGUIAsync $ mitWidgetShow window
    ergebnis <- atomically $ takeTMVar tmVarErgebnis
    Gtk.postGUIAsync $ mitWidgetHide window
    pure ergebnis

-- | Erhalte das Ergebnis einer 'HinzufügenSeite'.
hinzufügenErgebnis :: (StatusVarReader r ObjektGui m, MonadIO m) => AssistantHinzufügen -> m ()
hinzufügenErgebnis
    AssistantHinzufügen {notebook, fließendAuswahl, zugtypAuswahl, indexSeiten, tmVarErgebnis} =
    do
        aktuelleSeite <- liftIO $ Gtk.get notebook Gtk.notebookPage
        ergebnis <- seiteErgebnis fließendAuswahl zugtypAuswahl $ indexSeiten Map.! aktuelleSeite
        liftIO $ atomically $ putTMVar tmVarErgebnis $ HinzufügenErfolgreich ergebnis

-- | Erstelle einen neuen 'AssistantHinzufügen'.
assistantHinzufügenNew :: (MitWindow p, MonadIO m)
                        => p
                        -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                        -> m AssistantHinzufügen
assistantHinzufügenNew = do
    _undefined --TODO
#endif
--
