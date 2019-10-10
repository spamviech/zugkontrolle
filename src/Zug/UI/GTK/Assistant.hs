{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}

{-|
Description : Eigenes Assistant-Widget, nachdem das von Gtk bereitgestellte nicht funktioniert.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Assistant () where
#else
module Zug.UI.Gtk.Assistant (Assistant(), assistantNew, assistantAuswerten, AssistantResult(..)) where

-- Bibliotheken
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Tree (Tree(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), MitWindow(..))

-- | Fenster zum erstellen eines Objekts, potentiell in mehreren Schritten
data Assistant w a = Assistant {
    fenster :: Gtk.Window,
    seiten :: Tree (w, Text),
    auswahl :: NonEmpty w -> IO a}

instance MitWidget (Assistant w a) where
    erhalteWidget :: Assistant w a -> Gtk.Widget
    erhalteWidget = erhalteWidget . fenster

instance MitContainer (Assistant w a) where
    erhalteContainer :: Assistant w a -> Gtk.Container
    erhalteContainer = erhalteContainer . fenster

instance MitWindow (Assistant w a) where
    erhalteWindow :: Assistant w a -> Gtk.Window
    erhalteWindow = fenster

-- | Erstelle einen neuen 'Assistant'.
-- Die /seiten/ werden in 'Tree'-Reihenfolge von Wurzel zu Blatt angezeigt.
-- Wenn es mehr als einen Nachfolgeknoten gibt wird der Nutzer gefragt, welcher als nächster gezeigt werden soll.
-- Falls es bereitgestellt wird ersetzt dass mitgelieferte 'Gtk.Label' dabei die Standard-Frage.
-- Existiert kein Nachfolgeknoten wird der /Weiter/-Knopf durch einen /abschlussAktion/-benannten Knopf ersetzt.
--
-- Die /auswertFunktion/ wird gespeichert und durch 'assistantAuswerten' aufgerufen.
-- Sie erhält als Argument die ausgewählten /seiten/.
assistantNew :: (MonadIO m, MitWidget w) => Tree (w, Text, Maybe Gtk.Label) -> Text -> (NonEmpty w -> IO a) -> m (Assistant w a)
assistantNew seiten abschlussAktion auswertFunktion = do
    _

-- | Ergebnis-Typ von 'assistantAuswerten'
data AssistantResult a
    = AssistantSuccessful a
    | AssistantCancel

-- | Zeige einen Assistant, warte auf finale Nutzer-Eingabe und werte die Eingaben aus.
assistantAuswerten :: (MonadIO m) => Assistant w a -> m a
assistantAuswerten = _
#endif