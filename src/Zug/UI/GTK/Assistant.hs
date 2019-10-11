{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}

{-|
Description : Eigenes Assistant-Widget, nachdem das von Gtk bereitgestellte nicht funktioniert.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Assistant () where
#else
module Zug.UI.Gtk.Assistant (
    Assistant(), AssistantSeiten(..), assistantNew, assistantAuswerten, AssistantResult(..)) where

-- Bibliotheken
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), MitWindow(..))

-- | Fenster zum erstellen eines Objekts, potentiell in mehreren Schritten
data Assistant w a = Assistant {
    fenster :: Gtk.Window,
    seiten :: AssistantSeiten w,
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

-- | Seiten eines 'Assistant'.
-- Die Form ist wie ein Rose-tree, ergänzt um zusätzliche Informationen.
-- Der Name wird im Titel des Assistant und bei der Auswahl der Nachfolgeseite gezeigt.
data AssistantSeiten w
    -- | Seite mit einem direkten Nachfolger
    = AssistantSeiteLinear {
        seite :: w,
        name :: Text,
        fortfahrenLabel :: Text,
        nachfolger :: AssistantSeiten w}
    -- | Seite mit meheren direkten Nachfolger
    | AssistantSeiteAuswahl {
        seite :: w,
        name :: Text,
        fortfahrenLabel :: Text,
        nachfolgerFrage :: Text,
        nachfolgerListe :: NonEmpty (AssistantSeiten w)}
    -- | Seite ohne Nachfolger
    | AssistantSeiteLetzte {
        seite :: w,
        name :: Text,
        finalisierenLabel :: Text}
    deriving (Eq)

-- | Erstelle einen neuen 'Assistant'.
-- Die /seiten/ werden in 'Tree'-Reihenfolge von Wurzel zu Blatt angezeigt.
-- Wenn es mehr als einen Nachfolgeknoten gibt wird der Nutzer gefragt, welcher als nächster gezeigt werden soll.
-- Falls es bereitgestellt wird ersetzt dass mitgelieferte 'Gtk.Label' dabei die Standard-Frage.
-- Existiert kein Nachfolgeknoten wird der /Weiter/-Knopf durch einen /abschlussAktion/-benannten Knopf ersetzt.
--
-- Die /auswertFunktion/ wird gespeichert und durch 'assistantAuswerten' aufgerufen.
-- Sie erhält als Argument die ausgewählten /seiten/.
assistantNew :: (MonadIO m, MitWidget w) => AssistantSeiten w -> (NonEmpty w -> IO a) -> m (Assistant w a)
assistantNew seiten auswertFunktion = do
    _

-- | Ergebnis-Typ von 'assistantAuswerten'
data AssistantResult a
    = AssistantSuccessful a
    | AssistantCancel
    | AssistantDestroy

-- | Zeige einen Assistant, warte auf finale Nutzer-Eingabe und werte die Eingaben aus.
assistantAuswerten :: (MonadIO m) => Assistant w a -> m a
assistantAuswerten assistant = do
    _
#endif