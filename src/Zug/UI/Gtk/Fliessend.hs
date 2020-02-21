{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

-- Fließend (mit 'ß' im Dateinamen) nicht möglich. Führt zu UTF-8 Fehlern. :(
{-|
Description: Anzeige und Auswahl des Fließend-Value
-}
module Zug.UI.Gtk.Fliessend
  (
#ifdef ZUGKONTROLLEGUI
    FließendWidget()
  , fließendNew
  , fließendPackNew
  , FließendAuswahlWidget()
  , fließendAuswahlNew
  , fließendAuswahlPackNew
  , aktuellerFließendValue
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Trans (MonadIO(..))
import qualified Graphics.UI.Gtk as Gtk

-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (StreckenAtom(..), Value(..))
import Zug.Language (Sprache(), (<:>))
import qualified Zug.Language as Language
import Zug.UI.Gtk.Auswahl (AuswahlWidget, boundedEnumAuswahlComboBoxNew, aktuelleAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen
       (boxPackWidgetNew, boxPackWidgetNewDefault, packingDefault, positionDefault, labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitLabel(..), MitBox(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader())

-- | Widget zur Anzeige des Fließend-Value
newtype FließendWidget = FließendWidget Gtk.Label
    deriving (Eq, MitWidget, MitLabel)

-- | Erstelle ein neues 'FließendWidget' und packe es in eine 'MitBox'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fließendPackNew :: (SpracheGuiReader r m, MonadIO m, StreckenAtom s, MitBox b)
                 => b
                 -> s
                 -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                 -> m FließendWidget
fließendPackNew box s maybeTVar =
    boxPackWidgetNew box packingDefault 3 positionDefault $ fließendNew s maybeTVar

-- | Füge neues 'Label' zu 'Box' hinzu, in dem der 'Value' eines 'StreckenAtom's angezeigt wird, bei dem Strom fließt.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fließendNew :: (SpracheGuiReader r m, MonadIO m, StreckenAtom s)
             => s
             -> Maybe (TVar (Maybe [Sprache -> IO ()]))
             -> m FließendWidget
fließendNew s maybeTVar =
    fmap FließendWidget $ labelSpracheNew maybeTVar $ Language.fließendValue <:> fließend s

-- | Widget zur Eingabe des Fließend-Value
newtype FließendAuswahlWidget =
    FließendAuswahlWidget
    { erhalteAuswahlWidget :: AuswahlWidget Value
    }
    deriving (Eq, MitWidget)

-- | Erstelle ein neues 'FließendAuswahlWidget' und packe es in eine 'MitBox'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fließendAuswahlPackNew :: (SpracheGuiReader r m, MonadIO m, MitBox b)
                        => b
                        -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                        -> m FließendAuswahlWidget
fließendAuswahlPackNew box = boxPackWidgetNewDefault box . fließendAuswahlNew

-- | Erstelle ein neues 'FließendAuswahlWidget'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fließendAuswahlNew :: (SpracheGuiReader r m, MonadIO m)
                    => Maybe (TVar (Maybe [Sprache -> IO ()]))
                    -> m FließendAuswahlWidget
fließendAuswahlNew maybeTVar =
    FließendAuswahlWidget <$> boundedEnumAuswahlComboBoxNew LOW maybeTVar Language.fließend

-- | Erhalte den aktuell gewählten Fließend-Value
aktuellerFließendValue :: (MonadIO m) => FließendAuswahlWidget -> m Value
aktuellerFließendValue = aktuelleAuswahl . erhalteAuswahlWidget
#endif

