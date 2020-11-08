{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}

-- Fließend (mit 'ß' im Dateinamen) nicht möglich. Führt zu UTF-8 Fehlern. :(
-- Problem von CPP
{-|
Description: Anzeige und Auswahl des Fließend-Value
-}
module Zug.UI.Gtk.Fliessend
  ( FließendWidget()
  , fließendNew
  , fließendPackNew
  , FließendAuswahlWidget()
  , fließendAuswahlNew
  , fließendAuswahlPackNew
  , aktuellerFließendValue
  , setzeFließendValue
  ) where

import Control.Monad.Trans (MonadIO())
import qualified GI.Gtk as Gtk

import Zug.Anbindung (StreckenAtom(..), Value(..))
import Zug.Language ((<:>))
import qualified Zug.Language as Language
import Zug.UI.Gtk.Auswahl
       (AuswahlWidget, boundedEnumAuswahlComboBoxNew, aktuelleAuswahl, setzeAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen
       (boxPackWidgetNew, boxPackWidgetNewDefault, packingDefault, positionDefault, labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitLabel(..), MitBox(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), TVarSprachewechselAktionen)

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
                 -> Maybe TVarSprachewechselAktionen
                 -> m FließendWidget
fließendPackNew box s maybeTVar =
    boxPackWidgetNew box packingDefault 3 positionDefault $ fließendNew s maybeTVar

-- | Füge neues 'Label' zu 'Box' hinzu, in dem der 'Value' eines 'StreckenAtom's angezeigt wird, bei dem Strom fließt.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fließendNew :: (SpracheGuiReader r m, MonadIO m, StreckenAtom s)
             => s
             -> Maybe TVarSprachewechselAktionen
             -> m FließendWidget
fließendNew s maybeTVar =
    fmap FließendWidget $ labelSpracheNew maybeTVar $ Language.fließendValue <:> fließend s

-- | Widget zur Eingabe des Fließend-Value
newtype FließendAuswahlWidget =
    FließendAuswahlWidget { erhalteAuswahlWidget :: AuswahlWidget Value }
    deriving (Eq, MitWidget)

-- | Erstelle ein neues 'FließendAuswahlWidget' und packe es in eine 'MitBox'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fließendAuswahlPackNew :: (SpracheGuiReader r m, MonadIO m, MitBox b)
                        => b
                        -> Maybe TVarSprachewechselAktionen
                        -> m FließendAuswahlWidget
fließendAuswahlPackNew box = boxPackWidgetNewDefault box . fließendAuswahlNew

-- | Erstelle ein neues 'FließendAuswahlWidget'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
fließendAuswahlNew :: (SpracheGuiReader r m, MonadIO m)
                    => Maybe TVarSprachewechselAktionen
                    -> m FließendAuswahlWidget
fließendAuswahlNew maybeTVar =
    FließendAuswahlWidget <$> boundedEnumAuswahlComboBoxNew LOW maybeTVar Language.fließend

-- | Erhalte den aktuell gewählten Fließend-Value.
aktuellerFließendValue :: (MonadIO m) => FließendAuswahlWidget -> m Value
aktuellerFließendValue = aktuelleAuswahl . erhalteAuswahlWidget

-- | Setze den aktuellen Fließend-Value.
setzeFließendValue :: (MonadIO m) => FließendAuswahlWidget -> Value -> m ()
setzeFließendValue = setzeAuswahl . erhalteAuswahlWidget
