{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}
-- Fließend (mit 'ß' im Dateinamen) nicht möglich. Führt zu UTF-8 Fehlern. :(

{-|
Description: Anzeige und Auswahl des Fließend-Value
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Fliessend () where
#else
module Zug.UI.Gtk.Fliessend (
    FließendWidget(), fließendNew, fließendPackNew,
    FließendAuswahlWidget(), fließendAuswahlNew, fließendAuswahlPackNew, aktuellerFließendValue) where

import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (StreckenAtom(..), Value(..))
import Zug.Language ((<:>), showText)
import qualified Zug.Language as Language
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNew, boxPackWidgetNewDefault, packingDefault, positionDefault)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitLabel(..), MitBox(..))
import Zug.UI.Gtk.Auswahl (AuswahlWidget, boundedEnumAuswahlComboBoxNew, aktuelleAuswahl)

-- | Widget zur Anzeige des Fließend-Value
newtype FließendWidget = FließendWidget Gtk.Label
                            deriving (Eq, MitWidget, MitLabel)

-- | Erstelle ein neues 'FließendWidget' und packe es in eine 'MitBox'
fließendPackNew :: (MonadIO m, StreckenAtom s, MitBox b) => b -> s -> m FließendWidget
fließendPackNew box = boxPackWidgetNew box packingDefault 3 positionDefault . fließendNew

-- | Füge neues 'Label' zu 'Box' hinzu, in dem der 'Value' eines 'StreckenAtom's angezeigt wird, bei dem Strom fließt.
fließendNew :: (MonadIO m, StreckenAtom s) => s -> m FließendWidget
fließendNew s = liftIO $ fmap FließendWidget $
    Gtk.labelNew $ Just $ (Language.fließendValue <:> showText (fließend s) :: Text)

-- | Widget zur Eingabe des Fließend-Value
newtype FließendAuswahlWidget = FließendAuswahlWidget {erhalteAuswahlWidget :: AuswahlWidget Value}
                                    deriving (Eq, MitWidget)

-- | Erstelle ein neues 'FließendAuswahlWidget' und packe es in eine 'MitBox'
fließendAuswahlPackNew :: (MonadIO m, MitBox b) => b -> m FließendAuswahlWidget
fließendAuswahlPackNew
    = flip boxPackWidgetNewDefault fließendAuswahlNew

fließendAuswahlNew :: (MonadIO m) => m FließendAuswahlWidget
fließendAuswahlNew = liftIO $ FließendAuswahlWidget <$> boundedEnumAuswahlComboBoxNew LOW Language.fließend

-- | Erhalte den aktuell gewählten Fließend-Value
aktuellerFließendValue :: (MonadIO m) => FließendAuswahlWidget -> m Value
aktuellerFließendValue = aktuelleAuswahl . erhalteAuswahlWidget
#endif