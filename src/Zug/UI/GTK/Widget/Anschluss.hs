{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

{-|
Description: Widget zur Darstellung und Auswahl eines Anschluss
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Widget.Anschluss () where
#else
module Zug.UI.Gtk.Widget.Anschluss (
    AnschlussWidget(), anschlussLabelNew,
    AnschlussAuswahlWidget(..), anschlussAuswahlNew) where

import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Widget.Klassen (MitWidget(..))
import Zug.UI.Gtk.Widget.BoundedEnumAuswahl (BoundedEnumAuswahlWidget, boundedEnumAuswahlNew, aktuellerEnum)

-- | Anzeige eines 'Anschluss'
newtype AnschlussWidget = AnschlussWidget Gtk.Label
            deriving (Eq, Show)

instance MitWidget AnschlussWidget where
    erhalteWidget :: AnschlussWidget -> Gtk.Widget
    erhalteWidget (AnschlussWidget label) = erhalteWidget label

instance MitLabel AnschlussWidget where
    erhalteLabel :: AnschlussWidget -> Gtk.Label
    erhalteLabel (AnschlussWidget label) = label

-- | 'Label' für 'Anschluss' erstellen
anschlussLabelNew :: Text -> Anschluss -> IO AnschlussWidget
anschlussLabelNew name anschluss = labelNew $ Just $ name <-> Language.anschluss <:> showText anschluss

-- | Widgets zum erzeugen eines 'Anschluss'
data AnschlussAuswahlWidget = AnschlussAuswahlWidget {
    aawNotebook :: Gtk.Notebook,
    aawSpinBox :: Gtk.SpinButton,
    aawVariante :: Gtk.ComboBox,
    aawA0 :: BoundedEnumAuswahlWidget,
    aawA1 :: BoundedEnumAuswahlWidget,
    aawA2 :: BoundedEnumAuswahlWidget}

-- | Erzeugen eines'Anschluss'
anschlussAuswahlNew :: Text -> IO AnschlussAuswahlWidget
anschlussAuswahlNew name = do
    hBox <- hBoxNew False 0
    boxPackWidgetNewDefault hBox $ labelNew $ Just $ name <-> Language.pin <:> ""
    spinButton <- boxPackWidgetNewDefault hBox $ spinButtonNewWithRange 0 27 1
    _
#endif