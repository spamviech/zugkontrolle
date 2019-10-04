{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

{-|
Description: Widget zur Auswahl eines Bounded Enums
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Widgets.Auswahl () where
#else
module Zug.UI.Gtk.Widgets.Auswahl (
    -- * Datentyp
    AuswahlWidget(), aktuelleAuswahl,
    -- * Konstruktoren
    auswahlRadioButtonNew, auswahlComboBoxNew,
    boundedEnumAuswahlRadioButtonNew, boundedEnumAuswahlComboBoxNew,
    -- * Klasse für Typen mit AuswahlWidget
    MitAuswahlWidget(..), mitAuswahlWidget, auswahlWidget) where

import qualified Control.Lens as Lens
import Control.Monad (when, forM, foldM)
import Control.Monad.Trans (MonadIO(..))
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.Language (showText)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault)
import Zug.UI.Gtk.Klassen (MitWidget(..))

-- | Auswahl-Widget für ein 'Bounded' 'Enum'
data AuswahlWidget e
    = AuswahlRadioButton {
        widget :: Gtk.Widget,
        enumButtons :: NonEmpty (e, Gtk.RadioButton)}
    | AuswahlComboBox {
        widget :: Gtk.Widget,
        comboBox :: Gtk.ComboBox,
        enumIndizes :: NonEmpty (e, Int)}
            deriving (Eq)

instance MitWidget (AuswahlWidget e) where
    erhalteWidget :: AuswahlWidget e -> Gtk.Widget
    erhalteWidget = widget

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's
auswahlRadioButtonNew :: (MonadIO m, Eq e, Show e) => NonEmpty e -> Text -> m (AuswahlWidget e)
auswahlRadioButtonNew (h :| t) name = liftIO $ do
    hBox <- Gtk.hBoxNew False 0
    boxPackWidgetNewDefault hBox $ Gtk.labelNew $ Just name
    vBox <- boxPackWidgetNewDefault hBox $ Gtk.vBoxNew False 0
    -- Erstelle RadioButtons
    hRadioButton <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNewWithLabel $ show h
    tEnumButtons <- forM t $ \enum -> do
        radioButton <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNewWithLabelFromWidget hRadioButton $ show enum
        -- Setze Start-Wert
        when (enum == h) $ Gtk.toggleButtonSetActive radioButton True
        pure (enum, radioButton)
    pure $ AuswahlRadioButton {
        widget = erhalteWidget hBox,
        enumButtons = ((h, hRadioButton) :| tEnumButtons)}

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's für alle Elemente eines 'Bounded' 'Enum's
boundedEnumAuswahlRadioButtonNew :: (MonadIO m, Bounded e, Enum e, Eq e, Show e) => e -> Text -> m (AuswahlWidget e)
boundedEnumAuswahlRadioButtonNew standard = auswahlRadioButtonNew $ standard :| delete standard [minBound..maxBound]

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox'
auswahlComboBoxNew :: (MonadIO m, Eq e, Show e) => NonEmpty e -> Text -> m (AuswahlWidget e)
auswahlComboBoxNew elemente@(h :| _t) name = liftIO $ do
    hBox <- Gtk.hBoxNew False 0
    boxPackWidgetNewDefault hBox $ Gtk.labelNew $ Just name
    comboBox <- boxPackWidgetNewDefault hBox $ Gtk.comboBoxNewText
    -- Erstelle ComboBox-Einträge
    enumIndizes <- forM elemente $ \enum -> do
        index <- Gtk.comboBoxAppendText comboBox $ showText enum
        when (enum == h) $ Gtk.comboBoxSetActive comboBox index
        pure (enum, index)
    pure AuswahlComboBox {
        widget = erhalteWidget hBox,
        comboBox,
        enumIndizes}

boundedEnumAuswahlComboBoxNew :: (MonadIO m, Bounded e, Enum e, Eq e, Show e) => e -> Text -> m (AuswahlWidget e)
boundedEnumAuswahlComboBoxNew standard = auswahlComboBoxNew $ standard :| delete standard [minBound..maxBound]

-- | Erhalte den aktuell ausgewählten 'Value'
aktuelleAuswahl :: (MonadIO m, Eq e) => AuswahlWidget e -> m e
aktuelleAuswahl
    AuswahlRadioButton {enumButtons}
        = liftIO $ fromJust <$> foldM foldEnum Nothing enumButtons
    where
        foldEnum :: Maybe e -> (e, Gtk.RadioButton) -> IO (Maybe e)
        foldEnum    justE@(Just _e) _enumButton         = pure justE
        foldEnum    Nothing         (e, radioButton)    = liftIO $ do
            toggled <- Gtk.get radioButton Gtk.toggleButtonActive
            pure $ if toggled
                then Just e
                else Nothing
aktuelleAuswahl
    AuswahlComboBox {comboBox, enumIndizes}
        = liftIO $ do
            activeIndex <- Gtk.comboBoxGetActive comboBox
            let
                foldEnum :: (Eq e) => (e, Int) -> Maybe e -> Maybe e
                foldEnum    _enumIndex  justE@(Just _e) = justE
                foldEnum    (e, index)  Nothing
                    | index == activeIndex
                        = Just e
                    | otherwise
                        = Nothing
            pure $ fromJust $ foldr foldEnum Nothing enumIndizes

-- | Klasse für Typen mit 'AuswahlWidget'
class (MitWidget a) => MitAuswahlWidget a e where
    erhalteAuswahlWidget :: a -> AuswahlWidget e

instance MitAuswahlWidget (AuswahlWidget e) e where
    erhalteAuswahlWidget :: AuswahlWidget e -> AuswahlWidget e
    erhalteAuswahlWidget = id

-- | Führe eine Funktion 'MitAuswahlWidget' aus
mitAuswahlWidget :: (MitAuswahlWidget a e) => (AuswahlWidget e -> b) -> a -> b
mitAuswahlWidget funktion = funktion . erhalteAuswahlWidget

-- | Assoziierter 'Lens.Getter' zu 'erhalteAuswahlWidget'
auswahlWidget :: (MitAuswahlWidget a e) => Lens.Getter a (AuswahlWidget e)
auswahlWidget = Lens.to erhalteAuswahlWidget
#endif