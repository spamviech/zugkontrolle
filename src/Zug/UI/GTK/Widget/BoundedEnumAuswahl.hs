{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}

{-|
Description: Widget zur Auswahl eines Bounded Enums
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Widget.BoundedEnumAuswahl () where
#else
module Zug.UI.Gtk.Widget.BoundedEnumAuswahl (
    BoundedEnumAuswahlWidget(), aktuellerEnum,
    boundedEnumAuswahlRadioButtonNew, boundedEnumAuswahlComboBoxNew) where

import Control.Monad (when, forM, foldM)
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.Language (showText)
import Zug.UI.Gtk.Widget.Klassen (MitWidget(..))
import Zug.UI.Gtk.Widget.Hilfsfunktionen (boxPackWidgetNewDefault)

-- | Auswahl-Widget für ein 'Bounded' 'Enum'
data BoundedEnumAuswahlWidget e
    = BoundedEnumAuswahlRadioButton {
        widget :: Gtk.Widget,
        enumButtons :: [(e, Gtk.RadioButton)]}
    | BoundedEnumAuswahlComboBox {
        widget :: Gtk.Widget,
        comboBox :: Gtk.ComboBox,
        enumIndizes :: [(e, Int)]}
            deriving (Eq)

instance MitWidget (BoundedEnumAuswahlWidget e) where
    erhalteWidget :: BoundedEnumAuswahlWidget e -> Gtk.Widget
    erhalteWidget = widget

-- | Konstruiere ein 'BoundedEnumAuswahlWidget' mit 'Gtk.RadioButton's
boundedEnumAuswahlRadioButtonNew :: (MonadIO m, Bounded e, Enum e, Eq e, Show e) => e -> Text -> m (BoundedEnumAuswahlWidget e)
boundedEnumAuswahlRadioButtonNew standard name = liftIO $ do
    hBox <- Gtk.hBoxNew False 0
    boxPackWidgetNewDefault hBox $ Gtk.labelNew $ Just name
    vBox <- boxPackWidgetNewDefault hBox $ Gtk.vBoxNew False 0
    -- Nicht-Leerheit garantiert durch Bounded
    let (h : t) = [minBound..maxBound]
    hRadioButton <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNewWithLabel $ show h
    tEnumButtons <- forM t $ \enum -> do
        radioButton <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNewWithLabelFromWidget hRadioButton $ show enum
        -- Setze Start-Wert
        when (enum == standard) $ Gtk.toggleButtonSetActive radioButton True
        pure (enum, radioButton)
    pure $ BoundedEnumAuswahlRadioButton {
        widget = erhalteWidget hBox,
        enumButtons = ((h, hRadioButton) : tEnumButtons)}

-- | Konstruiere ein 'BoundedEnumAuswahlWidget' mit einer 'Gtk.ComboBox'
boundedEnumAuswahlComboBoxNew :: (MonadIO m, Bounded e, Enum e, Eq e, Show e) => e -> Text -> m (BoundedEnumAuswahlWidget e)
boundedEnumAuswahlComboBoxNew standard name = liftIO $ do
    hBox <- Gtk.hBoxNew False 0
    boxPackWidgetNewDefault hBox $ Gtk.labelNew $ Just name
    comboBox <- boxPackWidgetNewDefault hBox $ Gtk.comboBoxNewText
    -- Nicht-Leerheit garantiert durch Bounded
    enumIndizes <- forM [minBound..maxBound] $ \enum -> do
        index <- Gtk.comboBoxAppendText comboBox $ showText enum
        when (enum == standard) $ Gtk.comboBoxSetActive comboBox index
        pure (enum, index)
    pure BoundedEnumAuswahlComboBox {
        widget = erhalteWidget hBox,
        comboBox,
        enumIndizes}

-- | Erhalte den aktuell ausgewählten 'Value'
aktuellerEnum :: (MonadIO m, Eq e) => BoundedEnumAuswahlWidget e -> m e
aktuellerEnum   BoundedEnumAuswahlRadioButton {enumButtons}        = liftIO $ fromJust <$> foldM foldEnum Nothing enumButtons
    where
        foldEnum :: Maybe e -> (e, Gtk.RadioButton) -> IO (Maybe e)
        foldEnum    justE@(Just _e) _enumButton         = pure justE
        foldEnum    Nothing         (e, radioButton)    = liftIO $ do
            toggled <- Gtk.get radioButton Gtk.toggleButtonActive
            pure $ if toggled
                then Just e
                else Nothing
aktuellerEnum   BoundedEnumAuswahlComboBox {comboBox, enumIndizes}  = liftIO $ do
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
#endif