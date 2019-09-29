{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}

{-|
Description: Widget zur Auswahl eines Bounded Enums
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Widget.BoundedEnumAuswahl () where
#else
module Zug.UI.Gtk.Widget.BoundedEnumAuswahl (BoundedEnumAuswahlWidget(), boundedEnumAuswahlNew, aktuellerEnum) where

import Control.Monad (when, forM, foldM)
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Widget.Klassen (MitWidget(..))
import Zug.UI.Gtk.Widget.Hilfsfunktionen (boxPackWidgetNewDefault)

-- | Auswahl-Widget für ein 'Bounded' 'Enum'
data BoundedEnumAuswahlWidget e = BoundedEnumAuswahlWidget {
    widget :: Gtk.Widget,
    enumButtons :: [(e, Gtk.RadioButton)]}

instance MitWidget (BoundedEnumAuswahlWidget e) where
    erhalteWidget :: BoundedEnumAuswahlWidget e -> Gtk.Widget
    erhalteWidget = widget

boundedEnumAuswahlNew :: (MonadIO m, Bounded e, Enum e, Eq e, Show e) => e -> Text -> m (BoundedEnumAuswahlWidget e)
boundedEnumAuswahlNew standard name = liftIO $ do
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
    pure $ BoundedEnumAuswahlWidget {
        widget = Gtk.toWidget hBox,
        enumButtons = ((h, hRadioButton) : tEnumButtons)}

-- | Erhalte den aktuell ausgewählten 'Value'
aktuellerEnum :: (MonadIO m, Eq e) => BoundedEnumAuswahlWidget e -> m e
aktuellerEnum BoundedEnumAuswahlWidget {enumButtons} = liftIO $ foldM foldEnum Nothing enumButtons >>= pure . fromJust
    where
        foldEnum :: Maybe e -> (e, Gtk.RadioButton) -> IO (Maybe e)
        foldEnum    justE@(Just _e) _enumButton         = pure justE
        foldEnum    Nothing         (e, radioButton)    = liftIO $ do
            toggled <- Gtk.get radioButton Gtk.toggleButtonActive
            pure $ if toggled
                then Just e
                else Nothing
#endif