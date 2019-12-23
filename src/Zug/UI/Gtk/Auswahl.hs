{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}

{-|
Description: Widget zur Auswahl eines Bounded Enums
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Auswahl () where
#else
module Zug.UI.Gtk.Auswahl (
    -- * Datentyp
    AuswahlWidget(), aktuelleAuswahl, beiAuswahl,
    -- * Konstruktoren
    auswahlRadioButtonNamedNew, auswahlComboBoxNamedNew,
    -- ** Verwende Show-Instanz zum anzeigen
    auswahlRadioButtonNew, auswahlComboBoxNew,
    -- ** Verwende ['minBound'..'maxBound'] zur Elemente-Auswahl
    boundedEnumAuswahlRadioButtonNew, boundedEnumAuswahlComboBoxNew,
    -- * Klasse für Typen mit AuswahlWidget
    MitAuswahlWidget(..), mitAuswahlWidget, auswahlWidget) where

-- Bibliotheken
import qualified Control.Lens as Lens
import Control.Monad (when, void, forM, forM_, foldM)
import Control.Monad.Trans (MonadIO(..))
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.Language (Sprache(..), Anzeige(..))
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), verwendeSpracheGui)

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

-- | Wert von 'Gtk.labelMaxWidthChars'-proberty des Name-Labels
nameWrapSize :: Int
nameWrapSize = 16

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's
auswahlRadioButtonNamedNew :: (SpracheGuiReader r m, MonadIO m, Eq e) =>
    NonEmpty e -> (Sprache -> Text) -> (e -> Sprache -> Text) -> m (AuswahlWidget e)
auswahlRadioButtonNamedNew (h :| t) name anzeigeFunktion = do
    hBox <- liftIO $ Gtk.hBoxNew False 0
    nameLabel <- boxPackWidgetNewDefault hBox $ labelSpracheNew name
    enumButtons <- liftIO $ do
        Gtk.set nameLabel [Gtk.labelMaxWidthChars := nameWrapSize, Gtk.labelWrap := True]
        vBox <- boxPackWidgetNewDefault hBox $ Gtk.vBoxNew False 0
        -- Erstelle RadioButtons
        hRadioButton <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNew
        tEnumButtons <- forM t $ \e -> do
            radioButton <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNewFromWidget hRadioButton
            pure (e, radioButton)
        -- Setze Startwert
        Gtk.toggleButtonSetActive hRadioButton True
        pure $ (h, hRadioButton) :| tEnumButtons
    verwendeSpracheGui $ \sprache -> do
        forM_ enumButtons $ \(e, radioButton) ->
            Gtk.set radioButton [Gtk.buttonLabel := anzeigeFunktion e sprache]
    pure $ AuswahlRadioButton {
        widget = erhalteWidget hBox,
        enumButtons}

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's unter Verwendeung der 'Anzeige'-Instanz
auswahlRadioButtonNew :: (SpracheGuiReader r m, MonadIO m, Eq e, Anzeige e) =>
    NonEmpty e -> (Sprache -> Text) -> m (AuswahlWidget e)
auswahlRadioButtonNew elemente name = auswahlRadioButtonNamedNew elemente name anzeige

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's für alle Elemente eines 'Bounded' 'Enum's.
-- Verwende zur Anzeige die 'Anzeige'-Instanz.
boundedEnumAuswahlRadioButtonNew :: (SpracheGuiReader r m, MonadIO m, Bounded e, Enum e, Eq e, Anzeige e) =>
    e -> (Sprache -> Text) -> m (AuswahlWidget e)
boundedEnumAuswahlRadioButtonNew standard = auswahlRadioButtonNew $ standard :| delete standard [minBound..maxBound]

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox'
auswahlComboBoxNamedNew :: (SpracheGuiReader r m, MonadIO m, Eq e) =>
    NonEmpty e -> (Sprache -> Text) -> (e -> Sprache -> Text) -> m (AuswahlWidget e)
auswahlComboBoxNamedNew elemente@(h :| _t) name anzeigeFunktion = do
    hBox <- liftIO $ Gtk.hBoxNew False 0
    nameLabel <- boxPackWidgetNewDefault hBox $ labelSpracheNew name
    (comboBox, enumIndizes, listStore) <- liftIO $ do
        Gtk.set nameLabel [Gtk.labelMaxWidthChars := nameWrapSize, Gtk.labelWrap := True]
        comboBox <- boxPackWidgetNewDefault hBox $ Gtk.comboBoxNewText
        enumIndizes <- forM elemente $ \e -> do
            index <- Gtk.comboBoxAppendText comboBox $ anzeigeFunktion e Deutsch
            when (e == h) $ liftIO $ Gtk.comboBoxSetActive comboBox index
            pure (e, index)
        listStore <- Gtk.comboBoxGetModelText comboBox
        pure (comboBox, enumIndizes, listStore)
    -- Erstelle ComboBox-Einträge
    verwendeSpracheGui $ \sprache -> void $ do
        forM enumIndizes $ \(e, index) -> do
            Gtk.listStoreSetValue listStore index $ anzeigeFunktion e sprache
    pure AuswahlComboBox {
        widget = erhalteWidget hBox,
        comboBox,
        enumIndizes}

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox' unter Verwendung der 'Anzeige'-Instanz
auswahlComboBoxNew :: (SpracheGuiReader r m, MonadIO m, Eq e, Anzeige e) =>
    NonEmpty e -> (Sprache -> Text) -> m (AuswahlWidget e)
auswahlComboBoxNew elemente name = auswahlComboBoxNamedNew elemente name anzeige

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox' für alle Elemente eines 'Bounded' 'Enum's.
-- Verwende zur Anzeige die 'Anzeige'-Instanz.
boundedEnumAuswahlComboBoxNew :: (SpracheGuiReader r m, MonadIO m, Bounded e, Enum e, Eq e, Anzeige e) =>
    e -> (Sprache -> Text) -> m (AuswahlWidget e)
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

-- | Führe die übergebene Aktion bei Änderung der Auswahl aus (vgl. 'Gtk.on')
beiAuswahl :: (Eq e, MonadIO m) => AuswahlWidget e -> (e -> IO ()) -> m ()
beiAuswahl
    auswahlWidget@AuswahlRadioButton {enumButtons}
    aktion
        = liftIO $ forM_ enumButtons $ \(_e, radioButton) ->
            Gtk.on radioButton Gtk.toggled $ aktuelleAuswahl auswahlWidget >>= aktion
beiAuswahl
    auswahlWidget@AuswahlComboBox {comboBox}
    aktion
        = void $ liftIO $ Gtk.on comboBox Gtk.changed $ aktuelleAuswahl auswahlWidget >>= aktion

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