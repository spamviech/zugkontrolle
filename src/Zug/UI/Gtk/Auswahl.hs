{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

{-|
Description: Widget zur Auswahl eines Werts.
-}
module Zug.UI.Gtk.Auswahl
  (
#ifdef ZUGKONTROLLEGUI
    -- * Datentyp
    AuswahlWidget()
  , aktuelleAuswahl
  , setzeAuswahl
  , beiAuswahl
    -- * Konstruktoren
  , auswahlRadioButtonNamedNew
  , auswahlComboBoxNamedNew
    -- ** Verwende 'Anzeige'-Instanz zum anzeigen
  , auswahlRadioButtonNew
  , auswahlComboBoxNew
    -- ** Verwende ['minBound'..'maxBound'] zur Elemente-Auswahl
  , boundedEnumAuswahlRadioButtonNew
  , boundedEnumAuswahlComboBoxNew
    -- * Klasse für Typen mit AuswahlWidget
  , MitAuswahlWidget(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad (when, void, forM, forM_, foldM)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.GI.Gtk as Gtk
import Data.Int (Int32)
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import Data.Text (Text)
import GI.Gtk (AttrOp(..))

import Zug.Language (Sprache(..), Anzeige(..))
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), verwendeSpracheGui)

-- | Auswahl-Widget für ein 'Bounded' 'Enum'
data AuswahlWidget e
    = AuswahlRadioButton { widget :: Gtk.Widget, enumButtons :: NonEmpty (e, Gtk.RadioButton) }
    | AuswahlComboBox
          { widget :: Gtk.Widget
          , comboBox :: Gtk.ComboBox
          , enumIndizes :: NonEmpty (e, Int32)
          }
    deriving (Eq)

instance MitWidget (AuswahlWidget e) where
    erhalteWidget :: (MonadIO m) => AuswahlWidget e -> m Gtk.Widget
    erhalteWidget = pure . widget

-- | Wert von 'Gtk.labelMaxWidthChars'-property des Name-Labels
nameWrapSize :: Int32
nameWrapSize = 16

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlRadioButtonNamedNew
    :: (SpracheGuiReader r m, MonadIO m, Eq e)
    => NonEmpty e
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (Sprache -> Text)
    -> (e -> Sprache -> Text)
    -> m (AuswahlWidget e)
auswahlRadioButtonNamedNew (h :| t) maybeTVar name anzeigeFunktion = do
    hBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
    widget <- erhalteWidget hBox
    nameLabel <- boxPackWidgetNewDefault hBox $ labelSpracheNew maybeTVar name
    enumButtons <- liftIO $ do
        Gtk.set nameLabel [Gtk.labelMaxWidthChars := nameWrapSize, Gtk.labelWrap := True]
        vBox <- boxPackWidgetNewDefault hBox $ Gtk.boxNew Gtk.OrientationVertical 0
        -- Erstelle RadioButtons
        hRadioButton
            <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNewFromWidget Gtk.noRadioButton
        tEnumButtons <- forM t $ \e -> do
            radioButton
                <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNewFromWidget $ Just hRadioButton
            pure (e, radioButton)
        -- Setze Startwert
        Gtk.toggleButtonSetActive hRadioButton True
        pure $ (h, hRadioButton) :| tEnumButtons
    verwendeSpracheGui maybeTVar $ \sprache -> do
        forM_ enumButtons $ \(e, radioButton)
            -> Gtk.set radioButton [Gtk.buttonLabel := anzeigeFunktion e sprache]
    pure $ AuswahlRadioButton { widget, enumButtons }

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's unter Verwendung der 'Anzeige'-Instanz.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlRadioButtonNew :: (SpracheGuiReader r m, MonadIO m, Eq e, Anzeige e)
                      => NonEmpty e
                      -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                      -> (Sprache -> Text)
                      -> m (AuswahlWidget e)
auswahlRadioButtonNew elemente maybeTVar name =
    auswahlRadioButtonNamedNew elemente maybeTVar name anzeige

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's für alle Elemente eines 'Bounded' 'Enum's.
-- Verwende zur Anzeige die 'Anzeige'-Instanz.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
boundedEnumAuswahlRadioButtonNew
    :: (SpracheGuiReader r m, MonadIO m, Bounded e, Enum e, Eq e, Anzeige e)
    => e
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (Sprache -> Text)
    -> m (AuswahlWidget e)
boundedEnumAuswahlRadioButtonNew
    standard = auswahlRadioButtonNew $ standard :| delete standard [minBound .. maxBound]

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlComboBoxNamedNew
    :: (SpracheGuiReader r m, MonadIO m, Eq e)
    => NonEmpty e
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (Sprache -> Text)
    -> (e -> Sprache -> Text)
    -> m (AuswahlWidget e)
auswahlComboBoxNamedNew elemente@(h :| _t) maybeTVar name anzeigeFunktion = do
    hBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
    widget <- erhalteWidget hBox
    nameLabel <- boxPackWidgetNewDefault hBox $ labelSpracheNew maybeTVar name
    (comboBox, enumIndizes, seqStore) <- liftIO $ do
        Gtk.set nameLabel [Gtk.labelMaxWidthChars := nameWrapSize, Gtk.labelWrap := True]
        comboBox <- boxPackWidgetNewDefault hBox $ Gtk.comboBoxNewText
        enumIndizes <- forM elemente $ \e -> do
            index <- Gtk.comboBoxAppendText comboBox $ anzeigeFunktion e Deutsch
            when (e == h) $ liftIO $ Gtk.comboBoxSetActive comboBox index
            pure (e, index)
        Gtk.comboBoxSetModelText comboBox
        seqStore <- Gtk.comboBoxGetModelText comboBox
        pure (comboBox, enumIndizes, seqStore)
    -- Erstelle ComboBox-Einträge
    verwendeSpracheGui maybeTVar $ \sprache -> void $ do
        forM enumIndizes $ \(e, index) -> do
            Gtk.seqStoreSetValue seqStore index $ anzeigeFunktion e sprache
    pure AuswahlComboBox { widget, comboBox, enumIndizes }

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox' unter Verwendung der 'Anzeige'-Instanz.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlComboBoxNew :: (SpracheGuiReader r m, MonadIO m, Eq e, Anzeige e)
                   => NonEmpty e
                   -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                   -> (Sprache -> Text)
                   -> m (AuswahlWidget e)
auswahlComboBoxNew elemente maybeTVar name =
    auswahlComboBoxNamedNew elemente maybeTVar name anzeige

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox' für alle Elemente eines 'Bounded' 'Enum's.
-- Verwende zur Anzeige die 'Anzeige'-Instanz.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
boundedEnumAuswahlComboBoxNew
    :: (SpracheGuiReader r m, MonadIO m, Bounded e, Enum e, Eq e, Anzeige e)
    => e
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (Sprache -> Text)
    -> m (AuswahlWidget e)
boundedEnumAuswahlComboBoxNew
    standard = auswahlComboBoxNew $ standard :| delete standard [minBound .. maxBound]

-- | Setzte den aktuellen Wert eines 'AuswahlWidget'.
--
-- Wenn der Wert nicht im 'AuswahlWidget' enthalten ist wird der aktuelle Wert nicht verändert.
setzeAuswahl :: (MonadIO m, Eq e) => AuswahlWidget e -> e -> m ()
setzeAuswahl AuswahlRadioButton {enumButtons} wert = liftIO $ forM_ enumButtons $ \(e, radioButton)
    -> when (e == wert) $ Gtk.set radioButton [Gtk.toggleButtonActive := True]
setzeAuswahl AuswahlComboBox {comboBox, enumIndizes} wert =
    case lookup wert $ NonEmpty.toList enumIndizes of
        (Just index) -> liftIO $ Gtk.set comboBox [Gtk.comboBoxActive := index]
        Nothing -> pure ()

-- | Erhalte den aktuell ausgewählten Wert.
aktuelleAuswahl :: (MonadIO m, Eq e) => AuswahlWidget e -> m e
aktuelleAuswahl
    AuswahlRadioButton {enumButtons} = liftIO $ fromJust <$> foldM foldEnum Nothing enumButtons
    where
        foldEnum :: Maybe e -> (e, Gtk.RadioButton) -> IO (Maybe e)
        foldEnum justE@(Just _e) _enumButton = pure justE
        foldEnum Nothing (e, radioButton) = liftIO $ do
            toggled <- Gtk.get radioButton Gtk.toggleButtonActive
            pure
                $ if toggled
                    then Just e
                    else Nothing
aktuelleAuswahl AuswahlComboBox {comboBox, enumIndizes} = liftIO $ do
    activeIndex <- Gtk.get comboBox Gtk.comboBoxActive
    let foldEnum :: (Eq e) => (e, Int32) -> Maybe e -> Maybe e
        foldEnum _enumIndex justE@(Just _e) = justE
        foldEnum (e, index) Nothing
            | index == activeIndex = Just e
            | otherwise = Nothing
    pure $ fromJust $ foldr foldEnum Nothing enumIndizes

-- | Führe die übergebene Aktion bei Änderung der Auswahl aus (vgl. 'Gtk.on')
beiAuswahl :: (Eq e, MonadIO m) => AuswahlWidget e -> (e -> IO ()) -> m ()
beiAuswahl auswahlWidget@AuswahlRadioButton {enumButtons} aktion =
    liftIO $ forM_ enumButtons $ \(e, radioButton) -> Gtk.onToggleButtonToggled radioButton $ do
        auswahl <- aktuelleAuswahl auswahlWidget
        when (e == auswahl) $ aktion e
beiAuswahl auswahlWidget@AuswahlComboBox {comboBox} aktion =
    void $ liftIO $ Gtk.onComboBoxChanged comboBox $ aktuelleAuswahl auswahlWidget >>= aktion

-- | Klasse für Typen mit 'AuswahlWidget'
class (MitWidget a) => MitAuswahlWidget a e where
    erhalteAuswahlWidget :: a -> AuswahlWidget e

instance MitAuswahlWidget (AuswahlWidget e) e where
    erhalteAuswahlWidget :: AuswahlWidget e -> AuswahlWidget e
    erhalteAuswahlWidget = id
#endif
    --
