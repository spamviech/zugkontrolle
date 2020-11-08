{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Description: Widget zur Auswahl eines Werts.
-}
module Zug.UI.Gtk.Auswahl
  ( -- * Datentyp
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
  ) where

import Control.Monad (when, void, forM, forM_, foldM)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.GI.Base.GType as GI
import Data.Int (Int32)
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import Zug.Language (Sprache(..), MitSprache(leseSprache), Anzeige(..), (<:>))
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNewDefault, labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.SpracheGui
       (SpracheGuiReader(erhalteSpracheGui), verwendeSpracheGui, TVarSprachewechselAktionen)

-- | Auswahl-Widget für ein 'Bounded' 'Enum'
data AuswahlWidget e
    = AuswahlRadioButton { widget :: Gtk.Widget, enumButtons :: NonEmpty (e, Gtk.RadioButton) }
    | AuswahlComboBox
      { widget :: Gtk.Widget
      , comboBox :: Gtk.ComboBox
      , enumIndicesIters :: NonEmpty (e, Int32, Gtk.TreeIter)
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
    -> Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> (e -> Sprache -> Text)
    -> m (AuswahlWidget e)
auswahlRadioButtonNamedNew (h :| t) maybeTVar name anzeigeFunktion = do
    hBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    widget <- erhalteWidget hBox
    nameLabel <- boxPackWidgetNewDefault hBox $ labelSpracheNew maybeTVar name
    enumButtons <- do
        Gtk.setLabelMaxWidthChars nameLabel nameWrapSize
        Gtk.setLabelWrap nameLabel True
        vBox <- boxPackWidgetNewDefault hBox $ Gtk.boxNew Gtk.OrientationVertical 0
        -- Erstelle RadioButtons
        hRadioButton <- boxPackWidgetNewDefault vBox
            $ Gtk.radioButtonNewFromWidget (Nothing :: Maybe Gtk.RadioButton)
        tEnumButtons <- forM t $ \e -> do
            radioButton
                <- boxPackWidgetNewDefault vBox $ Gtk.radioButtonNewFromWidget $ Just hRadioButton
            pure (e, radioButton)
        -- Setze Startwert
        Gtk.toggleButtonSetActive hRadioButton True
        pure $ (h, hRadioButton) :| tEnumButtons
    verwendeSpracheGui maybeTVar $ \sprache -> do
        forM_ enumButtons
            $ \(e, radioButton) -> Gtk.setButtonLabel radioButton $ anzeigeFunktion e sprache
    pure $ AuswahlRadioButton { widget, enumButtons }

-- | Konstruiere ein 'AuswahlWidget' mit 'Gtk.RadioButton's unter Verwendung der 'Anzeige'-Instanz.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlRadioButtonNew :: (SpracheGuiReader r m, MonadIO m, Eq e, Anzeige e)
                      => NonEmpty e
                      -> Maybe TVarSprachewechselAktionen
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
    -> Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> m (AuswahlWidget e)
boundedEnumAuswahlRadioButtonNew
    standard = auswahlRadioButtonNew $ standard :| delete standard [minBound .. maxBound]

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlComboBoxNamedNew
    :: forall r m e.
    (SpracheGuiReader r m, MonadIO m, Eq e)
    => NonEmpty e
    -> Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> (e -> Sprache -> Text)
    -> m (AuswahlWidget e)
auswahlComboBoxNamedNew elemente@(h :| _t) maybeTVar name anzeigeFunktion = do
    listStore <- Gtk.listStoreNew [GI.gtypeString]
    comboBox <- Gtk.comboBoxNewWithModel listStore
    -- erste Spalte wird als text dargestellt
    Gtk.cellLayoutClear comboBox
    cellRenderer <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart comboBox cellRenderer True
    Gtk.cellLayoutAddAttribute comboBox cellRenderer "text" 0
    -- TODO Attribute type "widget" ausprobieren?
    -- zeige Titel-Label
    widget <- erhalteWidget comboBox
    nameLabel <- containerAddWidgetNew comboBox $ Gtk.labelNew Nothing
    Gtk.setLabelMaxWidthChars nameLabel nameWrapSize
    Gtk.setLabelWrap nameLabel True
    -- füge Element zu listStore hinzu
    let foldFn :: [(e, Int32, Gtk.TreeIter)] -> e -> m [(e, Int32, Gtk.TreeIter)]
        foldFn acc e = do
            iter <- Gtk.listStoreAppend listStore
            path <- Gtk.treeModelGetPath listStore iter
            Gtk.treeModelRowChanged listStore path iter
            let index = fromIntegral $ length acc
            when (e == h) $ Gtk.comboBoxSetActive comboBox index
            pure $ (e, index, iter) : acc
    enumIndicesIters <- NonEmpty.fromList . reverse <$> foldM foldFn [] elemente
    -- ändere Texte bei Sprachwechsel
    let auswahlComboBox = AuswahlComboBox { widget, comboBox, enumIndicesIters }
    verwendeSpracheGui maybeTVar $ \sprache -> do
        aktuellerWert <- aktuelleAuswahl auswahlComboBox
        Gtk.setLabelLabel nameLabel $ name <:> anzeigeFunktion aktuellerWert $ sprache
        forM_ enumIndicesIters $ \(e, _index, iter) -> do
            gValue <- liftIO $ Gtk.toGValue $ Just $ anzeigeFunktion e sprache
            Gtk.listStoreSetValue listStore iter 0 gValue
    spracheGui <- erhalteSpracheGui
    beiAuswahl auswahlComboBox $ \aktuellerWert -> flip leseSprache spracheGui $ \sprache
        -> Gtk.setLabelLabel nameLabel $ name <:> anzeigeFunktion aktuellerWert $ sprache
    pure auswahlComboBox

-- | Konstruiere ein 'AuswahlWidget' mit einer 'Gtk.ComboBox' unter Verwendung der 'Anzeige'-Instanz.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlComboBoxNew :: (SpracheGuiReader r m, MonadIO m, Eq e, Anzeige e)
                   => NonEmpty e
                   -> Maybe TVarSprachewechselAktionen
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
    -> Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> m (AuswahlWidget e)
boundedEnumAuswahlComboBoxNew
    standard = auswahlComboBoxNew $ standard :| delete standard [minBound .. maxBound]

-- | Setzte den aktuellen Wert eines 'AuswahlWidget'.
--
-- Wenn der Wert nicht im 'AuswahlWidget' enthalten ist wird der aktuelle Wert nicht verändert.
setzeAuswahl :: forall m e. (MonadIO m, Eq e) => AuswahlWidget e -> e -> m ()
setzeAuswahl AuswahlRadioButton {enumButtons} wert = forM_ enumButtons $ \(e, radioButton)
    -> when (e == wert) $ Gtk.setToggleButtonActive radioButton True
setzeAuswahl AuswahlComboBox {comboBox, enumIndicesIters} wert =
    setzeWert $ NonEmpty.toList enumIndicesIters
    where
        setzeWert :: [(e, Int32, Gtk.TreeIter)] -> m ()
        setzeWert [] = pure ()
        setzeWert ((e, index, _iter):t)
            | e == wert = Gtk.comboBoxSetActive comboBox index
            | otherwise = setzeWert t

-- | Erhalte den aktuell ausgewählten Wert.
aktuelleAuswahl :: (MonadIO m, Eq e) => AuswahlWidget e -> m e
aktuelleAuswahl AuswahlRadioButton {enumButtons} = fromJust <$> foldM foldEnum Nothing enumButtons
    where
        foldEnum :: (MonadIO m) => Maybe e -> (e, Gtk.RadioButton) -> m (Maybe e)
        foldEnum justE@(Just _e) _enumButton = pure justE
        foldEnum Nothing (e, radioButton) = do
            toggled <- Gtk.getToggleButtonActive radioButton
            pure
                $ if toggled
                    then Just e
                    else Nothing
aktuelleAuswahl AuswahlComboBox {comboBox, enumIndicesIters} = do
    activeIndex <- Gtk.comboBoxGetActive comboBox
    let foldEnum :: (Eq e) => (e, Int32, Gtk.TreeIter) -> Maybe e -> Maybe e
        foldEnum _enumIndex justE@(Just _e) = justE
        foldEnum (e, index, _iter) Nothing
            | index == activeIndex = Just e
            | otherwise = Nothing
    case foldr foldEnum Nothing enumIndicesIters of
        (Just e) -> pure e
        Nothing -> do
            let (e, index, _iter) = NonEmpty.head enumIndicesIters
            Gtk.comboBoxSetActive comboBox index
            pure e

-- | Führe die übergebene Aktion bei Änderung der Auswahl aus (vgl. 'Gtk.on')
beiAuswahl :: (Eq e, MonadIO m) => AuswahlWidget e -> (e -> IO ()) -> m ()
beiAuswahl auswahlWidget@AuswahlRadioButton {enumButtons} aktion =
    forM_ enumButtons $ \(e, radioButton) -> Gtk.onToggleButtonToggled radioButton $ do
        auswahl <- aktuelleAuswahl auswahlWidget
        when (e == auswahl) $ aktion e
beiAuswahl auswahlWidget@AuswahlComboBox {comboBox} aktion =
    void $ Gtk.onComboBoxChanged comboBox $ aktuelleAuswahl auswahlWidget >>= aktion

-- | Klasse für Typen mit 'AuswahlWidget'
class (MitWidget a) => MitAuswahlWidget a e where
    erhalteAuswahlWidget :: a -> AuswahlWidget e

instance MitAuswahlWidget (AuswahlWidget e) e where
    erhalteAuswahlWidget :: AuswahlWidget e -> AuswahlWidget e
    erhalteAuswahlWidget = id
