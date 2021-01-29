{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Description: Widget zur Darstellung und Auswahl eines Anschluss
-}
module Zug.UI.Gtk.Anschluss
  ( PinWidget()
  , pinNew
  , AnschlussWidget()
  , anschlussNew
  , anschlussAuswahlInterruptPinNew
  , PinAuswahlWidget()
  , pinAuswahlNew
  , aktuellerPin
  , setzePin
  , AnschlussAuswahlWidget()
  , anschlussAuswahlNew
  , aktuellerAnschluss
  , setzeAnschluss
  ) where

import Control.Monad.Trans (MonadIO())
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Text as Text
import qualified GI.Gtk as Gtk

import Zug.Anbindung
       (Anschluss(..), AnschlussEither(..), MitInterruptPin(..), InterruptPinBenötigt(..)
      , PCF8574Klasse(ohneInterruptPin), AnschlussKlasse(..), PCF8574Port(..), PCF8574(..)
      , PCF8574Variant(..), Value(..), Pin(Gpio))
import Zug.Language (Sprache(..), (<->), (<:>))
import qualified Zug.Language as Language
import Zug.UI.Gtk.Auswahl (AuswahlWidget, aktuelleAuswahl, setzeAuswahl
                         , boundedEnumAuswahlRadioButtonNew, boundedEnumAuswahlComboBoxNew)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, notebookAppendPageNew, labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitLabel(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), TVarSprachewechselAktionen)

-- | Anzeige eines 'Pin's.
newtype PinWidget = PinWidget Gtk.Label
    deriving (Eq)

instance MitWidget PinWidget where
    erhalteWidget :: (MonadIO m) => PinWidget -> m Gtk.Widget
    erhalteWidget (PinWidget label) = erhalteWidget label

instance MitLabel PinWidget where
    erhalteLabel :: (MonadIO m) => PinWidget -> m Gtk.Label
    erhalteLabel (PinWidget label) = pure label

-- | 'Label' für 'Pin' erstellen.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
pinNew :: (SpracheGuiReader r m, MonadIO m)
       => Maybe TVarSprachewechselAktionen
       -> (Sprache -> Text)
       -> Pin
       -> m PinWidget
pinNew maybeTVar name pin =
    fmap PinWidget $ labelSpracheNew maybeTVar $ name <-> Language.pin <:> pin

-- | Anzeige eines 'Anschluss'.
newtype AnschlussWidget = AnschlussWidget Gtk.Label
    deriving (Eq)

instance MitWidget AnschlussWidget where
    erhalteWidget :: (MonadIO m) => AnschlussWidget -> m Gtk.Widget
    erhalteWidget (AnschlussWidget label) = erhalteWidget label

instance MitLabel AnschlussWidget where
    erhalteLabel :: (MonadIO m) => AnschlussWidget -> m Gtk.Label
    erhalteLabel (AnschlussWidget label) = pure label

-- | 'Label' für 'Anschluss' erstellen.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
anschlussNew :: (SpracheGuiReader r m, MonadIO m)
             => Maybe TVarSprachewechselAktionen
             -> (Sprache -> Text)
             -> AnschlussEither
             -> m AnschlussWidget
anschlussNew maybeTVar name anschluss =
    fmap AnschlussWidget $ labelSpracheNew maybeTVar $ name <-> Language.anschluss <:> anschluss

-- | Widgets zum erzeugen eines 'Pin's.
data PinAuswahlWidget =
    PinAuswahlWidget { pawWidget :: Gtk.Widget, pawSpinButton :: Gtk.SpinButton }
    deriving (Eq)

instance MitWidget PinAuswahlWidget where
    erhalteWidget :: (MonadIO m) => PinAuswahlWidget -> m Gtk.Widget
    erhalteWidget = pure . pawWidget

-- | Erzeugen eines'Pin's.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
pinAuswahlNew :: (SpracheGuiReader r m, MonadIO m)
              => Maybe TVarSprachewechselAktionen
              -> (Sprache -> Text)
              -> m PinAuswahlWidget
pinAuswahlNew maybeTVar name = do
    hBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    pawWidget <- erhalteWidget hBox
    boxPackWidgetNewDefault hBox $ labelSpracheNew maybeTVar $ name <-> Language.pin <:> Text.empty
    pawSpinButton <- do
        aawPin <- boxPackWidgetNewDefault hBox $ Gtk.spinButtonNewWithRange 0 27 1
        Gtk.setSpinButtonSnapToTicks aawPin True
        Gtk.setSpinButtonNumeric aawPin True
        pure aawPin
    pure PinAuswahlWidget { pawWidget, pawSpinButton }

-- | Erhalte den aktuell gewählten 'Pin's.
aktuellerPin :: (MonadIO m) => PinAuswahlWidget -> m Pin
aktuellerPin PinAuswahlWidget {pawSpinButton} =
    Gpio . fromIntegral <$> Gtk.spinButtonGetValueAsInt pawSpinButton

-- | Setze den aktuellen Pin.
setzePin :: (MonadIO m) => PinAuswahlWidget -> Pin -> m ()
setzePin PinAuswahlWidget {pawSpinButton} pin =
    Gtk.setSpinButtonValue pawSpinButton $ fromJust $ zuPinGpio pin

-- | Widgets zum erzeugen eines 'Anschluss'.
data AnschlussAuswahlWidget (i :: InterruptPinBenötigt) where
    AnschlussAuswahlWidget
        :: { aawWidget :: Gtk.Widget
           , aawNotebook :: Gtk.Notebook
           , aawPinPage :: Int32
           , aawPin :: PinAuswahlWidget
           , aawPCF8574PortPage :: Int32
           , aawPCF8574PortBox :: Gtk.Box
           , aawPCF8574PortVariante :: AuswahlWidget PCF8574Variant
           , aawPCF8574PortA0 :: AuswahlWidget Value
           , aawPCF8574PortA1 :: AuswahlWidget Value
           , aawPCF8574PortA2 :: AuswahlWidget Value
           , aawPCF8574Port :: Gtk.SpinButton
           } -> AnschlussAuswahlWidget 'InterruptPinEgal
    AnschlussAuswahlWidgetInterruptPin
        :: { aawAnschlussAuswahlWidget :: AnschlussAuswahlWidget 'InterruptPinEgal
           , aawPCF8574PortInterruptPin :: PinAuswahlWidget
           } -> AnschlussAuswahlWidget 'InterruptPinBenötigt

deriving instance Eq (AnschlussAuswahlWidget i)

instance MitWidget (AnschlussAuswahlWidget i) where
    erhalteWidget :: (MonadIO m) => AnschlussAuswahlWidget i -> m Gtk.Widget
    erhalteWidget AnschlussAuswahlWidget {aawWidget} = pure aawWidget
    erhalteWidget AnschlussAuswahlWidgetInterruptPin {aawAnschlussAuswahlWidget} =
        erhalteWidget aawAnschlussAuswahlWidget

-- | Erzeugen eines'Anschluss'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus
-- 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
anschlussAuswahlNew :: (SpracheGuiReader r m, MonadIO m)
                    => Maybe TVarSprachewechselAktionen
                    -> (Sprache -> Text)
                    -> m (AnschlussAuswahlWidget 'InterruptPinEgal)
anschlussAuswahlNew maybeTVar name = do
    (vBox, aawNotebook) <- do
        vBox <- Gtk.boxNew Gtk.OrientationVertical 0
        aawNotebook <- boxPackWidgetNewDefault vBox Gtk.notebookNew
        pure (vBox, aawNotebook)
    aawWidget <- erhalteWidget vBox
    -- Pin
    (aawPin, aawPinPage)
        <- notebookAppendPageNew aawNotebook maybeTVar Language.pin $ pinAuswahlNew maybeTVar name
    -- PCF8574Port
    (aawPCF8574PortBox, aawPCF8574PortPage)
        <- notebookAppendPageNew aawNotebook maybeTVar Language.pcf8574Port
        $ Gtk.boxNew Gtk.OrientationHorizontal 0
    boxPackWidgetNewDefault aawPCF8574PortBox
        $ labelSpracheNew maybeTVar
        $ name <-> Language.pcf8574Port <:> Text.empty
    aawPCF8574PortVariante <- boxPackWidgetNewDefault aawPCF8574PortBox
        $ boundedEnumAuswahlComboBoxNew VariantA maybeTVar Language.variante
    aawPCF8574PortA0 <- boxPackWidgetNewDefault aawPCF8574PortBox
        $ boundedEnumAuswahlRadioButtonNew HIGH maybeTVar Language.a0
    aawPCF8574PortA1 <- boxPackWidgetNewDefault aawPCF8574PortBox
        $ boundedEnumAuswahlRadioButtonNew HIGH maybeTVar Language.a1
    aawPCF8574PortA2 <- boxPackWidgetNewDefault aawPCF8574PortBox
        $ boundedEnumAuswahlRadioButtonNew HIGH maybeTVar Language.a2
    boxPackWidgetNewDefault aawPCF8574PortBox
        $ labelSpracheNew maybeTVar
        $ Language.port <:> Text.empty
    aawPCF8574Port <- do
        aawPCF8574Port
            <- boxPackWidgetNewDefault aawPCF8574PortBox $ Gtk.spinButtonNewWithRange 0 7 1
        Gtk.setSpinButtonSnapToTicks aawPCF8574Port True
        Gtk.setSpinButtonNumeric aawPCF8574Port True
        pure aawPCF8574Port
    pure
        AnschlussAuswahlWidget
        { aawWidget
        , aawNotebook
        , aawPinPage
        , aawPin
        , aawPCF8574PortPage
        , aawPCF8574PortBox
        , aawPCF8574PortVariante
        , aawPCF8574PortA0
        , aawPCF8574PortA1
        , aawPCF8574PortA2
        , aawPCF8574Port
        }

-- | Erzeugen eines'Anschluss' mit InterruptPin.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus
-- 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
anschlussAuswahlInterruptPinNew
    :: (SpracheGuiReader r m, MonadIO m, MonadFail m)
    => Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> m (AnschlussAuswahlWidget 'InterruptPinBenötigt)
anschlussAuswahlInterruptPinNew maybeTVar name = do
    aawAnschlussAuswahlWidget@AnschlussAuswahlWidget
        {aawPCF8574PortBox} <- anschlussAuswahlNew maybeTVar name
    auswahlInterruptPin
        <- boxPackWidgetNewDefault aawPCF8574PortBox $ pinAuswahlNew maybeTVar Language.interrupt
    pure
        AnschlussAuswahlWidgetInterruptPin
        { aawAnschlussAuswahlWidget
        , aawPCF8574PortInterruptPin = auswahlInterruptPin
        }

type family AnschlussAuswahl (i :: InterruptPinBenötigt) where
    AnschlussAuswahl 'InterruptPinBenötigt = Anschluss 'MitInterruptPin
    AnschlussAuswahl 'InterruptPinEgal = AnschlussEither

-- | Erhalte den aktuell gewählten 'Anschluss'.
aktuellerAnschluss :: (MonadIO m) => AnschlussAuswahlWidget i -> m (AnschlussAuswahl i)
aktuellerAnschluss
    AnschlussAuswahlWidget
    { aawNotebook
    , aawPin
    , aawPCF8574PortPage
    , aawPCF8574PortVariante
    , aawPCF8574PortA0
    , aawPCF8574PortA1
    , aawPCF8574PortA2
    , aawPCF8574Port} = Gtk.notebookGetCurrentPage aawNotebook >>= \case
    page
        | page == aawPCF8574PortPage -> do
            variant <- aktuelleAuswahl aawPCF8574PortVariante
            a0 <- aktuelleAuswahl aawPCF8574PortA0
            a1 <- aktuelleAuswahl aawPCF8574PortA1
            a2 <- aktuelleAuswahl aawPCF8574PortA2
            port <- fromIntegral <$> Gtk.spinButtonGetValueAsInt aawPCF8574Port
            pure
                $ AnschlussOhne
                $ AnschlussPCF8574Port
                $ PCF8574Port { pcf8574 = PCF8574 { variant, a0, a1, a2 }, port }
        -- Verwende als Standard die Pin-Eingabe
        | otherwise -> AnschlussMit . AnschlussPin <$> aktuellerPin aawPin
aktuellerAnschluss
    AnschlussAuswahlWidgetInterruptPin
    { aawAnschlussAuswahlWidget = AnschlussAuswahlWidget
          { aawNotebook
          , aawPin
          , aawPCF8574PortPage
          , aawPCF8574PortVariante
          , aawPCF8574PortA0
          , aawPCF8574PortA1
          , aawPCF8574PortA2
          , aawPCF8574Port}
    , aawPCF8574PortInterruptPin} = Gtk.notebookGetCurrentPage aawNotebook >>= \case
    page
        | page == aawPCF8574PortPage -> do
            iVariant <- aktuelleAuswahl aawPCF8574PortVariante
            iA0 <- aktuelleAuswahl aawPCF8574PortA0
            iA1 <- aktuelleAuswahl aawPCF8574PortA1
            iA2 <- aktuelleAuswahl aawPCF8574PortA2
            port <- fromIntegral <$> Gtk.spinButtonGetValueAsInt aawPCF8574Port
            interruptPin <- aktuellerPin aawPCF8574PortInterruptPin
            pure
                $ AnschlussPCF8574Port
                $ PCF8574Port
                { pcf8574 = PCF8574InterruptPin { iVariant, iA0, iA1, iA2, interruptPin }
                , port
                }
        -- Verwende als Standard die Pin-Eingabe
        | otherwise -> AnschlussPin <$> aktuellerPin aawPin

-- | Setze den aktuellen 'Anschluss'.
setzeAnschluss :: (MonadIO m) => AnschlussAuswahlWidget i -> AnschlussAuswahl i -> m ()
setzeAnschluss
    AnschlussAuswahlWidget {aawNotebook, aawPin, aawPinPage}
    (AnschlussMit AnschlussPin {pin}) = do
    Gtk.setNotebookPage aawNotebook aawPinPage
    setzePin aawPin pin
setzeAnschluss
    anschlussAuswahlWidget@AnschlussAuswahlWidget {}
    (AnschlussMit AnschlussPCF8574Port {pcf8574Port}) =
    setzeAnschluss anschlussAuswahlWidget
    $ AnschlussOhne
    $ AnschlussPCF8574Port
    $ ohneInterruptPin pcf8574Port
setzeAnschluss
    AnschlussAuswahlWidget
    { aawNotebook
    , aawPCF8574PortPage
    , aawPCF8574PortVariante
    , aawPCF8574PortA0
    , aawPCF8574PortA1
    , aawPCF8574PortA2
    , aawPCF8574Port}
    (AnschlussOhne
         AnschlussPCF8574Port
         {pcf8574Port = PCF8574Port {pcf8574 = PCF8574 {variant, a0, a1, a2}, port}}) = do
    Gtk.setNotebookPage aawNotebook aawPCF8574PortPage
    setzeAuswahl aawPCF8574PortVariante variant
    setzeAuswahl aawPCF8574PortA0 a0
    setzeAuswahl aawPCF8574PortA1 a1
    setzeAuswahl aawPCF8574PortA2 a2
    Gtk.setSpinButtonValue aawPCF8574Port $ fromIntegral port
setzeAnschluss
    AnschlussAuswahlWidgetInterruptPin
    {aawAnschlussAuswahlWidget = AnschlussAuswahlWidget {aawNotebook, aawPin, aawPinPage}}
    AnschlussPin {pin} = do
    Gtk.setNotebookPage aawNotebook aawPinPage
    setzePin aawPin pin
setzeAnschluss
    AnschlussAuswahlWidgetInterruptPin
    { aawAnschlussAuswahlWidget = AnschlussAuswahlWidget
          { aawNotebook
          , aawPCF8574PortPage
          , aawPCF8574PortVariante
          , aawPCF8574PortA0
          , aawPCF8574PortA1
          , aawPCF8574PortA2
          , aawPCF8574Port}
    , aawPCF8574PortInterruptPin}
    AnschlussPCF8574Port
    { pcf8574Port =
          PCF8574Port {pcf8574 = PCF8574InterruptPin {iVariant, iA0, iA1, iA2, interruptPin}, port}} =
    do
        Gtk.setNotebookPage aawNotebook aawPCF8574PortPage
        setzeAuswahl aawPCF8574PortVariante iVariant
        setzeAuswahl aawPCF8574PortA0 iA0
        setzeAuswahl aawPCF8574PortA1 iA1
        setzeAuswahl aawPCF8574PortA2 iA2
        Gtk.setSpinButtonValue aawPCF8574Port $ fromIntegral port
        setzePin aawPCF8574PortInterruptPin interruptPin
