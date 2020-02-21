{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

{-|
Description: Widget zur Darstellung und Auswahl eines Anschluss
-}
module Zug.UI.Gtk.Anschluss
  (
#ifdef ZUGKONTROLLEGUI
    AnschlussWidget()
  , anschlussNew
  , AnschlussAuswahlWidget()
  , anschlussAuswahlNew
  , aktuellerAnschluss
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Data.Text as Text
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk

-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (Anschluss(), vonPinGpio, vonPCF8574Port, PCF8574Port(..), PCF8574(..)
                    , PCF8574Variant(..), Value(..))
import Zug.Language (Sprache(..), (<->), (<:>))
import qualified Zug.Language as Language
import Zug.UI.Gtk.Auswahl (AuswahlWidget, aktuelleAuswahl, boundedEnumAuswahlRadioButtonNew
                         , boundedEnumAuswahlComboBoxNew)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, notebookAppendPageNew, labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitLabel(..), MitNotebook(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader())

-- | Anzeige eines 'Anschluss'
newtype AnschlussWidget = AnschlussWidget Gtk.Label
    deriving (Eq)

instance MitWidget AnschlussWidget where
    erhalteWidget :: AnschlussWidget -> Gtk.Widget
    erhalteWidget (AnschlussWidget label) = erhalteWidget label

instance MitLabel AnschlussWidget where
    erhalteLabel :: AnschlussWidget -> Gtk.Label
    erhalteLabel (AnschlussWidget label) = label

-- | 'Label' für 'Anschluss' erstellen.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
anschlussNew :: (SpracheGuiReader r m, MonadIO m)
             => Maybe (TVar (Maybe [Sprache -> IO ()]))
             -> (Sprache -> Text)
             -> Anschluss
             -> m AnschlussWidget
anschlussNew maybeTVar name anschluss =
    fmap AnschlussWidget $ labelSpracheNew maybeTVar $ name <-> Language.anschluss <:> anschluss

-- | Widgets zum erzeugen eines 'Anschluss'
data AnschlussAuswahlWidget =
    AnschlussAuswahlWidget
    { aawWidget :: Gtk.Widget
    , aawNotebook :: Gtk.Notebook
    , aawPinPage :: Int
    , aawPin :: Gtk.SpinButton
    , aawPCF8574PortPage :: Int
    , aawPCF8574PortVariante :: AuswahlWidget PCF8574Variant
    , aawPCF8574PortA0 :: AuswahlWidget Value
    , aawPCF8574PortA1 :: AuswahlWidget Value
    , aawPCF8574PortA2 :: AuswahlWidget Value
    , aawPCF8574Port :: Gtk.SpinButton
    }
    deriving (Eq)

instance MitWidget AnschlussAuswahlWidget where
    erhalteWidget :: AnschlussAuswahlWidget -> Gtk.Widget
    erhalteWidget = aawWidget

-- | Erzeugen eines'Anschluss'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
anschlussAuswahlNew :: (SpracheGuiReader r m, MonadIO m)
                    => Maybe (TVar (Maybe [Sprache -> IO ()]))
                    -> (Sprache -> Text)
                    -> m AnschlussAuswahlWidget
anschlussAuswahlNew maybeTVar name = do
    (vBox, aawNotebook) <- liftIO $ do
        vBox <- Gtk.vBoxNew False 0
        aawNotebook <- boxPackWidgetNewDefault vBox Gtk.notebookNew
        pure (vBox, aawNotebook)
    -- Pin
    (pinBox, aawPinPage)
        <- notebookAppendPageNew aawNotebook maybeTVar Language.pin $ liftIO $ Gtk.hBoxNew False 0
    boxPackWidgetNewDefault pinBox
        $ labelSpracheNew maybeTVar
        $ name <-> Language.pin <:> Text.empty
    aawPin <- liftIO $ do
        aawPin <- boxPackWidgetNewDefault pinBox $ Gtk.spinButtonNewWithRange 0 27 1
        Gtk.set aawPin [Gtk.spinButtonSnapToTicks := True, Gtk.spinButtonNumeric := True]
        pure aawPin
    -- PCF8574Port
    (pcf8574Box, aawPCF8574PortPage)
        <- notebookAppendPageNew aawNotebook maybeTVar Language.pcf8574Port
        $ liftIO
        $ Gtk.hBoxNew False 0
    boxPackWidgetNewDefault pcf8574Box
        $ labelSpracheNew maybeTVar
        $ name <-> Language.pcf8574Port <:> Text.empty
    aawPCF8574PortVariante <- boxPackWidgetNewDefault pcf8574Box
        $ boundedEnumAuswahlComboBoxNew VariantA maybeTVar Language.variante
    aawPCF8574PortA0 <- boxPackWidgetNewDefault pcf8574Box
        $ boundedEnumAuswahlRadioButtonNew LOW maybeTVar Language.a0
    aawPCF8574PortA1 <- boxPackWidgetNewDefault pcf8574Box
        $ boundedEnumAuswahlRadioButtonNew LOW maybeTVar Language.a1
    aawPCF8574PortA2 <- boxPackWidgetNewDefault pcf8574Box
        $ boundedEnumAuswahlRadioButtonNew LOW maybeTVar Language.a2
    boxPackWidgetNewDefault pcf8574Box $ labelSpracheNew maybeTVar $ Language.port <:> Text.empty
    aawPCF8574Port <- liftIO $ do
        aawPCF8574Port <- boxPackWidgetNewDefault pcf8574Box $ Gtk.spinButtonNewWithRange 0 7 1
        Gtk.set aawPCF8574Port [Gtk.spinButtonSnapToTicks := True, Gtk.spinButtonNumeric := True]
        pure aawPCF8574Port
    pure
        AnschlussAuswahlWidget
        { aawWidget = erhalteWidget vBox,
          aawNotebook,
          aawPinPage,
          aawPin,
          aawPCF8574PortPage,
          aawPCF8574PortVariante,
          aawPCF8574PortA0,
          aawPCF8574PortA1,
          aawPCF8574PortA2,
          aawPCF8574Port
        }

-- | Erhalte den aktuell gewählten 'Anschluss'.
aktuellerAnschluss :: (MonadIO m) => AnschlussAuswahlWidget -> m Anschluss
aktuellerAnschluss
    AnschlussAuswahlWidget
    {aawNotebook,
     aawPin,
     aawPCF8574PortPage,
     aawPCF8574PortVariante,
     aawPCF8574PortA0,
     aawPCF8574PortA1,
     aawPCF8574PortA2,
     aawPCF8574Port} = liftIO $ do
    Gtk.notebookGetCurrentPage (erhalteNotebook aawNotebook) >>= \case
        page
            | page == aawPCF8574PortPage -> liftIO $ do
                variant <- aktuelleAuswahl aawPCF8574PortVariante
                a0 <- aktuelleAuswahl aawPCF8574PortA0
                a1 <- aktuelleAuswahl aawPCF8574PortA1
                a2 <- aktuelleAuswahl aawPCF8574PortA2
                port <- fromIntegral <$> Gtk.spinButtonGetValueAsInt aawPCF8574Port
                pure
                    $ vonPCF8574Port
                    $ PCF8574Port
                    { pcf8574 = PCF8574
                          { variant,
                            a0,
                            a1,
                            a2
                          },
                      port
                    }
            -- Verwende als Standard die Pin-Eingabe
            | otherwise -> liftIO $ vonPinGpio <$> Gtk.spinButtonGetValueAsInt aawPin
#endif






