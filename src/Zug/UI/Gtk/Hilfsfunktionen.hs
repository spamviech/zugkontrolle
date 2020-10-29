{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
#endif

{-|
Description: Allgemeine Hilfsfunktionen
-}
module Zug.UI.Gtk.Hilfsfunktionen
  (
#ifdef ZUGKONTROLLEGUI
    -- * Widget
    widgetShowNew
  , widgetShowIf
    -- * Container
  , containerAddWidgetNew
  , containerRemoveJust
    -- * Box
  , boxPack
  , boxPackDefault
  , boxPackWidgetNew
  , boxPackWidgetNewDefault
  , Packing(..)
  , packingDefault
  , Padding(..)
  , paddingDefault
  , Position(..)
  , positionDefault
    -- * Notebook
  , notebookAppendPageNew
    -- * Dialog
  , dialogGetUpper
  , dialogEval
    -- * Button
  , buttonNewWithEvent
  , buttonNewWithEventLabel -- buttonNewWithEventMnemonic,
    -- * ToggleButton
  , toggleButtonNewWithEvent
  , toggleButtonNewWithEventLabel
    -- * Label
  , labelSpracheNew
    -- * Name
  , NameWidget()
  , namePackNew
  , NameAuswahlWidget()
  , nameAuswahlPackNew
  , aktuellerName
  , setzeName
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Monad.Trans (MonadIO(..))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import qualified GI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(..))
import Zug.Language (Sprache(..), (<:>))
import qualified Zug.Language as Language
import Zug.UI.Gtk.Klassen
       (MitWidget(..), mitWidgetShow, mitWidgetHide, MitLabel(..), MitEntry(..), MitContainer(..)
      , mitContainerAdd, mitContainerRemove, MitButton(..), MitToggleButton(..), MitDialog(..)
      , MitBox(..), mitBoxPackStart, mitBoxPackEnd, MitNotebook(..), mitNotebookAppendPage)
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader, verwendeSpracheGui, TVarSprachewechselAktionen)

-- | 'Widget' erstellen und anzeigen
widgetShowNew :: (MonadIO m, MitWidget w) => m w -> m w
widgetShowNew konstruktor = do
    widget <- konstruktor
    mitWidgetShow widget
    pure widget

-- | Neu erstelltes 'Widget' zu 'Container' hinzufügen
containerAddWidgetNew :: (MonadIO m, MitContainer c, MitWidget w) => c -> m w -> m w
containerAddWidgetNew container konstruktor = do
    widget <- widgetShowNew konstruktor
    mitContainerAdd container widget
    pure widget

-- | 'Widget' in eine 'Box' packen
boxPack :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Packing -> Padding -> Position -> m ()
boxPack box widget packing padding position =
    liftIO
    $ boxPackPosition position box widget (expands packing) (fills packing)
    $ fromPadding padding
    where
        boxPackPosition
            :: (MitBox b, MitWidget w) => Position -> b -> w -> Bool -> Bool -> Word32 -> IO ()
        boxPackPosition Start = mitBoxPackStart
        boxPackPosition End = mitBoxPackEnd

-- | Neu erstelltes Widget in eine Box packen
boxPackWidgetNew
    :: (MonadIO m, MitBox b, MitWidget w) => b -> Packing -> Padding -> Position -> m w -> m w
boxPackWidgetNew box packing padding start konstruktor = do
    widget <- widgetShowNew konstruktor
    boxPack box widget packing padding start
    pure widget

-- | Packing-Einstellungen. Sie werden in /expand/- und /fill/-Werte übersetzt.
-- * PackNatural: expand=False
-- * PackGrow: expand=True, fill=True
-- * PackRepel: expand=True, fill=False
data Packing
    = PackNatural
    | PackGrow
    | PackRepel

expands :: Packing -> Bool
expands PackNatural = False
expands PackGrow = True
expands PackRepel = True

fills :: Packing -> Bool
fills PackNatural = False
fills PackGrow = True
fills PackRepel = False

-- | Normale Packing-Einstellung
packingDefault :: Packing
packingDefault = PackNatural

-- | Abstand zwischen 'Widget's
newtype Padding = Padding { fromPadding :: Word32 }
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

-- | Standart-Abstand zwischen 'Widget's in einer 'Box'
paddingDefault :: Padding
paddingDefault = 0

-- | Position zum Hinzufügen zu einer 'Box'
data Position
    = Start
    | End
    deriving (Show, Read, Eq, Ord)

-- | Standart-Position zum Hinzufügen zu einer 'Box'
positionDefault :: Position
positionDefault = Start

-- | Neu erstelltes Widget mit Standard Packing, Padding und Positionierung in eine Box packen
boxPackWidgetNewDefault :: (MonadIO m, MitBox b, MitWidget w) => b -> m w -> m w
boxPackWidgetNewDefault box = boxPackWidgetNew box packingDefault paddingDefault positionDefault

-- | Widget mit Standard Packing, Padding und Positionierung in eine Box packen
boxPackDefault :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> m ()
boxPackDefault box widget = boxPack box widget packingDefault paddingDefault positionDefault

-- | Neu erstelltes 'MitWidget' zu einem 'MitNotebook' hinzufügen.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
notebookAppendPageNew
    :: (SpracheGuiReader r m, MonadIO m, MitNotebook n, MitWidget w)
    => n
    -> Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> m w
    -> m (w, Int32)
notebookAppendPageNew mitNotebook maybeTVar name konstruktor = do
    mitWidget <- widgetShowNew konstruktor
    widget <- erhalteWidget mitWidget
    page <- mitNotebookAppendPage mitNotebook widget $ name Deutsch
    notebook <- erhalteNotebook mitNotebook
    verwendeSpracheGui maybeTVar
        $ \sprache -> Gtk.notebookSetMenuLabelText notebook widget $ name sprache
    pure (mitWidget, page)

-- | Entferne ein vielleicht vorhandenes 'MitWidget' aus einem 'MitContainer'
containerRemoveJust :: (MonadIO m, MitContainer c, MitWidget w) => c -> Maybe w -> m ()
containerRemoveJust _container Nothing = pure ()
containerRemoveJust container (Just w) = mitContainerRemove container w

-- | Zeige widget, falls eine Bedingung erfüllt ist
widgetShowIf :: (MonadIO m, MitWidget w) => Bool -> w -> m ()
widgetShowIf True = mitWidgetShow
widgetShowIf False = mitWidgetHide

-- | 'MitDialog' anzeigen, auswerten und wieder verstecken
dialogEval :: (MonadIO m, MitDialog d) => d -> m Int32
dialogEval mitDialog = liftIO $ do
    dialog <- erhalteDialog mitDialog
    Gtk.widgetShow dialog
    antwort <- Gtk.dialogRun dialog
    Gtk.widgetHide dialog
    pure antwort

-- | dialogGetUpper fehlt in gtk3, daher hier ersetzt
dialogGetUpper :: (MitDialog d) => d -> IO Gtk.Box
dialogGetUpper mitDialog = Gtk.dialogGetContentArea =<< erhalteDialog mitDialog

-- * Knöpfe mit einer Funktion
-- | Knopf mit Funktion erstellen
buttonNewWithEvent :: (MonadIO m, MitButton b) => m b -> IO () -> m b
buttonNewWithEvent konstruktor event = do
    mitButton <- konstruktor
    liftIO $ do
        button <- erhalteButton mitButton
        Gtk.onButtonClicked button event
        pure mitButton

-- -- | Knopf mit Mnemonic-Label und Funktion erstellen
-- buttonNewWithEventMnemonic :: (SpracheGuiReader r m, MonadIO m) => (Sprache -> Text) -> IO () -> m Gtk.Button
-- buttonNewWithEventMnemonic label event = do
--     button <- liftIO $ buttonNewWithEvent Gtk.buttonNew event
--     verwendeSpracheGui $ \sprache -> do
--         let labelSprache = label sprache
--         Gtk.set button [Gtk.buttonLabel := (Language.addMnemonic labelSprache)]
--         _adjustMnemonic $ Text.head labelSprache
--     pure button
-- | Knopf mit Label und Funktion erstellen.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonNewWithEventLabel :: (SpracheGuiReader r m, MonadIO m)
                        => Maybe TVarSprachewechselAktionen
                        -> (Sprache -> Text)
                        -> IO ()
                        -> m Gtk.Button
buttonNewWithEventLabel maybeTVar label event = do
    button <- liftIO $ buttonNewWithEvent Gtk.buttonNew event
    verwendeSpracheGui maybeTVar $ \sprache -> Gtk.setButtonLabel button $ label sprache
    pure button

-- * ToggleButton
-- | ToggleButton mit Funktion erstellen
toggleButtonNewWithEvent :: (MonadIO m, MitToggleButton b) => m b -> (Bool -> IO ()) -> m b
toggleButtonNewWithEvent konstruktor event = do
    mitToggleButton <- konstruktor
    toggleButton <- erhalteToggleButton mitToggleButton
    Gtk.onToggleButtonToggled toggleButton $ Gtk.getToggleButtonActive toggleButton >>= event
    pure mitToggleButton

-- | ToggleButton mit Label und Funktion erstellen.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
toggleButtonNewWithEventLabel
    :: (SpracheGuiReader r m, MonadIO m)
    => Maybe TVarSprachewechselAktionen
    -> (Sprache -> Text)
    -> (Bool -> IO ())
    -> m Gtk.ToggleButton
toggleButtonNewWithEventLabel maybeTVar label event = do
    toggleButton <- liftIO $ toggleButtonNewWithEvent Gtk.toggleButtonNew event
    verwendeSpracheGui maybeTVar $ \sprache -> Gtk.setButtonLabel toggleButton $ label sprache
    pure toggleButton

-- * Label
-- | Erzeuge ein Label, welches bei 'Zug.UI.Gtk.SpracheGui.sprachwechsel' den angezeigten Text anpasst.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
labelSpracheNew :: (SpracheGuiReader r m, MonadIO m)
                => Maybe TVarSprachewechselAktionen
                -> (Sprache -> Text)
                -> m Gtk.Label
labelSpracheNew maybeTVar text = do
    label <- Gtk.labelNew (Nothing :: Maybe Text)
    verwendeSpracheGui maybeTVar $ \sprache -> Gtk.setLabelLabel label $ text sprache
    pure label

-- * Namen
-- | Widget zur Anzeige eines Namen.
newtype NameWidget = NameWidget Gtk.Label
    deriving (Eq)

instance MitWidget NameWidget where
    erhalteWidget :: (MonadIO m) => NameWidget -> m Gtk.Widget
    erhalteWidget (NameWidget w) = erhalteWidget w

instance MitLabel NameWidget where
    erhalteLabel :: (MonadIO m) => NameWidget -> m Gtk.Label
    erhalteLabel (NameWidget w) = pure w

-- | Name anzeigen.
namePackNew :: (MonadIO m, MitBox b, StreckenObjekt s) => b -> s -> m NameWidget
namePackNew box objekt = liftIO $ do
    label <- boxPackWidgetNewDefault box $ Gtk.labelNew $ Just $ erhalteName objekt
    Gtk.setWidgetMarginRight label 5
    pure $ NameWidget label

-- | Widget zur Eingabe eines Namen.
newtype NameAuswahlWidget = NameAuswahlWidget Gtk.Entry
    deriving (Eq)

instance MitWidget NameAuswahlWidget where
    erhalteWidget :: (MonadIO m) => NameAuswahlWidget -> m Gtk.Widget
    erhalteWidget (NameAuswahlWidget w) = erhalteWidget w

instance MitEntry NameAuswahlWidget where
    erhalteEntry :: (MonadIO m) => NameAuswahlWidget -> m Gtk.Entry
    erhalteEntry (NameAuswahlWidget w) = pure w

-- | Name abfragen.
nameAuswahlPackNew :: (SpracheGuiReader r m, MonadIO m, MitBox b)
                   => b
                   -> Maybe TVarSprachewechselAktionen
                   -> m NameAuswahlWidget
nameAuswahlPackNew box maybeTVar = do
    hBox <- liftIO $ boxPackWidgetNewDefault box $ Gtk.boxNew Gtk.OrientationHorizontal 0
    boxPackWidgetNewDefault hBox $ labelSpracheNew maybeTVar $ Language.name <:> Text.empty
    entry <- liftIO $ boxPackWidgetNew hBox PackGrow paddingDefault positionDefault Gtk.entryNew
    verwendeSpracheGui maybeTVar
        $ \sprache -> Gtk.setEntryPlaceholderText entry $ Language.name sprache
    pure $ NameAuswahlWidget entry

-- | Erhalte den aktuell gewählten Namen.
aktuellerName :: (MonadIO m) => NameAuswahlWidget -> m Text
aktuellerName nameAuswahlWidget = liftIO $ do
    entry <- erhalteEntry nameAuswahlWidget
    Gtk.getEntryText entry

-- | Setze den aktuellen Namen.
setzeName :: (MonadIO m) => NameAuswahlWidget -> Text -> m ()
setzeName nameAuswahlWidget name = do
    entry <- erhalteEntry nameAuswahlWidget
    Gtk.setEntryText entry name
#endif
--
