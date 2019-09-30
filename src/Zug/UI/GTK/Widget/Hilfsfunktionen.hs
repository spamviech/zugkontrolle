{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}

{-|
Description: Allgemeine Hilfsfunktionen
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Widget.Hilfsfunktionen () where
#else
module Zug.UI.Gtk.Widget.Hilfsfunktionen (
    -- * Widget
    widgetShowNew, widgetShowIf,
    -- * Container
    containerAddWidgetNew, containerRemoveJust,
    -- * Box
    boxPack, boxPackDefault, boxPackWidgetNew, boxPackWidgetNewDefault,
    Packing(..), packingDefault, Padding(..), paddingDefault, Position(..), positionDefault,
    -- * Notebook
    notebookAppendPageNew, dialogEval, ResponseId,
    -- * Button
    buttonNewWithEvent, buttonNewWithEventLabel, buttonNewWithEventMnemonic,
    -- * Name / Fließend-Value
    nameEntryPackNew, nameLabelPackNew, labelFließendValuePackNew) where

import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Graphics.UI.Gtk (Packing(..), ResponseId)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (StreckenObjekt(..), StreckenAtom(..))
import Zug.Language ((<:>), showText)
import qualified Zug.Language as Language
import Zug.UI.Gtk.Widget.Klassen

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
boxPack box widget packing padding position = liftIO $ boxPackPosition position box widget packing $ fromPadding padding
    where
        boxPackPosition :: (MitBox b, MitWidget w) => Position -> b -> w -> Packing -> Int -> IO ()
        boxPackPosition Start   = mitBoxPackStart
        boxPackPosition End     = mitBoxPackEnd

-- | Neu erstelltes Widget in eine Box packen
boxPackWidgetNew :: (MonadIO m, MitBox b, MitWidget w) => b -> Packing -> Padding -> Position -> m w -> m w
boxPackWidgetNew box packing padding start konstruktor = do
    widget <- widgetShowNew konstruktor
    boxPack box widget packing padding start
    pure widget

-- | Normale Packing-Einstellung
packingDefault :: Packing
packingDefault = PackNatural

-- | Abstand zwischen 'Widget's
newtype Padding = Padding {fromPadding :: Int}
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

-- | Standart-Abstand zwischen 'Widget's in einer 'Box'
paddingDefault :: Padding
paddingDefault = 0

-- | Position zum Hinzufügen zu einer 'Box'
data Position = Start | End
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

-- | Neu erstelltes 'MitWidget' zu einem 'MitNotebook' hinzufügen
notebookAppendPageNew :: (MonadIO m, MitNotebook n, MitWidget w) => n -> Text -> m w -> m (w, Int)
notebookAppendPageNew notebook name konstruktor = do
    widget <- widgetShowNew konstruktor
    page <- mitNotebookAppendPage notebook widget name
    pure (widget, page)

-- | Entferne ein vielleicht vorhandenes 'MitWidget' aus einem 'MitContainer'
containerRemoveJust :: (MonadIO m, MitContainer c, MitWidget w) => c -> Maybe w -> m ()
containerRemoveJust _container  Nothing     = pure ()
containerRemoveJust container   (Just w)    = mitContainerRemove container w

-- | Zeige widget, falls eine Bedingung erfüllt ist
widgetShowIf :: (MonadIO m, MitWidget w) => Bool -> w -> m ()
widgetShowIf    True    = mitWidgetShow
widgetShowIf    False   = mitWidgetHide

-- | 'MitDialog' anzeigen, auswerten und wieder verstecken
dialogEval :: (MonadIO m, MitDialog d) => d -> m ResponseId
dialogEval dialog = liftIO $ do
    mitWidgetShow dialog
    antwort <- mitDialog Gtk.dialogRun dialog
    mitWidgetHide dialog
    pure antwort

-- ** Knöpfe mit einer Funktion
-- | Knopf mit Funktion erstellen
buttonNewWithEvent :: (MonadIO m, MitButton b) => m b -> IO () -> m b
buttonNewWithEvent konstruktor action = do
    button <- widgetShowNew konstruktor
    liftIO $ Gtk.on (erhalteButton button) Gtk.buttonActivated action
    pure button

-- | Knopf mit Mnemonic-Label und Funktion erstellen
buttonNewWithEventMnemonic :: (MonadIO m) => Text -> IO () -> m Gtk.Button
buttonNewWithEventMnemonic label = liftIO . (buttonNewWithEvent $ Gtk.buttonNewWithMnemonic $ Language.addMnemonic label)

-- | Knopf mit Label und Funktion erstellen
buttonNewWithEventLabel :: (MonadIO m) => Text -> IO () -> m Gtk.Button
buttonNewWithEventLabel label = liftIO . (buttonNewWithEvent $ Gtk.buttonNewWithLabel label)

-- ** Namen
-- | Name abfragen
nameEntryPackNew :: (MonadIO m, MitBox b) => b -> m Gtk.Entry
nameEntryPackNew box = liftIO $ do
    hBox <- boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
    boxPackWidgetNewDefault hBox $ Gtk.labelNew $ Just $ (Language.name <:> "" :: Text)
    entry <- boxPackWidgetNewDefault hBox Gtk.entryNew
    Gtk.set entry [Gtk.entryPlaceholderText := Just (Language.name :: Text)]
    pure entry

-- | Name anzeigen
nameLabelPackNew :: (MonadIO m, MitBox b, StreckenObjekt s) => b -> s -> m Gtk.Label
nameLabelPackNew box objekt = liftIO $ do
    label <- boxPackWidgetNewDefault box $ Gtk.labelNew $ Just $ erhalteName objekt
    Gtk.set label [Gtk.widgetMarginRight := 5]
    pure label

-- * Fließend-Value
-- | Füge neues 'Label' zu 'Box' hinzu, in dem der 'Value' eines 'StreckenAtom's angezeigt wird, bei dem Strom fließt.
labelFließendValuePackNew :: (MonadIO m, StreckenAtom s, MitBox b) => b -> s -> m Gtk.Label
labelFließendValuePackNew box s
    = liftIO $ boxPackWidgetNew
        box
        packingDefault
        3
        positionDefault
            $ Gtk.labelNew $ Just $
                (Language.fließendValue <:> showText (fließend s) :: Text)
#endif