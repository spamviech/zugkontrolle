{-# LANGUAGE CPP #-}

{-|
Description: Allgemeine Hilfsfunktionen
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.GTK.Widget.Hilfsfunktionen () where
#else
module Zug.UI.GTK.Widget.Hilfsfunktionen (
    widgetShowNew, containerAddWidgetNew, containerRemoveJust, widgetShowIf,
    boxPack, boxPackDefault, boxPackWidgetNew, boxPackWidgetNewDefault,
    Packing(..), packingDefault, Padding(..), paddingDefault, Position(..), positionDefault,
    notebookAppendPageNew, dialogEval, ResponseId) where

import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Graphics.UI.Gtk (Packing(..), ResponseId)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language
import Zug.UI.GTK.Widget.Klassen

-- | 'Widget' erstellen und anzeigen
widgetShowNew :: (MitWidget w) => IO w -> IO w
widgetShowNew konstruktor = do
    widget <- konstruktor
    mitWidgetShow widget
    pure widget

-- | Neu erstelltes 'Widget' zu 'Container' hinzufügen
containerAddWidgetNew :: (MitContainer c, MitWidget w) => c -> IO w -> IO w
containerAddWidgetNew container konstruktor = do
    widget <- widgetShowNew konstruktor
    mitContainerAdd container widget
    pure widget

-- | 'Widget' in eine 'Box' packen
boxPack :: (MitBox b, MitWidget w) => b -> w -> Packing -> Padding -> Position -> IO ()
boxPack box widget packing padding position = boxPackPosition position box widget packing $ fromPadding padding
    where
        boxPackPosition :: (MitBox b, MitWidget w) => Position -> b -> w -> Packing -> Int -> IO ()
        boxPackPosition Start   = mitBoxPackStart
        boxPackPosition End     = mitBoxPackEnd

-- | Neu erstelltes Widget in eine Box packen
boxPackWidgetNew :: (MitBox b, MitWidget w) => b -> Packing -> Padding -> Position -> IO w -> IO w
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
boxPackWidgetNewDefault :: (MitBox b, MitWidget w) => b -> IO w -> IO w
boxPackWidgetNewDefault box = boxPackWidgetNew box packingDefault paddingDefault positionDefault

-- | Widget mit Standard Packing, Padding und Positionierung in eine Box packen
boxPackDefault :: (MitBox b, MitWidget w) => b -> w -> IO ()
boxPackDefault box widget = boxPack box widget packingDefault paddingDefault positionDefault

-- | Neu erstelltes 'MitWidget' zu einem 'MitNotebook' hinzufügen
notebookAppendPageNew :: (MitNotebook n, MitWidget w) => n -> Text -> IO w -> IO w
notebookAppendPageNew notebook name konstruktor = do
    widget <- widgetShowNew konstruktor
    mitNotebookAppendPage notebook widget name
    pure widget

-- | Entferne ein vielleicht vorhandenes 'MitWidget' aus einem 'MitContainer'
containerRemoveJust :: (MitContainer c, MitWidget w) => c -> Maybe w -> IO ()
containerRemoveJust _container  Nothing     = pure ()
containerRemoveJust container   (Just w)    = mitContainerRemove container w

-- | Zeige widget, falls eine Bedingung erfüllt ist
widgetShowIf :: (MitWidget w) => Bool -> w -> IO ()
widgetShowIf    True    = mitWidgetShow
widgetShowIf    False   = mitWidgetHide

-- | 'MitDialog' anzeigen, auswerten und wieder verstecken
dialogEval :: (MonadIO m, MitDialog d) => d -> m ResponseId
dialogEval dialog = liftIO $ do
    mitWidgetShow dialog
    antwort <- mitDialog dialogRun dialog
    mitWidgetHide dialog
    pure antwort

-- ** Knöpfe mit einer Funktion
-- | Knopf mit Funktion erstellen
buttonNewWithEvent :: IO Button -> IO () -> IO Button
buttonNewWithEvent konstruktor action = do
    button <- widgetShowNew konstruktor
    on button buttonActivated action
    pure button

-- | Knopf mit Mnemonic-Label und Funktion erstellen
buttonNewWithEventMnemonic :: Text -> IO () -> IO Button
buttonNewWithEventMnemonic label = buttonNewWithEvent $ buttonNewWithMnemonic $ addMnemonic label

-- | Knopf mit Label und Funktion erstellen
buttonNewWithEventLabel :: Text -> IO () -> IO Button
buttonNewWithEventLabel label = buttonNewWithEvent $ buttonNewWithLabel label

-- ** Darstellung von Anschlüssen
-- | 'Label' für 'Anschluss' erstellen
anschlussLabelNew :: Text -> Anschluss -> IO Label
anschlussLabelNew name anschluss = labelNew $ Just $ name <-> Language.anschluss <:> showText anschluss

-- | 'SpinBox' zur Pin-Abfrage erstellen
pinSpinBoxNew :: Text -> IO (HBox, SpinButton)
pinSpinBoxNew name = do
    hBox <- hBoxNew False 0
    boxPackWidgetNewDefault hBox $ labelNew $ Just $ name <-> Language.pin <:> ""
    spinButton <- boxPackWidgetNewDefault hBox $ spinButtonNewWithRange 0 27 1
    pure (hBox, spinButton)

anschlussAuswahlNew :: Text -> IO AnschlussAuswahlWidget
anschlussAuswahlNew name = do
    _

-- ** Namen
-- | Name abfragen
nameEntryPackNew :: (MitBox b) => b -> IO Entry
nameEntryPackNew box = do
    hBox <- boxPackWidgetNewDefault box $ hBoxNew False 0
    boxPackWidgetNewDefault hBox $ labelNew $ Just $ (Language.name <:> "" :: Text)
    entry <- boxPackWidgetNewDefault hBox entryNew
    set entry [entryPlaceholderText := Just (Language.name :: Text)]
    pure entry

-- | Name anzeigen
nameLabelPackNew :: (MitBox b, StreckenObjekt s) => b -> s -> IO Label
nameLabelPackNew box objekt = do
    label <- boxPackWidgetNewDefault box $ labelNew $ Just $ erhalteName objekt
    set label [widgetMarginRight := 5]
    pure label

-- ** Scrollbare Widgets erstellen
-- | Erstelle neues ScrolledWindow mit automatisch erstelltem Viewport
scrolledWidgetNew :: (MitWidget w) => IO w -> IO (ScrolledWindow, w)
scrolledWidgetNew konstruktor = do
    widget <- widgetShowNew konstruktor
    scrolledWindow <- widgetShowNew $ scrolledWindowNew Nothing Nothing
    set scrolledWindow [scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyAlways]
    scrolledWindowAddWithViewport scrolledWindow widget
    pure (scrolledWindow, widget)

-- | Erstelle neues 'ScrolledWindow' mit automatisch erstelltem Viewport und packe sie in eine 'Box'
scrolledWidgetPackNew :: (MitBox b, MitWidget w) => b -> IO w -> IO (ScrolledWindow, w)
scrolledWidgetPackNew box konstruktor = do
    (scrolledWindow, widget) <- scrolledWidgetNew konstruktor
    boxPackWidgetNew box PackGrow paddingDefault positionDefault $ pure scrolledWindow
    pure (scrolledWindow, widget)

-- | Erstelle neues 'ScrolledWindow' mit automatisch erstelltem Viewport und füge sie zu 'Container' hinzu
scrolledWidgetAddNew :: (MitContainer c, MitWidget w) => c -> IO w -> IO (ScrolledWindow, w)
scrolledWidgetAddNew container konstruktor = do
    (scrolledWindow, widget) <- scrolledWidgetNew konstruktor
    containerAddWidgetNew container $ pure scrolledWindow
    pure (scrolledWindow, widget)

-- | Seite mit scrollbarer VBox einem Notebook hinzufügen
scrolledWidgedNotebookAppendPageNew :: (MitNotebook n, MitWidget w) => n -> Text -> IO w -> IO (ScrolledWindow, w)
scrolledWidgedNotebookAppendPageNew notebook name konstruktor = do
    widget <- widgetShowNew konstruktor
    scrolledWindow <- notebookAppendPageNew notebook name $ widgetShowNew $ scrolledWindowNew Nothing Nothing
    set scrolledWindow [scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyAlways]
    scrolledWindowAddWithViewport scrolledWindow widget
    pure (scrolledWindow, widget)

-- ** Widget mit Name und CheckButton erstellen
-- | Füge einen 'RCheckButton' mit einem 'Label' für den Namen zur Box hinzu.
hinzufügenWidgetWegstreckeNew :: (StreckenObjekt o, MitBox b) => o -> b -> FortfahrenWennToggled TMVar StatusGui -> IO (HBox, RCheckButton)
hinzufügenWidgetWegstreckeNew objekt box fortfahrenWennToggled = do
    hBoxHinzufügen <- boxPackWidgetNewDefault box $ hBoxNew False 0
    checkButton <- boxPackWidgetNewDefault hBoxHinzufügen checkButtonNew
    boxPackWidgetNewDefault hBoxHinzufügen $ labelNew $ Just $ erhalteName objekt
    registrierterCheckButton <- registrieren checkButton fortfahrenWennToggled traversalHinzufügenWegstrecke
    pure (hBoxHinzufügen, registrierterCheckButton)

-- | Füge einen Knopf mit dem Namen zur Box hinzu. Beim drücken wird die 'TMVar' mit dem Objekt gefüllt.
hinzufügenWidgetPlanNew :: (MitBox b) => b -> Objekt -> TMVar (Maybe Objekt) -> IO Button
hinzufügenWidgetPlanNew box objekt tmvar = boxPackWidgetNewDefault box $ buttonNewWithEventLabel (erhalteName objekt) $
    atomically $ putTMVar tmvar $ Just objekt

-- | Füge neues 'Label' zu 'Box' hinzu, in dem der 'Value' eines 'StreckenAtom's angezeigt wird, bei dem Strom fließt.
labelFließendValuePackNew :: (StreckenAtom s, MitBox b) => b -> s -> IO Label
labelFließendValuePackNew box s = boxPackWidgetNew box packingDefault 3 positionDefault $ labelNew $ Just $
    (Language.fließendValue <:> showText (fließend s) :: Text)
#endif