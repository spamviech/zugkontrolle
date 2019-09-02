{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

{-|
Description : Erstelle zusammengesetzte Widgets.

Allgemeine Hilfsfunktionen zum erstellen neuer Widgets
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.GTK.Widget () where
#else
module Zug.UI.GTK.Widget (
    -- * Allgemeine Widget-Funktionen
    widgetShowNew, containerAddWidgetNew, boxPackWidgetNew, notebookAppendPageNew, containerRemoveJust, widgetShowIf,
    boxPack, boxPackDefault, boxPackWidgetNewDefault, packingDefault, paddingDefault, positionDefault, dialogEval,
    -- *`* Scrollbare Widgets
    scrolledWidgetNew, scrolledWidgetPackNew, scrolledWidgetAddNew, scrolledWidgedNotebookAppendPageNew,
    -- ** Knopf erstellen
    buttonNewWithEvent, buttonNewWithEventLabel, buttonNewWithEventMnemonic,
    -- ** Pin darstellen
    pinLabelNew, pinSpinBoxNew, nameEntryPackNew,
    -- ** Spezifisches StreckenObjekt darstellen
    bahngeschwindigkeitPackNew, streckenabschnittPackNew, weichePackNew, kupplungPackNew, wegstreckePackNew, planPackNew,
    BahngeschwindigkeitWidget, StreckenabschnittWidget, WeicheWidget, KupplungWidget, WegstreckeWidget, PlanWidget,
    BahngeschwindigkeitWidgetHinzufügenWegstrecke, StreckenabschnittWidgetHinzufügenWegstrecke, WeicheWidgetHinzufügenWegstrecke, KupplungWidgetHinzufügenWegstrecke,
    BahngeschwindigkeitWidgetHinzufügenPlan, StreckenabschnittWidgetHinzufügenPlan, WeicheWidgetHinzufügenPlan, KupplungWidgetHinzufügenPlan, WegstreckeWidgetHinzufügenPlan,
    -- * Verwaltung des aktuellen Zustands
    DynamischeWidgets(..), StatusGUI, ObjektGUI, BefehlGUI, IOStatusGUI, MStatusGUI, MonadMStatusGUI, BGWidgets(..), STWidgets(..), WEWidgets(..), KUWidgets(..), WSWidgets(..), PLWidgets(..),
    traversalHinzufügenWegstrecke, WegstreckenElement(..), getterRichtungsRadioButtons, PlanElement(..), entferneHinzufügenPlanWidgets) where

-- Bibliotheken
import Control.Applicative (ZipList(..))
import Control.Concurrent.STM (atomically, TMVar, putTMVar, TVar)
import Control.Lens (Traversal', Lens', Getter, Fold, (%%~), (^.), (^..), Field1(..), Field2(..), Field3(..))
import qualified Control.Lens as Lens
import Control.Monad (void, unless)
import Control.Monad.State (State, StateT)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as Aeson
import Data.Foldable (Foldable(..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Graphics.UI.Gtk (VBox, HBox, ProgressBar, Window, Button, WidgetClass, ContainerClass, BoxClass,
                        NotebookClass, DialogClass, ResponseId, Label, SpinButton, Entry, ScrolledWindow,
                        HScale, RangeClass, ToggleButton, RadioButton, Frame,
                        MessageType(..), ButtonsType(..), PolicyType(..), Packing(..),
                        widgetShow, widgetHide, containerAdd, containerRemove, boxPackStart, boxPackEnd,
                        notebookAppendPage, set, get, AttrOp(..), widgetVisible, dialogRun, on, buttonActivated,
                        buttonNewWithLabel, buttonNewWithMnemonic, labelNew, hBoxNew, spinButtonNewWithRange,
                        entryNew, entryPlaceholderText, widgetMarginRight,
                        scrolledWindowNew, scrolledWindowHscrollbarPolicy, scrolledWindowVscrollbarPolicy,
                        scrolledWindowAddWithViewport, checkButtonNew, hScaleNewWithRange, valueChanged,
                        rangeValue, toggleButtonNewWithLabel, toggled, toggleButtonActive, progressBarFraction,
                        radioButtonNewWithLabel, radioButtonNewWithLabelFromWidget, frameNew, vBoxNew,
                        expanderNew, vBoxNew, hButtonBoxNew, messageDialogNew, windowTitle, messageDialogText)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (StreckenObjekt(..), StreckenAtom(..), Pin(), PwmMapIO,
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..),
                    Streckenabschnitt(..), StreckenabschnittKlasse(..),
                    Weiche(..), WeicheKlasse(..),
                    Kupplung(..), KupplungKlasse(..),
                    Wegstrecke(..), WegstreckeKlasse(..))
import Zug.Klassen (Zugtyp(..), Fahrtrichtung(..), Strom(..), Richtung(..))
import qualified Zug.Language as Language
import Zug.Language ((<^>), (<->), (<:>), (<°>), addMnemonic, showText)
import Zug.Menge (Menge, ausFoldable)
import Zug.Plan (PlanKlasse(..), Plan(..), ObjektAllgemein(..), Objekt, Ausführend(),
                AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionWeiche(..),
                AktionKupplung(..), AktionWegstrecke(..))
import Zug.UI.Base (StatusAllgemein(..), AusführenMöglich(..), bahngeschwindigkeiten, streckenabschnitte,
                    weichen, kupplungen, wegstrecken, pläne, tvarAusführend, tvarPwmMap, tvarI2CMap,
                    auswertenTMVarIOStatus, ausführenMöglich, entfernenBahngeschwindigkeit,
                    entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung,
                    entfernenWegstrecke, entfernenPlan)
import Zug.UI.Befehl (BefehlAllgemein(..), ausführenTMVarBefehl, ausführenTMVarAktion)
import Zug.UI.GTK.FortfahrenWennToggled (FortfahrenWennToggled, RCheckButton, registrieren)

-- * Sammel-Typ um dynamische Widgets zu speichern
-- | Sammel-Typ spezialiert auf GUI-Typen
type ObjektGUI = ObjektAllgemein BGWidgets STWidgets WEWidgets KUWidgets WSWidgets PLWidgets
-- | Befehl spezialiert auf GUI-Typen
type BefehlGUI = BefehlAllgemein ObjektGUI
-- | Zustands-Typ der Zustands-Monade spezialisiert auf GUI-Typen
type StatusGUI = StatusAllgemein ObjektGUI
-- | Zustands-Monaden-Transformer spezialisiert auf GUI-Typen in der IO-Monade
type IOStatusGUI = StateT StatusGUI IO
-- | Reine Zustands-Monade spezialiert auf GUI-Typen
type MStatusGUI = State StatusGUI
-- | Zustands-Monaden-Transformer spezialiert auf GUI-Typen
type MonadMStatusGUI m a = StateT StatusGUI m a

-- | Sammlung aller Widgets, welche während der Laufzeit benötigt werden.
data DynamischeWidgets = DynamischeWidgets {
    vBoxBahngeschwindigkeiten :: VBox,
    vBoxStreckenabschnitte :: VBox,
    vBoxWeichen :: VBox,
    vBoxKupplungen :: VBox,
    vBoxWegstrecken :: VBox,
    vBoxPläne :: VBox,
    vBoxHinzufügenWegstreckeBahngeschwindigkeiten :: VBox,
    vBoxHinzufügenPlanBahngeschwindigkeiten :: VBox,
    vBoxHinzufügenPlanBahngeschwindigkeitenLego :: VBox,
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin :: VBox,
    vBoxHinzufügenWegstreckeStreckenabschnitte :: VBox,
    vBoxHinzufügenPlanStreckenabschnitte :: VBox,
    vBoxHinzufügenWegstreckeWeichen :: VBox,
    vBoxHinzufügenPlanWeichenGerade :: VBox,
    vBoxHinzufügenPlanWeichenKurve :: VBox,
    vBoxHinzufügenPlanWeichenLinks :: VBox,
    vBoxHinzufügenPlanWeichenRechts :: VBox,
    vBoxHinzufügenWegstreckeKupplungen :: VBox,
    vBoxHinzufügenPlanKupplungen :: VBox,
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit :: VBox,
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego :: VBox,
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin :: VBox,
    vBoxHinzufügenPlanWegstreckenStreckenabschnitt :: VBox,
    vBoxHinzufügenPlanWegstreckenWeiche :: VBox,
    vBoxHinzufügenPlanWegstreckenKupplung :: VBox,
    progressBarPlan :: ProgressBar,
    windowMain :: Window,
    fortfahrenWennToggledWegstrecke :: FortfahrenWennToggled TMVar StatusGUI,
    tmvarPlanObjekt :: TMVar (Maybe Objekt)}

-- | Klasse für GUI-Darstellung von Typen, die zur Erstellung einer 'Wegstrecke' verwendet werden.
class WegstreckenElement s where
    -- | Linse auf 'CheckButton', ob 'StreckenObjekt' zu einer 'Wegstrecke' hinzugefügt werden soll
    lensWegstrecke :: Lens' s RCheckButton
    -- | Entferne 'Widget's zum Hinzufügen zu einer 'Wegstrecke' aus der entsprechenden Box
    entferneHinzufügenWegstreckeWidgets :: s -> DynamischeWidgets -> IO ()

-- | Klasse für GUI-Darstellungen von Typen, die zur Erstellung eines 'Plan's verwendet werden.
class PlanElement s where
    -- | Faltung auf 'Button's (falls vorhanden), welches 'StreckenObjekt' für eine 'Aktion' verwendet werden soll
    foldPlan :: Fold s (Maybe Button)
    -- | 'ZipList' aller 'VBox'en, in denen Widgets angezeigt werden. Die Reihenfolge passt zum Ergebnis von 'foldPlan'. Wird für 'entferneHinzufügenPlanWidgets' benötigt.
    vBoxenHinzufügenPlan :: s -> DynamischeWidgets -> ZipList VBox

-- | Entferne 'Widget's zum 'Plan' erstellen aus den entsprechenden 'Box'en.
entferneHinzufügenPlanWidgets :: (PlanElement p) => p -> DynamischeWidgets -> IO ()
entferneHinzufügenPlanWidgets p dynamischeWidgets = sequence_ $ containerRemoveJust <$> vBoxenHinzufügenPlan p dynamischeWidgets <*> ZipList (p ^.. foldPlan)

-- type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s
-- | 'Traversal'' über alle 'CheckButton's zum Hinzufügen einer 'Wegstrecke'
traversalHinzufügenWegstrecke :: Traversal' StatusGUI RCheckButton
traversalHinzufügenWegstrecke f status = Status <$>
    traverseList f (status ^. bahngeschwindigkeiten) <*>
    traverseList f (status ^. streckenabschnitte) <*>
    traverseList f (status ^. weichen) <*>
    traverseList f (status ^. kupplungen) <*>
    pure (status ^. wegstrecken) <*>
    pure (status ^. pläne) <*>
    pure (status ^. tvarAusführend) <*>
    pure (status ^. tvarPwmMap) <*>
    pure (status ^. tvarI2CMap)
        where
            traverseList :: (Applicative f, WegstreckenElement s) => (RCheckButton -> f RCheckButton) -> [s] -> f [s]
            traverseList f list = traverse . lensWegstrecke %%~ f $ list

-- * Hilfsfunktionen
-- | 'Widget' erstellen und anzeigen
widgetShowNew :: (WidgetClass w) => IO w -> IO w
widgetShowNew konstruktor = do
    widget <- konstruktor
    widgetShow widget
    pure widget

-- | Neu erstelltes 'Widget' zu 'Container' hinzufügen
containerAddWidgetNew :: (ContainerClass c, WidgetClass w) => c -> IO w -> IO w
containerAddWidgetNew container konstruktor = do
    widget <- widgetShowNew konstruktor
    containerAdd container widget
    pure widget

-- | 'Widget' in eine 'Box' packen
boxPack :: (BoxClass b, WidgetClass w) => b -> w -> Packing -> Int -> Bool -> IO ()
boxPack box widget packing padding start = (if start then boxPackStart else boxPackEnd) box widget packing padding

-- | Neu erstelltes Widget in eine Box packen
boxPackWidgetNew :: (BoxClass b, WidgetClass w) => b -> Packing -> Int -> Bool -> IO w -> IO w
boxPackWidgetNew box packing padding start konstruktor = do
    widget <- widgetShowNew konstruktor
    boxPack box widget packing padding start
    pure widget

-- | Normale Packing-Einstellung
packingDefault :: Packing
packingDefault = PackNatural

-- | Standart-Abstand zwischen Widgets in einer Box
paddingDefault :: Int
paddingDefault = 0

-- | Standart-Position zum Hinzufügen zu einer Box
positionDefault :: Bool
positionDefault = True

-- | Neu erstelltes Widget mit Standard Packing, Padding und Positionierung in eine Box packen
boxPackWidgetNewDefault :: (BoxClass b, WidgetClass w) => b -> IO w -> IO w
boxPackWidgetNewDefault box = boxPackWidgetNew box packingDefault paddingDefault positionDefault

-- | Widget mit Standard Packing, Padding und Positionierung in eine Box packen
boxPackDefault :: (BoxClass b, WidgetClass w) => b -> w -> IO ()
boxPackDefault box widget = boxPack box widget packingDefault paddingDefault positionDefault

-- | Neu erstelltes 'Widget' zu 'Notebook' hinzufügen
notebookAppendPageNew :: (NotebookClass n, WidgetClass w) => n -> Text -> IO w -> IO w
notebookAppendPageNew notebook name konstruktor = do
    widget <- widgetShowNew konstruktor
    notebookAppendPage notebook widget name
    pure widget

-- | Entferne ein vielleicht vorhandenes Widget aus einem Container
containerRemoveJust :: (ContainerClass c, WidgetClass w) => c -> Maybe w -> IO ()
containerRemoveJust _container  (Nothing)   = pure ()
containerRemoveJust container   (Just w)    = containerRemove container w

-- | Zeige widget, falls eine Bedingung erfüllt ist
widgetShowIf :: (WidgetClass w) => Bool -> w -> IO ()
widgetShowIf visible widget = set widget [widgetVisible := visible]

-- | Dialog anzeigen und auswerten
dialogEval :: (DialogClass d) => d -> IO ResponseId
dialogEval dialog = do
    widgetShow dialog
    antwort <- dialogRun dialog
    widgetHide dialog
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

-- | Entfernen-Knopf zu 'Box' hinzufügen. Beim drücken werden /removeActionGUI/ und /removeAction/ ausgeführt.
buttonEntfernenPack :: (BoxClass b) => b -> IO () -> IOStatusGUI () -> TMVar StatusGUI -> IO Button
buttonEntfernenPack box removeActionGUI removeAction tmvarStatus
    = boxPackWidgetNew box PackNatural paddingDefault False $
        buttonNewWithEventLabel Language.entfernen $ auswertenTMVarIOStatus removeAction tmvarStatus >> removeActionGUI

-- | Entfernen-Knopf zu Box hinzufügen. Beim drücken wird /parent/ aus der /box/ entfernt und die 'IOStatusGUI'-Aktion ausgeführt.
buttonEntfernenPackSimple :: (BoxClass b, ContainerClass c) => b -> c -> IOStatusGUI () -> TMVar StatusGUI -> IO Button
buttonEntfernenPackSimple box parent = buttonEntfernenPack box $ containerRemove parent box

-- ** Darstellung von Pins
-- | 'Label' für Pin erstellen
pinLabelNew :: Text -> Pin -> IO Label
pinLabelNew name pin = labelNew $ Just $ name <-> Language.pin <:> showText pin

-- | 'SpinBox' zur Pin-Abfrage erstellen
pinSpinBoxNew :: Text -> IO (HBox, SpinButton)
pinSpinBoxNew name = do
    hBox <- hBoxNew False 0
    boxPackWidgetNewDefault hBox $ labelNew $ Just $ name <-> Language.pin <:> ""
    spinButton <- boxPackWidgetNewDefault hBox $ spinButtonNewWithRange 0 27 1
    pure (hBox, spinButton)

-- ** Namen
-- | Name abfragen
nameEntryPackNew :: (BoxClass b) => b -> IO Entry
nameEntryPackNew box = do
    hBox <- boxPackWidgetNewDefault box $ hBoxNew False 0
    boxPackWidgetNewDefault hBox $ labelNew $ Just $ (Language.name <:> "" :: Text)
    entry <- boxPackWidgetNewDefault hBox entryNew
    set entry [entryPlaceholderText := Just (Language.name :: Text)]
    pure entry

-- | Name anzeigen
nameLabelPackNew :: (BoxClass b, StreckenObjekt s) => b -> s -> IO Label
nameLabelPackNew box objekt = do
    label <- boxPackWidgetNewDefault box $ labelNew $ Just $ erhalteName objekt
    set label [widgetMarginRight := 5]
    pure label

-- ** Scrollbare Widgets erstellen
-- | Erstelle neues ScrolledWindow mit automatisch erstelltem Viewport
scrolledWidgetNew :: (WidgetClass w) => IO w -> IO (ScrolledWindow, w)
scrolledWidgetNew konstruktor = do
    widget <- widgetShowNew konstruktor
    scrolledWindow <- widgetShowNew $ scrolledWindowNew Nothing Nothing
    set scrolledWindow [scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyAlways]
    scrolledWindowAddWithViewport scrolledWindow widget
    pure (scrolledWindow, widget)

-- | Erstelle neues 'ScrolledWindow' mit automatisch erstelltem Viewport und packe sie in eine 'Box'
scrolledWidgetPackNew :: (BoxClass b, WidgetClass w) => b -> IO w -> IO (ScrolledWindow, w)
scrolledWidgetPackNew box konstruktor = do
    (scrolledWindow, widget) <- scrolledWidgetNew konstruktor
    boxPackWidgetNew box PackGrow paddingDefault positionDefault $ pure scrolledWindow
    pure (scrolledWindow, widget)

-- | Erstelle neues 'ScrolledWindow' mit automatisch erstelltem Viewport und füge sie zu 'Container' hinzu
scrolledWidgetAddNew :: (ContainerClass c, WidgetClass w) => c -> IO w -> IO (ScrolledWindow, w)
scrolledWidgetAddNew container konstruktor = do
    (scrolledWindow, widget) <- scrolledWidgetNew konstruktor
    containerAddWidgetNew container $ pure scrolledWindow
    pure (scrolledWindow, widget)

-- | Seite mit scrollbarer VBox einem Notebook hinzufügen
scrolledWidgedNotebookAppendPageNew :: (NotebookClass n, WidgetClass w) => n -> Text -> IO w -> IO (ScrolledWindow, w)
scrolledWidgedNotebookAppendPageNew notebook name konstruktor = do
    widget <- widgetShowNew konstruktor
    scrolledWindow <- notebookAppendPageNew notebook name $ widgetShowNew $ scrolledWindowNew Nothing Nothing
    set scrolledWindow [scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyAlways]
    scrolledWindowAddWithViewport scrolledWindow widget
    pure (scrolledWindow, widget)

-- ** Widget mit Name und CheckButton erstellen
-- | Füge einen 'RCheckButton' mit einem 'Label' für den Namen zur Box hinzu.
hinzufügenWidgetWegstreckeNew :: (StreckenObjekt o, BoxClass b) => o -> b -> FortfahrenWennToggled TMVar StatusGUI -> IO (HBox, RCheckButton)
hinzufügenWidgetWegstreckeNew objekt box fortfahrenWennToggled = do
    hBoxHinzufügen <- boxPackWidgetNewDefault box $ hBoxNew False 0
    checkButton <- boxPackWidgetNewDefault hBoxHinzufügen checkButtonNew
    boxPackWidgetNewDefault hBoxHinzufügen $ labelNew $ Just $ erhalteName objekt
    registrierterCheckButton <- registrieren checkButton fortfahrenWennToggled traversalHinzufügenWegstrecke
    pure (hBoxHinzufügen, registrierterCheckButton)

-- | Füge einen Knopf mit dem Namen zur Box hinzu. Beim drücken wird die 'LikeMVar' mit dem Objekt gefüllt.
hinzufügenWidgetPlanNew :: (BoxClass b) => b -> Objekt -> TMVar (Maybe Objekt) -> IO Button
hinzufügenWidgetPlanNew box objekt tmvar = boxPackWidgetNewDefault box $ buttonNewWithEventLabel (erhalteName objekt) $
    atomically $ putTMVar tmvar $ Just objekt

-- | Füge neues 'Label' zu 'Box' hinzu, in dem der 'Value' eines 'StreckenAtom's angezeigt wird, bei dem Strom fließt.
labelFließendValuePackNew :: (StreckenAtom s, BoxClass b) => b -> s -> IO Label
labelFließendValuePackNew box s = boxPackWidgetNew box packingDefault 3 positionDefault $ labelNew $ Just $
    (Language.fließendValue <:> showText (fließend s) :: Text)

-- * Darstellung von Streckenobjekten
-- | 'Bahngeschwindigkeit' darstellen
bahngeschwindigkeitPackNew :: Bahngeschwindigkeit -> TMVar StatusGUI -> DynamischeWidgets -> IO BahngeschwindigkeitWidget
bahngeschwindigkeitPackNew bahngeschwindigkeit tmvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxBahngeschwindigkeiten, vBoxHinzufügenWegstreckeBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeitenLego, vBoxHinzufügenPlanBahngeschwindigkeitenMärklin, fortfahrenWennToggledWegstrecke, tmvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckeNew bahngeschwindigkeit vBoxHinzufügenWegstreckeBahngeschwindigkeiten fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew vBoxHinzufügenPlanBahngeschwindigkeiten (OBahngeschwindigkeit bahngeschwindigkeit) tmvarPlanObjekt
    hinzufügenPlanWidgetZT <- case zugtyp bahngeschwindigkeit of
        (Lego)          -> hinzufügenWidgetPlanNew vBoxHinzufügenPlanBahngeschwindigkeitenLego (OBahngeschwindigkeit bahngeschwindigkeit) tmvarPlanObjekt >>= pure . Left
        (Märklin)       -> hinzufügenWidgetPlanNew vBoxHinzufügenPlanBahngeschwindigkeitenMärklin (OBahngeschwindigkeit bahngeschwindigkeit) tmvarPlanObjekt >>= pure . Right
        (Undefiniert)   -> error "Bahngeschwindigkeit mit undefiniertem Zugtyp erstellt."
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxBahngeschwindigkeiten $ hBoxNew False 0
    nameLabelPackNew hBox bahngeschwindigkeit
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.geschwindigkeit $ getGeschwindigkeitsPin bahngeschwindigkeit
    hScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew hBox bahngeschwindigkeit tmvarStatus
    fahrtrichtungsPinLabelPackNew hBox bahngeschwindigkeit
    buttonUmdrehenPackNew hBox bahngeschwindigkeit hScaleGeschwindigkeit tmvarStatus
    labelFließendValuePackNew hBox bahngeschwindigkeit
    let bgWidgets = BGWidgets {bg=bahngeschwindigkeit, bgWidget=hBox, bgHinzPL=(hinzufügenPlanWidget, hinzufügenPlanWidgetZT), bgHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxBahngeschwindigkeiten (entfernenBahngeschwindigkeit bgWidgets >> liftIO (entferneHinzufügenWegstreckeWidgets bgWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets bgWidgets dynamischeWidgets)) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OBahngeschwindigkeit bgWidgets) tmvarStatus
    pure hBox
        where
            getGeschwindigkeitsPin :: Bahngeschwindigkeit -> Pin
            getGeschwindigkeitsPin (LegoBahngeschwindigkeit {geschwindigkeitsPin})      = geschwindigkeitsPin
            getGeschwindigkeitsPin (MärklinBahngeschwindigkeit {geschwindigkeitsPin})   = geschwindigkeitsPin
            fahrtrichtungsPinLabelPackNew :: (BoxClass b) => b -> Bahngeschwindigkeit -> IO ()
            fahrtrichtungsPinLabelPackNew box   (LegoBahngeschwindigkeit {fahrtrichtungsPin})   = void $ boxPackWidgetNewDefault box (pinLabelNew Language.fahrtrichtung fahrtrichtungsPin)
            fahrtrichtungsPinLabelPackNew _box  (MärklinBahngeschwindigkeit {})                 = pure ()
-- | Äußerstes Widget zur Darstellung einer 'Bahngeschwindigkeit'
type BahngeschwindigkeitWidget = HBox
-- | Widgets zum Hinzufügen einer 'Bahngeschwindigkeit' zu einer 'Wegstrecke'
type BahngeschwindigkeitWidgetHinzufügenWegstrecke = (HBox, RCheckButton)
-- | Widgets zum Hinzufügen einer 'Bahngeschwindigkeit' zu einem 'Plan'
type BahngeschwindigkeitWidgetHinzufügenPlan = (Button, Either Button Button)
-- | 'Bahngeschwindigkeit' mit zugehörigen Widgets
data BGWidgets = BGWidgets {
                    bg :: Bahngeschwindigkeit,
                    bgWidget :: BahngeschwindigkeitWidget,
                    bgHinzPL :: BahngeschwindigkeitWidgetHinzufügenPlan,
                    bgHinzWS :: BahngeschwindigkeitWidgetHinzufügenWegstrecke}
                        deriving (Eq)

instance WegstreckenElement BGWidgets where
    lensWegstrecke :: Lens' BGWidgets RCheckButton
    lensWegstrecke = (Lens.lens bgHinzWS (\bg v -> bg {bgHinzWS=v})) . _2
    entferneHinzufügenWegstreckeWidgets :: BGWidgets -> DynamischeWidgets -> IO ()
    entferneHinzufügenWegstreckeWidgets (BGWidgets {bgHinzWS}) (DynamischeWidgets {vBoxHinzufügenWegstreckeBahngeschwindigkeiten})
        = containerRemove vBoxHinzufügenWegstreckeBahngeschwindigkeiten $ bgHinzWS ^. _1

instance PlanElement BGWidgets where
    foldPlan :: Fold BGWidgets (Maybe Button)
    foldPlan = Lens.folding $ (\(b, bZT) -> (Just b) : eitherToMaybeList bZT) . bgHinzPL
        where
            eitherToMaybeList :: Either a a -> [Maybe a]
            eitherToMaybeList (Left bLego)      = [Just bLego, Nothing]
            eitherToMaybeList (Right bMärklin)  = [Nothing, Just bMärklin]
    vBoxenHinzufügenPlan :: BGWidgets -> DynamischeWidgets -> ZipList VBox
    vBoxenHinzufügenPlan _bgWidgets (DynamischeWidgets {vBoxHinzufügenPlanBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeitenLego, vBoxHinzufügenPlanBahngeschwindigkeitenMärklin})
        = ZipList [vBoxHinzufügenPlanBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeitenLego, vBoxHinzufügenPlanBahngeschwindigkeitenMärklin]

instance StreckenObjekt BGWidgets where
    zugtyp :: BGWidgets -> Zugtyp
    zugtyp (BGWidgets {bg}) = zugtyp bg
    pins :: BGWidgets -> [Pin]
    pins (BGWidgets {bg}) = pins bg
    erhalteName :: BGWidgets -> Text
    erhalteName (BGWidgets {bg}) = erhalteName bg

instance Aeson.ToJSON BGWidgets where
    toJSON :: BGWidgets -> Aeson.Value
    toJSON (BGWidgets {bg}) = Aeson.toJSON bg

instance BahngeschwindigkeitKlasse BGWidgets where
    geschwindigkeit :: BGWidgets -> Natural -> PwmMapIO ()
    geschwindigkeit (BGWidgets {bg}) = geschwindigkeit bg
    umdrehen :: BGWidgets -> Maybe Fahrtrichtung -> PwmMapIO ()
    umdrehen (BGWidgets {bg}) = umdrehen bg

-- | Füge 'Scale' zum einstellen der Geschwindigkeit zur Box hinzu
hScaleGeschwindigkeitPackNew :: (BoxClass b, BahngeschwindigkeitKlasse bg) => b -> bg -> TMVar StatusGUI -> IO HScale
hScaleGeschwindigkeitPackNew box bahngeschwindigkeit tmvarStatus = do
    scale <- boxPackWidgetNew box PackGrow paddingDefault positionDefault $ widgetShowNew $ hScaleNewWithRange 0 100 1
    on scale valueChanged $ get scale rangeValue >>= \wert -> ausführenTMVarAktion (Geschwindigkeit bahngeschwindigkeit $ fromIntegral $ fromEnum wert) tmvarStatus
    pure scale

-- | Füge 'Button' zum umdrehen zur Box hinzu
buttonUmdrehenPackNew :: (BoxClass b, BahngeschwindigkeitKlasse bg, RangeClass r) => b -> bg -> r -> TMVar StatusGUI -> IO (Either Button ToggleButton)
buttonUmdrehenPackNew box bahngeschwindigkeit rangeGeschwindigkeit tmvarStatus = do
    set rangeGeschwindigkeit [rangeValue := 0]
    if (zugtyp bahngeschwindigkeit == Lego)
        then do
            toggleButton <- boxPackWidgetNewDefault box $ toggleButtonNewWithLabel (Language.umdrehen :: Text)
            on toggleButton toggled $ get toggleButton toggleButtonActive >>= \vorwärts -> ausführenTMVarAktion (Umdrehen bahngeschwindigkeit (Just $ if vorwärts then Vorwärts else Rückwärts)) tmvarStatus
            pure $ Right toggleButton
        else boxPackWidgetNewDefault box (buttonNewWithEventLabel Language.umdrehen $ ausführenTMVarAktion (Umdrehen bahngeschwindigkeit Nothing) tmvarStatus) >>= pure . Left

-- | 'Streckenabschnitt' darstellen
streckenabschnittPackNew :: Streckenabschnitt -> TMVar StatusGUI -> DynamischeWidgets -> IO StreckenabschnittWidget
streckenabschnittPackNew streckenabschnitt@(Streckenabschnitt {stromPin}) tmvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxStreckenabschnitte, vBoxHinzufügenWegstreckeStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte, fortfahrenWennToggledWegstrecke, tmvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckeNew streckenabschnitt vBoxHinzufügenWegstreckeStreckenabschnitte fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew vBoxHinzufügenPlanStreckenabschnitte (OStreckenabschnitt streckenabschnitt) tmvarPlanObjekt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxStreckenabschnitte $ hBoxNew False 0
    nameLabelPackNew hBox streckenabschnitt
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.strom stromPin
    toggleButtonStromPackNew hBox streckenabschnitt tmvarStatus
    labelFließendValuePackNew hBox streckenabschnitt
    let stWidgets = STWidgets {st=streckenabschnitt, stWidget=hBox, stHinzPL=hinzufügenPlanWidget, stHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxStreckenabschnitte (entfernenStreckenabschnitt stWidgets >> liftIO (entferneHinzufügenWegstreckeWidgets stWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets stWidgets dynamischeWidgets)) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OStreckenabschnitt stWidgets) tmvarStatus
    pure hBox
-- | Äußerstes Widget zur Darstellung eines 'Streckenabschnitt's
type StreckenabschnittWidget = HBox
-- | Widget zum Hinzufügen eines 'Streckenabschnitt's zu einer 'Wegstrecke'
type StreckenabschnittWidgetHinzufügenWegstrecke = (HBox, RCheckButton)
-- | Widget zum Hinzufügen eines 'Streckenabschnitt's zu einem 'Plan'
type StreckenabschnittWidgetHinzufügenPlan = Button
-- | 'Streckenabschnitt' mit zugehörigen Widgets
data STWidgets = STWidgets {
                        st :: Streckenabschnitt,
                        stWidget :: StreckenabschnittWidget,
                        stHinzPL :: StreckenabschnittWidgetHinzufügenPlan,
                        stHinzWS :: StreckenabschnittWidgetHinzufügenWegstrecke}
                                                                    deriving (Eq)

instance WegstreckenElement STWidgets where
    lensWegstrecke :: Lens' STWidgets RCheckButton
    lensWegstrecke = (Lens.lens stHinzWS (\st v -> st {stHinzWS=v})) . _2
    entferneHinzufügenWegstreckeWidgets :: STWidgets -> DynamischeWidgets -> IO ()
    entferneHinzufügenWegstreckeWidgets (STWidgets {stHinzWS}) (DynamischeWidgets {vBoxHinzufügenWegstreckeStreckenabschnitte})
        = containerRemove vBoxHinzufügenWegstreckeStreckenabschnitte $ stHinzWS ^. _1

instance PlanElement STWidgets where
    foldPlan :: Fold STWidgets (Maybe Button)
    foldPlan = Lens.folding $ (:[]) . Just . stHinzPL
    vBoxenHinzufügenPlan :: STWidgets -> DynamischeWidgets -> ZipList VBox
    vBoxenHinzufügenPlan _stWidgets (DynamischeWidgets {vBoxHinzufügenPlanStreckenabschnitte})
        = ZipList [vBoxHinzufügenPlanStreckenabschnitte]

instance StreckenObjekt STWidgets where
    zugtyp :: STWidgets -> Zugtyp
    zugtyp (STWidgets {st}) = zugtyp st
    pins :: STWidgets -> [Pin]
    pins (STWidgets {st}) = pins st
    erhalteName :: STWidgets -> Text
    erhalteName (STWidgets {st}) = erhalteName st

instance Aeson.ToJSON STWidgets where
    toJSON :: STWidgets -> Aeson.Value
    toJSON (STWidgets {st}) = Aeson.toJSON st

instance StreckenabschnittKlasse STWidgets where
    strom :: STWidgets -> Strom -> PwmMapIO ()
    strom (STWidgets {st}) = strom st

-- | Füge 'ToggleButton' zum einstellen des Stroms zur Box hinzu
toggleButtonStromPackNew :: (BoxClass b, StreckenabschnittKlasse s) => b -> s -> TMVar StatusGUI -> IO ToggleButton
toggleButtonStromPackNew box streckenabschnitt tmvarStatus = do
    toggleButton <- boxPackWidgetNewDefault box $ toggleButtonNewWithLabel (Language.strom :: Text)
    on toggleButton toggled $ get toggleButton toggleButtonActive >>= \an -> ausführenTMVarAktion (Strom streckenabschnitt $ if an then Fließend else Gesperrt) tmvarStatus
    pure toggleButton

-- | 'Weiche' darstellen
weichePackNew :: Weiche -> TMVar StatusGUI -> DynamischeWidgets -> IO WeicheWidget
weichePackNew weiche tmvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxWeichen, vBoxHinzufügenWegstreckeWeichen, vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts, fortfahrenWennToggledWegstrecke, tmvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- do
        hBoxHinzufügen <- boxPackWidgetNewDefault vBoxHinzufügenWegstreckeWeichen $ hBoxNew False 0
        checkButton <- boxPackWidgetNewDefault hBoxHinzufügen checkButtonNew
        boxPackWidgetNewDefault hBoxHinzufügen $ labelNew $ Just $ erhalteName weiche
        richtungsRadioButtons <- do
            let (h:|t) = erhalteRichtungen weiche
            hRichtungRadioButton <- boxPackWidgetNewDefault hBoxHinzufügen (radioButtonNewWithLabel $ show h) >>= \radioButton -> pure (h, radioButton)
            tRichtungRadioButtons <- mapM (\richtung -> boxPackWidgetNewDefault hBoxHinzufügen (radioButtonNewWithLabelFromWidget (snd hRichtungRadioButton) $ show richtung) >>= \radioButton -> pure (richtung, radioButton)) t
            pure $ hRichtungRadioButton :| tRichtungRadioButtons
        registrierterCheckButton <- registrieren checkButton fortfahrenWennToggledWegstrecke traversalHinzufügenWegstrecke
        pure (hBoxHinzufügen, registrierterCheckButton, richtungsRadioButtons)
    hinzufügenPlanWidgetGerade  <- if hatRichtung weiche Gerade then hinzufügenWidgetPlanNew vBoxHinzufügenPlanWeichenGerade (OWeiche weiche) tmvarPlanObjekt >>= pure . Just else pure Nothing
    hinzufügenPlanWidgetKurve   <- if hatRichtung weiche Kurve  then hinzufügenWidgetPlanNew vBoxHinzufügenPlanWeichenKurve  (OWeiche weiche) tmvarPlanObjekt >>= pure . Just else pure Nothing
    hinzufügenPlanWidgetLinks   <- if hatRichtung weiche Links  then hinzufügenWidgetPlanNew vBoxHinzufügenPlanWeichenLinks  (OWeiche weiche) tmvarPlanObjekt >>= pure . Just else pure Nothing
    hinzufügenPlanWidgetRechts  <- if hatRichtung weiche Rechts then hinzufügenWidgetPlanNew vBoxHinzufügenPlanWeichenRechts (OWeiche weiche) tmvarPlanObjekt >>= pure . Just else pure Nothing
    let hinzufügenPlanWidget = (hinzufügenPlanWidgetGerade, hinzufügenPlanWidgetKurve, hinzufügenPlanWidgetLinks, hinzufügenPlanWidgetRechts)
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxWeichen $ hBoxNew False 0
    nameLabelPackNew hBox weiche
    richtungsButtonsPackNew weiche hBox
    labelFließendValuePackNew hBox weiche
    let weWidgets = WEWidgets {we=weiche, weWidget=hBox, weHinzPL=hinzufügenPlanWidget, weHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxWeichen (entfernenWeiche weWidgets >> liftIO (entferneHinzufügenWegstreckeWidgets weWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets weWidgets dynamischeWidgets)) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OWeiche weWidgets) tmvarStatus
    pure hBox
        where
            richtungsButtonsPackNew :: (BoxClass b) => Weiche -> b -> IO ()
            richtungsButtonsPackNew (LegoWeiche {richtungsPin, richtungen=(richtung1, richtung2)})    box = void $ do
                boxPackWidgetNewDefault box $ pinLabelNew Language.richtung richtungsPin
                boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung1) $ ausführenTMVarAktion (Stellen weiche richtung1) tmvarStatus
                boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung2) $ ausführenTMVarAktion (Stellen weiche richtung2) tmvarStatus
            richtungsButtonsPackNew (MärklinWeiche {richtungsPins})                                     box = mapM_ (\(richtung, pin) -> boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung <:> showText pin) $ ausführenTMVarAktion (Stellen weiche richtung) tmvarStatus) richtungsPins
-- | Äußerstes Widget zur Darstellung einer 'Weiche'
type WeicheWidget = HBox
-- | Widget zum Hinzufügen einer 'Weiche' zu einer 'Wegstrecke'
type WeicheWidgetHinzufügenWegstrecke = (HBox, RCheckButton, NonEmpty (Richtung, RadioButton))
-- | Widget zum Hinzufügen einer 'Weiche' zu einem 'Plan'
type WeicheWidgetHinzufügenPlan = (Maybe Button, Maybe Button, Maybe Button, Maybe Button)
-- | 'Weiche' mit zugehörigen Widgets
data WEWidgets = WEWidgets {
                        we :: Weiche,
                        weWidget :: WeicheWidget,
                        weHinzPL :: WeicheWidgetHinzufügenPlan,
                        weHinzWS :: WeicheWidgetHinzufügenWegstrecke}
                                                            deriving (Eq)

-- | Erhalte 'RadioButton's zum wählen der Richtungen einer Lego-Weiche
getterRichtungsRadioButtons :: Getter WEWidgets (NonEmpty (Richtung, RadioButton))
getterRichtungsRadioButtons = Lens.to $ \weWidgets -> (weHinzWS weWidgets) ^. _3

instance WegstreckenElement WEWidgets where
    lensWegstrecke :: Lens' WEWidgets RCheckButton
    lensWegstrecke = (Lens.lens weHinzWS (\we v -> we {weHinzWS=v})) . _2
    entferneHinzufügenWegstreckeWidgets :: WEWidgets -> DynamischeWidgets -> IO ()
    entferneHinzufügenWegstreckeWidgets (WEWidgets {weHinzWS}) (DynamischeWidgets {vBoxHinzufügenWegstreckeWeichen})
        = containerRemove vBoxHinzufügenWegstreckeWeichen $ weHinzWS ^. _1

instance PlanElement WEWidgets where
    foldPlan :: Fold WEWidgets (Maybe Button)
    foldPlan = Lens.folding $ \(WEWidgets {weHinzPL=(a, b, c, d)}) -> [a, b, c, d]
    vBoxenHinzufügenPlan :: WEWidgets -> DynamischeWidgets -> ZipList VBox
    vBoxenHinzufügenPlan _weWidgets (DynamischeWidgets {vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts})
        = ZipList [vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts]

instance StreckenObjekt WEWidgets where
    zugtyp :: WEWidgets -> Zugtyp
    zugtyp  (WEWidgets {we})    = zugtyp we
    pins :: WEWidgets -> [Pin]
    pins    (WEWidgets {we})    = pins we
    erhalteName :: WEWidgets -> Text
    erhalteName (WEWidgets {we})    = erhalteName we

instance Aeson.ToJSON WEWidgets where
    toJSON :: WEWidgets -> Aeson.Value
    toJSON (WEWidgets {we}) = Aeson.toJSON we

instance WeicheKlasse WEWidgets where
    stellen :: WEWidgets -> Richtung -> PwmMapIO ()
    stellen (WEWidgets {we}) = stellen we
    erhalteRichtungen :: WEWidgets -> NonEmpty Richtung
    erhalteRichtungen (WEWidgets {we}) = erhalteRichtungen we

-- | 'Kupplung' darstellen
kupplungPackNew :: Kupplung -> TMVar StatusGUI -> DynamischeWidgets -> IO KupplungWidget
kupplungPackNew kupplung@(Kupplung {kupplungsPin}) tmvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxKupplungen, vBoxHinzufügenWegstreckeKupplungen, vBoxHinzufügenPlanKupplungen, fortfahrenWennToggledWegstrecke, tmvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckeNew kupplung vBoxHinzufügenWegstreckeKupplungen fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew vBoxHinzufügenPlanKupplungen (OKupplung kupplung) tmvarPlanObjekt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxKupplungen $ hBoxNew False 0
    nameLabelPackNew hBox kupplung
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.kupplung kupplungsPin
    buttonKuppelnPackNew hBox kupplung tmvarStatus
    labelFließendValuePackNew hBox kupplung
    let kuWidgets = KUWidgets {ku=kupplung, kuWidget=hBox, kuHinzPL=hinzufügenPlanWidget, kuHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxKupplungen (entfernenKupplung kuWidgets >> liftIO (entferneHinzufügenWegstreckeWidgets kuWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets kuWidgets dynamischeWidgets)) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OKupplung kuWidgets) tmvarStatus
    pure hBox
-- | Äußerstes Widget zur Darstellung einer 'Kupplung'
type KupplungWidget = HBox
-- | Widget zum Hinzufügen einer 'Kupplung' zu einer 'Wegstrecke'
type KupplungWidgetHinzufügenWegstrecke = (HBox, RCheckButton)
-- | Widget zum Hinzufügen einer 'Kupplung' zu einem 'Plan'
type KupplungWidgetHinzufügenPlan = Button
-- | 'Kupplung' mit zugehörigen Widgets
data KUWidgets = KUWidgets {
                        ku :: Kupplung,
                        kuWidget :: KupplungWidget,
                        kuHinzPL :: KupplungWidgetHinzufügenPlan,
                        kuHinzWS :: KupplungWidgetHinzufügenWegstrecke}
                                                                deriving (Eq)

instance WegstreckenElement KUWidgets where
    lensWegstrecke :: Lens' KUWidgets RCheckButton
    lensWegstrecke = (Lens.lens kuHinzWS (\ku v -> ku {kuHinzWS=v})) . _2
    entferneHinzufügenWegstreckeWidgets :: KUWidgets -> DynamischeWidgets -> IO ()
    entferneHinzufügenWegstreckeWidgets (KUWidgets {kuHinzWS}) (DynamischeWidgets {vBoxHinzufügenWegstreckeKupplungen})
        = containerRemove vBoxHinzufügenWegstreckeKupplungen $ kuHinzWS ^. _1

instance PlanElement KUWidgets where
    foldPlan :: Fold KUWidgets (Maybe Button)
    foldPlan = Lens.folding $ (:[]) . Just . kuHinzPL
    vBoxenHinzufügenPlan :: KUWidgets -> DynamischeWidgets -> ZipList VBox
    vBoxenHinzufügenPlan _kuWidgets (DynamischeWidgets {vBoxHinzufügenPlanKupplungen})
        = ZipList [vBoxHinzufügenPlanKupplungen]

instance StreckenObjekt KUWidgets where
    zugtyp :: KUWidgets -> Zugtyp
    zugtyp (KUWidgets {ku}) = zugtyp ku
    pins :: KUWidgets -> [Pin]
    pins (KUWidgets {ku}) = pins ku
    erhalteName :: KUWidgets -> Text
    erhalteName (KUWidgets {ku}) = erhalteName ku

instance Aeson.ToJSON KUWidgets where
    toJSON :: KUWidgets -> Aeson.Value
    toJSON (KUWidgets {ku}) = Aeson.toJSON ku

instance KupplungKlasse KUWidgets where
    kuppeln :: KUWidgets -> PwmMapIO ()
    kuppeln (KUWidgets {ku}) = kuppeln ku

-- | Füge 'Button' zum kuppeln zur Box hinzu
buttonKuppelnPackNew :: (BoxClass b, KupplungKlasse k) => b -> k -> TMVar StatusGUI -> IO Button
buttonKuppelnPackNew box kupplung tmvarStatus = boxPackWidgetNewDefault box $ buttonNewWithEventLabel Language.kuppeln $ ausführenTMVarAktion (Kuppeln kupplung) tmvarStatus

-- | 'Wegstrecke' darstellen
wegstreckePackNew :: Wegstrecke -> TMVar StatusGUI -> DynamischeWidgets -> IO WegstreckeWidget
wegstreckePackNew wegstrecke@(Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen}) tmvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxWegstrecken, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung, tmvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenPlanWidgetBG  <- if null wsBahngeschwindigkeiten  then pure Nothing else do
        hinzufügenPlanWidgetBG <- hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit        (OWegstrecke wegstrecke) tmvarPlanObjekt
        hinzufügenPlanWidgetBGZ <- case zugtyp wegstrecke of
            (Lego)          -> hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego     (OWegstrecke wegstrecke) tmvarPlanObjekt >>= pure . Left
            (Märklin)       -> hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin  (OWegstrecke wegstrecke) tmvarPlanObjekt >>= pure . Right
            (Undefiniert)   -> error "Wegstrecke mit Bahngeschwindigkeit und Undefiniertem Zugtyp erstellt."
        pure $ Just (hinzufügenPlanWidgetBG, hinzufügenPlanWidgetBGZ)
    hinzufügenPlanWidgetST  <- if null wsStreckenabschnitte     then pure Nothing else hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenStreckenabschnitt          (OWegstrecke wegstrecke) tmvarPlanObjekt >>= pure . Just
    hinzufügenPlanWidgetWE  <- if null wsWeichenRichtungen      then pure Nothing else hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenWeiche                     (OWegstrecke wegstrecke) tmvarPlanObjekt >>= pure . Just
    hinzufügenPlanWidgetKU  <- if null wsKupplungen             then pure Nothing else hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenKupplung                   (OWegstrecke wegstrecke) tmvarPlanObjekt >>= pure . Just
    let hinzufügenPlanWidget = (hinzufügenPlanWidgetBG, hinzufügenPlanWidgetST, hinzufügenPlanWidgetWE, hinzufügenPlanWidgetKU)
    -- Widget erstellen
    frame <- boxPackWidgetNewDefault vBoxWegstrecken frameNew
    vBox <- containerAddWidgetNew frame $ vBoxNew False 0
    nameLabelPackNew vBox wegstrecke
    expander <- boxPackWidgetNewDefault vBox $ expanderNew (Language.wegstreckenElemente :: Text)
    vBoxExpander <- containerAddWidgetNew expander $ vBoxNew False 0
    functionBox <- boxPackWidgetNewDefault vBox $ hBoxNew False 0
    unless (null wsBahngeschwindigkeiten) $ void $ do
        boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.bahngeschwindigkeiten <:> foldl appendName ("") wsBahngeschwindigkeiten
        hScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew functionBox wegstrecke tmvarStatus
        buttonUmdrehenPackNew functionBox wegstrecke hScaleGeschwindigkeit tmvarStatus
    unless (null wsStreckenabschnitte) $ void $ do
        boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.streckenabschnitte <:> foldl appendName ("") wsStreckenabschnitte
        toggleButtonStromPackNew functionBox wegstrecke tmvarStatus
    unless (null wsWeichenRichtungen) $ void $ do
        boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.weichen <:> foldl (\acc (weiche, richtung) -> appendName acc weiche <°> showText richtung) ("") wsWeichenRichtungen
        boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.einstellen $ ausführenTMVarAktion (Einstellen wegstrecke) tmvarStatus
    unless (null wsKupplungen) $ void $ do
        boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.kupplungen <:> foldl appendName ("") wsKupplungen
        buttonKuppelnPackNew functionBox wegstrecke tmvarStatus
    let wsWidgets = WSWidgets {ws=wegstrecke, wsWidget=frame, wsHinzPL=hinzufügenPlanWidget}
    buttonEntfernenPack functionBox (containerRemove vBoxWegstrecken frame >> entferneHinzufügenPlanWidgets wsWidgets dynamischeWidgets) (entfernenWegstrecke wsWidgets) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OWegstrecke wsWidgets) tmvarStatus
    pure frame
        where
            appendName :: (StreckenObjekt o) => Text -> o -> Text
            appendName ("")     objekt = erhalteName objekt
            appendName string   objekt = string <^> erhalteName objekt
-- | Äußerstes Widget zur Darstellung einer 'Wegstrecke'
type WegstreckeWidget = Frame
-- | Widget zum Hinzufügen einer 'Wegstrecke' zu einem 'Plan'
type WegstreckeWidgetHinzufügenPlan = (Maybe (Button, Either Button Button), Maybe Button, Maybe Button, Maybe Button)
-- | 'Wegstrecke' mit zugehörigen Widgets
data WSWidgets = WSWidgets {
                        ws :: Wegstrecke,
                        wsWidget :: WegstreckeWidget,
                        wsHinzPL :: WegstreckeWidgetHinzufügenPlan}
                                                            deriving (Eq)

instance PlanElement WSWidgets where
    foldPlan :: Fold WSWidgets (Maybe Button)
    foldPlan = Lens.folding $ \(WSWidgets {wsHinzPL=(bgs, st, we, ku)}) -> bgButtons bgs <> [st, we, ku]
        where
            bgButtons :: Maybe (Button, Either Button Button) -> [Maybe Button]
            bgButtons (Nothing)                 = [Nothing, Nothing, Nothing]
            bgButtons (Just (bg, (Left bgL)))   = [Just bg, Just bgL, Nothing]
            bgButtons (Just (bg, (Right bgM)))  = [Just bg, Nothing, Just bgM]
    vBoxenHinzufügenPlan :: WSWidgets -> DynamischeWidgets -> ZipList VBox
    vBoxenHinzufügenPlan _wsWidgets (DynamischeWidgets {vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung})
        = ZipList [vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung]

instance StreckenObjekt WSWidgets where
    zugtyp :: WSWidgets -> Zugtyp
    zugtyp (WSWidgets {ws}) = zugtyp ws
    pins :: WSWidgets -> [Pin]
    pins (WSWidgets {ws}) = pins ws
    erhalteName :: WSWidgets -> Text
    erhalteName (WSWidgets {ws}) = erhalteName ws

instance Aeson.ToJSON WSWidgets where
    toJSON :: WSWidgets -> Aeson.Value
    toJSON (WSWidgets {ws}) = Aeson.toJSON ws

instance BahngeschwindigkeitKlasse WSWidgets where
    geschwindigkeit :: WSWidgets -> Natural -> PwmMapIO ()
    geschwindigkeit (WSWidgets {ws}) = geschwindigkeit ws
    umdrehen :: WSWidgets -> Maybe Fahrtrichtung -> PwmMapIO ()
    umdrehen (WSWidgets {ws}) = umdrehen ws

instance StreckenabschnittKlasse WSWidgets where
    strom :: WSWidgets -> Strom -> PwmMapIO ()
    strom (WSWidgets {ws}) = strom ws

instance KupplungKlasse WSWidgets where
    kuppeln :: WSWidgets -> PwmMapIO ()
    kuppeln (WSWidgets {ws}) = kuppeln ws

instance WegstreckeKlasse WSWidgets where
    einstellen :: WSWidgets -> PwmMapIO ()
    einstellen (WSWidgets {ws}) = einstellen ws

-- | 'Plan' darstellen
planPackNew :: Plan -> TMVar StatusGUI -> DynamischeWidgets -> IO PlanWidget
planPackNew plan@(Plan {plAktionen}) tmvarStatus (DynamischeWidgets {vBoxPläne, progressBarPlan, windowMain})= do
    -- Widget erstellen
    frame <- boxPackWidgetNewDefault vBoxPläne $ frameNew
    vBox <- containerAddWidgetNew frame $ vBoxNew False 0
    nameLabelPackNew vBox plan
    expander <- boxPackWidgetNewDefault vBox $ expanderNew $ Language.aktionen <:> show (length plAktionen)
    vBoxExpander <- containerAddWidgetNew expander $ vBoxNew False 0
    mapM_ ((boxPackWidgetNewDefault vBoxExpander) . labelNew . Just . show) plAktionen
    functionBox <- boxPackWidgetNewDefault vBox hButtonBoxNew
    buttonAusführen <- boxPackWidgetNewDefault functionBox $ buttonNewWithLabel (Language.ausführen :: Text)
    buttonAbbrechen <- boxPackWidgetNewDefault functionBox $ buttonNewWithLabel (Language.ausführenAbbrechen :: Text)
    widgetHide buttonAbbrechen
    dialogGesperrt <- messageDialogNew (Just windowMain) [] MessageError ButtonsOk ("" :: Text)
    set dialogGesperrt [windowTitle := (Language.aktionGesperrt :: Text)]
    on buttonAusführen buttonActivated $ do
        auswertenTMVarIOStatus (ausführenMöglich plan) tmvarStatus >>= \case
            (AusführenMöglich)  -> do
                widgetHide buttonAusführen
                widgetShow buttonAbbrechen
                void $ ausführenTMVarBefehl (Ausführen plan (\wert -> set progressBarPlan [progressBarFraction := (toEnum $ fromIntegral wert) / (toEnum $ length plAktionen)]) $ widgetShow buttonAusführen >> widgetHide buttonAbbrechen) tmvarStatus
            (WirdAusgeführt)    -> error "Ausführen in GTK-UI erneut gestartet."
            (PinsBelegt pins)   -> do
                set dialogGesperrt [messageDialogText := Just (Language.ausführenGesperrt $ show $ ausFoldable pins)]
                void $ dialogEval dialogGesperrt
    on buttonAbbrechen buttonActivated $ do
        ausführenTMVarBefehl (AusführenAbbrechen plan) tmvarStatus
        widgetShow buttonAusführen
        widgetHide buttonAbbrechen
    let plWidgets = PLWidgets {pl=plan, plWidget=frame}
    buttonEntfernenPack functionBox (containerRemove vBoxPläne frame) (entfernenPlan plWidgets) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OPlan plWidgets) tmvarStatus
    pure frame
-- | Äußerstes Widget zur Darstellung eines 'Plan's
type PlanWidget = Frame
-- | 'Plan' mit zugehörigen Widgets
data PLWidgets = PLWidgets {
                        pl :: Plan,
                        plWidget :: PlanWidget}
                                        deriving (Eq)

instance StreckenObjekt PLWidgets where
    zugtyp :: PLWidgets -> Zugtyp
    zugtyp (PLWidgets {pl}) = zugtyp pl
    pins :: PLWidgets -> [Pin]
    pins (PLWidgets {pl}) = pins pl
    erhalteName :: PLWidgets -> Text
    erhalteName (PLWidgets {pl}) = erhalteName pl

instance Aeson.ToJSON PLWidgets where
    toJSON :: PLWidgets -> Aeson.Value
    toJSON (PLWidgets {pl}) = Aeson.toJSON pl

instance PlanKlasse PLWidgets where
    ausführenPlan :: PLWidgets -> (Natural -> IO ()) -> IO () -> TVar (Menge Ausführend) -> PwmMapIO ()
    ausführenPlan (PLWidgets {pl}) = ausführenPlan pl
#endif