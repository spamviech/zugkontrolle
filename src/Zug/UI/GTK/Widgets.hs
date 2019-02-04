{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DuplicateRecordFields, RankNTypes, InstanceSigs, CPP #-}

{-|
Description : Erstelle zusammengesetzte Widgets.

Allgemeine Hilfsfunktionen zum erstellen neuer Widgets
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.GTK.Widgets () where
#else
module Zug.UI.GTK.Widgets (
                        -- * Allgemeine Widget-Funktionen
                        widgetShowNew, widgetNewWithOptionsEvents, containerAddWidgetNew, boxPackWidgetNew, notebookAppendPageNew, containerRemoveJust, widgetShowIf,
                        boxPack, boxPackDefault, boxPackWidgetNewDefault, packingDefault, paddingDefault, positionDefault,
                        -- *`* Scrollbare Widgets
                        scrolledWidgetPackNew, scrolledWidgedNotebookAppendPageNew,
                        -- ** Knopf erstellen
                        buttonNewWithEvent, buttonNewWithEventLabel, buttonNewWithEventMnemonic,
                        -- ** Pin darstellen
                        pinLabelNew, pinSpinBoxNew, nameEntryPackNew,
                        -- ** Spezifisches StreckenObjekt darstellen
                        bahngeschwindigkeitPackNew, streckenabschnittPackNew, weichePackNew, kupplungPackNew, wegstreckePackNew, planPackNew,
                        -- * Verwaltung des aktuellen Zustands
                        DynamischeWidgets(..), StatusGUI, IOStatusGUI, MStatusGUI, MonadMStatusGUI, BGWidgets(..), STWidgets(..), WEWidgets(..), KUWidgets(..), WSWidgets(..), PLWidgets(..),
                        traversalHinzufügenWegstrecke, WegstreckenElement(..), getterRichtungsRadioButtons, PlanElement(..)) where

-- Bibliotheken
import Control.Applicative (ZipList(..))
import Control.Concurrent.MVar
import Control.Lens (Traversal', Lens', Getter, Fold, (%%~), (^.), (^..), Field2(..), Field3(..))
import qualified Control.Lens as Lens
import Control.Monad
import Control.Monad.State (State, StateT)
import Control.Monad.Trans
import Data.Aeson (ToJSON(..), Value)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Graphics.UI.Gtk
import Numeric.Natural
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar
import qualified Zug.Language as Language
import Zug.Language ((<^>), (<->), (<:>), (<°>), addMnemonic, showText)
import Zug.Klassen
import Zug.Anbindung
import Zug.Plan
import Zug.UI.Base
import Zug.UI.Befehl
import Zug.UI.GTK.FortfahrenWenn

-- * Sammel-Typ um dynamische Widgets zu speichern
type StatusGUI = StatusGeneral BGWidgets STWidgets WEWidgets KUWidgets WSWidgets PLWidgets
type IOStatusGUI = StateT StatusGUI IO
type MStatusGUI = State StatusGUI
type MonadMStatusGUI m a = StateT StatusGUI m a

data DynamischeWidgets = DynamischeWidgets {
    vBoxBahngeschwindigkeiten :: VBox,
    vBoxStreckenabschnitte :: VBox,
    vBoxWeichen :: VBox,
    vBoxKupplungen :: VBox,
    vBoxWegstrecken :: VBox,
    vBoxPläne :: VBox,
    vBoxHinzufügenWegstreckeBahngeschwindigkeiten :: VBox,
    vBoxHinzufügenPlanBahngeschwindigkeiten :: VBox,
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
    vBoxHinzufügenPlanWegstreckenStreckenabschnitt :: VBox,
    vBoxHinzufügenPlanWegstreckenWeiche :: VBox,
    vBoxHinzufügenPlanWegstreckenKupplung :: VBox,
    progressBarPlan :: ProgressBar,
    mvarPlanObjekt :: MVar (Maybe Objekt)}

-- | Linse auf CheckButton, ob 'StreckenObjekt' zu einer 'Wegstrecke' hinzugefügt werden soll
class WegstreckenElement s where
    lensWegstrecke :: Lens' s VRCheckButton

-- | Faltung auf Buttons (falls vorhanden), welches 'StreckenObjekt' für eine Aktion verwendet werden soll
class PlanElement s where
    foldPlan :: Fold s (Maybe Button)

-- type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s
traversalHinzufügenWegstrecke :: Traversal' StatusGUI VRCheckButton
traversalHinzufügenWegstrecke f status = Status <$> traverseList f (status ^. bahngeschwindigkeiten) <*> traverseList f (status ^. streckenabschnitte) <*> traverseList f (status ^. weichen) <*> traverseList f (status ^. kupplungen) <*> pure (status ^. wegstrecken) <*> pure (status ^. pläne) <*> pure (status ^. mvarPinMap)
    where
        traverseList :: (Applicative f, WegstreckenElement s) => (VRCheckButton -> f VRCheckButton) -> [s] -> f [s]
        traverseList f list = traverse . lensWegstrecke %%~ f $ list

-- * Hilfsfunktionen um Unter-Widgets zu erstellen
-- | Widget erstellen und anzeigen
widgetShowNew :: (WidgetClass w) => IO w -> IO w
widgetShowNew konstruktor = do
    widget <- konstruktor
    widgetShow widget
    pure widget

-- | Neu erstelltes Widget zu Container hinzufügen
containerAddWidgetNew :: (ContainerClass c, WidgetClass w) => c -> IO w -> IO w
containerAddWidgetNew container konstruktor = do
    widget <- widgetShowNew konstruktor
    containerAdd container widget
    pure widget

-- | Widget in eine Box packen
boxPack :: (BoxClass b, WidgetClass w) => b -> w -> Packing -> Int -> Bool -> IO ()
boxPack box widget packing padding start = (if start then boxPackStart else boxPackEnd) box widget packing padding

-- | Neu erstelltes Widget in eine Box packen
boxPackWidgetNew :: (BoxClass b, WidgetClass w) => b -> Packing -> Int -> Bool -> IO w -> IO w
boxPackWidgetNew box packing padding start konstruktor = do
    widget <- widgetShowNew konstruktor
    boxPack box widget packing padding start
    pure widget

packingDefault :: Packing
packingDefault = PackNatural

paddingDefault :: Int
paddingDefault = 0

positionDefault :: Bool
positionDefault = True

-- | Neu erstelltes Widget mit Standard Packing, Padding und Positionierung in eine Box packen
boxPackWidgetNewDefault :: (BoxClass b, WidgetClass w) => b -> IO w -> IO w
boxPackWidgetNewDefault box = boxPackWidgetNew box packingDefault paddingDefault positionDefault

-- | Widget mit Standard Packing, Padding und Positionierung in eine Box packen
boxPackDefault :: (BoxClass b, WidgetClass w) => b -> w -> IO ()
boxPackDefault box widget = boxPack box widget packingDefault paddingDefault positionDefault

-- | Neu erstelltes Widget zu Notebook hinzufügen
notebookAppendPageNew :: (NotebookClass n, WidgetClass w) => n -> Text -> IO w -> IO w
notebookAppendPageNew notebook name konstruktor = do
    widget <- widgetShowNew konstruktor
    notebookAppendPage notebook widget name
    pure widget

-- | Widget neu erstellen, Optionen setzen und Events erstellen
widgetNewWithOptionsEvents :: (WidgetClass w) => IO w -> [AttrOp w] -> [(Signal w a, Either a (w -> a))] -> IO w
widgetNewWithOptionsEvents konstruktor options eventActions = do
    widget <- konstruktor
    set widget options
    mapM_ (uncurryEither on widget) eventActions
    pure widget
        where
            uncurryEither :: (WidgetClass w) => (w -> a -> b -> c) -> w -> (a, Either b (w -> b)) -> c
            uncurryEither f w (a, (Left b))     = f w a b
            uncurryEither f w (a, (Right fb))   = f w a $ fb w

-- | Entferne ein vielleicht vorhandenes Widget aus einem Container
containerRemoveJust :: (ContainerClass c, WidgetClass w) => c -> Maybe w -> IO ()
containerRemoveJust _container  (Nothing)   = pure ()
containerRemoveJust container   (Just w)    = containerRemove container w

-- | Zeige widget, falls eine Bedingung erfüllt ist
widgetShowIf :: (WidgetClass w) => Bool -> w -> IO ()
widgetShowIf    True    = widgetShow
widgetShowIf    False   = widgetHide

-- ** Knöpfe mit einer Funktion
-- | Knopf mit Label und Funktion erstellen
buttonNewWithEvent :: IO Button -> IO () -> IO Button
buttonNewWithEvent konstruktor action = widgetNewWithOptionsEvents konstruktor [] [(buttonActivated, Left action)]

buttonNewWithEventMnemonic :: Text -> IO () -> IO Button
buttonNewWithEventMnemonic label = buttonNewWithEvent $ buttonNewWithMnemonic $ addMnemonic label

buttonNewWithEventLabel :: Text -> IO () -> IO Button
buttonNewWithEventLabel label = buttonNewWithEvent $ buttonNewWithLabel label

-- | Entfernen-Knopf zu Box hinzufügen
buttonEntfernenPack :: (BoxClass b, LikeMVar lmvar) => b -> IO () -> IOStatusGUI () -> lmvar StatusGUI -> IO Button
buttonEntfernenPack box removeActionGUI removeAction mvarStatus = boxPackWidgetNew box PackNatural paddingDefault False $ buttonNewWithEventLabel Language.entfernen $ evalMVarIOStatus removeAction mvarStatus >> removeActionGUI

buttonEntfernenPackSimple :: (BoxClass b, ContainerClass c, LikeMVar lmvar) => b -> c -> IOStatusGUI () -> lmvar StatusGUI -> IO Button
buttonEntfernenPackSimple box parent = buttonEntfernenPack box $ containerRemove parent box

-- ** Darstellung von Pins
-- | Label für Pin erstellen
pinLabelNew :: Text -> Pin -> IO Label
pinLabelNew name pin = labelNew $ Just $ name <-> Language.pin <:> showText pin

-- | SpinBox zur Pin-Abfrage erstellen
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
    boxPackWidgetNewDefault hBox $ widgetNewWithOptionsEvents entryNew [entryPlaceholderText := Just (Language.name :: Text)] []

-- | Name anzeigen
nameLabelPackNew :: (BoxClass b, StreckenObjekt s) => b -> s -> IO Label
nameLabelPackNew box objekt = boxPackWidgetNewDefault box $ widgetNewWithOptionsEvents (labelNew $ Just $ getName objekt) [widgetMarginRight := 5] []

-- ** Scrollbare Widgets erstellen
-- | Erstelle neues ScrolledWindow mit automatisch erstelltem Viewport
scrolledWidgetPackNew :: (BoxClass b, WidgetClass w) => b -> IO w -> IO (ScrolledWindow, w)
scrolledWidgetPackNew box konstruktor = do
    widget <- widgetShowNew konstruktor
    scrolledWindow <- boxPackWidgetNew box PackGrow paddingDefault positionDefault $ widgetNewWithOptionsEvents (scrolledWindowNew Nothing Nothing) [scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyAlways] []
    scrolledWindowAddWithViewport scrolledWindow widget
    pure (scrolledWindow, widget)

-- | Seite mit scrollbarer VBox einem Notebook hinzufügen
scrolledWidgedNotebookAppendPageNew :: (NotebookClass n, WidgetClass w) => n -> Text -> IO w -> IO (ScrolledWindow, w)
scrolledWidgedNotebookAppendPageNew notebook name konstruktor = do
    widget <- widgetShowNew konstruktor
    scrolledWindow <- notebookAppendPageNew notebook name $ widgetNewWithOptionsEvents (scrolledWindowNew Nothing Nothing) [scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyAlways] []
    scrolledWindowAddWithViewport scrolledWindow widget
    pure (scrolledWindow, widget)

-- ** Widget mit Name und CheckButton erstellen
hinzufügenWidgetSimpleNew :: (StreckenObjekt o, BoxClass b) => o -> b -> IO (HBox, VRCheckButton)
hinzufügenWidgetSimpleNew objekt box = do
    hBoxHinzufügen <- boxPackWidgetNewDefault box $ hBoxNew False 0
    checkButton <- boxPackWidgetNewDefault hBoxHinzufügen checkButtonNew
    boxPackWidgetNewDefault hBoxHinzufügen $ labelNew $ Just $ getName objekt
    pure (hBoxHinzufügen, unregistriert checkButton)

hinzufügenWidgetPlanNew :: (StreckenObjekt o, BoxClass b, LikeMVar lmvar) => o -> b -> (o -> Objekt) -> lmvar (Maybe Objekt) -> IO Button
hinzufügenWidgetPlanNew objekt box objektKonstruktor lmvar = boxPackWidgetNewDefault box $ buttonNewWithEventLabel (getName objekt) $ putLMVar lmvar $ Just $ objektKonstruktor objekt

-- * Darstellung von Streckenobjekten
-- | Bahngeschwindigkeit darstellen
bahngeschwindigkeitPackNew :: (LikeMVar lmvar) => Bahngeschwindigkeit -> lmvar StatusGUI -> DynamischeWidgets -> IO BahngeschwindigkeitWidget
bahngeschwindigkeitPackNew bahngeschwindigkeit mvarStatus (DynamischeWidgets {vBoxBahngeschwindigkeiten, vBoxHinzufügenWegstreckeBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeiten, mvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetSimpleNew bahngeschwindigkeit vBoxHinzufügenWegstreckeBahngeschwindigkeiten
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew bahngeschwindigkeit vBoxHinzufügenPlanBahngeschwindigkeiten OBahngeschwindigkeit mvarPlanObjekt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxBahngeschwindigkeiten $ hBoxNew False 0
    nameLabelPackNew hBox bahngeschwindigkeit
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.geschwindigkeit $ getGeschwindigkeitsPin bahngeschwindigkeit
    hScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew hBox bahngeschwindigkeit mvarStatus
    fahrtrichtungsPinLabelPackNew hBox bahngeschwindigkeit
    buttonUmdrehenPackNew hBox bahngeschwindigkeit hScaleGeschwindigkeit mvarStatus
    let bgWidgets = BGWidgets {bg=bahngeschwindigkeit, bgWidget=hBox, bgHinzPL=hinzufügenPlanWidget, bgHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxBahngeschwindigkeiten (entfernenBahngeschwindigkeit bgWidgets >> liftIO (containerRemove vBoxHinzufügenWegstreckeBahngeschwindigkeiten (fst hinzufügenWegstreckeWidget) >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanBahngeschwindigkeiten] <*> ZipList (bgWidgets ^.. foldPlan)))) mvarStatus
    -- Widgets merken
    runMVarBefehl (Hinzufügen $ OBahngeschwindigkeit bgWidgets) mvarStatus
    pure hBox
        where
            getGeschwindigkeitsPin :: Bahngeschwindigkeit -> Pin
            getGeschwindigkeitsPin (LegoBahngeschwindigkeit {geschwindigkeitsPin})      = geschwindigkeitsPin
            getGeschwindigkeitsPin (MärklinBahngeschwindigkeit {geschwindigkeitsPin})   = geschwindigkeitsPin
            fahrtrichtungsPinLabelPackNew :: (BoxClass b) => b -> Bahngeschwindigkeit -> IO ()
            fahrtrichtungsPinLabelPackNew box   (LegoBahngeschwindigkeit {fahrtrichtungsPin})   = void $ boxPackWidgetNewDefault box (pinLabelNew Language.fahrtrichtung fahrtrichtungsPin)
            fahrtrichtungsPinLabelPackNew _box  (MärklinBahngeschwindigkeit {})                 = pure ()
type BahngeschwindigkeitWidget = HBox
type BahngeschwindigkeitWidgetHinzufügenWegstrecke = (HBox, VRCheckButton)
type BahngeschwindigkeitWidgetHinzufügenPlan = Button
data BGWidgets = BGWidgets {
                        bg :: Bahngeschwindigkeit,
                        bgWidget :: BahngeschwindigkeitWidget,
                        bgHinzPL :: BahngeschwindigkeitWidgetHinzufügenPlan,
                        bgHinzWS :: BahngeschwindigkeitWidgetHinzufügenWegstrecke}
                                                                        deriving (Eq)

instance WegstreckenElement BGWidgets where
    lensWegstrecke :: Lens' BGWidgets VRCheckButton
    lensWegstrecke = (Lens.lens bgHinzWS (\bg v -> bg {bgHinzWS=v})) . _2

instance PlanElement BGWidgets where
    foldPlan :: Fold BGWidgets (Maybe Button)
    foldPlan = Lens.folding $ (:[]) . Just . bgHinzPL

instance StreckenObjekt BGWidgets where
    zugtyp :: BGWidgets -> Zugtyp
    zugtyp  (BGWidgets {bg})    = zugtyp bg
    pins :: BGWidgets -> [Pin]
    pins    (BGWidgets {bg})    = pins bg
    getName :: BGWidgets -> Text
    getName (BGWidgets {bg})    = getName bg

instance ToJSON BGWidgets where
    toJSON :: BGWidgets -> Value
    toJSON  (BGWidgets {bg})    = toJSON bg

instance BahngeschwindigkeitKlasse BGWidgets where
    geschwindigkeit :: BGWidgets -> Natural -> PinMapIO ()
    geschwindigkeit (BGWidgets {bg})    = geschwindigkeit bg
    umdrehen :: BGWidgets -> Maybe Fahrtrichtung -> PinMapIO ()
    umdrehen    (BGWidgets {bg})    = umdrehen bg

hScaleGeschwindigkeitPackNew :: (BoxClass b, BahngeschwindigkeitKlasse bg, LikeMVar lmvar) => b -> bg -> lmvar StatusGUI -> IO HScale
hScaleGeschwindigkeitPackNew box bahngeschwindigkeit mvarStatus = boxPackWidgetNew box PackGrow paddingDefault positionDefault $ widgetNewWithOptionsEvents (hScaleNewWithRange 0 100 1) [] [(valueChanged, Right $ \widget -> get widget rangeValue >>= \wert -> runMVarAktion (Geschwindigkeit bahngeschwindigkeit $ fromIntegral $ fromEnum wert) mvarStatus)]

buttonUmdrehenPackNew :: (BoxClass b, BahngeschwindigkeitKlasse bg, LikeMVar lmvar, RangeClass r) => b -> bg -> r -> lmvar StatusGUI -> IO (Either Button ToggleButton)
buttonUmdrehenPackNew box bahngeschwindigkeit rangeGeschwindigkeit mvarStatus = do
    set rangeGeschwindigkeit [rangeValue := 0]
    if (zugtyp bahngeschwindigkeit == Lego)
        then boxPackWidgetNewDefault box (widgetNewWithOptionsEvents (toggleButtonNewWithLabel (Language.umdrehen :: Text)) [] [(toggled, Right $ \widget -> get widget toggleButtonActive >>= \vorwärts -> runMVarAktion (Umdrehen bahngeschwindigkeit (Just $ if vorwärts then Vorwärts else Rückwärts)) mvarStatus)]) >>= pure . Right
        else boxPackWidgetNewDefault box (buttonNewWithEventLabel Language.umdrehen $ runMVarAktion (Umdrehen bahngeschwindigkeit Nothing) mvarStatus) >>= pure . Left

-- | Streckenabschnitt darstellen
streckenabschnittPackNew :: (LikeMVar lmvar) => Streckenabschnitt -> lmvar StatusGUI -> DynamischeWidgets -> IO StreckenabschnittWidget
streckenabschnittPackNew streckenabschnitt@(Streckenabschnitt {stromPin}) mvarStatus (DynamischeWidgets {vBoxStreckenabschnitte, vBoxHinzufügenWegstreckeStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte, mvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetSimpleNew streckenabschnitt vBoxHinzufügenWegstreckeStreckenabschnitte
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew streckenabschnitt vBoxHinzufügenPlanStreckenabschnitte OStreckenabschnitt mvarPlanObjekt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxStreckenabschnitte $ hBoxNew False 0
    nameLabelPackNew hBox streckenabschnitt
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.strom stromPin
    toggleButtonStromPackNew hBox streckenabschnitt mvarStatus
    let stWidgets = STWidgets {st=streckenabschnitt, stWidget=hBox, stHinzPL=hinzufügenPlanWidget, stHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxStreckenabschnitte (entfernenStreckenabschnitt stWidgets >> liftIO (containerRemove vBoxHinzufügenWegstreckeStreckenabschnitte (fst hinzufügenWegstreckeWidget) >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanStreckenabschnitte] <*> ZipList (stWidgets ^.. foldPlan)))) mvarStatus
    -- Widgets merken
    runMVarBefehl (Hinzufügen $ OStreckenabschnitt stWidgets) mvarStatus
    pure hBox
type StreckenabschnittWidget = HBox
type StreckenabschnittWidgetHinzufügenWegstrecke = (HBox, VRCheckButton)
type StreckenabschnittWidgetHinzufügenPlan = Button
data STWidgets = STWidgets {
                        st :: Streckenabschnitt,
                        stWidget :: StreckenabschnittWidget,
                        stHinzPL :: StreckenabschnittWidgetHinzufügenPlan,
                        stHinzWS :: StreckenabschnittWidgetHinzufügenWegstrecke}
                                                                    deriving (Eq)

instance WegstreckenElement STWidgets where
    lensWegstrecke :: Lens' STWidgets VRCheckButton
    lensWegstrecke = (Lens.lens stHinzWS (\st v -> st {stHinzWS=v})) . _2

instance PlanElement STWidgets where
    foldPlan :: Fold STWidgets (Maybe Button)
    foldPlan = Lens.folding $ (:[]) . Just . stHinzPL

instance StreckenObjekt STWidgets where
    zugtyp :: STWidgets -> Zugtyp
    zugtyp  (STWidgets {st})    = zugtyp st
    pins :: STWidgets -> [Pin]
    pins    (STWidgets {st})    = pins st
    getName :: STWidgets -> Text
    getName (STWidgets {st})    = getName st

instance ToJSON STWidgets where
    toJSON :: STWidgets -> Value
    toJSON  (STWidgets {st})    = toJSON st

instance StreckenabschnittKlasse STWidgets where
    strom :: STWidgets -> Bool -> MVar PinMap -> IO ()
    strom   (STWidgets {st})    = strom st

toggleButtonStromPackNew :: (BoxClass b, StreckenabschnittKlasse s, LikeMVar lmvar) => b -> s -> lmvar StatusGUI -> IO ToggleButton
toggleButtonStromPackNew box streckenabschnitt mvarStatus = boxPackWidgetNewDefault box $ widgetNewWithOptionsEvents (toggleButtonNewWithLabel (Language.strom :: Text)) [] [(toggled, Right $ \widget -> get widget toggleButtonActive >>= \an -> runMVarAktion (Strom streckenabschnitt an) mvarStatus)]

-- | Weiche darstellen
weichePackNew :: (LikeMVar lmvar) => Weiche -> lmvar StatusGUI -> DynamischeWidgets -> IO WeicheWidget
weichePackNew   weiche  mvarStatus  (DynamischeWidgets {vBoxWeichen, vBoxHinzufügenWegstreckeWeichen, vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts, mvarPlanObjekt})   = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- do
        hBoxHinzufügen <- boxPackWidgetNewDefault vBoxHinzufügenWegstreckeWeichen $ hBoxNew False 0
        checkButton <- boxPackWidgetNewDefault hBoxHinzufügen checkButtonNew
        boxPackWidgetNewDefault hBoxHinzufügen $ labelNew $ Just $ getName weiche
        richtungsRadioButtons <- do
            let (h:|t) = getRichtungen weiche
            hRichtungRadioButton <- boxPackWidgetNewDefault hBoxHinzufügen (radioButtonNewWithLabel $ show h) >>= \radioButton -> pure (h, radioButton)
            tRichtungRadioButtons <- mapM (\richtung -> boxPackWidgetNewDefault hBoxHinzufügen (radioButtonNewWithLabelFromWidget (snd hRichtungRadioButton) $ show richtung) >>= \radioButton -> pure (richtung, radioButton)) t
            pure $ hRichtungRadioButton :| tRichtungRadioButtons
        pure (hBoxHinzufügen, unregistriert checkButton, richtungsRadioButtons)
    hinzufügenPlanWidgetGerade <- if hatRichtung weiche Gerade then pure Nothing else hinzufügenWidgetPlanNew weiche vBoxHinzufügenPlanWeichenGerade OWeiche mvarPlanObjekt >>= pure . Just
    hinzufügenPlanWidgetKurve <- if hatRichtung weiche Kurve then pure Nothing else hinzufügenWidgetPlanNew weiche vBoxHinzufügenPlanWeichenKurve OWeiche mvarPlanObjekt >>= pure . Just
    hinzufügenPlanWidgetLinks <- if hatRichtung weiche Links then pure Nothing else hinzufügenWidgetPlanNew weiche vBoxHinzufügenPlanWeichenLinks OWeiche mvarPlanObjekt >>= pure . Just
    hinzufügenPlanWidgetRechts <- if hatRichtung weiche Rechts then pure Nothing else hinzufügenWidgetPlanNew weiche vBoxHinzufügenPlanWeichenRechts OWeiche mvarPlanObjekt >>= pure . Just
    let hinzufügenPlanWidget = (hinzufügenPlanWidgetGerade, hinzufügenPlanWidgetKurve, hinzufügenPlanWidgetLinks, hinzufügenPlanWidgetRechts)
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxWeichen $ hBoxNew False 0
    nameLabelPackNew hBox weiche
    richtungsButtonsNew weiche hBox
    let weWidgets = WEWidgets {we=weiche, weWidget=hBox, weHinzPL=hinzufügenPlanWidget, weHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxWeichen (entfernenWeiche weWidgets >> liftIO (containerRemove vBoxHinzufügenWegstreckeWeichen ((\(w,_,_) -> w) hinzufügenWegstreckeWidget) >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts] <*> ZipList (weWidgets ^.. foldPlan)))) mvarStatus
    -- Widgets merken
    runMVarBefehl (Hinzufügen $ OWeiche weWidgets) mvarStatus
    pure hBox
        where
            richtungsButtonsNew :: (BoxClass b) => Weiche -> b -> IO ()
            richtungsButtonsNew (LegoWeiche {richtungsPin, richtungen=(richtung1, richtung2)})    box = void $ do
                boxPackWidgetNewDefault box $ pinLabelNew Language.richtung richtungsPin
                boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung1) $ runMVarAktion (Stellen weiche richtung1) mvarStatus
                boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung2) $ runMVarAktion (Stellen weiche richtung2) mvarStatus
            richtungsButtonsNew (MärklinWeiche {richtungsPins})                                     box = mapM_ (\(richtung, pin) -> boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung <:> showText pin) $ runMVarAktion (Stellen weiche richtung) mvarStatus) richtungsPins
type WeicheWidget = HBox
type WeicheWidgetHinzufügenWegstrecke = (HBox, VRCheckButton, NonEmpty (Richtung, RadioButton))
type WeicheWidgetHinzufügenPlan = (Maybe Button, Maybe Button, Maybe Button, Maybe Button)
data WEWidgets = WEWidgets {
                        we :: Weiche,
                        weWidget :: WeicheWidget,
                        weHinzPL :: WeicheWidgetHinzufügenPlan,
                        weHinzWS :: WeicheWidgetHinzufügenWegstrecke}
                                                            deriving (Eq)

getterRichtungsRadioButtons :: Getter WEWidgets (NonEmpty (Richtung, RadioButton))
getterRichtungsRadioButtons = Lens.to $ \weWidgets -> (weHinzWS weWidgets) ^. _3

instance WegstreckenElement WEWidgets where
    lensWegstrecke :: Lens' WEWidgets VRCheckButton
    lensWegstrecke = (Lens.lens weHinzWS (\we v -> we {weHinzWS=v})) . _2

instance PlanElement WEWidgets where
    foldPlan :: Fold WEWidgets (Maybe Button)
    foldPlan = Lens.folding $ \(WEWidgets {weHinzPL=(a, b, c, d)}) -> [a, b, c, d]

instance StreckenObjekt WEWidgets where
    zugtyp :: WEWidgets -> Zugtyp
    zugtyp  (WEWidgets {we})    = zugtyp we
    pins :: WEWidgets -> [Pin]
    pins    (WEWidgets {we})    = pins we
    getName :: WEWidgets -> Text
    getName (WEWidgets {we})    = getName we

instance ToJSON WEWidgets where
    toJSON :: WEWidgets -> Value
    toJSON  (WEWidgets {we})    = toJSON we

instance WeicheKlasse WEWidgets where
    stellen :: WEWidgets -> Richtung -> PinMapIO ()
    stellen (WEWidgets {we})    = stellen we
    getRichtungen :: WEWidgets -> NonEmpty Richtung
    getRichtungen   (WEWidgets {we})    = getRichtungen we

-- | Kuppung darstellen
kupplungPackNew :: (LikeMVar lmvar) => Kupplung -> lmvar StatusGUI -> DynamischeWidgets -> IO KupplungWidget
kupplungPackNew kupplung@(Kupplung {kupplungsPin}) mvarStatus (DynamischeWidgets {vBoxKupplungen, vBoxHinzufügenWegstreckeKupplungen, vBoxHinzufügenPlanKupplungen, mvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetSimpleNew kupplung vBoxHinzufügenWegstreckeKupplungen
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew kupplung vBoxHinzufügenPlanKupplungen OKupplung mvarPlanObjekt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxKupplungen $ hBoxNew False 0
    nameLabelPackNew hBox kupplung
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.kupplung kupplungsPin
    buttonKuppelnPackNew hBox kupplung mvarStatus
    let kuWidgets = KUWidgets {ku=kupplung, kuWidget=hBox, kuHinzPL=hinzufügenPlanWidget, kuHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxKupplungen (entfernenKupplung kuWidgets >> liftIO (containerRemove vBoxHinzufügenWegstreckeKupplungen (fst hinzufügenWegstreckeWidget) >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanKupplungen] <*> ZipList (kuWidgets ^.. foldPlan)))) mvarStatus
    -- Widgets merken
    runMVarBefehl (Hinzufügen $ OKupplung kuWidgets) mvarStatus
    pure hBox
type KupplungWidget = HBox
type KupplungWidgetHinzufügenWegstrecke = (HBox, VRCheckButton)
type KupplungWidgetHinzufügenPlan = Button
data KUWidgets = KUWidgets {
                        ku :: Kupplung,
                        kuWidget :: KupplungWidget,
                        kuHinzPL :: KupplungWidgetHinzufügenPlan,
                        kuHinzWS :: KupplungWidgetHinzufügenWegstrecke}
                                                                deriving (Eq)

instance WegstreckenElement KUWidgets where
    lensWegstrecke :: Lens' KUWidgets VRCheckButton
    lensWegstrecke = (Lens.lens kuHinzWS (\ku v -> ku {kuHinzWS=v})) . _2

instance PlanElement KUWidgets where
    foldPlan :: Fold KUWidgets (Maybe Button)
    foldPlan = Lens.folding $ (:[]) . Just . kuHinzPL

instance StreckenObjekt KUWidgets where
    zugtyp :: KUWidgets -> Zugtyp
    zugtyp  (KUWidgets {ku})    = zugtyp ku
    pins :: KUWidgets -> [Pin]
    pins    (KUWidgets {ku})    = pins ku
    getName :: KUWidgets -> Text
    getName (KUWidgets {ku})    = getName ku

instance ToJSON KUWidgets where
    toJSON :: KUWidgets -> Value
    toJSON  (KUWidgets {ku})    = toJSON ku

instance KupplungKlasse KUWidgets where
    kuppeln :: KUWidgets -> PinMapIO ()
    kuppeln (KUWidgets {ku})    = kuppeln ku

buttonKuppelnPackNew :: (BoxClass b, KupplungKlasse k, LikeMVar lmvar) => b -> k -> lmvar StatusGUI -> IO Button
buttonKuppelnPackNew box kupplung mvarStatus = boxPackWidgetNewDefault box $ buttonNewWithEventLabel Language.kuppeln $ runMVarAktion (Kuppeln kupplung) mvarStatus

-- | Wegstrecke darstellen
wegstreckePackNew :: (LikeMVar lmvar) => Wegstrecke -> lmvar StatusGUI -> DynamischeWidgets -> IO WegstreckeWidget
wegstreckePackNew wegstrecke@(Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen}) mvarStatus (DynamischeWidgets {vBoxWegstrecken, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung, mvarPlanObjekt}) = do
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenPlanWidgetBG <- if null wsBahngeschwindigkeiten then pure Nothing else hinzufügenWidgetPlanNew wegstrecke vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit OWegstrecke mvarPlanObjekt >>= pure . Just
    hinzufügenPlanWidgetST <- if null wsStreckenabschnitte then pure Nothing else hinzufügenWidgetPlanNew wegstrecke vBoxHinzufügenPlanWegstreckenStreckenabschnitt OWegstrecke mvarPlanObjekt >>= pure . Just
    hinzufügenPlanWidgetWE <- if null wsWeichenRichtungen then pure Nothing else hinzufügenWidgetPlanNew wegstrecke vBoxHinzufügenPlanWegstreckenWeiche OWegstrecke mvarPlanObjekt >>= pure . Just
    hinzufügenPlanWidgetKU <- if null wsKupplungen then pure Nothing else hinzufügenWidgetPlanNew wegstrecke vBoxHinzufügenPlanWegstreckenKupplung OWegstrecke mvarPlanObjekt >>= pure . Just
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
        hScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew functionBox wegstrecke mvarStatus
        buttonUmdrehenPackNew functionBox wegstrecke hScaleGeschwindigkeit mvarStatus
    unless (null wsStreckenabschnitte) $ void $ do
        boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.streckenabschnitte <:> foldl appendName ("") wsStreckenabschnitte
        toggleButtonStromPackNew functionBox wegstrecke mvarStatus
    unless (null wsWeichenRichtungen) $ void $ do
        boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.weichen <:> foldl (\acc (weiche, richtung) -> appendName acc weiche <°> showText richtung) ("") wsWeichenRichtungen
        boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.einstellen $ runMVarAktion (Einstellen wegstrecke) mvarStatus
    unless (null wsKupplungen) $ void $ do
        boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.kupplungen <:> foldl appendName ("") wsKupplungen
        buttonKuppelnPackNew functionBox wegstrecke mvarStatus
    let wsWidgets = WSWidgets {ws=wegstrecke, wsWidget=frame, wsHinzPL=hinzufügenPlanWidget}
    buttonEntfernenPack functionBox (containerRemove vBoxWegstrecken frame >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung] <*> ZipList (wsWidgets ^.. foldPlan))) (entfernenWegstrecke wsWidgets) mvarStatus
    -- Widgets merken
    runMVarBefehl (Hinzufügen $ OWegstrecke wsWidgets) mvarStatus
    pure frame
        where
            appendName :: (StreckenObjekt o) => Text -> o -> Text
            appendName ("")     objekt = getName objekt
            appendName string   objekt = string <^> getName objekt
type WegstreckeWidget = Frame
type WegstreckeWidgetHinzufügenPlan = (Maybe Button, Maybe Button, Maybe Button, Maybe Button)
data WSWidgets = WSWidgets {
                        ws :: Wegstrecke,
                        wsWidget :: WegstreckeWidget,
                        wsHinzPL :: WegstreckeWidgetHinzufügenPlan}
                                                            deriving (Eq)

instance PlanElement WSWidgets where
    foldPlan :: Fold WSWidgets (Maybe Button)
    foldPlan = Lens.folding $ \(WSWidgets {wsHinzPL=(a, b, c, d)}) -> [a, b, c, d]

instance StreckenObjekt WSWidgets where
    zugtyp :: WSWidgets -> Zugtyp
    zugtyp  (WSWidgets {ws})    = zugtyp ws
    pins :: WSWidgets -> [Pin]
    pins    (WSWidgets {ws})    = pins ws
    getName :: WSWidgets -> Text
    getName (WSWidgets {ws})    = getName ws

instance ToJSON WSWidgets where
    toJSON :: WSWidgets -> Value
    toJSON  (WSWidgets {ws})    = toJSON ws

instance BahngeschwindigkeitKlasse WSWidgets where
    geschwindigkeit :: WSWidgets -> Natural -> PinMapIO ()
    geschwindigkeit (WSWidgets {ws})    = geschwindigkeit ws
    umdrehen :: WSWidgets -> Maybe Fahrtrichtung -> PinMapIO ()
    umdrehen    (WSWidgets {ws})    = umdrehen ws

instance StreckenabschnittKlasse WSWidgets where
    strom :: WSWidgets -> Bool -> MVar PinMap -> IO ()
    strom   (WSWidgets {ws})    = strom ws

instance KupplungKlasse WSWidgets where
    kuppeln :: WSWidgets -> PinMapIO ()
    kuppeln (WSWidgets {ws})    = kuppeln ws

instance WegstreckeKlasse WSWidgets where
    einstellen :: WSWidgets -> PinMapIO ()
    einstellen  (WSWidgets {ws})    = einstellen ws

planPackNew :: (LikeMVar lmvar) => Plan -> lmvar StatusGUI -> DynamischeWidgets -> IO PlanWidget
planPackNew plan@(Plan {plAktionen}) mvarStatus (DynamischeWidgets {vBoxPläne, progressBarPlan})= do
    -- Widget erstellen
    frame <- boxPackWidgetNewDefault vBoxPläne $ frameNew
    vBox <- containerAddWidgetNew frame $ vBoxNew False 0
    nameLabelPackNew vBox plan
    expander <- boxPackWidgetNewDefault vBox $ expanderNew $ Language.aktionen <:> show (length plAktionen)
    vBoxExpander <- containerAddWidgetNew expander $ vBoxNew False 0
    mapM_ ((boxPackWidgetNewDefault vBoxExpander) . labelNew . Just . show) plAktionen
    functionBox <- boxPackWidgetNewDefault vBox hButtonBoxNew
    boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.ausführen $ runMVarPlan plan (\wert -> set progressBarPlan [progressBarFraction := (toEnum $ fromIntegral wert) / (toEnum $ length plAktionen)]) mvarStatus
    let alleWidgets = PLWidgets {pl=plan, plWidget=frame}
    buttonEntfernenPack functionBox (containerRemove vBoxPläne frame) (entfernenPlan alleWidgets) mvarStatus
    -- Widgets merken
    runMVarBefehl (Hinzufügen $ OPlan alleWidgets) mvarStatus
    pure frame
type PlanWidget = Frame
data PLWidgets = PLWidgets {
                        pl :: Plan,
                        plWidget :: PlanWidget}
                                        deriving (Eq)

instance StreckenObjekt PLWidgets where
    zugtyp :: PLWidgets -> Zugtyp
    zugtyp  (PLWidgets {pl})    = zugtyp pl
    pins :: PLWidgets -> [Pin]
    pins    (PLWidgets {pl})    = pins pl
    getName :: PLWidgets -> Text
    getName (PLWidgets {pl})    = getName pl

instance ToJSON PLWidgets where
    toJSON :: PLWidgets -> Value
    toJSON  (PLWidgets {pl})    = toJSON pl

instance PlanKlasse PLWidgets where
    ausführenPlan :: PLWidgets -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan   (PLWidgets {pl})    = ausführenPlan pl
#endif