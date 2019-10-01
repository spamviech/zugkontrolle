{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

{-|
Description : Dialoge für GTK-UI.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Dialog () where
#else
module Zug.UI.Gtk.Dialog (
                        -- * Knöpfe mit zugehörigem Dialog erstellen
                        buttonSpeichernPack, buttonLadenPack, ladeWidgets, buttonHinzufügenPack) where

-- Bibliotheken
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TMVar, readTMVar, takeTMVar, putTMVar,
                                TVar, newTVarIO, readTVarIO, writeTVar, modifyTVar)
import Control.Lens ((^.))
import Control.Monad (void, when, foldM, forM_)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans (MonadIO(..), lift)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybe, fromJust)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Graphics.UI.Gtk (Box, HBox, VBox, BoxClass(), Window, WindowClass(),
                        Button, CheckButton, SpinButton, RadioButton,
                        Expander, Label, Entry, ComboBox,
                        Dialog, FileChooserDialog, MessageDialog, DialogClass(),
                        ResponseId(..), FileChooserAction(..),
                        AttrOp(..), set, get, on, widgetGetParent,
                        windowTitle, containerRemove, widgetShow, widgetHide,
                        fileChooserGetFilename, fileChooserDialogNew, fileChooserDoOverwriteConfirmation,
                        messageDialogNew, MessageType(..), ButtonsType(..), dialogRun,
                        comboBoxActive, entryText, widgetHasFocus, widgetSensitive, spinButtonValue,
                        toggleButtonActive, windowTransientFor, windowDefaultHeight,
                        dialogNew, dialogAddButton, dialogAddActionWidget, castToBox,
                        comboBoxNewText, comboBoxAppendText, vBoxNew, hBoxNew, labelNew,
                        radioButtonNewWithLabel, radioButtonNewWithLabelFromWidget,
                        changed, checkButtonNew, widgetVisible, Packing(..), expanderNew,
                        notebookNew, windowNew, windowModal, deleteEvent, spinButtonNewWithRange,
                        hScaleNewWithRange, postGUIAsync, rangeValue,
                        widgetExpand, comboBoxSetActive, expanderLabel, dialogGetActionArea)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes, zeigeErstes)
import Zug.Klassen (Zugtyp(..), Richtung(..), unterstützteRichtungen, Strom(..), Fahrtrichtung(..))
import Zug.Anbindung (Value(..), Pin(), zuPin, Bahngeschwindigkeit(..), Streckenabschnitt(..),
                    Weiche(..), Kupplung(..), Wegstrecke(..), StreckenObjekt(..))
import Zug.Plan (Plan(..), ObjektAllgemein(..), Aktion(..), AktionWegstrecke(..),
                AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionWeiche(..), AktionKupplung(..))
import qualified Zug.Language as Language
import Zug.Language (showText, (<!>), (<:>), (<^>))
import Zug.UI.Base (Status, auswertenTMVarIOStatus, liftIOFunction,
                    putBahngeschwindigkeiten, bahngeschwindigkeiten,
                    putStreckenabschnitte, streckenabschnitte,
                    putWeichen, weichen,
                    putKupplungen, kupplungen,
                    putWegstrecken, wegstrecken,
                    putPläne, pläne)
import Zug.UI.Befehl (BefehlKlasse(..), BefehlAllgemein(..), ausführenTMVarBefehl)
import Zug.UI.Gtk.Widget (StatusGUI, BefehlGUI, IOStatusGUI, DynamischeWidgets(..), WegstreckenElement(..),
                        BGWidgets(..), STWidgets(..), WEWidgets(..), KUWidgets(..), WSWidgets(..), PLWidgets(..),
                        widgetShowIf, containerAddWidgetNew, boxPackDefault, boxPack,
                        boxPackWidgetNewDefault, boxPackWidgetNew, paddingDefault, positionDefault,
                        buttonNewWithEventMnemonic, buttonNewWithEventLabel, dialogEval,
                        entferneHinzufügenPlanWidgets, bahngeschwindigkeitPackNew, streckenabschnittPackNew,
                        weichePackNew, kupplungPackNew, wegstreckePackNew, planPackNew,
                        getterRichtungsRadioButtons, nameEntryPackNew, pinSpinBoxNew, traversalHinzufügenWegstrecke,
                        scrolledWidgedNotebookAppendPageNew, scrolledWidgetPackNew, scrolledWidgetAddNew)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggled, tmvarCheckButtons, fortfahrenButton,
                                        MitCheckButton(..), fortfahrenWennToggledNew, aktiviereWennToggledTMVar)

-- | Speichern des aktuellen 'StatusGUI'
buttonSpeichernPack :: (BoxClass b) => Window -> b -> TMVar StatusGUI -> IO Button
buttonSpeichernPack windowMain box tmvarStatus = do
    dialogSpeichern <- dialogSpeichernNew windowMain
    boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.speichern $ do
        antwort <- dialogEval dialogSpeichern
        when (antwort == ResponseOk) $ void $ do
            (Just dateipfad) <- fileChooserGetFilename dialogSpeichern
            ausführenTMVarBefehl (Speichern dateipfad :: BefehlGUI) tmvarStatus

dialogSpeichernNew :: Window -> IO FileChooserDialog
dialogSpeichernNew window = do
    fileChooserDialog <- fileChooserDialogNew (Just Language.speichern :: Maybe Text) (Just window) FileChooserActionSave [(Language.speichern, ResponseOk), (Language.abbrechen, ResponseCancel)]
    set fileChooserDialog [fileChooserDoOverwriteConfirmation := True]
    pure fileChooserDialog

-- | Laden eines neuen 'StatusGUI' aus einer Datei
buttonLadenPack :: (BoxClass b) => Window -> b -> TMVar StatusGUI -> DynamischeWidgets -> IO Button
buttonLadenPack windowMain box tmvarStatus dynamischeWidgets = do
    dialogLaden <- dialogLadenNew windowMain
    dialogLadenFehler <- dialogLadenFehlerNew windowMain
    boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.laden $ do
        antwort <- dialogEval dialogLaden
        when (antwort == ResponseOk) $ void $ do
            fileChooserGetFilename dialogLaden >>= \case
                (Nothing)           -> void $ set dialogLadenFehler [windowTitle := (Language.nichtGefundeneDatei :: Text)] >> dialogEval dialogLadenFehler
                (Just dateipfad)    -> void $ do
                    statusInitial <- atomically $ readTMVar tmvarStatus
                    -- neuer Status ist schon in tmvarStatus gespeichert und muss nicht mehr neu gesetzt werden
                    let
                        ladeAktion :: Status -> IO StatusGUI
                        ladeAktion = ladeWidgets tmvarStatus dynamischeWidgets
                        fehlerBehandlung :: IOStatusGUI ()
                        fehlerBehandlung = liftIO $ void $ do
                            set dialogLadenFehler [windowTitle := dateipfad]
                            dialogEval dialogLadenFehler
                    flip State.evalStateT statusInitial $ ausführenBefehl $
                        Laden dateipfad ladeAktion fehlerBehandlung

-- | Passe angezeigte Widgets (inkl. 'StatusGUI' in 'LikeMVar') an reinen 'Status' an.
ladeWidgets :: TMVar StatusGUI -> DynamischeWidgets -> Status -> IO StatusGUI
ladeWidgets tmvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxBahngeschwindigkeiten, vBoxStreckenabschnitte, vBoxWeichen, vBoxKupplungen, vBoxWegstrecken, vBoxPläne}) status = do
    auswertenTMVarIOStatus löscheWidgets tmvarStatus
    erstelleWidgets tmvarStatus status
        where
            löscheWidgets :: IOStatusGUI ()
            löscheWidgets = State.get >>= liftIOFunction (löscheWidgetsAux) >> putBahngeschwindigkeiten [] >> putStreckenabschnitte [] >> putWeichen [] >> putKupplungen [] >> putWegstrecken [] >> putPläne []
            löscheWidgetsAux :: StatusGUI -> IO ()
            löscheWidgetsAux status = do
                mapM_ (\bgWidgets@(BGWidgets {bgWidget=w})  -> containerRemove vBoxBahngeschwindigkeiten w  >> entferneHinzufügenWegstreckeWidgets bgWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets bgWidgets dynamischeWidgets) $ status ^. bahngeschwindigkeiten
                mapM_ (\stWidgets@(STWidgets {stWidget=w})  -> containerRemove vBoxStreckenabschnitte w     >> entferneHinzufügenWegstreckeWidgets stWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets stWidgets dynamischeWidgets) $ status ^. streckenabschnitte
                mapM_ (\weWidgets@(WEWidgets {weWidget=w})  -> containerRemove vBoxWeichen w                >> entferneHinzufügenWegstreckeWidgets weWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets weWidgets dynamischeWidgets) $ status ^. weichen
                mapM_ (\kuWidgets@(KUWidgets {kuWidget=w})  -> containerRemove vBoxKupplungen w             >> entferneHinzufügenWegstreckeWidgets kuWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets kuWidgets dynamischeWidgets) $ status ^. kupplungen
                mapM_ (\wsWidgets@(WSWidgets {wsWidget=w})  -> containerRemove vBoxWegstrecken w            >> entferneHinzufügenPlanWidgets wsWidgets dynamischeWidgets) $ status ^. wegstrecken
                mapM_ (\(PLWidgets {plWidget=w})            -> containerRemove vBoxPläne w) $ status ^. pläne
            erstelleWidgets :: TMVar StatusGUI -> Status -> IO StatusGUI
            erstelleWidgets tmvarStatus status = do
                mapM_ (\bahngeschwindigkeit -> bahngeschwindigkeitPackNew bahngeschwindigkeit tmvarStatus dynamischeWidgets) $ reverse $ status ^. bahngeschwindigkeiten
                mapM_ (\streckenabschnitt -> streckenabschnittPackNew streckenabschnitt tmvarStatus dynamischeWidgets)       $ reverse $ status ^. streckenabschnitte
                mapM_ (\weiche -> weichePackNew weiche tmvarStatus dynamischeWidgets)                                        $ reverse $ status ^. weichen
                mapM_ (\kupplung -> kupplungPackNew kupplung tmvarStatus dynamischeWidgets)                                  $ reverse $ status ^. kupplungen
                mapM_ (\wegstrecke -> wegstreckePackNew wegstrecke tmvarStatus dynamischeWidgets)                            $ reverse $ status ^. wegstrecken
                mapM_ (\plan -> planPackNew plan tmvarStatus dynamischeWidgets)                                              $ reverse $ status ^. pläne
                atomically $ readTMVar tmvarStatus

dialogLadenNew :: Window -> IO FileChooserDialog
dialogLadenNew window = fileChooserDialogNew (Just Language.laden :: Maybe Text) (Just window) FileChooserActionOpen [(Language.laden, ResponseOk), (Language.abbrechen, ResponseCancel)]

dialogLadenFehlerNew :: Window -> IO MessageDialog
dialogLadenFehlerNew window = messageDialogNew (Just window) [] MessageError ButtonsOk (Language.nichtGefundeneDatei <!> "" :: Text)

-- | Hinzufügen eines 'StreckenObjekt'
buttonHinzufügenPack :: (WindowClass w, BoxClass b) => w -> b -> TMVar StatusGUI -> DynamischeWidgets -> IO Button
buttonHinzufügenPack parentWindow box tmvarStatus dynamischeWidgets = do
    dialogHinzufügen@(DialogHinzufügen {dialog}) <- dialogHinzufügenNew parentWindow dynamischeWidgets
    button <- liftIO $ boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.hinzufügen $ do
        widgetShow dialog
        runPage 0 dialogHinzufügen tmvarStatus dynamischeWidgets
    pure button
        where
            runPage :: Natural -> DialogHinzufügen -> TMVar StatusGUI -> DynamischeWidgets -> IO ()
            runPage
                i
                dialogHinzufügen@(DialogHinzufügen {dialog, pages, buttonWeiter, buttonZurück})
                tmvarStatus
                dynamischeWidgets
                    = do
                        showNth i pages >>= \case
                            (Nothing)   -> error $ "Seite beim Hinzufügen nicht gefunden: " ++ show i
                            (Just page) -> do
                                hinzufügenAktivieren dialogHinzufügen page
                                (if istLetzteSeite page then widgetHide else widgetShow) buttonWeiter
                                (if istErsteSeite page then widgetHide else widgetShow) buttonZurück
                                reset page
                                optionenAnzeigen page tmvarStatus
                                dialogRun dialog >>= \case
                                    ResponseOk
                                        -> do
                                            widgetHide dialog
                                            objektHinzufügen dialogHinzufügen page tmvarStatus dynamischeWidgets
                                    ResponseApply
                                        -> zeigeNächsteSeite page dialogHinzufügen tmvarStatus dynamischeWidgets
                                    ResponseReject
                                        -> zeigeVorherigeSeite page dialogHinzufügen tmvarStatus dynamischeWidgets
                                    -- erwartete Rückgabewerte: ResponseCancel, ResponseDeleteEvent -> Breche Hinzufügen ab
                                    _response
                                        -> widgetHide dialog
            istErsteSeite :: PageHinzufügen -> Bool
            istErsteSeite   (PageStart {})  = True
            istErsteSeite   _               = False
            istLetzteSeite :: PageHinzufügen -> Bool
            istLetzteSeite  (PageStart {})  = False
            istLetzteSeite  _               = True
            hinzufügenAktivieren :: DialogHinzufügen -> PageHinzufügen -> IO ()
            hinzufügenAktivieren
                (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan, comboBoxZugtyp, indizesZugtyp})
                (PageWeiche {})
                    = do
                        widgetHide buttonHinzufügen
                        index <- get comboBoxZugtyp comboBoxActive
                        widgetShowIf (indexStimmtÜberein indizesZugtyp Märklin index) buttonHinzufügenWeicheMärklin
                        widgetShowIf (indexStimmtÜberein indizesZugtyp Lego index) buttonHinzufügenWeicheLego
                        widgetHide buttonHinzufügenWegstrecke
                        widgetHide buttonHinzufügenPlan
            hinzufügenAktivieren
                (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan})
                (PageWegstrecke {})
                    = do
                        widgetHide buttonHinzufügen
                        widgetHide buttonHinzufügenWeicheMärklin
                        widgetHide buttonHinzufügenWeicheLego
                        widgetShow buttonHinzufügenWegstrecke
                        widgetHide buttonHinzufügenPlan
            hinzufügenAktivieren
                (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan})
                (PagePlan {})
                    = do
                        widgetHide buttonHinzufügen
                        widgetHide buttonHinzufügenWeicheMärklin
                        widgetHide buttonHinzufügenWeicheLego
                        widgetHide buttonHinzufügenWegstrecke
                        widgetShow buttonHinzufügenPlan
            hinzufügenAktivieren
                (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan})
                (PageStart {})
                    = do
                        widgetHide buttonHinzufügen
                        widgetHide buttonHinzufügenWeicheMärklin
                        widgetHide buttonHinzufügenWeicheLego
                        widgetHide buttonHinzufügenWegstrecke
                        widgetHide buttonHinzufügenPlan
            hinzufügenAktivieren
                (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan})
                _page
                    = do
                        widgetShow buttonHinzufügen
                        widgetHide buttonHinzufügenWeicheMärklin
                        widgetHide buttonHinzufügenWeicheLego
                        widgetHide buttonHinzufügenWegstrecke
                        widgetHide buttonHinzufügenPlan
            indexStimmtÜberein :: (Eq a) => NonEmpty (Int, a) -> a -> Int -> Bool
            indexStimmtÜberein ne a index = foldr (indexStimmtÜbereinAux a index) False ne
                where
                    indexStimmtÜbereinAux :: (Eq a) => a -> Int -> (Int, a) -> Bool -> Bool
                    indexStimmtÜbereinAux   _a  _index  _current            True
                        = True
                    indexStimmtÜbereinAux   a   index   (index1, a1)        False
                        = (index == index1) && (a == a1)
            reset :: PageHinzufügen -> IO ()
            reset page = do
                    -- Name zurücksetzten
                    case page of
                        (PageStart {})
                            -> pure ()
                        _page
                            -> set (nameEntry page) [entryText := ("" :: Text), widgetHasFocus := True]
                    case page of
                        (PageWegstrecke {wegstreckenElemente})
                            -> do
                                -- Hinzufügen-Sensitivity neu setzen
                                aktiviereWennToggledTMVar wegstreckenElemente traversalHinzufügenWegstrecke
                        (PagePlan {tvarAktionen, vBoxAktionen, expanderAktionen, tvarLabelAktionen, pgButtonHinzufügenPlan})
                            -> do
                                -- Aktionen zurücksetzen
                                atomically $ writeTVar tvarAktionen leer
                                showAktionen pgButtonHinzufügenPlan vBoxAktionen expanderAktionen tvarLabelAktionen leer
                        _page
                            -> pure ()
            optionenAnzeigen :: PageHinzufügen -> TMVar StatusGUI -> IO ()
            optionenAnzeigen
                (PagePlan {bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen})
                tmvarStatus
                    = do
                        -- Zeige nur verfügbare Aktionen an
                        status <- atomically $ readTMVar tmvarStatus
                        widgetShowIf (not $ null (status ^. bahngeschwindigkeiten) && null (status ^. wegstrecken)) bgFunktionen
                        widgetShowIf (not $ null (status ^. streckenabschnitte) && null (status ^. wegstrecken)) stFunktionen
                        widgetShowIf (not $ null (status ^. weichen)) weFunktionen
                        widgetShowIf (not $ null (status ^. kupplungen) && null (status ^. wegstrecken)) kuFunktionen
                        widgetShowIf (not $ null (status ^. wegstrecken) && null (status ^. wegstrecken)) wsFunktionen
            optionenAnzeigen
                _page
                _mvarStatus
                    = pure ()
            zeigeNächsteSeite :: PageHinzufügen -> DialogHinzufügen -> TMVar StatusGUI -> DynamischeWidgets -> IO ()
            zeigeNächsteSeite
                (PageStart {radioButtons})
                dialogHinzufügen
                tmvarStatus
                dynamischeWidgets
                    = do
                        pageNr <- erhalteToggledIndex radioButtons
                        runPage (succ pageNr) dialogHinzufügen tmvarStatus dynamischeWidgets
            zeigeNächsteSeite
                _page
                (DialogHinzufügen {buttonWeiter})
                _mvarStatus
                _dynamischeWidgets
                    = widgetHide buttonWeiter
            zeigeVorherigeSeite :: PageHinzufügen -> DialogHinzufügen -> TMVar StatusGUI -> DynamischeWidgets -> IO ()
            zeigeVorherigeSeite _page dialogHinzufügen = runPage 0 dialogHinzufügen
            erhalteFließendValue :: DialogHinzufügen -> IO Value
            erhalteFließendValue (DialogHinzufügen {comboBoxFließend, indizesFließend})
                = get comboBoxFließend comboBoxActive >>= \case
                    index
                        | indexStimmtÜberein indizesFließend HIGH index
                            -> pure HIGH
                        | indexStimmtÜberein indizesFließend LOW index
                            -> pure LOW
                        | otherwise
                            -> error "Unbekannter Fließend-Value ausgewählt."
            objektHinzufügen :: DialogHinzufügen -> PageHinzufügen -> TMVar StatusGUI -> DynamischeWidgets -> IO ()
            objektHinzufügen
                dialogHinzufügen@(DialogHinzufügen {comboBoxZugtyp, indizesZugtyp})
                (PageBahngeschwindigkeit {nameEntry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton})
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        bgName <- get nameEntry entryText
                        bgFließend <- erhalteFließendValue dialogHinzufügen
                        geschwindigkeitsPin <- get geschwindigkeitsPinSpinButton spinButtonValue >>= pure . zuPin
                        bahngeschwindigkeit <- get comboBoxZugtyp comboBoxActive >>= \case
                            index
                                | indexStimmtÜberein indizesZugtyp Märklin index
                                    -> pure MärklinBahngeschwindigkeit {bgName, bgFließend, geschwindigkeitsPin}
                                | indexStimmtÜberein indizesZugtyp Lego index
                                    -> do
                                        fahrtrichtungsPin <- get fahrtrichtungsPinSpinButton spinButtonValue >>= pure . zuPin
                                        pure LegoBahngeschwindigkeit {bgName, bgFließend, geschwindigkeitsPin, fahrtrichtungsPin}
                                | otherwise
                                    -> error $ "Unbekannter Zugtyp beim Hinzufügen einer Bahngeschwindigkeit ausgewählt: " ++ show index
                        bahngeschwindigkeitPackNew bahngeschwindigkeit tmvarStatus dynamischeWidgets
            objektHinzufügen
                dialogHinzufügen
                (PageStreckenabschnitt {nameEntry, stromPinSpinButton})
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        stName <- get nameEntry entryText
                        stFließend <- erhalteFließendValue dialogHinzufügen
                        stromPin <- get stromPinSpinButton spinButtonValue >>= pure . zuPin
                        streckenabschnittPackNew (Streckenabschnitt {stName, stFließend, stromPin}) tmvarStatus dynamischeWidgets
            objektHinzufügen
                dialogHinzufügen@(DialogHinzufügen {comboBoxZugtyp, indizesZugtyp})
                (PageWeiche {nameEntry, richtungsWidgetsMärklin, richtungsWidgetsLego})
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        weName <- get nameEntry entryText
                        weFließend <- erhalteFließendValue dialogHinzufügen
                        weiche <- get comboBoxZugtyp comboBoxActive >>= \case
                            index
                                | indexStimmtÜberein indizesZugtyp Märklin index
                                    -> do
                                        let
                                            erhalteToggledRichtungen :: [(Richtung, Pin)] -> (Richtung, CheckButton, SpinButton) -> IO [(Richtung, Pin)]
                                            erhalteToggledRichtungen acc (richtung, checkButton, spinButton) = do
                                                get checkButton toggleButtonActive >>= \case
                                                    True    -> do
                                                        pin <- get spinButton spinButtonValue
                                                        pure $ (richtung, zuPin pin) : acc
                                                    False   -> pure acc
                                        richtungsPins <- foldM erhalteToggledRichtungen [] richtungsWidgetsMärklin
                                        case richtungsPins of
                                            ([])    -> error "Keine Richtung beim Hinzufügen einer Märklin-Weiche ausgewählt."
                                            (h:t)   -> pure MärklinWeiche {weName, weFließend, richtungsPins=h:|t}
                                | indexStimmtÜberein indizesZugtyp Lego index
                                    -> do
                                        let (richtungsPinSpinButton, richtungenRadioButtons) = richtungsWidgetsLego
                                        richtungsPin <- get richtungsPinSpinButton spinButtonValue >>= pure . zuPin
                                        let
                                            erhalteGewählteRichtungen :: (Richtung, Richtung) -> (Richtung, Richtung, RadioButton) -> IO (Richtung, Richtung)
                                            erhalteGewählteRichtungen acc (r1, r2, rb) = do
                                                toggled <- get rb toggleButtonActive
                                                pure $ if toggled then (r1, r2) else acc
                                        richtungen <- foldM erhalteGewählteRichtungen (Gerade, Gerade) richtungenRadioButtons
                                        pure LegoWeiche {weName, weFließend, richtungsPin, richtungen}
                                | otherwise
                                    -> error $ "Unbekannter Zugtyp beim Hinzufügen einer Weiche ausgewählt: " ++ show index
                        weichePackNew weiche tmvarStatus dynamischeWidgets
            objektHinzufügen
                dialogHinzufügen
                (PageKupplung {nameEntry, kupplungsPinSpinButton})
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        kuName <- get nameEntry entryText
                        kuFließend <- erhalteFließendValue dialogHinzufügen
                        kupplungsPin <- get kupplungsPinSpinButton spinButtonValue >>= pure . zuPin
                        kupplungPackNew (Kupplung {kuName, kuFließend, kupplungsPin}) tmvarStatus dynamischeWidgets
            objektHinzufügen
                _dialogHinzufügen
                (PageWegstrecke {nameEntry, wegstreckenElemente})
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        wsName <- get nameEntry entryText
                        wegstreckenElementeCurrent <- atomically $ readTMVar $ wegstreckenElemente ^. tmvarCheckButtons
                        wsBahngeschwindigkeiten <- foldM (getToggledWegstreckenElemente $ pure . bg) [] $ wegstreckenElementeCurrent ^. bahngeschwindigkeiten
                        wsStreckenabschnitte <- foldM (getToggledWegstreckenElemente $ pure . st) [] $  wegstreckenElementeCurrent ^. streckenabschnitte
                        wsWeichenRichtungen <- foldM (getToggledWegstreckenElemente getWeichenRichtung) [] $ wegstreckenElementeCurrent ^. weichen
                        wsKupplungen <- foldM (getToggledWegstreckenElemente $ pure . ku) [] $ wegstreckenElementeCurrent ^. kupplungen
                        wegstreckePackNew (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen}) tmvarStatus dynamischeWidgets
                            where
                                getToggledWegstreckenElemente :: (WegstreckenElement s) => (s -> IO a) -> [a] -> s -> IO [a]
                                getToggledWegstreckenElemente
                                    selector
                                    acc
                                    element
                                        = do
                                            get (element ^. lensWegstrecke . checkButton) toggleButtonActive >>= \case
                                                True    -> do
                                                    h <- selector element
                                                    pure $ h : acc
                                                False   -> pure acc
                                getWeichenRichtung :: WEWidgets -> IO (Weiche, Richtung)
                                getWeichenRichtung weWidgets = do
                                    richtung <- getToggledRichtung (weWidgets ^. getterRichtungsRadioButtons)
                                    pure (we weWidgets, richtung)
                                getToggledRichtung :: NonEmpty (Richtung, RadioButton) -> IO Richtung
                                getToggledRichtung  ((richtung, radioButton):|tail) = do
                                    toggled <- get radioButton toggleButtonActive
                                    if toggled
                                        then pure richtung
                                        else case tail of
                                            (h:t)   -> getToggledRichtung $ h:|t
                                            []      -> error "getToggledRichtung ohne ausgewählten RadioButton aufgerufen."
            objektHinzufügen
                _dialogHinzufügen
                (PagePlan {nameEntry, tvarAktionen})
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        plName <- get nameEntry entryText
                        aktionenQueue <- readTVarIO tvarAktionen
                        planPackNew (Plan {plName, plAktionen=toList aktionenQueue}) tmvarStatus dynamischeWidgets
            objektHinzufügen
                _dialogHinzufügen
                page
                _mvarStatus
                _dynamischeWidgets
                    = error $ "Unbekannte Seite während dem Hinzufügen angezeigt: " ++ show page

dialogHinzufügenNew :: (WindowClass w) => w -> DynamischeWidgets -> IO DialogHinzufügen
dialogHinzufügenNew
    parent
    (DynamischeWidgets {vBoxHinzufügenWegstreckeBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeitenLego, vBoxHinzufügenPlanBahngeschwindigkeitenMärklin, vBoxHinzufügenWegstreckeStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte, vBoxHinzufügenWegstreckeWeichen, vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts, vBoxHinzufügenWegstreckeKupplungen, vBoxHinzufügenPlanKupplungen, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung, fortfahrenWennToggledWegstrecke, tmvarPlanObjekt})
        = do
            dialog <- dialogNew
            set dialog [
                windowTitle := (Language.hinzufügen :: Text),
                windowTransientFor := parent,
                windowDefaultHeight := 320]
            -- Eigene Hinzufügen-Knöpfe für Seiten, bei denen er temporär deaktiert sein kann
            buttonHinzufügen <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
            buttonHinzufügenWeicheMärklin <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
            buttonHinzufügenWeicheLego <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
            let buttonHinzufügenWegstrecke = fortfahrenWennToggledWegstrecke ^. fortfahrenButton
            dialogAddActionWidget dialog buttonHinzufügenWegstrecke ResponseOk
            buttonHinzufügenPlan <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
            -- ComboBox zur Zugtyp-Auswahl
            buttonBox <- widgetGetParent buttonHinzufügen >>= pure . castToBox . fromJust
            comboBoxFließend <- boxPackWidgetNewDefault buttonBox comboBoxNewText
            indexHigh <- comboBoxAppendText comboBoxFließend Language.high
            indexLow <- comboBoxAppendText comboBoxFließend Language.low
            let indizesFließend = (indexHigh, HIGH) :| (indexLow, LOW) : []
            comboBoxZugtyp <- boxPackWidgetNewDefault buttonBox comboBoxNewText
            indexMärklin <- comboBoxAppendText comboBoxZugtyp Language.märklin
            indexLego <-comboBoxAppendText comboBoxZugtyp Language.lego
            let indizesZugtyp = (indexMärklin, Märklin) :| (indexLego, Lego) : []
            -- Fluss-Kontrolle des Dialogs
            buttonWeiter <- dialogAddButton dialog (Language.weiter :: Text) ResponseApply
            buttonZurück <- dialogAddButton dialog (Language.zurück :: Text) ResponseReject
            _buttonAbbrechen <- dialogAddButton dialog (Language.abbrechen :: Text) ResponseCancel
            -- Seiten mit Einstellungs-Möglichkeiten
            contentBox <- dialogGetUpper dialog
            pages <- flip State.execStateT leer $ do
                appendPage contentBox $ do
                    vBox <- vBoxNew False 0
                    rbBahngeschwindigkeit   <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabel (Language.bahngeschwindigkeit :: Text)
                    rbStreckenabschnitt     <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.streckenabschnitt :: Text)
                    rbWeiche                <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.weiche :: Text)
                    rbKupplung              <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.kupplung :: Text)
                    rbWegstrecke            <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.wegstrecke :: Text)
                    rbPlan                  <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.plan :: Text)
                    let 
                        radioButtons :: NonEmpty RadioButton
                        radioButtons
                            = rbBahngeschwindigkeit :|
                                rbStreckenabschnitt :
                                rbWeiche :
                                rbKupplung :
                                rbWegstrecke :
                                rbPlan : []
                    pure PageStart {widget=vBox, radioButtons}
                appendPage contentBox $ do
                    -- Bahngeschwindigkeit
                    widget <- vBoxNew False 0
                    boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.bahngeschwindigkeit :: Text)
                    nameEntry <- nameEntryPackNew widget
                    (geschwindigkeitsPinWidget, geschwindigkeitsPinSpinButton) <- pinSpinBoxNew Language.geschwindigkeit
                    boxPackWidgetNewDefault widget $ pure geschwindigkeitsPinWidget
                    -- Zeige Fahrtrichtungs-Pin nicht für Märklin-Bahngeschwindigkeit an
                    (fahrtrichtungsPinWidget, fahrtrichtungsPinSpinButton) <- pinSpinBoxNew Language.fahrtrichtung
                    boxPackWidgetNewDefault widget $ pure fahrtrichtungsPinWidget
                    on comboBoxZugtyp changed $ do
                        index <- get comboBoxZugtyp comboBoxActive
                        widgetShowIf (index == indexLego) fahrtrichtungsPinWidget
                    pure PageBahngeschwindigkeit {widget, nameEntry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton}
                appendPage contentBox $ do
                    -- Streckenabschnitt
                    widget <- vBoxNew False 0
                    boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.streckenabschnitt :: Text)
                    nameEntry <- nameEntryPackNew widget
                    (stromPinWidget, stromPinSpinButton) <- pinSpinBoxNew Language.strom
                    boxPackWidgetNewDefault widget $ pure stromPinWidget
                    pure PageStreckenabschnitt {widget, nameEntry, stromPinSpinButton}
                appendPage contentBox $ do
                    -- Weiche
                    widget <- vBoxNew False 0
                    boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.weiche :: Text)
                    nameEntry <- nameEntryPackNew widget
                    (richtungsPinWidget, richtungsPinSpinButton) <- pinSpinBoxNew Language.richtung
                    boxPackWidgetNewDefault widget $ pure richtungsPinWidget
                    let
                        createRichtungsPin :: Richtung -> IO (Richtung, HBox, CheckButton, SpinButton)
                        createRichtungsPin richtung = do
                            hBox <- boxPackWidgetNewDefault widget $ hBoxNew False 0
                            checkButton <- boxPackWidgetNewDefault hBox checkButtonNew
                            (pinWidget, spinButton) <- pinSpinBoxNew $ showText richtung
                            boxPackWidgetNewDefault hBox $ pure pinWidget
                            pure (richtung, hBox, checkButton, spinButton)
                        createRichtungenRadioButton :: Maybe (NonEmpty (Richtung, Richtung, RadioButton)) -> (Richtung, Richtung) -> IO (Maybe (NonEmpty (Richtung, Richtung, RadioButton)))
                        createRichtungenRadioButton (Nothing)               richtungen@(richtung1, richtung2)   = do
                            radioButton <- boxPackWidgetNewDefault widget $ radioButtonNewWithLabel $ getRichtungenText richtungen
                            pure $ Just $ (richtung1, richtung2, radioButton):|[]
                        createRichtungenRadioButton (Just (h@(_,_,rb):|t))  richtungen@(richtung1, richtung2)   = do
                            radioButton <- boxPackWidgetNewDefault widget $ radioButtonNewWithLabelFromWidget rb $ getRichtungenText richtungen
                            pure $ Just $ (richtung1, richtung2, radioButton):|h:t
                        getRichtungenText :: (Richtung, Richtung) -> Text
                        getRichtungenText (richtung1, richtung2) = showText richtung1 <^> showText richtung2
                    richtungsPins <- mapM createRichtungsPin unterstützteRichtungen
                    richtungsWidgetsMärklin <- fortfahrenWennToggledNew buttonHinzufügenWeicheMärklin $
                        (\(richtung, _hBox, checkButton, spinButton) -> (richtung, checkButton, spinButton)) <$> richtungsPins
                    let
                        richtungsKombinationen :: NonEmpty (Richtung, Richtung)
                        richtungsKombinationen = dreiecksKombinationen unterstützteRichtungen
                        -- Wenn gespiegelte Kombinationen gewünscht werden nutze Applicative-Instanz
                        -- richtungsKombinationen = (,) <&> unterstützteRichtungen <*> unterstützteRichtungen
                        dreiecksKombinationen :: NonEmpty a -> NonEmpty (a, a)
                        dreiecksKombinationen   (h:|[])     = (h, h) :| []
                        dreiecksKombinationen   (h:|s:[])   = (h, s) :| []
                        dreiecksKombinationen   (h:|t)      = let tNE = NE.fromList t in ((,) h <$> tNE) <> dreiecksKombinationen tNE
                    richtungsRadioButtons <- foldM createRichtungenRadioButton Nothing richtungsKombinationen >>= pure . fromJust
                    let richtungsWidgetsLego = (richtungsPinSpinButton, richtungsRadioButtons)
                    on comboBoxZugtyp changed $ do
                        index <- get comboBoxZugtyp comboBoxActive
                        visible <- get widget widgetVisible
                        widgetShowIf (visible && index == indexMärklin) buttonHinzufügenWeicheMärklin
                        mapM_ (\(_,hBox,_,_) -> widgetShowIf (index == indexMärklin) hBox) richtungsPins
                        widgetShowIf (visible && index == indexLego) buttonHinzufügenWeicheLego
                        widgetShowIf (index == indexLego) richtungsPinWidget
                        mapM_ (\(_,_,rb) -> widgetShowIf (index == indexLego) rb) richtungsRadioButtons
                    pure PageWeiche {widget, nameEntry, richtungsWidgetsMärklin, richtungsWidgetsLego}
                appendPage contentBox $ do
                    -- Kupplung
                    widget <- vBoxNew False 0
                    boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.kupplung :: Text)
                    nameEntry <- nameEntryPackNew widget
                    (kupplungPinWidget, kupplungsPinSpinButton) <- pinSpinBoxNew Language.kupplung
                    boxPackWidgetNewDefault widget $ pure kupplungPinWidget
                    pure PageKupplung {widget, nameEntry, kupplungsPinSpinButton}
                appendPage contentBox $ do
                    -- Wegstrecke
                    widget <- vBoxNew False 0
                    boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.wegstrecke :: Text)
                    nameEntry <- nameEntryPackNew widget
                    notebook <- boxPackWidgetNew widget PackGrow paddingDefault positionDefault notebookNew
                    scrolledWidgedNotebookAppendPageNew notebook Language.bahngeschwindigkeiten $ pure vBoxHinzufügenWegstreckeBahngeschwindigkeiten
                    scrolledWidgedNotebookAppendPageNew notebook Language.streckenabschnitte $ pure vBoxHinzufügenWegstreckeStreckenabschnitte
                    scrolledWidgedNotebookAppendPageNew notebook Language.weichen $ pure vBoxHinzufügenWegstreckeWeichen
                    scrolledWidgedNotebookAppendPageNew notebook Language.kupplungen $ pure vBoxHinzufügenWegstreckeKupplungen
                    pure PageWegstrecke {widget, nameEntry, wegstreckenElemente=fortfahrenWennToggledWegstrecke}
                appendPage contentBox $ do
                    -- Plan
                    -- Objekt-Buttons schreiben bei Druck Objekt in tmvarPlanObjekt
                    -- Sobald diese gefüllt ist kann die Aktion zur aktionen-TVar hinzufgefügt werden
                    tvarLabelAktionen <- newTVarIO []
                    tvarAktionen <- newTVarIO leer
                    expanderAktionen <- expanderNew (Language.aktionen :: Text)
                    vBoxAktionen <- vBoxNew False 0
                    let
                        showAktionenSpezifisch :: IO ()
                        showAktionenSpezifisch = do
                            aktionen <- readTVarIO tvarAktionen
                            showAktionen buttonHinzufügenPlan vBoxAktionen expanderAktionen tvarLabelAktionen aktionen
                    -- Hilfsdialog erstellen
                    windowObjekte <- windowNew
                    set windowObjekte [
                        windowTitle := (Language.aktion :: Text),
                        windowModal := True,
                        windowTransientFor := dialog]
                    on windowObjekte deleteEvent $ liftIO $ do
                        atomically $ putTMVar tmvarPlanObjekt Nothing
                        widgetHide windowObjekte
                        pure True
                    windowVBox <- containerAddWidgetNew windowObjekte $ vBoxNew False 0
                    (windowScrolledWindowBGGeschw, windowVBoxBGGeschw)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    (windowScrolledWindowBGUmdrehen, windowVBoxBGUmdrehen)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    (windowScrolledWindowST, windowVBoxST)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    (windowScrolledWindowWEGerade, windowVBoxWEGerade)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    (windowScrolledWindowWEKurve, windowVBoxWEKurve)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    (windowScrolledWindowWELinks, windowVBoxWELinks)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    (windowScrolledWindowWERechts, windowVBoxWERechts)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    (windowScrolledWindowKU, windowVBoxKU)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    (windowScrolledWindowWS, windowVBoxWS)
                        <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
                    boxPackWidgetNewDefault windowVBox $ buttonNewWithEventLabel Language.abbrechen $ do
                        atomically $ putTMVar tmvarPlanObjekt Nothing
                        widgetHide windowObjekte
                    boxPackWidgetNewDefault windowVBoxBGGeschw $ labelNew $
                        Just $ (Language.bahngeschwindigkeiten :: Text)
                    boxPackDefault windowVBoxBGGeschw vBoxHinzufügenPlanBahngeschwindigkeiten
                    boxPackWidgetNewDefault windowVBoxBGGeschw $ labelNew $
                        Just $ (Language.wegstrecken :: Text)
                    boxPackDefault windowVBoxBGGeschw vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit
                    boxPackWidgetNewDefault windowVBoxBGUmdrehen $ labelNew $
                        Just $ (Language.bahngeschwindigkeiten :: Text)
                    boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanBahngeschwindigkeitenLego
                    boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                    boxPackWidgetNewDefault windowVBoxBGUmdrehen $ labelNew $
                        Just $ (Language.wegstrecken :: Text)
                    boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                    boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                    boxPackWidgetNewDefault windowVBoxST $ labelNew $
                        Just $ (Language.streckenabschnitte :: Text)
                    boxPackDefault windowVBoxST vBoxHinzufügenPlanStreckenabschnitte
                    boxPackWidgetNewDefault windowVBoxST $ labelNew $
                        Just $ (Language.wegstrecken :: Text)
                    boxPackDefault windowVBoxST vBoxHinzufügenPlanWegstreckenStreckenabschnitt
                    boxPackWidgetNewDefault windowVBoxWEGerade $ labelNew $
                        Just $ (Language.weichen :: Text)
                    boxPackDefault windowVBoxWEGerade vBoxHinzufügenPlanWeichenGerade
                    boxPackWidgetNewDefault windowVBoxWEKurve $ labelNew $
                        Just $ (Language.weichen :: Text)
                    boxPackDefault windowVBoxWEKurve vBoxHinzufügenPlanWeichenKurve
                    boxPackWidgetNewDefault windowVBoxWELinks $ labelNew $
                        Just $ (Language.weichen :: Text)
                    boxPackDefault windowVBoxWELinks vBoxHinzufügenPlanWeichenLinks
                    boxPackWidgetNewDefault windowVBoxWERechts $ labelNew $
                        Just $ (Language.weichen :: Text)
                    boxPackDefault windowVBoxWERechts vBoxHinzufügenPlanWeichenRechts
                    boxPackWidgetNewDefault windowVBoxKU $ labelNew $
                        Just $ (Language.kupplungen :: Text)
                    boxPackDefault windowVBoxKU vBoxHinzufügenPlanKupplungen
                    boxPackWidgetNewDefault windowVBoxKU $ labelNew $
                        Just $ (Language.wegstrecken :: Text)
                    boxPackDefault windowVBoxKU vBoxHinzufügenPlanWegstreckenKupplung
                    boxPackWidgetNewDefault windowVBoxWS $ labelNew $
                        Just $ (Language.wegstrecken :: Text)
                    boxPackDefault windowVBoxWS vBoxHinzufügenPlanWegstreckenWeiche
                    -- Widget erstellen
                    widget <- vBoxNew False 0
                    boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.plan :: Text)
                    nameEntry <- nameEntryPackNew widget
                    functionBox <- boxPackWidgetNewDefault widget $ hBoxNew False 0
                    let µsInS = 1000000
                    spinButtonZeit <- spinButtonNewWithRange 0 (60*µsInS) (1*µsInS)
                    boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.warten $ do
                        wertDouble <- get spinButtonZeit spinButtonValue
                        atomically $ modifyTVar tvarAktionen $ anhängen $ Warten $ fromIntegral $ fromEnum wertDouble
                        showAktionenSpezifisch
                    boxPackWidgetNewDefault functionBox $ pure spinButtonZeit
                    boxPackWidgetNewDefault functionBox $ labelNew $ Just $ (Language.wartenEinheit :: Text)
                    bgFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
                    hScaleGeschwindigkeit <- hScaleNewWithRange 0 100 1
                    boxPackWidgetNewDefault bgFunktionen $ buttonNewWithEventLabel Language.geschwindigkeit $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetShow windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetHide windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        wertDouble <- get hScaleGeschwindigkeit rangeValue
                        let wert = fromIntegral $ fromEnum wertDouble
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OBahngeschwindigkeit bg))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    ABahngeschwindigkeit $ Geschwindigkeit bg wert
                            (Just (OWegstrecke ws))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWegstrecke $ AWSBahngeschwindigkeit $ Geschwindigkeit ws wert
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    boxPackWidgetNew bgFunktionen PackGrow paddingDefault positionDefault $ pure hScaleGeschwindigkeit
                    vorwärtsRadioButton <- radioButtonNewWithLabel (Language.vorwärts :: Text)
                    rückwärtsRadioButton <- radioButtonNewWithLabelFromWidget vorwärtsRadioButton (Language.rückwärts :: Text)
                    boxPackWidgetNewDefault bgFunktionen $ buttonNewWithEventLabel Language.umdrehen $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetShow windowScrolledWindowBGUmdrehen
                            widgetHide windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OBahngeschwindigkeit bg))
                                -> do
                                    maybeFahrtrichtung <- case zugtyp bg of
                                        Lego    -> do
                                            toggled <- get vorwärtsRadioButton toggleButtonActive
                                            pure $ Just $ if toggled then Vorwärts else Rückwärts
                                        _zugtyp -> pure Nothing
                                    atomically $ modifyTVar tvarAktionen $ anhängen $
                                        ABahngeschwindigkeit $ Umdrehen bg maybeFahrtrichtung
                            (Just (OWegstrecke ws))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWegstrecke $ AWSBahngeschwindigkeit $ Umdrehen ws Nothing
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    fahrtrichtungsVBox <- boxPackWidgetNewDefault bgFunktionen $ vBoxNew False 0
                    on comboBoxZugtyp changed $ do
                        index <- get comboBoxZugtyp comboBoxActive
                        mapM_ (widgetShowIf (index == indexLego)) [
                            vBoxHinzufügenPlanBahngeschwindigkeitenLego,
                            vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
                            fahrtrichtungsVBox]
                        mapM_ (widgetShowIf (index == indexMärklin)) [
                            vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
                            vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin]
                    boxPackWidgetNewDefault fahrtrichtungsVBox $ pure vorwärtsRadioButton
                    boxPackWidgetNewDefault fahrtrichtungsVBox $ pure rückwärtsRadioButton
                    stFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
                    boxPackWidgetNewDefault stFunktionen $ buttonNewWithEventLabel (Language.strom <:> Language.an) $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetShow windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OStreckenabschnitt st))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AStreckenabschnitt $ Strom st Fließend
                            (Just (OWegstrecke ws))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWegstrecke $ AWSStreckenabschnitt $ Strom ws Fließend
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    boxPackWidgetNewDefault stFunktionen $ buttonNewWithEventLabel (Language.strom <:> Language.aus) $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetShow windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        atomically $ case objekt of
                            (Just (OStreckenabschnitt st))
                                -> modifyTVar tvarAktionen $ anhängen $
                                    AStreckenabschnitt $ Strom st Gesperrt
                            (Just (OWegstrecke ws))
                                -> modifyTVar tvarAktionen $ anhängen $
                                    AWegstrecke $ AWSStreckenabschnitt $ Strom ws Gesperrt
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    weFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
                    boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.gerade) $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetHide windowScrolledWindowST
                            widgetShow windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OWeiche we))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWeiche $ Stellen we Gerade
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.kurve) $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetHide windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetShow windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OWeiche we))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWeiche $ Stellen we Kurve
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.links) $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetHide windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetShow windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OWeiche we))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWeiche $ Stellen we Links
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.rechts) $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetHide windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetShow windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OWeiche we))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWeiche $ Stellen we Rechts
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    kuFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
                    boxPackWidgetNewDefault kuFunktionen $ buttonNewWithEventLabel Language.kuppeln $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetHide windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetShow windowScrolledWindowKU
                            widgetHide windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OKupplung ku))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AKupplung $ Kuppeln ku
                            (Just (OWegstrecke ws))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWegstrecke $ AWSKupplung $ Kuppeln ws
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    wsFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
                    boxPackWidgetNewDefault wsFunktionen $ buttonNewWithEventLabel Language.einstellen $ void $ forkIO $ do
                        postGUIAsync $ do
                            widgetShow windowObjekte
                            widgetHide windowScrolledWindowBGGeschw
                            widgetHide windowScrolledWindowBGUmdrehen
                            widgetHide windowScrolledWindowST
                            widgetHide windowScrolledWindowWEGerade
                            widgetHide windowScrolledWindowWEKurve
                            widgetHide windowScrolledWindowWELinks
                            widgetHide windowScrolledWindowWERechts
                            widgetHide windowScrolledWindowKU
                            widgetShow windowScrolledWindowWS
                        objekt <- atomically $ takeTMVar tmvarPlanObjekt
                        case objekt of
                            (Just (OWegstrecke ws))
                                -> atomically $ modifyTVar tvarAktionen $ anhängen $
                                    AWegstrecke $ Einstellen ws
                            _objekt
                                -> pure ()
                        postGUIAsync $ do
                            widgetHide windowObjekte
                            showAktionenSpezifisch
                    boxPackWidgetNew widget PackGrow paddingDefault positionDefault $ pure expanderAktionen
                    scrolledWidgetAddNew expanderAktionen $ widgetShow vBoxAktionen >> pure vBoxAktionen
                    set vBoxAktionen [widgetExpand := True]
                    let
                        entferneLetzteAktion :: Warteschlange Aktion -> Warteschlange Aktion
                        entferneLetzteAktion aktionen = case zeigeLetztes aktionen of
                            Leer            -> leer
                            (Gefüllt _l p)  -> p
                    boxPackWidgetNewDefault widget $ buttonNewWithEventLabel Language.rückgängig $ do
                        atomically $ modifyTVar tvarAktionen entferneLetzteAktion
                        showAktionenSpezifisch
                    showAktionenSpezifisch
                    pure PagePlan {
                        widget,
                        nameEntry,
                        bgFunktionen,
                        stFunktionen,
                        weFunktionen,
                        kuFunktionen,
                        wsFunktionen,
                        tvarAktionen,
                        expanderAktionen,
                        tvarLabelAktionen,
                        vBoxAktionen,
                        pgButtonHinzufügenPlan=buttonHinzufügenPlan}
            -- Setze Wert der ComboBox am Ende um davon abhängige Widgets automatisch zu zeigen/verstecken
            comboBoxSetActive comboBoxFließend indexLow
            comboBoxSetActive comboBoxZugtyp indexMärklin
            pure DialogHinzufügen {dialog, pages, buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan, buttonWeiter, buttonZurück, comboBoxZugtyp, indizesZugtyp, comboBoxFließend, indizesFließend}
                where
                    appendPage :: (BoxClass b, Monad m, MonadIO m) => b -> m PageHinzufügen -> StateT (Warteschlange PageHinzufügen) m ()
                    appendPage box konstruktor = do
                        page <- lift konstruktor
                        liftIO $ boxPack box (widget page) PackGrow paddingDefault positionDefault
                        State.modify $ anhängen page

-- | Zeige 'Aktion'en richtig an.
showAktionen :: (BoxClass b, Foldable t) => Button -> b -> Expander -> TVar [Label] -> t Aktion -> IO ()
showAktionen buttonHinzufügen box expander tvarWidgets aktionen = do
    widgets <- readTVarIO tvarWidgets
    forM_ widgets $ containerRemove box
    widgetsNeu <- mapM (boxPackWidgetNewDefault box . labelNew . Just . show) $ toList aktionen
    set expander [expanderLabel := Language.aktionen <:> show (length aktionen)]
    atomically $ writeTVar tvarWidgets widgetsNeu
    set buttonHinzufügen [widgetSensitive := not $ null aktionen]

-- | Zeige nur die i-te Seite (start bei i=0) an
showNth :: Natural -> Warteschlange PageHinzufügen -> IO (Maybe PageHinzufügen)
showNth i queue = case zeigeErstes queue of
    (Leer)          -> pure Nothing
    (Gefüllt h t)
        | i <= 0    -> do
            widgetShow $ widget h
            forM_ t $ widgetHide . widget
            pure $ Just h
        | otherwise -> do
            widgetHide $ widget h
            showNth (pred i) t

-- | Gebe den Index des ersten eingeschalteten Radiobuttons an. Wenn kein RadioButton an ist, gebe 0 zurück.
erhalteToggledIndex :: NonEmpty RadioButton -> IO Natural
erhalteToggledIndex (h:|t) = do
    toggled <- get h toggleButtonActive
    if toggled
        then pure 0
        else succ <$> (maybe (pure 0) erhalteToggledIndex $ nonEmpty t)

data PageHinzufügen
    = PageStart {
        widget :: VBox,
        radioButtons :: NonEmpty RadioButton}
    | PageBahngeschwindigkeit {
        widget :: VBox,
        nameEntry :: Entry,
        geschwindigkeitsPinSpinButton,
        fahrtrichtungsPinSpinButton :: SpinButton}
    | PageStreckenabschnitt {
        widget :: VBox,
        nameEntry :: Entry,
        stromPinSpinButton :: SpinButton}
    | PageWeiche {
        widget :: VBox,
        nameEntry :: Entry,
        richtungsWidgetsMärklin :: FortfahrenWennToggled NonEmpty (Richtung, CheckButton, SpinButton),
        richtungsWidgetsLego :: (SpinButton, NonEmpty (Richtung, Richtung, RadioButton))}
    | PageKupplung {
        widget :: VBox,
        nameEntry :: Entry,
        kupplungsPinSpinButton :: SpinButton}
    | PageWegstrecke {
        widget :: VBox,
        nameEntry :: Entry,
        wegstreckenElemente :: FortfahrenWennToggled TMVar StatusGUI}
    | PagePlan {
        widget :: VBox,
        nameEntry :: Entry,
        bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen :: HBox,
        tvarAktionen :: TVar (Warteschlange Aktion),
        vBoxAktionen :: VBox,
        expanderAktionen :: Expander,
        tvarLabelAktionen :: TVar [Label],
        pgButtonHinzufügenPlan :: Button}

instance Show PageHinzufügen where
    show (PageStart {})                 = "PageStart"
    show (PageBahngeschwindigkeit {})   = "PageBahngeschwindigkeit"
    show (PageStreckenabschnitt {})     = "PageStreckenabschnitt"
    show (PageWeiche {})                = "PageWeiche"
    show (PageKupplung {})              = "PageKupplung"
    show (PageWegstrecke {})            = "PageWegstrecke"
    show (PagePlan {})                  = "PagePlan"

data DialogHinzufügen = DialogHinzufügen {
                            dialog :: Dialog,
                            pages :: Warteschlange PageHinzufügen,
                            buttonHinzufügen :: Button,
                            buttonHinzufügenWeicheMärklin :: Button,
                            buttonHinzufügenWeicheLego :: Button,
                            buttonHinzufügenWegstrecke :: Button,
                            buttonHinzufügenPlan :: Button,
                            buttonWeiter :: Button,
                            buttonZurück :: Button,
                            comboBoxZugtyp :: ComboBox,
                            indizesZugtyp :: NonEmpty (Int, Zugtyp),
                            comboBoxFließend :: ComboBox,
                            indizesFließend :: NonEmpty (Int, Value)}

-- | dialogGetUpper fehlt in gtk3, daher hier ersetzt
dialogGetUpper :: (DialogClass d) => d -> IO Box
dialogGetUpper dialog = dialogGetActionArea dialog >>= pure . castToBox
#endif