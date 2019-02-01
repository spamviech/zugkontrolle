{-# LANGUAGE InstanceSigs, NamedFieldPuns, LambdaCase, CPP #-}

{-|
Description : Dialoge für GTK-UI.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.GTK.Dialog () where
#else
module Zug.UI.GTK.Dialog (
                        -- * Dialog auswerten
                        dialogEval,
                        -- * Knöpfe mit zugehörigem Dialog erstellen
                        buttonSavePack, buttonLoadPack, loadWidgets, buttonHinzufügenPack) where

-- Bibliotheken
import Graphics.UI.Gtk
import Data.Sequence.Queue
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Foldable (toList)
import Control.Applicative (ZipList(..))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State
import Control.Concurrent
import Control.Lens ((^.), (^..))
import Numeric.Natural
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar
import Zug.Klassen
import Zug.Anbindung
import Zug.Plan
import qualified Zug.Language as Language
import Zug.Language ((<!>), (<:>))
import Zug.UI.Base
import Zug.UI.Befehl
import Zug.UI.GTK.Widgets
import Zug.UI.GTK.FortfahrenWenn

-- | Speichern des aktuellen 'StatusGUI'
buttonSavePack :: (BoxClass b, LikeMVar lmvar) => Window -> b -> lmvar StatusGUI -> IO Button
buttonSavePack windowMain box mvarStatus = do
    dialogSave <- dialogSaveNew windowMain
    boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.speichern $ dialogEval dialogSave >>= \response -> when (response == ResponseOk) $ fileChooserGetFilename dialogSave >>= \(Just dateipfad) -> void $ runMVarBefehl (Speichern dateipfad) mvarStatus

dialogSaveNew :: Window -> IO FileChooserDialog
dialogSaveNew window = widgetNewWithOptionsEvents (fileChooserDialogNew (Just Language.speichern :: Maybe String) (Just window) FileChooserActionSave [(Language.speichern, ResponseOk), (Language.abbrechen, ResponseCancel)]) [fileChooserDoOverwriteConfirmation := True] []

-- | Laden eines neuen 'StatusGUI' aus einer Datei
buttonLoadPack :: (BoxClass b, LikeMVar lmvar) => Window -> b -> lmvar StatusGUI -> DynamischeWidgets -> IO Button
buttonLoadPack windowMain box mvarStatus dynamischeWidgets = do
    dialogLoad <- dialogLoadNew windowMain
    dialogLoadFail <- dialogLoadFailNew windowMain
    boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.laden $ do
        response <- dialogEval dialogLoad
        when (response == ResponseOk) $ void $ do
            fileChooserGetFilename dialogLoad >>= \case
                (Nothing)           -> void $ set dialogLoadFail [windowTitle := (Language.nichtGefundeneDatei :: String)] >> dialogEval dialogLoadFail
                (Just dateipfad)    -> void $ do
                    statusInitial <- readLMVar mvarStatus
                    -- neuer Status ist schon in mvarStatus gespeichert und muss nicht mehr neu gesetzt werden
                    evalStateT (ausführenBefehl $ Laden dateipfad (loadWidgets mvarStatus dynamischeWidgets) (liftIO $ void $ set dialogLoadFail [windowTitle := dateipfad] >> dialogEval dialogLoadFail)) statusInitial

-- | Passe angezeigte Widgets (inkl. 'StatusGUI' in 'LikeMVar') an reinen 'Status' an.
loadWidgets :: (LikeMVar lmvar) => lmvar StatusGUI -> DynamischeWidgets -> Status -> IO StatusGUI
loadWidgets mvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxBahngeschwindigkeiten, vBoxStreckenabschnitte, vBoxWeichen, vBoxKupplungen, vBoxWegstrecken, vBoxPläne, vBoxHinzufügenWegstreckeBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeiten, vBoxHinzufügenWegstreckeStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte, vBoxHinzufügenWegstreckeWeichen, vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts, vBoxHinzufügenWegstreckeKupplungen, vBoxHinzufügenPlanKupplungen, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung}) status = do
    evalMVarIOStatus löscheWidgets mvarStatus
    erstelleWidgets mvarStatus status
        where
            löscheWidgets :: IOStatusGUI ()
            löscheWidgets = State.get >>= liftIOFunction (löscheWidgetsAux) >> putBahngeschwindigkeiten [] >> putStreckenabschnitte [] >> putWeichen [] >> putKupplungen [] >> putWegstrecken [] >> putPläne []
            löscheWidgetsAux :: StatusGUI -> IO ()
            löscheWidgetsAux status = do
                mapM_ (\bgWidgets@(BGWidgets {bgWidget=w, bgHinzWS=hww}) -> containerRemove vBoxBahngeschwindigkeiten w >> containerRemove vBoxHinzufügenWegstreckeBahngeschwindigkeiten (fst hww) >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanBahngeschwindigkeiten] <*> ZipList (bgWidgets ^.. foldPlan))) $ status ^. bahngeschwindigkeiten
                mapM_ (\stWidgets@(STWidgets {stWidget=w, stHinzWS=hww}) -> containerRemove vBoxStreckenabschnitte w >> containerRemove vBoxHinzufügenWegstreckeStreckenabschnitte (fst hww) >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanStreckenabschnitte] <*> ZipList (stWidgets ^.. foldPlan))) $ status ^. streckenabschnitte
                mapM_ (\weWidgets@(WEWidgets {weWidget=w, weHinzWS=hww}) -> containerRemove vBoxWeichen w >> containerRemove vBoxHinzufügenWegstreckeWeichen ((\(w,_,_) -> w) hww) >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts] <*> ZipList (weWidgets ^.. foldPlan))) $ status ^. weichen
                mapM_ (\kuWidgets@(KUWidgets {kuWidget=w, kuHinzWS=hww}) -> containerRemove vBoxKupplungen w >> containerRemove vBoxHinzufügenWegstreckeKupplungen (fst hww) >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanKupplungen] <*> ZipList (kuWidgets ^.. foldPlan))) $ status ^. kupplungen
                mapM_ (\wsWidgets@(WSWidgets {wsWidget=w}) -> containerRemove vBoxWegstrecken w >> sequence_ (getZipList $ containerRemoveJust <$> ZipList [vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung] <*> ZipList (wsWidgets ^.. foldPlan))) $ status ^. wegstrecken
                mapM_ (\(PLWidgets {plWidget=w}) -> containerRemove vBoxPläne w) $ status ^. pläne
            erstelleWidgets :: (LikeMVar lmvar) => lmvar StatusGUI -> Status -> IO StatusGUI
            erstelleWidgets mvarStatus status = do
                mapM_ (\bahngeschwindigkeit -> bahngeschwindigkeitPackNew bahngeschwindigkeit mvarStatus dynamischeWidgets) $ status ^. bahngeschwindigkeiten
                mapM_ (\streckenabschnitt -> streckenabschnittPackNew streckenabschnitt mvarStatus dynamischeWidgets) $ status ^. streckenabschnitte
                mapM_ (\weiche -> weichePackNew weiche mvarStatus dynamischeWidgets) $ status ^. weichen
                mapM_ (\kupplung -> kupplungPackNew kupplung mvarStatus dynamischeWidgets) $ status ^. kupplungen
                mapM_ (\wegstrecke -> wegstreckePackNew wegstrecke mvarStatus dynamischeWidgets) $ status ^. wegstrecken
                mapM_ (\plan -> planPackNew plan mvarStatus dynamischeWidgets) $ status ^. pläne
                readLMVar mvarStatus

dialogLoadNew :: Window -> IO FileChooserDialog
dialogLoadNew window = fileChooserDialogNew (Just Language.laden :: Maybe String) (Just window) FileChooserActionOpen [(Language.laden, ResponseOk), (Language.abbrechen, ResponseCancel)]

dialogLoadFailNew :: Window -> IO MessageDialog
dialogLoadFailNew window = messageDialogNew (Just window) [] MessageError ButtonsOk (Language.nichtGefundeneDatei <!> "" :: String)

-- | Hinzufügen eines 'StreckenObjekt'
buttonHinzufügenPack :: (WindowClass w, BoxClass b) => w -> b -> DynamischeWidgets -> IO (Button, LinkedMVar StatusGUI)
buttonHinzufügenPack parentWindow box dynamischeWidgets = do
    (dialogHinzufügen@(DialogHinzufügen {dialog}), lmvar) <- dialogHinzufügenNew parentWindow dynamischeWidgets
    button <- liftIO $ boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.hinzufügen $ do
        widgetShow dialog
        runPage 0 dialogHinzufügen lmvar dynamischeWidgets
    pure (button, lmvar)
    where
        runPage :: (LikeMVar lmvar) => Natural -> DialogHinzufügen -> lmvar StatusGUI -> DynamischeWidgets -> IO ()
        runPage i   dialogHinzufügen@(DialogHinzufügen {dialog, pages, buttonWeiter, buttonZurück})   mvarStatus  dynamischeWidgets   = do
            showNth i pages >>= \case
                (Nothing)   -> error $ "Seite beim Hinzufügen nicht gefunden: " ++ show i
                (Just page) -> do
                    hinzufügenAktivieren dialogHinzufügen page
                    (if istLetzteSeite page then widgetHide else widgetShow) buttonWeiter
                    (if istErsteSeite page then widgetHide else widgetShow) buttonZurück
                    resetEntry page
                    optionenAnzeigen page mvarStatus
                    dialogRun dialog >>= \case
                        ResponseOk      -> widgetHide dialog >> objektHinzufügen page mvarStatus dynamischeWidgets
                        ResponseApply   -> zeigeNächsteSeite page dialogHinzufügen mvarStatus dynamischeWidgets
                        ResponseReject  -> zeigeVorherigeSeite page dialogHinzufügen mvarStatus dynamischeWidgets
                        -- erwartete Rückgabewerte: ResponseCancel, ResponseDeleteEvent -> Breche Hinzufügen ab
                        _response       -> widgetHide dialog
        istErsteSeite :: PageHinzufügen -> Bool
        istErsteSeite   (PageStart {})  = True
        istErsteSeite   _               = False
        istLetzteSeite :: PageHinzufügen -> Bool
        istLetzteSeite  (PageStart {})  = False
        istLetzteSeite  _               = True
        hinzufügenAktivieren :: DialogHinzufügen -> PageHinzufügen -> IO ()
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeiche, buttonHinzufügenWegstrecke, buttonHinzufügenPlan}) (PageWeiche {})     = widgetHide buttonHinzufügen >> widgetShow buttonHinzufügenWeiche >> widgetHide buttonHinzufügenWegstrecke >> widgetHide buttonHinzufügenPlan
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeiche, buttonHinzufügenWegstrecke, buttonHinzufügenPlan}) (PageWegstrecke {}) = widgetHide buttonHinzufügen >> widgetHide buttonHinzufügenWeiche >> widgetShow buttonHinzufügenWegstrecke >> widgetHide buttonHinzufügenPlan
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeiche, buttonHinzufügenWegstrecke, buttonHinzufügenPlan}) (PagePlan {})       = widgetHide buttonHinzufügen >> widgetHide buttonHinzufügenWeiche >> widgetHide buttonHinzufügenWegstrecke >> widgetShow buttonHinzufügenPlan
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeiche, buttonHinzufügenWegstrecke, buttonHinzufügenPlan}) (PageStart {})      = widgetHide buttonHinzufügen >> widgetHide buttonHinzufügenWeiche >> widgetHide buttonHinzufügenWegstrecke >> widgetHide buttonHinzufügenPlan
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeiche, buttonHinzufügenWegstrecke, buttonHinzufügenPlan}) _page               = widgetShow buttonHinzufügen >> widgetHide buttonHinzufügenWeiche >> widgetHide buttonHinzufügenWegstrecke >> widgetHide buttonHinzufügenPlan
        resetEntry :: PageHinzufügen -> IO ()
        resetEntry  (PageBahngeschwindigkeit {nameEntry})   = set nameEntry [entryText := "", widgetHasFocus := True]
        resetEntry  (PageStreckenabschnitt {nameEntry})     = set nameEntry [entryText := "", widgetHasFocus := True]
        resetEntry  (PageWeiche {nameEntry})                = set nameEntry [entryText := "", widgetHasFocus := True]
        resetEntry  (PageKupplung {nameEntry})              = set nameEntry [entryText := "", widgetHasFocus := True]
        resetEntry  (PageWegstrecke {nameEntry})            = set nameEntry [entryText := "", widgetHasFocus := True]
        resetEntry  (PagePlan {nameEntry})                  = set nameEntry [entryText := "", widgetHasFocus := True]
        resetEntry  _page                                   = pure ()
        optionenAnzeigen :: (LikeMVar lmvar) => PageHinzufügen -> lmvar StatusGUI -> IO ()
        optionenAnzeigen    (PagePlan {bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen, aktionen}) mvarStatus  = do
            -- Zeige nur verfügbare Aktionen an
            status <- readLMVar mvarStatus
            widgetShowIf (not $ null (status ^. bahngeschwindigkeiten)) bgFunktionen
            widgetShowIf (not $ null (status ^. streckenabschnitte)) stFunktionen
            widgetShowIf (not $ null (status ^. weichen)) weFunktionen
            widgetShowIf (not $ null (status ^. kupplungen)) kuFunktionen
            widgetShowIf (not $ null (status ^. wegstrecken)) wsFunktionen
            -- Aktionen zurücksetzen
            modifyLMVar_ (aktionen ^. linkedMVarElemente) $ pure . const seEmpty
        optionenAnzeigen    _page                                                                               _mvarStatus = pure ()
        zeigeNächsteSeite :: (LikeMVar lmvar) => PageHinzufügen -> DialogHinzufügen -> lmvar StatusGUI -> DynamischeWidgets -> IO ()
        zeigeNächsteSeite   (PageStart {radioButtons})  dialogHinzufügen                    mvarStatus  dynamischeWidgets   = getToggled radioButtons >>= \pageNr -> runPage (succ pageNr) dialogHinzufügen mvarStatus dynamischeWidgets
        zeigeNächsteSeite   _page                       (DialogHinzufügen {buttonWeiter})   _mvarStatus _dynamischeWidgets  = widgetHide buttonWeiter
        zeigeVorherigeSeite :: (LikeMVar lmvar) => PageHinzufügen -> DialogHinzufügen -> lmvar StatusGUI -> DynamischeWidgets -> IO ()
        zeigeVorherigeSeite _   dialogHinzufügen    = runPage 0 dialogHinzufügen
        objektHinzufügen :: (LikeMVar lmvar) => PageHinzufügen -> lmvar StatusGUI -> DynamischeWidgets -> IO ()
        objektHinzufügen    (PageBahngeschwindigkeit {nameEntry, geschwindigkeitsPinSpinButton})    mvarStatus  dynamischeWidgets   = void $ do
            -- Kein Zugtyp Lego: ToDo!!!
            bgName <- get nameEntry entryText
            pin <- get geschwindigkeitsPinSpinButton spinButtonValue
            bahngeschwindigkeitPackNew (MärklinBahngeschwindigkeit {bgName, geschwindigkeitsPin=toPin pin}) mvarStatus dynamischeWidgets
        objektHinzufügen    (PageStreckenabschnitt {nameEntry, stromPinSpinButton})                 mvarStatus  dynamischeWidgets   = void $ do
            stName <- get nameEntry entryText
            pin <- get stromPinSpinButton spinButtonValue
            streckenabschnittPackNew (Streckenabschnitt {stName, stromPin=toPin pin}) mvarStatus dynamischeWidgets
        objektHinzufügen    (PageWeiche {nameEntry, richtungsWidgets})                              mvarStatus  dynamischeWidgets   = void $ do
            -- Kein Zugtyp Lego (verwende Radio-Buttons für Richtungs-Kombinationen): ToDo!!!
            weName <- get nameEntry entryText
            richtungsPins <- foldM (\acc (richtung, checkButton, spinButton) -> get checkButton toggleButtonActive >>= \pressed -> if pressed then get spinButton spinButtonValue >>= \pin -> pure ((richtung, toPin pin):acc) else pure acc) [] richtungsWidgets
            case richtungsPins of
                ([])    -> error "Keine Richtung beim hinzufügen einer Weiche ausgewählt."
                (h:t)   -> weichePackNew (MärklinWeiche {weName, richtungsPins=h:|t}) mvarStatus dynamischeWidgets
        objektHinzufügen    (PageKupplung {nameEntry, kupplungsPinSpinButton})                      mvarStatus  dynamischeWidgets   = void $ do
            kuName <- get nameEntry entryText
            pin <- get kupplungsPinSpinButton spinButtonValue
            kupplungPackNew (Kupplung {kuName, kupplungsPin=toPin pin}) mvarStatus dynamischeWidgets
        objektHinzufügen    (PageWegstrecke {nameEntry, wegstreckenElemente})                       mvarStatus  dynamischeWidgets   = void $ do
            wsName <- get nameEntry entryText
            wegstreckenElementeCurrent <- readLMVar $ wegstreckenElemente ^. linkedMVarCheckButtons
            wsBahngeschwindigkeiten <- foldM (getToggledWegstreckenElemente $ pure . bg) [] $ wegstreckenElementeCurrent ^. bahngeschwindigkeiten
            wsStreckenabschnitte <- foldM (getToggledWegstreckenElemente $ pure . st) [] $  wegstreckenElementeCurrent ^. streckenabschnitte
            wsWeichenRichtungen <- foldM (getToggledWegstreckenElemente $ getWeichenRichtung) [] $ wegstreckenElementeCurrent ^. weichen
            wsKupplungen <- foldM (getToggledWegstreckenElemente $ pure . ku) [] $ wegstreckenElementeCurrent ^. kupplungen
            wegstreckePackNew (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen}) mvarStatus dynamischeWidgets
                where
                    getToggledWegstreckenElemente :: (WegstreckenElement s) => (s -> IO a) -> [a] -> s -> IO [a]
                    getToggledWegstreckenElemente   selector    acc s   = get (s ^. lensWegstrecke . checkButton) toggleButtonActive >>= \pressed -> if pressed then (selector s) >>= \h -> pure (h:acc) else pure acc
                    getWeichenRichtung :: WEWidgets -> IO (Weiche, Richtung)
                    getWeichenRichtung weWidgets = getToggledRichtung (weWidgets ^. getterRichtungsRadioButtons) >>= \richtung -> pure (we weWidgets, richtung)
                    getToggledRichtung :: NonEmpty (Richtung, RadioButton) -> IO Richtung
                    getToggledRichtung  ((richtung, radioButton):|tail) = do
                        toggled <- get radioButton toggleButtonActive
                        if toggled
                            then pure richtung
                            else case tail of
                                ([])    -> error "getToggledRichtung ohne ausgewählten RadioButton aufgerufen."
                                (h:t)   -> getToggledRichtung $ h:|t
        objektHinzufügen    (PagePlan {nameEntry, aktionen})                                        mvarStatus  dynamischeWidgets   = void $ do
            plName <- get nameEntry entryText
            aktionenQueue <- readLMVar $ aktionen ^. linkedMVarElemente
            planPackNew (Plan {plName, plAktionen=toList aktionenQueue}) mvarStatus dynamischeWidgets
        objektHinzufügen    page                                                                    _mvarStatus _dynamischeWidgets   = do
            error $ "Unbekannte Seite während dem Hinzufügen angezeigt: " ++ show page

dialogHinzufügenNew :: (WindowClass w) => w -> DynamischeWidgets -> IO (DialogHinzufügen, LinkedMVar StatusGUI)
dialogHinzufügenNew parent (DynamischeWidgets {vBoxHinzufügenWegstreckeBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeiten, vBoxHinzufügenWegstreckeStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte, vBoxHinzufügenWegstreckeWeichen, vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts, vBoxHinzufügenWegstreckeKupplungen, vBoxHinzufügenPlanKupplungen, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung, mvarPlanObjekt}) = do
    dialog <- widgetNewWithOptionsEvents dialogNew [windowTitle := (Language.hinzufügen :: String), windowTransientFor := parent, windowDefaultHeight := 320] []
    buttonHinzufügen <- dialogAddButton dialog (Language.hinzufügen :: String) ResponseOk
    buttonHinzufügenWeiche <- dialogAddButton dialog (Language.hinzufügen :: String) ResponseOk
    buttonHinzufügenWegstrecke <- dialogAddButton dialog (Language.hinzufügen :: String) ResponseOk
    buttonHinzufügenPlan <- dialogAddButton dialog (Language.hinzufügen :: String) ResponseOk
    buttonWeiter <- dialogAddButton dialog (Language.weiter :: String) ResponseApply
    buttonZurück <- dialogAddButton dialog (Language.zurück :: String) ResponseReject
    _buttonAbbrechen <- dialogAddButton dialog (Language.abbrechen :: String) ResponseCancel
    contentBox <- dialogGetUpperNew dialog
    (linkedMVar, pages) <- flip State.runStateT seEmpty $ do
        appendPage contentBox $ do
            vBox <- vBoxNew False 0
            rbBahngeschwindigkeit   <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabel (Language.bahngeschwindigkeit :: String)
            rbStreckenabschnitt     <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.streckenabschnitt :: String)
            rbWeiche                <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.weiche :: String)
            rbKupplung              <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.kupplung :: String)
            rbWegstrecke            <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.wegstrecke :: String)
            rbPlan                  <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.plan :: String)
            pure (PageStart {widget=vBox, radioButtons=rbBahngeschwindigkeit:|rbStreckenabschnitt:rbWeiche:rbKupplung:rbWegstrecke:rbPlan:[]}, Nothing)
        appendPage contentBox $ do
            -- Bahngeschwindigkeit
            -- Lego-Zugtyp: ToDo!!!
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.bahngeschwindigkeit :: String)
            nameEntry <- nameEntryPackNew widget
            (geschwindigkeitsPinWidget, geschwindigkeitsPinSpinButton) <- pinSpinBoxNew Language.geschwindigkeit
            boxPackWidgetNewDefault widget $ pure geschwindigkeitsPinWidget
            -- Zeige Fahrtrichtungs-Pin nicht für Märklin-Bahngeschwindigkeit an
            (fahrtrichtungsPinWidget, fahrtrichtungsPinSpinButton) <- pinSpinBoxNew Language.fahrtrichtung
            boxPackDefault widget fahrtrichtungsPinWidget
            pure (PageBahngeschwindigkeit {widget, nameEntry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton}, Nothing)
        appendPage contentBox $ do
            -- Streckenabschnitt
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.streckenabschnitt :: String)
            nameEntry <- nameEntryPackNew widget
            (stromPinWidget, stromPinSpinButton) <- pinSpinBoxNew Language.strom
            boxPackWidgetNewDefault widget $ pure stromPinWidget
            pure (PageStreckenabschnitt {widget, nameEntry, stromPinSpinButton}, Nothing)
        appendPage contentBox $ do
            -- Weiche
            -- Lego-Zugtyp: ToDo!!!
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.weiche :: String)
            nameEntry <- nameEntryPackNew widget
            let
                createRichtungsPin :: Richtung -> IO (Richtung, CheckButton, SpinButton)
                createRichtungsPin richtung = do
                    hBox <- boxPackWidgetNewDefault widget $ hBoxNew False 0
                    checkButton <- boxPackWidgetNewDefault hBox checkButtonNew
                    (pinWidget, spinButton) <- pinSpinBoxNew $ show richtung
                    boxPackWidgetNewDefault hBox $ pure pinWidget
                    pure (richtung, checkButton, spinButton)
            richtungsWidgets <- mapM createRichtungsPin unterstützteRichtungen >>= fortfahrenWennToggledNew buttonHinzufügenWeiche
            pure (PageWeiche {widget, nameEntry, richtungsWidgets}, Nothing)
        appendPage contentBox $ do
            -- Kupplung
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.kupplung :: String)
            nameEntry <- nameEntryPackNew widget
            (kupplungPinWidget, kupplungsPinSpinButton) <- pinSpinBoxNew Language.kupplung
            boxPackWidgetNewDefault widget $ pure kupplungPinWidget
            pure (PageKupplung {widget, nameEntry, kupplungsPinSpinButton}, Nothing)
        (Just linkedMVar) <- appendPage contentBox $ do
            -- Wegstrecke
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.wegstrecke :: String)
            nameEntry <- nameEntryPackNew widget
            notebook <- boxPackWidgetNew widget PackGrow paddingDefault positionDefault notebookNew
            scrolledWidgedNotebookAppendPageNew notebook Language.bahngeschwindigkeiten $ pure vBoxHinzufügenWegstreckeBahngeschwindigkeiten
            scrolledWidgedNotebookAppendPageNew notebook Language.streckenabschnitte $ pure vBoxHinzufügenWegstreckeStreckenabschnitte
            scrolledWidgedNotebookAppendPageNew notebook Language.weichen $ pure vBoxHinzufügenWegstreckeWeichen
            scrolledWidgedNotebookAppendPageNew notebook Language.kupplungen $ pure vBoxHinzufügenWegstreckeKupplungen
            wegstreckenElemente <- fortfahrenWennToggledEmptyLinkedMVarNew buttonHinzufügenWegstrecke traversalHinzufügenWegstrecke (Nothing :: Maybe (LinkedMVar StatusGUI))
            pure (PageWegstrecke {widget, nameEntry, wegstreckenElemente}, Just $ wegstreckenElemente ^. linkedMVarCheckButtons)
        appendPage contentBox $ do
            -- Plan
            -- Objekt-Buttons schreiben bei Druck Objekt in mvarPlanObjekt
            -- Sobald diese gefüllt ist kann die Aktion zur LinkedMVar hinzufgefügt werden
            aktionenWidgets <- newMVar []
            expanderAktionen <-  expanderNew (Language.aktionen :: String)
            vBoxAktionen <- vBoxNew False 0
            lmvarTemp <- newEmptyLinkedMVar $ \_updateAktion aktionen -> showAktionen vBoxAktionen expanderAktionen aktionenWidgets aktionen >> pure aktionen
            aktionen <- fortfahrenWennGefülltEmptyLinkedMVarNew buttonHinzufügenPlan $ Just lmvarTemp
            let lmvarElemente = aktionen ^. linkedMVarElemente
            -- Hilfsdialog erstellen
            windowObjekte <- widgetNewWithOptionsEvents windowNew [windowTitle := (Language.aktion :: String), windowModal := True, windowTransientFor := dialog] [(deleteEvent, Right $ \window -> liftIO $ putLMVar mvarPlanObjekt Nothing >> widgetHide window >> pure True)]
            widgetHide windowObjekte
            windowVBox <- containerAddWidgetNew windowObjekte $ vBoxNew False 0
            (windowScrolledWindowBG         , windowVBoxBG)         <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowST         , windowVBoxST)         <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWEGerade   , windowVBoxWEGerade)   <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWEKurve    , windowVBoxWEKurve)    <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWELinks    , windowVBoxWELinks)    <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWERechts   , windowVBoxWERechts)   <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowKU         , windowVBoxKU)         <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWS         , windowVBoxWS)         <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            boxPackWidgetNewDefault windowVBox $ buttonNewWithEventLabel Language.abbrechen $ putLMVar mvarPlanObjekt Nothing >> widgetHide windowObjekte
            boxPackWidgetNewDefault windowVBoxBG $ labelNew $ Just $ (Language.bahngeschwindigkeiten :: String) 
            boxPackDefault windowVBoxBG vBoxHinzufügenPlanBahngeschwindigkeiten
            boxPackWidgetNewDefault windowVBoxBG $ labelNew $ Just $ (Language.wegstrecken :: String)
            boxPackDefault windowVBoxBG vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit
            boxPackWidgetNewDefault windowVBoxST $ labelNew $ Just $ (Language.streckenabschnitte :: String)
            boxPackDefault windowVBoxST vBoxHinzufügenPlanStreckenabschnitte
            boxPackWidgetNewDefault windowVBoxST $ labelNew $ Just $ (Language.wegstrecken :: String)
            boxPackDefault windowVBoxST vBoxHinzufügenPlanWegstreckenStreckenabschnitt
            boxPackWidgetNewDefault windowVBoxWEGerade $ labelNew $ Just $ (Language.weichen :: String)
            boxPackDefault windowVBoxWEGerade vBoxHinzufügenPlanWeichenGerade
            boxPackWidgetNewDefault windowVBoxWEKurve $ labelNew $ Just $ (Language.weichen :: String)
            boxPackDefault windowVBoxWEKurve vBoxHinzufügenPlanWeichenKurve
            boxPackWidgetNewDefault windowVBoxWELinks $ labelNew $ Just $ (Language.weichen :: String)
            boxPackDefault windowVBoxWELinks vBoxHinzufügenPlanWeichenLinks
            boxPackWidgetNewDefault windowVBoxWERechts $ labelNew $ Just $ (Language.weichen :: String)
            boxPackDefault windowVBoxWERechts vBoxHinzufügenPlanWeichenRechts
            boxPackWidgetNewDefault windowVBoxKU $ labelNew $ Just $ (Language.kupplungen :: String)
            boxPackDefault windowVBoxKU vBoxHinzufügenPlanKupplungen
            boxPackWidgetNewDefault windowVBoxKU $ labelNew $ Just $ (Language.wegstrecken :: String)
            boxPackDefault windowVBoxKU vBoxHinzufügenPlanWegstreckenKupplung
            boxPackWidgetNewDefault windowVBoxWS $ labelNew $ Just $ (Language.wegstrecken :: String)
            boxPackDefault windowVBoxWS vBoxHinzufügenPlanWegstreckenWeiche
            -- Widget erstellen
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.plan :: String)
            nameEntry <- nameEntryPackNew widget
            functionBox <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            let µsInS = 1000000
            spinButtonZeit <- spinButtonNewWithRange 0 (60*µsInS) (1*µsInS)
            boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.warten $ do
                wertDouble <- get spinButtonZeit spinButtonValue
                modifyLMVar_ lmvarElemente $ pure . (append (Warten $ fromIntegral $ fromEnum wertDouble))
            boxPackWidgetNewDefault functionBox $ pure spinButtonZeit
            boxPackWidgetNewDefault functionBox $ labelNew $ Just $ (Language.wartenEinheit :: String)
            bgFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            hScaleGeschwindigkeit <- hScaleNewWithRange 0 100 1
            boxPackWidgetNewDefault bgFunktionen $ buttonNewWithEventLabel Language.geschwindigkeit $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetShow windowScrolledWindowBG
                    widgetHide windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                wertDouble <- get hScaleGeschwindigkeit rangeValue
                let wert = fromIntegral $ fromEnum wertDouble
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OBahngeschwindigkeit bg))    -> modifyLMVar_ lmvarElemente $ pure . (append (ABahngeschwindigkeit (Geschwindigkeit bg wert)))
                    (Just (OWegstrecke ws))             -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSBahngeschwindigkeit (Geschwindigkeit ws wert))))
                    _objekt                             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            boxPackWidgetNew bgFunktionen PackGrow paddingDefault positionDefault $ pure hScaleGeschwindigkeit
            boxPackWidgetNewDefault bgFunktionen $ buttonNewWithEventLabel Language.umdrehen $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetShow windowScrolledWindowBG
                    widgetHide windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                -- Lego-Zugtyp mit vorgegebener Fahrtrichtung: ToDo!!!
                case objekt of
                    (Just (OBahngeschwindigkeit bg))    -> modifyLMVar_ lmvarElemente $ pure . (append (ABahngeschwindigkeit (Umdrehen bg Nothing)))
                    (Just (OWegstrecke ws))             -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSBahngeschwindigkeit (Umdrehen ws Nothing))))
                    _objekt                             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            stFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            boxPackWidgetNewDefault stFunktionen $ buttonNewWithEventLabel (Language.strom <:> Language.an) $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetHide windowScrolledWindowBG
                    widgetShow windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OStreckenabschnitt st))  -> modifyLMVar_ lmvarElemente $ pure . (append (AStreckenabschnitt (Strom st True)))
                    (Just (OWegstrecke ws))         -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSStreckenabschnitt (Strom ws True))))
                    _objekt                         -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            boxPackWidgetNewDefault stFunktionen $ buttonNewWithEventLabel (Language.strom <:> Language.aus) $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetHide windowScrolledWindowBG
                    widgetShow windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OStreckenabschnitt st))  -> modifyLMVar_ lmvarElemente $ pure . (append (AStreckenabschnitt (Strom st False)))
                    (Just (OWegstrecke ws))         -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSStreckenabschnitt (Strom ws False))))
                    _objekt                         -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            weFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.gerade) $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetHide windowScrolledWindowBG
                    widgetHide windowScrolledWindowST
                    widgetShow windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWeiche we)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWeiche (Stellen we Gerade)))
                    _objekt             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.kurve) $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetHide windowScrolledWindowBG
                    widgetHide windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetShow windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWeiche we)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWeiche (Stellen we Kurve)))
                    _objekt             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.links) $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetHide windowScrolledWindowBG
                    widgetHide windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetShow windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWeiche we)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWeiche (Stellen we Links)))
                    _objekt             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.rechts) $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetHide windowScrolledWindowBG
                    widgetHide windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetShow windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWeiche we)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWeiche (Stellen we Rechts)))
                    _objekt             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            kuFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            boxPackWidgetNewDefault kuFunktionen $ buttonNewWithEventLabel (Language.kuppeln) $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetHide windowScrolledWindowBG
                    widgetHide windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetShow windowScrolledWindowKU
                    widgetHide windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OKupplung ku))   -> modifyLMVar_ lmvarElemente $ pure . (append (AKupplung (Kuppeln ku)))
                    (Just (OWegstrecke ws)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSKupplung (Kuppeln ws))))
                    _objekt                 -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            wsFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            boxPackWidgetNewDefault wsFunktionen $ buttonNewWithEventLabel (Language.einstellen) $ void $ forkIO $ do
                postGUIAsync $ do
                    widgetShow windowObjekte
                    widgetHide windowScrolledWindowBG
                    widgetHide windowScrolledWindowST
                    widgetHide windowScrolledWindowWEGerade
                    widgetHide windowScrolledWindowWEKurve
                    widgetHide windowScrolledWindowWELinks
                    widgetHide windowScrolledWindowWERechts
                    widgetHide windowScrolledWindowKU
                    widgetShow windowScrolledWindowWS
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWegstrecke ws)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (Einstellen ws)))
                    _objekt                 -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            boxPackWidgetNewDefault widget $ pure expanderAktionen
            containerAddWidgetNew expanderAktionen $ pure vBoxAktionen
            putLMVar lmvarElemente seEmpty
            pure (PagePlan {widget, nameEntry, bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen, aktionen}, Nothing)
        pure linkedMVar
    pure (DialogHinzufügen {dialog, pages, buttonHinzufügen, buttonHinzufügenWeiche, buttonHinzufügenWegstrecke, buttonHinzufügenPlan, buttonWeiter, buttonZurück}, linkedMVar)
        where
            appendPage :: (BoxClass b, Monad m, MonadIO m) => b -> m (PageHinzufügen, Maybe (LinkedMVar StatusGUI)) -> StateT (SEQueue PageHinzufügen) m (Maybe (LinkedMVar StatusGUI))
            appendPage box konstruktor = do
                (page, maybeLinkedMVar) <- lift konstruktor
                liftIO $ boxPack box (widget page) PackGrow paddingDefault positionDefault
                State.modify (append page)
                pure maybeLinkedMVar
            showAktionen :: (BoxClass b, Foldable t, LikeMVar lmvar) => b -> Expander -> lmvar [Label] -> t Aktion -> IO ()
            showAktionen box expander mvarWidgets aktionen = do
                widgets <- takeLMVar mvarWidgets
                mapM_ (containerRemove box) widgets
                widgetsNeu <- mapM (boxPackWidgetNewDefault box . labelNew . Just . show) $ toList aktionen
                set expander [expanderLabel := Language.aktionen <:> show (length aktionen)]
                putLMVar mvarWidgets widgetsNeu

-- | Zeige nur die n-te Seite (start bei n=0) an
showNth :: Natural -> SEQueue PageHinzufügen -> IO (Maybe PageHinzufügen)
showNth i   queue   = case view queue of
    (EmptyL)        -> pure Nothing
    (h :< t)
        | i <= 0    -> do
            widgetShow $ widget h
            mapM_ (widgetHide . widget) t
            pure $ Just h
        | otherwise -> do
            widgetHide $ widget h
            showNth (pred i) t

-- | Gebe den Index des ersten eingeschalteten Radiobuttons an. Wenn kein RadioButton an ist, gebe 0 zurück.
getToggled :: NonEmpty RadioButton -> IO Natural
getToggled (h:|t) = do
    toggled <- get h toggleButtonActive 
    if toggled then pure 0 else succ <$> (maybe (pure 0) getToggled $ nonEmpty t)

data PageHinzufügen = PageStart {widget :: VBox, radioButtons :: NonEmpty RadioButton}
                    | PageBahngeschwindigkeit {widget :: VBox, nameEntry :: Entry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton :: SpinButton}
                    | PageStreckenabschnitt {widget :: VBox, nameEntry :: Entry, stromPinSpinButton :: SpinButton}
                    | PageWeiche {widget :: VBox, nameEntry :: Entry, richtungsWidgets :: FortfahrenWennToggled NonEmpty (Richtung, CheckButton, SpinButton)}
                    | PageKupplung {widget :: VBox, nameEntry :: Entry, kupplungsPinSpinButton :: SpinButton}
                    | PageWegstrecke {widget :: VBox, nameEntry :: Entry, wegstreckenElemente :: FortfahrenWennToggled LinkedMVar StatusGUI}
                    | PagePlan {widget :: VBox, nameEntry :: Entry, bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen :: HBox, aktionen :: FortfahrenWennGefüllt SEQueue Aktion}

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
                                    pages :: SEQueue PageHinzufügen,
                                    buttonHinzufügen :: Button,
                                    buttonHinzufügenWeiche :: Button,
                                    buttonHinzufügenWegstrecke :: Button,
                                    buttonHinzufügenPlan :: Button,
                                    buttonWeiter :: Button,
                                    buttonZurück :: Button}

-- * Dialog-spezifische Funktionen
-- | Dialog anzeigen und auswerten
dialogEval :: (DialogClass d) => d -> IO ResponseId
dialogEval dialog = do
    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    pure response

-- | dialogGetUpper fehlt in gtk3 (Box ist nicht existent), daher hier ersetzt
dialogGetUpperNew :: (DialogClass d) => d -> IO VBox
dialogGetUpperNew dialog = do
    dialogBox <- dialogGetContentArea dialog >>= pure . castToBox
    boxPackWidgetNew dialogBox PackGrow paddingDefault positionDefault $ vBoxNew False 0

-- * Single Ended Queue-Wrapper
-- | Foldable-Instanz von Queue erzeugt Liste von toList in umgekehrter Reihenfolge
newtype SEQueue a = SEQueue (Queue a)

seEmpty :: SEQueue a
seEmpty = SEQueue empty

append :: a -> SEQueue a -> SEQueue a
append a (SEQueue queue) = SEQueue $ queue |> a

view :: (SEQueue a) -> ViewL SEQueue a
view (SEQueue queue) = case viewl queue of
    (EmptyL)        -> EmptyL
    (h :< t) -> h :< SEQueue t

instance Foldable SEQueue where
    foldMap :: Monoid m => (a -> m) -> SEQueue a -> m 
    foldMap f seQueue = case view seQueue of
        (EmptyL)    -> mempty
        (h :< t)    -> mappend (f h) $ foldMap f t

instance Functor SEQueue where
    fmap :: (a -> b) -> SEQueue a -> SEQueue b
    fmap f (SEQueue queue) = SEQueue $ fmap f queue
#endif