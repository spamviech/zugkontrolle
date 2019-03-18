{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

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
                        buttonSpeichernPack, buttonLadenPack, ladeWidgets, buttonHinzufügenPack) where

-- Bibliotheken
import Control.Concurrent (forkIO, newMVar)
import Control.Lens ((^.))
import Control.Monad (void, when, foldM)
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans (MonadIO(..), lift)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybe, fromJust)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Graphics.UI.Gtk
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar
import Zug.SEQueue
import Zug.Klassen
import Zug.Anbindung
import Zug.Plan
import qualified Zug.Language as Language
import Zug.Language (showText, (<!>), (<:>), (<^>))
import Zug.UI.Base
import Zug.UI.Befehl
import Zug.UI.GTK.Widgets
import Zug.UI.GTK.FortfahrenWenn

-- | Speichern des aktuellen 'StatusGUI'
buttonSpeichernPack :: (BoxClass b, LikeMVar lmvar) => Window -> b -> lmvar StatusGUI -> IO Button
buttonSpeichernPack windowMain box mvarStatus = do
    dialogSpeichern <- dialogSpeichernNew windowMain
    boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.speichern $ dialogEval dialogSpeichern >>= \antwort -> when (antwort == ResponseOk) $ fileChooserGetFilename dialogSpeichern >>= \(Just dateipfad) -> void $ ausführenMVarBefehl (Speichern dateipfad) mvarStatus

dialogSpeichernNew :: Window -> IO FileChooserDialog
dialogSpeichernNew window = do
    fileChooserDialog <- fileChooserDialogNew (Just Language.speichern :: Maybe Text) (Just window) FileChooserActionSave [(Language.speichern, ResponseOk), (Language.abbrechen, ResponseCancel)]
    set fileChooserDialog [fileChooserDoOverwriteConfirmation := True]
    pure fileChooserDialog

-- | Laden eines neuen 'StatusGUI' aus einer Datei
buttonLadenPack :: (BoxClass b, LikeMVar lmvar) => Window -> b -> lmvar StatusGUI -> DynamischeWidgets -> IO Button
buttonLadenPack windowMain box mvarStatus dynamischeWidgets = do
    dialogLaden <- dialogLadenNew windowMain
    dialogLadenFehler <- dialogLadenFehlerNew windowMain
    boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.laden $ do
        antwort <- dialogEval dialogLaden
        when (antwort == ResponseOk) $ void $ do
            fileChooserGetFilename dialogLaden >>= \case
                (Nothing)           -> void $ set dialogLadenFehler [windowTitle := (Language.nichtGefundeneDatei :: Text)] >> dialogEval dialogLadenFehler
                (Just dateipfad)    -> void $ do
                    statusInitial <- readLMVar mvarStatus
                    -- neuer Status ist schon in mvarStatus gespeichert und muss nicht mehr neu gesetzt werden
                    evalStateT (ausführenBefehl $ Laden dateipfad (ladeWidgets mvarStatus dynamischeWidgets) (liftIO $ void $ set dialogLadenFehler [windowTitle := dateipfad] >> dialogEval dialogLadenFehler)) statusInitial

-- | Passe angezeigte Widgets (inkl. 'StatusGUI' in 'LikeMVar') an reinen 'Status' an.
ladeWidgets :: (LikeMVar lmvar) => lmvar StatusGUI -> DynamischeWidgets -> Status -> IO StatusGUI
ladeWidgets mvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxBahngeschwindigkeiten, vBoxStreckenabschnitte, vBoxWeichen, vBoxKupplungen, vBoxWegstrecken, vBoxPläne}) status = do
    auswertenMVarIOStatus löscheWidgets mvarStatus
    erstelleWidgets mvarStatus status
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
            erstelleWidgets :: (LikeMVar lmvar) => lmvar StatusGUI -> Status -> IO StatusGUI
            erstelleWidgets mvarStatus status = do
                mapM_ (\bahngeschwindigkeit -> bahngeschwindigkeitPackNew bahngeschwindigkeit mvarStatus dynamischeWidgets) $ reverse $ status ^. bahngeschwindigkeiten
                mapM_ (\streckenabschnitt -> streckenabschnittPackNew streckenabschnitt mvarStatus dynamischeWidgets)       $ reverse $ status ^. streckenabschnitte
                mapM_ (\weiche -> weichePackNew weiche mvarStatus dynamischeWidgets)                                        $ reverse $ status ^. weichen
                mapM_ (\kupplung -> kupplungPackNew kupplung mvarStatus dynamischeWidgets)                                  $ reverse $ status ^. kupplungen
                mapM_ (\wegstrecke -> wegstreckePackNew wegstrecke mvarStatus dynamischeWidgets)                            $ reverse $ status ^. wegstrecken
                mapM_ (\plan -> planPackNew plan mvarStatus dynamischeWidgets)                                              $ reverse $ status ^. pläne
                readLMVar mvarStatus

dialogLadenNew :: Window -> IO FileChooserDialog
dialogLadenNew window = fileChooserDialogNew (Just Language.laden :: Maybe Text) (Just window) FileChooserActionOpen [(Language.laden, ResponseOk), (Language.abbrechen, ResponseCancel)]

dialogLadenFehlerNew :: Window -> IO MessageDialog
dialogLadenFehlerNew window = messageDialogNew (Just window) [] MessageError ButtonsOk (Language.nichtGefundeneDatei <!> "" :: Text)

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
                        ResponseOk      -> widgetHide dialog >> objektHinzufügen dialogHinzufügen page mvarStatus dynamischeWidgets
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
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan, comboBoxZugtyp, indizesZugtyp})   (PageWeiche {})     = widgetHide buttonHinzufügen >> get comboBoxZugtyp comboBoxActive >>= \index -> widgetShowIf (indexStimmtÜberein indizesZugtyp Märklin index) buttonHinzufügenWeicheMärklin >> widgetShowIf (indexStimmtÜberein indizesZugtyp Lego index) buttonHinzufügenWeicheLego >> widgetHide buttonHinzufügenWegstrecke >> widgetHide buttonHinzufügenPlan
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan})                                  (PageWegstrecke {}) = widgetHide buttonHinzufügen >> widgetHide buttonHinzufügenWeicheMärklin >> widgetHide buttonHinzufügenWeicheLego >> widgetShow buttonHinzufügenWegstrecke >> widgetHide buttonHinzufügenPlan
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan})                                  (PagePlan {})       = widgetHide buttonHinzufügen >> widgetHide buttonHinzufügenWeicheMärklin >> widgetHide buttonHinzufügenWeicheLego >> widgetHide buttonHinzufügenWegstrecke >> widgetShow buttonHinzufügenPlan
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan})                                  (PageStart {})      = widgetHide buttonHinzufügen >> widgetHide buttonHinzufügenWeicheMärklin >> widgetHide buttonHinzufügenWeicheLego >> widgetHide buttonHinzufügenWegstrecke >> widgetHide buttonHinzufügenPlan
        hinzufügenAktivieren    (DialogHinzufügen {buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan})                                  _page               = widgetShow buttonHinzufügen >> widgetHide buttonHinzufügenWeicheMärklin >> widgetHide buttonHinzufügenWeicheLego >> widgetHide buttonHinzufügenWegstrecke >> widgetHide buttonHinzufügenPlan
        indexStimmtÜberein :: NonEmpty (Int, Zugtyp) -> Zugtyp -> Int -> Bool
        indexStimmtÜberein ne zugtyp index = foldr (indexStimmtÜbereinAux zugtyp index) False ne
            where
                indexStimmtÜbereinAux :: Zugtyp -> Int -> (Int, Zugtyp) -> Bool -> Bool
                indexStimmtÜbereinAux   _zugtyp _index  _current            True    = True
                indexStimmtÜbereinAux   zugtyp  index   (index1, zugtyp1)   False
                    | index == index1, zugtyp == zugtyp1                            = True
                    | otherwise                                                     = False
        resetEntry :: PageHinzufügen -> IO ()
        resetEntry  (PageBahngeschwindigkeit {nameEntry})   = set nameEntry [entryText := ("" :: Text), widgetHasFocus := True]
        resetEntry  (PageStreckenabschnitt {nameEntry})     = set nameEntry [entryText := ("" :: Text), widgetHasFocus := True]
        resetEntry  (PageWeiche {nameEntry})                = set nameEntry [entryText := ("" :: Text), widgetHasFocus := True]
        resetEntry  (PageKupplung {nameEntry})              = set nameEntry [entryText := ("" :: Text), widgetHasFocus := True]
        resetEntry  (PageWegstrecke {nameEntry})            = set nameEntry [entryText := ("" :: Text), widgetHasFocus := True]
        resetEntry  (PagePlan {nameEntry})                  = set nameEntry [entryText := ("" :: Text), widgetHasFocus := True]
        resetEntry  _page                                   = pure ()
        optionenAnzeigen :: (LikeMVar lmvar) => PageHinzufügen -> lmvar StatusGUI -> IO ()
        optionenAnzeigen    (PagePlan {bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen, aktionen}) mvarStatus  = do
            -- Zeige nur verfügbare Aktionen an
            status <- readLMVar mvarStatus
            widgetShowIf (not $ null (status ^. bahngeschwindigkeiten) && null (status ^. wegstrecken)) bgFunktionen
            widgetShowIf (not $ null (status ^. streckenabschnitte) && null (status ^. wegstrecken)) stFunktionen
            widgetShowIf (not $ null (status ^. weichen)) weFunktionen
            widgetShowIf (not $ null (status ^. kupplungen) && null (status ^. wegstrecken)) kuFunktionen
            widgetShowIf (not $ null (status ^. wegstrecken) && null (status ^. wegstrecken)) wsFunktionen
            -- Aktionen zurücksetzen
            modifyLMVar_ (aktionen ^. linkedMVarElemente) $ pure . const empty
        optionenAnzeigen    _page                                                                               _mvarStatus = pure ()
        zeigeNächsteSeite :: (LikeMVar lmvar) => PageHinzufügen -> DialogHinzufügen -> lmvar StatusGUI -> DynamischeWidgets -> IO ()
        zeigeNächsteSeite   (PageStart {radioButtons})  dialogHinzufügen                    mvarStatus  dynamischeWidgets   = erhalteToggledIndex radioButtons >>= \pageNr -> runPage (succ pageNr) dialogHinzufügen mvarStatus dynamischeWidgets
        zeigeNächsteSeite   _page                       (DialogHinzufügen {buttonWeiter})   _mvarStatus _dynamischeWidgets  = widgetHide buttonWeiter
        zeigeVorherigeSeite :: (LikeMVar lmvar) => PageHinzufügen -> DialogHinzufügen -> lmvar StatusGUI -> DynamischeWidgets -> IO ()
        zeigeVorherigeSeite _   dialogHinzufügen    = runPage 0 dialogHinzufügen
        objektHinzufügen :: (LikeMVar lmvar) => DialogHinzufügen -> PageHinzufügen -> lmvar StatusGUI -> DynamischeWidgets -> IO ()
        objektHinzufügen    (DialogHinzufügen {comboBoxZugtyp, indizesZugtyp})  (PageBahngeschwindigkeit {nameEntry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton})   mvarStatus  dynamischeWidgets   = void $ do
            bgName <- get nameEntry entryText
            geschwindigkeitsPin <- get geschwindigkeitsPinSpinButton spinButtonValue >>= pure . toPin
            bahngeschwindigkeit <- get comboBoxZugtyp comboBoxActive >>= \case
                index   | indexStimmtÜberein indizesZugtyp Märklin index    -> pure MärklinBahngeschwindigkeit {bgName, geschwindigkeitsPin}
                        | indexStimmtÜberein indizesZugtyp Lego index       -> do
                            fahrtrichtungsPin <- get fahrtrichtungsPinSpinButton spinButtonValue >>= pure . toPin
                            pure LegoBahngeschwindigkeit {bgName, geschwindigkeitsPin, fahrtrichtungsPin}
                        | otherwise                                         -> error $ "Unbekannter Zugtyp beim Hinzufügen einer Bahngeschwindigkeit ausgewählt: " ++ show index
            bahngeschwindigkeitPackNew bahngeschwindigkeit mvarStatus dynamischeWidgets
        objektHinzufügen    _dialogHinzufügen                                   (PageStreckenabschnitt {nameEntry, stromPinSpinButton})                                             mvarStatus  dynamischeWidgets   = void $ do
            stName <- get nameEntry entryText
            stromPin <- get stromPinSpinButton spinButtonValue >>= pure . toPin
            streckenabschnittPackNew (Streckenabschnitt {stName, stromPin}) mvarStatus dynamischeWidgets
        objektHinzufügen    (DialogHinzufügen {comboBoxZugtyp, indizesZugtyp})  (PageWeiche {nameEntry, richtungsWidgetsMärklin, richtungsWidgetsLego}) mvarStatus  dynamischeWidgets   = void $ do
            weName <- get nameEntry entryText
            weiche <- get comboBoxZugtyp comboBoxActive >>= \case
                index   | indexStimmtÜberein indizesZugtyp Märklin index    -> do
                            richtungsPins <- foldM (\acc (richtung, checkButton, spinButton) -> get checkButton toggleButtonActive >>= \pressed -> if pressed then get spinButton spinButtonValue >>= \pin -> pure ((richtung, toPin pin):acc) else pure acc) [] richtungsWidgetsMärklin
                            case richtungsPins of
                                ([])    -> error "Keine Richtung beim Hinzufügen einer Märklin-Weiche ausgewählt."
                                (h:t)   -> pure MärklinWeiche {weName, richtungsPins=h:|t}
                        | indexStimmtÜberein indizesZugtyp Lego index       -> do
                            let (richtungsPinSpinButton, richtungenRadioButtons) = richtungsWidgetsLego
                            richtungsPin <- get richtungsPinSpinButton spinButtonValue >>= pure . toPin
                            richtungen <- foldM (\(r1, r2) (rn1, rn2, rb) -> get rb toggleButtonActive >>= \toggled -> pure $ if toggled then (rn1, rn2) else (r1, r2)) (Gerade, Gerade) richtungenRadioButtons
                            pure LegoWeiche {weName, richtungsPin, richtungen}
                        | otherwise                                         -> error $ "Unbekannter Zugtyp beim Hinzufügen einer Weiche ausgewählt: " ++ show index
            weichePackNew weiche mvarStatus dynamischeWidgets
        objektHinzufügen    _dialogHinzufügen                                   (PageKupplung {nameEntry, kupplungsPinSpinButton})                                                  mvarStatus  dynamischeWidgets   = void $ do
            kuName <- get nameEntry entryText
            kupplungsPin <- get kupplungsPinSpinButton spinButtonValue >>= pure . toPin
            kupplungPackNew (Kupplung {kuName, kupplungsPin}) mvarStatus dynamischeWidgets
        objektHinzufügen    _dialogHinzufügen                                   (PageWegstrecke {nameEntry, wegstreckenElemente})                                                   mvarStatus  dynamischeWidgets   = void $ do
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
        objektHinzufügen    _dialogHinzufügen                                   (PagePlan {nameEntry, aktionen})                                                                    mvarStatus  dynamischeWidgets   = void $ do
            plName <- get nameEntry entryText
            aktionenQueue <- readLMVar $ aktionen ^. linkedMVarElemente
            planPackNew (Plan {plName, plAktionen=toList aktionenQueue}) mvarStatus dynamischeWidgets
        objektHinzufügen    _dialogHinzufügen                                   page                                                                                                _mvarStatus _dynamischeWidgets   = do
            error $ "Unbekannte Seite während dem Hinzufügen angezeigt: " ++ show page

dialogHinzufügenNew :: (WindowClass w) => w -> DynamischeWidgets -> IO (DialogHinzufügen, LinkedMVar StatusGUI)
dialogHinzufügenNew parent (DynamischeWidgets {vBoxHinzufügenWegstreckeBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeiten, vBoxHinzufügenPlanBahngeschwindigkeitenLego, vBoxHinzufügenPlanBahngeschwindigkeitenMärklin, vBoxHinzufügenWegstreckeStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte, vBoxHinzufügenWegstreckeWeichen, vBoxHinzufügenPlanWeichenGerade, vBoxHinzufügenPlanWeichenKurve, vBoxHinzufügenPlanWeichenLinks, vBoxHinzufügenPlanWeichenRechts, vBoxHinzufügenWegstreckeKupplungen, vBoxHinzufügenPlanKupplungen, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung, mvarPlanObjekt}) = do
    dialog <- dialogNew
    set dialog [windowTitle := (Language.hinzufügen :: Text), windowTransientFor := parent, windowDefaultHeight := 320]
    -- Eigene Hinzufügen-Knöpfe für Seiten, bei denen er temporär deaktiert sein kann
    buttonHinzufügen <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
    buttonHinzufügenWeicheMärklin <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
    buttonHinzufügenWeicheLego <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
    buttonHinzufügenWegstrecke <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
    buttonHinzufügenPlan <- dialogAddButton dialog (Language.hinzufügen :: Text) ResponseOk
    -- ComboBox zur Zugtyp-Auswahl
    buttonBox <- widgetGetParent buttonHinzufügen >>= pure . castToBox . fromJust
    comboBoxZugtyp <- boxPackWidgetNewDefault buttonBox comboBoxNewText
    indexMärklin <- comboBoxAppendText comboBoxZugtyp Language.märklin
    indexLego <-comboBoxAppendText comboBoxZugtyp Language.lego
    let indizesZugtyp = (indexMärklin, Märklin):|(indexLego, Lego):[]
    -- Fluss-Kontrolle des Dialogs
    buttonWeiter <- dialogAddButton dialog (Language.weiter :: Text) ResponseApply
    buttonZurück <- dialogAddButton dialog (Language.zurück :: Text) ResponseReject
    _buttonAbbrechen <- dialogAddButton dialog (Language.abbrechen :: Text) ResponseCancel
    -- Seiten mit Einstellungs-Möglichkeiten
    contentBox <- dialogGetUpper dialog
    (linkedMVar, pages) <- flip State.runStateT empty $ do
        appendPage contentBox $ do
            vBox <- vBoxNew False 0
            rbBahngeschwindigkeit   <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabel (Language.bahngeschwindigkeit :: Text)
            rbStreckenabschnitt     <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.streckenabschnitt :: Text)
            rbWeiche                <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.weiche :: Text)
            rbKupplung              <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.kupplung :: Text)
            rbWegstrecke            <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.wegstrecke :: Text)
            rbPlan                  <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.plan :: Text)
            pure (PageStart {widget=vBox, radioButtons=rbBahngeschwindigkeit:|rbStreckenabschnitt:rbWeiche:rbKupplung:rbWegstrecke:rbPlan:[]}, Nothing)
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
            pure (PageBahngeschwindigkeit {widget, nameEntry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton}, Nothing)
        appendPage contentBox $ do
            -- Streckenabschnitt
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.streckenabschnitt :: Text)
            nameEntry <- nameEntryPackNew widget
            (stromPinWidget, stromPinSpinButton) <- pinSpinBoxNew Language.strom
            boxPackWidgetNewDefault widget $ pure stromPinWidget
            pure (PageStreckenabschnitt {widget, nameEntry, stromPinSpinButton}, Nothing)
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
            richtungsWidgetsMärklin <- fortfahrenWennToggledNew buttonHinzufügenWeicheMärklin $ (\(richtung, _hBox, checkButton, spinButton) -> (richtung, checkButton, spinButton)) <$> richtungsPins
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
            pure (PageWeiche {widget, nameEntry, richtungsWidgetsMärklin, richtungsWidgetsLego}, Nothing)
        appendPage contentBox $ do
            -- Kupplung
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.kupplung :: Text)
            nameEntry <- nameEntryPackNew widget
            (kupplungPinWidget, kupplungsPinSpinButton) <- pinSpinBoxNew Language.kupplung
            boxPackWidgetNewDefault widget $ pure kupplungPinWidget
            pure (PageKupplung {widget, nameEntry, kupplungsPinSpinButton}, Nothing)
        (Just linkedMVar) <- appendPage contentBox $ do
            -- Wegstrecke
            widget <- vBoxNew False 0
            boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.wegstrecke :: Text)
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
            expanderAktionen <-  expanderNew (Language.aktionen :: Text)
            vBoxAktionen <- vBoxNew False 0
            lmvarTemp <- newEmptyLinkedMVar $ \aktionen -> showAktionen vBoxAktionen expanderAktionen aktionenWidgets aktionen >> pure aktionen
            aktionen <- fortfahrenWennGefülltEmptyLinkedMVarNew buttonHinzufügenPlan $ Just lmvarTemp
            let lmvarElemente = aktionen ^. linkedMVarElemente
            -- Hilfsdialog erstellen
            windowObjekte <- windowNew
            set windowObjekte [windowTitle := (Language.aktion :: Text), windowModal := True, windowTransientFor := dialog]
            on windowObjekte deleteEvent $ liftIO $ putLMVar mvarPlanObjekt Nothing >> widgetHide windowObjekte >> pure True
            windowVBox <- containerAddWidgetNew windowObjekte $ vBoxNew False 0
            (windowScrolledWindowBGGeschw   , windowVBoxBGGeschw)   <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowBGUmdrehen , windowVBoxBGUmdrehen) <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowST         , windowVBoxST)         <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWEGerade   , windowVBoxWEGerade)   <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWEKurve    , windowVBoxWEKurve)    <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWELinks    , windowVBoxWELinks)    <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWERechts   , windowVBoxWERechts)   <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowKU         , windowVBoxKU)         <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            (windowScrolledWindowWS         , windowVBoxWS)         <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            boxPackWidgetNewDefault windowVBox $ buttonNewWithEventLabel Language.abbrechen $ putLMVar mvarPlanObjekt Nothing >> widgetHide windowObjekte
            boxPackWidgetNewDefault windowVBoxBGGeschw $ labelNew $ Just $ (Language.bahngeschwindigkeiten :: Text)
            boxPackDefault windowVBoxBGGeschw vBoxHinzufügenPlanBahngeschwindigkeiten
            boxPackWidgetNewDefault windowVBoxBGGeschw $ labelNew $ Just $ (Language.wegstrecken :: Text)
            boxPackDefault windowVBoxBGGeschw vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit
            boxPackWidgetNewDefault windowVBoxBGUmdrehen $ labelNew $ Just $ (Language.bahngeschwindigkeiten :: Text)
            boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanBahngeschwindigkeitenLego
            boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            boxPackWidgetNewDefault windowVBoxBGUmdrehen $ labelNew $ Just $ (Language.wegstrecken :: Text)
            boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            boxPackWidgetNewDefault windowVBoxST $ labelNew $ Just $ (Language.streckenabschnitte :: Text)
            boxPackDefault windowVBoxST vBoxHinzufügenPlanStreckenabschnitte
            boxPackWidgetNewDefault windowVBoxST $ labelNew $ Just $ (Language.wegstrecken :: Text)
            boxPackDefault windowVBoxST vBoxHinzufügenPlanWegstreckenStreckenabschnitt
            boxPackWidgetNewDefault windowVBoxWEGerade $ labelNew $ Just $ (Language.weichen :: Text)
            boxPackDefault windowVBoxWEGerade vBoxHinzufügenPlanWeichenGerade
            boxPackWidgetNewDefault windowVBoxWEKurve $ labelNew $ Just $ (Language.weichen :: Text)
            boxPackDefault windowVBoxWEKurve vBoxHinzufügenPlanWeichenKurve
            boxPackWidgetNewDefault windowVBoxWELinks $ labelNew $ Just $ (Language.weichen :: Text)
            boxPackDefault windowVBoxWELinks vBoxHinzufügenPlanWeichenLinks
            boxPackWidgetNewDefault windowVBoxWERechts $ labelNew $ Just $ (Language.weichen :: Text)
            boxPackDefault windowVBoxWERechts vBoxHinzufügenPlanWeichenRechts
            boxPackWidgetNewDefault windowVBoxKU $ labelNew $ Just $ (Language.kupplungen :: Text)
            boxPackDefault windowVBoxKU vBoxHinzufügenPlanKupplungen
            boxPackWidgetNewDefault windowVBoxKU $ labelNew $ Just $ (Language.wegstrecken :: Text)
            boxPackDefault windowVBoxKU vBoxHinzufügenPlanWegstreckenKupplung
            boxPackWidgetNewDefault windowVBoxWS $ labelNew $ Just $ (Language.wegstrecken :: Text)
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
                modifyLMVar_ lmvarElemente $ pure . (append (Warten $ fromIntegral $ fromEnum wertDouble))
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OBahngeschwindigkeit bg))    -> modifyLMVar_ lmvarElemente $ pure . (append (ABahngeschwindigkeit (Geschwindigkeit bg wert)))
                    (Just (OWegstrecke ws))             -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSBahngeschwindigkeit (Geschwindigkeit ws wert))))
                    _objekt                             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OBahngeschwindigkeit bg))    -> do
                        maybeFahrtrichtung <- case zugtyp bg of
                            (Lego)  -> get vorwärtsRadioButton toggleButtonActive >>= \toggled -> pure $ Just $ if toggled then Vorwärts else Rückwärts
                            _zugtyp -> pure Nothing
                        modifyLMVar_ lmvarElemente $ pure . (append (ABahngeschwindigkeit (Umdrehen bg maybeFahrtrichtung)))
                    (Just (OWegstrecke ws))             -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSBahngeschwindigkeit (Umdrehen ws Nothing))))
                    _objekt                             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            fahrtrichtungsVBox <- boxPackWidgetNewDefault bgFunktionen $ vBoxNew False 0
            on comboBoxZugtyp changed $ do
                index <- get comboBoxZugtyp comboBoxActive
                mapM_ (widgetShowIf (index == indexLego)) [vBoxHinzufügenPlanBahngeschwindigkeitenLego, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego, fahrtrichtungsVBox]
                mapM_ (widgetShowIf (index == indexMärklin)) [vBoxHinzufügenPlanBahngeschwindigkeitenMärklin, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin]
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OStreckenabschnitt st))  -> modifyLMVar_ lmvarElemente $ pure . (append (AStreckenabschnitt (Strom st Fließend)))
                    (Just (OWegstrecke ws))         -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSStreckenabschnitt (Strom ws Fließend))))
                    _objekt                         -> pure ()
                postGUIAsync (widgetHide windowObjekte)
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OStreckenabschnitt st))  -> modifyLMVar_ lmvarElemente $ pure . (append (AStreckenabschnitt (Strom st Gesperrt)))
                    (Just (OWegstrecke ws))         -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSStreckenabschnitt (Strom ws Gesperrt))))
                    _objekt                         -> pure ()
                postGUIAsync (widgetHide windowObjekte)
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWeiche we)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWeiche (Stellen we Gerade)))
                    _objekt             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWeiche we)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWeiche (Stellen we Kurve)))
                    _objekt             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWeiche we)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWeiche (Stellen we Links)))
                    _objekt             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWeiche we)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWeiche (Stellen we Rechts)))
                    _objekt             -> pure ()
                postGUIAsync (widgetHide windowObjekte)
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OKupplung ku))   -> modifyLMVar_ lmvarElemente $ pure . (append (AKupplung (Kuppeln ku)))
                    (Just (OWegstrecke ws)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (AWSKupplung (Kuppeln ws))))
                    _objekt                 -> pure ()
                postGUIAsync (widgetHide windowObjekte)
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
                objekt <- takeLMVar mvarPlanObjekt
                case objekt of
                    (Just (OWegstrecke ws)) -> modifyLMVar_ lmvarElemente $ pure . (append (AWegstrecke (Einstellen ws)))
                    _objekt                 -> pure ()
                postGUIAsync (widgetHide windowObjekte)
            boxPackWidgetNew widget PackGrow paddingDefault positionDefault $ pure expanderAktionen
            scrolledWidgetAddNew expanderAktionen $ widgetShow vBoxAktionen >> pure vBoxAktionen
            set vBoxAktionen [widgetExpand := True]
            boxPackWidgetNewDefault widget $ buttonNewWithEventLabel Language.rückgängig $ modifyLMVar_ (aktionen ^. linkedMVarElemente) $ pure . (\acc -> let prevAcc = case viewLast acc of {(Empty) -> empty; (Filled _l p) -> p} in prevAcc)
            putLMVar lmvarElemente empty
            pure (PagePlan {widget, nameEntry, bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen, aktionen}, Nothing)
        pure linkedMVar
    -- Setze Wert der ComboBox am Ende um davon abhängige Widgets automatisch zu zeigen/verstecken
    comboBoxSetActive comboBoxZugtyp indexMärklin
    pure (DialogHinzufügen {dialog, pages, buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan, buttonWeiter, buttonZurück, comboBoxZugtyp, indizesZugtyp}, linkedMVar)
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
    (Empty)        -> pure Nothing
    (Filled h t)
        | i <= 0    -> do
            widgetShow $ widget h
            mapM_ (widgetHide . widget) t
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

data PageHinzufügen = PageStart {widget :: VBox, radioButtons :: NonEmpty RadioButton}
                    | PageBahngeschwindigkeit {widget :: VBox, nameEntry :: Entry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton :: SpinButton}
                    | PageStreckenabschnitt {widget :: VBox, nameEntry :: Entry, stromPinSpinButton :: SpinButton}
                    | PageWeiche {widget :: VBox, nameEntry :: Entry, richtungsWidgetsMärklin :: FortfahrenWennToggled NonEmpty (Richtung, CheckButton, SpinButton), richtungsWidgetsLego :: (SpinButton, NonEmpty (Richtung, Richtung, RadioButton))}
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
                                    buttonHinzufügenWeicheMärklin :: Button,
                                    buttonHinzufügenWeicheLego :: Button,
                                    buttonHinzufügenWegstrecke :: Button,
                                    buttonHinzufügenPlan :: Button,
                                    buttonWeiter :: Button,
                                    buttonZurück :: Button,
                                    comboBoxZugtyp :: ComboBox,
                                    indizesZugtyp :: NonEmpty (Int, Zugtyp)}

-- * Dialog-spezifische Funktionen
-- | Dialog anzeigen und auswerten
dialogEval :: (DialogClass d) => d -> IO ResponseId
dialogEval dialog = do
    widgetShow dialog
    antwort <- dialogRun dialog
    widgetHide dialog
    pure antwort

-- | dialogGetUpper fehlt in gtk3, daher hier ersetzt
dialogGetUpper :: (DialogClass d) => d -> IO Box
dialogGetUpper dialog = dialogGetActionArea dialog >>= pure . castToBox
#endif