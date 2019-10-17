{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

{-|
Description : Abfragen mit neuem Fenster für das Gtk-UI.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Fenster () where
#else
module Zug.UI.Gtk.Fenster (
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
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes, zeigeErstes)
import Zug.Klassen (Zugtyp(..), Richtung(..), unterstützteRichtungen, Strom(..), Fahrtrichtung(..), ZugtypEither(..))
import Zug.Anbindung (Value(..), Pin(), zuPin, Bahngeschwindigkeit(..), Streckenabschnitt(..),
                    Weiche(..), Kupplung(..), Wegstrecke(..), StreckenObjekt(..))
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (Plan(..), Aktion(..), AktionWegstrecke(..),
                AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionWeiche(..), AktionKupplung(..))
import qualified Zug.Language as Language
import Zug.Language (showText, (<!>), (<:>), (<^>))
import Zug.UI.Base (Status, auswertenTMVarIOStatus,
                    ReaderFamilie, ObjektReader,
                    putBahngeschwindigkeiten, bahngeschwindigkeiten,
                    putStreckenabschnitte, streckenabschnitte,
                    putWeichen, weichen,
                    putKupplungen, kupplungen,
                    putWegstrecken, wegstrecken,
                    putPläne, pläne)
import Zug.UI.Befehl (BefehlKlasse(..), BefehlAllgemein(..), ausführenTMVarBefehl)
import Zug.UI.Gtk.Assistant (Assistant, AssistantSeite(..), AssistantSeitenBaum(..),
                                assistantNew, assistantAuswerten, AssistantResult(..))
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggled, FortfahrenWennToggledTMVar, tmvarCheckButtons,
                                        fortfahrenWennToggledNew, aktiviereWennToggledTMVar,
                                        RegistrierterCheckButton)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventMnemonic, dialogEval, dialogGetUpper,
                                    widgetShowIf)
import Zug.UI.Gtk.Klassen (MitBox(), MitWindow(), MitDialog(), mitContainerRemove)
import Zug.UI.Gtk.StreckenObjekt (StatusGui, BefehlGui, IOStatusGui,
                                    DynamischeWidgets(..), DynamischeWidgetsReader(..),
                                    StatusReader(..), WegstreckenElement(..), WEWidgets,
                                    WidgetsTyp(..))

-- | Speichern des aktuellen 'StatusGui'
buttonSpeichernPack :: (MitBox b, MonadIO m) => Gtk.Window -> b -> TMVar StatusGui -> m Gtk.Button
buttonSpeichernPack windowMain box tmvarStatus = do
    dialogSpeichern <- dialogSpeichernNew windowMain
    boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.speichern $ do
        antwort <- dialogEval dialogSpeichern
        when (antwort == Gtk.ResponseOk) $ void $ do
            (Just dateipfad) <- Gtk.fileChooserGetFilename dialogSpeichern
            _ausführenTMVarBefehl (Speichern dateipfad :: BefehlGui) tmvarStatus

dialogSpeichernNew :: (MonadIO m) => Gtk.Window -> m Gtk.FileChooserDialog
dialogSpeichernNew window = liftIO $ do
    fileChooserDialog <- Gtk.fileChooserDialogNew (Just Language.speichern :: Maybe Text) (Just window) Gtk.FileChooserActionSave [(Language.speichern, Gtk.ResponseOk), (Language.abbrechen, Gtk.ResponseCancel)]
    Gtk.set fileChooserDialog [Gtk.fileChooserDoOverwriteConfirmation := True]
    pure fileChooserDialog

-- | Laden eines neuen 'StatusGui' aus einer Datei
buttonLadenPack :: (MitBox b) => Gtk.Window -> b -> TMVar StatusGui -> DynamischeWidgets -> IO Gtk.Button
buttonLadenPack windowMain box tmvarStatus dynamischeWidgets = do
    dialogLaden <- dialogLadenNew windowMain
    dialogLadenFehler <- dialogLadenFehlerNew windowMain
    boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.laden $ do
        antwort <- dialogEval dialogLaden
        when (antwort == Gtk.ResponseOk) $ void $ do
            Gtk.fileChooserGetFilename dialogLaden >>= \case
                (Nothing)           -> void $ do
                    Gtk.set dialogLadenFehler [Gtk.windowTitle := (Language.nichtGefundeneDatei :: Text)]
                    dialogEval dialogLadenFehler
                (Just dateipfad)    -> void $ do
                    statusInitial <- atomically $ readTMVar tmvarStatus
                    -- neuer Status ist schon in tmvarStatus gespeichert und muss nicht mehr neu gesetzt werden
                    let
                        ladeAktion :: Status -> IO StatusGui
                        ladeAktion = ladeWidgets tmvarStatus dynamischeWidgets
                        fehlerBehandlung :: IOStatusGui ()
                        fehlerBehandlung = liftIO $ void $ do
                            Gtk.set dialogLadenFehler [Gtk.windowTitle := dateipfad]
                            dialogEval dialogLadenFehler
                    flip State.evalStateT statusInitial $ _ausführenBefehl $
                        Laden dateipfad ladeAktion fehlerBehandlung

-- | Passe angezeigte Widgets (inkl. 'StatusGui' in 'LikeMVar') an reinen 'Status' an.
ladeWidgets :: TMVar StatusGui -> DynamischeWidgets -> Status -> IO StatusGui
ladeWidgets tmvarStatus dynamischeWidgets@(DynamischeWidgets {vBoxBahngeschwindigkeiten, vBoxStreckenabschnitte, vBoxWeichen, vBoxKupplungen, vBoxWegstrecken, vBoxPläne}) status = do
    _auswertenTMVarIOStatus löscheWidgets tmvarStatus
    erstelleWidgets tmvarStatus status
        where
            löscheWidgets :: IOStatusGui ()
            löscheWidgets = do
                State.get >>= löscheWidgetsAux
                putBahngeschwindigkeiten []
                putStreckenabschnitte []
                putWeichen []
                putKupplungen []
                putWegstrecken []
                putPläne []
            löscheWidgetsAux :: (DynamischeWidgetsReader r m, MonadIO m) => StatusGui -> m ()
            löscheWidgetsAux status = do
                mapM_ entferneWidgets $ status ^. bahngeschwindigkeiten
                mapM_ entferneWidgets $ status ^. streckenabschnitte
                mapM_ entferneWidgets $ status ^. weichen
                mapM_ entferneWidgets $ status ^. kupplungen
                mapM_ entferneWidgets $ status ^. wegstrecken
                mapM_ entferneWidgets $ status ^. pläne
            erstelleWidgets :: TMVar StatusGui -> Status -> IO StatusGui
            erstelleWidgets tmvarStatus status = do
                mapM_ (\bahngeschwindigkeit -> _bahngeschwindigkeitPackNew bahngeschwindigkeit tmvarStatus dynamischeWidgets) $ reverse $ status ^. bahngeschwindigkeiten
                mapM_ (\streckenabschnitt -> _streckenabschnittPackNew streckenabschnitt tmvarStatus dynamischeWidgets)       $ reverse $ status ^. streckenabschnitte
                mapM_ (\weiche -> _weichePackNew weiche tmvarStatus dynamischeWidgets)                                        $ reverse $ status ^. weichen
                mapM_ (\kupplung -> _kupplungPackNew kupplung tmvarStatus dynamischeWidgets)                                  $ reverse $ status ^. kupplungen
                mapM_ (\wegstrecke -> _wegstreckePackNew wegstrecke tmvarStatus dynamischeWidgets)                            $ reverse $ status ^. wegstrecken
                mapM_ (\plan -> _planPackNew plan tmvarStatus dynamischeWidgets)                                              $ reverse $ status ^. pläne
                atomically $ readTMVar tmvarStatus

dialogLadenNew :: Gtk.Window -> IO Gtk.FileChooserDialog
dialogLadenNew window = Gtk.fileChooserDialogNew (Just Language.laden :: Maybe Text) (Just window) Gtk.FileChooserActionOpen [(Language.laden, Gtk.ResponseOk), (Language.abbrechen, Gtk.ResponseCancel)]

dialogLadenFehlerNew :: Gtk.Window -> IO Gtk.MessageDialog
dialogLadenFehlerNew window = Gtk.messageDialogNew (Just window) [] Gtk.MessageError Gtk.ButtonsOk (Language.nichtGefundeneDatei <!> "" :: Text)

-- | Hinzufügen eines 'StreckenObjekt'
buttonHinzufügenPack :: (MitWindow w, MitBox b) => w -> b -> TMVar StatusGui -> DynamischeWidgets -> IO Gtk.Button
buttonHinzufügenPack parentWindow box tmvarStatus dynamischeWidgets = do
    assistantHinzufügen <- assistantHinzufügenNew parentWindow dynamischeWidgets
    button <- liftIO $ boxPackWidgetNewDefault box $ buttonNewWithEventMnemonic Language.hinzufügen $ do
        objekt <- assistantAuswerten assistantHinzufügen
        -- über /case objekt of/ und individuelle pack-Funktionen lösen?
        _objektPackNew _box objekt
    pure button
    {-
        where
            runPage :: Natural -> DialogHinzufügen -> TMVar StatusGui -> DynamischeWidgets -> IO ()
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
                                widgetShowIf (istLetzteSeite page) buttonWeiter
                                widgetShowIf (istErsteSeite page) buttonZurück
                                reset page
                                optionenAnzeigen page tmvarStatus
                                Gtk.dialogRun dialog >>= \case
                                    Gtk.ResponseOk
                                        -> do
                                            Gtk.widgetHide dialog
                                            objektHinzufügen dialogHinzufügen page tmvarStatus dynamischeWidgets
                                    Gtk.ResponseApply
                                        -> zeigeNächsteSeite page dialogHinzufügen tmvarStatus dynamischeWidgets
                                    Gtk.ResponseReject
                                        -> zeigeVorherigeSeite page dialogHinzufügen tmvarStatus dynamischeWidgets
                                    -- erwartete Rückgabewerte: ResponseCancel, ResponseDeleteEvent -> Breche Hinzufügen ab
                                    _response
                                        -> Gtk.widgetHide dialog
            istErsteSeite :: PageHinzufügen -> Bool
            istErsteSeite   PageStart {}    = True
            istErsteSeite   _               = False
            istLetzteSeite :: PageHinzufügen -> Bool
            istLetzteSeite  PageStart {}    = False
            istLetzteSeite  _               = True
            hinzufügenAktivieren :: DialogHinzufügen -> PageHinzufügen -> IO ()
            hinzufügenAktivieren
                DialogHinzufügen {
                    buttonHinzufügen,
                    buttonHinzufügenWeicheMärklin,
                    buttonHinzufügenWeicheLego,
                    buttonHinzufügenWegstrecke,
                    buttonHinzufügenPlan,
                    comboBoxZugtyp,
                    indizesZugtyp}
                PageWeiche {}
                    = do
                        Gtk.widgetHide buttonHinzufügen
                        index <- Gtk.get comboBoxZugtyp Gtk.comboBoxActive
                        widgetShowIf (indexStimmtÜberein indizesZugtyp Märklin index) buttonHinzufügenWeicheMärklin
                        widgetShowIf (indexStimmtÜberein indizesZugtyp Lego index) buttonHinzufügenWeicheLego
                        Gtk.widgetHide buttonHinzufügenWegstrecke
                        Gtk.widgetHide buttonHinzufügenPlan
            hinzufügenAktivieren
                DialogHinzufügen {
                    buttonHinzufügen,
                    buttonHinzufügenWeicheMärklin,
                    buttonHinzufügenWeicheLego,
                    buttonHinzufügenWegstrecke,
                    buttonHinzufügenPlan}
                PageWegstrecke {}
                    = do
                        Gtk.widgetHide buttonHinzufügen
                        Gtk.widgetHide buttonHinzufügenWeicheMärklin
                        Gtk.widgetHide buttonHinzufügenWeicheLego
                        Gtk.widgetShow buttonHinzufügenWegstrecke
                        Gtk.widgetHide buttonHinzufügenPlan
            hinzufügenAktivieren
                DialogHinzufügen {
                    buttonHinzufügen,
                    buttonHinzufügenWeicheMärklin,
                    buttonHinzufügenWeicheLego,
                    buttonHinzufügenWegstrecke,
                    buttonHinzufügenPlan}
                PagePlan {}
                    = do
                        Gtk.widgetHide buttonHinzufügen
                        Gtk.widgetHide buttonHinzufügenWeicheMärklin
                        Gtk.widgetHide buttonHinzufügenWeicheLego
                        Gtk.widgetHide buttonHinzufügenWegstrecke
                        Gtk.widgetShow buttonHinzufügenPlan
            hinzufügenAktivieren
                DialogHinzufügen {
                    buttonHinzufügen,
                    buttonHinzufügenWeicheMärklin,
                    buttonHinzufügenWeicheLego,
                    buttonHinzufügenWegstrecke,
                    buttonHinzufügenPlan}
                PageStart {}
                    = do
                        Gtk.widgetHide buttonHinzufügen
                        Gtk.widgetHide buttonHinzufügenWeicheMärklin
                        Gtk.widgetHide buttonHinzufügenWeicheLego
                        Gtk.widgetHide buttonHinzufügenWegstrecke
                        Gtk.widgetHide buttonHinzufügenPlan
            hinzufügenAktivieren
                DialogHinzufügen {
                    buttonHinzufügen,
                    buttonHinzufügenWeicheMärklin,
                    buttonHinzufügenWeicheLego,
                    buttonHinzufügenWegstrecke,
                    buttonHinzufügenPlan}
                _page
                    = do
                        Gtk.widgetShow buttonHinzufügen
                        Gtk.widgetHide buttonHinzufügenWeicheMärklin
                        Gtk.widgetHide buttonHinzufügenWeicheLego
                        Gtk.widgetHide buttonHinzufügenWegstrecke
                        Gtk.widgetHide buttonHinzufügenPlan
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
                        PageStart {}
                            -> pure ()
                        _page
                            -> Gtk.set (nameEntry page) [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True]
                    case page of
                        PageWegstrecke {wegstreckenElemente}
                            -> do
                                -- Hinzufügen-Sensitivity neu setzen
                                _aktiviereWennToggledTMVar wegstreckenElemente
                        PagePlan {tvarAktionen, vBoxAktionen, expanderAktionen, tvarLabelAktionen, pgButtonHinzufügenPlan}
                            -> do
                                -- Aktionen zurücksetzen
                                atomically $ writeTVar tvarAktionen leer
                                showAktionen pgButtonHinzufügenPlan vBoxAktionen expanderAktionen tvarLabelAktionen leer
                        _page
                            -> pure ()
            optionenAnzeigen :: PageHinzufügen -> TMVar StatusGui -> IO ()
            optionenAnzeigen
                PagePlan {bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen}
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
            zeigeNächsteSeite :: PageHinzufügen -> DialogHinzufügen -> TMVar StatusGui -> DynamischeWidgets -> IO ()
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
                    = Gtk.widgetHide buttonWeiter
            zeigeVorherigeSeite :: PageHinzufügen -> DialogHinzufügen -> TMVar StatusGui -> DynamischeWidgets -> IO ()
            zeigeVorherigeSeite _page dialogHinzufügen = runPage 0 dialogHinzufügen
            erhalteFließendValue :: DialogHinzufügen -> IO Value
            erhalteFließendValue (DialogHinzufügen {comboBoxFließend, indizesFließend})
                = Gtk.get comboBoxFließend Gtk.comboBoxActive >>= \case
                    index
                        | indexStimmtÜberein indizesFließend HIGH index
                            -> pure HIGH
                        | indexStimmtÜberein indizesFließend LOW index
                            -> pure LOW
                        | otherwise
                            -> error "Unbekannter Fließend-Value ausgewählt."
            objektHinzufügen :: DialogHinzufügen -> PageHinzufügen -> TMVar StatusGui -> DynamischeWidgets -> IO ()
            objektHinzufügen
                dialogHinzufügen@(DialogHinzufügen {comboBoxZugtyp, indizesZugtyp})
                PageBahngeschwindigkeit {nameEntry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton}
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        name <- Gtk.get nameEntry Gtk.entryText
                        fließend <- erhalteFließendValue dialogHinzufügen
                        geschwindigkeitsAnschluss <- _zuPin <$> Gtk.get geschwindigkeitsPinSpinButton Gtk.spinButtonValue
                        bahngeschwindigkeit <- Gtk.get comboBoxZugtyp Gtk.comboBoxActive >>= \case
                            index
                                | indexStimmtÜberein indizesZugtyp Märklin index
                                    -> pure $ ZugtypMärklin MärklinBahngeschwindigkeit {
                                        bgmName = name,
                                        bgmFließend = fließend,
                                        bgmGeschwindigkeitsAnschluss = geschwindigkeitsAnschluss}
                                | indexStimmtÜberein indizesZugtyp Lego index
                                    -> do
                                        fahrtrichtungsAnschluss
                                            <- _zuPin <$> Gtk.get fahrtrichtungsPinSpinButton Gtk.spinButtonValue
                                        pure $ ZugtypLego LegoBahngeschwindigkeit {
                                            bglName = name,
                                            bglFließend = fließend,
                                            bglGeschwindigkeitsAnschluss = geschwindigkeitsAnschluss,
                                            bglFahrtrichtungsAnschluss = fahrtrichtungsAnschluss}
                                | otherwise
                                    -> error $ "Unbekannter Zugtyp beim Hinzufügen einer Bahngeschwindigkeit ausgewählt: " ++ show index
                        _bahngeschwindigkeitPackNew bahngeschwindigkeit tmvarStatus dynamischeWidgets
            objektHinzufügen
                dialogHinzufügen
                PageStreckenabschnitt {nameEntry, stromPinSpinButton}
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        stName <- Gtk.get nameEntry Gtk.entryText
                        stFließend <- erhalteFließendValue dialogHinzufügen
                        stromAnschluss <- _zuPin <$> Gtk.get stromPinSpinButton Gtk.spinButtonValue
                        _streckenabschnittPackNew (Streckenabschnitt {stName, stFließend, stromAnschluss}) tmvarStatus dynamischeWidgets
            objektHinzufügen
                dialogHinzufügen@(DialogHinzufügen {comboBoxZugtyp, indizesZugtyp})
                PageWeiche {nameEntry, richtungsWidgetsMärklin, richtungsWidgetsLego}
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        weName <- Gtk.get nameEntry Gtk.entryText :: IO Text
                        weFließend <- erhalteFließendValue dialogHinzufügen
                        weiche <- Gtk.get comboBoxZugtyp Gtk.comboBoxActive >>= \case
                            index
                                | indexStimmtÜberein indizesZugtyp Märklin index
                                    -> do
                                        let
                                            erhalteToggledRichtungen
                                                :: [(Richtung, Pin)]
                                                -> (Richtung, Gtk.CheckButton, Gtk.SpinButton)
                                                    -> IO [(Richtung, Pin)]
                                            erhalteToggledRichtungen acc (richtung, checkButton, spinButton) = do
                                                Gtk.get checkButton Gtk.toggleButtonActive >>= \case
                                                    True    -> do
                                                        pin <- Gtk.get spinButton Gtk.spinButtonValue
                                                        pure $ (richtung, _zuPin pin) : acc
                                                    False   -> pure acc
                                        richtungsPins <- foldM _erhalteToggledRichtungen [] (_richtungsWidgetsMärklin :: [Weiche 'Märklin])
                                        case richtungsPins of
                                            []      -> error "Keine Richtung beim Hinzufügen einer Märklin-Weiche ausgewählt."
                                            (h : t) -> pure $ ZugtypMärklin MärklinWeiche {
                                                wemName = _,
                                                wemFließend = _,
                                                wemRichtungsAnschlüsse = h :| t}
                                | indexStimmtÜberein indizesZugtyp Lego index
                                    -> do
                                        let (richtungsPinSpinButton, richtungenRadioButtons) = richtungsWidgetsLego
                                        richtungsPin <- _zuPin <$> Gtk.get richtungsPinSpinButton Gtk.spinButtonValue
                                        let
                                            erhalteGewählteRichtungen :: (Richtung, Richtung) -> (Richtung, Richtung, Gtk.RadioButton) -> IO (Richtung, Richtung)
                                            erhalteGewählteRichtungen acc (r1, r2, rb) = do
                                                toggled <- Gtk.get rb Gtk.toggleButtonActive
                                                pure $ if toggled then (r1, r2) else acc
                                        richtungen <- foldM erhalteGewählteRichtungen (Gerade, Gerade) richtungenRadioButtons
                                        pure $ ZugtypLego LegoWeiche {
                                            welName = _,
                                            welFließend = _,
                                            welRichtungsAnschluss = _,
                                            welRichtungen = _}
                                | otherwise
                                    -> error $ "Unbekannter Zugtyp beim Hinzufügen einer Weiche ausgewählt: " ++ show index
                        _weichePackNew weiche tmvarStatus dynamischeWidgets
            objektHinzufügen
                dialogHinzufügen
                PageKupplung {nameEntry, kupplungsPinSpinButton}
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        kuName <- Gtk.get nameEntry Gtk.entryText
                        kuFließend <- erhalteFließendValue dialogHinzufügen
                        kupplungsAnschluss <- _zuPin <$> Gtk.get kupplungsPinSpinButton Gtk.spinButtonValue
                        _kupplungPackNew (Kupplung {kuName, kuFließend, kupplungsAnschluss}) tmvarStatus dynamischeWidgets
            objektHinzufügen
                _dialogHinzufügen
                PageWegstrecke {nameEntry, wegstreckenElemente}
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        wsName <- Gtk.get nameEntry Gtk.entryText
                        wegstreckenElementeCurrent <- atomically $ readTMVar $ wegstreckenElemente ^. _tmvarCheckButtons
                        wsBahngeschwindigkeiten <- foldM (_getToggledWegstreckenElemente $ _pure . _bg) [] $ wegstreckenElementeCurrent ^. bahngeschwindigkeiten
                        wsStreckenabschnitte <- foldM (_getToggledWegstreckenElemente $ _pure . _st) [] $  wegstreckenElementeCurrent ^. streckenabschnitte
                        wsWeichenRichtungen <- foldM (_getToggledWegstreckenElemente _getWeichenRichtung) [] $ wegstreckenElementeCurrent ^. weichen
                        wsKupplungen <- foldM (_getToggledWegstreckenElemente $ _pure . _ku) [] $ wegstreckenElementeCurrent ^. kupplungen
                        _wegstreckePackNew (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen}) tmvarStatus dynamischeWidgets
                            where
                                getToggledWegstreckenElemente :: (WegstreckenElement s) => (s -> IO a) -> [a] -> s -> IO [a]
                                getToggledWegstreckenElemente
                                    selector
                                    acc
                                    element
                                        = Gtk.get (element ^. getterWegstrecke) _Gtk_toggleButtonActive >>= \case
                                                True    -> do
                                                    h <- selector element
                                                    pure $ h : acc
                                                False   -> pure acc
                                getWeichenRichtung :: (WegstreckenElement (WEWidgets z)) =>
                                    WEWidgets z -> IO (Weiche z, Richtung)
                                getWeichenRichtung weWidgets = do
                                    richtung <- getToggledRichtung (weWidgets ^. _getterRichtungsRadioButtons)
                                    pure (erhalteObjektTyp weWidgets, richtung)
                                getToggledRichtung :: NonEmpty (Richtung, Gtk.RadioButton) -> IO Richtung
                                getToggledRichtung  ((richtung, radioButton):|tail) = do
                                    toggled <- Gtk.get radioButton Gtk.toggleButtonActive
                                    if toggled
                                        then pure richtung
                                        else case tail of
                                            (h : t) -> getToggledRichtung $ h:|t
                                            []      -> error "getToggledRichtung ohne ausgewählten Gtk.RadioButton aufgerufen."
            objektHinzufügen
                _dialogHinzufügen
                PagePlan {nameEntry, tvarAktionen}
                tmvarStatus
                dynamischeWidgets
                    = void $ do
                        plName <- Gtk.get nameEntry Gtk.entryText
                        aktionenQueue <- readTVarIO tvarAktionen
                        _planPackNew (Plan {plName, plAktionen = toList aktionenQueue}) tmvarStatus dynamischeWidgets
            objektHinzufügen
                _dialogHinzufügen
                page
                _mvarStatus
                _dynamischeWidgets
                    = error $ "Unbekannte Seite während dem Hinzufügen angezeigt: " ++ show page
    -}

-- | Seiten des Hinzufügen-'Assistant'
data HinzufügenSeite = HinzufügenSeite

-- Durch Assistant ersetzten!
-- | Erstelle einen neuen Hinzufügen-'Assistant'
assistantHinzufügenNew :: (MitWindow w) => w -> DynamischeWidgets -> IO (Assistant HinzufügenSeite Objekt)
assistantHinzufügenNew
    parent
    dynamischeWidgets
        = do
            _assistantErstellen
            -- dialog <- Gtk.dialogNew
            -- Gtk.set dialog [
            --     Gtk.windowTitle := (Language.hinzufügen :: Text),
            --     Gtk.windowTransientFor := parent,
            --     Gtk.windowDefaultHeight := 320]
            -- -- Eigene Hinzufügen-Knöpfe für Seiten, bei denen er temporär deaktiert sein kann
            -- buttonHinzufügen <- dialogAddButton dialog (Language.hinzufügen :: Text) Gtk.ResponseOk
            -- buttonHinzufügenWeicheMärklin <- dialogAddButton dialog (Language.hinzufügen :: Text) Gtk.ResponseOk
            -- buttonHinzufügenWeicheLego <- dialogAddButton dialog (Language.hinzufügen :: Text) Gtk.ResponseOk
            -- let buttonHinzufügenWegstrecke = fortfahrenWennToggledWegstrecke ^. fortfahrenButton
            -- dialogAddActionWidget dialog buttonHinzufügenWegstrecke Gtk.ResponseOk
            -- buttonHinzufügenPlan <- dialogAddButton dialog (Language.hinzufügen :: Text) Gtk.ResponseOk
            -- -- ComboBox zur Zugtyp-Auswahl
            -- buttonBox <- widgetGetParent buttonHinzufügen >>= pure . castToBox . fromJust
            -- comboBoxFließend <- boxPackWidgetNewDefault buttonBox comboBoxNewText
            -- indexHigh <- comboBoxAppendText comboBoxFließend Language.high
            -- indexLow <- comboBoxAppendText comboBoxFließend Language.low
            -- let indizesFließend = (indexHigh, HIGH) :| (indexLow, LOW) : []
            -- comboBoxZugtyp <- boxPackWidgetNewDefault buttonBox comboBoxNewText
            -- indexMärklin <- comboBoxAppendText comboBoxZugtyp Language.märklin
            -- indexLego <-comboBoxAppendText comboBoxZugtyp Language.lego
            -- let indizesZugtyp = (indexMärklin, Märklin) :| (indexLego, Lego) : []
            -- -- Fluss-Kontrolle des Dialogs
            -- buttonWeiter <- dialogAddButton dialog (Language.weiter :: Text) Gtk.ResponseApply
            -- buttonZurück <- dialogAddButton dialog (Language.zurück :: Text) Gtk.ResponseReject
            -- _buttonAbbrechen <- dialogAddButton dialog (Language.abbrechen :: Text) ResponseCancel
            -- -- Seiten mit Einstellungs-Möglichkeiten
            -- contentBox <- dialogGetUpper dialog
            -- pages <- flip State.execStateT leer $ do
            --     appendPage contentBox $ do
            --         vBox <- vBoxNew False 0
            --         rbBahngeschwindigkeit   <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabel (Language.bahngeschwindigkeit :: Text)
            --         rbStreckenabschnitt     <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.streckenabschnitt :: Text)
            --         rbWeiche                <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.weiche :: Text)
            --         rbKupplung              <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.kupplung :: Text)
            --         rbWegstrecke            <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.wegstrecke :: Text)
            --         rbPlan                  <- boxPackWidgetNewDefault vBox $ radioButtonNewWithLabelFromWidget rbBahngeschwindigkeit (Language.plan :: Text)
            --         let 
            --             radioButtons :: NonEmpty Gtk.RadioButton
            --             radioButtons
            --                 = rbBahngeschwindigkeit :|
            --                     rbStreckenabschnitt :
            --                     rbWeiche :
            --                     rbKupplung :
            --                     rbWegstrecke :
            --                     rbPlan : []
            --         pure PageStart {widget=vBox, radioButtons}
            --     appendPage contentBox $ do
            --         -- Bahngeschwindigkeit
            --         widget <- vBoxNew False 0
            --         boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.bahngeschwindigkeit :: Text)
            --         nameEntry <- nameEntryPackNew widget
            --         (geschwindigkeitsPinWidget, geschwindigkeitsPinSpinButton) <- pinSpinBoxNew Language.geschwindigkeit
            --         boxPackWidgetNewDefault widget $ pure geschwindigkeitsPinWidget
            --         -- Zeige Fahrtrichtungs-Pin nicht für Märklin-Bahngeschwindigkeit an
            --         (fahrtrichtungsPinWidget, fahrtrichtungsPinSpinButton) <- pinSpinBoxNew Language.fahrtrichtung
            --         boxPackWidgetNewDefault widget $ pure fahrtrichtungsPinWidget
            --         on comboBoxZugtyp changed $ do
            --             index <- get comboBoxZugtyp comboBoxActive
            --             widgetShowIf (index == indexLego) fahrtrichtungsPinWidget
            --         pure PageBahngeschwindigkeit {widget, nameEntry, geschwindigkeitsPinSpinButton, fahrtrichtungsPinSpinButton}
            --     appendPage contentBox $ do
            --         -- Streckenabschnitt
            --         widget <- vBoxNew False 0
            --         boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.streckenabschnitt :: Text)
            --         nameEntry <- nameEntryPackNew widget
            --         (stromPinWidget, stromPinSpinButton) <- pinSpinBoxNew Language.strom
            --         boxPackWidgetNewDefault widget $ pure stromPinWidget
            --         pure PageStreckenabschnitt {widget, nameEntry, stromPinSpinButton}
            --     appendPage contentBox $ do
            --         -- Weiche
            --         widget <- vBoxNew False 0
            --         boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.weiche :: Text)
            --         nameEntry <- nameEntryPackNew widget
            --         (richtungsPinWidget, richtungsPinSpinButton) <- pinSpinBoxNew Language.richtung
            --         boxPackWidgetNewDefault widget $ pure richtungsPinWidget
            --         let
            --             createRichtungsPin :: Richtung -> IO (Richtung, Gtk.HBox, Gtk.CheckButton, Gtk.SpinButton)
            --             createRichtungsPin richtung = do
            --                 hBox <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            --                 checkButton <- boxPackWidgetNewDefault hBox checkButtonNew
            --                 (pinWidget, spinButton) <- pinSpinBoxNew $ showText richtung
            --                 boxPackWidgetNewDefault hBox $ pure pinWidget
            --                 pure (richtung, hBox, checkButton, spinButton)
            --             createRichtungenRadioButton :: Maybe (NonEmpty (Richtung, Richtung, Gtk.RadioButton)) -> (Richtung, Richtung) -> IO (Maybe (NonEmpty (Richtung, Richtung, Gtk.RadioButton)))
            --             createRichtungenRadioButton (Nothing)               richtungen@(richtung1, richtung2)   = do
            --                 radioButton <- boxPackWidgetNewDefault widget $ radioButtonNewWithLabel $ getRichtungenText richtungen
            --                 pure $ Just $ (richtung1, richtung2, radioButton):|[]
            --             createRichtungenRadioButton (Just (h@(_,_,rb):|t))  richtungen@(richtung1, richtung2)   = do
            --                 radioButton <- boxPackWidgetNewDefault widget $ radioButtonNewWithLabelFromWidget rb $ getRichtungenText richtungen
            --                 pure $ Just $ (richtung1, richtung2, radioButton):|h:t
            --             getRichtungenText :: (Richtung, Richtung) -> Text
            --             getRichtungenText (richtung1, richtung2) = showText richtung1 <^> showText richtung2
            --         richtungsPins <- mapM createRichtungsPin unterstützteRichtungen
            --         richtungsWidgetsMärklin <- fortfahrenWennToggledNew buttonHinzufügenWeicheMärklin $
            --             (\(richtung, _hBox, checkButton, spinButton) -> (richtung, checkButton, spinButton)) <$> richtungsPins
            --         let
            --             richtungsKombinationen :: NonEmpty (Richtung, Richtung)
            --             richtungsKombinationen = dreiecksKombinationen unterstützteRichtungen
            --             -- Wenn gespiegelte Kombinationen gewünscht werden nutze Applicative-Instanz
            --             -- richtungsKombinationen = (,) <&> unterstützteRichtungen <*> unterstützteRichtungen
            --             dreiecksKombinationen :: NonEmpty a -> NonEmpty (a, a)
            --             dreiecksKombinationen   (h:|[])     = (h, h) :| []
            --             dreiecksKombinationen   (h:|s:[])   = (h, s) :| []
            --             dreiecksKombinationen   (h:|t)      = let tNE = NE.fromList t in ((,) h <$> tNE) <> dreiecksKombinationen tNE
            --         richtungsRadioButtons <- foldM createRichtungenRadioButton Nothing richtungsKombinationen >>= pure . fromJust
            --         let richtungsWidgetsLego = (richtungsPinSpinButton, richtungsRadioButtons)
            --         on comboBoxZugtyp changed $ do
            --             index <- get comboBoxZugtyp comboBoxActive
            --             visible <- get widget widgetVisible
            --             widgetShowIf (visible && index == indexMärklin) buttonHinzufügenWeicheMärklin
            --             mapM_ (\(_,hBox,_,_) -> widgetShowIf (index == indexMärklin) hBox) richtungsPins
            --             widgetShowIf (visible && index == indexLego) buttonHinzufügenWeicheLego
            --             widgetShowIf (index == indexLego) richtungsPinWidget
            --             mapM_ (\(_,_,rb) -> widgetShowIf (index == indexLego) rb) richtungsRadioButtons
            --         pure PageWeiche {widget, nameEntry, richtungsWidgetsMärklin, richtungsWidgetsLego}
            --     appendPage contentBox $ do
            --         -- Kupplung
            --         widget <- vBoxNew False 0
            --         boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.kupplung :: Text)
            --         nameEntry <- nameEntryPackNew widget
            --         (kupplungPinWidget, kupplungsPinSpinButton) <- pinSpinBoxNew Language.kupplung
            --         boxPackWidgetNewDefault widget $ pure kupplungPinWidget
            --         pure PageKupplung {widget, nameEntry, kupplungsPinSpinButton}
            --     appendPage contentBox $ do
            --         -- Wegstrecke
            --         widget <- vBoxNew False 0
            --         boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.wegstrecke :: Text)
            --         nameEntry <- nameEntryPackNew widget
            --         notebook <- boxPackWidgetNew widget PackGrow paddingDefault positionDefault notebookNew
            --         scrolledWidgedNotebookAppendPageNew notebook Language.bahngeschwindigkeiten $ pure vBoxHinzufügenWegstreckeBahngeschwindigkeiten
            --         scrolledWidgedNotebookAppendPageNew notebook Language.streckenabschnitte $ pure vBoxHinzufügenWegstreckeStreckenabschnitte
            --         scrolledWidgedNotebookAppendPageNew notebook Language.weichen $ pure vBoxHinzufügenWegstreckeWeichen
            --         scrolledWidgedNotebookAppendPageNew notebook Language.kupplungen $ pure vBoxHinzufügenWegstreckeKupplungen
            --         pure PageWegstrecke {widget, nameEntry, wegstreckenElemente=fortfahrenWennToggledWegstrecke}
            --     appendPage contentBox $ do
            --         -- Plan
            --         -- Objekt-Buttons schreiben bei Druck Objekt in tmvarPlanObjekt
            --         -- Sobald diese gefüllt ist kann die Aktion zur aktionen-TVar hinzufgefügt werden
            --         tvarLabelAktionen <- newTVarIO []
            --         tvarAktionen <- newTVarIO leer
            --         expanderAktionen <- expanderNew (Language.aktionen :: Text)
            --         vBoxAktionen <- vBoxNew False 0
            --         let
            --             showAktionenSpezifisch :: IO ()
            --             showAktionenSpezifisch = do
            --                 aktionen <- readTVarIO tvarAktionen
            --                 showAktionen buttonHinzufügenPlan vBoxAktionen expanderAktionen tvarLabelAktionen aktionen
            --         -- Hilfsdialog erstellen
            --         windowObjekte <- windowNew
            --         set windowObjekte [
            --             windowTitle := (Language.aktion :: Text),
            --             windowModal := True,
            --             windowTransientFor := dialog]
            --         on windowObjekte deleteEvent $ liftIO $ do
            --             atomically $ putTMVar tmvarPlanObjekt Nothing
            --             widgetHide windowObjekte
            --             pure True
            --         windowVBox <- containerAddWidgetNew windowObjekte $ vBoxNew False 0
            --         (windowScrolledWindowBGGeschw, windowVBoxBGGeschw)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         (windowScrolledWindowBGUmdrehen, windowVBoxBGUmdrehen)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         (windowScrolledWindowST, windowVBoxST)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         (windowScrolledWindowWEGerade, windowVBoxWEGerade)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         (windowScrolledWindowWEKurve, windowVBoxWEKurve)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         (windowScrolledWindowWELinks, windowVBoxWELinks)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         (windowScrolledWindowWERechts, windowVBoxWERechts)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         (windowScrolledWindowKU, windowVBoxKU)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         (windowScrolledWindowWS, windowVBoxWS)
            --             <- scrolledWidgetPackNew windowVBox $ vBoxNew False 0
            --         boxPackWidgetNewDefault windowVBox $ buttonNewWithEventLabel Language.abbrechen $ do
            --             atomically $ putTMVar tmvarPlanObjekt Nothing
            --             widgetHide windowObjekte
            --         boxPackWidgetNewDefault windowVBoxBGGeschw $ labelNew $
            --             Just $ (Language.bahngeschwindigkeiten :: Text)
            --         boxPackDefault windowVBoxBGGeschw vBoxHinzufügenPlanBahngeschwindigkeiten
            --         boxPackWidgetNewDefault windowVBoxBGGeschw $ labelNew $
            --             Just $ (Language.wegstrecken :: Text)
            --         boxPackDefault windowVBoxBGGeschw vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit
            --         boxPackWidgetNewDefault windowVBoxBGUmdrehen $ labelNew $
            --             Just $ (Language.bahngeschwindigkeiten :: Text)
            --         boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanBahngeschwindigkeitenLego
            --         boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            --         boxPackWidgetNewDefault windowVBoxBGUmdrehen $ labelNew $
            --             Just $ (Language.wegstrecken :: Text)
            --         boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            --         boxPackDefault windowVBoxBGUmdrehen vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            --         boxPackWidgetNewDefault windowVBoxST $ labelNew $
            --             Just $ (Language.streckenabschnitte :: Text)
            --         boxPackDefault windowVBoxST vBoxHinzufügenPlanStreckenabschnitte
            --         boxPackWidgetNewDefault windowVBoxST $ labelNew $
            --             Just $ (Language.wegstrecken :: Text)
            --         boxPackDefault windowVBoxST vBoxHinzufügenPlanWegstreckenStreckenabschnitt
            --         boxPackWidgetNewDefault windowVBoxWEGerade $ labelNew $
            --             Just $ (Language.weichen :: Text)
            --         boxPackDefault windowVBoxWEGerade vBoxHinzufügenPlanWeichenGerade
            --         boxPackWidgetNewDefault windowVBoxWEKurve $ labelNew $
            --             Just $ (Language.weichen :: Text)
            --         boxPackDefault windowVBoxWEKurve vBoxHinzufügenPlanWeichenKurve
            --         boxPackWidgetNewDefault windowVBoxWELinks $ labelNew $
            --             Just $ (Language.weichen :: Text)
            --         boxPackDefault windowVBoxWELinks vBoxHinzufügenPlanWeichenLinks
            --         boxPackWidgetNewDefault windowVBoxWERechts $ labelNew $
            --             Just $ (Language.weichen :: Text)
            --         boxPackDefault windowVBoxWERechts vBoxHinzufügenPlanWeichenRechts
            --         boxPackWidgetNewDefault windowVBoxKU $ labelNew $
            --             Just $ (Language.kupplungen :: Text)
            --         boxPackDefault windowVBoxKU vBoxHinzufügenPlanKupplungen
            --         boxPackWidgetNewDefault windowVBoxKU $ labelNew $
            --             Just $ (Language.wegstrecken :: Text)
            --         boxPackDefault windowVBoxKU vBoxHinzufügenPlanWegstreckenKupplung
            --         boxPackWidgetNewDefault windowVBoxWS $ labelNew $
            --             Just $ (Language.wegstrecken :: Text)
            --         boxPackDefault windowVBoxWS vBoxHinzufügenPlanWegstreckenWeiche
            --         -- Widget erstellen
            --         widget <- vBoxNew False 0
            --         boxPackWidgetNewDefault widget $ labelNew $ Just $ (Language.plan :: Text)
            --         nameEntry <- nameEntryPackNew widget
            --         functionBox <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            --         let µsInS = 1000000
            --         spinButtonZeit <- spinButtonNewWithRange 0 (60*µsInS) (1*µsInS)
            --         boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.warten $ do
            --             wertDouble <- get spinButtonZeit spinButtonValue
            --             atomically $ modifyTVar tvarAktionen $ anhängen $ Warten $ fromIntegral $ fromEnum wertDouble
            --             showAktionenSpezifisch
            --         boxPackWidgetNewDefault functionBox $ pure spinButtonZeit
            --         boxPackWidgetNewDefault functionBox $ labelNew $ Just $ (Language.wartenEinheit :: Text)
            --         bgFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            --         hScaleGeschwindigkeit <- hScaleNewWithRange 0 100 1
            --         boxPackWidgetNewDefault bgFunktionen $ buttonNewWithEventLabel Language.geschwindigkeit $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetShow windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetHide windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             wertDouble <- get hScaleGeschwindigkeit rangeValue
            --             let wert = fromIntegral $ fromEnum wertDouble
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OBahngeschwindigkeit bg))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         ABahngeschwindigkeit $ Geschwindigkeit bg wert
            --                 (Just (OWegstrecke ws))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWegstrecke $ AWSBahngeschwindigkeit $ Geschwindigkeit ws wert
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         boxPackWidgetNew bgFunktionen PackGrow paddingDefault positionDefault $ pure hScaleGeschwindigkeit
            --         vorwärtsRadioButton <- radioButtonNewWithLabel (Language.vorwärts :: Text)
            --         rückwärtsRadioButton <- radioButtonNewWithLabelFromWidget vorwärtsRadioButton (Language.rückwärts :: Text)
            --         boxPackWidgetNewDefault bgFunktionen $ buttonNewWithEventLabel Language.umdrehen $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetShow windowScrolledWindowBGUmdrehen
            --                 widgetHide windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OBahngeschwindigkeit bg))
            --                     -> do
            --                         maybeFahrtrichtung <- case zugtyp bg of
            --                             Lego    -> do
            --                                 toggled <- get vorwärtsRadioButton toggleButtonActive
            --                                 pure $ Just $ if toggled then Vorwärts else Rückwärts
            --                             _zugtyp -> pure Nothing
            --                         atomically $ modifyTVar tvarAktionen $ anhängen $
            --                             ABahngeschwindigkeit $ Umdrehen bg maybeFahrtrichtung
            --                 (Just (OWegstrecke ws))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWegstrecke $ AWSBahngeschwindigkeit $ Umdrehen ws Nothing
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         fahrtrichtungsVBox <- boxPackWidgetNewDefault bgFunktionen $ vBoxNew False 0
            --         on comboBoxZugtyp changed $ do
            --             index <- get comboBoxZugtyp comboBoxActive
            --             mapM_ (widgetShowIf (index == indexLego)) [
            --                 vBoxHinzufügenPlanBahngeschwindigkeitenLego,
            --                 vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
            --                 fahrtrichtungsVBox]
            --             mapM_ (widgetShowIf (index == indexMärklin)) [
            --                 vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
            --                 vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin]
            --         boxPackWidgetNewDefault fahrtrichtungsVBox $ pure vorwärtsRadioButton
            --         boxPackWidgetNewDefault fahrtrichtungsVBox $ pure rückwärtsRadioButton
            --         stFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            --         boxPackWidgetNewDefault stFunktionen $ buttonNewWithEventLabel (Language.strom <:> Language.an) $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetShow windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OStreckenabschnitt st))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AStreckenabschnitt $ Strom st Fließend
            --                 (Just (OWegstrecke ws))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWegstrecke $ AWSStreckenabschnitt $ Strom ws Fließend
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         boxPackWidgetNewDefault stFunktionen $ buttonNewWithEventLabel (Language.strom <:> Language.aus) $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetShow windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             atomically $ case objekt of
            --                 (Just (OStreckenabschnitt st))
            --                     -> modifyTVar tvarAktionen $ anhängen $
            --                         AStreckenabschnitt $ Strom st Gesperrt
            --                 (Just (OWegstrecke ws))
            --                     -> modifyTVar tvarAktionen $ anhängen $
            --                         AWegstrecke $ AWSStreckenabschnitt $ Strom ws Gesperrt
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         weFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            --         boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.gerade) $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetHide windowScrolledWindowST
            --                 widgetShow windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OWeiche we))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWeiche $ Stellen we Gerade
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.kurve) $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetHide windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetShow windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OWeiche we))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWeiche $ Stellen we Kurve
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.links) $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetHide windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetShow windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OWeiche we))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWeiche $ Stellen we Links
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         boxPackWidgetNewDefault weFunktionen $ buttonNewWithEventLabel (Language.stellen <:> Language.rechts) $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetHide windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetShow windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OWeiche we))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWeiche $ Stellen we Rechts
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         kuFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            --         boxPackWidgetNewDefault kuFunktionen $ buttonNewWithEventLabel Language.kuppeln $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetHide windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetShow windowScrolledWindowKU
            --                 widgetHide windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OKupplung ku))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AKupplung $ Kuppeln ku
            --                 (Just (OWegstrecke ws))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWegstrecke $ AWSKupplung $ Kuppeln ws
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         wsFunktionen <- boxPackWidgetNewDefault widget $ hBoxNew False 0
            --         boxPackWidgetNewDefault wsFunktionen $ buttonNewWithEventLabel Language.einstellen $ void $ forkIO $ do
            --             postGuiAsync $ do
            --                 widgetShow windowObjekte
            --                 widgetHide windowScrolledWindowBGGeschw
            --                 widgetHide windowScrolledWindowBGUmdrehen
            --                 widgetHide windowScrolledWindowST
            --                 widgetHide windowScrolledWindowWEGerade
            --                 widgetHide windowScrolledWindowWEKurve
            --                 widgetHide windowScrolledWindowWELinks
            --                 widgetHide windowScrolledWindowWERechts
            --                 widgetHide windowScrolledWindowKU
            --                 widgetShow windowScrolledWindowWS
            --             objekt <- atomically $ takeTMVar tmvarPlanObjekt
            --             case objekt of
            --                 (Just (OWegstrecke ws))
            --                     -> atomically $ modifyTVar tvarAktionen $ anhängen $
            --                         AWegstrecke $ Einstellen ws
            --                 _objekt
            --                     -> pure ()
            --             postGuiAsync $ do
            --                 widgetHide windowObjekte
            --                 showAktionenSpezifisch
            --         boxPackWidgetNew widget PackGrow paddingDefault positionDefault $ pure expanderAktionen
            --         scrolledWidgetAddNew expanderAktionen $ widgetShow vBoxAktionen >> pure vBoxAktionen
            --         set vBoxAktionen [widgetExpand := True]
            --         let
            --             entferneLetzteAktion :: Warteschlange Aktion -> Warteschlange Aktion
            --             entferneLetzteAktion aktionen = case zeigeLetztes aktionen of
            --                 Leer            -> leer
            --                 (Gefüllt _l p)  -> p
            --         boxPackWidgetNewDefault widget $ buttonNewWithEventLabel Language.rückgängig $ do
            --             atomically $ modifyTVar tvarAktionen entferneLetzteAktion
            --             showAktionenSpezifisch
            --         showAktionenSpezifisch
            --         pure PagePlan {
            --             widget,
            --             nameEntry,
            --             bgFunktionen,
            --             stFunktionen,
            --             weFunktionen,
            --             kuFunktionen,
            --             wsFunktionen,
            --             tvarAktionen,
            --             expanderAktionen,
            --             tvarLabelAktionen,
            --             vBoxAktionen,
            --             pgButtonHinzufügenPlan=buttonHinzufügenPlan}
            -- -- Setze Wert der ComboBox am Ende um davon abhängige Widgets automatisch zu zeigen/verstecken
            -- comboBoxSetActive comboBoxFließend indexLow
            -- comboBoxSetActive comboBoxZugtyp indexMärklin
            -- pure DialogHinzufügen {dialog, pages, buttonHinzufügen, buttonHinzufügenWeicheMärklin, buttonHinzufügenWeicheLego, buttonHinzufügenWegstrecke, buttonHinzufügenPlan, buttonWeiter, buttonZurück, comboBoxZugtyp, indizesZugtyp, comboBoxFließend, indizesFließend}
            --     where
            --         appendPage :: (MitBox b, Monad m, MonadIO m) => b -> m PageHinzufügen -> StateT (Warteschlange PageHinzufügen) m ()
            --         appendPage box konstruktor = do
            --             page <- lift konstruktor
            --             liftIO $ boxPack box (widget page) PackGrow paddingDefault positionDefault
            --             State.modify $ anhängen page

-- | Zeige 'Aktion'en richtig an.
showAktionen :: (MitBox b, Foldable t) => Gtk.Button -> b -> Gtk.Expander -> TVar [Gtk.Label] -> t Aktion -> IO ()
showAktionen buttonHinzufügen box expander tvarWidgets aktionen = do
    widgets <- readTVarIO tvarWidgets
    forM_ widgets $ mitContainerRemove box
    widgetsNeu <- mapM (boxPackWidgetNewDefault box . Gtk.labelNew . Just . show) $ toList aktionen
    Gtk.set expander [Gtk.expanderLabel := Language.aktionen <:> show (length aktionen)]
    atomically $ writeTVar tvarWidgets widgetsNeu
    Gtk.set buttonHinzufügen [Gtk.widgetSensitive := not $ null aktionen]

-- | Zeige nur die i-te Seite (start bei i=0) an
showNth :: Natural -> Warteschlange PageHinzufügen -> IO (Maybe PageHinzufügen)
showNth i queue = case zeigeErstes queue of
    (Leer)          -> pure Nothing
    (Gefüllt h t)
        | i <= 0    -> do
            Gtk.widgetShow $ widget h
            forM_ t $ Gtk.widgetHide . widget
            pure $ Just h
        | otherwise -> do
            Gtk.widgetHide $ widget h
            showNth (pred i) t

-- | Gebe den Index des ersten eingeschalteten Radiobuttons an. Wenn kein Gtk.RadioButton an ist, gebe 0 zurück.
erhalteToggledIndex :: NonEmpty Gtk.RadioButton -> IO Natural
erhalteToggledIndex (h:|t) = do
    toggled <- Gtk.get h Gtk.toggleButtonActive
    if toggled
        then pure 0
        else succ <$> (maybe (pure 0) erhalteToggledIndex $ nonEmpty t)

data PageHinzufügen
    = PageStart {
        widget :: Gtk.VBox,
        radioButtons :: NonEmpty Gtk.RadioButton}
    | PageBahngeschwindigkeit {
        widget :: Gtk.VBox,
        nameEntry :: Gtk.Entry,
        geschwindigkeitsPinSpinButton,
        fahrtrichtungsPinSpinButton :: Gtk.SpinButton}
    | PageStreckenabschnitt {
        widget :: Gtk.VBox,
        nameEntry :: Gtk.Entry,
        stromPinSpinButton :: Gtk.SpinButton}
    | PageWeiche {
        widget :: Gtk.VBox,
        nameEntry :: Gtk.Entry,
        richtungsWidgetsMärklin :: FortfahrenWennToggled,
        richtungsWidgetsLego :: (Gtk.SpinButton, NonEmpty (Richtung, Richtung, Gtk.RadioButton))}
    | PageKupplung {
        widget :: Gtk.VBox,
        nameEntry :: Gtk.Entry,
        kupplungsPinSpinButton :: Gtk.SpinButton}
    | PageWegstrecke {
        widget :: Gtk.VBox,
        nameEntry :: Gtk.Entry,
        wegstreckenElemente :: FortfahrenWennToggledTMVar StatusGui RegistrierterCheckButton}
    | PagePlan {
        widget :: Gtk.VBox,
        nameEntry :: Gtk.Entry,
        bgFunktionen, stFunktionen, weFunktionen, kuFunktionen, wsFunktionen :: Gtk.HBox,
        tvarAktionen :: TVar (Warteschlange Aktion),
        vBoxAktionen :: Gtk.VBox,
        expanderAktionen :: Gtk.Expander,
        tvarLabelAktionen :: TVar [Gtk.Label],
        pgButtonHinzufügenPlan :: Gtk.Button}

instance Show PageHinzufügen where
    show (PageStart {})                 = "PageStart"
    show (PageBahngeschwindigkeit {})   = "PageBahngeschwindigkeit"
    show (PageStreckenabschnitt {})     = "PageStreckenabschnitt"
    show (PageWeiche {})                = "PageWeiche"
    show (PageKupplung {})              = "PageKupplung"
    show (PageWegstrecke {})            = "PageWegstrecke"
    show (PagePlan {})                  = "PagePlan"

data DialogHinzufügen
    = DialogHinzufügen {
        dialog :: Gtk.Dialog,
        pages :: Warteschlange PageHinzufügen,
        buttonHinzufügen :: Gtk.Button,
        buttonHinzufügenWeicheMärklin :: Gtk.Button,
        buttonHinzufügenWeicheLego :: Gtk.Button,
        buttonHinzufügenWegstrecke :: Gtk.Button,
        buttonHinzufügenPlan :: Gtk.Button,
        buttonWeiter :: Gtk.Button,
        buttonZurück :: Gtk.Button,
        comboBoxZugtyp :: Gtk.ComboBox,
        indizesZugtyp :: NonEmpty (Int, Zugtyp),
        comboBoxFließend :: Gtk.ComboBox,
        indizesFließend :: NonEmpty (Int, Value)}
#endif