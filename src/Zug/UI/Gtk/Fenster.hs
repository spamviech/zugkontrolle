{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Concurrent.STM (
    atomically, TMVar, readTMVar, takeTMVar, putTMVar,
    TVar, newTVarIO, readTVarIO, readTVar, writeTVar)
import Control.Lens ((^.))
import Control.Monad (void, when, foldM, forM, forM_)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.RWS (runRWST)
import qualified Control.Monad.RWS as RWS
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybe, fromJust, isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes)
import Zug.Enums (
    Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..),
    Richtung(..), unterstützteRichtungen, Fahrtrichtung(..), Strom(..))
import Zug.Anbindung (
    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(),
    Streckenabschnitt(..), StreckenabschnittKlasse(),
    Weiche(..),
    Kupplung(..), KupplungKlasse(),
    Wegstrecke(..), WegstreckeKlasse(),
    Wartezeit(..))
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (
    Plan(..), Aktion(..), AktionWegstrecke(..),
    AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionWeiche(..), AktionKupplung(..))
import qualified Zug.Language as Language
import Zug.Language (MitSprache(..), showText, (<!>), (<:>))
import Zug.UI.Base (
    Status, auswertenTMVarIOStatus, auswertenTMVarMStatusT,
    ObjektReader, sprache,
    putBahngeschwindigkeiten, bahngeschwindigkeiten,
    putStreckenabschnitte, streckenabschnitte,
    putWeichen, weichen,
    putKupplungen, kupplungen,
    putWegstrecken, wegstrecken,
    putPläne, pläne)
import Zug.UI.Befehl (BefehlAllgemein(..), BefehlKlasse(..), ausführenTMVarBefehl)
import Zug.UI.Gtk.Anschluss (AnschlussAuswahlWidget, anschlussAuswahlNew, aktuellerAnschluss)
import Zug.UI.Gtk.Assistant (
    Assistant, AssistantSeite(..), SeitenAbschluss(..), AssistantSeitenBaum(..),
    assistantNew, assistantAuswerten, AssistantResult(..))
import Zug.UI.Gtk.Auswahl (
    AuswahlWidget, auswahlComboBoxNew, boundedEnumAuswahlComboBoxNew,
    boundedEnumAuswahlRadioButtonNew,
    aktuelleAuswahl, MitAuswahlWidget())
import Zug.UI.Gtk.Fliessend (FließendAuswahlWidget, fließendAuswahlNew, aktuellerFließendValue)
import Zug.UI.Gtk.FortfahrenWennToggled (
    checkButtons, fortfahrenWennToggledNew, aktiviereWennToggledTMVar,
    RegistrierterCheckButton, registrierterCheckButtonToggled)
import Zug.UI.Gtk.Hilfsfunktionen (
    boxPackWidgetNewDefault, boxPackDefault, widgetShowNew, containerAddWidgetNew,
    boxPackWidgetNew, Packing(..), paddingDefault, positionDefault,
    buttonNewWithEventLabel, dialogEval,
    widgetShowIf, NameAuswahlWidget, nameAuswahlPackNew, aktuellerName)
import Zug.UI.Gtk.Klassen (
    MitWidget(..), mitWidgetShow, mitWidgetHide, MitBox(..),
    mitContainerAdd, mitContainerRemove, MitEntry(..), MitButton(..), MitWindow(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt (
    StatusGui, MStatusGuiT, IOStatusGui, ObjektGui,
    DynamischeWidgets(..), DynamischeWidgetsReader(..),
    StatusReader(..), BoxPlanHinzufügen,
    WegstreckenElement(..), WegstreckeCheckButton(),
    WidgetsTyp(..), widgetHinzufügenToggled, widgetHinzufügenAktuelleAuswahl,
    widgetHinzufügenContainerGefüllt,
    bahngeschwindigkeitPackNew, BGWidgets,
    streckenabschnittPackNew,
    weichePackNew, WEWidgets,
    kupplungPackNew,
    wegstreckePackNew, WSWidgets,
    planPackNew)
import Zug.UI.Gtk.ZugtypSpezifisch (zugtypSpezifischNew, zugtypSpezifischButtonNew)

-- | Speichern des aktuellen 'StatusGui'
buttonSpeichernPack :: forall b m. (MitBox b, ObjektReader ObjektGui m, MonadIO m) => Gtk.Window -> b -> m Gtk.Button
buttonSpeichernPack windowMain box = do
    dialogSpeichern <- dialogSpeichernNew windowMain
    tmvarStatus <- erhalteStatus :: m (TMVar StatusGui)
    objektReader <- ask
    boxPackWidgetNewDefault box $ buttonNewWithEventLabel Language.speichern $ do
        antwort <- dialogEval dialogSpeichern
        when (antwort == Gtk.ResponseOk) $ void $ do
            (Just dateipfad) <- Gtk.fileChooserGetFilename dialogSpeichern
            flip runReaderT objektReader $
                ausführenTMVarBefehl (Speichern dateipfad) tmvarStatus

dialogSpeichernNew :: (SpracheGuiReader r m, MonadIO m) => Gtk.Window -> m Gtk.FileChooserDialog
dialogSpeichernNew window = do
    (fileChooserDialog, buttonSpeichern, buttonAbbrechen) <- liftIO $ do
        fileChooserDialog <- Gtk.fileChooserDialogNew (Nothing :: Maybe Text) (Just window) Gtk.FileChooserActionSave []
        Gtk.set fileChooserDialog [Gtk.fileChooserDoOverwriteConfirmation := True]
        buttonSpeichern <- Gtk.dialogAddButton fileChooserDialog Text.empty Gtk.ResponseOk
        buttonAbbrechen <- Gtk.dialogAddButton fileChooserDialog Text.empty Gtk.ResponseCancel
        pure (fileChooserDialog, buttonSpeichern, buttonAbbrechen)
    verwendeSpracheGui $ \sprache -> do
        Gtk.set fileChooserDialog [Gtk.windowTitle := Language.speichern sprache]
        Gtk.set buttonSpeichern [Gtk.buttonLabel := Language.speichern sprache]
        Gtk.set buttonAbbrechen [Gtk.buttonLabel := Language.abbrechen sprache]
    pure fileChooserDialog

-- | Laden eines neuen 'StatusGui' aus einer Datei
buttonLadenPack :: (MitWindow p, MitBox b, ObjektReader ObjektGui m, MonadIO m) => p -> b -> m Gtk.Button
buttonLadenPack parent box = do
    dialogLaden <- dialogLadenNew parent
    dialogLadenFehler <- dialogLadenFehlerNew parent
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    boxPackWidgetNewDefault box $ buttonNewWithEventLabel Language.laden $ do
        antwort <- dialogEval dialogLaden
        when (antwort == Gtk.ResponseOk) $ void $ do
            Gtk.fileChooserGetFilename dialogLaden >>= \case
                Nothing             -> void $ do
                    status <- atomically $ readTMVar tmvarStatus
                    Gtk.set dialogLadenFehler
                        [Gtk.windowTitle := leseSprache Language.nichtGefundeneDatei (status ^. sprache)]
                    dialogEval dialogLadenFehler
                (Just dateipfad)    -> void $ do
                    let
                        ladeAktion :: Status -> IOStatusGui ()
                        ladeAktion statusNeu = do
                            state0 <- RWS.get
                            state1 <- liftIO $ flip runReaderT objektReader $ fst <$>
                                RWS.execRWST (ladeWidgets statusNeu) objektReader state0
                            RWS.put state1
                        fehlerBehandlung :: IOStatusGui ()
                        fehlerBehandlung = liftIO $ void $ do
                            Gtk.set dialogLadenFehler [Gtk.windowTitle := dateipfad]
                            dialogEval dialogLadenFehler
                    flip runReaderT objektReader $
                        ausführenTMVarBefehl (Laden dateipfad ladeAktion fehlerBehandlung) tmvarStatus
                    -- neuer Status ist schon in tmvarStatus gespeichert und muss nicht mehr neu gesetzt werden
                    -- TMVar wird in ladeAktion beeinflusst
                    -- ausführenTMVarBefehl dadurch nicht möglich
                    -- Ergebnis & Status der RWST-Aktion ebenfalls uninteressant
                    -- statusAktuell <- atomically $ readTMVar tmvarStatus
                    -- runRWST (ausführenBefehl $ Laden dateipfad ladeAktion fehlerBehandlung) objektReader statusAktuell

-- | Passe angezeigte Widgets (inkl. 'StatusGui' in 'TMVar') an reinen 'Status' an.
ladeWidgets :: (ObjektReader ObjektGui m, MonadIO m) => Status -> MStatusGuiT m ()
ladeWidgets status = do
    löscheWidgets
    erstelleWidgets status
        where
            löscheWidgets :: (DynamischeWidgetsReader r m, MonadIO m) => MStatusGuiT m ()
            löscheWidgets = do
                status <- RWS.get
                mapM_ entferneWidgets $ status ^. bahngeschwindigkeiten
                mapM_ entferneWidgets $ status ^. streckenabschnitte
                mapM_ entferneWidgets $ status ^. weichen
                mapM_ entferneWidgets $ status ^. kupplungen
                mapM_ entferneWidgets $ status ^. wegstrecken
                mapM_ entferneWidgets $ status ^. pläne
            erstelleWidgets :: (ObjektReader ObjektGui m, MonadIO m) => Status -> MStatusGuiT m ()
            erstelleWidgets status = do
                let 
                    packBG :: (ObjektReader ObjektGui m, MonadIO m) =>
                        ZugtypEither Bahngeschwindigkeit -> MStatusGuiT m (ZugtypEither BGWidgets)
                    packBG  (ZugtypMärklin bg)  = ZugtypMärklin <$> bahngeschwindigkeitPackNew bg
                    packBG  (ZugtypLego bg)     = ZugtypLego <$> bahngeschwindigkeitPackNew bg
                mapM_ packBG $ reverse $ status ^. bahngeschwindigkeiten
                mapM_ streckenabschnittPackNew $ reverse $ status ^. streckenabschnitte
                let
                    packWE :: (ObjektReader ObjektGui m, MonadIO m) =>
                        ZugtypEither Weiche -> MStatusGuiT m (ZugtypEither WEWidgets)
                    packWE  (ZugtypMärklin we)  = ZugtypMärklin <$> weichePackNew we
                    packWE  (ZugtypLego we)     = ZugtypLego <$> weichePackNew we
                mapM_ packWE $ reverse $ status ^. weichen
                mapM_ kupplungPackNew $ reverse $ status ^. kupplungen
                let
                    packWS :: (ObjektReader ObjektGui m, MonadIO m) =>
                        ZugtypEither Wegstrecke -> MStatusGuiT m (ZugtypEither WSWidgets)
                    packWS  (ZugtypMärklin ws)  = ZugtypMärklin <$> wegstreckePackNew ws
                    packWS  (ZugtypLego ws)     = ZugtypLego <$> wegstreckePackNew ws
                mapM_ packWS $ reverse $ status ^. wegstrecken
                mapM_ planPackNew $ reverse $ status ^. pläne

dialogLadenNew :: (MitWindow p, SpracheGuiReader r m, MonadIO m) => p -> m Gtk.FileChooserDialog
dialogLadenNew parent = do
    (dialog, buttonLaden, buttonAbbrechen) <- liftIO $ do
        dialog <- Gtk.fileChooserDialogNew
            (Nothing :: Maybe Text)
            (Just $ erhalteWindow parent)
            Gtk.FileChooserActionOpen
            []
        buttonLaden <- Gtk.dialogAddButton dialog Text.empty Gtk.ResponseOk
        buttonAbbrechen <- Gtk.dialogAddButton dialog Text.empty Gtk.ResponseCancel
        pure (dialog, buttonLaden, buttonAbbrechen)
    verwendeSpracheGui $ \sprache -> do
        Gtk.set dialog [Gtk.windowTitle := Language.laden sprache]
        Gtk.set buttonLaden [Gtk.buttonLabel := Language.laden sprache]
        Gtk.set buttonAbbrechen [Gtk.buttonLabel := Language.abbrechen sprache]
    pure dialog

dialogLadenFehlerNew :: (MitWindow p, SpracheGuiReader r m, MonadIO m) => p -> m Gtk.MessageDialog
dialogLadenFehlerNew parent = do
    dialog <- liftIO $ Gtk.messageDialogNew
        (Just $ erhalteWindow parent)
        []
        Gtk.MessageError
        Gtk.ButtonsOk
        Text.empty
    verwendeSpracheGui $
        \sprache -> Gtk.set dialog [Gtk.windowTitle := (Language.nichtGefundeneDatei <!> Text.empty) sprache]
    pure dialog

-- | Hinzufügen eines 'StreckenObjekt'
buttonHinzufügenPack :: (MitWindow p, MitBox b, ObjektReader ObjektGui m, MonadIO m) => p -> b -> m Gtk.Button
buttonHinzufügenPack parentWindow box = do
    assistantHinzufügen <- assistantHinzufügenNew parentWindow
    objektReader <- ask
    tmvarStatus <- erhalteStatus
    button <- boxPackWidgetNewDefault box $ buttonNewWithEventLabel Language.hinzufügen $ void $ forkIO $ do
        flip runReaderT objektReader $ do
            assistantAuswerten assistantHinzufügen >>= flip auswertenTMVarMStatusT tmvarStatus . \case
                (AssistantErfolgreich (OBahngeschwindigkeit (ZugtypMärklin bgMärklin)))
                    -> void $ bahngeschwindigkeitPackNew bgMärklin
                (AssistantErfolgreich (OBahngeschwindigkeit (ZugtypLego bgLego)))
                    -> void $ bahngeschwindigkeitPackNew bgLego
                (AssistantErfolgreich (OStreckenabschnitt st))
                    -> void $ streckenabschnittPackNew st
                (AssistantErfolgreich (OWeiche (ZugtypMärklin weMärklin)))
                    -> void $ weichePackNew weMärklin
                (AssistantErfolgreich (OWeiche (ZugtypLego weLego)))
                    -> void $ weichePackNew weLego
                (AssistantErfolgreich (OKupplung ku))
                    -> void $ kupplungPackNew ku
                (AssistantErfolgreich (OWegstrecke (ZugtypMärklin wsMärklin)))
                    -> void $ wegstreckePackNew wsMärklin
                (AssistantErfolgreich (OWegstrecke (ZugtypLego wsLego)))
                    -> void $ wegstreckePackNew wsLego
                (AssistantErfolgreich (OPlan pl))
                    -> void $ planPackNew pl
                -- Kein catch-all Pattern um Fehlermeldung des Compilers
                -- bei neu hinzugefügten Objekten nicht zu verpassen
                AssistantBeenden
                    -> pure ()
                AssistantAbbrechen
                    -> pure ()
    pure button

-- | Seiten des Hinzufügen-'Assistant'
data HinzufügenSeite
    = HinzufügenSeiteAuswahl {
        widget :: Gtk.Widget}
    | HinzufügenSeiteBahngeschwindigkeit {
        widget :: Gtk.Widget,
        nameAuswahl :: NameAuswahlWidget,
        geschwindigkeitAuswahl :: AnschlussAuswahlWidget,
        -- Lego
        fahrtrichtungsAuswahl :: AnschlussAuswahlWidget}
    | HinzufügenSeiteStreckenabschnitt {
        widget :: Gtk.Widget,
        nameAuswahl :: NameAuswahlWidget,
        stromAuswahl :: AnschlussAuswahlWidget}
    | HinzufügenSeiteWeiche {
        widget :: Gtk.Widget,
        nameAuswahl :: NameAuswahlWidget,
        -- Märklin
        märklinRichtungsAuswahl :: NonEmpty (Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget),
        -- Lego
        legoRichtungsAuswahl :: AnschlussAuswahlWidget,
        legoRichtungenAuswahl :: AuswahlWidget (Richtung, Richtung)}
    | HinzufügenSeiteKupplung {
        widget :: Gtk.Widget,
        nameAuswahl :: NameAuswahlWidget,
        kupplungsAuswahl :: AnschlussAuswahlWidget}
    | HinzufügenSeiteWegstrecke {
        widget :: Gtk.Widget,
        nameAuswahl :: NameAuswahlWidget}
    | HinzufügenSeitePlan {
        widget :: Gtk.Widget,
        nameAuswahl :: NameAuswahlWidget,
        tvarAktionen :: TVar (Warteschlange Aktion),
        checkButtonDauerschleife :: Gtk.CheckButton}
    deriving (Eq)

instance MitWidget HinzufügenSeite where
    erhalteWidget :: HinzufügenSeite -> Gtk.Widget
    erhalteWidget = widget

hinzufügenErgebnis :: (StatusReader r ObjektGui m, MonadIO m) =>
    AuswahlWidget Zugtyp -> FließendAuswahlWidget -> NonEmpty HinzufügenSeite -> m Objekt
hinzufügenErgebnis zugtypAuswahl fließendAuswahl gezeigteSeiten = case NonEmpty.last gezeigteSeiten of
    HinzufügenSeiteAuswahl {}
        -> error "Auswahl-Seite zum Hinzufügen als letzte Seite angezeigt"
    HinzufügenSeiteBahngeschwindigkeit {nameAuswahl, geschwindigkeitAuswahl, fahrtrichtungsAuswahl}
        -> do
            name <- aktuellerName nameAuswahl
            fließend <- aktuellerFließendValue fließendAuswahl
            geschwindigkeitsAnschluss <- aktuellerAnschluss geschwindigkeitAuswahl
            aktuelleAuswahl zugtypAuswahl >>= \case
                Märklin
                    -> pure $ OBahngeschwindigkeit $ ZugtypMärklin MärklinBahngeschwindigkeit {
                        bgmName = name,
                        bgmFließend = fließend,
                        bgmGeschwindigkeitsAnschluss = geschwindigkeitsAnschluss}
                Lego
                    -> do
                        bglFahrtrichtungsAnschluss <- aktuellerAnschluss fahrtrichtungsAuswahl
                        pure $ OBahngeschwindigkeit $ ZugtypLego LegoBahngeschwindigkeit {
                            bglName = name,
                            bglFließend = fließend,
                            bglGeschwindigkeitsAnschluss = geschwindigkeitsAnschluss,
                            bglFahrtrichtungsAnschluss}
    HinzufügenSeiteStreckenabschnitt {nameAuswahl, stromAuswahl}
        -> do
            stName <- aktuellerName nameAuswahl
            stFließend <- aktuellerFließendValue fließendAuswahl
            stromAnschluss <- aktuellerAnschluss stromAuswahl
            pure $ OStreckenabschnitt Streckenabschnitt {stName, stFließend, stromAnschluss}
    HinzufügenSeiteWeiche {
        nameAuswahl,
        märklinRichtungsAuswahl,
        legoRichtungsAuswahl,
        legoRichtungenAuswahl}
            -> do
                name <- aktuellerName nameAuswahl
                fließend <- aktuellerFließendValue fließendAuswahl
                aktuelleAuswahl zugtypAuswahl >>= \case
                    Märklin
                        -> do
                            -- Nicht-Leerheit garantiert durch FortfahrenWennToggled
                            wemRichtungsAnschlüsse <-
                                fmap (NonEmpty.fromList . map fromJust . NonEmpty.filter isJust) $
                                    forM märklinRichtungsAuswahl $ \(richtung, rcb, anschlussAuswahl)
                                        -> registrierterCheckButtonToggled rcb >>= \case
                                            True
                                                -> Just . (\anschluss -> (richtung, anschluss)) <$> aktuellerAnschluss anschlussAuswahl
                                            False
                                                -> pure Nothing
                            pure $ OWeiche $ ZugtypMärklin MärklinWeiche {
                                wemName = name,
                                wemFließend = fließend,
                                wemRichtungsAnschlüsse}
                    Lego
                        -> do
                            welRichtungen <- aktuelleAuswahl legoRichtungenAuswahl
                            welRichtungsAnschluss <- aktuellerAnschluss legoRichtungsAuswahl
                            pure $ OWeiche $ ZugtypLego LegoWeiche {
                                welName = name,
                                welFließend = fließend,
                                welRichtungen,
                                welRichtungsAnschluss}
    HinzufügenSeiteKupplung {nameAuswahl, kupplungsAuswahl}
        -> do
            kuName <- aktuellerName nameAuswahl
            kuFließend <- aktuellerFließendValue fließendAuswahl
            kupplungsAnschluss <- aktuellerAnschluss kupplungsAuswahl
            pure $ OKupplung Kupplung {kuName, kuFließend, kupplungsAnschluss}
    HinzufügenSeiteWegstrecke {nameAuswahl}
        -> do
            tmvarStatus <- erhalteStatus
            aktuellerStatus <- liftIO $ atomically $ readTMVar tmvarStatus
            wsName <- aktuellerName nameAuswahl
            let
                gewählteWegstrecke ::
                    (MonadIO m, ZugtypKlasse z, WegstreckenElement (BGWidgets z), WegstreckenElement (WEWidgets z),
                    MitAuswahlWidget (WegstreckeCheckButton (CheckButtonAuswahl (WEWidgets z))) Richtung) =>
                        m (Wegstrecke z)
                gewählteWegstrecke = do
                    wsBahngeschwindigkeiten <- foldM anhängenWennToggled [] $
                        catMaybes $ map vonZugtypEither $ aktuellerStatus ^. bahngeschwindigkeiten
                    wsStreckenabschnitte <- foldM anhängenWennToggled [] $ aktuellerStatus ^. streckenabschnitte
                    wsWeichenRichtungen <- foldM weichenRichtungAnhängenWennToggled [] $
                        catMaybes $ map vonZugtypEither $ aktuellerStatus ^. weichen
                    wsKupplungen <- foldM anhängenWennToggled [] $ aktuellerStatus ^. kupplungen
                    pure Wegstrecke {
                        wsName,
                        wsBahngeschwindigkeiten,
                        wsStreckenabschnitte,
                        wsWeichenRichtungen,
                        wsKupplungen}
                anhängenWennToggled :: (WidgetsTyp a, WegstreckenElement a, MonadIO m) =>
                    [ObjektTyp a] -> a -> m [ObjektTyp a]
                anhängenWennToggled acc a = widgetHinzufügenToggled (a ^. getterWegstrecke) >>= \case
                    True
                        -> pure $ erhalteObjektTyp a : acc
                    False
                        -> pure acc
                weichenRichtungAnhängenWennToggled ::
                    (WegstreckenElement (WEWidgets z), MonadIO m,
                    MitAuswahlWidget (WegstreckeCheckButton (CheckButtonAuswahl (WEWidgets z))) Richtung) =>
                        [(Weiche z, Richtung)] -> WEWidgets z -> m [(Weiche z, Richtung)]
                weichenRichtungAnhängenWennToggled acc weiche = do
                    let widgetHinzufügen = weiche ^. getterWegstrecke
                    toggled <- widgetHinzufügenToggled widgetHinzufügen
                    if toggled
                        then do
                            richtung <- widgetHinzufügenAktuelleAuswahl widgetHinzufügen
                            pure $ (erhalteObjektTyp weiche, richtung) : acc
                        else pure acc
            -- Explizite Zugtyp-Auswahl notwendig für den Typ-Checker
            -- Dieser kann sonst Typ-Klassen nicht überprüfen
            aktuelleAuswahl zugtypAuswahl >>= \case
                Märklin
                    -> OWegstrecke . ZugtypMärklin <$> gewählteWegstrecke
                Lego
                    -> OWegstrecke . ZugtypLego <$> gewählteWegstrecke
    HinzufügenSeitePlan {nameAuswahl, tvarAktionen, checkButtonDauerschleife}
        -> liftIO $ do
            plName <- aktuellerName nameAuswahl
            aktionen <- toList <$> readTVarIO tvarAktionen
            Gtk.get checkButtonDauerschleife Gtk.toggleButtonActive >>= pure . OPlan . \case
                True
                    -> let plan = Plan {plName, plAktionen = aktionen ++ [AktionAusführen plan]} in plan
                False
                    -> Plan {plName, plAktionen = aktionen}

-- Durch Assistant ersetzten!
-- | Erstelle einen neuen Hinzufügen-'Assistant'
assistantHinzufügenNew :: (MitWindow w, ObjektReader ObjektGui m, MonadIO m) =>
    w -> m (Assistant HinzufügenSeite Objekt)
assistantHinzufügenNew
    parent
        = do
            objektReader <- ask
            DynamischeWidgets {
                fortfahrenWennToggledWegstrecke,
                tmvarPlanObjekt,
                vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin,
                vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego,
                vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
                vBoxHinzufügenPlanBahngeschwindigkeitenLego,
                vBoxHinzufügenWegstreckeStreckenabschnitte,
                vBoxHinzufügenPlanStreckenabschnitte,
                vBoxHinzufügenWegstreckeWeichenMärklin,
                vBoxHinzufügenWegstreckeWeichenLego,
                vBoxHinzufügenPlanWeichenGeradeMärklin,
                vBoxHinzufügenPlanWeichenKurveMärklin,
                vBoxHinzufügenPlanWeichenLinksMärklin,
                vBoxHinzufügenPlanWeichenRechtsMärklin,
                vBoxHinzufügenPlanWeichenGeradeLego,
                vBoxHinzufügenPlanWeichenKurveLego,
                vBoxHinzufügenPlanWeichenLinksLego,
                vBoxHinzufügenPlanWeichenRechtsLego,
                vBoxHinzufügenWegstreckeKupplungen,
                vBoxHinzufügenPlanKupplungen,
                vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
                vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
                vBoxHinzufügenPlanWegstreckenKupplungMärklin,
                vBoxHinzufügenPlanWegstreckenMärklin,
                vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
                vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
                vBoxHinzufügenPlanWegstreckenKupplungLego,
                vBoxHinzufügenPlanWegstreckenLego,
                vBoxHinzufügenPlanPläne}
                    <- erhalteDynamischeWidgets
            liftIO $ do
                -- Globale Widgets
                zugtypAuswahl <- boundedEnumAuswahlComboBoxNew Märklin Language.zugtyp
                fließendAuswahl <- fließendAuswahlNew
                let globaleWidgets = [Left zugtypAuswahl, Right fließendAuswahl]
                -- Dummy-Widget zur Seitenauswahl. Auswahl wird durch Assistant übernommen.
                auswahl <- erhalteWidget <$> Gtk.labelNew (Nothing :: Maybe Text)
                let
                    seiteAuswahl :: AssistantSeite HinzufügenSeite
                    seiteAuswahl = AssistantSeite {
                        seite = HinzufügenSeiteAuswahl {widget = auswahl},
                        name = Language.hinzufügen,
                        seiteZurücksetzen = pure (),
                        seitenAbschluss = SeitenAbschluss Language.weiter}
                -- Bahngeschwindigkeit
                boxBahngeschwindigkeit <- Gtk.vBoxNew False 0
                nameAuswahlBahngeschwindigkeit <- nameAuswahlPackNew boxBahngeschwindigkeit
                geschwindigkeitAuswahl <- boxPackWidgetNewDefault boxBahngeschwindigkeit $
                    anschlussAuswahlNew Language.geschwindigkeit
                legoBoxBahngeschwindigkeit <- erhalteBox <$> Gtk.vBoxNew False 0
                fahrtrichtungsAuswahl <- boxPackWidgetNewDefault legoBoxBahngeschwindigkeit $
                    anschlussAuswahlNew Language.fahrtrichtung
                boxPackWidgetNewDefault boxBahngeschwindigkeit $
                    zugtypSpezifischNew ((Lego, legoBoxBahngeschwindigkeit) :| []) zugtypAuswahl
                let
                    seiteBahngeschwindigkeit :: AssistantSeite HinzufügenSeite
                    seiteBahngeschwindigkeit = AssistantSeite {
                        seite = HinzufügenSeiteBahngeschwindigkeit {
                            widget = erhalteWidget boxBahngeschwindigkeit,
                            nameAuswahl = nameAuswahlBahngeschwindigkeit,
                            geschwindigkeitAuswahl,
                            fahrtrichtungsAuswahl},
                        name = Language.bahngeschwindigkeit ,
                        seiteZurücksetzen = Gtk.set (erhalteEntry nameAuswahlBahngeschwindigkeit)
                            [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True],
                        seitenAbschluss = SeitenAbschluss Language.hinzufügen}
                -- Streckenabschnitt
                boxStreckenabschnitt <- Gtk.vBoxNew False 0
                nameAuswahlStreckenabschnitt <- nameAuswahlPackNew boxStreckenabschnitt
                stromAuswahl <- boxPackWidgetNewDefault boxStreckenabschnitt $
                    anschlussAuswahlNew Language.strom
                let
                    seiteStreckenabschnitt :: AssistantSeite HinzufügenSeite
                    seiteStreckenabschnitt = AssistantSeite {
                        seite = HinzufügenSeiteStreckenabschnitt{
                            widget = erhalteWidget boxStreckenabschnitt,
                            nameAuswahl = nameAuswahlStreckenabschnitt,
                            stromAuswahl},
                        name = Language.streckenabschnitt,
                        seiteZurücksetzen = Gtk.set (erhalteEntry nameAuswahlStreckenabschnitt)
                            [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True],
                        seitenAbschluss = SeitenAbschluss Language.hinzufügen}
                -- Weiche
                boxWeiche <- Gtk.vBoxNew False 0
                nameAuswahlWeiche <- nameAuswahlPackNew boxWeiche
                märklinBoxWeiche <- fmap erhalteBox $ Gtk.vBoxNew False 0
                märklinFortfahrenWennToggledTMVar <- fortfahrenWennToggledNew Language.hinzufügen $
                    showText <$> unterstützteRichtungen
                let
                    richtungsCheckButtons :: NonEmpty (Richtung, RegistrierterCheckButton)
                    richtungsCheckButtons = NonEmpty.zip unterstützteRichtungen $
                        checkButtons märklinFortfahrenWennToggledTMVar
                märklinRichtungsAuswahl <- forM richtungsCheckButtons $ \(richtung, checkButton) -> do
                    box <- boxPackWidgetNewDefault märklinBoxWeiche $ Gtk.hBoxNew False 0
                    boxPackDefault box checkButton
                    anschlussAuswahl <- boxPackWidgetNewDefault box $ anschlussAuswahlNew $ showText richtung
                    pure (richtung, checkButton, anschlussAuswahl)
                legoBoxWeiche <- fmap erhalteBox $ Gtk.vBoxNew False 0
                legoSeitenAbschluss <- Gtk.buttonNewWithLabel (Language.hinzufügen :: Text)
                legoRichtungsAuswahl <- boxPackWidgetNewDefault legoBoxWeiche $
                    anschlussAuswahlNew Language.richtung
                legoRichtungenAuswahl <- boxPackWidgetNewDefault legoBoxWeiche $
                    flip auswahlComboBoxNew Language.richtungen $
                        (Gerade, Kurve) :| [
                        (Gerade, Links),
                        (Gerade, Rechts),
                        (Kurve, Links),
                        (Kurve, Rechts),
                        (Links, Rechts)]
                seitenAbschlussWeiche <- SeitenAbschlussZugtyp <$> zugtypSpezifischButtonNew
                    ((Märklin, erhalteButton märklinFortfahrenWennToggledTMVar) :| [(Lego, legoSeitenAbschluss)])
                    zugtypAuswahl
                boxPackWidgetNewDefault boxWeiche $ zugtypSpezifischNew
                    ((Märklin, märklinBoxWeiche) :| [(Lego, legoBoxWeiche)])
                    zugtypAuswahl
                let
                    seiteWeiche :: AssistantSeite HinzufügenSeite
                    seiteWeiche = AssistantSeite {
                        seite = HinzufügenSeiteWeiche {
                            widget = erhalteWidget boxWeiche,
                            nameAuswahl = nameAuswahlWeiche,
                            märklinRichtungsAuswahl,
                            legoRichtungsAuswahl,
                            legoRichtungenAuswahl},
                        name = Language.weiche,
                        seiteZurücksetzen = Gtk.set (erhalteEntry nameAuswahlWeiche)
                            [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True],
                        seitenAbschluss = seitenAbschlussWeiche}
                -- Kupplung
                boxKupplung <- Gtk.vBoxNew False 0
                nameAuswahlKupplung <- nameAuswahlPackNew boxKupplung
                kupplungsAuswahl <- boxPackWidgetNewDefault boxKupplung $
                    anschlussAuswahlNew Language.kuppeln
                let
                    seiteKupplung :: AssistantSeite HinzufügenSeite
                    seiteKupplung = AssistantSeite {
                        seite = HinzufügenSeiteKupplung {
                            widget = erhalteWidget boxKupplung,
                            nameAuswahl = nameAuswahlKupplung,
                            kupplungsAuswahl},
                        name = Language.kupplung,
                        seiteZurücksetzen = Gtk.set (erhalteEntry nameAuswahlKupplung)
                            [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True],
                        seitenAbschluss = SeitenAbschluss Language.hinzufügen}
                -- Wegstrecke
                boxWegstrecke <- Gtk.vBoxNew False 0
                nameAuswahlWegstrecke <- nameAuswahlPackNew boxWegstrecke
                hPaned <- boxPackWidgetNewDefault boxWegstrecke Gtk.hPanedNew
                vPanedLeft <- widgetShowNew Gtk.vPanedNew
                Gtk.panedAdd1 hPaned vPanedLeft
                frameLeftTop <- widgetShowNew Gtk.frameNew
                Gtk.set frameLeftTop [Gtk.frameShadowType := Gtk.ShadowIn]
                Gtk.panedAdd1 vPanedLeft frameLeftTop
                frameLeftBot <- widgetShowNew Gtk.frameNew
                Gtk.set frameLeftBot [Gtk.frameShadowType := Gtk.ShadowIn]
                Gtk.panedAdd2 vPanedLeft frameLeftBot
                vPanedRight <- widgetShowNew Gtk.vPanedNew
                Gtk.panedAdd2 hPaned vPanedRight
                frameRightTop <- widgetShowNew Gtk.frameNew
                Gtk.set frameRightTop [Gtk.frameShadowType := Gtk.ShadowIn]
                Gtk.panedAdd1 vPanedRight frameRightTop
                frameRightBot <- widgetShowNew Gtk.frameNew
                Gtk.set frameRightBot [Gtk.frameShadowType := Gtk.ShadowIn]
                Gtk.panedAdd2 vPanedRight frameRightBot
                containerAddWidgetNew frameLeftTop $ flip zugtypSpezifischNew zugtypAuswahl $
                    (Märklin, erhalteWidget vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin) :|
                    [(Lego, erhalteWidget vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego)]
                mitContainerAdd frameLeftBot vBoxHinzufügenWegstreckeStreckenabschnitte
                containerAddWidgetNew frameRightTop $ flip zugtypSpezifischNew zugtypAuswahl $
                    (Märklin, erhalteWidget vBoxHinzufügenWegstreckeWeichenMärklin) :|
                    [(Lego, erhalteWidget vBoxHinzufügenWegstreckeWeichenLego)]
                mitContainerAdd frameRightBot vBoxHinzufügenWegstreckeKupplungen
                let
                    seiteZurücksetzenWegstrecke :: IO ()
                    seiteZurücksetzenWegstrecke = do
                        aktiviereWennToggledTMVar fortfahrenWennToggledWegstrecke
                        Gtk.set (erhalteEntry nameAuswahlWegstrecke)
                            [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True]
                    seiteWegstrecke :: AssistantSeite HinzufügenSeite
                    seiteWegstrecke = AssistantSeite {
                        seite = HinzufügenSeiteWegstrecke {
                            widget = erhalteWidget boxWegstrecke,
                            nameAuswahl = nameAuswahlWegstrecke},
                        name = Language.wegstrecke,
                        seiteZurücksetzen = seiteZurücksetzenWegstrecke,
                        seitenAbschluss = SeitenAbschlussToggledTMVar fortfahrenWennToggledWegstrecke}
                -- Hilfsdialog für Plan
                windowAktionObjektAuswahl <- Gtk.windowNew
                Gtk.on windowAktionObjektAuswahl Gtk.deleteEvent $ liftIO $ do
                    atomically $ putTMVar tmvarPlanObjekt Nothing
                    mitWidgetHide windowAktionObjektAuswahl
                    pure True
                boxAktionObjektAuswahl <- containerAddWidgetNew windowAktionObjektAuswahl $ Gtk.vBoxNew False 0
                -- Anzeige der Boxen explizit beim Anzeigen des Fensters
                mapM_ (boxPackDefault boxAktionObjektAuswahl) [
                    erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
                    erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenLego,
                    erhalteWidget vBoxHinzufügenPlanStreckenabschnitte,
                    erhalteWidget vBoxHinzufügenPlanWeichenGeradeMärklin,
                    erhalteWidget vBoxHinzufügenPlanWeichenKurveMärklin,
                    erhalteWidget vBoxHinzufügenPlanWeichenLinksMärklin,
                    erhalteWidget vBoxHinzufügenPlanWeichenRechtsMärklin,
                    erhalteWidget vBoxHinzufügenPlanWeichenGeradeLego,
                    erhalteWidget vBoxHinzufügenPlanWeichenKurveLego,
                    erhalteWidget vBoxHinzufügenPlanWeichenLinksLego,
                    erhalteWidget vBoxHinzufügenPlanWeichenRechtsLego,
                    erhalteWidget vBoxHinzufügenPlanKupplungen,
                    erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
                    erhalteWidget vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
                    erhalteWidget vBoxHinzufügenPlanWegstreckenKupplungMärklin,
                    erhalteWidget vBoxHinzufügenPlanWegstreckenMärklin,
                    erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
                    erhalteWidget vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
                    erhalteWidget vBoxHinzufügenPlanWegstreckenKupplungLego,
                    erhalteWidget vBoxHinzufügenPlanWegstreckenLego,
                    erhalteWidget vBoxHinzufügenPlanPläne]
                boxPackWidgetNewDefault boxAktionObjektAuswahl $ buttonNewWithEventLabel Language.abbrechen $
                    atomically $ putTMVar tmvarPlanObjekt Nothing
                -- Plan
                boxPlan <- Gtk.vBoxNew False 0
                nameAuswahlPlan <- nameAuswahlPackNew boxPlan
                expanderAktionen <- widgetShowNew $ Gtk.expanderNew (Language.aktionen :: Text)
                boxAktionen <- containerAddWidgetNew expanderAktionen $ Gtk.vBoxNew False 0
                seitenAbschlussPlan <- Gtk.buttonNewWithLabel (Language.hinzufügen :: Text)
                tvarAktionen <- newTVarIO leer
                tvarWidgets <- newTVarIO []
                let
                    zeigeAktionen :: (Foldable t, MonadIO m) => t Aktion -> m ()
                    zeigeAktionen aktionen = liftIO $ do
                        widgets <- readTVarIO tvarWidgets
                        forM_ widgets $ mitContainerRemove boxAktionen
                        widgetsNeu <- mapM (boxPackWidgetNewDefault boxAktionen . Gtk.labelNew . Just . show) $
                            toList aktionen
                        Gtk.set expanderAktionen [Gtk.expanderLabel := Language.aktionen <:> show (length aktionen)]
                        atomically $ writeTVar tvarWidgets widgetsNeu
                        Gtk.set seitenAbschlussPlan [Gtk.widgetSensitive := not $ null aktionen]
                    aktionHinzufügen :: Aktion -> IO ()
                    aktionHinzufügen aktion = do
                        aktionenDanach <- atomically $ do
                            aktionen <- readTVar tvarAktionen
                            let aktionenDanach = anhängen aktion aktionen
                            writeTVar tvarAktionen aktionenDanach
                            pure aktionenDanach
                        zeigeAktionen aktionenDanach
                -- Warten
                boxAktionWarten <- boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
                let wartenMinµs = 0
                    wartenMaxµs = 10000000  -- 10 s
                    wartenStepµs = 1000     -- 1 ms
                wartenSpinButton <- widgetShowNew $ Gtk.spinButtonNewWithRange wartenMinµs wartenMaxµs wartenStepµs
                boxPackWidgetNewDefault boxAktionWarten $ buttonNewWithEventLabel Language.warten $ do
                    wartezeit <- MikroSekunden . fromIntegral <$> Gtk.spinButtonGetValueAsInt wartenSpinButton
                    aktionHinzufügen $ Warten wartezeit
                boxPackDefault boxAktionWarten wartenSpinButton
                boxPackWidgetNewDefault boxAktionWarten $ Gtk.labelNew $ Just (Language.wartenEinheit :: Text)
                -- AktionBahngeschwindigkeit 'Märklin
                boxAktionBahngeschwindigkeitMärklin <- Gtk.hBoxNew False 0
                let
                    zeigeMärklinBahngeschwindigkeitAktionAuswahl :: IO ()
                    zeigeMärklinBahngeschwindigkeitAktionAuswahl = do
                        mitWidgetShow vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
                        mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
                        mitWidgetHide vBoxHinzufügenPlanKupplungen
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
                        mitWidgetHide vBoxHinzufügenPlanPläne
                        mitWidgetShow windowAktionObjektAuswahl
                    märklinBahngeschwindigkeitAktionHinzufügen :: 
                        (forall b. (BahngeschwindigkeitKlasse b) =>
                            b 'Märklin -> IO (AktionBahngeschwindigkeit b 'Märklin)) ->
                                IO ()
                    märklinBahngeschwindigkeitAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
                        Gtk.postGUIAsync $ zeigeMärklinBahngeschwindigkeitAktionAuswahl
                        atomically (takeTMVar tmvarPlanObjekt) >>= \case
                            (Just (OBahngeschwindigkeit (ZugtypMärklin bgMärklin)))
                                -> aktionKonstruktor bgMärklin >>= aktionHinzufügen . ABahngeschwindigkeitMärklin
                            (Just (OWegstrecke (ZugtypMärklin wsMärklin)))
                                -> aktionKonstruktor wsMärklin >>=
                                    aktionHinzufügen . AWegstreckeMärklin . AWSBahngeschwindigkeit
                            (Just anderesObjekt)
                                -> error $
                                    "unerwartetes Objekt für Märklin-Bahngeschwindigkeit-Aktion erhalten: " ++
                                    show anderesObjekt
                            Nothing
                                -> pure ()
                        Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
                märklinGeschwindigkeitsScale <-
                    boxPackWidgetNew boxAktionBahngeschwindigkeitMärklin PackGrow paddingDefault positionDefault $
                        Gtk.hScaleNewWithRange 0 100 1
                boxPackWidgetNewDefault boxAktionBahngeschwindigkeitMärklin $
                    buttonNewWithEventLabel Language.geschwindigkeit $
                        märklinBahngeschwindigkeitAktionHinzufügen $ \bg -> do
                            Geschwindigkeit bg . floor <$> Gtk.get märklinGeschwindigkeitsScale Gtk.rangeValue
                boxPackWidgetNewDefault boxAktionBahngeschwindigkeitMärklin $
                    buttonNewWithEventLabel Language.umdrehen $
                        märklinBahngeschwindigkeitAktionHinzufügen $ pure . Umdrehen
                -- AktionBahngeschwindigkeit 'Lego
                boxAktionBahngeschwindigkeitLego <- Gtk.hBoxNew False 0
                let
                    zeigeLegoBahngeschwindigkeitAktionAuswahl :: IO ()
                    zeigeLegoBahngeschwindigkeitAktionAuswahl = do
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                        mitWidgetShow vBoxHinzufügenPlanBahngeschwindigkeitenLego
                        mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
                        mitWidgetHide vBoxHinzufügenPlanKupplungen
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
                        mitWidgetHide vBoxHinzufügenPlanPläne
                        mitWidgetShow windowAktionObjektAuswahl
                    legoBahngeschwindigkeitAktionHinzufügen :: 
                        (forall b. (BahngeschwindigkeitKlasse b) =>
                            b 'Lego -> IO (AktionBahngeschwindigkeit b 'Lego)) ->
                                IO ()
                    legoBahngeschwindigkeitAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
                        Gtk.postGUIAsync $ zeigeLegoBahngeschwindigkeitAktionAuswahl
                        atomically (takeTMVar tmvarPlanObjekt) >>= \case
                            (Just (OBahngeschwindigkeit (ZugtypLego bgLego)))
                                -> aktionKonstruktor bgLego >>= aktionHinzufügen . ABahngeschwindigkeitLego
                            (Just (OWegstrecke (ZugtypLego wsLego)))
                                -> aktionKonstruktor wsLego >>=
                                    aktionHinzufügen . AWegstreckeLego . AWSBahngeschwindigkeit
                            (Just anderesObjekt)
                                -> error $
                                    "unerwartetes Objekt für Lego-Bahngeschwindigkeit-Aktion erhalten: " ++
                                    show anderesObjekt
                            Nothing
                                -> pure ()
                        Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
                legoGeschwindigkeitsScale <-
                    boxPackWidgetNew boxAktionBahngeschwindigkeitLego PackGrow paddingDefault positionDefault $
                        Gtk.hScaleNewWithRange 0 100 1
                boxPackWidgetNewDefault boxAktionBahngeschwindigkeitLego $
                    buttonNewWithEventLabel Language.geschwindigkeit $
                        legoBahngeschwindigkeitAktionHinzufügen $ \bg -> 
                            Geschwindigkeit bg . floor <$> Gtk.get legoGeschwindigkeitsScale Gtk.rangeValue
                legoFahrtrichtungAuswahl <- widgetShowNew $ boundedEnumAuswahlRadioButtonNew Vorwärts ("" :: Text)
                boxPackWidgetNewDefault boxAktionBahngeschwindigkeitLego $
                    buttonNewWithEventLabel Language.fahrtrichtungEinstellen $
                        legoBahngeschwindigkeitAktionHinzufügen $ \bg -> FahrtrichtungEinstellen bg <$> aktuelleAuswahl legoFahrtrichtungAuswahl
                boxPackDefault boxAktionBahngeschwindigkeitLego legoFahrtrichtungAuswahl
                -- ZugtypSpezifisch Bahngeschwindigkeit
                boxPackWidgetNewDefault boxPlan $ zugtypSpezifischNew
                    ((Märklin, boxAktionBahngeschwindigkeitMärklin) :| [(Lego, boxAktionBahngeschwindigkeitLego)])
                    zugtypAuswahl
                -- AktionStreckenabschnitt
                boxAktionStreckenabschnitt <- boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
                let
                    zeigeStreckenabschnittAktionAuswahl :: IO ()
                    zeigeStreckenabschnittAktionAuswahl = do
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
                        mitWidgetShow vBoxHinzufügenPlanStreckenabschnitte
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
                        mitWidgetHide vBoxHinzufügenPlanKupplungen
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
                        mitWidgetHide vBoxHinzufügenPlanPläne
                        mitWidgetShow windowAktionObjektAuswahl
                    streckenabschnittAktionHinzufügen :: 
                        (forall s. (StreckenabschnittKlasse s) =>
                            s -> IO (AktionStreckenabschnitt s)) ->
                                IO ()
                    streckenabschnittAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
                        Gtk.postGUIAsync $ zeigeStreckenabschnittAktionAuswahl
                        atomically (takeTMVar tmvarPlanObjekt) >>= \case
                            (Just (OStreckenabschnitt st))
                                -> aktionKonstruktor st >>= aktionHinzufügen . AStreckenabschnitt
                            (Just (OWegstrecke (ZugtypMärklin wsMärklin)))
                                -> aktionKonstruktor wsMärklin >>=
                                    aktionHinzufügen . AWegstreckeMärklin . AWSStreckenabschnitt
                            (Just (OWegstrecke (ZugtypLego wsLego)))
                                -> aktionKonstruktor wsLego >>=
                                    aktionHinzufügen . AWegstreckeLego . AWSStreckenabschnitt
                            (Just anderesObjekt)
                                -> error $
                                    "unerwartetes Objekt für Streckenabschnitt-Aktion erhalten: " ++
                                    show anderesObjekt
                            Nothing
                                -> pure ()
                        Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
                auswahlStrom <- widgetShowNew $ boundedEnumAuswahlRadioButtonNew Fließend ("" :: Text)
                boxPackWidgetNewDefault boxAktionStreckenabschnitt $
                    buttonNewWithEventLabel Language.strom $
                        streckenabschnittAktionHinzufügen $ \st -> Strom st <$> aktuelleAuswahl auswahlStrom
                boxPackDefault boxAktionStreckenabschnitt auswahlStrom
                -- AktionWeiche
                boxAktionWeiche <- boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
                let
                    zeigeWeicheAktionAuswahl :: Richtung -> IO ()
                    zeigeWeicheAktionAuswahl richtung = do
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
                        mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
                        widgetShowIf (richtung == Gerade) vBoxHinzufügenPlanWeichenGeradeMärklin
                        widgetShowIf (richtung == Kurve) vBoxHinzufügenPlanWeichenKurveMärklin
                        widgetShowIf (richtung == Links) vBoxHinzufügenPlanWeichenLinksMärklin
                        widgetShowIf (richtung == Rechts) vBoxHinzufügenPlanWeichenRechtsMärklin
                        widgetShowIf (richtung == Gerade) vBoxHinzufügenPlanWeichenGeradeLego
                        widgetShowIf (richtung == Kurve) vBoxHinzufügenPlanWeichenKurveLego
                        widgetShowIf (richtung == Links) vBoxHinzufügenPlanWeichenLinksLego
                        widgetShowIf (richtung == Rechts) vBoxHinzufügenPlanWeichenRechtsLego
                        mitWidgetHide vBoxHinzufügenPlanKupplungen
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
                        mitWidgetHide vBoxHinzufügenPlanPläne
                        mitWidgetShow windowAktionObjektAuswahl
                    weicheAktionHinzufügen :: Richtung -> IO ()
                    weicheAktionHinzufügen richtung = void $ forkIO $ do
                        Gtk.postGUIAsync $ zeigeWeicheAktionAuswahl richtung
                        atomically (takeTMVar tmvarPlanObjekt) >>= \case
                            (Just (OWeiche we))
                                -> aktionHinzufügen $ AWeiche $ Stellen we richtung
                            (Just anderesObjekt)
                                -> error $
                                    "unerwartetes Objekt zum Weiche stellen erhalten: " ++ show anderesObjekt
                            Nothing
                                -> pure ()
                        Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
                buttonAktionWeicheGerade <- boxPackWidgetNewDefault boxAktionWeiche $
                    buttonNewWithEventLabel (Language.stellen <:> Language.gerade) $
                        weicheAktionHinzufügen Gerade
                buttonAktionWeicheKurve <- boxPackWidgetNewDefault boxAktionWeiche $
                    buttonNewWithEventLabel (Language.stellen <:> Language.kurve) $
                        weicheAktionHinzufügen Kurve
                buttonAktionWeicheLinks <- boxPackWidgetNewDefault boxAktionWeiche $
                    buttonNewWithEventLabel (Language.stellen <:> Language.links) $
                        weicheAktionHinzufügen Links
                buttonAktionWeicheRechts <- boxPackWidgetNewDefault boxAktionWeiche $
                    buttonNewWithEventLabel (Language.stellen <:> Language.rechts) $
                        weicheAktionHinzufügen Rechts
                -- AktionKupplung
                boxAktionKupplung <- boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
                let
                    zeigeKupplungAktionAuswahl :: IO ()
                    zeigeKupplungAktionAuswahl = do
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
                        mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
                        mitWidgetShow vBoxHinzufügenPlanKupplungen
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenKupplungMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenKupplungLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
                        mitWidgetHide vBoxHinzufügenPlanPläne
                        mitWidgetShow windowAktionObjektAuswahl
                    kupplungAktionHinzufügen :: 
                        (forall k. (KupplungKlasse k) =>
                            k -> IO (AktionKupplung k)) ->
                                IO ()
                    kupplungAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
                        Gtk.postGUIAsync $ zeigeKupplungAktionAuswahl
                        atomically (takeTMVar tmvarPlanObjekt) >>= \case
                            (Just (OKupplung ku))
                                -> aktionKonstruktor ku >>= aktionHinzufügen . AKupplung
                            (Just (OWegstrecke (ZugtypMärklin wsMärklin)))
                                -> aktionKonstruktor wsMärklin >>=
                                    aktionHinzufügen . AWegstreckeMärklin . AWSKupplung
                            (Just (OWegstrecke (ZugtypLego wsLego)))
                                -> aktionKonstruktor wsLego >>=
                                    aktionHinzufügen . AWegstreckeLego . AWSKupplung
                            (Just anderesObjekt)
                                -> error $
                                    "unerwartetes Objekt für Streckenabschnitt-Aktion erhalten: " ++
                                    show anderesObjekt
                            Nothing
                                -> pure ()
                        Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
                boxPackWidgetNewDefault boxAktionKupplung $
                    buttonNewWithEventLabel Language.kuppeln $
                        kupplungAktionHinzufügen $ pure . Kuppeln
                -- AktionWegstrecke
                boxAktionWegstrecke <- boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
                let
                    zeigeWegstreckeAktionAuswahl :: IO ()
                    zeigeWegstreckeAktionAuswahl = do
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
                        mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
                        mitWidgetHide vBoxHinzufügenPlanKupplungen
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
                        mitWidgetShow vBoxHinzufügenPlanWegstreckenLego
                        mitWidgetHide vBoxHinzufügenPlanPläne
                        mitWidgetShow windowAktionObjektAuswahl
                    wegstreckeAktionHinzufügen ::
                        (forall w z. (WegstreckeKlasse (w z)) =>
                            w z -> IO (AktionWegstrecke w z)) ->
                                IO ()
                    wegstreckeAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
                        Gtk.postGUIAsync $ zeigeWegstreckeAktionAuswahl 
                        atomically (takeTMVar tmvarPlanObjekt) >>= \case
                            (Just (OWegstrecke (ZugtypMärklin wsMärklin)))
                                -> aktionKonstruktor wsMärklin >>= aktionHinzufügen . AWegstreckeMärklin
                            (Just (OWegstrecke (ZugtypLego wsLego)))
                                -> aktionKonstruktor wsLego >>= aktionHinzufügen . AWegstreckeLego
                            (Just anderesObjekt)
                                -> error $
                                    "unerwartetes Objekt für Wegstrecke-Aktion erhalten: " ++
                                    show anderesObjekt
                            Nothing
                                -> pure ()
                        Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
                boxPackWidgetNewDefault boxAktionWegstrecke $
                    buttonNewWithEventLabel Language.einstellen $
                        wegstreckeAktionHinzufügen $ pure . Einstellen
                -- AktionPlan
                boxAktionPlan <- boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
                let
                    zeigePlanAktionAuswahl :: IO ()
                    zeigePlanAktionAuswahl = do
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                        mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
                        mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
                        mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
                        mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
                        mitWidgetHide vBoxHinzufügenPlanKupplungen
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
                        mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
                        mitWidgetShow vBoxHinzufügenPlanPläne
                        mitWidgetShow windowAktionObjektAuswahl
                    planAktionHinzufügen :: (Plan -> IO Aktion) -> IO ()
                    planAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
                        Gtk.postGUIAsync zeigePlanAktionAuswahl 
                        atomically (takeTMVar tmvarPlanObjekt) >>= \case
                            (Just (OPlan plan))
                                -> aktionKonstruktor plan >>= aktionHinzufügen
                            (Just anderesObjekt)
                                -> error $
                                    "unerwartetes Objekt für Plan-Aktion erhalten: " ++
                                    show anderesObjekt
                            Nothing
                                -> pure ()
                        Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
                boxPackWidgetNewDefault boxAktionPlan $
                    buttonNewWithEventLabel Language.aktionAusführen $
                        planAktionHinzufügen $ pure . AktionAusführen
                -- Zeige aktuelle Aktionen an
                boxPackDefault boxPlan expanderAktionen
                boxPackWidgetNewDefault boxPlan $ buttonNewWithEventLabel Language.rückgängig $ do
                    aktionenVorher <- atomically $ do
                        aktionen <- readTVar tvarAktionen
                        let aktionenVorher = case zeigeLetztes aktionen of
                                Leer
                                    -> leer
                                Gefüllt _letztes warteschlange
                                    -> warteschlange
                        writeTVar tvarAktionen aktionenVorher
                        pure aktionenVorher
                    zeigeAktionen aktionenVorher
                -- Dauerschleife
                checkButtonDauerschleife <- boxPackWidgetNewDefault boxPlan $
                    Gtk.checkButtonNewWithLabel (Language.dauerschleife :: Text)
                let
                    seiteZurücksetzenPlan :: IO ()
                    seiteZurücksetzenPlan = do
                        -- aktuelle Aktionen zurücksetzen
                        atomically $ writeTVar tvarAktionen leer
                        zeigeAktionen leer
                        -- Entry zurücksetzten
                        Gtk.set (erhalteEntry nameAuswahlPlan)
                            [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True]
                        let
                            versteckeWennLeer :: (MitWidget w) =>
                                BoxPlanHinzufügen a ->
                                Maybe (BoxPlanHinzufügen b) ->
                                Maybe (BoxPlanHinzufügen c) ->
                                w ->
                                    IO ()
                            versteckeWennLeer boxPlanA maybeBoxPlanB maybeBoxPlanC widget = do
                                a <- widgetHinzufügenContainerGefüllt boxPlanA
                                b <- maybe (pure False) widgetHinzufügenContainerGefüllt maybeBoxPlanB
                                c <- maybe (pure False) widgetHinzufügenContainerGefüllt maybeBoxPlanC
                                widgetShowIf (a || b || c) widget
                        versteckeWennLeer
                            vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                            (Just vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin)
                            Nothing
                            boxAktionBahngeschwindigkeitMärklin
                        versteckeWennLeer
                            vBoxHinzufügenPlanBahngeschwindigkeitenLego
                            (Just vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin)
                            Nothing
                            boxAktionBahngeschwindigkeitLego
                        versteckeWennLeer
                            vBoxHinzufügenPlanStreckenabschnitte
                            (Just vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin)
                            (Just vBoxHinzufügenPlanWegstreckenStreckenabschnittLego)
                            boxAktionStreckenabschnitt
                        versteckeWennLeer
                            vBoxHinzufügenPlanWeichenGeradeMärklin
                            (Just vBoxHinzufügenPlanWeichenGeradeLego)
                            Nothing
                            buttonAktionWeicheGerade
                        versteckeWennLeer
                            vBoxHinzufügenPlanWeichenKurveMärklin
                            (Just vBoxHinzufügenPlanWeichenKurveLego)
                            Nothing
                            buttonAktionWeicheKurve
                        versteckeWennLeer
                            vBoxHinzufügenPlanWeichenLinksMärklin
                            (Just vBoxHinzufügenPlanWeichenLinksLego)
                            Nothing
                            buttonAktionWeicheLinks
                        versteckeWennLeer
                            vBoxHinzufügenPlanWeichenRechtsMärklin
                            (Just vBoxHinzufügenPlanWeichenRechtsLego)
                            Nothing
                            buttonAktionWeicheRechts
                        versteckeWennLeer
                            vBoxHinzufügenPlanKupplungen
                            (Just vBoxHinzufügenPlanWegstreckenKupplungMärklin)
                            (Just vBoxHinzufügenPlanWegstreckenKupplungLego)
                            boxAktionKupplung
                        versteckeWennLeer
                            vBoxHinzufügenPlanWegstreckenMärklin
                            (Just vBoxHinzufügenPlanWegstreckenLego)
                            Nothing
                            boxAktionWegstrecke
                        versteckeWennLeer
                            vBoxHinzufügenPlanPläne
                            Nothing
                            Nothing
                            boxAktionPlan
                    seitePlan :: AssistantSeite HinzufügenSeite
                    seitePlan = AssistantSeite {
                        seite = HinzufügenSeitePlan {
                            widget = erhalteWidget boxPlan,
                            nameAuswahl = nameAuswahlPlan,
                            tvarAktionen,
                            checkButtonDauerschleife},
                        name = Language.plan,
                        seiteZurücksetzen = seiteZurücksetzenPlan,
                        seitenAbschluss = SeitenAbschlussButton seitenAbschlussPlan}
                -- konstruiere SeitenBaum
                let
                    seitenBaum :: AssistantSeitenBaum HinzufügenSeite
                    seitenBaum = AssistantSeiteAuswahl {
                        node = seiteAuswahl,
                        nachfolgerFrage = Language.welchesObjektHinzufügen,
                        nachfolgerListe = AssistantSeiteLetzte <$>
                            seiteBahngeschwindigkeit :| [
                            seiteStreckenabschnitt,
                            seiteWeiche,
                            seiteKupplung,
                            seiteWegstrecke,
                            seitePlan]}
                assistant <- assistantNew parent globaleWidgets seitenBaum $
                    flip runReaderT objektReader . hinzufügenErgebnis zugtypAuswahl fließendAuswahl
                Gtk.set windowAktionObjektAuswahl
                    [Gtk.windowTransientFor := erhalteWindow assistant, Gtk.windowModal := True]
                pure assistant
#endif