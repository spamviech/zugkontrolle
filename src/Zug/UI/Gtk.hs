{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-|
Description : Erstelle GUI und starte den GTK-Main-Loop.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk (main) where

import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))
import qualified Zug.Language as Language
import qualified Zug.UI.Cmd as Cmd

-- | GTK-main loop nicht verfügbar. Weiche auf Cmd-UI aus
main :: IO ()
main = setSGR [SetColor Foreground Vivid Red] >> putStrLn Language.uiNichtUnterstützt >> setSGR [Reset] >> Cmd.main
#else
module Zug.UI.Gtk (main, setupGUI) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, newEmptyTMVarIO)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import qualified Control.Monad.RWS as RWS
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.Options (Options(..), getOptions)
import Zug.Language ((<~>), (<|>))
import qualified Zug.Language as Language
import Zug.UI.Base (Status, statusLeer, tvarMapsNeu)
import Zug.UI.Befehl (BefehlAllgemein(..))
import Zug.UI.StatusVar (statusVarNew, ausführenStatusVarBefehl, readStatusVar)
import Zug.UI.Gtk.Auswahl (boundedEnumAuswahlComboBoxNew, beiAuswahl)
import Zug.UI.Gtk.Fenster (buttonSpeichernPack, buttonLadenPack, ladeWidgets, buttonHinzufügenPack)
import Zug.UI.Gtk.FortfahrenWennToggled (fortfahrenWennToggledVarNew)
import Zug.UI.Gtk.Hilfsfunktionen (
    widgetShowNew, buttonNewWithEventLabel,
    containerAddWidgetNew, boxPack, boxPackWidgetNew, boxPackWidgetNewDefault,
    Packing(..), packingDefault, paddingDefault,
    Position(..), positionDefault,
    notebookAppendPageNew, labelSpracheNew)
import Zug.UI.Gtk.ScrollbaresWidget (scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (spracheGuiNeu, verwendeSpracheGuiFn, sprachwechsel)
import Zug.UI.Gtk.StreckenObjekt (
    DynamischeWidgets(..), boxWegstreckeHinzufügenNew, boxPlanHinzufügenNew,
    MStatusGuiT, IOStatusGui, foldWegstreckeHinzufügen)

-- | main loop
main :: IO ()
main = do
    -- Initialisiere GTK+ engine
    Gtk.initGUI
    -- Erstelle GUI
    setupGUI
    -- GTK+ main loop
    Gtk.mainGUI

-- | Erstelle GUI inkl. sämtlicher Events.
-- 
-- Zur Verwendung muss vorher 'initGUI' aufgerufen werden.
setupGUI :: IO ()
setupGUI = void $ do
    Options {load=dateipfad, sprache} <- getOptions
    spracheGui <- spracheGuiNeu sprache
    -- Hauptfenster
    windowMain <- widgetShowNew Gtk.windowNew
    Gtk.set windowMain [
        Gtk.windowDefaultWidth := 640,
        Gtk.windowDefaultHeight := 480]
    verwendeSpracheGuiFn spracheGui $ \sprache ->
        Gtk.set windowMain [Gtk.windowTitle := (Language.zugkontrolle <~> Language.version) sprache]
    -- windowDefaultHeight wird aus irgendeinem Grund ignoriert.
    -- Wird hier trotzdem gesetzt für den Fall, dass sich das in einer neueren Version ändert.
    Gtk.on windowMain Gtk.deleteEvent $ liftIO $ Gtk.mainQuit >> pure False
    vBox <- containerAddWidgetNew windowMain $ Gtk.vBoxNew False 0
    tvarMaps <- tvarMapsNeu
    statusVar <- statusVarNew $ statusLeer spracheGui
    -- Notebook mit aktuellen Elementen
    notebookElemente <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    (panedEinzelObjekte, _page) <- flip runReaderT spracheGui $ notebookAppendPageNew
        notebookElemente
        (Language.bahngeschwindigkeiten <|> Language.streckenabschnitte <|> Language.weichen <|> Language.kupplungen )
        $ liftIO Gtk.hPanedNew
    vPanedLeft <- widgetShowNew Gtk.vPanedNew
    Gtk.panedAdd1 panedEinzelObjekte vPanedLeft
    frameLeftTop <- widgetShowNew Gtk.frameNew
    Gtk.set frameLeftTop [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 vPanedLeft frameLeftTop
    vBoxBahngeschwindigkeiten <- containerAddWidgetNew frameLeftTop $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui $ boxPackWidgetNewDefault vBoxBahngeschwindigkeiten $
        labelSpracheNew Language.bahngeschwindigkeiten
    frameLeftBot <- widgetShowNew Gtk.frameNew
    Gtk.set frameLeftBot [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 vPanedLeft frameLeftBot
    vBoxStreckenabschnitte <- containerAddWidgetNew frameLeftBot $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui $ boxPackWidgetNewDefault vBoxStreckenabschnitte $
        labelSpracheNew Language.streckenabschnitte
    vPanedRight <- widgetShowNew Gtk.vPanedNew
    Gtk.panedAdd2 panedEinzelObjekte vPanedRight
    frameRightTop <- widgetShowNew Gtk.frameNew
    Gtk.set frameRightTop [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 vPanedRight frameRightTop
    vBoxWeichen <- containerAddWidgetNew frameRightTop $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui $ boxPackWidgetNewDefault vBoxWeichen $
        labelSpracheNew Language.weichen
    frameRightBot <- widgetShowNew Gtk.frameNew
    Gtk.set frameRightBot [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 vPanedRight frameRightBot
    vBoxKupplungen <- containerAddWidgetNew frameRightBot $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui $ boxPackWidgetNewDefault vBoxKupplungen $
        labelSpracheNew Language.kupplungen
    (panedSammelObjekte, _page) <- flip runReaderT spracheGui $ notebookAppendPageNew
        notebookElemente
        (Language.wegstrecken <|> Language.pläne)
        $ liftIO Gtk.hPanedNew
    frameWegstrecken <- widgetShowNew Gtk.frameNew
    Gtk.set frameWegstrecken [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 panedSammelObjekte frameWegstrecken
    vBoxWegstrecken <- containerAddWidgetNew frameWegstrecken $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui $ boxPackWidgetNewDefault vBoxWegstrecken $
        labelSpracheNew Language.wegstrecken
    framePläne <- widgetShowNew Gtk.frameNew
    Gtk.set framePläne [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 panedSammelObjekte framePläne
    vBoxPläne <- containerAddWidgetNew framePläne $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui $ boxPackWidgetNewDefault vBoxPläne $
        labelSpracheNew Language.pläne
    -- DynamischeWidgets
    progressBarPlan
        <- widgetShowNew Gtk.progressBarNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanBahngeschwindigkeitenLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenWegstreckeStreckenabschnitte
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanStreckenabschnitte
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenWegstreckeWeichenMärklin
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenWegstreckeWeichenLego
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanWeichenGeradeMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenGeradeLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenKurveMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenKurveLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenLinksMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenLinksLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenRechtsMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenRechtsLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenWegstreckeKupplungen
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanKupplungen
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenKupplungMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenKupplungLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    vBoxHinzufügenPlanPläne
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew
    fortfahrenWennToggledWegstrecke <- flip runReaderT spracheGui $
        fortfahrenWennToggledVarNew Language.hinzufügen foldWegstreckeHinzufügen (atomically . readStatusVar) statusVar
    tmvarPlanObjekt
        <- newEmptyTMVarIO
    let dynamischeWidgets = DynamischeWidgets {
        vBoxBahngeschwindigkeiten,
        vBoxStreckenabschnitte,
        vBoxWeichen,
        vBoxKupplungen,
        vBoxWegstrecken,
        vBoxPläne,
        vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin,
        vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego,
        vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
        vBoxHinzufügenPlanBahngeschwindigkeitenLego,
        vBoxHinzufügenWegstreckeStreckenabschnitte,
        vBoxHinzufügenPlanStreckenabschnitte,
        vBoxHinzufügenWegstreckeWeichenMärklin,
        vBoxHinzufügenWegstreckeWeichenLego,
        vBoxHinzufügenPlanWeichenGeradeMärklin,
        vBoxHinzufügenPlanWeichenGeradeLego,
        vBoxHinzufügenPlanWeichenKurveMärklin,
        vBoxHinzufügenPlanWeichenKurveLego,
        vBoxHinzufügenPlanWeichenLinksMärklin,
        vBoxHinzufügenPlanWeichenLinksLego,
        vBoxHinzufügenPlanWeichenRechtsMärklin,
        vBoxHinzufügenPlanWeichenRechtsLego,
        vBoxHinzufügenWegstreckeKupplungen,
        vBoxHinzufügenPlanKupplungen,
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
        vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
        vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
        vBoxHinzufügenPlanWegstreckenKupplungMärklin,
        vBoxHinzufügenPlanWegstreckenKupplungLego,
        vBoxHinzufügenPlanWegstreckenMärklin,
        vBoxHinzufügenPlanWegstreckenLego,
        vBoxHinzufügenPlanPläne,
        progressBarPlan,
        windowMain,
        fortfahrenWennToggledWegstrecke,
        tmvarPlanObjekt}
    let objektReader = (tvarMaps, dynamischeWidgets, statusVar)
    -- Knopf-Leiste mit globalen Funktionen
    functionBox <- boxPackWidgetNew vBox PackNatural paddingDefault End $ Gtk.hBoxNew False 0
    flip runReaderT objektReader $ do
        buttonHinzufügenPack windowMain functionBox
        spracheAuswahl <- boxPackWidgetNewDefault functionBox $
            boundedEnumAuswahlComboBoxNew Language.Deutsch Language.sprache
        beiAuswahl spracheAuswahl $ \sprache -> void $ do
            spracheGuiNeu <- sprachwechsel spracheGui sprache
            flip runReaderT objektReader $ ausführenStatusVarBefehl (SprachWechsel spracheGuiNeu) statusVar
        liftIO $ boxPack functionBox progressBarPlan PackGrow paddingDefault positionDefault
        buttonSpeichernPack windowMain functionBox
        buttonLadenPack windowMain functionBox
        boxPackWidgetNew functionBox packingDefault paddingDefault End $
            buttonNewWithEventLabel Language.beenden $ Gtk.mainQuit
    -- Lade Datei angegeben in Kommandozeilenargument
    let
        ladeAktion :: Status -> IOStatusGui ()
        ladeAktion statusNeu = do
            state0 <- RWS.get
            state1 <- liftIO $ flip runReaderT objektReader $ fst <$>
                RWS.execRWST (ladeWidgets statusNeu) objektReader state0
            RWS.put state1
        fehlerBehandlung :: MStatusGuiT IO ()
        fehlerBehandlung = RWS.put $ statusLeer spracheGui
    flip runReaderT objektReader $
        ausführenStatusVarBefehl (Laden dateipfad ladeAktion fehlerBehandlung) statusVar
#endif