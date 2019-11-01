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
import Control.Concurrent.STM (newEmptyTMVarIO, newTMVarIO)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import qualified Control.Monad.RWS as RWS
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.Options (Options(..), getOptions)
import Zug.Language ((<~>), (<|>))
import qualified Zug.Language as Language
import Zug.UI.Base (Status, statusLeer, tvarMapsNeu, auswertenTMVarIOStatus)
import Zug.UI.Befehl (BefehlKlasse(..), BefehlAllgemein(..))
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), boxWegstreckeHinzufügenNew, boxPlanHinzufügenNew,
                                    StatusGui, MStatusGuiT, foldWegstreckeHinzufügen)
import Zug.UI.Gtk.Fenster (buttonSpeichernPack, buttonLadenPack, ladeWidgets, buttonHinzufügenPack)
import Zug.UI.Gtk.FortfahrenWennToggled (fortfahrenWennToggledTMVarNew)
import Zug.UI.Gtk.Hilfsfunktionen (widgetShowNew, buttonNewWithEventMnemonic,
                                    containerAddWidgetNew, boxPack, boxPackWidgetNew,
                                    Packing(..), packingDefault, paddingDefault,
                                    Position(..), positionDefault,
                                    notebookAppendPageNew)
import Zug.UI.Gtk.ScrollbaresWidget (scrollbaresWidgetNew)

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
    -- Hauptfenster
    windowMain <- widgetShowNew Gtk.windowNew
    Gtk.set windowMain [
        Gtk.windowTitle := (Language.zugkontrolle <~> ZUGKONTROLLEVERSION :: Text),
        Gtk.windowDefaultWidth := 640,
        Gtk.windowDefaultHeight := 480]
    -- windowDefaultHeight wird aus irgendeinem Grund ignoriert.
    -- Wird hier trotzdem gesetzt für den Fall, dass sich das in einer neueren Version ändert.
    Gtk.on windowMain Gtk.deleteEvent $ liftIO $ Gtk.mainQuit >> pure False
    vBox <- containerAddWidgetNew windowMain $ Gtk.vBoxNew False 0
    tvarMaps <- tvarMapsNeu
    tmvarStatus <- newTMVarIO statusLeer
    -- Notebook mit aktuellen Elementen
    notebookElemente <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    (panedEinzelObjekte, _page) <- notebookAppendPageNew
        notebookElemente
        (Language.bahngeschwindigkeiten <|> Language.streckenabschnitte <|> Language.weichen <|> Language.kupplungen )
        Gtk.hPanedNew
    vPanedLeft <- widgetShowNew Gtk.vPanedNew
    Gtk.panedAdd1 panedEinzelObjekte vPanedLeft
    frameLeftTop <- widgetShowNew Gtk.frameNew
    Gtk.set frameLeftTop [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 vPanedLeft frameLeftTop
    vBoxBahngeschwindigkeiten <- containerAddWidgetNew frameLeftTop $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    frameLeftBot <- widgetShowNew Gtk.frameNew
    Gtk.set frameLeftBot [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 vPanedLeft frameLeftBot
    vBoxStreckenabschnitte <- containerAddWidgetNew frameLeftBot $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    vPanedRight <- widgetShowNew Gtk.vPanedNew
    Gtk.panedAdd2 panedEinzelObjekte vPanedRight
    frameRightTop <- widgetShowNew Gtk.frameNew
    Gtk.set frameRightTop [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 vPanedLeft frameRightTop
    vBoxWeichen <- containerAddWidgetNew frameRightTop $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    frameRightBot <- widgetShowNew Gtk.frameNew
    Gtk.set frameRightBot [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 vPanedLeft frameRightBot
    vBoxKupplungen <- containerAddWidgetNew frameRightBot $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    (panedSammelObjekte, _page) <- notebookAppendPageNew
        notebookElemente
        (Language.wegstrecken <|> Language.pläne)
        Gtk.hPanedNew
    frameWegstrecken <- widgetShowNew Gtk.frameNew
    Gtk.set frameWegstrecken [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 panedSammelObjekte frameWegstrecken
    vBoxWegstrecken <- containerAddWidgetNew frameWegstrecken $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    framePläne <- widgetShowNew Gtk.frameNew
    Gtk.set framePläne [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 panedSammelObjekte framePläne
    vBoxPläne <- containerAddWidgetNew framePläne $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    -- DynamischeWidgets
    progressBarPlan
        <- widgetShowNew Gtk.progressBarNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
        <- boxWegstreckeHinzufügenNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
        <- boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanBahngeschwindigkeitenLego
        <- boxPlanHinzufügenNew
    vBoxHinzufügenWegstreckeStreckenabschnitte
        <- boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanStreckenabschnitte
        <- boxPlanHinzufügenNew
    vBoxHinzufügenWegstreckeWeichenMärklin
        <- boxWegstreckeHinzufügenNew
    vBoxHinzufügenWegstreckeWeichenLego
        <- boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanWeichenGeradeMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenGeradeLego
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenKurveMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenKurveLego
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenLinksMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenLinksLego
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenRechtsMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWeichenRechtsLego
        <- boxPlanHinzufügenNew
    vBoxHinzufügenWegstreckeKupplungen
        <- boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanKupplungen
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenKupplungMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenKupplungLego
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenMärklin
        <- boxPlanHinzufügenNew
    vBoxHinzufügenPlanWegstreckenLego
        <- boxPlanHinzufügenNew
    fortfahrenWennToggledWegstrecke <- fortfahrenWennToggledTMVarNew Language.hinzufügen foldWegstreckeHinzufügen tmvarStatus
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
        progressBarPlan,
        windowMain,
        fortfahrenWennToggledWegstrecke,
        tmvarPlanObjekt}
    let objektReader = (tvarMaps, dynamischeWidgets, tmvarStatus)
    -- Knopf-Leiste mit globalen Funktionen
    functionBox <- boxPackWidgetNew vBox PackNatural paddingDefault End $ Gtk.hBoxNew False 0
    flip runReaderT objektReader $ do
        buttonHinzufügenPack windowMain functionBox
        liftIO $ boxPack functionBox progressBarPlan PackGrow paddingDefault positionDefault
        buttonSpeichernPack windowMain functionBox
        buttonLadenPack windowMain functionBox
    boxPackWidgetNew functionBox packingDefault paddingDefault End $
        buttonNewWithEventMnemonic Language.beenden $ Gtk.mainQuit
    -- Lade Datei angegeben in Kommandozeilenargument
    (Options {load=dateipfad}) <- getOptions
    -- neuer Status ist schon in tmvarStatus gespeichert und muss nicht mehr neu gesetzt werden
    let
        ladeAktion :: Status -> IO StatusGui
        ladeAktion = flip runReaderT objektReader . ladeWidgets
        fehlerBehandlung :: MStatusGuiT IO ()
        fehlerBehandlung = RWS.put statusLeer
        statusAktion :: MStatusGuiT IO Bool
        statusAktion = ausführenBefehl $ Laden dateipfad ladeAktion fehlerBehandlung
    flip runReaderT objektReader $ auswertenTMVarIOStatus statusAktion tmvarStatus
#endif