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
import qualified Control.Monad.State as State
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Graphics.UI.Gtk (initGUI, mainGUI, mainQuit, set, on, AttrOp(..), vBoxNew, hBoxNew,
                    windowTitle, windowDefaultWidth, windowDefaultHeight, deleteEvent,
                    windowNew, Packing(..), notebookNew, progressBarNew, buttonNewWithLabel)
-- Abhängigkeiten von anderen Modulen
import Zug.Options (Options(..), getOptions)
import Zug.Language ((<~>))
import qualified Zug.Language as Language
import Zug.UI.Base (Status, statusLeer)
import Zug.UI.Befehl (BefehlKlasse(..), BefehlAllgemein(..))
import Zug.UI.Gtk.StreckenObjekt ()
import Zug.UI.Gtk.Fenster ()
import Zug.UI.Gtk.FortfahrenWennToggled ()

-- | main loop
main :: IO ()
main = do
    -- Initialisiere GTK+ engine
    initGUI
    -- Erstelle GUI
    setupGUI
    -- GTK+ main loop
    mainGUI

-- | Erstelle GUI inkl. sämtlicher Events.
-- 
-- Zur Verwendung muss vorher 'initGUI' aufgerufen werden.
setupGUI :: IO ()
setupGUI = void $ do
    -- Hauptfenster
    windowMain <- widgetShowNew windowNew
    set windowMain [
        windowTitle := (Language.zugkontrolle <~> ZUGKONTROLLEVERSION :: Text),
        windowDefaultWidth := 640,
        windowDefaultHeight := 480]
    -- windowDefaultHeight wird aus irgendeinem Grund ignoriert. Wird hier trotzdem gesetzt für den Fall, dass sich das in einer neueren Version ändert.
    on windowMain deleteEvent $ liftIO $ mainQuit >> pure False
    vBox <- containerAddWidgetNew windowMain $ vBoxNew False 0
    statusInitial <- statusLeerNeu
    tmvarStatus <- newTMVarIO statusInitial
    -- Notebook mit aktuellen Elementen
    notebookElemente <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault notebookNew
    (_scrolledWindow, vBoxBahngeschwindigkeiten)
        <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.bahngeschwindigkeiten $ vBoxNew False 0
    (_scrolledWindow, vBoxStreckenabschnitte)
        <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.streckenabschnitte $ vBoxNew False 0
    (_scrolledWindow, vBoxWeichen)
        <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.weichen $ vBoxNew False 0
    (_scrolledWindow, vBoxKupplungen)
        <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.kupplungen $ vBoxNew False 0
    (_scrolledWindow, vBoxWegstrecken)
        <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.wegstrecken $ vBoxNew False 0
    (_scrolledWindow, vBoxPläne)
        <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.pläne $ vBoxNew False 0
    progressBarPlan
        <- widgetShowNew progressBarNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeiten
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeiten
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeitenLego
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeStreckenabschnitte
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanStreckenabschnitte
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeWeichen
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenGerade
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenKurve
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenLinks
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenRechts
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeKupplungen
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanKupplungen
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenStreckenabschnitt
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenWeiche
        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenKupplung
        <- widgetShowNew $ vBoxNew False 0
    buttonHinzufügenWegstrecke
        <- buttonNewWithLabel (Language.hinzufügen :: Text)
    fortfahrenWennToggledWegstrecke <- fortfahrenWennToggledTMVar buttonHinzufügenWegstrecke tmvarStatus
    tmvarPlanObjekt
        <- newEmptyTMVarIO
    let dynamischeWidgets = DynamischeWidgets {
        vBoxBahngeschwindigkeiten,
        vBoxStreckenabschnitte,
        vBoxWeichen,
        vBoxKupplungen,
        vBoxWegstrecken,
        vBoxPläne,
        vBoxHinzufügenWegstreckeBahngeschwindigkeiten,
        vBoxHinzufügenPlanBahngeschwindigkeiten,
        vBoxHinzufügenPlanBahngeschwindigkeitenLego,
        vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
        vBoxHinzufügenWegstreckeStreckenabschnitte,
        vBoxHinzufügenPlanStreckenabschnitte,
        vBoxHinzufügenWegstreckeWeichen,
        vBoxHinzufügenPlanWeichenGerade,
        vBoxHinzufügenPlanWeichenKurve,
        vBoxHinzufügenPlanWeichenLinks,
        vBoxHinzufügenPlanWeichenRechts,
        vBoxHinzufügenWegstreckeKupplungen,
        vBoxHinzufügenPlanKupplungen,
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit,
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
        vBoxHinzufügenPlanWegstreckenStreckenabschnitt,
        vBoxHinzufügenPlanWegstreckenWeiche,
        vBoxHinzufügenPlanWegstreckenKupplung,
        progressBarPlan,
        windowMain,
        fortfahrenWennToggledWegstrecke,
        tmvarPlanObjekt}
    -- Knopf-Leiste mit globalen Funktionen
    functionBox <- boxPackWidgetNew vBox PackNatural paddingDefault Last $ hBoxNew False 0
    _buttonHinzufügen <- buttonHinzufügenPack windowMain functionBox tmvarStatus dynamischeWidgets
    boxPack functionBox progressBarPlan PackGrow paddingDefault positionDefault
    buttonSpeichernPack windowMain functionBox tmvarStatus
    buttonLadenPack windowMain functionBox tmvarStatus dynamischeWidgets
    boxPackWidgetNew functionBox packingDefault paddingDefault Last $
        buttonNewWithEventMnemonic Language.beenden $ mainQuit
    -- Lade Datei angegeben in Kommandozeilenargument
    (Options {load=dateipfad}) <- getOptions
    -- neuer Status ist schon in tmvarStatus gespeichert und muss nicht mehr neu gesetzt werden
    let
        ladeAktion :: Status -> IO StatusGUI
        ladeAktion = ladeWidgets tmvarStatus dynamischeWidgets
        fehlerBehandlung :: IOStatusGUI ()
        fehlerBehandlung = State.put statusInitial
    flip State.evalStateT statusInitial $ ausführenBefehl $
                        Laden dateipfad ladeAktion fehlerBehandlung
#endif