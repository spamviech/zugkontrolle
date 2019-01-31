{-# LANGUAGE NamedFieldPuns, LambdaCase, CPP #-}

{-|
Description : Erstelle GUI und starte den GTK-Main-Loop.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.GTK (main) where

import System.Console.ANSI
import qualified Zug.Language as Language
import qualified Zug.UI.Cmd as Cmd

-- | Weiche auf Cmd-UI aus
main :: IO ()
main = setSGR [SetColor Foreground Vivid Red] >> putStrLn Language.uiNichtUnterstützt >> setSGR [Reset] >> Cmd.main
#else
module Zug.UI.GTK (main, setupGUI) where

-- Bibliotheken
import Graphics.UI.Gtk
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar
import Zug.Options
import Zug.Anbindung (pinMapEmpty)
import qualified Zug.Language as Language
import Zug.UI.Base
import qualified Zug.UI.Save as Save
import Zug.UI.GTK.Widgets
import Zug.UI.GTK.Dialog

-- | main loop
main :: IO ()
main = do
    -- Initialisiere GTK+ engine
    initGUI
    -- Erstelle GUI
    setupGUI
    -- GTK+ main loop
    mainGUI

setupGUI :: IO ()
setupGUI = void $ do
    -- Hauptfenster
    windowMain <- widgetShowNew $ widgetNewWithOptionsEvents windowNew [windowTitle := (Language.zugkontrolle :: String), windowDefaultWidth := 640, windowDefaultHeight := 480] [(deleteEvent, Left . liftIO $ mainQuit >> pure False)]
    vBox <- containerAddWidgetNew windowMain $ vBoxNew False 0
    -- Notebook mit aktuellen Elementen
    notebookElemente <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault notebookNew
    (_scrolledWindow, vBoxBahngeschwindigkeiten)    <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.bahngeschwindigkeiten $ vBoxNew False 0
    (_scrolledWindow, vBoxStreckenabschnitte)       <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.streckenabschnitte $ vBoxNew False 0
    (_scrolledWindow, vBoxWeichen)                  <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.weichen $ vBoxNew False 0
    (_scrolledWindow, vBoxKupplungen)               <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.kupplungen $ vBoxNew False 0
    (_scrolledWindow, vBoxWegstrecken)              <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.wegstrecken $ vBoxNew False 0
    (_scrolledWindow, vBoxPläne)                    <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.pläne $ vBoxNew False 0
    progressBarPlan                                     <- widgetShowNew progressBarNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeiten       <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeiten             <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeStreckenabschnitte          <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanStreckenabschnitte                <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeWeichen                     <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenGerade                     <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenKurve                      <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenLinks                      <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenRechts                     <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeKupplungen                  <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanKupplungen                        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit    <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenStreckenabschnitt      <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenWeiche                 <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenKupplung               <- widgetShowNew $ vBoxNew False 0
    mvarPlanObjekt                                      <- newEmptyMVar
    let dynamischeWidgets = DynamischeWidgets {
        vBoxBahngeschwindigkeiten,
        vBoxStreckenabschnitte,
        vBoxWeichen,
        vBoxKupplungen,
        vBoxWegstrecken,
        vBoxPläne,
        vBoxHinzufügenWegstreckeBahngeschwindigkeiten,
        vBoxHinzufügenPlanBahngeschwindigkeiten,
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
        vBoxHinzufügenPlanWegstreckenStreckenabschnitt,
        vBoxHinzufügenPlanWegstreckenWeiche,
        vBoxHinzufügenPlanWegstreckenKupplung,
        progressBarPlan,
        mvarPlanObjekt}
    -- Knopf-Leiste mit globalen Funktionen
    functionBox <- boxPackWidgetNew vBox PackNatural paddingDefault False $ hBoxNew False 0
    (_button, mvarStatus) <- buttonHinzufügenPack windowMain functionBox dynamischeWidgets
    boxPack functionBox progressBarPlan PackGrow paddingDefault positionDefault
    buttonSavePack windowMain functionBox mvarStatus
    buttonLoadPack windowMain functionBox mvarStatus dynamischeWidgets
    boxPackWidgetNew functionBox packingDefault paddingDefault False $ buttonNewWithEventMnemonic Language.beenden $ mainQuit
    emptyStatusNew >>= putLMVar mvarStatus
    -- Lade Datei angegeben in Kommandozeilenargument
    (Options {load=path}) <- getOptions
    -- neuer Status ist schon in mvarStatus gespeichert und muss nicht mehr neu gesetzt werden
    Save.load path (loadWidgets mvarStatus dynamischeWidgets) >>= \case
        (Nothing)           -> pure ()
        (Just konstruktor)  -> newMVar pinMapEmpty >>= \mvarPinMap -> void $ konstruktor mvarPinMap
#endif