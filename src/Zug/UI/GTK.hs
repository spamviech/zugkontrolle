{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-|
Description : Erstelle GUI und starte den GTK-Main-Loop.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.GTK (main) where

import System.Console.ANSI
import qualified Zug.Language as Language
import qualified Zug.UI.Cmd as Cmd

-- | GTK-main loop nicht verfügbar. Weiche auf Cmd-UI aus
main :: IO ()
main = setSGR [SetColor Foreground Vivid Red] >> putStrLn Language.uiNichtUnterstützt >> setSGR [Reset] >> Cmd.main
#else
module Zug.UI.GTK (main, setupGUI) where

-- Bibliotheken
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Graphics.UI.Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar
import Zug.Options
import Zug.Anbindung (pinMapEmpty)
import Zug.Language ((<~>))
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

-- | Erstelle GUI inkl. sämtlicher Events.
-- 
-- Zur Verwendung muss vorher 'initGUI' aufgerufen werden.
setupGUI :: IO ()
setupGUI = void $ do
    -- Hauptfenster
    windowMain <- widgetShowNew windowNew
    set windowMain [windowTitle := (Language.zugkontrolle <~> ZUGKONTROLLEVERSION :: Text), windowDefaultWidth := 640, windowDefaultHeight := 480]
    on windowMain deleteEvent $ liftIO $ mainQuit >> pure False
    vBox <- containerAddWidgetNew windowMain $ vBoxNew False 0
    -- Notebook mit aktuellen Elementen
    notebookElemente <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault notebookNew
    (_scrolledWindow, vBoxBahngeschwindigkeiten)    <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.bahngeschwindigkeiten $ vBoxNew False 0
    (_scrolledWindow, vBoxStreckenabschnitte)       <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.streckenabschnitte $ vBoxNew False 0
    (_scrolledWindow, vBoxWeichen)                  <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.weichen $ vBoxNew False 0
    (_scrolledWindow, vBoxKupplungen)               <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.kupplungen $ vBoxNew False 0
    (_scrolledWindow, vBoxWegstrecken)              <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.wegstrecken $ vBoxNew False 0
    (_scrolledWindow, vBoxPläne)                    <- scrolledWidgedNotebookAppendPageNew notebookElemente Language.pläne $ vBoxNew False 0
    progressBarPlan                                         <- widgetShowNew progressBarNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeiten           <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeiten                 <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeitenLego             <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin          <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeStreckenabschnitte              <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanStreckenabschnitte                    <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeWeichen                         <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenGerade                         <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenKurve                          <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenLinks                          <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWeichenRechts                         <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenWegstreckeKupplungen                      <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanKupplungen                            <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit        <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego    <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenStreckenabschnitt          <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenWeiche                     <- widgetShowNew $ vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenKupplung                   <- widgetShowNew $ vBoxNew False 0
    mvarPlanObjekt                                          <- newEmptyMVar
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
        mvarPlanObjekt}
    -- Knopf-Leiste mit globalen Funktionen
    functionBox <- boxPackWidgetNew vBox PackNatural paddingDefault False $ hBoxNew False 0
    (_button, mvarStatus) <- buttonHinzufügenPack windowMain functionBox dynamischeWidgets
    boxPack functionBox progressBarPlan PackGrow paddingDefault positionDefault
    buttonSpeichernPack windowMain functionBox mvarStatus
    buttonLadenPack windowMain functionBox mvarStatus dynamischeWidgets
    boxPackWidgetNew functionBox packingDefault paddingDefault False $ buttonNewWithEventMnemonic Language.beenden $ mainQuit
    emptyStatusNew >>= putLMVar mvarStatus
    -- Lade Datei angegeben in Kommandozeilenargument
    (Options {load=dateipfad}) <- getOptions
    -- neuer Status ist schon in mvarStatus gespeichert und muss nicht mehr neu gesetzt werden
    Save.load dateipfad (ladeWidgets mvarStatus dynamischeWidgets) >>= \case
        (Nothing)           -> pure ()
        (Just konstruktor)  -> void $ newMVar pinMapEmpty >>= konstruktor
#endif