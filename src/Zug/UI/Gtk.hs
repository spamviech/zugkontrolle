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
import qualified Control.Monad.RWS as RWS
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.Options (Options(..), getOptions)
import Zug.Language ((<~>))
import qualified Zug.Language as Language
import Zug.UI.Base (Status, statusLeer)
import Zug.UI.Befehl (BefehlKlasse(..), BefehlAllgemein(..))
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), StatusGui, MStatusGuiT)
import Zug.UI.Gtk.Fenster (buttonSpeichernPack, buttonLadenPack, ladeWidgets, buttonHinzufügenPack)
import Zug.UI.Gtk.FortfahrenWennToggled (fortfahrenWennToggledTMVarNew)
import Zug.UI.Gtk.Hilfsfunktionen (widgetShowNew, buttonNewWithEventLabel, buttonNewWithEventMnemonic,
                                    containerAddWidgetNew, boxPack, boxPackWidgetNew,
                                    Packing(..), packingDefault, paddingDefault,
                                    Position(..), positionDefault)
import Zug.UI.Gtk.ScrollbaresWidget (scrollbaresWidgetNotebookAppendPageNew)

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
    tmvarStatus <- newTMVarIO statusLeer
    -- Notebook mit aktuellen Elementen
    notebookElemente <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    _notebookMitPanedErsetzen
    (vBoxBahngeschwindigkeiten, _page)
        <- scrollbaresWidgetNotebookAppendPageNew notebookElemente Language.bahngeschwindigkeiten $ Gtk.vBoxNew False 0
    (vBoxStreckenabschnitte, _page)
        <- scrollbaresWidgetNotebookAppendPageNew notebookElemente Language.streckenabschnitte $ Gtk.vBoxNew False 0
    (vBoxWeichen, _page)
        <- scrollbaresWidgetNotebookAppendPageNew notebookElemente Language.weichen $ Gtk.vBoxNew False 0
    (vBoxKupplungen, _page)
        <- scrollbaresWidgetNotebookAppendPageNew notebookElemente Language.kupplungen $ Gtk.vBoxNew False 0
    (vBoxWegstrecken, _page)
        <- scrollbaresWidgetNotebookAppendPageNew notebookElemente Language.wegstrecken $ Gtk.vBoxNew False 0
    (vBoxPläne, _page)
        <- scrollbaresWidgetNotebookAppendPageNew notebookElemente Language.pläne $ Gtk.vBoxNew False 0
    progressBarPlan
        <- widgetShowNew Gtk.progressBarNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeiten
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeiten
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeitenLego
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenWegstreckeStreckenabschnitte
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanStreckenabschnitte
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenWegstreckeWeichen
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWeichenGerade
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWeichenKurve
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWeichenLinks
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWeichenRechts
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenWegstreckeKupplungen
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanKupplungen
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenStreckenabschnitt
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenWeiche
        <- widgetShowNew $ Gtk.vBoxNew False 0
    vBoxHinzufügenPlanWegstreckenKupplung
        <- widgetShowNew $ Gtk.vBoxNew False 0
    buttonHinzufügenWegstrecke
        <- Gtk.buttonNewWithLabel (Language.hinzufügen :: Text)
    fortfahrenWennToggledWegstrecke <- fortfahrenWennToggledTMVarNew Language.hinzufügen _fold tmvarStatus
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
    -- Knopf-Leiste mit globalen Funktionen
    functionBox <- boxPackWidgetNew vBox PackNatural paddingDefault End $ Gtk.hBoxNew False 0
    _buttonHinzufügen <- buttonHinzufügenPack windowMain functionBox
    boxPack functionBox progressBarPlan PackGrow paddingDefault positionDefault
    buttonSpeichernPack windowMain functionBox
    buttonLadenPack windowMain functionBox
    boxPackWidgetNew functionBox packingDefault paddingDefault End $
        buttonNewWithEventMnemonic Language.beenden $ Gtk.mainQuit
    -- Lade Datei angegeben in Kommandozeilenargument
    (Options {load=dateipfad}) <- getOptions
    -- neuer Status ist schon in tmvarStatus gespeichert und muss nicht mehr neu gesetzt werden
    let
        ladeAktion :: Status -> IO StatusGui
        ladeAktion = ladeWidgets
        fehlerBehandlung :: MStatusGuiT IO ()
        fehlerBehandlung = RWS.put statusLeer
    flip RWS.evalRWST statusLeer _ $ ausführenBefehl $
        Laden dateipfad ladeAktion fehlerBehandlung
#endif