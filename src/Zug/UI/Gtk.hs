{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
#endif

{-|
Description : Erstelle GUI und starte den GTK-Main-Loop.
-}
module Zug.UI.Gtk (main, setupGUI) where

-- Bibliotheken
#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, TVar)
#else
import Control.Concurrent.STM.TVar (TVar)
#endif
#ifdef ZUGKONTROLLEGUI
import Control.Monad (void, forM_)
import qualified Control.Monad.RWS as RWS
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)
#else
import Data.Text (Text)
import qualified Data.Text.IO as Text
#endif
#ifdef ZUGKONTROLLEGUI
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
#else
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))
#endif

-- Abhängigkeiten von anderen Modulen
#ifndef ZUGKONTROLLEGUI
import Zug.Language (Sprache(..))
#else
import Zug.Language (Sprache(), (<~>), (<|>))
#endif
import qualified Zug.Language as Language
#ifdef ZUGKONTROLLEGUI
import Zug.Options (Options(..), getOptions)
import Zug.UI.Base (Status, statusLeer, tvarMapsNeu)
import Zug.UI.Befehl (BefehlAllgemein(..))
#endif
#ifndef ZUGKONTROLLEGUI
import qualified Zug.UI.Cmd as Cmd
#else
import Zug.UI.Gtk.Auswahl (boundedEnumAuswahlComboBoxNew, beiAuswahl)
import Zug.UI.Gtk.Fenster (buttonSpeichernPack, buttonLadenPack, ladeWidgets, buttonHinzufügenPack)
import Zug.UI.Gtk.FortfahrenWennToggled (fortfahrenWennToggledVarNew)
import Zug.UI.Gtk.Hilfsfunktionen
       (widgetShowNew, buttonNewWithEventLabel, containerAddWidgetNew, boxPackWidgetNew
      , boxPackWidgetNewDefault, Packing(..), packingDefault, paddingDefault, Position(..)
      , positionDefault, notebookAppendPageNew, labelSpracheNew)
import Zug.UI.Gtk.ScrollbaresWidget (scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (spracheGuiNeu, verwendeSpracheGuiFn, sprachwechsel)
import Zug.UI.Gtk.StreckenObjekt
       (DynamischeWidgets(..), boxWegstreckeHinzufügenNew, boxPlanHinzufügenNew, MStatusGuiT
      , IOStatusGui, foldWegstreckeHinzufügen)
import Zug.UI.StatusVar (statusVarNew, ausführenStatusVarBefehl, readStatusVar)
#endif

#ifndef ZUGKONTROLLEGUI
-- | GTK-main loop nicht verfügbar. Weiche auf Cmd-UI aus
main :: IO ()
main = do
    putWarningLn Language.uiNichtUnterstützt
    Cmd.main

setupGUI :: Maybe (TVar (Maybe [Sprache -> IO ()])) -> IO ()
setupGUI _maybeTVar = putWarningLn Language.uiNichtUnterstützt

putWarningLn :: (Sprache -> Text) -> IO ()
putWarningLn warning = do
    setSGR [SetColor Foreground Vivid Red]
    Text.putStrLn $ warning Deutsch
    setSGR [Reset]

#else
-- | main loop
main :: IO ()
main = do
    -- Initialisiere GTK+ engine
    Gtk.initGUI
    -- Erstelle GUI
    setupGUI Nothing
    -- GTK+ main loop
    Gtk.mainGUI

-- | Erstelle GUI inkl. sämtlicher Events.
--
-- Zur Verwendung muss vorher 'Gtk.initGUI' aufgerufen werden.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
setupGUI :: Maybe (TVar (Maybe [Sprache -> IO ()])) -> IO ()
setupGUI maybeTVar = void $ do
    Options {load = dateipfad, sprache} <- getOptions
    spracheGui <- spracheGuiNeu sprache
    -- Hauptfenster
    windowMain <- Gtk.windowNew
    -- native Auflösung des Raspi 7'' TouchScreen ist 800x480
    -- leicht kleinere Werte um Menüleisten zu berücksichtigen
    Gtk.set windowMain [Gtk.windowDefaultWidth := 720, Gtk.windowDefaultHeight := 450]
    Gtk.windowMaximize windowMain
    -- Titel
    verwendeSpracheGuiFn spracheGui maybeTVar $ \sprache -> Gtk.set
        windowMain
        [Gtk.windowTitle := (Language.zugkontrolle <~> Language.version) sprache]
    -- Drücken des X-Knopfes beendet das gesamte Program
    Gtk.on windowMain Gtk.deleteEvent $ liftIO $ do
        Gtk.mainQuit
        pure False
    vBox <- containerAddWidgetNew windowMain $ Gtk.vBoxNew False 0
    tvarMaps <- tvarMapsNeu
    statusVar <- statusVarNew $ statusLeer spracheGui
    -- Notebook mit aktuellen Elementen
    notebookElemente
        <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    (panedEinzelObjekte, _page) <- flip runReaderT spracheGui
        $ notebookAppendPageNew
            notebookElemente
            maybeTVar
            (Language.bahngeschwindigkeiten
             <|> Language.streckenabschnitte
             <|> Language.weichen
             <|> Language.kupplungen)
        $ liftIO Gtk.hPanedNew
    vPanedLeft <- widgetShowNew Gtk.vPanedNew
    Gtk.panedAdd1 panedEinzelObjekte vPanedLeft
    frameLeftTop <- widgetShowNew Gtk.frameNew
    Gtk.set frameLeftTop [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 vPanedLeft frameLeftTop
    vBoxBahngeschwindigkeiten
        <- containerAddWidgetNew frameLeftTop $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxBahngeschwindigkeiten
        $ labelSpracheNew maybeTVar Language.bahngeschwindigkeiten
    frameLeftBot <- widgetShowNew Gtk.frameNew
    Gtk.set frameLeftBot [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 vPanedLeft frameLeftBot
    vBoxStreckenabschnitte
        <- containerAddWidgetNew frameLeftBot $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxStreckenabschnitte
        $ labelSpracheNew maybeTVar Language.streckenabschnitte
    vPanedRight <- widgetShowNew Gtk.vPanedNew
    Gtk.panedAdd2 panedEinzelObjekte vPanedRight
    frameRightTop <- widgetShowNew Gtk.frameNew
    Gtk.set frameRightTop [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 vPanedRight frameRightTop
    vBoxWeichen <- containerAddWidgetNew frameRightTop $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxWeichen
        $ labelSpracheNew maybeTVar Language.weichen
    frameRightBot <- widgetShowNew Gtk.frameNew
    Gtk.set frameRightBot [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 vPanedRight frameRightBot
    vBoxKupplungen
        <- containerAddWidgetNew frameRightBot $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxKupplungen
        $ labelSpracheNew maybeTVar Language.kupplungen
    (panedSammelObjekte, _page) <- flip runReaderT spracheGui
        $ notebookAppendPageNew
            notebookElemente
            maybeTVar
            (Language.wegstrecken <|> Language.pläne)
        $ liftIO Gtk.hPanedNew
    frameWegstrecken <- widgetShowNew Gtk.frameNew
    Gtk.set frameWegstrecken [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 panedSammelObjekte frameWegstrecken
    vBoxWegstrecken
        <- containerAddWidgetNew frameWegstrecken $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxWegstrecken
        $ labelSpracheNew maybeTVar Language.wegstrecken
    framePläne <- widgetShowNew Gtk.frameNew
    Gtk.set framePläne [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 panedSammelObjekte framePläne
    vBoxPläne <- containerAddWidgetNew framePläne $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPläne
        $ labelSpracheNew maybeTVar Language.pläne
    -- Paned mittig setzten
    Gtk.screenGetDefault >>= \case
        (Just screen) -> do
            screenWidth <- Gtk.screenGetWidth screen
            forM_ [panedEinzelObjekte, panedSammelObjekte] $ \paned -> do
                Gtk.set paned [Gtk.panedPosition := div screenWidth 2]
            screenHeight <- Gtk.screenGetHeight screen
            -- estimated Value
            let decoratorHeight = 50
            forM_ [vPanedLeft, vPanedRight] $ \paned -> do
                Gtk.set paned [Gtk.panedPosition := div (screenHeight - decoratorHeight) 2]
        Nothing -> pure ()
    -- DynamischeWidgets
    vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanBahngeschwindigkeitenLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenWegstreckeStreckenabschnitte
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanStreckenabschnitte
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenWegstreckeWeichenMärklin
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenWegstreckeWeichenLego
        <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanWeichenGeradeMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWeichenGeradeLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWeichenKurveMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWeichenKurveLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWeichenLinksMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWeichenLinksLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWeichenRechtsMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWeichenRechtsLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenWegstreckeKupplungen <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanKupplungen <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenKupplungMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenKupplungLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanPläne <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    fortfahrenWennToggledWegstrecke <- flip runReaderT spracheGui
        $ fortfahrenWennToggledVarNew
            maybeTVar
            Language.hinzufügen
            foldWegstreckeHinzufügen
            (atomically . readStatusVar)
            statusVar
    tmvarPlanObjekt <- newEmptyTMVarIO
    let dynamischeWidgets =
            DynamischeWidgets
            { vBoxBahngeschwindigkeiten
            , vBoxStreckenabschnitte
            , vBoxWeichen
            , vBoxKupplungen
            , vBoxWegstrecken
            , vBoxPläne
            , vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
            , vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
            , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            , vBoxHinzufügenPlanBahngeschwindigkeitenLego
            , vBoxHinzufügenWegstreckeStreckenabschnitte
            , vBoxHinzufügenPlanStreckenabschnitte
            , vBoxHinzufügenWegstreckeWeichenMärklin
            , vBoxHinzufügenWegstreckeWeichenLego
            , vBoxHinzufügenPlanWeichenGeradeMärklin
            , vBoxHinzufügenPlanWeichenGeradeLego
            , vBoxHinzufügenPlanWeichenKurveMärklin
            , vBoxHinzufügenPlanWeichenKurveLego
            , vBoxHinzufügenPlanWeichenLinksMärklin
            , vBoxHinzufügenPlanWeichenLinksLego
            , vBoxHinzufügenPlanWeichenRechtsMärklin
            , vBoxHinzufügenPlanWeichenRechtsLego
            , vBoxHinzufügenWegstreckeKupplungen
            , vBoxHinzufügenPlanKupplungen
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            , vBoxHinzufügenPlanWegstreckenKupplungMärklin
            , vBoxHinzufügenPlanWegstreckenKupplungLego
            , vBoxHinzufügenPlanWegstreckenMärklin
            , vBoxHinzufügenPlanWegstreckenLego
            , vBoxHinzufügenPlanPläne
            , windowMain
            , fortfahrenWennToggledWegstrecke
            , tmvarPlanObjekt
            }
    let objektReader = (tvarMaps, dynamischeWidgets, statusVar)
    -- Knopf-Leiste mit globalen Funktionen
    functionBox <- boxPackWidgetNew vBox PackNatural paddingDefault End $ Gtk.hBoxNew False 0
    flip runReaderT objektReader $ do
        buttonHinzufügenPack windowMain functionBox maybeTVar
        spracheAuswahl <- boxPackWidgetNewDefault functionBox
            $ boundedEnumAuswahlComboBoxNew Language.Deutsch maybeTVar Language.sprache
        beiAuswahl spracheAuswahl $ \sprache -> void $ do
            spracheGuiNeu <- sprachwechsel spracheGui sprache
            flip runReaderT objektReader
                $ ausführenStatusVarBefehl (SprachWechsel spracheGuiNeu) statusVar
        buttonSpeichernPack windowMain functionBox maybeTVar
        buttonLadenPack windowMain functionBox maybeTVar
        boxPackWidgetNew functionBox packingDefault paddingDefault End
            $ buttonNewWithEventLabel maybeTVar Language.beenden
            $ Gtk.mainQuit
    -- Lade Datei angegeben in Kommandozeilenargument
    let ladeAktion :: Status -> IOStatusGui ()
        ladeAktion statusNeu = do
            state0 <- RWS.get
            state1 <- liftIO
                $ flip runReaderT objektReader
                $ fst <$> RWS.execRWST (ladeWidgets statusNeu) objektReader state0
            RWS.put state1
        fehlerBehandlung :: MStatusGuiT IO ()
        fehlerBehandlung = RWS.put $ statusLeer spracheGui
    flip runReaderT objektReader
        $ ausführenStatusVarBefehl (Laden dateipfad ladeAktion fehlerBehandlung) statusVar
    -- Fenster wird erst hier angezeigt, weil sonst windowDefaultWidth/Height keinen Effekt zeigen
    Gtk.widgetShow windowMain
#endif
--
