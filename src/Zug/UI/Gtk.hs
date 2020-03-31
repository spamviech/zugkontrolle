{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
#endif

{-|
Description : Erstelle GUI und starte den GTK-Main-Loop.
-}
module Zug.UI.Gtk (main, setupGUI) where

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

#ifndef ZUGKONTROLLEGUI
import Zug.Language (Sprache(..))
#else
import Zug.Language (Sprache(), MitSprache(leseSprache), (<~>), (<|>))
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
       (widgetShowNew, widgetShowIf, buttonNewWithEventLabel, containerAddWidgetNew
      , boxPackWidgetNew, boxPackWidgetNewDefault, Packing(..), packingDefault, paddingDefault
      , Position(..), positionDefault, notebookAppendPageNew, labelSpracheNew
      , toggleButtonNewWithEvent)
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget))
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
    -- Dummy-Fenster, damit etwas angezeigt wird
    windowDummy <- widgetShowNew $ Gtk.windowNew
    Gtk.set
        windowDummy
        [ Gtk.windowTitle := leseSprache (Language.zugkontrolle <~> Language.version) spracheGui
        , Gtk.windowDeletable := False]
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
    -- Einzelseite-Variante
    notebookElementeEinzelseiten
        <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    Gtk.widgetHide notebookElementeEinzelseiten
    (vBoxBG, vBoxST, vBoxWE, vBoxKU, vBoxWS, vBoxPL) <- flip runReaderT spracheGui $ do
        (vBoxBahngeschwindigkeitenEinzel, _page) <- notebookAppendPageNew
            notebookElementeEinzelseiten
            maybeTVar
            Language.bahngeschwindigkeiten
            $ liftIO
            $ Gtk.vBoxNew False 0
        (vBoxStreckenabschnitteEinzel, _page) <- notebookAppendPageNew
            notebookElementeEinzelseiten
            maybeTVar
            Language.streckenabschnitte
            $ liftIO
            $ Gtk.vBoxNew False 0
        (vBoxWeichenEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.weichen
            $ liftIO
            $ Gtk.vBoxNew False 0
        (vBoxKupplungenEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.kupplungen
            $ liftIO
            $ Gtk.vBoxNew False 0
        (vBoxWegstreckenEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.wegstrecken
            $ liftIO
            $ Gtk.vBoxNew False 0
        (vBoxPläneEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.pläne
            $ liftIO
            $ Gtk.vBoxNew False 0
        pure
            ( vBoxBahngeschwindigkeitenEinzel
            , vBoxStreckenabschnitteEinzel
            , vBoxWeichenEinzel
            , vBoxKupplungenEinzel
            , vBoxWegstreckenEinzel
            , vBoxPläneEinzel
            )
    -- Paned-Variante
    notebookElementePaned
        <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    (panedEinzelObjekte, _page) <- flip runReaderT spracheGui
        $ notebookAppendPageNew
            notebookElementePaned
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
    vBoxLeftTop <- containerAddWidgetNew frameLeftTop $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxLeftTop
        $ labelSpracheNew maybeTVar Language.bahngeschwindigkeiten
    vBoxBahngeschwindigkeiten
        <- boxPackWidgetNew vBoxLeftTop PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.vBoxNew False 0
    frameLeftBot <- widgetShowNew Gtk.frameNew
    Gtk.set frameLeftBot [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 vPanedLeft frameLeftBot
    vBoxLeftBot <- containerAddWidgetNew frameLeftBot $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxLeftBot
        $ labelSpracheNew maybeTVar Language.streckenabschnitte
    vBoxStreckenabschnitte <- boxPackWidgetNew vBoxLeftBot PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.vBoxNew False 0
    vPanedRight <- widgetShowNew Gtk.vPanedNew
    Gtk.panedAdd2 panedEinzelObjekte vPanedRight
    frameRightTop <- widgetShowNew Gtk.frameNew
    Gtk.set frameRightTop [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 vPanedRight frameRightTop
    vBoxRightTop <- containerAddWidgetNew frameRightTop $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxRightTop
        $ labelSpracheNew maybeTVar Language.weichen
    vBoxWeichen <- boxPackWidgetNew vBoxRightTop PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.vBoxNew False 0
    frameRightBot <- widgetShowNew Gtk.frameNew
    Gtk.set frameRightBot [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 vPanedRight frameRightBot
    vBoxRightBot <- containerAddWidgetNew frameRightBot $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxRightBot
        $ labelSpracheNew maybeTVar Language.kupplungen
    vBoxKupplungen <- boxPackWidgetNew vBoxRightBot PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.vBoxNew False 0
    (panedSammelObjekte, _page) <- flip runReaderT spracheGui
        $ notebookAppendPageNew
            notebookElementePaned
            maybeTVar
            (Language.wegstrecken <|> Language.pläne)
        $ liftIO Gtk.hPanedNew
    frameWegstrecken <- widgetShowNew Gtk.frameNew
    Gtk.set frameWegstrecken [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd1 panedSammelObjekte frameWegstrecken
    vBoxWegstreckenOuter <- containerAddWidgetNew frameWegstrecken $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxWegstreckenOuter
        $ labelSpracheNew maybeTVar Language.wegstrecken
    vBoxWegstrecken
        <- boxPackWidgetNew vBoxWegstreckenOuter PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.vBoxNew False 0
    framePläne <- widgetShowNew Gtk.frameNew
    Gtk.set framePläne [Gtk.frameShadowType := Gtk.ShadowIn]
    Gtk.panedAdd2 panedSammelObjekte framePläne
    vBoxPläneOuter <- containerAddWidgetNew framePläne $ Gtk.vBoxNew False 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPläneOuter
        $ labelSpracheNew maybeTVar Language.pläne
    vBoxPläne <- boxPackWidgetNew vBoxPläneOuter PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.vBoxNew False 0
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
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanBahngeschwindigkeitenLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
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
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
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
            , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
            , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
            , vBoxHinzufügenPlanBahngeschwindigkeitenLego
            , vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
            , vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
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
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
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
        -- Linke Seite
        buttonHinzufügenPack windowMain functionBox maybeTVar
        spracheAuswahl <- boxPackWidgetNewDefault functionBox
            $ boundedEnumAuswahlComboBoxNew Language.Deutsch maybeTVar Language.sprache
        beiAuswahl spracheAuswahl $ \sprache -> void $ do
            spracheGuiNeu <- sprachwechsel spracheGui sprache
            flip runReaderT objektReader
                $ ausführenStatusVarBefehl (SprachWechsel spracheGuiNeu) statusVar
        -- Rechte seite
        boxPackWidgetNew functionBox packingDefault paddingDefault End
            $ buttonNewWithEventLabel maybeTVar Language.beenden
            $ Gtk.mainQuit
        buttonLadenPack windowMain functionBox maybeTVar End
        buttonSpeichernPack windowMain functionBox maybeTVar End
    checkButtonNotebook <- boxPackWidgetNewDefault functionBox
        $ toggleButtonNewWithEvent Gtk.checkButtonNew
        $ \toggled -> do
            widgetShowIf toggled notebookElementeEinzelseiten
            widgetShowIf (not toggled) notebookElementePaned
            case toggled of
                True -> do
                    Gtk.widgetReparent (erhalteWidget vBoxBahngeschwindigkeiten) vBoxBG
                    Gtk.widgetReparent (erhalteWidget vBoxStreckenabschnitte) vBoxST
                    Gtk.widgetReparent (erhalteWidget vBoxWeichen) vBoxWE
                    Gtk.widgetReparent (erhalteWidget vBoxKupplungen) vBoxKU
                    Gtk.widgetReparent (erhalteWidget vBoxWegstrecken) vBoxWS
                    Gtk.widgetReparent (erhalteWidget vBoxPläne) vBoxPL
                False -> do
                    Gtk.widgetReparent (erhalteWidget vBoxBahngeschwindigkeiten) vBoxLeftTop
                    Gtk.widgetReparent (erhalteWidget vBoxStreckenabschnitte) vBoxLeftBot
                    Gtk.widgetReparent (erhalteWidget vBoxWeichen) vBoxRightTop
                    Gtk.widgetReparent (erhalteWidget vBoxKupplungen) vBoxRightBot
                    Gtk.widgetReparent (erhalteWidget vBoxWegstrecken) vBoxWegstreckenOuter
                    Gtk.widgetReparent (erhalteWidget vBoxPläne) vBoxPläneOuter
    verwendeSpracheGuiFn spracheGui maybeTVar $ \sprache
        -> Gtk.set checkButtonNotebook [Gtk.buttonLabel := Language.einzelseiten sprache]
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
    -- Dummy-Fenster löschen
    Gtk.widgetDestroy windowDummy
#endif
--
