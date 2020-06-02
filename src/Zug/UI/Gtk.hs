{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
#endif

{-|
Description : Erstelle GUI und starte den GTK-Main-Loop.
-}
module Zug.UI.Gtk (main, setupGUI) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent (runInBoundThread)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO)
#else
import Control.Concurrent.STM.TVar (TVar)
#endif
#ifdef ZUGKONTROLLEGUI
import Control.Monad (void, when, forM_)
import qualified Control.Monad.RWS.Strict as RWS
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.GI.Gtk.Threading as Gtk
#else
import Data.Text (Text)
import qualified Data.Text.IO as Text
#endif
#ifdef ZUGKONTROLLEGUI
import qualified GI.Gdk as Gdk
import GI.Gtk (AttrOp(..))
import qualified GI.Gtk as Gtk
#else
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))
#endif

#ifndef ZUGKONTROLLEGUI
import Zug.Language (Sprache(..))
#else
import Zug.Language (MitSprache(leseSprache), (<~>), (<|>))
#endif
import qualified Zug.Language as Language
#ifdef ZUGKONTROLLEGUI
import Zug.Options (Options(..), getOptions, GtkSeiten(Einzelseiten))
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
      , boxPackWidgetNew, boxPackWidgetNewDefault, boxPack, Packing(..), packingDefault
      , paddingDefault, Position(..), positionDefault, notebookAppendPageNew, labelSpracheNew
      , toggleButtonNewWithEvent)
import Zug.UI.Gtk.Klassen (mitContainerRemove)
import Zug.UI.Gtk.ScrollbaresWidget (scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui
       (spracheGuiNeu, verwendeSpracheGuiFn, sprachwechsel, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt
       (DynamischeWidgets(..), boxWegstreckeHinzufügenNew, boxPlanHinzufügenNew, MStatusGuiT
      , IOStatusGui, foldWegstreckeHinzufügen, BGWidgetsBoxen(..), STWidgetsBoxen(..)
      , WEWidgetsBoxen(..), KUWidgetsBoxen(..), KOWidgetsBoxen(..), WSWidgetsBoxen(..)
      , PLWidgetsBoxen(..))
import Zug.UI.StatusVar (statusVarNew, ausführenStatusVarBefehl, readStatusVar)
#endif

#ifndef ZUGKONTROLLEGUI
-- | Gtk-main loop nicht verfügbar. Weiche auf Cmd-UI aus.
main :: IO ()
main = do
    putWarningLn Language.uiNichtUnterstützt
    Cmd.main

setupGUI :: Maybe TVarSprachewechselAktionen -> IO ()
setupGUI _maybeTVar = putWarningLn Language.uiNichtUnterstützt

putWarningLn :: (Sprache -> Text) -> IO ()
putWarningLn warning = do
    setSGR [SetColor Foreground Vivid Red]
    Text.putStrLn $ warning Deutsch
    setSGR [Reset]

#else
-- | Gtk-main loop.
main :: IO ()
main = runInBoundThread $ do
    -- Initialisiere GTK+ engine
    Gtk.init Nothing
    Gtk.setCurrentThreadAsGUIThread
    -- Erstelle GUI
    setupGUI Nothing
    -- GTK+ main loop
    Gtk.main

-- | Erstelle GUI inkl. sämtlicher Events.
--
-- Zur Verwendung muss vorher 'Gtk.initGUI' aufgerufen werden.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
setupGUI :: Maybe TVarSprachewechselAktionen -> IO ()
setupGUI maybeTVar = void $ mdo
    Options {load = dateipfad, gtkSeiten, sprache} <- getOptions
    spracheGui <- spracheGuiNeu sprache
    -- Dummy-Fenster, damit etwas angezeigt wird
    windowDummy <- Gtk.windowNew Gtk.WindowTypeToplevel
    flip leseSprache spracheGui $ \sp -> Gtk.set
        windowDummy
        [ Gtk.windowTitle := (Language.zugkontrolle <~> Language.version) sp
        , Gtk.windowDeletable := False
        , Gtk.windowDefaultWidth := 400]
    Gtk.widgetShow windowDummy
    -- Hauptfenster
    dynWindowMain <- Gtk.windowNew Gtk.WindowTypeToplevel
    -- native Auflösung des Raspi 7'' TouchScreen ist 800x480
    -- leicht kleinere Werte um Menüleisten zu berücksichtigen
    Gtk.set dynWindowMain [Gtk.windowDefaultWidth := 720, Gtk.windowDefaultHeight := 450]
    Gtk.windowMaximize dynWindowMain
    -- Titel
    verwendeSpracheGuiFn spracheGui maybeTVar $ \sp -> Gtk.set
        dynWindowMain
        [Gtk.windowTitle := (Language.zugkontrolle <~> Language.version) sp]
    -- Drücken des X-Knopfes beendet das gesamte Program
    Gtk.onWidgetDeleteEvent dynWindowMain $ \_event -> liftIO $ do
        Gtk.mainQuit
        pure False
    vBox <- containerAddWidgetNew dynWindowMain $ Gtk.boxNew Gtk.OrientationVertical 0
    tvarMaps <- tvarMapsNeu
    statusVar <- statusVarNew $ statusLeer spracheGui
    -- Notebook mit aktuellen Elementen
    -- Einzelseite-Variante
    notebookElementeEinzelseiten
        <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    Gtk.widgetHide notebookElementeEinzelseiten
    (vBoxBG, vBoxST, vBoxWE, vBoxKU, vBoxKO, vBoxWS, vBoxPL) <- flip runReaderT spracheGui $ do
        (vBoxBahngeschwindigkeitenEinzel, _page) <- notebookAppendPageNew
            notebookElementeEinzelseiten
            maybeTVar
            Language.bahngeschwindigkeiten
            $ liftIO
            $ Gtk.boxNew Gtk.OrientationVertical 0
        (vBoxStreckenabschnitteEinzel, _page) <- notebookAppendPageNew
            notebookElementeEinzelseiten
            maybeTVar
            Language.streckenabschnitte
            $ liftIO
            $ Gtk.boxNew Gtk.OrientationVertical 0
        (vBoxWeichenEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.weichen
            $ liftIO
            $ Gtk.boxNew Gtk.OrientationVertical 0
        (vBoxKupplungenEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.kupplungen
            $ liftIO
            $ Gtk.boxNew Gtk.OrientationVertical 0
        (vBoxKontakteEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.kontakte
            $ liftIO
            $ Gtk.boxNew Gtk.OrientationVertical 0
        (vBoxWegstreckenEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.wegstrecken
            $ liftIO
            $ Gtk.boxNew Gtk.OrientationVertical 0
        (vBoxPläneEinzel, _page)
            <- notebookAppendPageNew notebookElementeEinzelseiten maybeTVar Language.pläne
            $ liftIO
            $ Gtk.boxNew Gtk.OrientationVertical 0
        pure
            ( vBoxBahngeschwindigkeitenEinzel
            , vBoxStreckenabschnitteEinzel
            , vBoxWeichenEinzel
            , vBoxKupplungenEinzel
            , vBoxKontakteEinzel
            , vBoxWegstreckenEinzel
            , vBoxPläneEinzel
            )
    -- Paned-Variante
    notebookElementePaned
        <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    (panedBahngeschwindigkeitStreckenabschnittWeiche, _pageBgStWe) <- flip runReaderT spracheGui
        $ notebookAppendPageNew
            notebookElementePaned
            maybeTVar
            (Language.bahngeschwindigkeiten <|> Language.streckenabschnitte <|> Language.weichen)
        $ liftIO
        $ Gtk.panedNew Gtk.OrientationHorizontal
    vPanedBahngeschwindigkeitStreckenabschnitt
        <- widgetShowNew $ Gtk.panedNew Gtk.OrientationVertical
    Gtk.panedAdd1
        panedBahngeschwindigkeitStreckenabschnittWeiche
        vPanedBahngeschwindigkeitStreckenabschnitt
    frameBahngeschwindigkeiten <- widgetShowNew $ Gtk.frameNew Nothing
    Gtk.set frameBahngeschwindigkeiten [Gtk.frameShadowType := Gtk.ShadowTypeIn]
    Gtk.panedAdd1 vPanedBahngeschwindigkeitStreckenabschnitt frameBahngeschwindigkeiten
    vBoxPanedBahngeschwindigkeiten
        <- containerAddWidgetNew frameBahngeschwindigkeiten $ Gtk.boxNew Gtk.OrientationVertical 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPanedBahngeschwindigkeiten
        $ labelSpracheNew maybeTVar Language.bahngeschwindigkeiten
    vBoxBahngeschwindigkeiten
        <- boxPackWidgetNew vBoxPanedBahngeschwindigkeiten PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.boxNew Gtk.OrientationVertical 0
    frameStreckenabschnitte <- widgetShowNew $ Gtk.frameNew Nothing
    Gtk.set frameStreckenabschnitte [Gtk.frameShadowType := Gtk.ShadowTypeIn]
    Gtk.panedAdd2 vPanedBahngeschwindigkeitStreckenabschnitt frameStreckenabschnitte
    vBoxPanedStreckenabschnitte
        <- containerAddWidgetNew frameStreckenabschnitte $ Gtk.boxNew Gtk.OrientationVertical 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPanedStreckenabschnitte
        $ labelSpracheNew maybeTVar Language.streckenabschnitte
    vBoxStreckenabschnitte
        <- boxPackWidgetNew vBoxPanedStreckenabschnitte PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.boxNew Gtk.OrientationVertical 0
    frameWeichen <- widgetShowNew $ Gtk.frameNew Nothing
    Gtk.set frameWeichen [Gtk.frameShadowType := Gtk.ShadowTypeIn]
    Gtk.panedAdd2 panedBahngeschwindigkeitStreckenabschnittWeiche frameWeichen
    vBoxPanedWeichen <- containerAddWidgetNew frameWeichen $ Gtk.boxNew Gtk.OrientationVertical 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPanedWeichen
        $ labelSpracheNew maybeTVar Language.weichen
    vBoxWeichen <- boxPackWidgetNew vBoxPanedWeichen PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.boxNew Gtk.OrientationVertical 0
    (panedKupplungKontakt, _pageKuKo) <- flip runReaderT spracheGui
        $ notebookAppendPageNew
            notebookElementePaned
            maybeTVar
            (Language.kupplungen <|> Language.kontakte)
        $ liftIO
        $ Gtk.panedNew Gtk.OrientationHorizontal
    frameKupplungen <- widgetShowNew $ Gtk.frameNew Nothing
    Gtk.set frameKupplungen [Gtk.frameShadowType := Gtk.ShadowTypeIn]
    Gtk.panedAdd1 panedKupplungKontakt frameKupplungen
    vBoxPanedKupplungen
        <- containerAddWidgetNew frameKupplungen $ Gtk.boxNew Gtk.OrientationVertical 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPanedKupplungen
        $ labelSpracheNew maybeTVar Language.kupplungen
    vBoxKupplungen <- boxPackWidgetNew vBoxPanedKupplungen PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.boxNew Gtk.OrientationVertical 0
    frameKontakte <- widgetShowNew $ Gtk.frameNew Nothing
    Gtk.set frameKontakte [Gtk.frameShadowType := Gtk.ShadowTypeIn]
    Gtk.panedAdd2 panedKupplungKontakt frameKontakte
    vBoxPanedKontakte <- containerAddWidgetNew frameKontakte $ Gtk.boxNew Gtk.OrientationVertical 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPanedKontakte
        $ labelSpracheNew maybeTVar Language.kontakte
    vBoxKontakte <- boxPackWidgetNew vBoxPanedKontakte PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.boxNew Gtk.OrientationVertical 0
    (panedWegstreckePlan, _pageWsPl) <- flip runReaderT spracheGui
        $ notebookAppendPageNew
            notebookElementePaned
            maybeTVar
            (Language.wegstrecken <|> Language.pläne)
        $ liftIO
        $ Gtk.panedNew Gtk.OrientationHorizontal
    frameWegstrecken <- widgetShowNew $ Gtk.frameNew Nothing
    Gtk.set frameWegstrecken [Gtk.frameShadowType := Gtk.ShadowTypeIn]
    Gtk.panedAdd1 panedWegstreckePlan frameWegstrecken
    vBoxPanedWegstrecken
        <- containerAddWidgetNew frameWegstrecken $ Gtk.boxNew Gtk.OrientationVertical 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPanedWegstrecken
        $ labelSpracheNew maybeTVar Language.wegstrecken
    vBoxWegstrecken
        <- boxPackWidgetNew vBoxPanedWegstrecken PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.boxNew Gtk.OrientationVertical 0
    framePläne <- widgetShowNew $ Gtk.frameNew Nothing
    Gtk.set framePläne [Gtk.frameShadowType := Gtk.ShadowTypeIn]
    Gtk.panedAdd2 panedWegstreckePlan framePläne
    vBoxPanedPläne <- containerAddWidgetNew framePläne $ Gtk.boxNew Gtk.OrientationVertical 0
    flip runReaderT spracheGui
        $ boxPackWidgetNewDefault vBoxPanedPläne
        $ labelSpracheNew maybeTVar Language.pläne
    vBoxPläne <- boxPackWidgetNew vBoxPanedPläne PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.boxNew Gtk.OrientationVertical 0
    -- Paned mittig setzten
    Gdk.displayGetDefault >>= \case
        (Just display) -> Gtk.getWidgetWindow dynWindowMain >>= \case
            (Just gdkWindow) -> do
                monitor <- Gdk.displayGetMonitorAtWindow display gdkWindow
                geometry <- Gdk.monitorGetGeometry monitor
                screenWidth <- Gdk.getRectangleWidth geometry
                forM_
                    [ panedBahngeschwindigkeitStreckenabschnittWeiche
                    , panedKupplungKontakt
                    , panedWegstreckePlan]
                    $ \paned -> Gdk.set paned [Gtk.panedPosition := div screenWidth 2]
                screenHeight <- Gdk.getRectangleHeight geometry
                -- geschätzter Wert
                let decoratorHeight = 50
                Gtk.set
                    vPanedBahngeschwindigkeitStreckenabschnitt
                    [Gtk.panedPosition := div (screenHeight - decoratorHeight) 3]
            Nothing -> pure ()
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
    vBoxHinzufügenWegstreckeKontakte <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
    vBoxHinzufügenPlanKontakte <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
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
    vBoxHinzufügenPlanWegstreckenKontaktMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenKontaktLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenMärklin
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanWegstreckenLego
        <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    vBoxHinzufügenPlanPläne <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
    dynFortfahrenWennToggledWegstrecke <- flip runReaderT spracheGui
        $ fortfahrenWennToggledVarNew
            maybeTVar
            Language.hinzufügen
            foldWegstreckeHinzufügen
            (atomically . readStatusVar)
            statusVar
    dynTMVarPlanObjekt <- newEmptyTMVarIO
    let dynamischeWidgets =
            DynamischeWidgets
            { dynBGWidgetsBoxen = BGWidgetsBoxen
                  { vBoxBahngeschwindigkeiten
                  , vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
                  , vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
                  , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                  , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
                  , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
                  , vBoxHinzufügenPlanBahngeschwindigkeitenLego
                  , vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
                  , vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
                  }
            , dynSTWidgetsBoxen = STWidgetsBoxen
                  { vBoxStreckenabschnitte
                  , vBoxHinzufügenWegstreckeStreckenabschnitte
                  , vBoxHinzufügenPlanStreckenabschnitte
                  }
            , dynWEWidgetsBoxen = WEWidgetsBoxen
                  { vBoxWeichen
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
                  }
            , dynKUWidgetsBoxen = KUWidgetsBoxen
                  { vBoxKupplungen
                  , vBoxHinzufügenWegstreckeKupplungen
                  , vBoxHinzufügenPlanKupplungen
                  }
            , dynKOWidgetsBoxen = KOWidgetsBoxen
                  { vBoxKontakte
                  , vBoxHinzufügenWegstreckeKontakte
                  , vBoxHinzufügenPlanKontakte
                  }
            , dynWSWidgetsBoxen = WSWidgetsBoxen
                  { vBoxWegstrecken
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
                  , vBoxHinzufügenPlanWegstreckenKontaktMärklin
                  , vBoxHinzufügenPlanWegstreckenKontaktLego
                  , vBoxHinzufügenPlanWegstreckenMärklin
                  , vBoxHinzufügenPlanWegstreckenLego
                  }
            , dynPLWidgetsBoxen = PLWidgetsBoxen { vBoxPläne, vBoxHinzufügenPlanPläne }
            , dynWindowMain
            , dynFortfahrenWennToggledWegstrecke
            , dynTMVarPlanObjekt
            , dynAktionBearbeiten = aktionBearbeiten
            }
    let objektReader = (tvarMaps, dynamischeWidgets, statusVar)
    -- Knopf-Leiste mit globalen Funktionen
    functionBox <- boxPackWidgetNew vBox PackNatural paddingDefault End
        $ Gtk.boxNew Gtk.OrientationHorizontal 0
    aktionBearbeiten <- flip runReaderT objektReader $ do
        -- Linke Seite
        (_buttonHinzufügen, aktionBearbeitenReader)
            <- buttonHinzufügenPack dynWindowMain functionBox maybeTVar
        spracheAuswahl <- boxPackWidgetNewDefault functionBox
            $ boundedEnumAuswahlComboBoxNew Language.Deutsch maybeTVar Language.sprache
        beiAuswahl spracheAuswahl $ \sp -> void $ do
            sprachwechsel spracheGui sp
            flip runReaderT objektReader
                $ ausführenStatusVarBefehl (SprachWechsel spracheGui) statusVar
        -- Rechte seite
        boxPackWidgetNew functionBox packingDefault paddingDefault End
            $ buttonNewWithEventLabel maybeTVar Language.beenden
            $ Gtk.mainQuit
        buttonLadenPack dynWindowMain functionBox maybeTVar End
        buttonSpeichernPack dynWindowMain functionBox maybeTVar End
        pure aktionBearbeitenReader
    checkButtonNotebook <- boxPackWidgetNewDefault functionBox
        $ toggleButtonNewWithEvent Gtk.checkButtonNew
        $ \toggled -> do
            widgetShowIf toggled notebookElementeEinzelseiten
            widgetShowIf (not toggled) notebookElementePaned
            case toggled of
                True -> do
                    mitContainerRemove vBoxPanedBahngeschwindigkeiten vBoxBahngeschwindigkeiten
                    boxPack
                        vBoxBG
                        vBoxBahngeschwindigkeiten
                        PackGrow
                        paddingDefault
                        positionDefault
                    mitContainerRemove vBoxPanedStreckenabschnitte vBoxStreckenabschnitte
                    boxPack vBoxST vBoxStreckenabschnitte PackGrow paddingDefault positionDefault
                    mitContainerRemove vBoxPanedWeichen vBoxWeichen
                    boxPack vBoxWE vBoxWeichen PackGrow paddingDefault positionDefault
                    mitContainerRemove vBoxPanedKupplungen vBoxKupplungen
                    boxPack vBoxKU vBoxKupplungen PackGrow paddingDefault positionDefault
                    mitContainerRemove vBoxPanedKontakte vBoxKontakte
                    boxPack vBoxKO vBoxKontakte PackGrow paddingDefault positionDefault
                    mitContainerRemove vBoxPanedWegstrecken vBoxWegstrecken
                    boxPack vBoxWS vBoxWegstrecken PackGrow paddingDefault positionDefault
                    mitContainerRemove vBoxPanedPläne vBoxPläne
                    boxPack vBoxPL vBoxPläne PackGrow paddingDefault positionDefault
                False -> do
                    mitContainerRemove vBoxBG vBoxBahngeschwindigkeiten
                    boxPack
                        vBoxPanedBahngeschwindigkeiten
                        vBoxBahngeschwindigkeiten
                        PackGrow
                        paddingDefault
                        positionDefault
                    mitContainerRemove vBoxST vBoxStreckenabschnitte
                    boxPack
                        vBoxPanedStreckenabschnitte
                        vBoxStreckenabschnitte
                        PackGrow
                        paddingDefault
                        positionDefault
                    mitContainerRemove vBoxWE vBoxWeichen
                    boxPack vBoxPanedWeichen vBoxWeichen PackGrow paddingDefault positionDefault
                    mitContainerRemove vBoxKU vBoxKupplungen
                    boxPack
                        vBoxPanedKupplungen
                        vBoxKupplungen
                        PackGrow
                        paddingDefault
                        positionDefault
                    mitContainerRemove vBoxKO vBoxKontakte
                    boxPack vBoxPanedKontakte vBoxKontakte PackGrow paddingDefault positionDefault
                    mitContainerRemove vBoxWS vBoxWegstrecken
                    boxPack
                        vBoxPanedWegstrecken
                        vBoxWegstrecken
                        PackGrow
                        paddingDefault
                        positionDefault
                    mitContainerRemove vBoxPL vBoxPläne
                    boxPack vBoxPanedPläne vBoxPläne PackGrow paddingDefault positionDefault
    verwendeSpracheGuiFn spracheGui maybeTVar
        $ \sp -> Gtk.set checkButtonNotebook [Gtk.buttonLabel := Language.einzelseiten sp]
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
    -- Zeige Einzelseiten an (falls gewünscht)
    when (gtkSeiten == Einzelseiten) $ Gtk.set checkButtonNotebook [Gtk.toggleButtonActive := True]
    -- Fenster wird erst hier angezeigt, weil sonst windowDefaultWidth/Height keinen Effekt zeigen
    Gtk.widgetShow dynWindowMain
    -- Dummy-Fenster löschen
    Gtk.widgetDestroy windowDummy
#endif
--
