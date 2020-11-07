{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description : Erstelle GUI und starte den GTK-Main-Loop.
-}
module Zug.UI.Gtk (main, setupGUI) where

import Control.Concurrent (runInBoundThread, ThreadId)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, readTMVar)
import Control.Monad (void, when, forM_, foldM)
import qualified Control.Monad.RWS.Strict as RWS
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.GI.Gtk.Threading as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import System.IO.Unsafe (unsafeInterleaveIO)

import Zug.Anbindung (warte, Wartezeit(MilliSekunden))
import Zug.Language (MitSprache(leseSprache), (<~>), (<|>))
import qualified Zug.Language as Language
import Zug.Options (Options(..), getOptions, GtkSeiten(Einzelseiten), VersionReader(erhalteVersion))
import Zug.UI.Base (Status, statusLeer, tvarMapsNeu)
import Zug.UI.Befehl (BefehlAllgemein(..))
import Zug.UI.Gtk.Auswahl (boundedEnumAuswahlComboBoxNew, beiAuswahl)
import Zug.UI.Gtk.Fenster (buttonSpeichernPack, buttonLadenPack, ladeWidgets, buttonHinzufügenPack)
import Zug.UI.Gtk.FortfahrenWennToggled (fortfahrenWennToggledVarNew)
import Zug.UI.Gtk.Gleise (gleisAnzeigeNew)
import Zug.UI.Gtk.Hilfsfunktionen
       (widgetShowNew, widgetShowIf, buttonNewWithEventLabel, containerAddWidgetNew
      , boxPackWidgetNew, boxPackWidgetNewDefault, boxPack, Packing(..), packingDefault
      , paddingDefault, Position(..), positionDefault, notebookAppendPageNew, labelSpracheNew
      , toggleButtonNewWithEvent)
import Zug.UI.Gtk.Klassen (mitNotebookSetCurrentPage, mitContainerRemove)
import Zug.UI.Gtk.ScrollbaresWidget (scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui
       (spracheGuiNeu, verwendeSpracheGuiFn, sprachwechsel, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt
       (DynamischeWidgets(..), boxWegstreckeHinzufügenNew, boxPlanHinzufügenNew, MStatusGuiT
      , IOStatusGui, checkButtonsWegstreckeHinzufügen, BGWidgetsBoxen(..), STWidgetsBoxen(..)
      , WEWidgetsBoxen(..), KUWidgetsBoxen(..), KOWidgetsBoxen(..), WSWidgetsBoxen(..)
      , PLWidgetsBoxen(..))
import Zug.UI.StatusVar (statusVarNew, ausführenStatusVarBefehl, readStatusVar)
import Zug.Util (forkIOSilent, maybeSilent)

-- | Gtk-main loop.
main :: (VersionReader r m, MonadIO m) => m ()
main = do
    v <- erhalteVersion
    liftIO $ runInBoundThread $ maybeSilent $ do
        -- Initialisiere GTK+ engine
        Gtk.init Nothing
        Gtk.setCurrentThreadAsGUIThread
        -- Erstelle GUI
        runReaderT (setupGUI Nothing) v
        -- GTK+ main loop
        Gtk.main

-- | Erstelle GUI inkl. sämtlicher Events.
--
-- Zur Verwendung muss vorher 'Gtk.initGUI' aufgerufen werden.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
setupGUI :: (VersionReader r m, MonadIO m) => Maybe TVarSprachewechselAktionen -> m ()
setupGUI maybeTVar = do
    Options {load = dateipfad, gtkSeiten, sprache} <- getOptions
    v <- erhalteVersion
    void $ liftIO $ do
        spracheGui <- spracheGuiNeu sprache
        -- Dummy-Fenster, damit etwas angezeigt wird
        windowDummy <- Gtk.windowNew Gtk.WindowTypeToplevel
        flip leseSprache spracheGui $ \sp -> do
            Gtk.setWindowTitle windowDummy $ Language.zugkontrolle <~> Language.version v $ sp
            Gtk.setWindowDeletable windowDummy False
            Gtk.setWindowDefaultWidth windowDummy 400
            Gtk.setWindowDefaultHeight windowDummy 200
        progressBarDummy <- containerAddWidgetNew windowDummy Gtk.progressBarNew
        Gtk.setProgressBarFraction progressBarDummy 0
        Gtk.setProgressBarShowText progressBarDummy True
        Gtk.setProgressBarText progressBarDummy "Loading"
        Gtk.widgetShow windowDummy
        -- Hauptfenster
        dynWindowMain <- Gtk.windowNew Gtk.WindowTypeToplevel
        -- native Auflösung des Raspi 7'' TouchScreen ist 800x480
        -- leicht kleinere Werte um Menüleisten zu berücksichtigen
        Gtk.setWindowDefaultWidth dynWindowMain 720
        Gtk.setWindowDefaultHeight dynWindowMain 450
        Gtk.windowMaximize dynWindowMain
        -- Titel
        verwendeSpracheGuiFn spracheGui maybeTVar $ \sp
            -> Gtk.setWindowTitle dynWindowMain $ Language.zugkontrolle <~> Language.version v $ sp
        -- Drücken des X-Knopfes beendet das gesamte Program
        Gtk.onWidgetDeleteEvent dynWindowMain $ \_event -> do
            Gtk.mainQuit
            pure False
        vBox <- containerAddWidgetNew dynWindowMain $ Gtk.boxNew Gtk.OrientationVertical 0
        tvarMaps <- tvarMapsNeu
        statusVar <- statusVarNew $ statusLeer spracheGui
        -- Notebook mit aktuellen Elementen
        -- Einzelseite-Variante
        tmvarNotebookElementeEinzel <- newEmptyTMVarIO
        tmvarVBoxBahngeschwindigkeitenEinzel <- newEmptyTMVarIO
        tmvarVBoxStreckenabschnitteEinzel <- newEmptyTMVarIO
        tmvarVBoxWeichenEinzel <- newEmptyTMVarIO
        tmvarVBoxKupplungenEinzel <- newEmptyTMVarIO
        tmvarVBoxKontakteEinzel <- newEmptyTMVarIO
        tmvarVBoxWegstreckenEinzel <- newEmptyTMVarIO
        tmvarVBoxPläneEinzel <- newEmptyTMVarIO
        tmvarVBoxGleiseEinzel <- newEmptyTMVarIO
        let aktionNotebookElementeEinzel = do
                notebookElementeEinzel <- boxPackWidgetNew
                    vBox
                    PackGrow
                    paddingDefault
                    positionDefault
                    Gtk.notebookNew
                atomically $ putTMVar tmvarNotebookElementeEinzel notebookElementeEinzel
                Gtk.widgetHide notebookElementeEinzel
                flip runReaderT spracheGui $ do
                    (vBoxBahngeschwindigkeitenEinzel, _page) <- notebookAppendPageNew
                        notebookElementeEinzel
                        maybeTVar
                        Language.bahngeschwindigkeiten
                        $ Gtk.boxNew Gtk.OrientationVertical 0
                    liftIO
                        $ atomically
                        $ putTMVar
                            tmvarVBoxBahngeschwindigkeitenEinzel
                            vBoxBahngeschwindigkeitenEinzel
                    (vBoxStreckenabschnitteEinzel, _page) <- notebookAppendPageNew
                        notebookElementeEinzel
                        maybeTVar
                        Language.streckenabschnitte
                        $ Gtk.boxNew Gtk.OrientationVertical 0
                    liftIO
                        $ atomically
                        $ putTMVar tmvarVBoxStreckenabschnitteEinzel vBoxStreckenabschnitteEinzel
                    (vBoxWeichenEinzel, _page)
                        <- notebookAppendPageNew notebookElementeEinzel maybeTVar Language.weichen
                        $ Gtk.boxNew Gtk.OrientationVertical 0
                    liftIO $ atomically $ putTMVar tmvarVBoxWeichenEinzel vBoxWeichenEinzel
                    (vBoxKupplungenEinzel, _page) <- notebookAppendPageNew
                        notebookElementeEinzel
                        maybeTVar
                        Language.kupplungen
                        $ Gtk.boxNew Gtk.OrientationVertical 0
                    liftIO $ atomically $ putTMVar tmvarVBoxKupplungenEinzel vBoxKupplungenEinzel
                    (vBoxKontakteEinzel, _page)
                        <- notebookAppendPageNew notebookElementeEinzel maybeTVar Language.kontakte
                        $ Gtk.boxNew Gtk.OrientationVertical 0
                    liftIO $ atomically $ putTMVar tmvarVBoxKontakteEinzel vBoxKontakteEinzel
                    (vBoxWegstreckenEinzel, _page) <- notebookAppendPageNew
                        notebookElementeEinzel
                        maybeTVar
                        Language.wegstrecken
                        $ Gtk.boxNew Gtk.OrientationVertical 0
                    liftIO $ atomically $ putTMVar tmvarVBoxWegstreckenEinzel vBoxWegstreckenEinzel
                    (vBoxPläneEinzel, _page)
                        <- notebookAppendPageNew notebookElementeEinzel maybeTVar Language.pläne
                        $ Gtk.boxNew Gtk.OrientationVertical 0
                    liftIO $ atomically $ putTMVar tmvarVBoxPläneEinzel vBoxPläneEinzel
                    -- TODO Gleise
                    (vBoxGleiseEinzel, pageGleise)
                        <- notebookAppendPageNew notebookElementeEinzel maybeTVar (const "Gleise")
                        $ Gtk.boxNew Gtk.OrientationVertical 0
                    liftIO $ atomically $ putTMVar tmvarVBoxGleiseEinzel vBoxGleiseEinzel
                    mitNotebookSetCurrentPage notebookElementeEinzel pageGleise
        -- Paned-Variante
        tmvarNotebookElementePaned <- newEmptyTMVarIO
        tmvarVBoxBahngeschwindigkeitenPaned <- newEmptyTMVarIO
        tmvarVBoxStreckenabschnittePaned <- newEmptyTMVarIO
        tmvarVBoxWeichenPaned <- newEmptyTMVarIO
        tmvarVBoxKupplungenPaned <- newEmptyTMVarIO
        tmvarVBoxKontaktePaned <- newEmptyTMVarIO
        tmvarVBoxWegstreckenPaned <- newEmptyTMVarIO
        tmvarVBoxPlänePaned <- newEmptyTMVarIO
        tmvarVBoxGleisePaned <- newEmptyTMVarIO
        tmvarVBoxBahngeschwindigkeiten <- newEmptyTMVarIO
        tmvarVBoxStreckenabschnitte <- newEmptyTMVarIO
        tmvarVBoxWeichen <- newEmptyTMVarIO
        tmvarVBoxKupplungen <- newEmptyTMVarIO
        tmvarVBoxKontakte <- newEmptyTMVarIO
        tmvarVBoxWegstrecken <- newEmptyTMVarIO
        tmvarVBoxPläne <- newEmptyTMVarIO
        tmvarGleisAnzeige <- newEmptyTMVarIO
        let aktionNotebookElementePaned = do
                notebookElementePaned <- boxPackWidgetNew
                    vBox
                    PackGrow
                    paddingDefault
                    positionDefault
                    Gtk.notebookNew
                (panedBahngeschwindigkeitStreckenabschnittWeiche, _pageBgStWe)
                    <- flip runReaderT spracheGui
                    $ notebookAppendPageNew
                        notebookElementePaned
                        maybeTVar
                        (Language.bahngeschwindigkeiten
                         <|> Language.streckenabschnitte
                         <|> Language.weichen)
                    $ Gtk.panedNew Gtk.OrientationHorizontal
                atomically $ putTMVar tmvarNotebookElementePaned notebookElementePaned
                vPanedBahngeschwindigkeitStreckenabschnitt
                    <- widgetShowNew $ Gtk.panedNew Gtk.OrientationVertical
                Gtk.panedAdd1
                    panedBahngeschwindigkeitStreckenabschnittWeiche
                    vPanedBahngeschwindigkeitStreckenabschnitt
                frameBahngeschwindigkeiten <- widgetShowNew $ Gtk.frameNew Nothing
                Gtk.setFrameShadowType frameBahngeschwindigkeiten Gtk.ShadowTypeIn
                Gtk.panedAdd1 vPanedBahngeschwindigkeitStreckenabschnitt frameBahngeschwindigkeiten
                vBoxBahngeschwindigkeitenPaned <- containerAddWidgetNew frameBahngeschwindigkeiten
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically
                    $ putTMVar tmvarVBoxBahngeschwindigkeitenPaned vBoxBahngeschwindigkeitenPaned
                flip runReaderT spracheGui
                    $ boxPackWidgetNewDefault vBoxBahngeschwindigkeitenPaned
                    $ labelSpracheNew maybeTVar Language.bahngeschwindigkeiten
                vBoxBahngeschwindigkeiten <- boxPackWidgetNew
                    vBoxBahngeschwindigkeitenPaned
                    PackGrow
                    paddingDefault
                    positionDefault
                    $ scrollbaresWidgetNew
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxBahngeschwindigkeiten vBoxBahngeschwindigkeiten
                frameStreckenabschnitte <- widgetShowNew $ Gtk.frameNew Nothing
                Gtk.setFrameShadowType frameStreckenabschnitte Gtk.ShadowTypeIn
                Gtk.panedAdd2 vPanedBahngeschwindigkeitStreckenabschnitt frameStreckenabschnitte
                vBoxStreckenabschnittePaned <- containerAddWidgetNew frameStreckenabschnitte
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxStreckenabschnittePaned vBoxStreckenabschnittePaned
                flip runReaderT spracheGui
                    $ boxPackWidgetNewDefault vBoxStreckenabschnittePaned
                    $ labelSpracheNew maybeTVar Language.streckenabschnitte
                vBoxStreckenabschnitte <- boxPackWidgetNew
                    vBoxStreckenabschnittePaned
                    PackGrow
                    paddingDefault
                    positionDefault
                    $ scrollbaresWidgetNew
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxStreckenabschnitte vBoxStreckenabschnitte
                frameWeichen <- widgetShowNew $ Gtk.frameNew Nothing
                Gtk.setFrameShadowType frameWeichen Gtk.ShadowTypeIn
                Gtk.panedAdd2 panedBahngeschwindigkeitStreckenabschnittWeiche frameWeichen
                vBoxWeichenPaned
                    <- containerAddWidgetNew frameWeichen $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxWeichenPaned vBoxWeichenPaned
                flip runReaderT spracheGui
                    $ boxPackWidgetNewDefault vBoxWeichenPaned
                    $ labelSpracheNew maybeTVar Language.weichen
                vBoxWeichen
                    <- boxPackWidgetNew vBoxWeichenPaned PackGrow paddingDefault positionDefault
                    $ scrollbaresWidgetNew
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxWeichen vBoxWeichen
                (panedKupplungKontakt, _pageKuKo) <- flip runReaderT spracheGui
                    $ notebookAppendPageNew
                        notebookElementePaned
                        maybeTVar
                        (Language.kupplungen <|> Language.kontakte)
                    $ Gtk.panedNew Gtk.OrientationHorizontal
                frameKupplungen <- widgetShowNew $ Gtk.frameNew Nothing
                Gtk.setFrameShadowType frameKupplungen Gtk.ShadowTypeIn
                Gtk.panedAdd1 panedKupplungKontakt frameKupplungen
                vBoxKupplungenPaned
                    <- containerAddWidgetNew frameKupplungen $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxKupplungenPaned vBoxKupplungenPaned
                flip runReaderT spracheGui
                    $ boxPackWidgetNewDefault vBoxKupplungenPaned
                    $ labelSpracheNew maybeTVar Language.kupplungen
                vBoxKupplungen
                    <- boxPackWidgetNew vBoxKupplungenPaned PackGrow paddingDefault positionDefault
                    $ scrollbaresWidgetNew
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxKupplungen vBoxKupplungen
                frameKontakte <- widgetShowNew $ Gtk.frameNew Nothing
                Gtk.setFrameShadowType frameKontakte Gtk.ShadowTypeIn
                Gtk.panedAdd2 panedKupplungKontakt frameKontakte
                vBoxKontaktePaned
                    <- containerAddWidgetNew frameKontakte $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxKontaktePaned vBoxKontaktePaned
                flip runReaderT spracheGui
                    $ boxPackWidgetNewDefault vBoxKontaktePaned
                    $ labelSpracheNew maybeTVar Language.kontakte
                vBoxKontakte
                    <- boxPackWidgetNew vBoxKontaktePaned PackGrow paddingDefault positionDefault
                    $ scrollbaresWidgetNew
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxKontakte vBoxKontakte
                (panedWegstreckePlan, _pageWsPl) <- flip runReaderT spracheGui
                    $ notebookAppendPageNew
                        notebookElementePaned
                        maybeTVar
                        (Language.wegstrecken <|> Language.pläne)
                    $ Gtk.panedNew Gtk.OrientationHorizontal
                frameWegstrecken <- widgetShowNew $ Gtk.frameNew Nothing
                Gtk.setFrameShadowType frameWegstrecken Gtk.ShadowTypeIn
                Gtk.panedAdd1 panedWegstreckePlan frameWegstrecken
                vBoxWegstreckenPaned <- containerAddWidgetNew frameWegstrecken
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxWegstreckenPaned vBoxWegstreckenPaned
                flip runReaderT spracheGui
                    $ boxPackWidgetNewDefault vBoxWegstreckenPaned
                    $ labelSpracheNew maybeTVar Language.wegstrecken
                vBoxWegstrecken <- boxPackWidgetNew
                    vBoxWegstreckenPaned
                    PackGrow
                    paddingDefault
                    positionDefault
                    $ scrollbaresWidgetNew
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxWegstrecken vBoxWegstrecken
                framePläne <- widgetShowNew $ Gtk.frameNew Nothing
                Gtk.setFrameShadowType framePläne Gtk.ShadowTypeIn
                Gtk.panedAdd2 panedWegstreckePlan framePläne
                vBoxPlänePaned
                    <- containerAddWidgetNew framePläne $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxPlänePaned vBoxPlänePaned
                flip runReaderT spracheGui
                    $ boxPackWidgetNewDefault vBoxPlänePaned
                    $ labelSpracheNew maybeTVar Language.pläne
                vBoxPläne
                    <- boxPackWidgetNew vBoxPlänePaned PackGrow paddingDefault positionDefault
                    $ scrollbaresWidgetNew
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                atomically $ putTMVar tmvarVBoxPläne vBoxPläne
                -- TODO Gleise
                (vBoxGleisePaned, pageGleise) <- flip runReaderT spracheGui
                    $ notebookAppendPageNew notebookElementePaned maybeTVar (const "Gleise")
                    $ Gtk.boxNew Gtk.OrientationVertical 0
                liftIO $ atomically $ putTMVar tmvarVBoxGleisePaned vBoxGleisePaned
                mitNotebookSetCurrentPage notebookElementePaned pageGleise
                gleisAnzeige
                    <- boxPackWidgetNew vBoxGleisePaned PackGrow paddingDefault positionDefault
                    $ scrollbaresWidgetNew
                    $ gleisAnzeigeNew
                atomically $ putTMVar tmvarGleisAnzeige gleisAnzeige
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
                                $ \paned -> Gtk.setPanedPosition paned $ div screenWidth 2
                            screenHeight <- Gdk.getRectangleHeight geometry
                            -- geschätzter Wert
                            let decoratorHeight = 50
                            Gtk.setPanedPosition vPanedBahngeschwindigkeitStreckenabschnitt
                                $ div (screenHeight - decoratorHeight) 3
                        Nothing -> pure ()
                    Nothing -> pure ()
        -- DynamischeWidgets
        tmvarObjektReader <- newEmptyTMVarIO
        tmvarAktionBearbeiten <- newEmptyTMVarIO
        let aktionObjektReader = do
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
                vBoxHinzufügenWegstreckeKupplungen
                    <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
                vBoxHinzufügenPlanKupplungen
                    <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
                vBoxHinzufügenWegstreckeKontakte
                    <- flip runReaderT spracheGui $ boxWegstreckeHinzufügenNew
                vBoxHinzufügenPlanKontakte
                    <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
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
                vBoxHinzufügenPlanPläne
                    <- flip runReaderT spracheGui $ boxPlanHinzufügenNew maybeTVar
                dynFortfahrenWennToggledWegstrecke <- flip runReaderT spracheGui
                    $ fortfahrenWennToggledVarNew
                        maybeTVar
                        Language.hinzufügen
                        checkButtonsWegstreckeHinzufügen
                        (liftIO . atomically . readStatusVar)
                        statusVar
                dynTMVarPlanObjekt <- newEmptyTMVarIO
                vBoxBahngeschwindigkeiten <- atomically $ readTMVar tmvarVBoxBahngeschwindigkeiten
                vBoxStreckenabschnitte <- atomically $ readTMVar tmvarVBoxStreckenabschnitte
                vBoxWeichen <- atomically $ readTMVar tmvarVBoxWeichen
                vBoxKupplungen <- atomically $ readTMVar tmvarVBoxKupplungen
                vBoxKontakte <- atomically $ readTMVar tmvarVBoxKontakte
                vBoxWegstrecken <- atomically $ readTMVar tmvarVBoxWegstrecken
                vBoxPläne <- atomically $ readTMVar tmvarVBoxPläne
                -- unsafeInterleaveIO necessary, since the TMVar is only filled later
                aktionBearbeiten
                    <- unsafeInterleaveIO $ atomically $ readTMVar tmvarAktionBearbeiten
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
                        , dynPLWidgetsBoxen =
                              PLWidgetsBoxen { vBoxPläne, vBoxHinzufügenPlanPläne }
                        , dynWindowMain
                        , dynFortfahrenWennToggledWegstrecke
                        , dynTMVarPlanObjekt
                        , dynAktionBearbeiten = aktionBearbeiten
                        }
                let objektReader = (tvarMaps, dynamischeWidgets, statusVar, v)
                atomically $ putTMVar tmvarObjektReader objektReader
        -- Knopf-Leiste mit globalen Funktionen
        tmvarCheckButtonNotebook <- newEmptyTMVarIO
        let aktionFunctionBox = do
                objektReader <- atomically $ readTMVar tmvarObjektReader
                notebookElementeEinzel <- atomically $ readTMVar tmvarNotebookElementeEinzel
                vBoxBahngeschwindigkeitenEinzel
                    <- atomically $ readTMVar tmvarVBoxBahngeschwindigkeitenEinzel
                vBoxStreckenabschnitteEinzel
                    <- atomically $ readTMVar tmvarVBoxStreckenabschnitteEinzel
                vBoxWeichenEinzel <- atomically $ readTMVar tmvarVBoxWeichenEinzel
                vBoxKupplungenEinzel <- atomically $ readTMVar tmvarVBoxKupplungenEinzel
                vBoxKontakteEinzel <- atomically $ readTMVar tmvarVBoxKontakteEinzel
                vBoxWegstreckenEinzel <- atomically $ readTMVar tmvarVBoxWegstreckenEinzel
                vBoxPläneEinzel <- atomically $ readTMVar tmvarVBoxPläneEinzel
                notebookElementePaned <- atomically $ readTMVar tmvarNotebookElementePaned
                vBoxBahngeschwindigkeitenPaned
                    <- atomically $ readTMVar tmvarVBoxBahngeschwindigkeitenPaned
                vBoxStreckenabschnittePaned
                    <- atomically $ readTMVar tmvarVBoxStreckenabschnittePaned
                vBoxWeichenPaned <- atomically $ readTMVar tmvarVBoxWeichenPaned
                vBoxKupplungenPaned <- atomically $ readTMVar tmvarVBoxKupplungenPaned
                vBoxKontaktePaned <- atomically $ readTMVar tmvarVBoxKontaktePaned
                vBoxWegstreckenPaned <- atomically $ readTMVar tmvarVBoxWegstreckenPaned
                vBoxPlänePaned <- atomically $ readTMVar tmvarVBoxPlänePaned
                vBoxBahngeschwindigkeiten <- atomically $ readTMVar tmvarVBoxBahngeschwindigkeiten
                vBoxStreckenabschnitte <- atomically $ readTMVar tmvarVBoxStreckenabschnitte
                vBoxWeichen <- atomically $ readTMVar tmvarVBoxWeichen
                vBoxKupplungen <- atomically $ readTMVar tmvarVBoxKupplungen
                vBoxKontakte <- atomically $ readTMVar tmvarVBoxKontakte
                vBoxWegstrecken <- atomically $ readTMVar tmvarVBoxWegstrecken
                vBoxPläne <- atomically $ readTMVar tmvarVBoxPläne
                -- TODO Gleise
                vBoxGleiseEinzel <- atomically $ readTMVar tmvarVBoxGleiseEinzel
                vBoxGleisePaned <- atomically $ readTMVar tmvarVBoxGleisePaned
                gleisAnzeige <- atomically $ readTMVar tmvarGleisAnzeige
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
                atomically $ putTMVar tmvarAktionBearbeiten aktionBearbeiten
                checkButtonNotebook <- boxPackWidgetNewDefault functionBox
                    $ toggleButtonNewWithEvent Gtk.checkButtonNew
                    $ \toggled -> do
                        widgetShowIf toggled notebookElementeEinzel
                        widgetShowIf (not toggled) notebookElementePaned
                        if toggled
                            then do
                                mitContainerRemove
                                    vBoxBahngeschwindigkeitenPaned
                                    vBoxBahngeschwindigkeiten
                                boxPack
                                    vBoxBahngeschwindigkeitenEinzel
                                    vBoxBahngeschwindigkeiten
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove
                                    vBoxStreckenabschnittePaned
                                    vBoxStreckenabschnitte
                                boxPack
                                    vBoxStreckenabschnitteEinzel
                                    vBoxStreckenabschnitte
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxWeichenPaned vBoxWeichen
                                boxPack
                                    vBoxWeichenEinzel
                                    vBoxWeichen
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxKupplungenPaned vBoxKupplungen
                                boxPack
                                    vBoxKupplungenEinzel
                                    vBoxKupplungen
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxKontaktePaned vBoxKontakte
                                boxPack
                                    vBoxKontakteEinzel
                                    vBoxKontakte
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxWegstreckenPaned vBoxWegstrecken
                                boxPack
                                    vBoxWegstreckenEinzel
                                    vBoxWegstrecken
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxPlänePaned vBoxPläne
                                boxPack
                                    vBoxPläneEinzel
                                    vBoxPläne
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                -- TODO Gleise
                                mitContainerRemove vBoxGleisePaned gleisAnzeige
                                boxPack
                                    vBoxGleiseEinzel
                                    gleisAnzeige
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                            else do
                                mitContainerRemove
                                    vBoxBahngeschwindigkeitenEinzel
                                    vBoxBahngeschwindigkeiten
                                boxPack
                                    vBoxBahngeschwindigkeitenPaned
                                    vBoxBahngeschwindigkeiten
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove
                                    vBoxStreckenabschnitteEinzel
                                    vBoxStreckenabschnitte
                                boxPack
                                    vBoxStreckenabschnittePaned
                                    vBoxStreckenabschnitte
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxWeichenEinzel vBoxWeichen
                                boxPack
                                    vBoxWeichenPaned
                                    vBoxWeichen
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxKupplungenEinzel vBoxKupplungen
                                boxPack
                                    vBoxKupplungenPaned
                                    vBoxKupplungen
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxKontakteEinzel vBoxKontakte
                                boxPack
                                    vBoxKontaktePaned
                                    vBoxKontakte
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxWegstreckenEinzel vBoxWegstrecken
                                boxPack
                                    vBoxWegstreckenPaned
                                    vBoxWegstrecken
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                mitContainerRemove vBoxPläneEinzel vBoxPläne
                                boxPack
                                    vBoxPlänePaned
                                    vBoxPläne
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                                -- TODO Gleise
                                mitContainerRemove vBoxGleiseEinzel gleisAnzeige
                                boxPack
                                    vBoxGleisePaned
                                    gleisAnzeige
                                    PackGrow
                                    paddingDefault
                                    positionDefault
                atomically $ putTMVar tmvarCheckButtonNotebook checkButtonNotebook
                verwendeSpracheGuiFn spracheGui maybeTVar
                    $ \sp -> Gtk.setButtonLabel checkButtonNotebook $ Language.einzelseiten sp
        -- Lade Datei angegeben in Kommandozeilenargument
        let aktionLaden = void $ do
                objektReader <- atomically $ readTMVar tmvarObjektReader
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
                    $ ausführenStatusVarBefehl
                        (Laden dateipfad ladeAktion fehlerBehandlung)
                        statusVar
            -- Zeige Einzelseiten an (falls gewünscht)
            aktionSeitenvariante = do
                checkButtonNotebook <- atomically $ readTMVar tmvarCheckButtonNotebook
                when (gtkSeiten == Einzelseiten)
                    $ Gtk.setToggleButtonActive checkButtonNotebook True
            aktionFinalisieren = do
                -- Fenster wird erst hier angezeigt, weil sonst windowDefaultWidth/Height keinen Effekt zeigen
                Gtk.widgetShow dynWindowMain
                -- Dummy-Fenster löschen
                Gtk.widgetDestroy windowDummy
        gtkWithProgress
            progressBarDummy
            [ aktionNotebookElementeEinzel
            , aktionNotebookElementePaned
            , aktionObjektReader
            , aktionFunctionBox
            , aktionLaden
            , aktionSeitenvariante
            , aktionFinalisieren]

-- | Execute several Gtk-Actions, showing progress after every one.
gtkWithProgress :: Gtk.ProgressBar -> [IO ()] -> IO ThreadId
gtkWithProgress progressBar actions = forkIOSilent $ void $ foldM foldFn 0 actions
    where
        step :: Double
        step = 1 / (fromIntegral $ length actions)

        foldFn :: Double -> IO () -> IO Double
        foldFn fraction action = do
            let newFraction = fraction + step
            Gtk.postGUIASync $ do
                action
                Gtk.setProgressBarFraction progressBar newFraction
            -- give gtk some time to actually display the changed progress bar
            warte $ MilliSekunden 300
            pure newFraction
