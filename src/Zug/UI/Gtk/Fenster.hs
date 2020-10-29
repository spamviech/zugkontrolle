{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
#endif

{-|
Description : Abfragen mit neuem Fenster für das Gtk-UI.
-}
module Zug.UI.Gtk.Fenster
  (
#ifdef ZUGKONTROLLEGUI
    -- * Knöpfe mit zugehörigem Dialog erstellen
    buttonSpeichernPack
  , buttonLadenPack
  , ladeWidgets
  , buttonHinzufügenPack
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTMVar, takeTMVar, putTMVar)
import Control.Monad (void, when)
import Control.Monad.Fix (MonadFix())
import qualified Control.Monad.RWS.Strict as RWS
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified GI.Gtk as Gtk

import Zug.Anbindung (Bahngeschwindigkeit(..), Weiche(..), Wegstrecke(..))
import Zug.Enums (ZugtypEither(..), ZugtypKlasse(..), GeschwindigkeitEither(..)
                , GeschwindigkeitPhantom(..), GeschwindigkeitKlasse(..))
import qualified Zug.Language as Language
import Zug.Language (MitSprache(..), (<!>))
import Zug.Objekt (ObjektAllgemein(..), Objekt, ObjektElement(ObjektTyp, zuObjektTyp))
import Zug.Plan (Plan, PlanAllgemein(..), Aktion, AktionAllgemein(..), AktionBahngeschwindigkeit(..)
               , AktionStreckenabschnitt(..), AktionWeiche(..), AktionKupplung(..)
               , AktionKontakt(..), AktionWegstrecke(..))
import Zug.UI.Base (Status, bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, kontakte
                  , wegstrecken, pläne, sprache, statusLeer)
import Zug.UI.Befehl (BefehlAllgemein(..))
import Zug.UI.Gtk.AssistantHinzufuegen
       (AssistantHinzufügen, assistantHinzufügenNew, assistantHinzufügenAuswerten
      , HinzufügenErgebnis(..), setzeAssistantHinzufügen)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, boxPackWidgetNew, packingDefault
                                 , paddingDefault, Position(), buttonNewWithEventLabel, dialogEval)
import Zug.UI.Gtk.Klassen (MitBox(..), MitWindow(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), verwendeSpracheGui, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt
       (MStatusGuiT, IOStatusGui, ObjektGuiReader, StatusVarGui, readSpracheGui, StatusGui
      , DynamischeWidgetsReader(..), WidgetsTyp(..), bahngeschwindigkeitPackNew, BGWidgets
      , streckenabschnittPackNew, weichePackNew, WEWidgets, kupplungPackNew, kontaktPackNew
      , wegstreckePackNew, WSWidgets, planPackNew, PlanGui, planGui, AktionGui)
import Zug.UI.StatusVar (auswertenStatusVarMStatusT, ausführenStatusVarBefehl, StatusVarReader(..))

-- | Speichern des aktuellen 'StatusGui'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonSpeichernPack
    :: forall b m.
    (MitBox b, ObjektGuiReader m, MonadIO m)
    => Gtk.Window
    -> b
    -> Maybe TVarSprachewechselAktionen
    -> Position
    -> m Gtk.Button
buttonSpeichernPack windowMain box maybeTVar position = do
    dialogSpeichern <- dialogSpeichernNew windowMain maybeTVar
    statusVar <- erhalteStatusVar :: m StatusVarGui
    objektReader <- ask
    boxPackWidgetNew box packingDefault paddingDefault position
        $ buttonNewWithEventLabel maybeTVar Language.speichern
        $ do
            antwort <- dialogEval dialogSpeichern
            when (antwort == fromIntegral (fromEnum Gtk.ResponseTypeOk)) $ void $ do
                (Just dateipfad) <- Gtk.fileChooserGetFilename dialogSpeichern
                flip runReaderT objektReader
                    $ ausführenStatusVarBefehl (Speichern dateipfad) statusVar

dialogSpeichernNew :: (SpracheGuiReader r m, MonadIO m)
                   => Gtk.Window
                   -> Maybe TVarSprachewechselAktionen
                   -> m Gtk.FileChooserDialog
dialogSpeichernNew window maybeTVar = do
    (fileChooserDialog, buttonSpeichern, buttonAbbrechen) <- liftIO $ do
        fileChooserDialog <- Gtk.new Gtk.FileChooserDialog []
        Gtk.setWindowTransientFor fileChooserDialog window
        Gtk.setFileChooserAction fileChooserDialog Gtk.FileChooserActionSave
        Gtk.setFileChooserDoOverwriteConfirmation fileChooserDialog True
        buttonSpeichern <- Gtk.unsafeCastTo Gtk.Button
            =<< (Gtk.dialogAddButton fileChooserDialog Text.empty
                 $ fromIntegral
                 $ fromEnum Gtk.ResponseTypeOk)
        buttonAbbrechen <- Gtk.unsafeCastTo Gtk.Button
            =<< (Gtk.dialogAddButton fileChooserDialog Text.empty
                 $ fromIntegral
                 $ fromEnum Gtk.ResponseTypeCancel)
        pure (fileChooserDialog, buttonSpeichern, buttonAbbrechen)
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.setWindowTitle fileChooserDialog $ Language.speichern sprache
        Gtk.setButtonLabel buttonSpeichern $ Language.speichern sprache
        Gtk.setButtonLabel buttonAbbrechen $ Language.abbrechen sprache
    pure fileChooserDialog

-- | Laden eines neuen 'StatusGui' aus einer Datei.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonLadenPack :: (MitWindow p, MitBox b, ObjektGuiReader m, MonadIO m)
                => p
                -> b
                -> Maybe TVarSprachewechselAktionen
                -> Position
                -> m Gtk.Button
buttonLadenPack parent box maybeTVar position = do
    dialogLaden <- dialogLadenNew parent maybeTVar
    dialogLadenFehler <- dialogLadenFehlerNew parent maybeTVar
    statusVar <- erhalteStatusVar
    objektReader <- ask
    boxPackWidgetNew box packingDefault paddingDefault position
        $ buttonNewWithEventLabel maybeTVar Language.laden
        $ do
            antwort <- dialogEval dialogLaden
            when (antwort == fromIntegral (fromEnum Gtk.ResponseTypeOk)) $ void $ do
                Gtk.fileChooserGetFilename dialogLaden >>= \case
                    Nothing -> void $ do
                        spracheGui <- readSpracheGui statusVar
                        flip leseSprache spracheGui $ \sprache -> Gtk.setWindowTitle
                            dialogLadenFehler
                            $ Language.nichtGefundeneDatei sprache
                        dialogEval dialogLadenFehler
                    (Just dateipfad) -> void $ do
                        let ladeAktion :: Status -> IOStatusGui ()
                            ladeAktion statusNeu = do
                                state0 <- RWS.get
                                state1 <- liftIO
                                    $ flip runReaderT objektReader
                                    $ fst
                                    <$> RWS.execRWST (ladeWidgets statusNeu) objektReader state0
                                RWS.put state1
                            fehlerBehandlung :: IOStatusGui ()
                            fehlerBehandlung = void $ do
                                Gtk.setWindowTitle dialogLadenFehler $ Text.pack dateipfad
                                dialogEval dialogLadenFehler
                        flip runReaderT objektReader
                            $ ausführenStatusVarBefehl
                                (Laden dateipfad ladeAktion fehlerBehandlung)
                                statusVar

-- | Passe angezeigte Widgets (inkl. 'StatusGui') an reinen 'Status' an.
ladeWidgets :: (ObjektGuiReader m, MonadIO m) => Status -> MStatusGuiT m ()
ladeWidgets status = do
    löscheWidgets
    erstelleWidgets
    where
        löscheWidgets :: (DynamischeWidgetsReader r m, MonadIO m) => MStatusGuiT m ()
        löscheWidgets = do
            aktuellerStatus <- RWS.get
            mapM_ entferneWidgets $ bahngeschwindigkeiten aktuellerStatus
            mapM_ entferneWidgets $ streckenabschnitte aktuellerStatus
            mapM_ entferneWidgets $ weichen aktuellerStatus
            mapM_ entferneWidgets $ kupplungen aktuellerStatus
            mapM_ entferneWidgets $ kontakte aktuellerStatus
            mapM_ entferneWidgets $ wegstrecken aktuellerStatus
            mapM_ entferneWidgets $ pläne aktuellerStatus
            RWS.put $ statusLeer $ sprache aktuellerStatus

        erstelleWidgets :: (ObjektGuiReader m, MonadIO m) => MStatusGuiT m ()
        erstelleWidgets = do
            let packBG :: (ObjektGuiReader m, MonadIO m)
                       => ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)
                       -> MStatusGuiT m (ZugtypEither (GeschwindigkeitEither BGWidgets))
                packBG (ZugtypMärklin (GeschwindigkeitPwm bg)) =
                    ZugtypMärklin . GeschwindigkeitPwm <$> bahngeschwindigkeitPackNew bg
                packBG (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bg)) =
                    ZugtypMärklin . GeschwindigkeitKonstanteSpannung
                    <$> bahngeschwindigkeitPackNew bg
                packBG (ZugtypLego (GeschwindigkeitPwm bg)) =
                    ZugtypLego . GeschwindigkeitPwm <$> bahngeschwindigkeitPackNew bg
                packBG (ZugtypLego (GeschwindigkeitKonstanteSpannung bg)) =
                    ZugtypLego . GeschwindigkeitKonstanteSpannung <$> bahngeschwindigkeitPackNew bg
            mapM_ packBG $ reverse $bahngeschwindigkeiten status
            mapM_ streckenabschnittPackNew $ reverse $streckenabschnitte status
            let packWE :: (ObjektGuiReader m, MonadIO m)
                       => ZugtypEither Weiche
                       -> MStatusGuiT m (ZugtypEither WEWidgets)
                packWE (ZugtypMärklin we) = ZugtypMärklin <$> weichePackNew we
                packWE (ZugtypLego we) = ZugtypLego <$> weichePackNew we
            mapM_ packWE $ reverse $weichen status
            mapM_ kupplungPackNew $ reverse $kupplungen status
            mapM_ kontaktPackNew $ reverse $kontakte status
            let packWS :: (ObjektGuiReader m, MonadIO m)
                       => ZugtypEither Wegstrecke
                       -> MStatusGuiT m (ZugtypEither WSWidgets)
                packWS (ZugtypMärklin ws) = ZugtypMärklin <$> wegstreckePackNew ws
                packWS (ZugtypLego ws) = ZugtypLego <$> wegstreckePackNew ws
            mapM_ packWS $ reverse $wegstrecken status
            statusGui <- RWS.get
            -- TODO Pläne mit Dauerschleife scheitern hier!!!
            mapM_ planPackNew $ reverse $ catMaybes $ alsPlanGui statusGui <$> pläne status

alsPlanGui :: StatusGui -> Plan -> Maybe PlanGui
alsPlanGui statusGui Plan {plName, plAktionen} = mdo
    let result = Plan { plName, plAktionen = guiAktionen }
    guiAktionen <- mapM (konvertiereAktion result) plAktionen
    pure result
    where
        lookupWidgetsTyp
            :: (Eq (ObjektTyp a), ObjektElement a) => (StatusGui -> [a]) -> ObjektTyp a -> Maybe a
        lookupWidgetsTyp getter objektTyp = find ((==) objektTyp . zuObjektTyp) $getter statusGui

        konvertiereAktion :: PlanGui -> Aktion -> Maybe AktionGui
        konvertiereAktion _result (Warten wartezeit) = Just $ Warten wartezeit
        konvertiereAktion _result (ABahngeschwindigkeitMärklinPwm aktion) =
            ABahngeschwindigkeitMärklinPwm <$> konvertiereAktionBahngeschwindigkeit aktion
        konvertiereAktion _result (ABahngeschwindigkeitMärklinKonstanteSpannung aktion) =
            ABahngeschwindigkeitMärklinKonstanteSpannung
            <$> konvertiereAktionBahngeschwindigkeit aktion
        konvertiereAktion _result (ABahngeschwindigkeitLegoPwm aktion) =
            ABahngeschwindigkeitLegoPwm <$> konvertiereAktionBahngeschwindigkeit aktion
        konvertiereAktion _result (ABahngeschwindigkeitLegoKonstanteSpannung aktion) =
            ABahngeschwindigkeitLegoKonstanteSpannung
            <$> konvertiereAktionBahngeschwindigkeit aktion
        konvertiereAktion _result (AStreckenabschnitt (Strom st fließend)) =
            AStreckenabschnitt . flip Strom fließend <$> lookupWidgetsTyp streckenabschnitte st
        konvertiereAktion _result (AWeiche (Stellen we richtung)) =
            AWeiche . flip Stellen richtung <$> lookupWidgetsTyp weichen we
        konvertiereAktion _result (AKupplung (Kuppeln ku)) =
            AKupplung . Kuppeln <$> lookupWidgetsTyp kupplungen ku
        konvertiereAktion _result (AKontakt (WartenAuf ko)) =
            AKontakt . WartenAuf <$> lookupWidgetsTyp kontakte ko
        konvertiereAktion _result (AWegstreckeMärklin aktion) =
            AWegstreckeMärklin <$> konvertiereAktionWegstrecke aktion
        konvertiereAktion _result (AWegstreckeLego aktion) =
            AWegstreckeLego <$> konvertiereAktionWegstrecke aktion
        konvertiereAktion result (AktionAusführen pl@Plan {plName = plName0}) =
            case AktionAusführen . planGui <$> lookupWidgetsTyp pläne pl of
                justAktion@(Just _aktionGui) -> justAktion
                Nothing
                    | plName == plName0 -> Just $ AktionAusführen result
                    | otherwise -> Nothing

        lookupBG :: (ZugtypKlasse z, GeschwindigkeitKlasse g)
                 => Bahngeschwindigkeit g z
                 -> Maybe (BGWidgets g z)
        lookupBG bg = do
            bgZugtypEither <- lookupWidgetsTyp bahngeschwindigkeiten
                $ zuZugtypEither
                $ zuGeschwindigkeitEither bg
            bgGeschwindigkeitEither <- vonZugtypEither bgZugtypEither
            vonGeschwindigkeitEither bgGeschwindigkeitEither

        konvertiereAktionBahngeschwindigkeit :: (ZugtypKlasse z, GeschwindigkeitKlasse g)
                                             => AktionBahngeschwindigkeit Bahngeschwindigkeit g z
                                             -> Maybe (AktionBahngeschwindigkeit BGWidgets g z)
        konvertiereAktionBahngeschwindigkeit (Geschwindigkeit bg wert) =
            flip Geschwindigkeit wert <$> lookupBG bg
        konvertiereAktionBahngeschwindigkeit (Fahrstrom bg wert) =
            flip Fahrstrom wert <$> lookupBG bg
        konvertiereAktionBahngeschwindigkeit (Umdrehen bg) = Umdrehen <$> lookupBG bg
        konvertiereAktionBahngeschwindigkeit (FahrtrichtungEinstellen bg fahrtrichtung) =
            flip FahrtrichtungEinstellen fahrtrichtung <$> lookupBG bg

        lookupWS :: (ZugtypKlasse z) => Wegstrecke z -> Maybe (WSWidgets z)
        lookupWS ws = do
            wsZugtypEither <- lookupWidgetsTyp wegstrecken $ zuZugtypEither ws
            vonZugtypEither wsZugtypEither

        konvertiereAktionWegstrecke :: (ZugtypKlasse z)
                                    => AktionWegstrecke Wegstrecke z
                                    -> Maybe (AktionWegstrecke WSWidgets z)
        konvertiereAktionWegstrecke (Einstellen ws) = Einstellen <$> lookupWS ws
        konvertiereAktionWegstrecke
            (AWSBahngeschwindigkeit
                 (GeschwindigkeitPwm (Geschwindigkeit (GeschwindigkeitPhantom ws) wert))) =
            AWSBahngeschwindigkeit
            . GeschwindigkeitPwm
            . flip Geschwindigkeit wert
            . GeschwindigkeitPhantom
            <$> lookupWS ws
        konvertiereAktionWegstrecke
            (AWSBahngeschwindigkeit
                 (GeschwindigkeitKonstanteSpannung (Fahrstrom (GeschwindigkeitPhantom ws) wert))) =
            AWSBahngeschwindigkeit
            . GeschwindigkeitKonstanteSpannung
            . flip Fahrstrom wert
            . GeschwindigkeitPhantom
            <$> lookupWS ws
        konvertiereAktionWegstrecke
            (AWSBahngeschwindigkeit (GeschwindigkeitPwm (Umdrehen (GeschwindigkeitPhantom ws)))) =
            AWSBahngeschwindigkeit . GeschwindigkeitPwm . Umdrehen . GeschwindigkeitPhantom
            <$> lookupWS ws
        konvertiereAktionWegstrecke
            (AWSBahngeschwindigkeit
                 (GeschwindigkeitKonstanteSpannung (Umdrehen (GeschwindigkeitPhantom ws)))) =
            AWSBahngeschwindigkeit
            . GeschwindigkeitKonstanteSpannung
            . Umdrehen
            . GeschwindigkeitPhantom
            <$> lookupWS ws
        konvertiereAktionWegstrecke
            (AWSBahngeschwindigkeit
                 (GeschwindigkeitPwm
                      (FahrtrichtungEinstellen (GeschwindigkeitPhantom ws) fahrtrichtung))) =
            AWSBahngeschwindigkeit
            . GeschwindigkeitPwm
            . flip FahrtrichtungEinstellen fahrtrichtung
            . GeschwindigkeitPhantom
            <$> lookupWS ws
        konvertiereAktionWegstrecke
            (AWSBahngeschwindigkeit
                 (GeschwindigkeitKonstanteSpannung
                      (FahrtrichtungEinstellen (GeschwindigkeitPhantom ws) fahrtrichtung))) =
            AWSBahngeschwindigkeit
            . GeschwindigkeitKonstanteSpannung
            . flip FahrtrichtungEinstellen fahrtrichtung
            . GeschwindigkeitPhantom
            <$> lookupWS ws
        konvertiereAktionWegstrecke (AWSStreckenabschnitt (Strom ws fließend)) =
            AWSStreckenabschnitt . flip Strom fließend <$> lookupWS ws
        konvertiereAktionWegstrecke (AWSKupplung (Kuppeln ws)) =
            AWSKupplung . Kuppeln <$> lookupWS ws
        konvertiereAktionWegstrecke (AWSKontakt (WartenAuf ws)) =
            AWSKontakt . WartenAuf <$> lookupWS ws

dialogLadenNew :: (MitWindow p, SpracheGuiReader r m, MonadIO m)
               => p
               -> Maybe TVarSprachewechselAktionen
               -> m Gtk.FileChooserDialog
dialogLadenNew parent maybeTVar = do
    (dialog, buttonLaden, buttonAbbrechen) <- liftIO $ do
        parentWindow <- erhalteWindow parent
        dialog <- Gtk.new Gtk.FileChooserDialog []
        Gtk.setWindowTransientFor dialog parentWindow
        Gtk.setFileChooserAction dialog Gtk.FileChooserActionOpen
        buttonLaden <- Gtk.unsafeCastTo Gtk.Button
            =<< (Gtk.dialogAddButton dialog Text.empty $ fromIntegral $ fromEnum Gtk.ResponseTypeOk)
        buttonAbbrechen <- Gtk.unsafeCastTo Gtk.Button
            =<< (Gtk.dialogAddButton dialog Text.empty
                 $ fromIntegral
                 $ fromEnum Gtk.ResponseTypeCancel)
        pure (dialog, buttonLaden, buttonAbbrechen)
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.setWindowTitle dialog $ Language.laden sprache
        Gtk.setButtonLabel buttonLaden $ Language.laden sprache
        Gtk.setButtonLabel buttonAbbrechen $ Language.abbrechen sprache
    pure dialog

dialogLadenFehlerNew :: (MitWindow p, SpracheGuiReader r m, MonadIO m)
                     => p
                     -> Maybe TVarSprachewechselAktionen
                     -> m Gtk.MessageDialog
dialogLadenFehlerNew parent maybeTVar = do
    parentWindow <- erhalteWindow parent
    dialog <- Gtk.new Gtk.MessageDialog []
    Gtk.setWindowTransientFor dialog parentWindow
    Gtk.setMessageDialogMessageType dialog Gtk.MessageTypeError
    buttonOk <- liftIO $ Gtk.unsafeCastTo Gtk.Button =<< Gtk.dialogAddButton dialog Text.empty 0
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.setWindowTitle dialog $ Language.nichtGefundeneDatei <!> Text.empty $ sprache
        Gtk.setButtonLabel buttonOk $ Language.ok sprache
    pure dialog

-- | Hinzufügen eines 'StreckenObjekt'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonHinzufügenPack
    :: (MitWindow p, MitBox b, ObjektGuiReader m, MonadFix m, MonadIO m, MonadFail m)
    => p
    -> b
    -> Maybe TVarSprachewechselAktionen
    -> m ( Gtk.Button
         , Objekt
               -> IO ()
         )
buttonHinzufügenPack parentWindow box maybeTVar = do
    tmvarAssistantHinzufügen <- liftIO $ atomically $ newTMVar Nothing
    objektReader <- ask
    statusVar <- erhalteStatusVar
    let erzeugeAssistantHinzufügen
            :: (ObjektGuiReader m, MonadFix m, MonadIO m, MonadFail m) => m AssistantHinzufügen
        erzeugeAssistantHinzufügen = do
            -- erzeuge AssistantHinzufügen nur, wenn er benötigt wird
            assistantHinzufügen
                <- (liftIO $ atomically $ takeTMVar tmvarAssistantHinzufügen) >>= \case
                    (Just assistantHinzufügen) -> pure assistantHinzufügen
                    Nothing -> assistantHinzufügenNew parentWindow maybeTVar
            liftIO $ atomically $ putTMVar tmvarAssistantHinzufügen $ Just assistantHinzufügen
            pure assistantHinzufügen
        assistantAuswerten :: (ObjektGuiReader m, MonadFix m, MonadIO m, MonadFail m) => m ()
        assistantAuswerten = do
            assistantHinzufügen <- erzeugeAssistantHinzufügen
            assistantHinzufügenAuswerten assistantHinzufügen
                >>= flip auswertenStatusVarMStatusT statusVar . \case
                    (HinzufügenErfolgreich
                         (OBahngeschwindigkeit (ZugtypMärklin (GeschwindigkeitPwm bgMärklinPwm))))
                        -> void $ bahngeschwindigkeitPackNew bgMärklinPwm
                    (HinzufügenErfolgreich
                         (OBahngeschwindigkeit
                              (ZugtypMärklin
                                   (GeschwindigkeitKonstanteSpannung bgMärklinKonstanteSpannung))))
                        -> void $ bahngeschwindigkeitPackNew bgMärklinKonstanteSpannung
                    (HinzufügenErfolgreich
                         (OBahngeschwindigkeit (ZugtypLego (GeschwindigkeitPwm bgLegoPwm))))
                        -> void $ bahngeschwindigkeitPackNew bgLegoPwm
                    (HinzufügenErfolgreich
                         (OBahngeschwindigkeit
                              (ZugtypLego
                                   (GeschwindigkeitKonstanteSpannung bgLegoKonstanteSpannung))))
                        -> void $ bahngeschwindigkeitPackNew bgLegoKonstanteSpannung
                    (HinzufügenErfolgreich (OStreckenabschnitt st))
                        -> void $ streckenabschnittPackNew st
                    (HinzufügenErfolgreich (OWeiche (ZugtypMärklin weMärklin)))
                        -> void $ weichePackNew weMärklin
                    (HinzufügenErfolgreich (OWeiche (ZugtypLego weLego)))
                        -> void $ weichePackNew weLego
                    (HinzufügenErfolgreich (OKupplung ku)) -> void $ kupplungPackNew ku
                    (HinzufügenErfolgreich (OKontakt ko)) -> void $ kontaktPackNew ko
                    (HinzufügenErfolgreich (OWegstrecke (ZugtypMärklin wsMärklin)))
                        -> void $ wegstreckePackNew wsMärklin
                    (HinzufügenErfolgreich (OWegstrecke (ZugtypLego wsLego)))
                        -> void $ wegstreckePackNew wsLego
                    (HinzufügenErfolgreich (OPlan pl)) -> void $ do
                        statusGui <- RWS.get
                        case alsPlanGui statusGui pl of
                            (Just plGui) -> planPackNew plGui
                            Nothing -> error
                                $ "Konvertieren einen neu erstellten Plans fehlgeschlagen: "
                                ++ show pl
                    -- Kein catch-all Pattern um Fehlermeldung des Compilers
                    -- bei neu hinzugefügten Objekten nicht zu verpassen
                    HinzufügenBeenden -> pure ()
                    HinzufügenAbbrechen -> pure ()
        assistantBearbeiten
            :: (ObjektGuiReader m, MonadFix m, MonadIO m, MonadFail m) => Objekt -> m ()
        assistantBearbeiten objekt = do
            assistantHinzufügen <- erzeugeAssistantHinzufügen
            setzeAssistantHinzufügen assistantHinzufügen objekt
    button <- boxPackWidgetNewDefault box
        $ buttonNewWithEventLabel maybeTVar Language.hinzufügen
        $ void
        $ forkIO
        $ flip runReaderT objektReader
        $ assistantAuswerten
    pure (button, \objekt -> void $ do
        flip runReaderT objektReader $ assistantBearbeiten objekt
        forkIO $ runReaderT assistantAuswerten objektReader)
#endif
--
