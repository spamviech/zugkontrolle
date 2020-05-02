{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Concurrent.STM (atomically, TVar, newTMVar, takeTMVar, putTMVar)
import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.Fix (MonadFix())
import qualified Control.Monad.RWS.Strict as RWS
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (Bahngeschwindigkeit(..), Weiche(..), Wegstrecke(..))
import Zug.Enums (ZugtypEither(..), GeschwindigkeitEither(..))
import qualified Zug.Language as Language
import Zug.Language (Sprache(), MitSprache(..), (<!>))
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.UI.Base (Status, bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, kontakte
                  , wegstrecken, pläne, sprache, statusLeer)
import Zug.UI.Befehl (BefehlAllgemein(..))
import Zug.UI.Gtk.AssistantHinzufuegen
       (AssistantHinzufügen, assistantHinzufügenNew, assistantHinzufügenAuswerten
      , HinzufügenErgebnis(..), setzeAssistantHinzufügen)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, boxPackWidgetNew, packingDefault
                                 , paddingDefault, Position(), buttonNewWithEventLabel, dialogEval)
import Zug.UI.Gtk.Klassen (MitBox(..), MitWindow(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt
       (MStatusGuiT, IOStatusGui, ObjektGuiReader, StatusVarGui, readSpracheGui
      , DynamischeWidgetsReader(..), WidgetsTyp(..), bahngeschwindigkeitPackNew, BGWidgets
      , streckenabschnittPackNew, weichePackNew, WEWidgets, kupplungPackNew, kontaktPackNew
      , wegstreckePackNew, WSWidgets, planPackNew)
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
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
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
            when (antwort == Gtk.ResponseOk) $ void $ do
                (Just dateipfad) <- Gtk.fileChooserGetFilename dialogSpeichern
                flip runReaderT objektReader
                    $ ausführenStatusVarBefehl (Speichern dateipfad) statusVar

dialogSpeichernNew :: (SpracheGuiReader r m, MonadIO m)
                   => Gtk.Window
                   -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                   -> m Gtk.FileChooserDialog
dialogSpeichernNew window maybeTVar = do
    (fileChooserDialog, buttonSpeichern, buttonAbbrechen) <- liftIO $ do
        fileChooserDialog <- Gtk.fileChooserDialogNew
            (Nothing :: Maybe Text)
            (Just window)
            Gtk.FileChooserActionSave
            []
        Gtk.set fileChooserDialog [Gtk.fileChooserDoOverwriteConfirmation := True]
        buttonSpeichern <- Gtk.dialogAddButton fileChooserDialog Text.empty Gtk.ResponseOk
        buttonAbbrechen <- Gtk.dialogAddButton fileChooserDialog Text.empty Gtk.ResponseCancel
        pure (fileChooserDialog, buttonSpeichern, buttonAbbrechen)
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.set fileChooserDialog [Gtk.windowTitle := Language.speichern sprache]
        Gtk.set buttonSpeichern [Gtk.buttonLabel := Language.speichern sprache]
        Gtk.set buttonAbbrechen [Gtk.buttonLabel := Language.abbrechen sprache]
    pure fileChooserDialog

-- | Laden eines neuen 'StatusGui' aus einer Datei.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonLadenPack :: (MitWindow p, MitBox b, ObjektGuiReader m, MonadIO m)
                => p
                -> b
                -> Maybe (TVar (Maybe [Sprache -> IO ()]))
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
            when (antwort == Gtk.ResponseOk) $ void $ do
                Gtk.fileChooserGetFilename dialogLaden >>= \case
                    Nothing -> void $ do
                        spracheGui <- readSpracheGui statusVar
                        Gtk.set
                            dialogLadenFehler
                            [ Gtk.windowTitle
                                  := leseSprache Language.nichtGefundeneDatei spracheGui]
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
                            fehlerBehandlung = liftIO $ void $ do
                                Gtk.set dialogLadenFehler [Gtk.windowTitle := dateipfad]
                                dialogEval dialogLadenFehler
                        flip runReaderT objektReader
                            $ ausführenStatusVarBefehl
                                (Laden dateipfad ladeAktion fehlerBehandlung)
                                statusVar

-- | Passe angezeigte Widgets (inkl. 'StatusGui') an reinen 'Status' an.
ladeWidgets :: (ObjektGuiReader m, MonadIO m) => Status -> MStatusGuiT m ()
ladeWidgets status = do
    löscheWidgets
    erstelleWidgets status
    where
        löscheWidgets :: (DynamischeWidgetsReader r m, MonadIO m) => MStatusGuiT m ()
        löscheWidgets = do
            status <- RWS.get
            mapM_ entferneWidgets $ status ^. bahngeschwindigkeiten
            mapM_ entferneWidgets $ status ^. streckenabschnitte
            mapM_ entferneWidgets $ status ^. weichen
            mapM_ entferneWidgets $ status ^. kupplungen
            mapM_ entferneWidgets $ status ^. kontakte
            mapM_ entferneWidgets $ status ^. wegstrecken
            mapM_ entferneWidgets $ status ^. pläne
            RWS.put $ statusLeer $ status ^. sprache

        erstelleWidgets :: (ObjektGuiReader m, MonadIO m) => Status -> MStatusGuiT m ()
        erstelleWidgets status = do
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
            mapM_ packBG $ reverse $ status ^. bahngeschwindigkeiten
            mapM_ streckenabschnittPackNew $ reverse $ status ^. streckenabschnitte
            let packWE :: (ObjektGuiReader m, MonadIO m)
                       => ZugtypEither Weiche
                       -> MStatusGuiT m (ZugtypEither WEWidgets)
                packWE (ZugtypMärklin we) = ZugtypMärklin <$> weichePackNew we
                packWE (ZugtypLego we) = ZugtypLego <$> weichePackNew we
            mapM_ packWE $ reverse $ status ^. weichen
            mapM_ kupplungPackNew $ reverse $ status ^. kupplungen
            mapM_ kontaktPackNew $ reverse $ status ^. kontakte
            let packWS :: (ObjektGuiReader m, MonadIO m)
                       => ZugtypEither Wegstrecke
                       -> MStatusGuiT m (ZugtypEither WSWidgets)
                packWS (ZugtypMärklin ws) = ZugtypMärklin <$> wegstreckePackNew ws
                packWS (ZugtypLego ws) = ZugtypLego <$> wegstreckePackNew ws
            mapM_ packWS $ reverse $ status ^. wegstrecken
            mapM_ planPackNew $ reverse $ status ^. pläne

dialogLadenNew :: (MitWindow p, SpracheGuiReader r m, MonadIO m)
               => p
               -> Maybe (TVar (Maybe [Sprache -> IO ()]))
               -> m Gtk.FileChooserDialog
dialogLadenNew parent maybeTVar = do
    (dialog, buttonLaden, buttonAbbrechen) <- liftIO $ do
        dialog <- Gtk.fileChooserDialogNew
            (Nothing :: Maybe Text)
            (Just $ erhalteWindow parent)
            Gtk.FileChooserActionOpen
            []
        buttonLaden <- Gtk.dialogAddButton dialog Text.empty Gtk.ResponseOk
        buttonAbbrechen <- Gtk.dialogAddButton dialog Text.empty Gtk.ResponseCancel
        pure (dialog, buttonLaden, buttonAbbrechen)
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.set dialog [Gtk.windowTitle := Language.laden sprache]
        Gtk.set buttonLaden [Gtk.buttonLabel := Language.laden sprache]
        Gtk.set buttonAbbrechen [Gtk.buttonLabel := Language.abbrechen sprache]
    pure dialog

dialogLadenFehlerNew :: (MitWindow p, SpracheGuiReader r m, MonadIO m)
                     => p
                     -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                     -> m Gtk.MessageDialog
dialogLadenFehlerNew parent maybeTVar = do
    dialog <- liftIO
        $ Gtk.messageDialogNew
            (Just $ erhalteWindow parent)
            []
            Gtk.MessageError
            Gtk.ButtonsOk
            Text.empty
    verwendeSpracheGui maybeTVar $ \sprache -> Gtk.set
        dialog
        [Gtk.windowTitle := (Language.nichtGefundeneDatei <!> Text.empty) sprache]
    pure dialog

-- | Hinzufügen eines 'StreckenObjekt'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonHinzufügenPack
    :: (MitWindow p, MitBox b, ObjektGuiReader m, MonadFix m, MonadIO m)
    => p
    -> b
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> m ( Gtk.Button
         , Objekt
               -> IO ()
         )
buttonHinzufügenPack parentWindow box maybeTVar = do
    tmvarAssistantHinzufügen <- liftIO $ atomically $ newTMVar Nothing
    objektReader <- ask
    statusVar <- erhalteStatusVar
    let erzeugeAssistantHinzufügen :: (ObjektGuiReader m, MonadFix m, MonadIO m) => m AssistantHinzufügen
        erzeugeAssistantHinzufügen =
            -- erzeuge AssistantHinzufügen nur, wenn er benötigt wird
            liftIO (atomically $ takeTMVar tmvarAssistantHinzufügen) >>= \case
                (Just assistantHinzufügen) -> pure assistantHinzufügen
                Nothing -> do
                    assistantHinzufügen <- assistantHinzufügenNew parentWindow maybeTVar
                    liftIO
                        $ atomically
                        $ putTMVar tmvarAssistantHinzufügen
                        $ Just assistantHinzufügen
                    pure assistantHinzufügen
        assistantAuswerten :: (ObjektGuiReader m, MonadFix m, MonadIO m) => m ()
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
                    (HinzufügenErfolgreich (OPlan pl)) -> void $ planPackNew pl
                    -- Kein catch-all Pattern um Fehlermeldung des Compilers
                    -- bei neu hinzugefügten Objekten nicht zu verpassen
                    HinzufügenBeenden -> pure ()
                    HinzufügenAbbrechen -> pure ()
        assistantBearbeiten :: (ObjektGuiReader m, MonadFix m, MonadIO m) => Objekt -> m ()
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
