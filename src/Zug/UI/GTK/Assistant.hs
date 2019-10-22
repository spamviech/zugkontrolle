{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}

{-|
Description : Eigenes Assistant-Widget, nachdem das von Gtk bereitgestellte nicht funktioniert.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Assistant () where
#else
module Zug.UI.Gtk.Assistant (
    Assistant(), AssistantSeite(..), SeitenAbschluss(..), AssistantSeitenBaum(..),
    assistantNew, assistantAuswerten, AssistantResult(..)) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, retry, STM, TVar, newTVarIO, readTVarIO, readTVar, writeTVar, modifyTVar)
import Data.Maybe (catMaybes)
import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO(..))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import qualified Zug.Language as Language
import Zug.UI.Gtk.Auswahl (AuswahlWidget, auswahlRadioButtonNamedNew, aktuelleAuswahl)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggled, FortfahrenWennToggledTMVar)
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNew, boxPackWidgetNewDefault, boxPack, boxPackDefault,
                                    Packing(..), packingDefault, Position(..), paddingDefault,
                                    buttonNewWithEventLabel, widgetShowIf)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitWidgetShow, mitWidgetHide, MitButton(..),
                            MitContainer(..), MitBox(..), MitWindow(..))
import Zug.UI.Gtk.StreckenObjekt (StatusGui, WegstreckeCheckButtonVoid)

-- | Fenster zum erstellen eines Wertes, potentiell in mehreren Schritten
data Assistant w a
    = Assistant {
        fenster :: Gtk.Window,
        seiten :: AssistantSeitenBaumPacked w,
        tvarAuswahl :: TVar (Either ([AssistantSeitenBaumPacked w], AssistantSeitenBaumPacked w) (AssistantResult (NonEmpty w))),
        auswertFunktion :: NonEmpty w -> IO a}

instance MitWidget (Assistant w a) where
    erhalteWidget :: Assistant w a -> Gtk.Widget
    erhalteWidget = erhalteWidget . fenster

instance MitContainer (Assistant w a) where
    erhalteContainer :: Assistant w a -> Gtk.Container
    erhalteContainer = erhalteContainer . fenster

instance MitWindow (Assistant w a) where
    erhalteWindow :: Assistant w a -> Gtk.Window
    erhalteWindow = fenster

-- | Der Text des Knopfs zum Anzeigen der nächsten Seite/Finalisieren des 'Assistant'.
-- Alternativ können bereits erstellte spezielle Knopf-Varianten übergeben werden.
data SeitenAbschluss
    = SeitenAbschluss
        Text
    | SeitenAbschlussToggled
        FortfahrenWennToggled
    | SeitenAbschlussToggledTMVar
        (FortfahrenWennToggledTMVar StatusGui WegstreckeCheckButtonVoid)
    deriving (Eq)

-- | Seite eines 'Assistant'.
-- Der Name wird im Titel des 'Assistant' und bei der Auswahl der Nachfolgerseite angezeigt.
data AssistantSeite w
    = AssistantSeite {
        seite :: w,
        name :: Text,
        seitenAbschluss :: SeitenAbschluss}
            deriving (Eq, Functor)

instance (MitWidget w) => MitWidget (AssistantSeite w) where
    erhalteWidget :: AssistantSeite w -> Gtk.Widget
    erhalteWidget = erhalteWidget . seite

-- | Seiten eines 'Assistant'.
-- Die Form ist wie ein Rose-tree, ergänzt um zusätzliche Informationen.
data AssistantSeitenBaum w
    -- | Seite mit einem direkten Nachfolger
    = AssistantSeiteLinear {
        node :: AssistantSeite w,
        nachfolger :: AssistantSeitenBaum w}
    -- | Seite mit meheren direkten Nachfolger
    | AssistantSeiteAuswahl {
        node :: AssistantSeite w,
        nachfolgerFrage :: Text,
        nachfolgerListe :: NonEmpty (AssistantSeitenBaum w)}
    -- | Seite ohne Nachfolger
    | AssistantSeiteLetzte {
        node :: AssistantSeite w}

instance (MitWidget w) => MitWidget (AssistantSeitenBaum w) where
    erhalteWidget :: AssistantSeitenBaum w -> Gtk.Widget
    erhalteWidget = erhalteWidget . node

-- | Darstellung des 'AssistantSeitenBaum's, inklusiver zugehörigem 'AuswahlWidget'
data AssistantSeitenBaumPacked w
    -- | Seite mit einem direkten Nachfolger
    = PackedSeiteLinear {
        packedNode :: AssistantSeite w,
        packedNachfolger :: AssistantSeitenBaumPacked w}
    -- | Seite mit meheren direkten Nachfolger
    | PackedSeiteAuswahl {
        packedNode :: AssistantSeite w,
        packedNachfolgerFrage :: Text,
        packedNachfolgerListe :: NonEmpty (AssistantSeitenBaumPacked w),
        packedBox :: Gtk.Box,
        packedNachfolgerAuswahl :: AuswahlWidget (AssistantSeite w)}
    -- | Seite ohne Nachfolger
    | PackedSeiteLetzte {
        packedNode :: AssistantSeite w}

instance (MitWidget w) => MitWidget (AssistantSeitenBaumPacked w) where
    erhalteWidget :: AssistantSeitenBaumPacked w -> Gtk.Widget
    erhalteWidget   PackedSeiteLinear {packedNode}  = erhalteWidget packedNode
    erhalteWidget   PackedSeiteAuswahl {packedBox}  = erhalteWidget packedBox
    erhalteWidget   PackedSeiteLetzte {packedNode}  = erhalteWidget packedNode

besondereSeitenAbschlüsse :: AssistantSeitenBaumPacked w -> [Maybe Gtk.Button]
besondereSeitenAbschlüsse
    PackedSeiteLinear {packedNode, packedNachfolger}
        = besondererSeitenAbschluss packedNode : besondereSeitenAbschlüsse packedNachfolger
besondereSeitenAbschlüsse
    PackedSeiteAuswahl {packedNode, packedNachfolgerListe}
        = besondererSeitenAbschluss packedNode : concat (besondereSeitenAbschlüsse <$> packedNachfolgerListe)
besondereSeitenAbschlüsse
    PackedSeiteLetzte {packedNode}
        = [besondererSeitenAbschluss packedNode]

besondererSeitenAbschluss :: AssistantSeite w -> Maybe Gtk.Button
besondererSeitenAbschluss
    AssistantSeite {seitenAbschluss = (SeitenAbschlussToggled fortfahrenWennToggled)}
        = Just $ erhalteButton fortfahrenWennToggled
besondererSeitenAbschluss
    AssistantSeite {seitenAbschluss = (SeitenAbschlussToggledTMVar fortfahrenWennToggledTMVar)}
        = Just $ erhalteButton fortfahrenWennToggledTMVar
besondererSeitenAbschluss
    AssistantSeite {seitenAbschluss = (SeitenAbschluss _text)}
        = Nothing

-- | Erstelle einen neuen 'Assistant'.
-- Die /globalenWidgets/ werden permanent in der Fußleiste mit dem /Weiter/-Knopf (etc.) angezeigt.
-- Die /seiten/ werden in 'Tree'-Reihenfolge von Wurzel zu Blatt angezeigt.
-- Wenn es mehr als einen Nachfolgeknoten gibt wird der Nutzer gefragt, welcher als nächster gezeigt werden soll.
-- Falls es bereitgestellt wird ersetzt dass mitgelieferte 'Gtk.Label' dabei die Standard-Frage.
-- Existiert kein Nachfolgeknoten wird der /Weiter/-Knopf durch einen /abschlussAktion/-benannten Knopf ersetzt.
--
-- Die /auswertFunktion/ wird gespeichert und durch 'assistantAuswerten' aufgerufen.
-- Sie erhält als Argument die ausgewählten /seiten/.
assistantNew :: (MonadIO m, MitWidget w, Eq w, MitWidget g, MitWindow p) =>
    p -> [g] -> AssistantSeitenBaum w -> (NonEmpty w -> IO a) -> m (Assistant w a)
assistantNew parent globaleWidgets seitenEingabe auswertFunktion = liftIO $ do
    -- Erstelle Fenster
    fenster <- Gtk.windowNew
    Gtk.set fenster [
        Gtk.windowTransientFor := erhalteWindow parent,
        Gtk.windowModal := True,
        Gtk.windowTitle := name (node seitenEingabe)]
    vBox <- containerAddWidgetNew fenster $ Gtk.vBoxNew False 0
    -- Packe Seiten in entsprechende Box
    flowControlBox <- boxPackWidgetNew vBox PackNatural paddingDefault End Gtk.hButtonBoxNew
    seiten <- packSeiten vBox flowControlBox seitenEingabe
    tvarAuswahl <- newTVarIO $ Left ([], seiten)
    tvarAktuelleSeite <- newTVarIO seiten
    -- Füge Reaktion auf beenden des Assistant durch 'X' in der Titelleiste hinzu
    Gtk.on fenster Gtk.deleteEvent $ liftIO $ do
        atomically $ writeTVar tvarAuswahl $ Right AssistantBeenden
        pure True
    -- Knopf-Leiste für permanente Funktionen
    boxPackWidgetNew flowControlBox packingDefault paddingDefault End $
        buttonNewWithEventLabel Language.abbrechen $ atomically $ writeTVar tvarAuswahl $ Right AssistantAbbrechen
    zurückKnopf <- boxPackWidgetNew flowControlBox packingDefault  paddingDefault End $
        Gtk.buttonNewWithLabel (Language.zurück :: Text)
    Gtk.widgetHide zurückKnopf
    seitenAbschlussKnopf <- boxPackWidgetNew flowControlBox packingDefault paddingDefault End $
        Gtk.buttonNewWithLabel (Language.weiter :: Text)
    -- Füge Reaktion auf drücken des Vorwärts- und Zurück-Knopfes hinzu
    Gtk.on zurückKnopf Gtk.buttonActivated $ do
        aktuelleSeite <- readTVarIO tvarAktuelleSeite
        mitWidgetHide aktuelleSeite
        maybeLetzteSeite <- atomically $ do
            auswahl <- readTVar tvarAuswahl
            case auswahl of
                (Left ((letzteSeite : besuchteSeiten), _aktuelleSeite))
                    -> do
                        writeTVar tvarAuswahl $ Left (besuchteSeiten, letzteSeite)
                        pure $ Just letzteSeite
                _otherwise
                    -> pure Nothing
        case maybeLetzteSeite of
            (Just letzteSeite)
                -> do
                    widgetShowIf (node seitenEingabe /= packedNode letzteSeite) zurückKnopf
                    zeigeSeite seitenAbschlussKnopf tvarAktuelleSeite letzteSeite
            Nothing
                -> error "Zurück-Knopf an unerwarteter Stelle gedrückt."
    let 
        seitenAbschlussAktion :: IO ()
        seitenAbschlussAktion = do
            aktuelleSeite <- readTVarIO tvarAktuelleSeite
            mitWidgetHide aktuelleSeite
            case seitenAbschluss $ packedNode aktuelleSeite of
                (SeitenAbschluss _name)
                    -> pure ()
                (SeitenAbschlussToggled fortfahrenWennToggled)
                    -> mitWidgetHide fortfahrenWennToggled
                (SeitenAbschlussToggledTMVar fortfahrenWennToggledTMVar)
                    -> mitWidgetHide fortfahrenWennToggledTMVar
            mitWidgetShow zurückKnopf
            case aktuelleSeite of
                PackedSeiteLinear {packedNachfolger}
                    -> do
                        atomically $ modifyTVar tvarAuswahl $ \case
                            (Left (besuchteSeiten, _aktuelleSeite))
                                -> Left $ (aktuelleSeite :  besuchteSeiten, packedNachfolger)
                            ergebnis
                                -> ergebnis
                        zeigeSeite seitenAbschlussKnopf tvarAktuelleSeite packedNachfolger
                PackedSeiteAuswahl {packedNachfolgerListe, packedNachfolgerAuswahl}
                    -> do
                        nachfolgerSeite <- aktuelleAuswahl packedNachfolgerAuswahl
                        let packedNachfolger = case find ((==) nachfolgerSeite . packedNode) packedNachfolgerListe of
                                (Just packedNachfolger) -> packedNachfolger
                                Nothing                 -> error "unbekannte Seite bei AuswahlWidget ausgewählt."
                        atomically $ modifyTVar tvarAuswahl $ \case
                            (Left (besuchteSeiten, _aktuelleSeite))
                                -> Left $ (aktuelleSeite :  besuchteSeiten, packedNachfolger)
                            ergebnis
                                -> ergebnis
                        zeigeSeite seitenAbschlussKnopf tvarAktuelleSeite $ packedNachfolger
                assistantSeite@PackedSeiteLetzte {}
                    -> do
                        atomically $ modifyTVar tvarAuswahl $ \case
                            (Left (besuchteSeiten, _aktuelleSeite))
                                -> Right $ AssistantErfolgreich $ NonEmpty.reverse $
                                    (seite $ packedNode assistantSeite) :| (seite . packedNode <$> besuchteSeiten)
                            ergebnis
                                -> ergebnis
                        -- Zeige erste Seite (für nächsten Assistant-Aufruf)
                        mitWidgetShow seiten
    Gtk.on seitenAbschlussKnopf Gtk.buttonActivated seitenAbschlussAktion
    forM_ (catMaybes $ besondereSeitenAbschlüsse seiten) $
        \knopf -> Gtk.on knopf Gtk.buttonActivated seitenAbschlussAktion
    -- Füge permanente Widgets zur FlowControlBox hinzu und zeige sie an
    forM_ globaleWidgets $ \widget -> do
        boxPack flowControlBox widget packingDefault paddingDefault Start
        mitWidgetShow widget
    -- Zeige erste Seite an
    zeigeSeite seitenAbschlussKnopf tvarAktuelleSeite seiten
    pure Assistant {fenster, seiten, tvarAuswahl, auswertFunktion}

packSeiten :: (MitBox b, MitWidget w, Eq w, MonadIO m) =>
    b -> Gtk.HButtonBox -> AssistantSeitenBaum w -> m (AssistantSeitenBaumPacked w)
packSeiten
    box
    flowControlBox
    AssistantSeiteLinear {node, nachfolger}
        = liftIO $ do
            boxPackDefault box node
            mitWidgetHide node
            packedNachfolger <- packSeiten box flowControlBox nachfolger
            case seitenAbschluss node of
                (SeitenAbschluss _name)
                    -> pure ()
                (SeitenAbschlussToggled fortfahrenWennToggled)
                    -> do
                        boxPack flowControlBox fortfahrenWennToggled packingDefault paddingDefault End
                        mitWidgetHide fortfahrenWennToggled
                (SeitenAbschlussToggledTMVar fortfahrenWennToggledTMVar)
                    -> do
                        boxPack flowControlBox fortfahrenWennToggledTMVar packingDefault paddingDefault End
                        mitWidgetHide fortfahrenWennToggledTMVar
            pure PackedSeiteLinear {packedNode = node, packedNachfolger}
packSeiten
    box
    flowControlBox
    AssistantSeiteAuswahl {node, nachfolgerFrage, nachfolgerListe}
        = liftIO $ do
            case seitenAbschluss node of
                (SeitenAbschluss _name)
                    -> pure ()
                (SeitenAbschlussToggled fortfahrenWennToggled)
                    -> do
                        boxPack flowControlBox fortfahrenWennToggled packingDefault paddingDefault End
                        mitWidgetHide fortfahrenWennToggled
                (SeitenAbschlussToggledTMVar fortfahrenWennToggledTMVar)
                    -> do
                        boxPack flowControlBox fortfahrenWennToggledTMVar packingDefault paddingDefault End
                        mitWidgetHide fortfahrenWennToggledTMVar
            vBox <- boxPackWidgetNewDefault box $ Gtk.vBoxNew False 0
            mitWidgetHide vBox
            boxPackDefault vBox node
            mitWidgetShow node
            packedNachfolgerListe <- mapM (packSeiten box flowControlBox) nachfolgerListe
            packedNachfolgerAuswahl <- boxPackWidgetNewDefault vBox $
                auswahlRadioButtonNamedNew (packedNode <$> packedNachfolgerListe) nachfolgerFrage name
            pure $ PackedSeiteAuswahl {
                packedNode = node,
                packedNachfolgerFrage = nachfolgerFrage,
                packedNachfolgerListe,
                packedBox = erhalteBox vBox,
                packedNachfolgerAuswahl}
packSeiten
    box
    flowControlBox
    AssistantSeiteLetzte {node}
        = liftIO $ do
            case seitenAbschluss node of
                (SeitenAbschluss _name)
                    -> pure ()
                (SeitenAbschlussToggled fortfahrenWennToggled)
                    -> do
                        boxPack flowControlBox fortfahrenWennToggled packingDefault paddingDefault End
                        mitWidgetHide fortfahrenWennToggled
                (SeitenAbschlussToggledTMVar fortfahrenWennToggledTMVar)
                    -> do
                        boxPack flowControlBox fortfahrenWennToggledTMVar packingDefault paddingDefault End
                        mitWidgetHide fortfahrenWennToggledTMVar
            boxPackDefault box node
            mitWidgetHide node
            pure $ PackedSeiteLetzte {packedNode = node}

zeigeSeite :: (MonadIO m, MitWidget w, Eq w) =>
    Gtk.Button -> TVar (AssistantSeitenBaumPacked w) -> AssistantSeitenBaumPacked w -> m ()
zeigeSeite seitenAbschlussKnopf tvarAktuelleSeite nachfolger = liftIO $ do
    atomically $ writeTVar tvarAktuelleSeite nachfolger
    mitWidgetShow nachfolger
    case seitenAbschluss $ packedNode nachfolger of
        (SeitenAbschluss name)
            -> do
                Gtk.set seitenAbschlussKnopf [Gtk.buttonLabel := name]
                mitWidgetShow seitenAbschlussKnopf
        (SeitenAbschlussToggled fortfahrenWennToggled)
            -> do
                mitWidgetShow fortfahrenWennToggled
                mitWidgetHide seitenAbschlussKnopf
        (SeitenAbschlussToggledTMVar fortfahrenWennToggledTMVar)
            -> do
                mitWidgetShow fortfahrenWennToggledTMVar
                mitWidgetHide seitenAbschlussKnopf

-- | Ergebnis-Typ von 'assistantAuswerten'
data AssistantResult a
    = AssistantErfolgreich a    -- ^ Assistant erfolgreich
    | AssistantAbbrechen        -- ^ Der Abbrechen-Knopf wurde gedrückt
    | AssistantBeenden          -- ^ Das Fenster wurde durch einen Druck des 'X' in der Titelleiste beendet
        deriving (Eq)

-- | Zeige einen Assistant, warte auf finale Nutzer-Eingabe und werte die Eingaben aus.
assistantAuswerten :: (MonadIO m) => Assistant w a -> m (AssistantResult a)
assistantAuswerten assistant@Assistant {fenster, auswertFunktion} = liftIO $ do
    Gtk.widgetShow fenster
    -- Warte auf eine vollständige Eingabe (realisiert durch takeAuswahl)
    ergebnis <- atomically (takeAuswahl assistant) >>= \case
        (AssistantErfolgreich auswahl)
            -> AssistantErfolgreich <$> auswertFunktion auswahl
        AssistantAbbrechen
            -> pure AssistantAbbrechen
        AssistantBeenden
            -> pure AssistantBeenden
    Gtk.widgetHide fenster
    pure ergebnis
        where
            -- Warte auf eine vollständige (Right) Eingabe und gebe diese zurück.
            -- Analog zu 'Control.Concurrent.STM.takeTMVar'.
            takeAuswahl ::Assistant w a -> STM (AssistantResult (NonEmpty w))
            takeAuswahl Assistant {tvarAuswahl, seiten} = do
                readTVar tvarAuswahl >>= \case
                    (Left _a)
                        -> retry
                    (Right result)
                        -> do
                            -- Setze auf Startwert zurück
                            writeTVar tvarAuswahl $ Left ([], seiten)
                            -- Gebe Ergebnis zurück
                            pure result
#endif