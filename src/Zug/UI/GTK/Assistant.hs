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
    Assistant(), AssistantSeite(..), AssistantSeitenBaum(..),
    assistantNew, assistantAuswerten, AssistantResult(..)) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, retry, STM, TVar, newTVarIO, readTVarIO, readTVar, writeTVar)
import Control.Lens (Field2(..))
import qualified Control.Lens as Lens
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
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNew, boxPackWidgetNewDefault, boxPackDefault,
                                    Packing(..), packingDefault, Position(..), paddingDefault,
                                    buttonNewWithEventLabel, widgetShowIf)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitWidgetShow, mitWidgetHide, MitContainer(..), MitBox(..), MitWindow(..))

-- | Fenster zum erstellen eines Objekts, potentiell in mehreren Schritten
data Assistant w a
    = Assistant {
        fenster :: Gtk.Window,
        seiten :: AssistantSeitenBaumPacked w,
        tvarAuswahl :: TVar (Either ([w], AssistantSeitenBaumPacked w) (AssistantResult (NonEmpty w))),
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

-- | Seite eines 'Assistant'.
-- Der Name wird im Titel des 'Assistant' und bei der Auswahl der Nachfolgerseite angezeigt.
-- /seitenAbschluss/ ist der Text des Knopfs zum Anzeigen der nächsten Seite/Finalisieren des 'Assistant'.
data AssistantSeite w
    = AssistantSeite {
        seite :: w,
        name :: Text,
        seitenAbschluss :: Text}
            deriving (Eq, Show, Functor)

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
newtype AssistantSeitenBaumPacked w
    = AssistantSeitenBaumPacked {
        unpackSeitenBaum :: AssistantSeitenBaum (Either w (Gtk.Box, w, AuswahlWidget (AssistantSeite w)))}

instance (MitWidget w) => MitWidget (AssistantSeitenBaumPacked w) where
    erhalteWidget :: AssistantSeitenBaumPacked w -> Gtk.Widget
    erhalteWidget = erhalteWidget . either id (Lens.view _2) . seite . node . unpackSeitenBaum

-- | Erstelle einen neuen 'Assistant'.
-- Die /seiten/ werden in 'Tree'-Reihenfolge von Wurzel zu Blatt angezeigt.
-- Wenn es mehr als einen Nachfolgeknoten gibt wird der Nutzer gefragt, welcher als nächster gezeigt werden soll.
-- Falls es bereitgestellt wird ersetzt dass mitgelieferte 'Gtk.Label' dabei die Standard-Frage.
-- Existiert kein Nachfolgeknoten wird der /Weiter/-Knopf durch einen /abschlussAktion/-benannten Knopf ersetzt.
--
-- Die /auswertFunktion/ wird gespeichert und durch 'assistantAuswerten' aufgerufen.
-- Sie erhält als Argument die ausgewählten /seiten/.
assistantNew :: (MonadIO m, MitWidget w, Eq w, MitWindow p) =>
    p -> AssistantSeitenBaum w -> (NonEmpty w -> IO a) -> m (Assistant w a)
assistantNew parent seitenEingabe auswertFunktion = liftIO $ do
    -- Erstelle Fenster
    fenster <- Gtk.windowNew
    Gtk.set fenster [
        Gtk.windowTransientFor := erhalteWindow parent,
        Gtk.windowModal := True,
        Gtk.windowTitle := name (node seitenEingabe)]
    vBox <- liftIO $ containerAddWidgetNew fenster $ Gtk.vBoxNew False 0
    -- Packe Seiten in entsprechende Box und zeige nur die erste an.
    seiten <- packSeiten vBox (node seitenEingabe) seitenEingabe
    tvarAuswahl <- newTVarIO $ Left ([], seiten)
    -- Füge Reaktion auf beenden des Assistant durch 'X' in der Titelleiste hinzu
    Gtk.on fenster Gtk.deleteEvent $ liftIO $ do
        atomically $ writeTVar tvarAuswahl $ Right AssistantBeenden
        pure True
    -- Knopf-Leiste für permanente Funktionen
    tvarAktuelleSeite <- newTVarIO seiten
    flowControlBox <- liftIO $ boxPackWidgetNew vBox PackNatural paddingDefault End Gtk.hButtonBoxNew
    boxPackWidgetNew flowControlBox packingDefault paddingDefault End $
        buttonNewWithEventLabel Language.abbrechen $ atomically $ writeTVar tvarAuswahl $ Right AssistantAbbrechen
    seitenAbschlussKnopf <- liftIO $ boxPackWidgetNewDefault flowControlBox $
        Gtk.buttonNewWithLabel (Language.weiter :: Text)
    Gtk.on seitenAbschlussKnopf Gtk.buttonActivated $ do
        aktuelleSeite <- readTVarIO tvarAktuelleSeite
        mitWidgetHide aktuelleSeite
        case unpackSeitenBaum aktuelleSeite of
            AssistantSeiteLinear {nachfolger}
                -> zeigeNachfolger seitenAbschlussKnopf $ AssistantSeitenBaumPacked nachfolger
            assistant@AssistantSeiteAuswahl {nachfolgerListe}
                -> do
                    let Right (_box, _w, auswahlWidget) = seite $ node assistant
                        vergleich :: (Eq w) =>
                            AssistantSeite w ->
                            AssistantSeitenBaum (Either w (Gtk.Box, w, AuswahlWidget (AssistantSeite w))) ->
                                Bool
                        vergleich vergleichsSeite seitenBaum = case seite $ node seitenBaum of
                            (Left gezeigtSeite)
                                -> (gezeigtSeite <$ node seitenBaum) == vergleichsSeite
                            (Right (_box, gezeigtSeite, _auswahlWidget))
                                -> (gezeigtSeite <$ node seitenBaum) == vergleichsSeite
                    nachfolgerSeite <- aktuelleAuswahl auswahlWidget
                    let maybeNachfolger = find (vergleich nachfolgerSeite) nachfolgerListe
                        nachfolger = case maybeNachfolger of
                            (Just nachfolger)   -> nachfolger
                            Nothing             -> error "unbekannte Seite bei AuswahlWidget ausgewählt."
                    zeigeNachfolger seitenAbschlussKnopf $ AssistantSeitenBaumPacked nachfolger
            assistantSeite@AssistantSeiteLetzte {}
                -> do
                    auswahl <- readTVarIO tvarAuswahl
                    atomically $ writeTVar tvarAuswahl $ Right $ case auswahl of
                        (Left (besuchteSeiten, _aktuelleSeite))
                            -> AssistantErfolgreich $ NonEmpty.reverse $
                                (either id (Lens.view _2) $ seite $ node assistantSeite) :| besuchteSeiten
                        (Right ergebnis)
                            -> ergebnis
                    -- Zeige erste Seite (für nächsten Assistant-Aufruf)
                    mitWidgetShow seiten
    pure Assistant {fenster, seiten, tvarAuswahl, auswertFunktion}

packSeiten :: (MitBox b, MitWidget w, Eq w, MonadIO m) =>
    b -> AssistantSeite w -> AssistantSeitenBaum w -> m (AssistantSeitenBaumPacked w)
packSeiten
    box
    ersteSeite
    AssistantSeiteLinear {node, nachfolger}
        = liftIO $ do
            boxPackDefault box node
            widgetShowIf (ersteSeite == node) node
            nachfolgerPacked <- packSeiten box ersteSeite nachfolger
            pure $ AssistantSeitenBaumPacked AssistantSeiteLinear {
                node = Left <$> node,
                nachfolger = unpackSeitenBaum nachfolgerPacked}
packSeiten
    box
    ersteSeite
    AssistantSeiteAuswahl {node = nodeW, nachfolgerFrage, nachfolgerListe}
        = liftIO $ do
            vBox <- boxPackWidgetNewDefault box $ Gtk.vBoxNew False 0
            boxPackDefault vBox nodeW
            widgetShowIf (ersteSeite == nodeW) nodeW
            nachfolgerListe <- mapM (fmap unpackSeitenBaum . packSeiten box ersteSeite) nachfolgerListe
            let nodeListe = node <$> nachfolgerListe
            auswahlWidget <- boxPackWidgetNewDefault vBox $
                auswahlRadioButtonNamedNew (erhalteSeite <$> nodeListe) nachfolgerFrage name
            pure $ AssistantSeitenBaumPacked AssistantSeiteAuswahl {
                node = nodeW {seite = Right (erhalteBox vBox, seite nodeW, auswahlWidget)},
                nachfolgerFrage,
                nachfolgerListe}
    where
        erhalteSeite :: AssistantSeite (Either w (a, w, b)) -> AssistantSeite w
        erhalteSeite assistantSeite = case seite assistantSeite of
            (Left w)
                -> w <$ assistantSeite
            (Right (_a, w, _b))
                -> w <$ assistantSeite
packSeiten
    box
    ersteSeite
    AssistantSeiteLetzte {node}
        = liftIO $ do
            boxPackDefault box node
            widgetShowIf (ersteSeite == node) node
            pure $ AssistantSeitenBaumPacked AssistantSeiteLetzte {node = Left <$> node}

zeigeNachfolger :: (MonadIO m, MitWidget w) => Gtk.Button -> AssistantSeitenBaumPacked w -> m ()
zeigeNachfolger seitenAbschlussKnopf nachfolger = liftIO $ do
    mitWidgetShow nachfolger
    Gtk.set seitenAbschlussKnopf [Gtk.buttonLabel := seitenAbschluss (node $ unpackSeitenBaum nachfolger)]

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