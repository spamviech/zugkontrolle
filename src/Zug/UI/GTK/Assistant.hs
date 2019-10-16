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
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import qualified Zug.Language as Language
import Zug.UI.Gtk.Auswahl (AuswahlWidget, auswahlRadioButtonNamedNew, aktuelleAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNew, boxPackWidgetNewDefault, boxPackDefault,
                                    Packing(..), packingDefault, Position(..), paddingDefault,
                                    buttonNewWithEventLabel)
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
    deriving (Eq, Show)

-- | Darstellung des 'AssistantSeitenBaum's, inklusiver zugehörigem 'AuswahlWidget'
type AssistantSeitenBaumPacked w = AssistantSeitenBaum (Either w (Gtk.Box, w, AuswahlWidget (AssistantSeitenBaum w)))

instance (MitWidget w) => MitWidget (AssistantSeitenBaum w) where
    erhalteWidget :: AssistantSeitenBaum w -> Gtk.Widget
    erhalteWidget = erhalteWidget . node

instance Foldable AssistantSeitenBaum where
    foldMap :: Monoid m => (w -> m) -> AssistantSeitenBaum w -> m
    foldMap
        f
        AssistantSeiteLinear {node, nachfolger}
            = f (seite node) `mappend` foldMap f nachfolger
    foldMap
        f
        AssistantSeiteAuswahl {node, nachfolgerListe}
            = f (seite node) `mappend` foldMap (foldMap f) nachfolgerListe
    foldMap
        f
        AssistantSeiteLetzte {node}
            = f $ seite node

-- | Maximale Anzahl an 'AssistantSeiten' (Tiefe des Baums)
anzahlSeiten :: AssistantSeitenBaum w -> Natural
anzahlSeiten
    AssistantSeiteLinear {nachfolger}
        = succ $ anzahlSeiten nachfolger
anzahlSeiten
    AssistantSeiteAuswahl {nachfolgerListe}
        = succ $ maximum $ anzahlSeiten <$> nachfolgerListe
anzahlSeiten
    AssistantSeiteLetzte {}
        = 1

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
    tvarAuswahl <- newTVarIO $ Left ([], _seiten)
    -- Erstelle Fenster
    fenster <- Gtk.windowNew
    Gtk.set fenster [
        Gtk.windowTransientFor := erhalteWindow parent,
        Gtk.windowModal := True,
        Gtk.windowTitle := name (node seitenEingabe)]
    Gtk.on fenster Gtk.deleteEvent $ liftIO $ do
        atomically $ writeTVar tvarAuswahl $ Right AssistantBeenden
        pure True
    vBox <- liftIO $ containerAddWidgetNew fenster $ Gtk.vBoxNew False 0
    -- Packe Seiten in entsprechende Box und zeige nur die erste an.
    seiten <- packSeiten vBox seitenEingabe
    -- Knopf-Leiste für permanente Funktionen
    tvarAktuelleSeite <- newTVarIO seitenEingabe
    flowControlBox <- liftIO $ boxPackWidgetNew vBox PackNatural paddingDefault End Gtk.hButtonBoxNew
    liftIO $ boxPackWidgetNew flowControlBox packingDefault paddingDefault End $
        buttonNewWithEventLabel Language.abbrechen $ atomically $ writeTVar tvarAuswahl $ Right AssistantAbbrechen
    seitenAbschlussKnopf <- liftIO $ boxPackWidgetNewDefault flowControlBox $
        buttonNewWithEventLabel Language.weiter $ do
            aktuelleSeite <- readTVarIO tvarAktuelleSeite
            mitWidgetHide aktuelleSeite
            case aktuelleSeite of
                AssistantSeiteLinear {nachfolger}
                    -> zeigeNachfolger _seitenAbschlussKnopf nachfolger
                AssistantSeiteAuswahl {node, nachfolgerFrage, nachfolgerListe}
                    -> _        -- wie seitenAbschluss nach nachfolgerAuswahl
                assistantSeite@AssistantSeiteLetzte {}
                    -> do
                        auswahl <- readTVarIO tvarAuswahl
                        atomically $ writeTVar tvarAuswahl $ Right $ case auswahl of
                            (Left (besuchteSeiten, _aktuelleSeite))
                                -> AssistantErfolgreich $ NonEmpty.reverse $
                                    (seite $ node assistantSeite) :| besuchteSeiten
                            (Right ergebnis)
                                -> ergebnis
            _
    _
    pure Assistant {fenster, seiten, tvarAuswahl, auswertFunktion}

zeigeNachfolger :: (MonadIO m, MitWidget w) => Gtk.Button -> AssistantSeitenBaum w -> m ()
zeigeNachfolger seitenAbschlussKnopf nachfolger = liftIO $ do
    mitWidgetShow nachfolger
    Gtk.set seitenAbschlussKnopf [Gtk.buttonLabel := seitenAbschluss (node nachfolger)]

packSeiten :: (MitBox b, MitWidget w, Eq w, MonadIO m) =>
    b -> AssistantSeitenBaum w -> m (AssistantSeitenBaumPacked w)
packSeiten
    box
    AssistantSeiteLinear {node, nachfolger}
        = liftIO $ do
            boxPackDefault box node
            nachfolgerPacked <- packSeiten box nachfolger
            pure AssistantSeiteLinear {node = Left <$> node, nachfolger = nachfolgerPacked}
packSeiten
    box
    AssistantSeiteAuswahl {node = nodeW, nachfolgerFrage, nachfolgerListe}
        = liftIO $ do
            vBox <- boxPackWidgetNewDefault box $ Gtk.vBoxNew False 0
            boxPackDefault vBox nodeW
            auswahlWidget <- boxPackWidgetNewDefault vBox $
                auswahlRadioButtonNamedNew nachfolgerListe nachfolgerFrage $ name . node
            nachfolgerListePacked <- mapM (packSeiten box) nachfolgerListe
            pure AssistantSeiteAuswahl {
                node = nodeW {seite = Right (erhalteBox vBox, seite nodeW, auswahlWidget)},
                nachfolgerFrage,
                nachfolgerListe = nachfolgerListePacked}
packSeiten
    box
    AssistantSeiteLetzte {node}
        = liftIO $ do
            boxPackDefault box node
            pure AssistantSeiteLetzte {node = Left <$> node}

-- | Ergebnis-Typ von 'assistantAuswerten'
data AssistantResult a
    = AssistantErfolgreich a    -- ^ Assistant erfolgreich
    | AssistantAbbrechen        -- ^ Der Abbrechen-Knopf wurde gedrückt
    | AssistantBeenden          -- ^ Das Fenster wurde durch einen Druck des 'X' in der Titelleiste beendet
        deriving (Eq)

-- | Zeige einen Assistant, warte auf finale Nutzer-Eingabe und werte die Eingaben aus.
assistantAuswerten :: (MonadIO m) => Assistant w a -> m (AssistantResult a)
assistantAuswerten Assistant {fenster, tvarAuswahl, auswertFunktion} = liftIO $ do
    Gtk.widgetShow fenster
    -- Warte auf eine vollständige Eingabe (realisiert durch takeAuswahl)
    ergebnis <- atomically (takeAuswahl tvarAuswahl) >>= \case
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
            takeAuswahl :: TVar (Either a (AssistantResult (NonEmpty w))) -> STM (AssistantResult (NonEmpty w))
            takeAuswahl tvarAuswahl = do
                readTVar tvarAuswahl >>= \case
                    (Left _a)
                        -> retry
                    (Right result)
                        -> pure result
#endif