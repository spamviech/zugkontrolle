{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

{-|
Description : Eigenes Assistant-Widget, nachdem das von Gtk bereitgestellte nicht funktioniert.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Assistant () where
#else
module Zug.UI.Gtk.Assistant (
    Assistant(), AssistantSeiten(..), assistantNew, assistantAuswerten, AssistantResult(..)) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, retry, STM, TVar, newTVarIO, readTVarIO, readTVar, writeTVar)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import qualified Zug.Language as Language
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNew, boxPackWidgetNewDefault,
                                    Packing(..), packingDefault, Position(..), paddingDefault,
                                    buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitWidgetShow, mitWidgetHide, MitContainer(..), MitBox(), MitWindow(..))

-- | Fenster zum erstellen eines Objekts, potentiell in mehreren Schritten
data Assistant w a
    = Assistant {
        fenster :: Gtk.Window,
        seiten :: AssistantSeiten Gtk.Box,
        tvarAuswahl :: TVar (Either ([w], AssistantSeiten Gtk.Box) (AssistantResult (NonEmpty w))),
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

-- | Seiten eines 'Assistant'.
-- Die Form ist wie ein Rose-tree, ergänzt um zusätzliche Informationen.
-- Der Name wird im Titel des Assistant und bei der Auswahl der Nachfolgeseite gezeigt.
data AssistantSeiten w
    -- | Seite mit einem direkten Nachfolger
    = AssistantSeiteLinear {
        seite :: w,
        name :: Text,
        weiter :: Text,
        nachfolger :: AssistantSeiten w}
    -- | Seite mit meheren direkten Nachfolger
    | AssistantSeiteAuswahl {
        seite :: w,
        name :: Text,
        weiter :: Text,
        nachfolgerFrage :: Text,
        nachfolgerListe :: NonEmpty (AssistantSeiten w)}
    -- | Seite ohne Nachfolger
    | AssistantSeiteLetzte {
        seite :: w,
        name :: Text,
        finalisieren :: Text}
    deriving (Eq, Show)

instance (MitWidget w) => MitWidget (AssistantSeiten w) where
    erhalteWidget :: AssistantSeiten w -> Gtk.Widget
    erhalteWidget = erhalteWidget . seite

instance Foldable AssistantSeiten where
    foldMap :: Monoid m => (w -> m) -> AssistantSeiten w -> m
    foldMap
        f
        AssistantSeiteLinear {seite, nachfolger}
            = f seite `mappend` foldMap f nachfolger
    foldMap
        f
        AssistantSeiteAuswahl {seite, nachfolgerListe}
            = f seite `mappend` foldMap (foldMap f) nachfolgerListe
    foldMap
        f
        AssistantSeiteLetzte {seite}
            = f seite

-- | Maximale Anzahl an 'AssistantSeiten' (Tiefe des Baums)
anzahlSeiten :: AssistantSeiten w -> Natural
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
assistantNew :: (MonadIO m, MitWidget w) => Gtk.Window -> AssistantSeiten w -> (NonEmpty w -> IO a) -> m (Assistant w a)
assistantNew parent seitenEingabe auswertFunktion = liftIO $ do
    tvarAuswahl <- newTVarIO $ Left ([], _seiten)
    -- Erstelle Fenster
    fenster <- Gtk.windowNew
    Gtk.set fenster [Gtk.windowTransientFor := parent, Gtk.windowModal := True, Gtk.windowTitle := name seitenEingabe]
    Gtk.on fenster Gtk.deleteEvent $ liftIO $ do
        atomically $ writeTVar tvarAuswahl $ Right AssistantBeenden
        pure True
    vBox <- liftIO $ containerAddWidgetNew fenster $ Gtk.vBoxNew False 0
    -- Knopf-Leiste für permanente Funktionen
    flowControlBox <- liftIO $ boxPackWidgetNew vBox PackNatural paddingDefault End Gtk.hButtonBoxNew
    liftIO $ boxPackWidgetNew flowControlBox packingDefault paddingDefault End $
        buttonNewWithEventLabel Language.abbrechen $ atomically $ writeTVar tvarAuswahl $ Right AssistantAbbrechen
    -- Packe Seiten in entsprechende Box und zeige nur die erste an.
    seiten <- packSeiten vBox flowControlBox seitenEingabe
    tvarAktuelleSeite <- newTVarIO seiten
    weiterKnopf <- liftIO $ boxPackWidgetNewDefault flowControlBox $
        buttonNewWithEventLabel Language.weiter $ do
            aktuelleSeite <- readTVarIO tvarAktuelleSeite
            mitWidgetHide aktuelleSeite
            case aktuelleSeite of
                AssistantSeiteLinear {nachfolger}
                    -> do
                        mitWidgetShow nachfolger
                        case nachfolger of
                            AssistantSeiteLetzte {finalisieren}
                                -> Gtk.set (_finalisierenKnopf :: Gtk.Button) [Gtk.buttonLabel := finalisieren]
                            assistantSeite
                                -> Gtk.set (_weiterKnopf :: Gtk.Button) [Gtk.buttonLabel := weiter assistantSeite]
                AssistantSeiteAuswahl {seite, name, weiter, nachfolgerFrage, nachfolgerListe}
                    -> _        -- wie AssistantSeiteLinear nach nachfolgerAuswahl
                assistantSeite@AssistantSeiteLetzte {}
                    -> error $ "weiterKnopf aus AssistantSeiteLetzte aufgerufen!"
            _
    _
    pure Assistant {fenster, seiten, tvarAuswahl, auswertFunktion}

packSeiten :: (MitBox b, MitBox fc, MitWidget w, MonadIO m) =>
    b -> fc -> AssistantSeiten w -> m (AssistantSeiten Gtk.Box)
packSeiten
    box
    flowControlBox
    assistant@AssistantSeiteLinear {seite, name, weiter, nachfolger}
        = liftIO $ do
            _
packSeiten
    box
    flowControlBox
    AssistantSeiteAuswahl {seite, name, weiter, nachfolgerFrage, nachfolgerListe}
        = liftIO _
packSeiten
    box
    flowControlBox
    AssistantSeiteLetzte {seite, name, finalisieren}
        = liftIO _

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
            -- Warte auf eine vollständige (Right) Eingabe
            takeAuswahl :: TVar (Either a (AssistantResult (NonEmpty w))) -> STM (AssistantResult (NonEmpty w))
            takeAuswahl tvarAuswahl = do
                readTVar tvarAuswahl >>= \case
                    (Left _a)
                        -> retry
                    (Right result)
                        -> pure result
#endif