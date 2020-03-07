{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveFunctor #-}
#endif

{-|
Description : Eigenes Assistant-Widget, nachdem das von Gtk bereitgestellte nicht funktioniert.
-}
module Zug.UI.Gtk.Assistant
  (
#ifdef ZUGKONTROLLEGUI
    Assistant()
  , AssistantSeite(..)
  , SeitenAbschluss(..)
  , AssistantSeitenBaum(..)
  , assistantNew
  , assistantAuswerten
  , AssistantResult(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
-- Bibliotheken
import Control.Concurrent.STM
       (atomically, retry, STM, TVar, newTVarIO, readTVarIO, readTVar, writeTVar, modifyTVar)
import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Data.Either (rights)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk

-- Abhängigkeit von anderen Modulen
import Zug.Language (Sprache(..), MitSprache(..), alleSprachen)
import qualified Zug.Language as Language
import Zug.UI.Gtk.Auswahl (AuswahlWidget, auswahlRadioButtonNamedNew, aktuelleAuswahl)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggled, FortfahrenWennToggledVar)
import Zug.UI.Gtk.Hilfsfunktionen
       (containerAddWidgetNew, boxPackWidgetNew, boxPackWidgetNewDefault, boxPack, boxPackDefault
      , Packing(..), packingDefault, Position(..), paddingDefault, buttonNewWithEventLabel
      , widgetShowIf)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitWidgetShow, mitWidgetHide, MitButton(..)
                         , MitContainer(..), MitBox(..), MitWindow(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), MitSpracheGui(), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt (StatusGui, StatusVarGui, WegstreckeCheckButtonVoid)
import Zug.UI.Gtk.ZugtypSpezifisch (ZugtypSpezifisch)

-- | Fenster zum erstellen eines Wertes, potentiell in mehreren Schritten
data Assistant w a =
    Assistant
    { fenster :: Gtk.Window
    , seiten :: AssistantSeitenBaumPacked w
    , tvarAuswahl :: TVar (Either ([AssistantSeitenBaumPacked w],
                                   AssistantSeitenBaumPacked w
                                  ) (AssistantResult (NonEmpty w)))
    , auswertFunktion :: NonEmpty w -> IO a
    , seitenAbschlussKnopf :: Gtk.Button
    , zurückKnopf :: Gtk.Button
    , tvarAktuelleSeite :: TVar (AssistantSeitenBaumPacked w)
    }

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
    = SeitenAbschluss (Sprache -> Text)
    | SeitenAbschlussToggled FortfahrenWennToggled
    | SeitenAbschlussToggledVar (FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid)
    | SeitenAbschlussZugtyp (ZugtypSpezifisch Gtk.Button)
    | SeitenAbschlussButton Gtk.Button

instance Eq SeitenAbschluss where
    (==) :: SeitenAbschluss -> SeitenAbschluss -> Bool
    (==) (SeitenAbschluss label0) (SeitenAbschluss label1) =
        all (uncurry (==)) $ zip (map label0 alleSprachen) (map label1 alleSprachen)
    (==) (SeitenAbschlussToggled f0) (SeitenAbschlussToggled f1) = f0 == f1
    (==) (SeitenAbschlussToggledVar f0) (SeitenAbschlussToggledVar f1) = f0 == f1
    (==) (SeitenAbschlussZugtyp z0) (SeitenAbschlussZugtyp z1) = z0 == z1
    (==) (SeitenAbschlussButton b0) (SeitenAbschlussButton b1) = b0 == b1
    (==) _s0 _s1 = False

-- | Seite eines 'Assistant'.
-- Der Name wird im Titel des 'Assistant' und bei der Auswahl der Nachfolgerseite angezeigt.
-- /seiteZurücksetzten/ wird vor jedem anzeigen der Seite aufgerufen
data AssistantSeite w =
    AssistantSeite
    { seite :: w
    , name :: Sprache -> Text
    , seiteZurücksetzen :: IO ()
    , seitenAbschluss :: SeitenAbschluss
    }
    deriving (Functor)

instance (Eq w) => Eq (AssistantSeite w) where
    (==) :: AssistantSeite w -> AssistantSeite w -> Bool
    (==)
        AssistantSeite {seite = seite0, name = name0, seitenAbschluss = seitenAbschluss0}
        AssistantSeite {seite = seite1, name = name1, seitenAbschluss = seitenAbschluss1} =
        seite0 == seite1
        && all (\sprache -> name0 sprache == name1 sprache) alleSprachen
        && seitenAbschluss0 == seitenAbschluss1

instance (MitWidget w) => MitWidget (AssistantSeite w) where
    erhalteWidget :: AssistantSeite w -> Gtk.Widget
    erhalteWidget = erhalteWidget . seite

-- | Seiten eines 'Assistant'.
-- Die Form ist wie ein Rose-tree, ergänzt um zusätzliche Informationen.
data AssistantSeitenBaum w
    = -- | Seite mit einem direkten Nachfolger
      AssistantSeiteLinear { node :: AssistantSeite w, nachfolger :: AssistantSeitenBaum w }
      -- | Seite mit mehreren direkten Nachfolger
    | AssistantSeiteAuswahl
          { node :: AssistantSeite w
          , nachfolgerFrage :: Sprache -> Text
          , nachfolgerListe :: NonEmpty (AssistantSeitenBaum w)
          }
      -- | Seite ohne Nachfolger
    | AssistantSeiteLetzte { node :: AssistantSeite w }

instance (MitWidget w) => MitWidget (AssistantSeitenBaum w) where
    erhalteWidget :: AssistantSeitenBaum w -> Gtk.Widget
    erhalteWidget = erhalteWidget . node

-- | Darstellung des 'AssistantSeitenBaum's, inklusiver zugehörigem 'AuswahlWidget'
data AssistantSeitenBaumPacked w
    = -- | Seite mit einem direkten Nachfolger
      PackedSeiteLinear
          { packedNode :: AssistantSeite w
          , packedNachfolger :: AssistantSeitenBaumPacked w
          }
      -- | Seite mit mehreren direkten Nachfolger
    | PackedSeiteAuswahl
          { packedNode :: AssistantSeite w
          , packedNachfolgerFrage :: Sprache -> Text
          , packedNachfolgerListe :: NonEmpty (AssistantSeitenBaumPacked w)
          , packedBox :: Gtk.Box
          , packedNachfolgerAuswahl :: AuswahlWidget (AssistantSeite w)
          }
      -- | Seite ohne Nachfolger
    | PackedSeiteLetzte { packedNode :: AssistantSeite w }

instance (MitWidget w) => MitWidget (AssistantSeitenBaumPacked w) where
    erhalteWidget :: AssistantSeitenBaumPacked w -> Gtk.Widget
    erhalteWidget PackedSeiteLinear {packedNode} = erhalteWidget packedNode
    erhalteWidget PackedSeiteAuswahl {packedBox} = erhalteWidget packedBox
    erhalteWidget PackedSeiteLetzte {packedNode} = erhalteWidget packedNode

besondereSeitenAbschlussKnöpfe
    :: AssistantSeitenBaumPacked w -> [Either (Sprache -> Text) Gtk.Button]
besondereSeitenAbschlussKnöpfe PackedSeiteLinear {packedNode, packedNachfolger} =
    besondererSeitenAbschlussKnopf packedNode : besondereSeitenAbschlussKnöpfe packedNachfolger
besondereSeitenAbschlussKnöpfe PackedSeiteAuswahl {packedNode, packedNachfolgerListe} =
    besondererSeitenAbschlussKnopf packedNode
    : concat (besondereSeitenAbschlussKnöpfe <$> packedNachfolgerListe)
besondereSeitenAbschlussKnöpfe
    PackedSeiteLetzte {packedNode} = [besondererSeitenAbschlussKnopf packedNode]

besondererSeitenAbschlussKnopf :: AssistantSeite w -> Either (Sprache -> Text) Gtk.Button
besondererSeitenAbschlussKnopf AssistantSeite {seitenAbschluss = (SeitenAbschluss text)} =
    Left text
besondererSeitenAbschlussKnopf
    AssistantSeite {seitenAbschluss = (SeitenAbschlussToggled fortfahrenWennToggled)} =
    Right $ erhalteButton fortfahrenWennToggled
besondererSeitenAbschlussKnopf
    AssistantSeite {seitenAbschluss = (SeitenAbschlussToggledVar fortfahrenWennToggledVar)} =
    Right $ erhalteButton fortfahrenWennToggledVar
besondererSeitenAbschlussKnopf
    AssistantSeite {seitenAbschluss = (SeitenAbschlussZugtyp zugtypSpezifisch)} =
    Right $ erhalteButton zugtypSpezifisch
besondererSeitenAbschlussKnopf AssistantSeite {seitenAbschluss = (SeitenAbschlussButton button)} =
    Right button

besondererSeitenAbschlussWidget :: AssistantSeite w -> Either (Sprache -> Text) Gtk.Widget
besondererSeitenAbschlussWidget AssistantSeite {seitenAbschluss = (SeitenAbschluss text)} =
    Left text
besondererSeitenAbschlussWidget
    AssistantSeite {seitenAbschluss = (SeitenAbschlussToggled fortfahrenWennToggled)} =
    Right $ erhalteWidget fortfahrenWennToggled
besondererSeitenAbschlussWidget
    AssistantSeite {seitenAbschluss = (SeitenAbschlussToggledVar fortfahrenWennToggledVar)} =
    Right $ erhalteWidget fortfahrenWennToggledVar
besondererSeitenAbschlussWidget
    AssistantSeite {seitenAbschluss = (SeitenAbschlussZugtyp zugtypSpezifisch)} =
    Right $ erhalteWidget zugtypSpezifisch
besondererSeitenAbschlussWidget AssistantSeite {seitenAbschluss = (SeitenAbschlussButton button)} =
    Right $ erhalteWidget button

-- | Erstelle einen neuen 'Assistant'.
-- Die /globalenWidgets/ werden permanent in der Fußleiste mit dem /Weiter/-Knopf (etc.) angezeigt.
-- Die /seiten/ werden in 'Tree'-Reihenfolge von Wurzel zu Blatt angezeigt.
-- Wenn es mehr als einen Nachfolgeknoten gibt wird der Nutzer gefragt, welcher als nächster gezeigt werden soll.
-- Falls es bereitgestellt wird ersetzt dass mitgelieferte 'Gtk.Label' dabei die Standard-Frage.
-- Existiert kein Nachfolgeknoten wird der /Weiter/-Knopf durch einen /abschlussAktion/-benannten Knopf ersetzt.
--
-- Die /auswertFunktion/ wird gespeichert und durch 'assistantAuswerten' aufgerufen.
-- Sie erhält als Argument die ausgewählten /seiten/.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
assistantNew
    :: (MonadReader r m, MitSpracheGui r, MonadIO m, MitWidget w, Eq w, MitWidget g, MitWindow p)
    => p
    -> [g]
    -> AssistantSeitenBaum w
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (NonEmpty w -> IO a)
    -> m (Assistant w a)
assistantNew parent globaleWidgets seitenEingabe maybeTVar auswertFunktion = do
    spracheReader <- ask
    -- Erstelle Fenster
    (fenster, vBox, flowControlBox) <- liftIO $ do
        fenster <- Gtk.windowNew
        Gtk.set fenster [Gtk.windowTransientFor := erhalteWindow parent, Gtk.windowModal := True]
        vBox <- containerAddWidgetNew fenster $ Gtk.vBoxNew False 0
        -- Packe Seiten in entsprechende Box
        flowControlBox <- boxPackWidgetNew vBox PackNatural paddingDefault End Gtk.hButtonBoxNew
        pure (fenster, vBox, flowControlBox)
    seiten <- packSeiten vBox flowControlBox seitenEingabe maybeTVar
    (tvarAuswahl, tvarAktuelleSeite, seitenAbschlussKnopf) <- liftIO $ do
        tvarAuswahl <- newTVarIO $ Left ([], seiten)
        tvarAktuelleSeite <- newTVarIO seiten
        -- Füge Reaktion auf Beenden des Assistant durch 'X' in der Titelleiste hinzu
        Gtk.on fenster Gtk.deleteEvent $ liftIO $ do
            atomically $ writeTVar tvarAuswahl $ Right AssistantBeenden
            pure True
        -- Knopf-Leiste für permanente Funktionen und globaleWidgets
        seitenAbschlussKnopf <- boxPackWidgetNewDefault flowControlBox Gtk.buttonNew
        pure (tvarAuswahl, tvarAktuelleSeite, seitenAbschlussKnopf)
    verwendeSpracheGui maybeTVar
        $ \sprache -> Gtk.set seitenAbschlussKnopf [Gtk.buttonLabel := Language.weiter sprache]
    zurückKnopf <- liftIO $ boxPackWidgetNewDefault flowControlBox Gtk.buttonNew
    verwendeSpracheGui maybeTVar
        $ \sprache -> Gtk.set zurückKnopf [Gtk.buttonLabel := Language.zurück sprache]
    forM_ globaleWidgets $ \widget -> do
        boxPackDefault flowControlBox widget
        mitWidgetShow widget
    boxPackWidgetNewDefault flowControlBox
        $ buttonNewWithEventLabel maybeTVar Language.abbrechen
        $ atomically
        $ writeTVar tvarAuswahl
        $ Right AssistantAbbrechen
    -- Konstruiere Ergebnistyp
    let assistant =
            Assistant
            { fenster,
              seiten,
              tvarAuswahl,
              auswertFunktion,
              seitenAbschlussKnopf,
              zurückKnopf,
              tvarAktuelleSeite
            }
    -- Füge Reaktion auf drücken des Vorwärts- und Zurück-Knopfes hinzu
    liftIO $ Gtk.on zurückKnopf Gtk.buttonActivated $ do
        aktuelleSeite <- readTVarIO tvarAktuelleSeite
        mitWidgetHide aktuelleSeite
        case besondererSeitenAbschlussWidget $ packedNode aktuelleSeite of
            (Left _name) -> pure ()
            (Right widget) -> mitWidgetHide widget
        maybeLetzteSeite <- atomically $ do
            auswahl <- readTVar tvarAuswahl
            case auswahl of
                (Left ((letzteSeite:besuchteSeiten), _aktuelleSeite)) -> do
                    writeTVar tvarAuswahl $ Left (besuchteSeiten, letzteSeite)
                    pure $ Just letzteSeite
                _otherwise -> pure Nothing
        case maybeLetzteSeite of
            (Just letzteSeite) -> flip runReaderT spracheReader $ zeigeSeite assistant letzteSeite
            Nothing -> error "Zurück-Knopf an unerwarteter Stelle gedrückt."
    let seitenAbschlussAktion :: IO ()
        seitenAbschlussAktion = do
            aktuelleSeite <- readTVarIO tvarAktuelleSeite
            mitWidgetHide aktuelleSeite
            case besondererSeitenAbschlussWidget $ packedNode aktuelleSeite of
                (Left _name) -> pure ()
                (Right widget) -> mitWidgetHide widget
            mitWidgetShow zurückKnopf
            case aktuelleSeite of
                PackedSeiteLinear {packedNachfolger} -> do
                    atomically $ modifyTVar tvarAuswahl $ \case
                        (Left (besuchteSeiten, _aktuelleSeite))
                            -> Left $ (aktuelleSeite : besuchteSeiten, packedNachfolger)
                        ergebnis -> ergebnis
                    flip runReaderT spracheReader $ zeigeSeite assistant packedNachfolger
                PackedSeiteAuswahl {packedNachfolgerListe, packedNachfolgerAuswahl} -> do
                    nachfolgerSeite <- aktuelleAuswahl packedNachfolgerAuswahl
                    let packedNachfolger =
                            case find ((==) nachfolgerSeite . packedNode) packedNachfolgerListe of
                                (Just packedNachfolger) -> packedNachfolger
                                Nothing -> error
                                    $ "Unbekannte Seite bei AuswahlWidget ausgewählt: "
                                    ++ Text.unpack (name nachfolgerSeite Deutsch)
                    atomically $ modifyTVar tvarAuswahl $ \case
                        (Left (besuchteSeiten, _aktuelleSeite))
                            -> Left $ (aktuelleSeite : besuchteSeiten, packedNachfolger)
                        ergebnis -> ergebnis
                    flip runReaderT spracheReader $ zeigeSeite assistant packedNachfolger
                assistantSeite@PackedSeiteLetzte {} -> do
                    atomically $ modifyTVar tvarAuswahl $ \case
                        (Left (besuchteSeiten, _aktuelleSeite)) -> Right
                            $ AssistantErfolgreich
                            $ NonEmpty.reverse
                            $ (seite $ packedNode assistantSeite)
                            :| (seite . packedNode <$> besuchteSeiten)
                        ergebnis -> ergebnis
                    -- Zeige erste Seite (für nächsten Assistant-Aufruf)
                    mitWidgetShow seiten
    liftIO $ do
        Gtk.on seitenAbschlussKnopf Gtk.buttonActivated seitenAbschlussAktion
        forM_ (rights $ besondereSeitenAbschlussKnöpfe seiten)
            $ \knopf -> Gtk.on knopf Gtk.buttonActivated seitenAbschlussAktion
    -- Zeige erste Seite an
    zeigeSeite assistant seiten
    pure assistant

-- | Packe den übergebenen 'SeitenBaum' in die 'MitBox' und erzeuge notwendige Hilfswidgets.
--
-- Wird eine 'TVar' übergeben kann das Anpassen des Labels aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
packSeiten :: (MitBox b, MitWidget w, Eq w, SpracheGuiReader r m, MonadIO m)
           => b
           -> Gtk.HButtonBox
           -> AssistantSeitenBaum w
           -> Maybe (TVar (Maybe [Sprache -> IO ()]))
           -> m (AssistantSeitenBaumPacked w)
packSeiten box flowControlBox AssistantSeiteLinear {node, nachfolger} maybeTVar = do
    case besondererSeitenAbschlussWidget node of
        (Left _text) -> pure ()
        (Right widget) -> do
            boxPack flowControlBox widget packingDefault paddingDefault End
            mitWidgetHide widget
    boxPackDefault box node
    mitWidgetHide node
    packedNachfolger <- packSeiten box flowControlBox nachfolger maybeTVar
    pure PackedSeiteLinear { packedNode = node, packedNachfolger }
packSeiten
    box
    flowControlBox
    AssistantSeiteAuswahl {node, nachfolgerFrage, nachfolgerListe}
    maybeTVar = do
    case besondererSeitenAbschlussWidget node of
        (Left _text) -> pure ()
        (Right widget) -> do
            boxPack flowControlBox widget packingDefault paddingDefault End
            mitWidgetHide widget
    vBox <- liftIO $ boxPackWidgetNewDefault box $ Gtk.vBoxNew False 0
    mitWidgetHide vBox
    boxPackDefault vBox node
    mitWidgetShow node
    packedNachfolgerListe <- mapM (packSeiten box flowControlBox `flip` maybeTVar) nachfolgerListe
    packedNachfolgerAuswahl <- boxPackWidgetNewDefault vBox
        $ auswahlRadioButtonNamedNew
            (packedNode <$> packedNachfolgerListe)
            maybeTVar
            nachfolgerFrage
            name
    pure
        $ PackedSeiteAuswahl
        { packedNode = node,
          packedNachfolgerFrage = nachfolgerFrage,
          packedNachfolgerListe,
          packedBox = erhalteBox vBox,
          packedNachfolgerAuswahl
        }
packSeiten box flowControlBox AssistantSeiteLetzte {node} _maybeTVar = liftIO $ do
    case besondererSeitenAbschlussWidget node of
        (Left _text) -> pure ()
        (Right widget) -> do
            boxPack flowControlBox widget packingDefault paddingDefault End
            mitWidgetHide widget
    boxPackDefault box node
    mitWidgetHide node
    pure $ PackedSeiteLetzte { packedNode = node }

-- | Zeige die übergebene Seite an
zeigeSeite :: (SpracheGuiReader r m, MonadIO m, MitWidget w, Eq w)
           => Assistant w a
           -> AssistantSeitenBaumPacked w
           -> m ()
zeigeSeite
    Assistant {fenster, seiten, seitenAbschlussKnopf, zurückKnopf, tvarAktuelleSeite}
    nachfolger = do
    spracheGui <- erhalteSpracheGui
    liftIO $ do
        let nachfolgerSeite = packedNode nachfolger
        seiteZurücksetzen nachfolgerSeite
        Gtk.set fenster [Gtk.windowTitle := leseSprache (name nachfolgerSeite) spracheGui]
        widgetShowIf (packedNode seiten /= nachfolgerSeite) zurückKnopf
        atomically $ writeTVar tvarAktuelleSeite nachfolger
        mitWidgetShow nachfolger
        case besondererSeitenAbschlussWidget nachfolgerSeite of
            (Left text) -> do
                Gtk.set seitenAbschlussKnopf [Gtk.buttonLabel := leseSprache text spracheGui]
                mitWidgetShow seitenAbschlussKnopf
            (Right widget) -> do
                mitWidgetShow widget
                mitWidgetHide seitenAbschlussKnopf

-- | Ergebnis-Typ von 'assistantAuswerten'
data AssistantResult a
    = AssistantErfolgreich a    -- ^ Assistant erfolgreich
    | AssistantAbbrechen        -- ^ Der Abbrechen-Knopf wurde gedrückt
    | AssistantBeenden          -- ^ Das Fenster wurde durch einen Druck des 'X' in der Titelleiste beendet
    deriving (Eq)

-- | Zeige einen Assistant, warte auf finale Nutzer-Eingabe und werte die Eingaben aus.
-- Es wird erwartet, dass diese Funktion geforkt vom GTK-Hauptthread aufgerufen wird.
-- Entsprechend wird 'Gtk.postGUIAsync' verwendet.
assistantAuswerten :: (MonadReader r m, MitSpracheGui r, MonadIO m, MitWidget w, Eq w)
                   => Assistant w a
                   -> m (AssistantResult a)
assistantAuswerten assistant@Assistant {fenster, seiten, auswertFunktion, tvarAktuelleSeite} = do
    spracheReader <- ask
    liftIO $ do
        Gtk.postGUIAsync $ do
            flip runReaderT spracheReader $ zeigeSeite assistant seiten
            Gtk.widgetShow fenster
        -- Warte auf eine vollständige Eingabe (realisiert durch takeAuswahl)
        ergebnis <- atomically (takeAuswahl assistant) >>= \case
            (AssistantErfolgreich auswahl) -> AssistantErfolgreich <$> auswertFunktion auswahl
            AssistantAbbrechen -> pure AssistantAbbrechen
            AssistantBeenden -> pure AssistantBeenden
        letzteSeite <- readTVarIO tvarAktuelleSeite
        Gtk.postGUIAsync $ do
            mitWidgetHide letzteSeite
            Gtk.widgetHide fenster
        pure ergebnis
    -- Warte auf eine vollständige (Right) Eingabe und gebe diese zurück.
    -- Analog zu 'Control.Concurrent.STM.takeTMVar'.

        where
            takeAuswahl :: Assistant w a -> STM (AssistantResult (NonEmpty w))
            takeAuswahl Assistant {tvarAuswahl, seiten} = do
                readTVar tvarAuswahl >>= \case
                    (Left _a) -> retry
                    (Right result) -> do
                        -- Setze auf Startwert zurück
                        writeTVar tvarAuswahl $ Left ([], seiten)
                        -- Gebe Ergebnis zurück
                        pure result
#endif


















