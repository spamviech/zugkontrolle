{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
#endif

{-|
Description : Erstellen eines Assistant zum Hinzufügen eines 'StreckenObjekt'es.
-}
module Zug.UI.Gtk.AssistantHinzufuegen
  (
#ifdef ZUGKONTROLLEGUI
    assistantHinzufügenNew
  , AssistantHinzufügen()
  , assistantHinzufügenAuswerten
  , HinzufügenErgebnis(..)
  , setzeAssistantHinzufügen
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TMVar, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Monad (forM_, foldM, when)
import Control.Monad.Fix (MonadFix())
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.GI.Gtk.Threading as Gtk
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified GI.Gtk as Gtk

import Zug.Enums (Zugtyp(), unterstützteZugtypen)
import qualified Zug.Language as Language
import Zug.Objekt (Objekt)
import Zug.UI.Gtk.AssistantHinzufuegen.HinzufuegenSeite
       (HinzufügenSeite, ButtonHinzufügen(ButtonHinzufügen), spezifischerButtonHinzufügen
      , seiteErgebnis, setzeSeite, hinzufügenBahngeschwindigkeitNew
      , hinzufügenStreckenabschnittNew, hinzufügenWeicheNew, hinzufügenKupplungNew
      , hinzufügenKontaktNew, hinzufügenWegstreckeNew, hinzufügenPlanNew)
import Zug.UI.Gtk.Auswahl (AuswahlWidget, auswahlComboBoxNew)
import Zug.UI.Gtk.Fliessend (FließendAuswahlWidget, fließendAuswahlNew)
import Zug.UI.Gtk.Hilfsfunktionen
       (widgetShowNew, containerAddWidgetNew, boxPackWidgetNewDefault, boxPackDefault
      , notebookAppendPageNew, buttonNewWithEventLabel, boxPackWidgetNew, Position(End)
      , positionDefault, Packing(PackGrow), packingDefault, paddingDefault)
import Zug.UI.Gtk.Klassen
       (MitWidget(..), mitWidgetShow, mitWidgetHide, MitWindow(..), MitButton(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), verwendeSpracheGui, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt (StatusVarGui, StatusVarGuiReader, DynamischeWidgetsReader)
import Zug.UI.StatusVar (StatusVarReader(..))

-- | Seiten des Hinzufügen-'Assistant'
data AssistantHinzufügen =
    AssistantHinzufügen
    { window :: Gtk.Window
    , notebook :: Gtk.Notebook
    , fließendAuswahl :: FließendAuswahlWidget
    , zugtypAuswahl :: AuswahlWidget Zugtyp
    , indexSeiten :: Map Int32 HinzufügenSeite
    , tmVarErgebnis :: TMVar HinzufügenErgebnis
    }
    deriving (Eq)

instance MitWidget AssistantHinzufügen where
    erhalteWidget :: (MonadIO m) => AssistantHinzufügen -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . window

-- | Hat der 'AssistantHinzufügen' ein Ergebnis geliefert?
data HinzufügenErgebnis
    = HinzufügenErfolgreich Objekt
    | HinzufügenAbbrechen
    | HinzufügenBeenden

-- | Zeige den 'AssistantHinzufügen' an und warte bis ein 'HinzufügenErgebnis' vorliegt.
--
-- Es wird erwartet, dass diese Funktion in einem eigenen Thread ausgeführt wird.
assistantHinzufügenAuswerten :: (MonadIO m) => AssistantHinzufügen -> m HinzufügenErgebnis
assistantHinzufügenAuswerten AssistantHinzufügen {window, tmVarErgebnis} = liftIO $ do
    Gtk.postGUIASync $ mitWidgetShow window
    ergebnis <- atomically $ takeTMVar tmVarErgebnis
    Gtk.postGUIASync $ mitWidgetHide window
    pure ergebnis

-- | Erhalte das Ergebnis eines 'AssistantHinzufügen'.
hinzufügenErgebnis :: (StatusVarGuiReader r m, MonadIO m) => AssistantHinzufügen -> m ()
hinzufügenErgebnis
    AssistantHinzufügen {notebook, fließendAuswahl, zugtypAuswahl, indexSeiten, tmVarErgebnis} =
    do
        aktuelleSeite <- liftIO $ Gtk.getNotebookPage notebook
        ergebnis <- seiteErgebnis fließendAuswahl zugtypAuswahl $ indexSeiten Map.! aktuelleSeite
        liftIO $ atomically $ putTMVar tmVarErgebnis $ HinzufügenErfolgreich ergebnis

-- | Setze den aktuellen Wert eines 'AssistantHinzufügen'.
setzeAssistantHinzufügen :: (StatusVarGuiReader r m, SpracheGuiReader r m, MonadIO m)
                          => AssistantHinzufügen
                          -> Objekt
                          -> m ()
setzeAssistantHinzufügen
    AssistantHinzufügen {notebook, fließendAuswahl, zugtypAuswahl, indexSeiten}
    objekt = forM_ (Map.toList indexSeiten) $ \(index, seite) -> do
    richtigeSeite <- setzeSeite fließendAuswahl zugtypAuswahl seite objekt
    liftIO $ when richtigeSeite $ Gtk.setNotebookPage notebook index

-- | Erstelle einen neuen 'AssistantHinzufügen'.
assistantHinzufügenNew
    :: forall p r m.
    ( MitWindow p
    , SpracheGuiReader r m
    , StatusVarGuiReader r m
    , DynamischeWidgetsReader r m
    , MonadFix m
    , MonadIO m
    , MonadFail m
    )
    => p
    -> Maybe TVarSprachewechselAktionen
    -> m AssistantHinzufügen
assistantHinzufügenNew parent maybeTVar = mdo
    (tmVarErgebnis, window, vBox, notebook) <- liftIO $ do
        tmVarErgebnisIO <- atomically newEmptyTMVar
        windowIO <- Gtk.windowNew Gtk.WindowTypeToplevel
        parentWindow <- erhalteWindow parent
        Gtk.setWindowTransientFor windowIO parentWindow
        Gtk.setWindowModal windowIO True
        Gtk.onWidgetDeleteEvent windowIO $ \_event -> liftIO $ do
            atomically (putTMVar tmVarErgebnisIO HinzufügenBeenden)
            pure True
        vBoxIO <- containerAddWidgetNew windowIO $ Gtk.boxNew Gtk.OrientationVertical 0
        notebookIO
            <- boxPackWidgetNew vBoxIO PackGrow paddingDefault positionDefault Gtk.notebookNew
        pure (tmVarErgebnisIO, windowIO, vBoxIO, notebookIO)
    -- Wird in diesem Thread benötigt, bevor es erzeugt gepackt wird
    -- Führt zu Deadlock (thread blocked indefinitely in an MVar operation), wenn mdo verwendet wird
    zugtypAuswahl
        <- widgetShowNew $ auswahlComboBoxNew unterstützteZugtypen maybeTVar Language.zugtyp
    indexSeiten <- foldM
        (\acc (konstruktor, name) -> do
             (seite, seitenIndex) <- notebookAppendPageNew notebook maybeTVar name konstruktor
             pure $ Map.insert seitenIndex seite acc)
        Map.empty
        [ (hinzufügenBahngeschwindigkeitNew zugtypAuswahl maybeTVar, Language.bahngeschwindigkeit)
        , (hinzufügenStreckenabschnittNew maybeTVar, Language.streckenabschnitt)
        , (hinzufügenWeicheNew zugtypAuswahl maybeTVar, Language.weiche)
        , (hinzufügenKupplungNew maybeTVar, Language.kupplung)
        , (hinzufügenKontaktNew maybeTVar, Language.kontakt)
        , (hinzufügenWegstreckeNew zugtypAuswahl maybeTVar, Language.wegstrecke)
        , (hinzufügenPlanNew window zugtypAuswahl maybeTVar, Language.plan)]
    let assistantHinzufügen =
            AssistantHinzufügen
            { window
            , notebook
            , fließendAuswahl
            , zugtypAuswahl
            , indexSeiten
            , tmVarErgebnis
            }
    functionBox <- liftIO
        $ boxPackWidgetNew vBox packingDefault paddingDefault End
        $ Gtk.boxNew Gtk.OrientationHorizontal 0
    statusVar <- erhalteStatusVar :: m StatusVarGui
    buttonHinzufügen <- liftIO $ do
        buttonHinzufügenIO <- widgetShowNew Gtk.buttonNew
        let alleButtonHinzufügen =
                ButtonHinzufügen buttonHinzufügenIO
                : catMaybes (spezifischerButtonHinzufügen <$> Map.elems indexSeiten)
        forM_ alleButtonHinzufügen $ \mitButton -> do
            button <- erhalteButton mitButton
            boxPackDefault functionBox button
            Gtk.onButtonClicked button
                $ flip runReaderT statusVar
                $ hinzufügenErgebnis assistantHinzufügen
        Gtk.onNotebookSwitchPage notebook $ \_widget pageIndex -> do
            mapM_ mitWidgetHide alleButtonHinzufügen
            case Map.lookup (fromIntegral pageIndex) indexSeiten
                >>= spezifischerButtonHinzufügen of
                    (Just button) -> mitWidgetShow button
                    _otherwise -> mitWidgetShow buttonHinzufügenIO
        pure buttonHinzufügenIO
    fließendAuswahl <- boxPackWidgetNewDefault functionBox $ fließendAuswahlNew maybeTVar
    boxPackDefault functionBox zugtypAuswahl
    boxPackWidgetNew functionBox packingDefault paddingDefault End
        $ buttonNewWithEventLabel maybeTVar Language.abbrechen
        $ atomically
        $ putTMVar tmVarErgebnis HinzufügenAbbrechen
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.setWindowTitle window $ Language.hinzufügen sprache
        Gtk.setButtonLabel buttonHinzufügen $ Language.hinzufügen sprache
    pure assistantHinzufügen
#endif
--
