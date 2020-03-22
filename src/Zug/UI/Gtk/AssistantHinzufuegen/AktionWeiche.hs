{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

module Zug.UI.Gtk.AssistantHinzufuegen.AktionWeiche
  (
#ifdef ZUGKONTROLLEGUI
    aktionWeicheAuswahlPackNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TVar, takeTMVar)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Enums (Richtung())
import Zug.Language (Sprache(), MitSprache(leseSprache), (<:>))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OWeiche))
import Zug.Plan (Aktion(AWeiche), AktionWeiche(..))
import Zug.UI.Gtk.Auswahl (boundedEnumAuswahlRadioButtonNew, aktuelleAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen
       (widgetShowNew, boxPackWidgetNewDefault, boxPackDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..))
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))

aktionWeicheAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
    => b
    -> Gtk.Window
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> NonEmpty (Richtung, IO ())
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.HBox
aktionWeicheAuswahlPackNew box windowObjektAuswahl maybeTVar showRichtungen aktionHinzufügen = do
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {tmvarPlanObjekt} <- erhalteDynamischeWidgets
    hBoxWeiche <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
    auswahlRichtung <- widgetShowNew
        $ boundedEnumAuswahlRadioButtonNew (fst $ NonEmpty.head showRichtungen) maybeTVar
        $ const Text.empty
    boxPackWidgetNewDefault hBoxWeiche
        $ buttonNewWithEventLabel maybeTVar Language.stellen
        $ void
        $ do
            richtung <- aktuelleAuswahl auswahlRichtung
            forkIO $ do
                Gtk.postGUIAsync $ do
                    Gtk.set
                        windowObjektAuswahl
                        [Gtk.windowTitle := leseSprache (Language.stellen <:> richtung) spracheGui]
                    snd $ head $ NonEmpty.filter ((== richtung) . fst) showRichtungen
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
                Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
                flip runReaderT spracheGui $ case maybeObjekt of
                    (Just (OWeiche we)) -> aktionHinzufügen $ AWeiche $ Stellen we richtung
                    _sonst -> pure ()
    boxPackDefault hBoxWeiche auswahlRichtung
    pure hBoxWeiche
#endif
--
