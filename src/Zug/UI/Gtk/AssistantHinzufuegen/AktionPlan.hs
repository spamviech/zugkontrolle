{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

{-|
Description: Seite zum Hinzufügen einer 'Plan'-'Aktion'.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.AktionPlan
  (
#ifdef ZUGKONTROLLEGUI
    aktionPlanAuswahlPackNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TVar, takeTMVar)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(..))
import GI.Gtk (AttrOp((:=)))
import qualified GI.Gtk as Gtk

import Zug.Language (Sprache(), MitSprache(leseSprache))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OPlan))
import Zug.Plan (AktionAllgemein(AktionAusführen), Aktion)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..))
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))

-- | Erzeuge die Widgets zur Auswahl einer 'Plan'-'Aktion'.
aktionPlanAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
    => b
    -> Gtk.Window
    -> Maybe TVarSprachewechselAktionen
    -> IO ()
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.HBox
aktionPlanAuswahlPackNew box windowObjektAuswahl maybeTVar showPL aktionHinzufügen = do
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {dynTMVarPlanObjekt} <- erhalteDynamischeWidgets
    hBoxPlan <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
    boxPackWidgetNewDefault hBoxPlan
        $ buttonNewWithEventLabel maybeTVar Language.ausführen
        $ void
        $ forkIO
        $ do
            Gtk.postGUIASync $ do
                Gtk.set
                    windowObjektAuswahl
                    [Gtk.windowTitle := leseSprache Language.ausführen spracheGui]
                showPL
                mitWidgetShow windowObjektAuswahl
            maybeObjekt <- atomically $ takeTMVar dynTMVarPlanObjekt
            Gtk.postGUIASync $ mitWidgetHide windowObjektAuswahl
            flip runReaderT spracheGui $ case maybeObjekt of
                (Just (OPlan pl)) -> aktionHinzufügen $ AktionAusführen pl
                _sonst -> pure ()
    pure hBoxPlan
#endif
--
