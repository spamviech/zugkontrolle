{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

{-|
Description: Seite zum Hinzufügen einer 'Wegstrecke'n-'Aktion'.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.AktionWegstrecke
  (
#ifdef ZUGKONTROLLEGUI
    aktionWegstreckeAuswahlPackNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TVar, takeTMVar)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Enums (ZugtypEither(..))
import Zug.Language (Sprache(), MitSprache(leseSprache))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OWegstrecke))
import Zug.Plan (Aktion(AWegstreckeMärklin, AWegstreckeLego), AktionWegstrecke(Einstellen))
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..))
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))

-- | Erzeuge die Widgets zur Auswahl einer 'Wegstrecke'n-'Aktion'.
aktionWegstreckeAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
    => b
    -> Gtk.Window
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> IO ()
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.HBox
aktionWegstreckeAuswahlPackNew box windowObjektAuswahl maybeTVar showWS aktionHinzufügen = do
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {tmvarPlanObjekt} <- erhalteDynamischeWidgets
    hBoxWegstrecke <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
    boxPackWidgetNewDefault hBoxWegstrecke
        $ buttonNewWithEventLabel maybeTVar Language.einstellen
        $ void
        $ forkIO
        $ do
            Gtk.postGUIAsync $ do
                Gtk.set
                    windowObjektAuswahl
                    [Gtk.windowTitle := leseSprache Language.einstellen spracheGui]
                showWS
                mitWidgetShow windowObjektAuswahl
            maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
            Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
            flip runReaderT spracheGui $ case maybeObjekt of
                (Just (OWegstrecke (ZugtypMärklin ws)))
                    -> aktionHinzufügen $ AWegstreckeMärklin $ Einstellen ws
                (Just (OWegstrecke (ZugtypLego ws)))
                    -> aktionHinzufügen $ AWegstreckeLego $ Einstellen ws
                _sonst -> pure ()
    pure hBoxWegstrecke
#endif
--
