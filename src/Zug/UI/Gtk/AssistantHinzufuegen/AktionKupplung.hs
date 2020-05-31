{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

{-|
Description: Seite zum Hinzufügen einer 'Kupplung's-'Aktion'.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.AktionKupplung
  (
#ifdef ZUGKONTROLLEGUI
    aktionKupplungAuswahlPackNew
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

import Zug.Enums (ZugtypEither(..))
import Zug.Language (Sprache(), MitSprache(leseSprache))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OKupplung, OWegstrecke))
import Zug.Plan (AktionAllgemein(AKupplung, AWegstreckeMärklin, AWegstreckeLego), Aktion
               , AktionKupplung(..), AktionWegstrecke(AWSKupplung))
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..))
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))

-- | Erzeuge die Widgets zur Auswahl einer 'Kupplung's-'Aktion'.
aktionKupplungAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
    => b
    -> Gtk.Window
    -> Maybe TVarSprachewechselAktionen
    -> IO ()
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.HBox
aktionKupplungAuswahlPackNew box windowObjektAuswahl maybeTVar showKU aktionHinzufügen = do
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {dynTMVarPlanObjekt} <- erhalteDynamischeWidgets
    hBoxKupplung <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
    boxPackWidgetNewDefault hBoxKupplung
        $ buttonNewWithEventLabel maybeTVar Language.kuppeln
        $ void
        $ forkIO
        $ do
            Gtk.postGUIASync $ do
                Gtk.set
                    windowObjektAuswahl
                    [Gtk.windowTitle := leseSprache Language.kuppeln spracheGui]
                showKU
                mitWidgetShow windowObjektAuswahl
            maybeObjekt <- atomically $ takeTMVar dynTMVarPlanObjekt
            Gtk.postGUIASync $ mitWidgetHide windowObjektAuswahl
            flip runReaderT spracheGui $ case maybeObjekt of
                (Just (OKupplung ku)) -> aktionHinzufügen $ AKupplung $ Kuppeln ku
                (Just (OWegstrecke (ZugtypMärklin ws)))
                    -> aktionHinzufügen $ AWegstreckeMärklin $ AWSKupplung $ Kuppeln ws
                (Just (OWegstrecke (ZugtypLego ws)))
                    -> aktionHinzufügen $ AWegstreckeLego $ AWSKupplung $ Kuppeln ws
                _sonst -> pure ()
    pure hBoxKupplung
#endif
--
