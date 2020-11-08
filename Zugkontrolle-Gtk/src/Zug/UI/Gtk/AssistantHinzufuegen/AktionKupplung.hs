{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description: Seite zum Hinzufügen einer 'Kupplung's-'Aktion'.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.AktionKupplung (aktionKupplungAuswahlPackNew) where

import Control.Concurrent.STM (atomically, takeTMVar)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO())
import qualified Data.GI.Gtk.Threading as Gtk
import qualified GI.Gtk as Gtk

import Zug.Enums (ZugtypEither(..))
import Zug.Language (MitSprache(leseSprache))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OKupplung, OWegstrecke))
import Zug.Plan (AktionAllgemein(AKupplung, AWegstreckeMärklin, AWegstreckeLego), Aktion
               , AktionKupplung(..), AktionWegstrecke(AWSKupplung))
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))
import Zug.Util (forkIOSilent)

-- | Erzeuge die Widgets zur Auswahl einer 'Kupplung's-'Aktion'.
aktionKupplungAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
    => b
    -> Gtk.Window
    -> Maybe TVarSprachewechselAktionen
    -> IO ()
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.Box
aktionKupplungAuswahlPackNew box windowObjektAuswahl maybeTVar showKU aktionHinzufügen = do
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {dynTMVarPlanObjekt} <- erhalteDynamischeWidgets
    hBoxKupplung <- boxPackWidgetNewDefault box $ Gtk.boxNew Gtk.OrientationHorizontal 0
    boxPackWidgetNewDefault hBoxKupplung
        $ buttonNewWithEventLabel maybeTVar Language.kuppeln
        $ void
        $ forkIOSilent
        $ do
            Gtk.postGUIASync $ flip leseSprache spracheGui $ \sprache -> do
                Gtk.setWindowTitle windowObjektAuswahl $ Language.kuppeln sprache
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
