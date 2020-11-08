{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description: Seite zum Hinzufügen einer 'Kontakt'-'Aktion'.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.AktionKontakt (aktionKontaktAuswahlPackNew) where

import Control.Concurrent.STM (atomically, takeTMVar)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO())
import qualified Data.GI.Gtk.Threading as Gtk
import qualified GI.Gtk as Gtk

import Zug.Enums (ZugtypEither(..))
import Zug.Language (MitSprache(leseSprache))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OKontakt, OWegstrecke))
import Zug.Plan (AktionAllgemein(AKontakt, AWegstreckeMärklin, AWegstreckeLego), Aktion
               , AktionKontakt(..), AktionWegstrecke(AWSKontakt))
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))
import Zug.Util (forkIOSilent)

-- | Erzeuge die Widgets zur Auswahl einer 'Kupplung's-'Aktion'.
aktionKontaktAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
    => b
    -> Gtk.Window
    -> Maybe TVarSprachewechselAktionen
    -> IO ()
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.Box
aktionKontaktAuswahlPackNew box windowObjektAuswahl maybeTVar showKU aktionHinzufügen = do
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {dynTMVarPlanObjekt} <- erhalteDynamischeWidgets
    hBoxKontakt <- boxPackWidgetNewDefault box $ Gtk.boxNew Gtk.OrientationHorizontal 0
    boxPackWidgetNewDefault hBoxKontakt
        $ buttonNewWithEventLabel maybeTVar Language.wartenAuf
        $ void
        $ forkIOSilent
        $ do
            Gtk.postGUIASync $ flip leseSprache spracheGui $ \sprache -> do
                Gtk.setWindowTitle windowObjektAuswahl $ Language.wartenAuf sprache
                showKU
                mitWidgetShow windowObjektAuswahl
            maybeObjekt <- atomically $ takeTMVar dynTMVarPlanObjekt
            Gtk.postGUIASync $ mitWidgetHide windowObjektAuswahl
            flip runReaderT spracheGui $ case maybeObjekt of
                (Just (OKontakt ko)) -> aktionHinzufügen $ AKontakt $ WartenAuf ko
                (Just (OWegstrecke (ZugtypMärklin ws)))
                    -> aktionHinzufügen $ AWegstreckeMärklin $ AWSKontakt $ WartenAuf ws
                (Just (OWegstrecke (ZugtypLego ws)))
                    -> aktionHinzufügen $ AWegstreckeLego $ AWSKontakt $ WartenAuf ws
                _sonst -> pure ()
    pure hBoxKontakt
