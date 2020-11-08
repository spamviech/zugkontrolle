{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}

{-|
Description: Seite zum Hinzufügen einer 'Streckenabschnitt's-'Aktion'.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.AktionStreckenabschnitt
  ( aktionStreckenabschnittAuswahlPackNew
  ) where

import Control.Concurrent.STM (atomically, takeTMVar)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix())
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO())
import qualified Data.GI.Gtk.Threading as Gtk
import qualified Data.Text as Text
import qualified GI.Gtk as Gtk

import Zug.Enums (ZugtypEither(..), Strom(Fließend))
import Zug.Language (MitSprache(leseSprache), (<:>))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OStreckenabschnitt, OWegstrecke))
import Zug.Plan
       (AktionAllgemein(AStreckenabschnitt, AWegstreckeMärklin,
                AWegstreckeLego)
      , Aktion, AktionStreckenabschnitt(..), AktionWegstrecke(AWSStreckenabschnitt))
import Zug.UI.Gtk.Auswahl (boundedEnumAuswahlRadioButtonNew, aktuelleAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))
import Zug.Util (forkIOSilent)

-- | Erzeuge die Widgets zur Auswahl einer 'Streckenabschnitt'-'Aktion'.
aktionStreckenabschnittAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadFix m, MonadIO m)
    => b
    -> Gtk.Window
    -> Maybe TVarSprachewechselAktionen
    -> IO ()
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.Box
aktionStreckenabschnittAuswahlPackNew box windowObjektAuswahl maybeTVar showST aktionHinzufügen = mdo
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {dynTMVarPlanObjekt} <- erhalteDynamischeWidgets
    hBoxStreckenabschnitt <- boxPackWidgetNewDefault box $ Gtk.boxNew Gtk.OrientationHorizontal 0
    boxPackWidgetNewDefault hBoxStreckenabschnitt
        $ buttonNewWithEventLabel maybeTVar Language.strom
        $ void
        $ do
            strom <- aktuelleAuswahl auswahlStrom
            forkIOSilent $ do
                Gtk.postGUIASync $ flip leseSprache spracheGui $ \sprache -> do
                    Gtk.setWindowTitle windowObjektAuswahl $ Language.strom <:> strom $ sprache
                    showST
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar dynTMVarPlanObjekt
                Gtk.postGUIASync $ mitWidgetHide windowObjektAuswahl
                flip runReaderT spracheGui $ case maybeObjekt of
                    (Just (OStreckenabschnitt st))
                        -> aktionHinzufügen $ AStreckenabschnitt $ Strom st strom
                    (Just (OWegstrecke (ZugtypMärklin ws))) -> aktionHinzufügen
                        $ AWegstreckeMärklin
                        $ AWSStreckenabschnitt
                        $ Strom ws strom
                    (Just (OWegstrecke (ZugtypLego ws))) -> aktionHinzufügen
                        $ AWegstreckeLego
                        $ AWSStreckenabschnitt
                        $ Strom ws strom
                    _sonst -> pure ()
    auswahlStrom <- boxPackWidgetNewDefault hBoxStreckenabschnitt
        $ boundedEnumAuswahlRadioButtonNew Fließend maybeTVar
        $ const Text.empty
    pure hBoxStreckenabschnitt
