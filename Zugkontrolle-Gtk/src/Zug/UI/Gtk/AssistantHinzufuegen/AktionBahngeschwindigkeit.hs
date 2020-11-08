{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}

{-|
Description: Seite zum Hinzufügen einer 'Bahngeschwindigkeit's-'Aktion'.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.AktionBahngeschwindigkeit
  ( aktionBahngeschwindigkeitAuswahlPackNew
  ) where

import Control.Concurrent.STM (atomically, takeTMVar)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix())
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.GI.Gtk.Threading as Gtk
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified GI.Gtk as Gtk

import Zug.Enums (ZugtypEither(..), Zugtyp(..), GeschwindigkeitVariante(..)
                , GeschwindigkeitEither(..), GeschwindigkeitPhantom(..), Fahrtrichtung(Vorwärts))
import Zug.Language (MitSprache(leseSprache), (<:>))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OBahngeschwindigkeit, OWegstrecke))
import Zug.Plan
       (AktionAllgemein(ABahngeschwindigkeitMärklinPwm,
                ABahngeschwindigkeitMärklinKonstanteSpannung,
                ABahngeschwindigkeitLegoPwm,
                ABahngeschwindigkeitLegoKonstanteSpannung, AWegstreckeMärklin,
                AWegstreckeLego), Aktion
      , AktionBahngeschwindigkeit(..), AktionWegstrecke(AWSBahngeschwindigkeit))
import Zug.UI.Gtk.Auswahl (AuswahlWidget, boundedEnumAuswahlRadioButtonNew, aktuelleAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, boxPackWidgetNew, Packing(PackGrow)
                                 , paddingDefault, positionDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget), mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))
import Zug.UI.Gtk.ZugtypSpezifisch (zugtypSpezifischNew)
import Zug.Util (forkIOSilent)

-- | Erzeuge die Widgets zur Auswahl einer 'Bahngeschwindigkeit's-'Aktion'.
aktionBahngeschwindigkeitAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadFix m, MonadIO m)
    => b
    -> Gtk.Window
    -> AuswahlWidget Zugtyp
    -> Maybe TVarSprachewechselAktionen
    -> (Maybe GeschwindigkeitVariante -> IO ())
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.Box
aktionBahngeschwindigkeitAuswahlPackNew
    box
    windowObjektAuswahl
    auswahlZugtyp
    maybeTVar
    showBG
    aktionHinzufügen = mdo
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {dynTMVarPlanObjekt} <- erhalteDynamischeWidgets
    hBoxBahngeschwindigkeit <- boxPackWidgetNewDefault box $ Gtk.boxNew Gtk.OrientationHorizontal 0
    boxPackWidgetNewDefault hBoxBahngeschwindigkeit
        $ buttonNewWithEventLabel maybeTVar Language.geschwindigkeit
        $ void
        $ do
            adjustment <- Gtk.getRangeAdjustment scaleBahngeschwindigkeit
            wert <- floor <$> Gtk.getAdjustmentValue adjustment
            forkIOSilent $ do
                Gtk.postGUIASync $ flip leseSprache spracheGui $ \sprache -> do
                    Gtk.setWindowTitle windowObjektAuswahl
                        $ Language.geschwindigkeit <:> wert
                        $ sprache
                    showBG $ Just Pwm
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar dynTMVarPlanObjekt
                Gtk.postGUIASync $ mitWidgetHide windowObjektAuswahl
                flip runReaderT spracheGui $ case maybeObjekt of
                    (Just (OBahngeschwindigkeit (ZugtypMärklin (GeschwindigkeitPwm bg))))
                        -> aktionHinzufügen
                        $ ABahngeschwindigkeitMärklinPwm
                        $ Geschwindigkeit bg wert
                    (Just (OBahngeschwindigkeit (ZugtypLego (GeschwindigkeitPwm bg))))
                        -> aktionHinzufügen
                        $ ABahngeschwindigkeitLegoPwm
                        $ Geschwindigkeit bg wert
                    (Just (OWegstrecke (ZugtypMärklin ws))) -> aktionHinzufügen
                        $ AWegstreckeMärklin
                        $ AWSBahngeschwindigkeit
                        $ GeschwindigkeitPwm
                        $ Geschwindigkeit (GeschwindigkeitPhantom ws) wert
                    (Just (OWegstrecke (ZugtypLego ws))) -> aktionHinzufügen
                        $ AWegstreckeLego
                        $ AWSBahngeschwindigkeit
                        $ GeschwindigkeitPwm
                        $ Geschwindigkeit (GeschwindigkeitPhantom ws) wert
                    _sonst -> pure ()
    scaleBahngeschwindigkeit
        <- boxPackWidgetNew hBoxBahngeschwindigkeit PackGrow paddingDefault positionDefault
        $ Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0 100 1
    boxPackWidgetNewDefault hBoxBahngeschwindigkeit
        $ buttonNewWithEventLabel maybeTVar Language.fahrstrom
        $ void
        $ do
            fahrstromAnschluss <- floor <$> Gtk.getSpinButtonValue spinButtonFahrstrom
            forkIOSilent $ do
                Gtk.postGUIASync $ flip leseSprache spracheGui $ \sprache -> do
                    Gtk.setWindowTitle windowObjektAuswahl
                        $ Language.fahrstrom <:> fahrstromAnschluss
                        $ sprache
                    showBG $ Just KonstanteSpannung
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar dynTMVarPlanObjekt
                Gtk.postGUIASync $ mitWidgetHide windowObjektAuswahl
                flip runReaderT spracheGui $ case maybeObjekt of
                    (Just
                         (OBahngeschwindigkeit
                              (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bg))))
                        -> aktionHinzufügen
                        $ ABahngeschwindigkeitMärklinKonstanteSpannung
                        $ Fahrstrom bg fahrstromAnschluss
                    (Just (OBahngeschwindigkeit (ZugtypLego (GeschwindigkeitKonstanteSpannung bg))))
                        -> aktionHinzufügen
                        $ ABahngeschwindigkeitLegoKonstanteSpannung
                        $ Fahrstrom bg fahrstromAnschluss
                    (Just (OWegstrecke (ZugtypMärklin ws))) -> aktionHinzufügen
                        $ AWegstreckeMärklin
                        $ AWSBahngeschwindigkeit
                        $ GeschwindigkeitKonstanteSpannung
                        $ Fahrstrom (GeschwindigkeitPhantom ws) fahrstromAnschluss
                    (Just (OWegstrecke (ZugtypLego ws))) -> aktionHinzufügen
                        $ AWegstreckeLego
                        $ AWSBahngeschwindigkeit
                        $ GeschwindigkeitKonstanteSpannung
                        $ Fahrstrom (GeschwindigkeitPhantom ws) fahrstromAnschluss
                    _sonst -> pure ()
    spinButtonFahrstrom <- boxPackWidgetNewDefault hBoxBahngeschwindigkeit
        $ Gtk.spinButtonNewWithRange 0 (fromIntegral (maxBound :: Word8)) 1
    buttonUmdrehen <- buttonNewWithEventLabel maybeTVar Language.umdrehen $ void $ forkIOSilent $ do
        Gtk.postGUIASync $ flip leseSprache spracheGui $ \sprache -> do
            Gtk.setWindowTitle windowObjektAuswahl $ Language.umdrehen sprache
            showBG Nothing
            mitWidgetShow windowObjektAuswahl
        maybeObjekt <- atomically $ takeTMVar dynTMVarPlanObjekt
        Gtk.postGUIASync $ mitWidgetHide windowObjektAuswahl
        flip runReaderT spracheGui $ case maybeObjekt of
            (Just (OBahngeschwindigkeit (ZugtypMärklin (GeschwindigkeitPwm bg))))
                -> aktionHinzufügen $ ABahngeschwindigkeitMärklinPwm $ Umdrehen bg
            (Just (OBahngeschwindigkeit (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bg))))
                -> aktionHinzufügen $ ABahngeschwindigkeitMärklinKonstanteSpannung $ Umdrehen bg
            (Just (OWegstrecke (ZugtypMärklin ws))) -> aktionHinzufügen
                $ AWegstreckeMärklin
                $ AWSBahngeschwindigkeit
                $ GeschwindigkeitPwm
                $ Umdrehen (GeschwindigkeitPhantom ws)
            _sonst -> pure ()
    hBoxFahrtrichtung <- Gtk.boxNew Gtk.OrientationHorizontal 0
    boxPackWidgetNewDefault hBoxFahrtrichtung
        $ buttonNewWithEventLabel maybeTVar Language.fahrtrichtungEinstellen
        $ void
        $ do
            fahrtrichtung <- aktuelleAuswahl auswahlFahrtrichtung
            forkIOSilent $ do
                Gtk.postGUIASync $ flip leseSprache spracheGui $ \sprache -> do
                    Gtk.setWindowTitle windowObjektAuswahl
                        $ Language.fahrtrichtungEinstellen <:> fahrtrichtung
                        $ sprache
                    showBG Nothing
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar dynTMVarPlanObjekt
                Gtk.postGUIASync $ mitWidgetHide windowObjektAuswahl
                flip runReaderT spracheGui $ case maybeObjekt of
                    (Just (OBahngeschwindigkeit (ZugtypLego (GeschwindigkeitPwm bg))))
                        -> aktionHinzufügen
                        $ ABahngeschwindigkeitLegoPwm
                        $ FahrtrichtungEinstellen bg fahrtrichtung
                    (Just (OBahngeschwindigkeit (ZugtypLego (GeschwindigkeitKonstanteSpannung bg))))
                        -> aktionHinzufügen
                        $ ABahngeschwindigkeitLegoKonstanteSpannung
                        $ FahrtrichtungEinstellen bg fahrtrichtung
                    (Just (OWegstrecke (ZugtypLego ws))) -> aktionHinzufügen
                        $ AWegstreckeLego
                        $ AWSBahngeschwindigkeit
                        $ GeschwindigkeitPwm
                        $ FahrtrichtungEinstellen (GeschwindigkeitPhantom ws) fahrtrichtung
                    _sonst -> pure ()
    auswahlFahrtrichtung <- boxPackWidgetNewDefault hBoxFahrtrichtung
        $ boundedEnumAuswahlRadioButtonNew Vorwärts maybeTVar
        $ const Text.empty
    widgetMärklin <- erhalteWidget buttonUmdrehen
    widgetLego <- erhalteWidget hBoxFahrtrichtung
    boxPackWidgetNewDefault hBoxBahngeschwindigkeit
        $ zugtypSpezifischNew [(Märklin, widgetMärklin), (Lego, widgetLego)] auswahlZugtyp
    pure hBoxBahngeschwindigkeit
