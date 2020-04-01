{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

{-|
Description: Seite zum Hinzufügen einer 'Bahngeschwindigkeit's-'Aktion'.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.AktionBahngeschwindigkeit
  (
#ifdef ZUGKONTROLLEGUI
    aktionBahngeschwindigkeitAuswahlPackNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TVar, takeTMVar)
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Enums
       (ZugtypEither(..), Zugtyp(..), GeschwindigkeitVariante(..), GeschwindigkeitEither(..)
      , GeschwindigkeitPhantom(..), Fahrtrichtung(Vorwärts), Strom(Fließend))
import Zug.Language (Sprache(), MitSprache(leseSprache), (<:>))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OBahngeschwindigkeit, OWegstrecke))
import Zug.Plan
       (Aktion(ABahngeschwindigkeitMärklinPwm,
       ABahngeschwindigkeitMärklinKonstanteSpannung,
       ABahngeschwindigkeitLegoPwm,
       ABahngeschwindigkeitLegoKonstanteSpannung, AWegstreckeMärklin,
       AWegstreckeLego)
      , AktionBahngeschwindigkeit(..), AktionWegstrecke(AWSBahngeschwindigkeit))
import Zug.UI.Gtk.Auswahl (AuswahlWidget, boundedEnumAuswahlRadioButtonNew, aktuelleAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen
       (widgetShowNew, boxPackWidgetNewDefault, boxPackDefault, boxPack, Packing(PackGrow)
      , paddingDefault, positionDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget), mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..))
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))
import Zug.UI.Gtk.ZugtypSpezifisch (zugtypSpezifischNew)

-- | Erzeuge die Widgets zur Auswahl einer 'Bahngeschwindigkeit's-'Aktion'.
aktionBahngeschwindigkeitAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
    => b
    -> Gtk.Window
    -> AuswahlWidget Zugtyp
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> (Maybe GeschwindigkeitVariante -> IO ())
    -> (forall rr mm. (SpracheGuiReader rr mm, MonadIO mm) => Aktion -> mm ())
    -> m Gtk.HBox
aktionBahngeschwindigkeitAuswahlPackNew
    box
    windowObjektAuswahl
    auswahlZugtyp
    maybeTVar
    showBG
    aktionHinzufügen = do
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets {tmvarPlanObjekt} <- erhalteDynamischeWidgets
    (hBoxBahngeschwindigkeit, scaleBahngeschwindigkeit) <- liftIO $ do
        hBoxBahngeschwindigkeit <- boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
        scaleBahngeschwindigkeit <- widgetShowNew $ Gtk.hScaleNewWithRange 0 100 1
        pure (hBoxBahngeschwindigkeit, scaleBahngeschwindigkeit)
    boxPackWidgetNewDefault hBoxBahngeschwindigkeit
        $ buttonNewWithEventLabel maybeTVar Language.geschwindigkeit
        $ void
        $ do
            wert <- floor <$> Gtk.get scaleBahngeschwindigkeit Gtk.rangeValue
            forkIO $ do
                Gtk.postGUIAsync $ do
                    Gtk.set
                        windowObjektAuswahl
                        [ Gtk.windowTitle
                              := leseSprache (Language.geschwindigkeit <:> wert) spracheGui]
                    showBG $ Just Pwm
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
                Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
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
    boxPack
        hBoxBahngeschwindigkeit
        scaleBahngeschwindigkeit
        PackGrow
        paddingDefault
        positionDefault
    auswahlFahrstrom
        <- widgetShowNew $ boundedEnumAuswahlRadioButtonNew Fließend maybeTVar $ const Text.empty
    boxPackWidgetNewDefault hBoxBahngeschwindigkeit
        $ buttonNewWithEventLabel maybeTVar Language.fahrstrom
        $ void
        $ do
            strom <- aktuelleAuswahl auswahlFahrstrom
            forkIO $ do
                Gtk.postGUIAsync $ do
                    Gtk.set
                        windowObjektAuswahl
                        [Gtk.windowTitle := leseSprache (Language.fahrstrom <:> strom) spracheGui]
                    showBG $ Just KonstanteSpannung
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
                Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
                flip runReaderT spracheGui $ case maybeObjekt of
                    (Just
                         (OBahngeschwindigkeit
                              (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bg))))
                        -> aktionHinzufügen
                        $ ABahngeschwindigkeitMärklinKonstanteSpannung
                        $ Fahrstrom bg strom
                    (Just (OBahngeschwindigkeit (ZugtypLego (GeschwindigkeitKonstanteSpannung bg))))
                        -> aktionHinzufügen
                        $ ABahngeschwindigkeitLegoKonstanteSpannung
                        $ Fahrstrom bg strom
                    (Just (OWegstrecke (ZugtypMärklin ws))) -> aktionHinzufügen
                        $ AWegstreckeMärklin
                        $ AWSBahngeschwindigkeit
                        $ GeschwindigkeitKonstanteSpannung
                        $ Fahrstrom (GeschwindigkeitPhantom ws) strom
                    (Just (OWegstrecke (ZugtypLego ws))) -> aktionHinzufügen
                        $ AWegstreckeLego
                        $ AWSBahngeschwindigkeit
                        $ GeschwindigkeitKonstanteSpannung
                        $ Fahrstrom (GeschwindigkeitPhantom ws) strom
                    _sonst -> pure ()
    boxPackDefault hBoxBahngeschwindigkeit auswahlFahrstrom
    buttonUmdrehen <- buttonNewWithEventLabel maybeTVar Language.umdrehen $ void $ forkIO $ do
        Gtk.postGUIAsync $ do
            Gtk.set
                windowObjektAuswahl
                [Gtk.windowTitle := leseSprache Language.umdrehen spracheGui]
            showBG Nothing
            mitWidgetShow windowObjektAuswahl
        maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
        Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
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
    hBoxFahrtrichtung <- liftIO $ Gtk.hBoxNew False 0
    auswahlFahrtrichtung
        <- widgetShowNew $ boundedEnumAuswahlRadioButtonNew Vorwärts maybeTVar $ const Text.empty
    boxPackWidgetNewDefault hBoxFahrtrichtung
        $ buttonNewWithEventLabel maybeTVar Language.fahrtrichtungEinstellen
        $ void
        $ do
            fahrtrichtung <- aktuelleAuswahl auswahlFahrtrichtung
            forkIO $ do
                Gtk.postGUIAsync $ do
                    Gtk.set
                        windowObjektAuswahl
                        [ Gtk.windowTitle
                              := leseSprache
                                  (Language.fahrtrichtungEinstellen <:> fahrtrichtung)
                                  spracheGui]
                    showBG Nothing
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
                Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
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
    boxPackDefault hBoxFahrtrichtung auswahlFahrtrichtung
    boxPackWidgetNewDefault hBoxBahngeschwindigkeit
        $ zugtypSpezifischNew
            [(Märklin, erhalteWidget buttonUmdrehen), (Lego, erhalteWidget hBoxFahrtrichtung)]
            auswahlZugtyp
    pure hBoxBahngeschwindigkeit
#endif
--
