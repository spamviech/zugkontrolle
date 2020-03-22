{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

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
import qualified Graphics.UI.Gtk as Gtk

import Zug.Enums (ZugtypEither(..), Zugtyp(..), Fahrtrichtung(Vorwärts))
import Zug.Language (Sprache())
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OBahngeschwindigkeit))
import Zug.Plan (Aktion(ABahngeschwindigkeitMärklin, ABahngeschwindigkeitLego)
               , AktionBahngeschwindigkeit(..))
import Zug.UI.Gtk.Auswahl (AuswahlWidget, boundedEnumAuswahlRadioButtonNew, aktuelleAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen
       (widgetShowNew, boxPackWidgetNewDefault, boxPackDefault, boxPack, Packing(PackGrow)
      , paddingDefault, positionDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget), mitWidgetShow, mitWidgetHide, MitBox())
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..))
import Zug.UI.Gtk.StreckenObjekt (DynamischeWidgets(..), DynamischeWidgetsReader(..))
import Zug.UI.Gtk.ZugtypSpezifisch (zugtypSpezifischNew)

aktionBahngeschwindigkeitAuswahlPackNew
    :: (MitBox b, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
    => b
    -> Gtk.Window
    -> AuswahlWidget Zugtyp
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> IO ()
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
                    showBG
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
                Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
                flip runReaderT spracheGui $ case maybeObjekt of
                    (Just (OBahngeschwindigkeit (ZugtypMärklin bg))) -> aktionHinzufügen
                        $ ABahngeschwindigkeitMärklin
                        $ Geschwindigkeit bg wert
                    (Just (OBahngeschwindigkeit (ZugtypLego bg)))
                        -> aktionHinzufügen $ ABahngeschwindigkeitLego $ Geschwindigkeit bg wert
                    _sonst -> pure ()
    boxPack
        hBoxBahngeschwindigkeit
        scaleBahngeschwindigkeit
        PackGrow
        paddingDefault
        positionDefault
    buttonUmdrehen <- buttonNewWithEventLabel maybeTVar Language.umdrehen $ void $ forkIO $ do
        Gtk.postGUIAsync $ do
            showBG
            mitWidgetShow windowObjektAuswahl
        maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
        Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
        flip runReaderT spracheGui $ case maybeObjekt of
            (Just (OBahngeschwindigkeit (ZugtypMärklin bg)))
                -> aktionHinzufügen $ ABahngeschwindigkeitMärklin $ Umdrehen bg
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
                    showBG
                    mitWidgetShow windowObjektAuswahl
                maybeObjekt <- atomically $ takeTMVar tmvarPlanObjekt
                Gtk.postGUIAsync $ mitWidgetHide windowObjektAuswahl
                flip runReaderT spracheGui $ case maybeObjekt of
                    (Just (OBahngeschwindigkeit (ZugtypLego bg))) -> aktionHinzufügen
                        $ ABahngeschwindigkeitLego
                        $ FahrtrichtungEinstellen bg fahrtrichtung
                    _sonst -> pure ()
    boxPackDefault hBoxFahrtrichtung auswahlFahrtrichtung
    boxPackWidgetNewDefault hBoxBahngeschwindigkeit
        $ zugtypSpezifischNew
            [(Märklin, erhalteWidget buttonUmdrehen), (Lego, erhalteWidget hBoxFahrtrichtung)]
            auswahlZugtyp
    pure hBoxBahngeschwindigkeit
#endif
--
