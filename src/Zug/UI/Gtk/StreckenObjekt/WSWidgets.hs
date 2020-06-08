{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
#endif

module Zug.UI.Gtk.StreckenObjekt.WSWidgets
  (
#ifdef ZUGKONTROLLEGUI
    WSWidgets()
  , wegstreckePackNew
  , WSWidgetsBoxen(..)
  , MitWSWidgetsBoxen(..)
  , WSWidgetsBoxenReader(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word8)
import GI.Gtk (AttrOp((:=)))
import qualified GI.Gtk as Gtk

import Zug.Anbindung
       (StreckenObjekt(..), Bahngeschwindigkeit(..)
      , GeschwindigkeitsAnschlüsse(fahrstromAnschlüsse), BahngeschwindigkeitKlasse(..), PwmZugtyp
      , BahngeschwindigkeitContainer(..), verwendetPwm, Streckenabschnitt()
      , StreckenabschnittKlasse(..), StreckenabschnittContainer(..), Weiche(), WeicheContainer(..)
      , Kupplung(), KupplungKlasse(..), KupplungContainer(..), Kontakt(), KontaktKlasse(..)
      , KontaktContainer(..), Wegstrecke(..), WegstreckeKlasse(..), AnschlussEither(), I2CReader()
      , PwmReader(), InterruptReader())
import Zug.Enums
       (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(zuZugtypEither), mapZugtypEither, ausZugtypEither
      , GeschwindigkeitVariante(..), GeschwindigkeitEither(..), GeschwindigkeitPhantom(..)
      , ausGeschwindigkeitEither, catKonstanteSpannung, Fahrtrichtung(), Strom(Fließend))
import Zug.Language (Sprache(), (<:>), (<°>), (<^>))
import qualified Zug.Language as Language
import Zug.Objekt (Objekt, ObjektAllgemein(OWegstrecke), ObjektKlasse(..), ObjektElement(..))
import Zug.Plan (AktionWegstrecke(..))
import Zug.UI.Base (MStatusAllgemeinT, IOStatusAllgemein, entfernenWegstrecke, ObjektReader
                  , ReaderFamilie, MitTVarMaps)
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen), BefehlConstraints)
import Zug.UI.Gtk.Auswahl (AuswahlWidget, setzeAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNewDefault, namePackNew
                                 , labelSpracheNew, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove)
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui
       (SpracheGui, MitSpracheGui(), verwendeSpracheGui, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt.BGWidgets
       (BGWidgets, BGWidgetsKlasse(..), hScaleGeschwindigkeitPackNew, auswahlFahrstromPackNew
      , buttonUmdrehenPackNew, auswahlFahrtrichtungEinstellenPackNew)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen (PlanElement(..), entferneHinzufügenPlanWidgets
                                               , hinzufügenWidgetPlanPackNew, MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.KUWidgets (buttonKuppelnPackNew)
import Zug.UI.Gtk.StreckenObjekt.STWidgets
       (STWidgets, STWidgetsKlasse(..), toggleButtonStromPackNew)
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufuegen
       (Kategorie(..), KategorieText(..), BoxPlanHinzufügen, ButtonPlanHinzufügen
      , widgetHinzufügenZugtypEither)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), eventAusführen
      , buttonEntfernenPackNew, buttonBearbeitenPackNew, MitAktionBearbeiten())
import Zug.UI.StatusVar
       (StatusVar, MitStatusVar, StatusVarReader(erhalteStatusVar), ausführenStatusVarAktion)

instance Kategorie (WSWidgets z) where
    kategorie :: KategorieText (WSWidgets z)
    kategorie = KategorieText Language.wegstrecken

-- | 'Wegstrecke' mit zugehörigen Widgets
data WSWidgets (z :: Zugtyp) =
    WSWidgets
    { ws :: Wegstrecke z
    , wsWidget :: Gtk.Frame
    , wsFunctionBox :: Gtk.Box
    , wsHinzPL :: WegstreckePlanHinzufügenWidget z
    , wsTVarSprache :: TVarSprachewechselAktionen
    , wsTVarEvent :: TVar EventAusführen
    , wsScaleGeschwindigkeit :: Maybe Gtk.Scale
    , wsAuswahlFahrstrom :: Maybe (AuswahlWidget Word8)
    , wsButtonUmdrehen :: Maybe Gtk.Button
    , wsAuswahlFahrtrichtung :: Maybe (AuswahlWidget Fahrtrichtung)
    , wsToggleButtonStrom :: Maybe Gtk.ToggleButton
    , wsButtonKuppeln :: Maybe Gtk.Button
    , wsButtonEinstellen :: Maybe Gtk.Button
    }
    deriving (Eq)

-- | Widget zum Hinzufügen einer 'Wegstrecke' zu einem 'Plan'
data WegstreckePlanHinzufügenWidget (z :: Zugtyp) =
    WegstreckePlanHinzufügenWidget
    { bahngeschwindigkeit :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , bahngeschwindigkeitPwm :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , bahngeschwindigkeitKonstanteSpannung :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , streckenabschnitt :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , kupplung :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , kontakt :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    , wegstrecke :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    }
    deriving (Eq)

instance MitWidget (WSWidgets z) where
    erhalteWidget :: (MonadIO m) => WSWidgets z -> m Gtk.Widget
    erhalteWidget = erhalteWidget . wsWidget

instance MitWidget (GeschwindigkeitPhantom WSWidgets g z) where
    erhalteWidget :: (MonadIO m) => GeschwindigkeitPhantom WSWidgets g z -> m Gtk.Widget
    erhalteWidget (GeschwindigkeitPhantom wsWidgets) = erhalteWidget wsWidgets

data WSWidgetsBoxen =
    WSWidgetsBoxen
    { vBoxWegstrecken :: ScrollbaresWidget Gtk.Box
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
          :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
          :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
          :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
          :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
          :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
          :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenKupplungMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenKupplungLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenKontaktMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenKontaktLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    }

class MitWSWidgetsBoxen r where
    wsWidgetsBoxen :: r -> WSWidgetsBoxen

instance MitWSWidgetsBoxen WSWidgetsBoxen where
    wsWidgetsBoxen :: WSWidgetsBoxen -> WSWidgetsBoxen
    wsWidgetsBoxen = id

class (MonadReader r m, MitWSWidgetsBoxen r) => WSWidgetsBoxenReader r m | m -> r where
    erhalteWSWidgetsBoxen :: m WSWidgetsBoxen
    erhalteWSWidgetsBoxen = asks wsWidgetsBoxen

instance (MonadReader r m, MitWSWidgetsBoxen r) => WSWidgetsBoxenReader r m

instance (ZugtypKlasse z) => ObjektElement (WSWidgets z) where
    type ObjektTyp (WSWidgets z) = Wegstrecke z

    zuObjektTyp :: WSWidgets z -> Wegstrecke z
    zuObjektTyp = ws

instance (PlanElement (WSWidgets z), ZugtypKlasse z) => WidgetsTyp (WSWidgets z) where
    type ReaderConstraint (WSWidgets z) = MitWSWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (WSWidgets z) m) => WSWidgets z -> m ()
    entferneWidgets wsWidgets@WSWidgets {wsTVarSprache} = do
        WSWidgetsBoxen {vBoxWegstrecken} <- erhalteWSWidgetsBoxen
        mitContainerRemove vBoxWegstrecken wsWidgets
        entferneHinzufügenPlanWidgets wsWidgets
        liftIO $ atomically $ writeTVar wsTVarSprache Nothing

    boxButtonEntfernen :: WSWidgets z -> Gtk.Box
    boxButtonEntfernen = wsFunctionBox

    tvarSprache :: WSWidgets z -> TVarSprachewechselAktionen
    tvarSprache = wsTVarSprache

    tvarEvent :: WSWidgets z -> TVar EventAusführen
    tvarEvent = wsTVarEvent

instance ObjektElement (ZugtypEither WSWidgets) where
    type ObjektTyp (ZugtypEither WSWidgets) = ZugtypEither Wegstrecke

    zuObjektTyp :: ZugtypEither WSWidgets -> ZugtypEither Wegstrecke
    zuObjektTyp = mapZugtypEither ws

    zuObjekt :: ZugtypEither WSWidgets -> Objekt
    zuObjekt = OWegstrecke . mapZugtypEither ws

instance WidgetsTyp (ZugtypEither WSWidgets) where
    type ReaderConstraint (ZugtypEither WSWidgets) = MitWSWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (ZugtypEither WSWidgets) m)
                    => ZugtypEither WSWidgets
                    -> m ()
    entferneWidgets (ZugtypMärklin wsWidgets) = entferneWidgets wsWidgets
    entferneWidgets (ZugtypLego wsWidgets) = entferneWidgets wsWidgets

    boxButtonEntfernen :: ZugtypEither WSWidgets -> Gtk.Box
    boxButtonEntfernen (ZugtypMärklin wsWidgets) = boxButtonEntfernen wsWidgets
    boxButtonEntfernen (ZugtypLego wsWidgets) = boxButtonEntfernen wsWidgets

    tvarSprache :: ZugtypEither WSWidgets -> TVarSprachewechselAktionen
    tvarSprache (ZugtypMärklin wsWidgets) = tvarSprache wsWidgets
    tvarSprache (ZugtypLego wsWidgets) = tvarSprache wsWidgets

    tvarEvent :: ZugtypEither WSWidgets -> TVar EventAusführen
    tvarEvent (ZugtypMärklin wsWidgets) = tvarEvent wsWidgets
    tvarEvent (ZugtypLego wsWidgets) = tvarEvent wsWidgets

instance (ZugtypKlasse z) => ObjektElement (GeschwindigkeitPhantom WSWidgets g z) where
    type ObjektTyp (GeschwindigkeitPhantom WSWidgets g z) = GeschwindigkeitPhantom Wegstrecke g z

    zuObjektTyp :: GeschwindigkeitPhantom WSWidgets g z -> GeschwindigkeitPhantom Wegstrecke g z
    zuObjektTyp (GeschwindigkeitPhantom WSWidgets {ws}) = GeschwindigkeitPhantom ws

    zuObjekt :: GeschwindigkeitPhantom WSWidgets g z -> Objekt
    zuObjekt (GeschwindigkeitPhantom ws) = zuObjekt ws

instance (PlanElement (WSWidgets z), ZugtypKlasse z)
    => WidgetsTyp (GeschwindigkeitPhantom WSWidgets g z) where
    type ReaderConstraint (GeschwindigkeitPhantom WSWidgets g z) = MitWSWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (GeschwindigkeitPhantom WSWidgets g z) m)
                    => GeschwindigkeitPhantom WSWidgets g z
                    -> m ()
    entferneWidgets (GeschwindigkeitPhantom wsWidgets) = entferneWidgets wsWidgets

    boxButtonEntfernen :: GeschwindigkeitPhantom WSWidgets g z -> Gtk.Box
    boxButtonEntfernen (GeschwindigkeitPhantom wsWidgets) = boxButtonEntfernen wsWidgets

    tvarSprache :: GeschwindigkeitPhantom WSWidgets g z -> TVarSprachewechselAktionen
    tvarSprache (GeschwindigkeitPhantom wsWidgets) = tvarSprache wsWidgets

    tvarEvent :: GeschwindigkeitPhantom WSWidgets g z -> TVar EventAusführen
    tvarEvent (GeschwindigkeitPhantom wsWidgets) = tvarEvent wsWidgets

-- little helper function originally defined in the lens package
-- redefined here, so I don't have to rename everything
(??) :: [a -> b] -> a -> [b]
(??) [] _a = []
(??) (h:t) a = h a : (??) t a

instance PlanElement (WSWidgets 'Märklin) where
    buttonsPlan :: WSWidgets 'Märklin -> [Maybe (ButtonPlanHinzufügen (WSWidgets 'Märklin))]
    buttonsPlan =
        (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , kontakt
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (WSWidgets 'Märklin) r)
              => Wegstrecke 'Märklin
              -> r
              -> [BoxPlanHinzufügen (WSWidgets 'Märklin)]
    boxenPlan _wsWidgets =
        (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            , vBoxHinzufügenPlanWegstreckenKupplungMärklin
            , vBoxHinzufügenPlanWegstreckenKontaktMärklin
            , vBoxHinzufügenPlanWegstreckenMärklin]
        . wsWidgetsBoxen

instance PlanElement (WSWidgets 'Lego) where
    buttonsPlan :: WSWidgets 'Lego -> [Maybe (ButtonPlanHinzufügen (WSWidgets 'Lego))]
    buttonsPlan =
        (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , kontakt
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (WSWidgets 'Lego) r)
              => Wegstrecke 'Lego
              -> r
              -> [BoxPlanHinzufügen (WSWidgets 'Lego)]
    boxenPlan _wsWidgets =
        (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            , vBoxHinzufügenPlanWegstreckenKupplungLego
            , vBoxHinzufügenPlanWegstreckenKontaktLego
            , vBoxHinzufügenPlanWegstreckenLego]
        . wsWidgetsBoxen

instance PlanElement (ZugtypEither WSWidgets) where
    buttonsPlan :: ZugtypEither WSWidgets
                -> [Maybe (ButtonPlanHinzufügen (ZugtypEither WSWidgets))]
    buttonsPlan =
        ausZugtypEither
        $ map (fmap widgetHinzufügenZugtypEither)
        . (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , kontakt
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (ZugtypEither WSWidgets) r)
              => ZugtypEither Wegstrecke
              -> r
              -> [BoxPlanHinzufügen (ZugtypEither WSWidgets)]
    boxenPlan (ZugtypMärklin _wegstrecke) =
        map widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            , vBoxHinzufügenPlanWegstreckenKupplungMärklin
            , vBoxHinzufügenPlanWegstreckenKontaktMärklin
            , vBoxHinzufügenPlanWegstreckenMärklin]
        . wsWidgetsBoxen
    boxenPlan (ZugtypLego _wegstrecke) =
        map widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            , vBoxHinzufügenPlanWegstreckenKupplungLego
            , vBoxHinzufügenPlanWegstreckenKontaktLego
            , vBoxHinzufügenPlanWegstreckenLego]
        . wsWidgetsBoxen

instance StreckenObjekt (WSWidgets z) where
    anschlüsse :: WSWidgets z -> Set AnschlussEither
    anschlüsse = anschlüsse . ws

    erhalteName :: WSWidgets z -> Text
    erhalteName = erhalteName . ws

instance (ZugtypKlasse z) => Aeson.ToJSON (WSWidgets z) where
    toJSON :: WSWidgets z -> Aeson.Value
    toJSON = Aeson.toJSON . ws

instance BahngeschwindigkeitKlasse (GeschwindigkeitPhantom WSWidgets) where
    geschwindigkeit :: (I2CReader r m, PwmReader r m, MonadIO m)
                    => GeschwindigkeitPhantom WSWidgets 'Pwm z
                    -> Word8
                    -> m ()
    geschwindigkeit (GeschwindigkeitPhantom WSWidgets {wsScaleGeschwindigkeit = Just scale}) wert =
        liftIO $ do
            adjustment <- Gtk.get scale Gtk.rangeAdjustment
            Gtk.set adjustment [Gtk.adjustmentValue := fromIntegral wert]
    geschwindigkeit _wsWidgets _wert = pure ()

    fahrstrom :: (I2CReader r m, MonadIO m)
              => GeschwindigkeitPhantom WSWidgets 'KonstanteSpannung z
              -> Word8
              -> m ()
    fahrstrom (GeschwindigkeitPhantom WSWidgets {wsAuswahlFahrstrom = Just auswahl}) =
        liftIO . setzeAuswahl auswahl
    fahrstrom _bg = const $ pure ()

    umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m)
             => GeschwindigkeitPhantom WSWidgets g 'Märklin
             -> m ()
    umdrehen (GeschwindigkeitPhantom WSWidgets {wsButtonUmdrehen = Just buttonUmdrehen}) =
        liftIO $ Gtk.buttonClicked buttonUmdrehen
    umdrehen _wsWidgets = pure ()

    fahrtrichtungEinstellen :: (I2CReader r m, PwmReader r m, MonadIO m)
                            => GeschwindigkeitPhantom WSWidgets g 'Lego
                            -> Fahrtrichtung
                            -> m ()
    fahrtrichtungEinstellen
        (GeschwindigkeitPhantom WSWidgets {wsAuswahlFahrtrichtung = Just auswahl}) =
        liftIO . setzeAuswahl auswahl
    fahrtrichtungEinstellen _wsWidgets = const $ pure ()

instance BGWidgetsKlasse (GeschwindigkeitPhantom WSWidgets) where
    scaleGeschwindigkeit :: GeschwindigkeitPhantom WSWidgets 'Pwm z -> Maybe Gtk.Scale
    scaleGeschwindigkeit (GeschwindigkeitPhantom ws) = wsScaleGeschwindigkeit ws

    auswahlFahrstrom :: GeschwindigkeitPhantom WSWidgets 'KonstanteSpannung z
                     -> Maybe (AuswahlWidget Word8)
    auswahlFahrstrom (GeschwindigkeitPhantom ws) = wsAuswahlFahrstrom ws

    auswahlFahrtrichtung :: GeschwindigkeitPhantom WSWidgets g 'Lego
                         -> Maybe (AuswahlWidget Fahrtrichtung)
    auswahlFahrtrichtung (GeschwindigkeitPhantom ws) = wsAuswahlFahrtrichtung ws

instance StreckenabschnittKlasse (WSWidgets z) where
    strom :: (I2CReader r m, MonadIO m) => WSWidgets z -> Strom -> m ()
    strom WSWidgets {wsToggleButtonStrom = Just toggleButton} wert =
        liftIO $ Gtk.set toggleButton [Gtk.toggleButtonActive := (wert == Fließend)]
    strom _wsWidgets _wert = pure ()

instance (PlanElement (WSWidgets z), ZugtypKlasse z) => STWidgetsKlasse (WSWidgets z) where
    toggleButtonStrom :: WSWidgets z -> Maybe Gtk.ToggleButton
    toggleButtonStrom = wsToggleButtonStrom

instance KupplungKlasse (WSWidgets z) where
    kuppeln :: (I2CReader r m, MonadIO m) => WSWidgets z -> m ()
    kuppeln
        WSWidgets {wsButtonKuppeln = Just buttonKuppeln} = liftIO $ Gtk.buttonClicked buttonKuppeln
    kuppeln _wsWidgets = pure ()

instance KontaktKlasse (WSWidgets z) where
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => WSWidgets z -> m ()
    warteAufSignal WSWidgets {ws, wsTVarEvent} = eventAusführen wsTVarEvent $ warteAufSignal ws

instance (WegstreckeKlasse (Wegstrecke z)) => WegstreckeKlasse (WSWidgets z) where
    einstellen :: (I2CReader r m, PwmReader r m, MonadIO m) => WSWidgets z -> m ()
    einstellen WSWidgets {wsButtonEinstellen = Just buttonEinstellen} =
        liftIO $ Gtk.buttonClicked buttonEinstellen
    einstellen _wsWidgets = pure ()

instance (ZugtypKlasse z) => BahngeschwindigkeitContainer (WSWidgets z) where
    enthalteneBahngeschwindigkeiten
        :: WSWidgets z -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = enthalteneBahngeschwindigkeiten . ws

instance StreckenabschnittContainer (WSWidgets z) where
    enthalteneStreckenabschnitte :: WSWidgets z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = enthalteneStreckenabschnitte . ws

instance (ZugtypKlasse z) => WeicheContainer (WSWidgets z) where
    enthalteneWeichen :: WSWidgets z -> Set (ZugtypEither Weiche)
    enthalteneWeichen = enthalteneWeichen . ws

instance KupplungContainer (WSWidgets z) where
    enthalteneKupplungen :: WSWidgets z -> Set Kupplung
    enthalteneKupplungen = enthalteneKupplungen . ws

instance KontaktContainer (WSWidgets z) where
    enthalteneKontakte :: WSWidgets z -> Set Kontakt
    enthalteneKontakte = enthalteneKontakte . ws

-- | 'Wegstrecke' darstellen.
wegstreckePackNew
    :: forall o m z.
    ( BefehlConstraints o
    , BG o ~ BGWidgets
    , ST o ~ STWidgets
    , WS o ~ WSWidgets
    , SP o ~ SpracheGui
    , ObjektReader o m
    , MitStatusVar (ReaderFamilie o) o
    , MitWSWidgetsBoxen (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , MitTMVarPlanObjekt (ReaderFamilie o)
    , MitAktionBearbeiten (ReaderFamilie o)
    , MitTVarMaps (ReaderFamilie o)
    , MonadIO m
    , ZugtypKlasse z
    , PwmZugtyp z
    , PlanElement (WSWidgets z)
    , WegstreckeKlasse (Wegstrecke z)
    )
    => Wegstrecke z
    -> MStatusAllgemeinT m o (WSWidgets z)
wegstreckePackNew
    wegstrecke@Wegstrecke
    {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen, wsKontakte} =
    do
        objektReader <- ask
        statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
        widgetsBoxen@WSWidgetsBoxen {vBoxWegstrecken} <- erhalteWSWidgetsBoxen
        (wsTVarSprache, wsTVarEvent) <- liftIO $ do
            wsTVarSprache <- newTVarIO $ Just []
            wsTVarEvent <- newTVarIO EventAusführen
            pure (wsTVarSprache, wsTVarEvent)
        let justTVarSprache = Just wsTVarSprache
        -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
        let [ boxBGPwm
                , boxBGKonstanteSpannung
                , boxBG
                , boxStreckenabschnitt
                , boxKupplung
                , boxKontakte
                , boxWegstrecke] = boxenPlan wegstrecke widgetsBoxen
        hinzufügenPlanWidgetBGPwm
            <- if any (ausGeschwindigkeitEither $ (== Pwm) . verwendetPwm) wsBahngeschwindigkeiten
                then Just <$> hinzufügenWidgetPlanPackNew boxBGPwm wegstrecke wsTVarSprache
                else pure Nothing
        hinzufügenPlanWidgetBGKonstanteSpannung <- if any
            (ausGeschwindigkeitEither $ (== KonstanteSpannung) . verwendetPwm)
            wsBahngeschwindigkeiten
            then Just
                <$> hinzufügenWidgetPlanPackNew boxBGKonstanteSpannung wegstrecke wsTVarSprache
            else pure Nothing
        hinzufügenPlanWidgetBG <- if null wsBahngeschwindigkeiten
            then pure Nothing
            else Just <$> hinzufügenWidgetPlanPackNew boxBG wegstrecke wsTVarSprache
        hinzufügenPlanWidgetST <- if null wsStreckenabschnitte
            then pure Nothing
            else Just
                <$> hinzufügenWidgetPlanPackNew boxStreckenabschnitt wegstrecke wsTVarSprache
        hinzufügenPlanWidgetKU <- if null wsKupplungen
            then pure Nothing
            else Just <$> hinzufügenWidgetPlanPackNew boxKupplung wegstrecke wsTVarSprache
        hinzufügenPlanWidgetKO <- if null wsKontakte
            then pure Nothing
            else Just <$> hinzufügenWidgetPlanPackNew boxKontakte wegstrecke wsTVarSprache
        hinzufügenPlanWidgetWS <- if null wsWeichenRichtungen
            then pure Nothing
            else Just <$> hinzufügenWidgetPlanPackNew boxWegstrecke wegstrecke wsTVarSprache
        let hinzufügenPlanWidget =
                WegstreckePlanHinzufügenWidget
                { bahngeschwindigkeitPwm = hinzufügenPlanWidgetBGPwm
                , bahngeschwindigkeitKonstanteSpannung = hinzufügenPlanWidgetBGKonstanteSpannung
                , bahngeschwindigkeit = hinzufügenPlanWidgetBG
                , streckenabschnitt = hinzufügenPlanWidgetST
                , kupplung = hinzufügenPlanWidgetKU
                , kontakt = hinzufügenPlanWidgetKO
                , wegstrecke = hinzufügenPlanWidgetWS
                }
        -- Widget erstellen
        (frame, expander, vBoxExpander, wsFunctionBox) <- liftIO $ do
            frame <- boxPackWidgetNewDefault vBoxWegstrecken $ Gtk.frameNew Nothing
            vBox <- containerAddWidgetNew frame $ Gtk.boxNew Gtk.OrientationVertical 0
            namePackNew vBox wegstrecke
            expander <- boxPackWidgetNewDefault vBox $ Gtk.expanderNew Nothing
            vBoxExpander <- containerAddWidgetNew expander
                $ scrollbaresWidgetNew
                $ Gtk.boxNew Gtk.OrientationVertical 0
            wsFunctionBox <- boxPackWidgetNewDefault vBox $ Gtk.boxNew Gtk.OrientationHorizontal 0
            pure (frame, expander, vBoxExpander, wsFunctionBox)
        verwendeSpracheGui justTVarSprache $ \sprache
            -> Gtk.set expander [Gtk.expanderLabel := Language.wegstreckenElemente sprache]
        (wsScaleGeschwindigkeit, wsAuswahlFahrstrom, wsButtonUmdrehen, wsAuswahlFahrtrichtung) <- if null
            wsBahngeschwindigkeiten
            then pure (Nothing, Nothing, Nothing, Nothing)
            else do
                boxPackWidgetNewDefault vBoxExpander
                    $ labelSpracheNew justTVarSprache
                    $ Language.bahngeschwindigkeiten
                    <:> fromJust (foldl appendName Nothing wsBahngeschwindigkeiten)
                maybeScale <- if any
                    (ausGeschwindigkeitEither $ (== Pwm) . verwendetPwm)
                    wsBahngeschwindigkeiten
                    then Just
                        <$> hScaleGeschwindigkeitPackNew
                            wsFunctionBox
                            (GeschwindigkeitPhantom wegstrecke)
                            wsTVarEvent
                            statusVar
                    else pure Nothing
                let geschwindigkeitenKonstanteSpannung =
                        catKonstanteSpannung wsBahngeschwindigkeiten
                maybeAuswahlFahrstrom <- if null geschwindigkeitenKonstanteSpannung
                    then pure Nothing
                    else fmap Just
                        -- fahrstromAnschlüsse total, weil g ~ 'KonstanteSpannung sichergestellt ist
                        $ auswahlFahrstromPackNew
                            wsFunctionBox
                            (GeschwindigkeitPhantom wegstrecke)
                            (maximum
                             $ fromIntegral
                             . length
                             . fahrstromAnschlüsse
                             . bgGeschwindigkeitsAnschlüsse
                             <$> geschwindigkeitenKonstanteSpannung)
                            wsTVarSprache
                            wsTVarEvent
                            statusVar
                eitherFahrtrichtungWidget <- case zuZugtypEither wegstrecke of
                    (ZugtypMärklin wsMärklin) -> Left
                        <$> buttonUmdrehenPackNew
                            wsFunctionBox
                            (GeschwindigkeitPhantom wsMärklin
                             :: GeschwindigkeitPhantom Wegstrecke 'Pwm 'Märklin)
                            wsTVarSprache
                            wsTVarEvent
                            statusVar
                    (ZugtypLego wsLego) -> Right
                        <$> auswahlFahrtrichtungEinstellenPackNew
                            wsFunctionBox
                            (GeschwindigkeitPhantom wsLego
                             :: GeschwindigkeitPhantom Wegstrecke 'Pwm 'Lego)
                            wsTVarSprache
                            wsTVarEvent
                            statusVar
                pure
                    ( maybeScale
                    , maybeAuswahlFahrstrom
                    , leftToMaybe eitherFahrtrichtungWidget
                    , rightToMaybe eitherFahrtrichtungWidget
                    )
        wsToggleButtonStrom <- if null wsStreckenabschnitte
            then pure Nothing
            else do
                boxPackWidgetNewDefault vBoxExpander
                    $ labelSpracheNew justTVarSprache
                    $ Language.streckenabschnitte
                    <:> fromJust (foldl appendName Nothing wsStreckenabschnitte)
                Just
                    <$> toggleButtonStromPackNew
                        wsFunctionBox
                        wegstrecke
                        wsTVarSprache
                        wsTVarEvent
                        statusVar
        wsButtonEinstellen <- if null wsWeichenRichtungen
            then pure Nothing
            else do
                boxPackWidgetNewDefault vBoxExpander
                    $ labelSpracheNew justTVarSprache
                    $ Language.weichen
                    <:> fromJust
                        (foldl
                             (\acc (weiche, richtung) -> Just
                              $ fromJust (appendName acc weiche) <°> richtung)
                             Nothing
                             wsWeichenRichtungen)
                fmap Just
                    $ boxPackWidgetNewDefault wsFunctionBox
                    $ buttonNewWithEventLabel justTVarSprache Language.einstellen
                    $ eventAusführen wsTVarEvent
                    $ flip runReaderT objektReader
                    $ ausführenStatusVarAktion (Einstellen wegstrecke) statusVar
        wsButtonKuppeln <- if null wsKupplungen
            then pure Nothing
            else do
                boxPackWidgetNewDefault vBoxExpander
                    $ labelSpracheNew justTVarSprache
                    $ Language.kupplungen <:> fromJust (foldl appendName Nothing wsKupplungen)
                Just
                    <$> buttonKuppelnPackNew
                        wsFunctionBox
                        wegstrecke
                        wsTVarSprache
                        wsTVarEvent
                        statusVar
        let wsWidgets =
                WSWidgets
                { ws = wegstrecke
                , wsWidget = frame
                , wsFunctionBox
                , wsHinzPL = hinzufügenPlanWidget
                , wsTVarSprache
                , wsTVarEvent
                , wsScaleGeschwindigkeit
                , wsAuswahlFahrstrom
                , wsButtonUmdrehen
                , wsAuswahlFahrtrichtung
                , wsToggleButtonStrom
                , wsButtonKuppeln
                , wsButtonEinstellen
                }
        buttonEntfernenPackNew wsWidgets
            $ (entfernenWegstrecke $ zuZugtypEither wsWidgets :: IOStatusAllgemein o ())
        buttonBearbeitenPackNew wsWidgets
        -- Widgets merken
        ausführenBefehl $ Hinzufügen $ ausObjekt $ OWegstrecke $ zuZugtypEither wsWidgets
        pure wsWidgets
    where
        appendName :: forall o1.
                   (StreckenObjekt o1)
                   => Maybe (Sprache -> Text)
                   -> o1
                   -> Maybe (Sprache -> Text)

        -- Maybe necessary here, because otherwise (compare strings) this would lead to O(n!) runtime
        appendName Nothing objekt = Just $ const $ erhalteName objekt
        appendName (Just acc) objekt = Just $ acc <^> erhalteName objekt
#endif
--
