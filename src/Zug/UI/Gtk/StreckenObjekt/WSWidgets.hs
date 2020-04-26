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

module Zug.UI.Gtk.StreckenObjekt.WSWidgets
  ( WSWidgets()
  , wegstreckePackNew
  , WSWidgetsBoxen(..)
  , MitWSWidgetsBoxen(..)
  , WSWidgetsBoxenReader(..)
  ) where

import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import qualified Control.Lens as Lens
import Control.Lens ((??), (^..))
import Control.Monad (void, unless)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.Either.Combinators (rightToMaybe)
import Data.List.NonEmpty (NonEmpty())
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung
       (StreckenObjekt(..), Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..)
      , BahngeschwindigkeitContainer(..), verwendetPwm, Streckenabschnitt()
      , StreckenabschnittKlasse(..), StreckenabschnittContainer(..), Weiche(), WeicheContainer(..)
      , Kupplung(), KupplungKlasse(..), KupplungContainer(..), Kontakt(), KontaktKlasse(..)
      , KontaktContainer(..), Wegstrecke(..), WegstreckeKlasse(..), Anschluss(), I2CReader()
      , PwmReader(), InterruptReader())
import Zug.Enums
       (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(zuZugtypEither), mapZugtypEither, ausZugtypEither
      , GeschwindigkeitVariante(..), GeschwindigkeitEither(..), GeschwindigkeitPhantom(..)
      , ausGeschwindigkeitEither, catKonstanteSpannung, Fahrtrichtung(), Strom(Fließend))
import Zug.Language (Sprache(), MitSprache(), (<:>), (<°>), (<^>))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OWegstrecke), ObjektKlasse(..))
import Zug.Plan (AktionWegstrecke(..))
import Zug.UI.Base (MStatusAllgemeinT, IOStatusAllgemein, entfernenWegstrecke, ObjektReader
                  , ReaderFamilie, MitTVarMaps)
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen))
import Zug.UI.Gtk.Auswahl (AuswahlWidget, setzeAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNewDefault, namePackNew
                                 , labelSpracheNew, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove, MitBox(..))
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (MitSpracheGui(), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt.BGWidgets
       (BGWidgets, BGWidgetsKlasse(..), hScaleGeschwindigkeitPackNew, auswahlFahrstromPackNew
      , buttonUmdrehenPackNew, auswahlFahrtrichtungEinstellenPackNew)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen (PlanElement(..), entferneHinzufügenPlanWidgets
                                               , hinzufügenWidgetPlanPackNew, MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.KUWidgets (buttonKuppelnPackNew)
import Zug.UI.Gtk.StreckenObjekt.STWidgets
       (STWidgets, STWidgetsKlasse(..), toggleButtonStromPackNew)
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen
       (Kategorie(..), KategorieText(..), BoxPlanHinzufügen, ButtonPlanHinzufügen
      , widgetHinzufügenZugtypEither)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), eventAusführen
      , ohneEvent, buttonEntfernenPackNew)
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
    , wsFunktionBox :: Gtk.Box
    , wsHinzPL :: WegstreckePlanHinzufügenWidget z
    , wsTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , wsTVarEvent :: TVar EventAusführen
    , wsScaleGeschwindigkeit :: Maybe Gtk.HScale
    , wsAuswahlFahrstrom :: Maybe (AuswahlWidget Word8)
    , wsAuswahlFahrtrichtung :: Maybe (AuswahlWidget Fahrtrichtung)
    , wsToggleButtonStrom :: Maybe Gtk.ToggleButton
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
    , wegstrecke :: Maybe (ButtonPlanHinzufügen (WSWidgets z))
    }
    deriving (Eq)

instance MitWidget (WSWidgets z) where
    erhalteWidget :: WSWidgets z -> Gtk.Widget
    erhalteWidget = erhalteWidget . wsWidget

instance MitWidget (GeschwindigkeitPhantom WSWidgets g z) where
    erhalteWidget :: GeschwindigkeitPhantom WSWidgets g z -> Gtk.Widget
    erhalteWidget (GeschwindigkeitPhantom wsWidgets) = erhalteWidget wsWidgets

data WSWidgetsBoxen =
    WSWidgetsBoxen
    { vBoxWegstrecken :: ScrollbaresWidget Gtk.VBox
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
    , vBoxHinzufügenPlanWegstreckenKontakteMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenKontakteLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
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

instance (PlanElement (WSWidgets z)) => WidgetsTyp (WSWidgets z) where
    type ObjektTyp (WSWidgets z) = Wegstrecke z

    type ReaderConstraint (WSWidgets z) = MitWSWidgetsBoxen

    erhalteObjektTyp :: WSWidgets z -> Wegstrecke z
    erhalteObjektTyp = ws

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (WSWidgets z) m) => WSWidgets z -> m ()
    entferneWidgets wsWidgets@WSWidgets {wsTVarSprache} = do
        WSWidgetsBoxen {vBoxWegstrecken} <- erhalteWSWidgetsBoxen
        mitContainerRemove vBoxWegstrecken wsWidgets
        entferneHinzufügenPlanWidgets wsWidgets
        liftIO $ atomically $ writeTVar wsTVarSprache Nothing

    boxButtonEntfernen :: WSWidgets z -> Gtk.Box
    boxButtonEntfernen = wsFunktionBox

    tvarSprache :: WSWidgets z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = wsTVarSprache

    tvarEvent :: WSWidgets z -> TVar EventAusführen
    tvarEvent = wsTVarEvent

instance WidgetsTyp (ZugtypEither WSWidgets) where
    type ObjektTyp (ZugtypEither WSWidgets) = ZugtypEither Wegstrecke

    type ReaderConstraint (ZugtypEither WSWidgets) = MitWSWidgetsBoxen

    erhalteObjektTyp :: ZugtypEither WSWidgets -> ZugtypEither Wegstrecke
    erhalteObjektTyp = mapZugtypEither ws

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (ZugtypEither WSWidgets) m)
                    => ZugtypEither WSWidgets
                    -> m ()
    entferneWidgets (ZugtypMärklin wsWidgets) = entferneWidgets wsWidgets
    entferneWidgets (ZugtypLego wsWidgets) = entferneWidgets wsWidgets

    boxButtonEntfernen :: ZugtypEither WSWidgets -> Gtk.Box
    boxButtonEntfernen (ZugtypMärklin wsWidgets) = boxButtonEntfernen wsWidgets
    boxButtonEntfernen (ZugtypLego wsWidgets) = boxButtonEntfernen wsWidgets

    tvarSprache :: ZugtypEither WSWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (ZugtypMärklin wsWidgets) = tvarSprache wsWidgets
    tvarSprache (ZugtypLego wsWidgets) = tvarSprache wsWidgets

    tvarEvent :: ZugtypEither WSWidgets -> TVar EventAusführen
    tvarEvent (ZugtypMärklin wsWidgets) = tvarEvent wsWidgets
    tvarEvent (ZugtypLego wsWidgets) = tvarEvent wsWidgets

instance (PlanElement (WSWidgets z)) => WidgetsTyp (GeschwindigkeitPhantom WSWidgets g z) where
    type ObjektTyp (GeschwindigkeitPhantom WSWidgets g z) = GeschwindigkeitPhantom Wegstrecke g z

    type ReaderConstraint (GeschwindigkeitPhantom WSWidgets g z) = MitWSWidgetsBoxen

    erhalteObjektTyp
        :: GeschwindigkeitPhantom WSWidgets g z -> GeschwindigkeitPhantom Wegstrecke g z
    erhalteObjektTyp (GeschwindigkeitPhantom WSWidgets {ws}) = GeschwindigkeitPhantom ws

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (GeschwindigkeitPhantom WSWidgets g z) m)
                    => GeschwindigkeitPhantom WSWidgets g z
                    -> m ()
    entferneWidgets (GeschwindigkeitPhantom wsWidgets) = entferneWidgets wsWidgets

    boxButtonEntfernen :: GeschwindigkeitPhantom WSWidgets g z -> Gtk.Box
    boxButtonEntfernen (GeschwindigkeitPhantom wsWidgets) = boxButtonEntfernen wsWidgets

    tvarSprache :: GeschwindigkeitPhantom WSWidgets g z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (GeschwindigkeitPhantom wsWidgets) = tvarSprache wsWidgets

    tvarEvent :: GeschwindigkeitPhantom WSWidgets g z -> TVar EventAusführen
    tvarEvent (GeschwindigkeitPhantom wsWidgets) = tvarEvent wsWidgets

instance PlanElement (WSWidgets 'Märklin) where
    foldPlan
        :: Lens.Fold (WSWidgets 'Märklin) (Maybe (ButtonPlanHinzufügen (WSWidgets 'Märklin)))
    foldPlan =
        Lens.folding
        $ (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (WSWidgets 'Märklin) r)
              => Wegstrecke 'Märklin
              -> Lens.Fold r (BoxPlanHinzufügen (WSWidgets 'Märklin))
    boxenPlan _wsWidgets =
        Lens.folding
        $ (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            , vBoxHinzufügenPlanWegstreckenKupplungMärklin
            , vBoxHinzufügenPlanWegstreckenMärklin]
        . wsWidgetsBoxen

instance PlanElement (WSWidgets 'Lego) where
    foldPlan :: Lens.Fold (WSWidgets 'Lego) (Maybe (ButtonPlanHinzufügen (WSWidgets 'Lego)))
    foldPlan =
        Lens.folding
        $ (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (WSWidgets 'Lego) r)
              => Wegstrecke 'Lego
              -> Lens.Fold r (BoxPlanHinzufügen (WSWidgets 'Lego))
    boxenPlan _wsWidgets =
        Lens.folding
        $ (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            , vBoxHinzufügenPlanWegstreckenKupplungLego
            , vBoxHinzufügenPlanWegstreckenLego]
        . wsWidgetsBoxen

instance PlanElement (ZugtypEither WSWidgets) where
    foldPlan :: Lens.Fold (ZugtypEither WSWidgets) (Maybe (ButtonPlanHinzufügen (ZugtypEither WSWidgets)))
    foldPlan =
        Lens.folding
        $ ausZugtypEither
        $ map (fmap widgetHinzufügenZugtypEither)
        . (??)
            [ bahngeschwindigkeitPwm
            , bahngeschwindigkeitKonstanteSpannung
            , bahngeschwindigkeit
            , streckenabschnitt
            , kupplung
            , wegstrecke]
        . wsHinzPL

    boxenPlan :: (ReaderConstraint (ZugtypEither WSWidgets) r)
              => ZugtypEither Wegstrecke
              -> Lens.Fold r (BoxPlanHinzufügen (ZugtypEither WSWidgets))
    boxenPlan (ZugtypMärklin _wegstrecke) =
        Lens.folding
        $ map widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            , vBoxHinzufügenPlanWegstreckenKupplungMärklin
            , vBoxHinzufügenPlanWegstreckenMärklin]
        . wsWidgetsBoxen
    boxenPlan (ZugtypLego _wegstrecke) =
        Lens.folding
        $ map widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
            , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            , vBoxHinzufügenPlanWegstreckenKupplungLego
            , vBoxHinzufügenPlanWegstreckenLego]
        . wsWidgetsBoxen

instance StreckenObjekt (WSWidgets z) where
    anschlüsse :: WSWidgets z -> Set Anschluss
    anschlüsse = anschlüsse . ws

    erhalteName :: WSWidgets z -> Text
    erhalteName = erhalteName . ws

instance Aeson.ToJSON (WSWidgets z) where
    toJSON :: WSWidgets z -> Aeson.Value
    toJSON = Aeson.toJSON . ws

instance BahngeschwindigkeitKlasse (GeschwindigkeitPhantom WSWidgets) where
    geschwindigkeit :: (I2CReader r m, PwmReader r m, MonadIO m)
                    => GeschwindigkeitPhantom WSWidgets 'Pwm z
                    -> Word8
                    -> m ()
    geschwindigkeit
        (GeschwindigkeitPhantom WSWidgets {ws, wsScaleGeschwindigkeit, wsTVarEvent})
        wert = do
        eventAusführen wsTVarEvent $ geschwindigkeit (GeschwindigkeitPhantom ws) wert
        case wsScaleGeschwindigkeit of
            (Just scaleGeschwindigkeit) -> liftIO
                $ ohneEvent wsTVarEvent
                $ Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
            Nothing -> pure ()

    fahrstrom :: (I2CReader r m, MonadIO m)
              => GeschwindigkeitPhantom WSWidgets 'KonstanteSpannung z
              -> Word8
              -> m ()
    fahrstrom (GeschwindigkeitPhantom WSWidgets {ws, wsAuswahlFahrstrom, wsTVarEvent}) wert = do
        eventAusführen wsTVarEvent $ fahrstrom (GeschwindigkeitPhantom ws) wert
        case wsAuswahlFahrstrom of
            (Just auswahlFahrstrom)
                -> liftIO $ ohneEvent wsTVarEvent $ setzeAuswahl auswahlFahrstrom wert
            Nothing -> pure ()

    umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m)
             => GeschwindigkeitPhantom WSWidgets g 'Märklin
             -> m ()
    umdrehen (GeschwindigkeitPhantom WSWidgets {ws, wsTVarEvent}) =
        eventAusführen wsTVarEvent $ umdrehen $ GeschwindigkeitPhantom ws

    fahrtrichtungEinstellen :: (I2CReader r m, PwmReader r m, MonadIO m)
                            => GeschwindigkeitPhantom WSWidgets g 'Lego
                            -> Fahrtrichtung
                            -> m ()
    fahrtrichtungEinstellen
        (GeschwindigkeitPhantom WSWidgets {ws, wsAuswahlFahrtrichtung, wsTVarEvent})
        wert = do
        eventAusführen wsTVarEvent $ fahrtrichtungEinstellen (GeschwindigkeitPhantom ws) wert
        case wsAuswahlFahrtrichtung of
            (Just auswahlFahrtrichtung)
                -> liftIO $ ohneEvent wsTVarEvent $ setzeAuswahl auswahlFahrtrichtung wert
            Nothing -> pure ()

instance BGWidgetsKlasse (GeschwindigkeitPhantom WSWidgets) where
    scaleGeschwindigkeit :: GeschwindigkeitPhantom WSWidgets 'Pwm z -> Maybe Gtk.HScale
    scaleGeschwindigkeit (GeschwindigkeitPhantom ws) = wsScaleGeschwindigkeit ws

    auswahlFahrstrom :: GeschwindigkeitPhantom WSWidgets 'KonstanteSpannung z
                     -> Maybe (AuswahlWidget Word8)
    auswahlFahrstrom (GeschwindigkeitPhantom ws) = wsAuswahlFahrstrom ws

    auswahlFahrtrichtung :: GeschwindigkeitPhantom WSWidgets g 'Lego
                         -> Maybe (AuswahlWidget Fahrtrichtung)
    auswahlFahrtrichtung (GeschwindigkeitPhantom ws) = wsAuswahlFahrtrichtung ws

instance StreckenabschnittKlasse (WSWidgets z) where
    strom :: (I2CReader r m, MonadIO m) => WSWidgets z -> Strom -> m ()
    strom WSWidgets {ws, wsToggleButtonStrom, wsTVarEvent} wert = do
        eventAusführen wsTVarEvent $ strom ws wert
        case wsToggleButtonStrom of
            (Just toggleButtonStrom) -> liftIO
                $ ohneEvent wsTVarEvent
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (wert == Fließend)]
            Nothing -> pure ()

instance (PlanElement (WSWidgets z)) => STWidgetsKlasse (WSWidgets z) where
    toggleButtonStrom :: WSWidgets z -> Maybe Gtk.ToggleButton
    toggleButtonStrom = wsToggleButtonStrom

instance KupplungKlasse (WSWidgets z) where
    kuppeln :: (I2CReader r m, MonadIO m) => WSWidgets z -> m ()
    kuppeln WSWidgets {ws, wsTVarEvent} = eventAusführen wsTVarEvent $ kuppeln ws

instance KontaktKlasse (WSWidgets z) where
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => WSWidgets z -> m ()
    warteAufSignal WSWidgets {ws, wsTVarEvent} = eventAusführen wsTVarEvent $ warteAufSignal ws

instance (WegstreckeKlasse (Wegstrecke z)) => WegstreckeKlasse (WSWidgets z) where
    einstellen :: (I2CReader r m, PwmReader r m, MonadIO m) => WSWidgets z -> m ()
    einstellen WSWidgets {ws, wsTVarEvent} = eventAusführen wsTVarEvent $ einstellen ws

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

-- | 'Wegstrecke' darstellen
wegstreckePackNew
    :: forall o m z.
    ( BG o ~ BGWidgets
    , ST o ~ STWidgets
    , Eq (WE o 'Märklin)
    , Eq (WE o 'Lego)
    , Eq (KU o)
    , Eq (KO o)
    , WS o ~ WSWidgets
    , Eq (PL o)
    , MitSprache (SP o)
    , ObjektKlasse o
    , Aeson.ToJSON o
    , ObjektReader o m
    , MitStatusVar (ReaderFamilie o) o
    , MitWSWidgetsBoxen (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , MitTMVarPlanObjekt (ReaderFamilie o)
    , MitTVarMaps (ReaderFamilie o)
    , MonadIO m
    , ZugtypKlasse z
    , PlanElement (WSWidgets z)
    , WegstreckeKlasse (Wegstrecke z)
    )
    => Wegstrecke z
    -> MStatusAllgemeinT m o (WSWidgets z)
wegstreckePackNew
    wegstrecke@Wegstrecke
    {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen} = do
    objektReader <- ask
    statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
    wsWidgetsBoxen@WSWidgetsBoxen {vBoxWegstrecken} <- erhalteWSWidgetsBoxen
    (wsTVarSprache, wsTVarEvent) <- liftIO $ do
        wsTVarSprache <- newTVarIO $ Just []
        wsTVarEvent <- newTVarIO EventAusführen
        pure (wsTVarSprache, wsTVarEvent)
    let justTVarSprache = Just wsTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    let [boxBGPwm, boxBGKonstanteSpannung, boxBG, boxStreckenabschnitt, boxKupplung, boxWegstrecke] =
            wsWidgetsBoxen ^.. boxenPlan wegstrecke
    hinzufügenPlanWidgetBGPwm
        <- if any (ausGeschwindigkeitEither $ (== Pwm) . verwendetPwm) wsBahngeschwindigkeiten
            then Just <$> hinzufügenWidgetPlanPackNew boxBGPwm wegstrecke wsTVarSprache
            else pure Nothing
    hinzufügenPlanWidgetBGKonstanteSpannung <- if any
        (ausGeschwindigkeitEither $ (== KonstanteSpannung) . verwendetPwm)
        wsBahngeschwindigkeiten
        then Just <$> hinzufügenWidgetPlanPackNew boxBGKonstanteSpannung wegstrecke wsTVarSprache
        else pure Nothing
    hinzufügenPlanWidgetBG <- if null wsBahngeschwindigkeiten
        then pure Nothing
        else Just <$> hinzufügenWidgetPlanPackNew boxBG wegstrecke wsTVarSprache
    hinzufügenPlanWidgetST <- if null wsStreckenabschnitte
        then pure Nothing
        else Just <$> hinzufügenWidgetPlanPackNew boxStreckenabschnitt wegstrecke wsTVarSprache
    hinzufügenPlanWidgetKU <- if null wsKupplungen
        then pure Nothing
        else Just <$> hinzufügenWidgetPlanPackNew boxKupplung wegstrecke wsTVarSprache
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
            , wegstrecke = hinzufügenPlanWidgetWS
            }
    -- Widget erstellen
    (frame, expander, vBoxExpander, functionBox) <- liftIO $ do
        frame <- boxPackWidgetNewDefault vBoxWegstrecken Gtk.frameNew
        vBox <- containerAddWidgetNew frame $ Gtk.vBoxNew False 0
        namePackNew vBox wegstrecke
        expander <- boxPackWidgetNewDefault vBox $ Gtk.expanderNew Text.empty
        vBoxExpander <- containerAddWidgetNew expander $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0
        functionBox <- boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
        pure (frame, expander, vBoxExpander, functionBox)
    verwendeSpracheGui justTVarSprache
        $ \sprache -> Gtk.set expander [Gtk.expanderLabel := Language.wegstreckenElemente sprache]
    (wsScaleGeschwindigkeit, wsAuswahlFahrstrom, wsAuswahlFahrtrichtung) <- if null
        wsBahngeschwindigkeiten
        then pure (Nothing, Nothing, Nothing)
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
                        functionBox
                        (GeschwindigkeitPhantom wegstrecke)
                        wsTVarEvent
                        statusVar
                else pure Nothing
            let geschwindigkeitenKonstanteSpannung = catKonstanteSpannung wsBahngeschwindigkeiten
            maybeAuswahlFahrstrom <- if null geschwindigkeitenKonstanteSpannung
                then pure Nothing
                else fmap Just
                    $ auswahlFahrstromPackNew
                        functionBox
                        (GeschwindigkeitPhantom wegstrecke)
                        (maximum
                         $ fromIntegral . length . fahrstromAnschlüsse
                         <$> geschwindigkeitenKonstanteSpannung)
                        wsTVarSprache
                        wsTVarEvent
                        statusVar
            eitherFahrtrichtungWidget <- case zuZugtypEither wegstrecke of
                (ZugtypMärklin wsMärklin) -> Left
                    <$> buttonUmdrehenPackNew
                        functionBox
                        (GeschwindigkeitPhantom wsMärklin
                         :: GeschwindigkeitPhantom Wegstrecke 'Pwm 'Märklin)
                        wsTVarSprache
                        wsTVarEvent
                        statusVar
                (ZugtypLego wsLego) -> Right
                    <$> auswahlFahrtrichtungEinstellenPackNew
                        functionBox
                        (GeschwindigkeitPhantom wsLego
                         :: GeschwindigkeitPhantom Wegstrecke 'Pwm 'Lego)
                        wsTVarSprache
                        wsTVarEvent
                        statusVar
            pure (maybeScale, maybeAuswahlFahrstrom, rightToMaybe eitherFahrtrichtungWidget)
    wsToggleButtonStrom <- if null wsStreckenabschnitte
        then pure Nothing
        else do
            boxPackWidgetNewDefault vBoxExpander
                $ labelSpracheNew justTVarSprache
                $ Language.streckenabschnitte
                <:> fromJust (foldl appendName Nothing wsStreckenabschnitte)
            Just
                <$> toggleButtonStromPackNew
                    functionBox
                    wegstrecke
                    wsTVarSprache
                    wsTVarEvent
                    statusVar
    unless (null wsWeichenRichtungen) $ void $ do
        boxPackWidgetNewDefault vBoxExpander
            $ labelSpracheNew justTVarSprache
            $ Language.weichen <:> fromJust (foldl (\acc (weiche, richtung) -> Just
                                                    $ fromJust (appendName acc weiche)
                                                    <°> richtung) Nothing wsWeichenRichtungen)
        boxPackWidgetNewDefault functionBox
            $ buttonNewWithEventLabel justTVarSprache Language.einstellen
            $ eventAusführen wsTVarEvent
            $ flip runReaderT objektReader
            $ ausführenStatusVarAktion (Einstellen wegstrecke) statusVar
    unless (null wsKupplungen) $ void $ do
        boxPackWidgetNewDefault vBoxExpander
            $ labelSpracheNew justTVarSprache
            $ Language.kupplungen <:> fromJust (foldl appendName Nothing wsKupplungen)
        buttonKuppelnPackNew functionBox wegstrecke wsTVarSprache wsTVarEvent statusVar
    let wsWidgets =
            WSWidgets
            { ws = wegstrecke
            , wsWidget = frame
            , wsFunktionBox = erhalteBox functionBox
            , wsHinzPL = hinzufügenPlanWidget
            , wsTVarSprache
            , wsTVarEvent
            , wsScaleGeschwindigkeit
            , wsAuswahlFahrstrom
            , wsAuswahlFahrtrichtung
            , wsToggleButtonStrom
            }
    buttonEntfernenPackNew wsWidgets
        $ (entfernenWegstrecke $ zuZugtypEither wsWidgets :: IOStatusAllgemein o ())
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OWegstrecke $ zuZugtypEither wsWidgets
    pure wsWidgets
    where
        appendName :: forall o.
                   (StreckenObjekt o)
                   => Maybe (Sprache -> Text)
                   -> o
                   -> Maybe (Sprache -> Text)

        -- Maybe necessary here, because otherwise (compare strings) this would lead to O(n!) runtime
        appendName Nothing objekt = Just $ const $ erhalteName objekt
        appendName (Just acc) objekt = Just $ acc <^> erhalteName objekt

        fahrstromAnschlüsse :: Bahngeschwindigkeit 'KonstanteSpannung z -> NonEmpty Anschluss
        fahrstromAnschlüsse
            MärklinBahngeschwindigkeitKonstanteSpannung {bgmkFahrstromAnschlüsse} =
            bgmkFahrstromAnschlüsse