{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
#endif

{-|
Description : Erstelle zusammengesetzte Widgets.

Allgemeine Hilfsfunktionen zum erstellen neuer Widgets
-}
module Zug.UI.Gtk.StreckenObjekt
  (
#ifdef ZUGKONTROLLEGUI
    -- ** Spezifisches StreckenObjekt darstellen
    BGWidgets()
  , bahngeschwindigkeitPackNew
  , STWidgets()
  , streckenabschnittPackNew
  , WEWidgets()
  , weichePackNew
  , KUWidgets()
  , kupplungPackNew
  , KOWidgets()
  , kontaktPackNew
  , WSWidgets()
  , wegstreckePackNew
  , PLWidgets()
  , planPackNew
  , WidgetsTyp(..)
    -- * Verwaltung des aktuellen Zustands
  , DynamischeWidgets(..)
  , MitDynamischeWidgets(..)
  , DynamischeWidgetsReader(..)
  , StatusGui
  , ObjektGui
  , ObjektGuiReader
  , BefehlGui
  , IOStatusGui
  , MStatusGui
  , MStatusGuiT
  , StatusVarGui
  , StatusVarGuiReader
  , readSpracheGui
  , EventAusführen(..)
  , eventAusführen
  , ohneEvent
    -- * Hinzufügen zu einem Plan/einer Wegstrecke
  , WidgetHinzufügen()
  , HinzufügenZiel(..)
  , CheckButtonWegstreckeHinzufügen
  , WegstreckeCheckButton()
  , WegstreckeCheckButtonVoid
  , KategorieText(..)
  , Kategorie(..)
  , BoxWegstreckeHinzufügen
  , boxWegstreckeHinzufügenNew
  , ButtonPlanHinzufügen
  , BoxPlanHinzufügen
  , boxPlanHinzufügenNew
  , widgetHinzufügenZugtypEither
  , widgetHinzufügenRegistrierterCheckButtonVoid
  , widgetHinzufügenAktuelleAuswahl
  , widgetHinzufügenToggled
  , widgetHinzufügenContainerRemoveJust
  , widgetHinzufügenBoxPackNew
  , foldWegstreckeHinzufügen
  , WegstreckenElement(..)
  , entferneHinzufügenWegstreckeWidgets
  , PlanElement(..)
  , entferneHinzufügenPlanWidgets
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Applicative (ZipList(..))
import Control.Concurrent.STM
       (atomically, TVar, readTVarIO, writeTVar, swapTVar, newTVarIO, TMVar, putTMVar)
import Control.Lens ((^.), (^..), (??))
import qualified Control.Lens as Lens
import Control.Monad (void, when, unless, forM_, foldM)
import Control.Monad.Reader (MonadReader(..), asks, runReaderT)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.Aeson as Aeson
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (Foldable(..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import Data.Semigroup (Semigroup((<>)))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word8)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
import Numeric.Natural (Natural)

import Zug.Anbindung
       (StreckenObjekt(..), Anschluss(), PwmReader(), I2CReader(), Bahngeschwindigkeit(..)
      , verwendetPwm, BahngeschwindigkeitKlasse(..), BahngeschwindigkeitContainer(..)
      , Streckenabschnitt(..), StreckenabschnittKlasse(..), StreckenabschnittContainer(..)
      , Weiche(..), WeicheKlasse(..), WeicheContainer(..), Kupplung(..), KupplungKlasse(..)
      , KupplungContainer(..), Kontakt(..), KontaktKlasse(..), KontaktContainer(..), Wegstrecke(..)
      , WegstreckeKlasse(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), ausZugtypEither, mapZugtypEither
                , GeschwindigkeitVariante(..), GeschwindigkeitEither(..)
                , GeschwindigkeitEitherKlasse(zuGeschwindigkeitEither), ausGeschwindigkeitEither
                , catKonstanteSpannung, GeschwindigkeitPhantom(..), Fahrtrichtung(..), Strom(..)
                , Richtung(..))
import Zug.Language (MitSprache(..), (<°>))
import qualified Zug.Language as Language
import Zug.Language (Sprache(), Anzeige(..), ($#), (<^>), (<:>))
import Zug.Objekt (ObjektAllgemein(..), ObjektElement(..), Objekt, ObjektKlasse(..))
import Zug.Plan (PlanKlasse(..), Plan(..), AusführendReader(), AktionKlasse(..)
               , AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionWeiche(..)
               , AktionKupplung(..), AktionWegstrecke(..))
import Zug.UI.Base
       (StatusAllgemein(..), IOStatusAllgemein, MStatusAllgemein, MStatusAllgemeinT
      , AusführenMöglich(..), ReaderFamilie, ObjektReader, TVarMaps(..), MitTVarMaps(..)
      , bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, sprache, ausführenMöglich
      , entfernenBahngeschwindigkeit, entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung
      , entfernenWegstrecke, entfernenPlan, getBahngeschwindigkeiten, getStreckenabschnitte
      , getWegstrecken)
import Zug.UI.Befehl (BefehlAllgemein(..), BefehlKlasse(..))
import Zug.UI.Gtk.Anschluss (anschlussNew, pinNew)
import Zug.UI.Gtk.Auswahl
       (AuswahlWidget(), aktuelleAuswahl, setzeAuswahl, beiAuswahl, auswahlRadioButtonNew
      , auswahlComboBoxNew, boundedEnumAuswahlRadioButtonNew, MitAuswahlWidget(..))
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.FortfahrenWennToggled
       (FortfahrenWennToggledVar, registrierterCheckButtonNew, RegistrierterCheckButton
      , MitRegistrierterCheckButton(..), registrierterCheckButtonToggled)
import Zug.UI.Gtk.Hilfsfunktionen
       (containerAddWidgetNew, containerRemoveJust, boxPackWidgetNew, boxPackWidgetNewDefault
      , paddingDefault, positionDefault, Packing(..), Position(..), dialogEval
      , buttonNewWithEventLabel, toggleButtonNewWithEventLabel, namePackNew, widgetShowNew
      , labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), mitContainerRemove, MitBox(..))
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui
       (SpracheGui, SpracheGuiReader(..), MitSpracheGui(..), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt.BGWidgets
       (BGWidgets(..), BGWidgetsKlasse(..), bahngeschwindigkeitPackNew, hScaleGeschwindigkeitPackNew
      , auswahlFahrstromPackNew, buttonUmdrehenPackNew, auswahlFahrtrichtungEinstellenPackNew
      , BGWidgetsBoxen(..), MitBGWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (WegstreckenElement(..), WegstreckeCheckButtonVoid, MitFortfahrenWennToggledWegstrecke(..)
      , FortfahrenWenToggledWegstreckeReader(..), hinzufügenWidgetWegstreckePackNew
      , hinzufügenWidgetWegstreckeRichtungPackNew, entferneHinzufügenWegstreckeWidgets
      , foldWegstreckeHinzufügen, PlanElement(..), MitTMVarPlanObjekt(..)
      , TMVarPlanObjektReader(..), hinzufügenWidgetPlanPackNew, entferneHinzufügenPlanWidgets)
import Zug.UI.Gtk.StreckenObjekt.STWidgets
       (STWidgets(..), STWidgetsKlasse(..), streckenabschnittPackNew, toggleButtonStromPackNew
      , STWidgetsBoxen(..), MitSTWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.WEWidgets
       (WEWidgets(..), weichePackNew, WEWidgetsBoxen(..), MitWEWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen
       (Kategorie(..), KategorieText(..), WidgetHinzufügen, HinzufügenZiel(..)
      , BoxWegstreckeHinzufügen, boxWegstreckeHinzufügenNew, CheckButtonWegstreckeHinzufügen
      , WegstreckeCheckButton(..), BoxPlanHinzufügen, boxPlanHinzufügenNew, ButtonPlanHinzufügen
      , widgetHinzufügenContainerRemoveJust, widgetHinzufügenBoxPackNew
      , widgetHinzufügenRegistrierterCheckButtonVoid, widgetHinzufügenAktuelleAuswahl
      , widgetHinzufügenToggled, widgetHinzufügenGeschwindigkeitVariante
      , widgetHinzufügenGeschwindigkeitEither, widgetHinzufügenZugtypEither)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp (WidgetsTyp(..), WidgetsTypReader, EventAusführen(..)
                                           , eventAusführen, ohneEvent, buttonEntfernenPackNew)
import Zug.UI.StatusVar (StatusVar, MitStatusVar(..), StatusVarReader(..), tryReadStatusVar
                       , auswertenStatusVarIOStatus, ausführenStatusVarBefehl
                       , ausführenStatusVarAktion, auswertenStatusVarMStatusT)

-- * Sammel-Typ um dynamische Widgets zu speichern
-- | Sammel-Typ spezialisiert auf Gui-Typen
type ObjektGui =
    ObjektAllgemein BGWidgets STWidgets WEWidgets KUWidgets KOWidgets WSWidgets PLWidgets

-- | 'ObjektReader' spezialisiert auf Gui-Typen
type ObjektGuiReader m = ObjektReader ObjektGui m

-- | Befehl spezialisiert auf Gui-Typen
type BefehlGui = BefehlAllgemein ObjektGui

-- | Zustands-Typ der Zustands-Monade spezialisiert auf Gui-Typen
type StatusGui = StatusAllgemein ObjektGui

-- | 'StatusVar' spezialisiert auf Gui-Typen
type StatusVarGui = StatusVar ObjektGui

-- | 'StatusVarReader' spezialisiert auf Gui-Typen
type StatusVarGuiReader r m = StatusVarReader r ObjektGui m

-- | Zustands-Monaden-Transformer spezialisiert auf Gui-Typen in der IO-Monade
type IOStatusGui a = IOStatusAllgemein ObjektGui a

-- | Reine Zustands-Monade spezialisiert auf Gui-Typen
type MStatusGui a = MStatusAllgemein ObjektGui a

-- | Zustands-Monaden-Transformer spezialisiert auf Gui-Typen
type MStatusGuiT m a = MStatusAllgemeinT m ObjektGui a

instance ObjektKlasse ObjektGui where
    type BG ObjektGui = BGWidgets

    type ST ObjektGui = STWidgets

    type WE ObjektGui = WEWidgets

    type KU ObjektGui = KUWidgets

    type KO ObjektGui = KOWidgets

    type WS ObjektGui = WSWidgets

    type PL ObjektGui = PLWidgets

    type SP ObjektGui = SpracheGui

    erhalteObjekt :: ObjektGui -> ObjektGui
    erhalteObjekt = id

    ausObjekt :: ObjektGui -> ObjektGui
    ausObjekt = id

-- | Sammlung aller Widgets, welche während der Laufzeit benötigt werden.
data DynamischeWidgets =
    DynamischeWidgets
    { vBoxBahngeschwindigkeiten :: ScrollbaresWidget Gtk.VBox
    , vBoxStreckenabschnitte :: ScrollbaresWidget Gtk.VBox
    , vBoxWeichen :: ScrollbaresWidget Gtk.VBox
    , vBoxKupplungen :: ScrollbaresWidget Gtk.VBox
    , vBoxWegstrecken :: ScrollbaresWidget Gtk.VBox
    , vBoxPläne :: ScrollbaresWidget Gtk.VBox
    , vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
          :: BoxWegstreckeHinzufügen (GeschwindigkeitEither BGWidgets 'Märklin)
    , vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
          :: BoxWegstreckeHinzufügen (GeschwindigkeitEither BGWidgets 'Lego)
    , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
          :: BoxPlanHinzufügen (GeschwindigkeitEither BGWidgets 'Märklin)
    , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
          :: BoxPlanHinzufügen (BGWidgets 'Pwm 'Märklin)
    , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
          :: BoxPlanHinzufügen (BGWidgets 'KonstanteSpannung 'Märklin)
    , vBoxHinzufügenPlanBahngeschwindigkeitenLego
          :: BoxPlanHinzufügen (GeschwindigkeitEither BGWidgets 'Lego)
    , vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm :: BoxPlanHinzufügen (BGWidgets 'Pwm 'Lego)
    , vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
          :: BoxPlanHinzufügen (BGWidgets 'KonstanteSpannung 'Lego)
    , vBoxHinzufügenWegstreckeStreckenabschnitte :: BoxWegstreckeHinzufügen STWidgets
    , vBoxHinzufügenPlanStreckenabschnitte :: BoxPlanHinzufügen STWidgets
    , vBoxHinzufügenWegstreckeWeichenMärklin :: BoxWegstreckeHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenWegstreckeWeichenLego :: BoxWegstreckeHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenPlanWeichenGeradeMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenPlanWeichenKurveMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenPlanWeichenLinksMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenPlanWeichenRechtsMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenPlanWeichenGeradeLego :: BoxPlanHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenPlanWeichenKurveLego :: BoxPlanHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenPlanWeichenLinksLego :: BoxPlanHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenPlanWeichenRechtsLego :: BoxPlanHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenWegstreckeKupplungen :: BoxWegstreckeHinzufügen KUWidgets
    , vBoxHinzufügenPlanKupplungen :: BoxPlanHinzufügen KUWidgets
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
          :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
          :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
          :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
          :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenKupplungMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
          :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
          :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenKupplungLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanWegstreckenLego :: BoxPlanHinzufügen (WSWidgets 'Lego)
    , vBoxHinzufügenPlanPläne :: BoxPlanHinzufügen PLWidgets
    , windowMain :: Gtk.Window
    , fortfahrenWennToggledWegstrecke
          :: FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid
    , tmvarPlanObjekt :: TMVar (Maybe Objekt)
    }

-- | Klasse für Typen mit 'DynamischeWidgets'
class MitDynamischeWidgets r where
    dynamischeWidgets :: r -> DynamischeWidgets

instance MitDynamischeWidgets DynamischeWidgets where
    dynamischeWidgets :: DynamischeWidgets -> DynamischeWidgets
    dynamischeWidgets = id

-- | Abkürzung für Funktionen, die 'DynamischeWidgets' benötigen
class (MonadReader r m) => DynamischeWidgetsReader r m where
    erhalteDynamischeWidgets :: m DynamischeWidgets

instance (MonadReader r m, MitDynamischeWidgets r) => DynamischeWidgetsReader r m where
    erhalteDynamischeWidgets :: m DynamischeWidgets
    erhalteDynamischeWidgets = asks dynamischeWidgets

type instance ReaderFamilie ObjektGui = (TVarMaps, DynamischeWidgets, StatusVar ObjektGui)

instance MitTVarMaps (TVarMaps, DynamischeWidgets, StatusVar ObjektGui) where
    tvarMaps :: (TVarMaps, DynamischeWidgets, StatusVar ObjektGui) -> TVarMaps
    tvarMaps (tvarMaps, _dynamischeWidgets, _tmvarStatus) = tvarMaps

instance MitDynamischeWidgets (TVarMaps, DynamischeWidgets, StatusVar ObjektGui) where
    dynamischeWidgets :: (TVarMaps, DynamischeWidgets, StatusVar ObjektGui) -> DynamischeWidgets
    dynamischeWidgets (_tvarMaps, dynamischeWidgets, _tmvarStatus) = dynamischeWidgets

instance MitStatusVar (TVarMaps, DynamischeWidgets, StatusVar ObjektGui) ObjektGui where
    statusVar :: (TVarMaps, DynamischeWidgets, StatusVar ObjektGui) -> StatusVar ObjektGui
    statusVar (_tvarMaps, _dynamischeWidgets, statusVar) = statusVar

instance MitSpracheGui (TVarMaps, DynamischeWidgets, StatusVar ObjektGui) where
    spracheGui :: (MonadIO m) => (TVarMaps, DynamischeWidgets, StatusVar ObjektGui) -> m SpracheGui
    spracheGui (_tvarMaps, _dynamischeWidgets, statusVar) = readSpracheGui statusVar

-- | Lese die 'SpracheGui' aus einer 'StatusVarGui'
readSpracheGui :: (MonadIO m) => StatusVarGui -> m SpracheGui
readSpracheGui statusVar = liftIO $ atomically (tryReadStatusVar statusVar) >>= pure . \case
    (Left status) -> status ^. sprache
    (Right spracheGui) -> spracheGui

-- * Darstellung von Streckenobjekten
-- ** Bahngeschwindigkeit
-- ** Streckenabschnitt
-- ** Weiche
-- ** Kupplung
instance Kategorie KUWidgets where
    kategorie :: KategorieText KUWidgets
    kategorie = KategorieText Language.kupplungen

-- | 'Kupplung' mit zugehörigen Widgets
data KUWidgets =
    KUWidgets
    { ku :: Kupplung
    , kuWidget :: Gtk.VBox
    , kuFunctionBox :: Gtk.HBox
    , kuHinzWS :: CheckButtonWegstreckeHinzufügen Void KUWidgets
    , kuHinzPL :: ButtonPlanHinzufügen KUWidgets
    , kuTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , kuTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

instance MitWidget KUWidgets where
    erhalteWidget :: KUWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . kuWidget

instance WidgetsTyp KUWidgets where
    type ObjektTyp KUWidgets = Kupplung

    erhalteObjektTyp :: KUWidgets -> Kupplung
    erhalteObjektTyp = ku

    entferneWidgets :: (MonadIO m, WidgetsTypReader r KUWidgets m) => KUWidgets -> m ()
    entferneWidgets kuWidgets@KUWidgets {kuTVarSprache} = do
        DynamischeWidgets {vBoxKupplungen} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxKupplungen kuWidgets
        entferneHinzufügenWegstreckeWidgets kuWidgets
        entferneHinzufügenPlanWidgets kuWidgets
        liftIO $ atomically $ writeTVar kuTVarSprache Nothing

    boxButtonEntfernen :: KUWidgets -> Gtk.Box
    boxButtonEntfernen = erhalteBox . kuFunctionBox

    tvarSprache :: KUWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = kuTVarSprache

    tvarEvent :: KUWidgets -> TVar EventAusführen
    tvarEvent = kuTVarEvent

instance WegstreckenElement KUWidgets where
    getterWegstrecke :: Lens.Getter KUWidgets (CheckButtonWegstreckeHinzufügen Void KUWidgets)
    getterWegstrecke = Lens.to kuHinzWS

    boxWegstrecke :: (ReaderConstraint KUWidgets r)
                  => Kupplung
                  -> Lens.Getter r (BoxWegstreckeHinzufügen KUWidgets)
    boxWegstrecke _kuWidgets = Lens.to vBoxHinzufügenWegstreckeKupplungen

instance PlanElement KUWidgets where
    foldPlan :: Lens.Fold KUWidgets (Maybe (ButtonPlanHinzufügen KUWidgets))
    foldPlan = Lens.to $ Just . kuHinzPL

    boxenPlan
        :: (ReaderConstraint KUWidgets r) => Kupplung -> Lens.Fold r (BoxPlanHinzufügen KUWidgets)
    boxenPlan _kuWidgets = Lens.to vBoxHinzufügenPlanKupplungen

instance StreckenObjekt KUWidgets where
    anschlüsse :: KUWidgets -> Set Anschluss
    anschlüsse KUWidgets {ku} = anschlüsse ku

    erhalteName :: KUWidgets -> Text
    erhalteName KUWidgets {ku} = erhalteName ku

instance Aeson.ToJSON KUWidgets where
    toJSON :: KUWidgets -> Aeson.Value
    toJSON KUWidgets {ku} = Aeson.toJSON ku

instance KupplungKlasse KUWidgets where
    kuppeln :: (I2CReader r m, MonadIO m) => KUWidgets -> m ()
    kuppeln KUWidgets {ku} = kuppeln ku

-- | 'Kupplung' darstellen und zum Status hinzufügen
kupplungPackNew :: forall m. (ObjektGuiReader m, MonadIO m) => Kupplung -> MStatusGuiT m KUWidgets
kupplungPackNew kupplung@Kupplung {kupplungsAnschluss} = do
    DynamischeWidgets {vBoxKupplungen, vBoxHinzufügenPlanKupplungen} <- erhalteDynamischeWidgets
    (kuTVarSprache, kuTVarEvent) <- liftIO $ do
        kuTVarSprache <- newTVarIO $ Just []
        kuTVarEvent <- newTVarIO EventAusführen
        pure (kuTVarSprache, kuTVarEvent)
    let justTVarSprache = Just kuTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckePackNew kupplung kuTVarSprache
    hinzufügenPlanWidget
        <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanKupplungen kupplung kuTVarSprache
    -- Widget erstellen
    (vBox, kuFunctionBox) <- liftIO $ do
        vBox <- boxPackWidgetNewDefault vBoxKupplungen $ Gtk.vBoxNew False 0
        kuFunctionBox <- boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
        pure (vBox, kuFunctionBox)
    let kuWidgets =
            KUWidgets
            { ku = kupplung
            , kuWidget = vBox
            , kuFunctionBox
            , kuHinzPL = hinzufügenPlanWidget
            , kuHinzWS = hinzufügenWegstreckeWidget
            , kuTVarSprache
            , kuTVarEvent
            }
    namePackNew kuFunctionBox kupplung
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse
            <- boxPackWidgetNew kuFunctionBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    boxPackWidgetNewDefault vBoxAnschlüsse
        $ anschlussNew justTVarSprache Language.kupplung kupplungsAnschluss
    buttonKuppelnPackNew kuFunctionBox kupplung kuTVarSprache kuTVarEvent
    fließendPackNew vBoxAnschlüsse kupplung justTVarSprache
    buttonEntfernenPackNew kuWidgets $ entfernenKupplung kuWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OKupplung kuWidgets
    pure kuWidgets

-- | Füge 'Gtk.Button' zum kuppeln zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus
-- 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonKuppelnPackNew
    :: forall b k m.
    (MitBox b, KupplungKlasse k, ObjektGuiReader m, MonadIO m)
    => b
    -> k
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> m Gtk.Button
buttonKuppelnPackNew box kupplung tvarSprachwechsel tvarEventAusführen = do
    statusVar <- erhalteStatusVar :: m StatusVarGui
    objektReader <- ask
    boxPackWidgetNewDefault box
        $ buttonNewWithEventLabel (Just tvarSprachwechsel) Language.kuppeln
        $ eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ ausführenStatusVarAktion (Kuppeln kupplung) statusVar

-- ** Kontakt
instance Kategorie KOWidgets where
    kategorie :: KategorieText KOWidgets
    kategorie = KategorieText Language.kontakte

data KOWidgets = KOWidgets
    deriving Eq

kontaktPackNew :: (MonadIO m) => Kontakt -> m KOWidgets
kontaktPackNew = _undefined --TODO

instance Aeson.ToJSON KOWidgets where
    toJSON :: KOWidgets -> Aeson.Value
    toJSON = _undefined --TODO

-- ** Wegstrecke
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

instance (PlanElement (WSWidgets z)) => WidgetsTyp (WSWidgets z) where
    type ObjektTyp (WSWidgets z) = Wegstrecke z

    erhalteObjektTyp :: WSWidgets z -> Wegstrecke z
    erhalteObjektTyp = ws

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (WSWidgets z) m) => WSWidgets z -> m ()
    entferneWidgets wsWidgets@WSWidgets {wsTVarSprache} = do
        DynamischeWidgets {vBoxWegstrecken} <- erhalteDynamischeWidgets
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

instance StreckenabschnittKlasse (WSWidgets z) where
    strom :: (I2CReader r m, MonadIO m) => WSWidgets z -> Strom -> m ()
    strom WSWidgets {ws, wsToggleButtonStrom, wsTVarEvent} wert = do
        eventAusführen wsTVarEvent $ strom ws wert
        case wsToggleButtonStrom of
            (Just toggleButtonStrom) -> liftIO
                $ ohneEvent wsTVarEvent
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (wert == Fließend)]
            Nothing -> pure ()

instance KupplungKlasse (WSWidgets z) where
    kuppeln :: (I2CReader r m, MonadIO m) => WSWidgets z -> m ()
    kuppeln WSWidgets {ws, wsTVarEvent} = eventAusführen wsTVarEvent $ kuppeln ws

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
    :: forall m z.
    ( ObjektGuiReader m
    , MonadIO m
    , ZugtypKlasse z
    , PlanElement (WSWidgets z)
    , WegstreckeKlasse (Wegstrecke z)
    )
    => Wegstrecke z
    -> MStatusGuiT m (WSWidgets z)
wegstreckePackNew
    wegstrecke@Wegstrecke
    {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen} = do
    objektReader <- ask
    statusVar <- erhalteStatusVar :: MStatusGuiT m StatusVarGui
    dynamischeWidgets@DynamischeWidgets {vBoxWegstrecken} <- erhalteDynamischeWidgets
    (wsTVarSprache, wsTVarEvent) <- liftIO $ do
        wsTVarSprache <- newTVarIO $ Just []
        wsTVarEvent <- newTVarIO EventAusführen
        pure (wsTVarSprache, wsTVarEvent)
    let justTVarSprache = Just wsTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    let [boxBGPwm, boxBGKonstanteSpannung, boxBG, boxStreckenabschnitt, boxKupplung, boxWegstrecke] =
            dynamischeWidgets ^.. boxenPlan wegstrecke
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
        buttonKuppelnPackNew functionBox wegstrecke wsTVarSprache wsTVarEvent
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
    buttonEntfernenPackNew wsWidgets $ entfernenWegstrecke $ zuZugtypEither wsWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OWegstrecke $ zuZugtypEither wsWidgets
    pure wsWidgets
    where
        appendName :: (StreckenObjekt o) => Maybe (Sprache -> Text) -> o -> Maybe (Sprache -> Text)

        -- Maybe necessary here, because otherwise (compare strings) this would lead to O(n!) runtime
        appendName Nothing objekt = Just $ const $ erhalteName objekt
        appendName (Just acc) objekt = Just $ acc <^> erhalteName objekt

        fahrstromAnschlüsse :: Bahngeschwindigkeit 'KonstanteSpannung z -> NonEmpty Anschluss
        fahrstromAnschlüsse
            MärklinBahngeschwindigkeitKonstanteSpannung {bgmkFahrstromAnschlüsse} =
            bgmkFahrstromAnschlüsse

-- ** Plan
instance Kategorie PLWidgets where
    kategorie :: KategorieText PLWidgets
    kategorie = KategorieText Language.pläne

-- | 'Plan' mit zugehörigen Widgets
data PLWidgets =
    PLWidgets
    { pl :: Plan
    , plWidget :: Gtk.Frame
    , plFunktionBox :: Gtk.Box
    , plHinzPL :: ButtonPlanHinzufügen PLWidgets
    , plTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , plTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

instance MitWidget PLWidgets where
    erhalteWidget :: PLWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . plWidget

instance WidgetsTyp PLWidgets where
    type ObjektTyp PLWidgets = Plan

    erhalteObjektTyp :: PLWidgets -> Plan
    erhalteObjektTyp = pl

    entferneWidgets :: (MonadIO m, WidgetsTypReader r PLWidgets m) => PLWidgets -> m ()
    entferneWidgets plWidgets@PLWidgets {plTVarSprache} = do
        DynamischeWidgets {vBoxPläne} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxPläne plWidgets
        liftIO $ atomically $ writeTVar plTVarSprache Nothing

    boxButtonEntfernen :: PLWidgets -> Gtk.Box
    boxButtonEntfernen = plFunktionBox

    tvarSprache :: PLWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = plTVarSprache

    tvarEvent :: PLWidgets -> TVar EventAusführen
    tvarEvent = plTVarEvent

instance PlanElement PLWidgets where
    foldPlan :: Lens.Fold PLWidgets (Maybe (ButtonPlanHinzufügen PLWidgets))
    foldPlan = Lens.to $ Just . plHinzPL

    boxenPlan
        :: (ReaderConstraint PLWidgets r) => Plan -> Lens.Fold r (BoxPlanHinzufügen PLWidgets)
    boxenPlan _kuWidgets = Lens.to vBoxHinzufügenPlanPläne

instance StreckenObjekt PLWidgets where
    anschlüsse :: PLWidgets -> Set Anschluss
    anschlüsse PLWidgets {pl} = anschlüsse pl

    erhalteName :: PLWidgets -> Text
    erhalteName PLWidgets {pl} = erhalteName pl

instance Aeson.ToJSON PLWidgets where
    toJSON :: PLWidgets -> Aeson.Value
    toJSON (PLWidgets {pl}) = Aeson.toJSON pl

instance PlanKlasse PLWidgets where
    ausführenPlan
        :: (AusführendReader r m, MonadIO m) => PLWidgets -> (Natural -> IO ()) -> IO () -> m ()
    ausführenPlan PLWidgets {pl, plTVarEvent} anzeigeAktion abschlussAktion =
        eventAusführen plTVarEvent $ ausführenPlan pl anzeigeAktion abschlussAktion

-- | 'Plan' darstellen
planPackNew :: forall m. (ObjektGuiReader m, MonadIO m) => Plan -> MStatusGuiT m PLWidgets
planPackNew plan@Plan {plAktionen} = do
    statusVar <- erhalteStatusVar :: MStatusGuiT m StatusVarGui
    objektReader <- ask
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets
        {vBoxPläne, windowMain, vBoxHinzufügenPlanPläne} <- erhalteDynamischeWidgets
    (plTVarSprache, plTVarEvent, frame, fnBox, expander, btAusführen, btAbbrechen, dialogGesperrt)
        <- liftIO $ do
            plTVarSprache <- newTVarIO $ Just []
            plTVarEvent <- newTVarIO EventAusführen
            -- Widget erstellen
            frame <- boxPackWidgetNewDefault vBoxPläne $ Gtk.frameNew
            vBox <- containerAddWidgetNew frame $ Gtk.vBoxNew False 0
            namePackNew vBox plan
            expander <- boxPackWidgetNewDefault vBox $ Gtk.expanderNew $ Text.empty
            vBoxExpander <- containerAddWidgetNew expander $ Gtk.vBoxNew False 0
            forM_ plAktionen
                $ boxPackWidgetNewDefault vBoxExpander
                . Gtk.labelNew
                . Just
                . flip leseSprache spracheGui
                . anzeige
            functionBox <- boxPackWidgetNewDefault vBox Gtk.hButtonBoxNew
            buttonAusführen
                <- boxPackWidgetNew functionBox PackNatural paddingDefault positionDefault
                $ Gtk.buttonNew
            buttonAbbrechen
                <- boxPackWidgetNew functionBox PackNatural paddingDefault positionDefault
                $ Gtk.buttonNew
            Gtk.widgetHide buttonAbbrechen
            dialogGesperrt <- Gtk.messageDialogNew
                (Just windowMain)
                []
                Gtk.MessageError
                Gtk.ButtonsOk
                ("" :: Text)
            progressBar <- boxPackWidgetNew
                functionBox
                PackGrow
                paddingDefault
                positionDefault
                Gtk.progressBarNew
            Gtk.on buttonAusführen Gtk.buttonActivated
                $ flip runReaderT objektReader
                $ auswertenStatusVarIOStatus (ausführenMöglich plan) statusVar >>= \case
                    AusführenMöglich -> void $ do
                        liftIO $ do
                            Gtk.widgetHide buttonAusführen
                            Gtk.widgetShow buttonAbbrechen
                        ausführenStatusVarBefehl
                            (Ausführen plan (const . anzeigeAktion) abschlussAktion)
                            statusVar
                        where
                            anzeigeAktion :: Natural -> IO ()
                            anzeigeAktion wert =
                                Gtk.set
                                    progressBar
                                    [ Gtk.progressBarFraction := (fromIntegral wert)
                                          / (fromIntegral $ length plAktionen)]

                            abschlussAktion :: IO ()
                            abschlussAktion = do
                                Gtk.widgetShow buttonAusführen
                                Gtk.widgetHide buttonAbbrechen
                    WirdAusgeführt -> error "Ausführen in GTK-UI erneut gestartet."
                    (AnschlüsseBelegt anschlüsse) -> void $ do
                        liftIO $ flip leseSprache spracheGui $ \sprache -> Gtk.set
                            dialogGesperrt
                            [ Gtk.messageDialogText := Just
                                  $ (Language.ausführenGesperrt $# anschlüsse) sprache]
                        dialogEval dialogGesperrt
            Gtk.on buttonAbbrechen Gtk.buttonActivated $ do
                flip runReaderT objektReader
                    $ ausführenStatusVarBefehl (AusführenAbbrechen plan) statusVar
                Gtk.widgetShow buttonAusführen
                Gtk.widgetHide buttonAbbrechen
            pure
                ( plTVarSprache
                , plTVarEvent
                , frame
                , functionBox
                , expander
                , buttonAusführen
                , buttonAbbrechen
                , dialogGesperrt
                )
    plHinzPL <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanPläne plan plTVarSprache
    let plWidgets =
            PLWidgets
            { pl = plan
            , plWidget = frame
            , plFunktionBox = erhalteBox fnBox
            , plHinzPL
            , plTVarSprache
            , plTVarEvent
            }
    buttonEntfernenPackNew plWidgets $ entfernenPlan plWidgets
    let justTVarSprache = Just plTVarSprache
    verwendeSpracheGui justTVarSprache $ \sprache -> do
        Gtk.set expander [Gtk.expanderLabel := (Language.aktionen <:> length plAktionen $ sprache)]
        Gtk.set btAusführen [Gtk.buttonLabel := Language.ausführen sprache]
        Gtk.set btAbbrechen [Gtk.buttonLabel := Language.ausführenAbbrechen sprache]
        Gtk.set dialogGesperrt [Gtk.windowTitle := Language.aktionGesperrt sprache]
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OPlan plWidgets
    pure plWidgets
#endif
--
