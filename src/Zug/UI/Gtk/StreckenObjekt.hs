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
import Zug.UI.Gtk.StreckenObjekt.KOWidgets (KOWidgets, kontaktPackNew)
import Zug.UI.Gtk.StreckenObjekt.KUWidgets (KUWidgets, kupplungPackNew)
import Zug.UI.Gtk.StreckenObjekt.PLWidgets (PLWidgets, planPackNew)
import Zug.UI.Gtk.StreckenObjekt.STWidgets
       (STWidgets(..), STWidgetsKlasse(..), streckenabschnittPackNew, toggleButtonStromPackNew
      , STWidgetsBoxen(..), MitSTWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.WEWidgets
       (WEWidgets(..), weichePackNew, WEWidgetsBoxen(..), MitWEWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.WSWidgets (WSWidgets, wegstreckePackNew)
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
-- ** Kontakt
-- ** Wegstrecke
-- ** Plan
#endif
--
