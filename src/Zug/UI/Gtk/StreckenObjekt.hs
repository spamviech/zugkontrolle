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
  , STWidgets()
  , WEWidgets()
  , KUWidgets()
  , WSWidgets()
  , PLWidgets()
  , WidgetsTyp(..)
  , bahngeschwindigkeitPackNew
  , streckenabschnittPackNew
  , weichePackNew
  , kupplungPackNew
  , wegstreckePackNew
  , planPackNew
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
      , verwendetPwm, BahngeschwindigkeitKlasse(..), Streckenabschnitt(..)
      , StreckenabschnittKlasse(..), Weiche(..), WeicheKlasse(..), Kupplung(..), KupplungKlasse(..)
      , Wegstrecke(..), WegstreckeKlasse(..))
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
import Zug.UI.StatusVar (StatusVar, MitStatusVar(..), StatusVarReader(..), tryReadStatusVar
                       , auswertenStatusVarIOStatus, ausführenStatusVarBefehl
                       , ausführenStatusVarAktion, auswertenStatusVarMStatusT)

-- * Sammel-Typ um dynamische Widgets zu speichern
-- | Sammel-Typ spezialisiert auf Gui-Typen
type ObjektGui = ObjektAllgemein BGWidgets STWidgets WEWidgets KUWidgets WSWidgets PLWidgets

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

    type WS ObjektGui = WSWidgets

    type PL ObjektGui = PLWidgets

    type SP ObjektGui = SpracheGui

    erhalteObjekt :: ObjektGui -> ObjektGui
    erhalteObjekt = id

    ausObjekt :: ObjektGui -> ObjektGui
    ausObjekt = id

-- | Auswahlmöglichkeiten zu 'WidgetHinzufügen'
data HinzufügenZiel
    = HinzufügenWegstrecke
    | HinzufügenPlan

-- | Widget zum Hinzufügen einer Wegstrecke/eines Plans
newtype WidgetHinzufügen (e :: HinzufügenZiel) (w :: Type) (a :: Type) =
    WidgetHinzufügen { widgetHinzufügen :: w }
    deriving (Eq)

instance (MitWidget w) => MitWidget (WidgetHinzufügen e w a) where
    erhalteWidget :: WidgetHinzufügen e w a -> Gtk.Widget
    erhalteWidget = erhalteWidget . widgetHinzufügen

instance (MitRegistrierterCheckButton w)
    => MitRegistrierterCheckButton (WidgetHinzufügen e w a) where
    erhalteRegistrierterCheckButton :: WidgetHinzufügen e w a -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton = erhalteRegistrierterCheckButton . widgetHinzufügen

-- | Konvertiere ein 'WidgetHinzufügen' in das zugehörige 'ZugtypEither'-Äquivalent
widgetHinzufügenZugtypEither
    :: WidgetHinzufügen e w (a z) -> WidgetHinzufügen e w (ZugtypEither a)
widgetHinzufügenZugtypEither = WidgetHinzufügen . widgetHinzufügen

-- | Konvertiere ein 'WidgetHinzufügen' in das zugehörige 'GeschwindigkeitEither'-Äquivalent.
widgetHinzufügenGeschwindigkeitEither
    :: WidgetHinzufügen e w (a g z) -> WidgetHinzufügen e w (GeschwindigkeitEither a z)
widgetHinzufügenGeschwindigkeitEither = WidgetHinzufügen . widgetHinzufügen

-- | Konvertiere ein 'WidgetHinzufügen' in eine beliebiges 'GeschwindigkeitVariante'-Äquivalent.
widgetHinzufügenGeschwindigkeitVariante
    :: WidgetHinzufügen e w (GeschwindigkeitEither a z) -> WidgetHinzufügen e w (a g z)
widgetHinzufügenGeschwindigkeitVariante = WidgetHinzufügen . widgetHinzufügen

-- | Konvertiere ein 'WidgetHinzufügen'-'MitRegistrierterCheckButton' in den zugehörigen 'Void'-Typ
widgetHinzufügenRegistrierterCheckButtonVoid
    :: (MitRegistrierterCheckButton r)
    => WidgetHinzufügen e r a
    -> WidgetHinzufügen e RegistrierterCheckButton Void
widgetHinzufügenRegistrierterCheckButtonVoid =
    WidgetHinzufügen . erhalteRegistrierterCheckButton . widgetHinzufügen

-- | Erhalte die aktuelle Auswahl des inkludierten 'MitAuswahlWidget'
widgetHinzufügenAktuelleAuswahl
    :: (Eq b, MitAuswahlWidget w b, MonadIO m) => WidgetHinzufügen e w a -> m b
widgetHinzufügenAktuelleAuswahl = aktuelleAuswahl . erhalteAuswahlWidget . widgetHinzufügen

-- | Überprüfe, ob der inkludierte 'MitRegistrierterCheckButton' aktuell gedrückt ist.
widgetHinzufügenToggled
    :: (MitRegistrierterCheckButton t, MonadIO m) => WidgetHinzufügen e t a -> m Bool
widgetHinzufügenToggled =
    registrierterCheckButtonToggled . erhalteRegistrierterCheckButton . widgetHinzufügen

-- | Entferne ein 'WidgetHinzufügen' (falls vorhanden) aus dem zugehörigen Container
widgetHinzufügenContainerRemoveJust
    :: (MitContainer c, MitWidget w, MonadIO m)
    => WidgetHinzufügen e c a
    -> Maybe (WidgetHinzufügen e w a)
    -> m ()
widgetHinzufügenContainerRemoveJust c w =
    containerRemoveJust (widgetHinzufügen c) $ widgetHinzufügen <$> w

-- | Füge ein 'WidgetHinzufügen' zu einer zugehörigen Box hinzu
widgetHinzufügenBoxPackNew :: (MitBox b, MitWidget w, MonadIO m)
                            => WidgetHinzufügen e b a
                            -> m w
                            -> m (WidgetHinzufügen e w a)
widgetHinzufügenBoxPackNew b =
    fmap WidgetHinzufügen . boxPackWidgetNewDefault (widgetHinzufügen b)

-- | Text mit Typ-Annotation
newtype KategorieText a = KategorieText { kategorieText :: Sprache -> Text }

-- | Label für 'BoxWegstreckeHinzufügen'/'BoxPlanHinzufügen'
class Kategorie a where
    kategorie :: KategorieText a

instance Kategorie (BGWidgets g z) where
    kategorie :: KategorieText (BGWidgets g z)
    kategorie = KategorieText Language.bahngeschwindigkeiten

instance Kategorie (GeschwindigkeitEither BGWidgets z) where
    kategorie :: KategorieText (GeschwindigkeitEither BGWidgets z)
    kategorie = KategorieText Language.bahngeschwindigkeiten

instance Kategorie STWidgets where
    kategorie :: KategorieText STWidgets
    kategorie = KategorieText Language.streckenabschnitte

instance Kategorie (WEWidgets z) where
    kategorie :: KategorieText (WEWidgets z)
    kategorie = KategorieText Language.weichen

instance Kategorie KUWidgets where
    kategorie :: KategorieText KUWidgets
    kategorie = KategorieText Language.kupplungen

instance Kategorie (WSWidgets z) where
    kategorie :: KategorieText (WSWidgets z)
    kategorie = KategorieText Language.wegstrecken

instance Kategorie PLWidgets where
    kategorie :: KategorieText PLWidgets
    kategorie = KategorieText Language.pläne

-- | CheckButton zum hinzufügen zu einer Wegstrecke
type CheckButtonWegstreckeHinzufügen e a =
    WidgetHinzufügen 'HinzufügenWegstrecke (WegstreckeCheckButton e) a

-- | Box zur Auswahl der 'Wegstrecke'n-Elemente
type BoxWegstreckeHinzufügen a =
    WidgetHinzufügen 'HinzufügenWegstrecke (ScrollbaresWidget Gtk.VBox) a

-- | Erstelle eine neue 'BoxWegstreckeHinzufügen'.
boxWegstreckeHinzufügenNew :: (MonadIO m) => m (BoxWegstreckeHinzufügen a)
boxWegstreckeHinzufügenNew =
    liftIO $ fmap WidgetHinzufügen $ scrollbaresWidgetNew $ Gtk.vBoxNew False 0

deriving instance (Eq e) => Eq (WegstreckeCheckButton e)

-- | 'RegistrierterCheckButton', potentiell mit zusätzlicher Richtungsauswahl.
data WegstreckeCheckButton e where
    WegstreckeCheckButton :: { wcbvRegistrierterCheckButton :: RegistrierterCheckButton }
        -> WegstreckeCheckButton Void
    WegstreckeCheckButtonRichtung :: { wcbrWidget :: Gtk.Widget
                                     , wcbrRegistrierterCheckButton :: RegistrierterCheckButton
                                     , wcbrRichtungsAuswahl :: AuswahlWidget Richtung
                                     } -> WegstreckeCheckButton Richtung

instance MitWidget (WegstreckeCheckButton e) where
    erhalteWidget :: WegstreckeCheckButton e -> Gtk.Widget
    erhalteWidget WegstreckeCheckButton {wcbvRegistrierterCheckButton} =
        erhalteWidget wcbvRegistrierterCheckButton
    erhalteWidget WegstreckeCheckButtonRichtung {wcbrWidget} = wcbrWidget

instance MitRegistrierterCheckButton (WegstreckeCheckButton e) where
    erhalteRegistrierterCheckButton :: WegstreckeCheckButton e -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton
        WegstreckeCheckButton {wcbvRegistrierterCheckButton} = wcbvRegistrierterCheckButton
    erhalteRegistrierterCheckButton
        WegstreckeCheckButtonRichtung {wcbrRegistrierterCheckButton} = wcbrRegistrierterCheckButton

instance MitAuswahlWidget (WegstreckeCheckButton Richtung) Richtung where
    erhalteAuswahlWidget :: WegstreckeCheckButton Richtung -> AuswahlWidget Richtung
    erhalteAuswahlWidget
        WegstreckeCheckButtonRichtung {wcbrRichtungsAuswahl} = wcbrRichtungsAuswahl

-- | Button zum hinzufügen eines Plans
type ButtonPlanHinzufügen a = WidgetHinzufügen 'HinzufügenPlan Gtk.Button a

-- | Box zum hinzufügen eines Plans
type BoxPlanHinzufügen a = WidgetHinzufügen 'HinzufügenPlan (ScrollbaresWidget Gtk.VBox) a

-- | Erstelle eine neue 'BoxPlanHinzufügen'.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
boxPlanHinzufügenNew :: forall a r m.
                      (Kategorie a, SpracheGuiReader r m, MonadIO m)
                      => Maybe (TVar (Maybe [Sprache -> IO ()]))
                      -> m (BoxPlanHinzufügen a)
boxPlanHinzufügenNew maybeTVar = fmap WidgetHinzufügen $ scrollbaresWidgetNew $ do
    box <- liftIO $ Gtk.vBoxNew False 0
    boxPackWidgetNewDefault box
        $ labelSpracheNew maybeTVar
        $ kategorieText (kategorie :: KategorieText a)
    pure box

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

-- | Klasse für Widgets-Repräsentation von Objekt-Typen
class (MitWidget s) => WidgetsTyp s where
    -- | Assoziierter 'Objekt'-Typ
    type ObjektTyp s

    -- | Erhalte den eingebetteten 'ObjektTyp'
    erhalteObjektTyp :: s -> ObjektTyp s

    -- | Entferne Widgets inklusive aller Hilfswidgets aus den entsprechenden Boxen.
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => s -> m ()

    -- | 'Gtk.Box', in die der Entfernen-Knopf von 'buttonEntfernenPack' gepackt wird.
    -- Hier definiert, damit keine 'MitBox'-Instanz notwendig ist.
    boxButtonEntfernen :: s -> Gtk.Box

    -- | Erhalte die 'TVar', die steuert welche Widgets bei 'Zug.UI.Gtk.SpracheGui.sprachwechsel'
    -- angepasst werden.
    tvarSprache :: s -> TVar (Maybe [Sprache -> IO ()])

    -- | Erhalte die 'TVar', die steuert ob Events ausgeführt werden.
    tvarEvent :: s -> TVar EventAusführen

-- | Soll das zugehörige Event ausgeführt werden?
data EventAusführen
    = EventAusführen
    | EventIgnorieren
    deriving (Eq, Show)

-- | Führe ein Event aus oder ignoriere es.
eventAusführen :: (MonadIO m) => TVar EventAusführen -> m () -> m ()
eventAusführen tvar aktion = liftIO (readTVarIO tvar) >>= \case
    EventAusführen -> aktion
    EventIgnorieren -> pure ()

-- | Führe eine Gtk-Aktion ohne zugehöriges Event aus.
ohneEvent :: TVar EventAusführen -> IO () -> IO ()
ohneEvent tvarEventAusführen aktion = Gtk.postGUIAsync $ do
    alterWert <- atomically $ swapTVar tvarEventAusführen EventIgnorieren
    aktion
    when (alterWert == EventAusführen)
        $ atomically
        $ writeTVar tvarEventAusführen EventAusführen

-- | Klasse für Gui-Darstellung von Typen, die zur Erstellung einer 'Wegstrecke' verwendet werden.
class (WidgetsTyp s) => WegstreckenElement s where
    -- | Auswahl-Typ beim erstellen einer Wegstrecke
    type CheckButtonAuswahl s

    type CheckButtonAuswahl s = Void

    -- | Getter auf 'RegistrierterCheckButton', ob 'StreckenObjekt' zu einer 'Wegstrecke' hinzugefügt werden soll
    getterWegstrecke :: Lens.Getter s (CheckButtonWegstreckeHinzufügen (CheckButtonAuswahl s) s)

    -- | Assoziierte 'BoxWegstreckeHinzufügen', in der 'MitRegistrierterCheckButton' gepackt ist.
    boxWegstrecke :: ObjektTyp s -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen s)

-- | Entferne 'Widget's zum Hinzufügen zu einer 'Wegstrecke' aus der entsprechenden Box
entferneHinzufügenWegstreckeWidgets
    :: forall s r m. (WegstreckenElement s, DynamischeWidgetsReader r m, MonadIO m) => s -> m ()
entferneHinzufügenWegstreckeWidgets wegsteckenElement = do
    box <- Lens.view (boxWegstrecke $ erhalteObjektTyp wegsteckenElement)
        <$> erhalteDynamischeWidgets :: m (BoxWegstreckeHinzufügen s)
    widgetHinzufügenContainerRemoveJust box $ Just $ wegsteckenElement ^. getterWegstrecke

-- | Typ-unspezifischer 'RegistrierterCheckButton' zum hinzufügen einer Wegstrecke
type WegstreckeCheckButtonVoid =
    WidgetHinzufügen 'HinzufügenWegstrecke RegistrierterCheckButton Void

-- | Alle 'RegistrierterCheckButton' zum hinzufügen einer Wegstrecke im aktuellen 'StatusGui'
foldWegstreckeHinzufügen :: Lens.Fold StatusGui WegstreckeCheckButtonVoid
foldWegstreckeHinzufügen = Lens.folding registrierteCheckButtons
    where
        registrierteCheckButtons :: StatusGui -> [WegstreckeCheckButtonVoid]
        registrierteCheckButtons status =
            map
                (widgetHinzufügenRegistrierterCheckButtonVoid . Lens.view getterWegstrecke)
                (status ^. bahngeschwindigkeiten)
            ++ map
                (widgetHinzufügenRegistrierterCheckButtonVoid . Lens.view getterWegstrecke)
                (status ^. streckenabschnitte)
            ++ map
                (widgetHinzufügenRegistrierterCheckButtonVoid . Lens.view getterWegstrecke)
                (status ^. weichen)
            ++ map
                (widgetHinzufügenRegistrierterCheckButtonVoid . Lens.view getterWegstrecke)
                (status ^. kupplungen)

-- | Klasse für Gui-Darstellungen von Typen, die zur Erstellung eines 'Plan's verwendet werden.
class (WidgetsTyp s) => PlanElement s where
    -- | Faltung auf 'Gtk.Button's (falls vorhanden), welches 'StreckenObjekt' für eine 'Aktion' verwendet werden soll
    foldPlan :: Lens.Fold s (Maybe (ButtonPlanHinzufügen s))

    -- | Aller assoziierten 'BoxPlanHinzufügen', in denen jeweiliger 'ButtonPlanHinzufügen' gepackt ist.
    -- Die Reihenfolge muss zum Ergebnis von 'foldPlan' passen.
    -- Wird für 'entferneHinzufügenPlanWidgets' benötigt.
    boxenPlan :: ObjektTyp s -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen s)

-- | Entferne 'Widget's zum 'Plan' erstellen aus den entsprechenden 'Box'en.
entferneHinzufügenPlanWidgets
    :: forall s r m. (PlanElement s, DynamischeWidgetsReader r m, MonadIO m) => s -> m ()
entferneHinzufügenPlanWidgets planElement = do
    boxenPlan <- Lens.toListOf (boxenPlan $ erhalteObjektTyp planElement)
        <$> erhalteDynamischeWidgets :: m [BoxPlanHinzufügen s]
    sequence_
        $ widgetHinzufügenContainerRemoveJust <$> ZipList boxenPlan
        <*> ZipList (planElement ^.. foldPlan)

-- | Neuen Entfernen-Knopf an das Ende der zugehörigen 'Box' hinzufügen.
-- Beim drücken werden 'entferneWidgets' und die übergebene 'IOStatusGui'-Aktion ausgeführt.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonEntfernenPackNew
    :: (WidgetsTyp w, ObjektGuiReader m, MonadIO m) => w -> IOStatusGui () -> m Gtk.Button
buttonEntfernenPackNew w entfernenAktion = do
    statusVar <- erhalteStatusVar
    objektReader <- ask
    boxPackWidgetNew (boxButtonEntfernen w) PackNatural paddingDefault End
        $ buttonNewWithEventLabel (Just $ tvarSprache w) Language.entfernen
        $ flip runReaderT objektReader
        $ do
            auswertenStatusVarIOStatus entfernenAktion statusVar
            entferneWidgets w

-- ** Widget mit Name und CheckButton erstellen
-- | Erzeuge einen 'RegistrierterCheckButton' mit einem 'Label' für den Namen.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
hinzufügenWidgetWegstreckePackNew
    :: forall o m.
    (ObjektGuiReader m, StreckenObjekt (ObjektTyp o), WegstreckenElement o, MonadIO m)
    => ObjektTyp o
    -> TVar (Maybe [Sprache -> IO ()])
    -> m (CheckButtonWegstreckeHinzufügen Void o)
hinzufügenWidgetWegstreckePackNew objekt tvar = do
    dynamischeWidgets@DynamischeWidgets
        {fortfahrenWennToggledWegstrecke} <- erhalteDynamischeWidgets
    let box = dynamischeWidgets ^. boxWegstrecke objekt :: BoxWegstreckeHinzufügen o
    widgetHinzufügenBoxPackNew box
        $ WegstreckeCheckButton
        <$> registrierterCheckButtonNew
            (Just tvar)
            (const $ erhalteName objekt)
            fortfahrenWennToggledWegstrecke

-- | Erzeuge einen 'RegistrierterCheckButton'.
-- Dieser enthält ein 'Label' für den Namen und einem 'AuswahlWidget' für die übergebenen 'Richtung'en.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
hinzufügenWidgetWegstreckeRichtungPackNew
    :: forall o m.
    (ObjektGuiReader m, StreckenObjekt (ObjektTyp o), WegstreckenElement o, MonadIO m)
    => ObjektTyp o
    -> NonEmpty Richtung
    -> TVar (Maybe [Sprache -> IO ()])
    -> m (CheckButtonWegstreckeHinzufügen Richtung o)
hinzufügenWidgetWegstreckeRichtungPackNew objekt richtungen tvar = do
    dynamischeWidgets@DynamischeWidgets
        {fortfahrenWennToggledWegstrecke} <- erhalteDynamischeWidgets
    let box = dynamischeWidgets ^. boxWegstrecke objekt :: BoxWegstreckeHinzufügen o
    let justTVar = Just tvar
    widgetHinzufügenBoxPackNew box $ do
        hBox <- liftIO $ Gtk.hBoxNew False 0
        wcbrRegistrierterCheckButton <- boxPackWidgetNewDefault hBox
            $ registrierterCheckButtonNew
                justTVar
                (const $ erhalteName objekt)
                fortfahrenWennToggledWegstrecke
        wcbrRichtungsAuswahl <- boxPackWidgetNewDefault hBox
            $ auswahlRadioButtonNew richtungen justTVar
            $ const Text.empty
        pure
            WegstreckeCheckButtonRichtung
            { wcbrWidget = erhalteWidget hBox
            , wcbrRegistrierterCheckButton
            , wcbrRichtungsAuswahl
            }

-- | Füge einen Knopf mit dem Namen zur Box hinzu. Beim drücken wird die 'TMVar' mit dem Objekt gefüllt.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
hinzufügenWidgetPlanPackNew
    :: (ObjektGuiReader m, StreckenObjekt (ObjektTyp o), ObjektElement (ObjektTyp o), MonadIO m)
    => BoxPlanHinzufügen o
    -> ObjektTyp o
    -> TVar (Maybe [Sprache -> IO ()])
    -> m (ButtonPlanHinzufügen o)
hinzufügenWidgetPlanPackNew box objekt tvar = do
    DynamischeWidgets {tmvarPlanObjekt} <- erhalteDynamischeWidgets
    widgetHinzufügenBoxPackNew box
        $ buttonNewWithEventLabel (Just tvar) (const $ erhalteName objekt)
        $ atomically
        $ putTMVar tmvarPlanObjekt
        $ Just
        $ zuObjekt objekt

-- * Darstellung von Streckenobjekten
-- ** Bahngeschwindigkeit
-- | 'Bahngeschwindigkeit' mit zugehörigen Widgets
data BGWidgets (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
    BGWidgetsPwmMärklin
        :: { bgpm :: Bahngeschwindigkeit 'Pwm 'Märklin
           , bgpmWidget :: Gtk.VBox
           , bgpmFunctionBox :: Gtk.HBox
           , bgpmHinzWS :: CheckButtonWegstreckeHinzufügen Void (BGWidgets 'Pwm 'Märklin)
           , bgpmHinzPL :: ( ButtonPlanHinzufügen (BGWidgets 'Pwm 'Märklin)
                           , ButtonPlanHinzufügen (GeschwindigkeitEither BGWidgets 'Märklin)
                           )
           , bgpmTVarSprache :: TVar (Maybe [Sprache -> IO ()])
           , bgpmTVarEvent :: TVar EventAusführen
           , bgpmScaleGeschwindigkeit :: Gtk.HScale
           } -> BGWidgets 'Pwm 'Märklin
    BGWidgetsKonstanteSpannungMärklin
        :: { bgkm :: Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
           , bgkmWidget :: Gtk.VBox
           , bgkmFunctionBox :: Gtk.HBox
           , bgkmHinzWS
                 :: CheckButtonWegstreckeHinzufügen Void (BGWidgets 'KonstanteSpannung 'Märklin)
           , bgkmHinzPL :: ( ButtonPlanHinzufügen (BGWidgets 'KonstanteSpannung 'Märklin)
                           , ButtonPlanHinzufügen (GeschwindigkeitEither BGWidgets 'Märklin)
                           )
           , bgkmTVarSprache :: TVar (Maybe [Sprache -> IO ()])
           , bgkmTVarEvent :: TVar EventAusführen
           , bgkmAuswahlFahrstrom :: AuswahlWidget Word8
           } -> BGWidgets 'KonstanteSpannung 'Märklin
    BGWidgetsPwmLego
        :: { bgpl :: Bahngeschwindigkeit 'Pwm 'Lego
           , bgplWidget :: Gtk.VBox
           , bgplFunctionBox :: Gtk.HBox
           , bgplHinzWS :: CheckButtonWegstreckeHinzufügen Void (BGWidgets 'Pwm 'Lego)
           , bgplHinzPL :: ( ButtonPlanHinzufügen (BGWidgets 'Pwm 'Lego)
                           , ButtonPlanHinzufügen (GeschwindigkeitEither BGWidgets 'Lego)
                           )
           , bgplTVarSprache :: TVar (Maybe [Sprache -> IO ()])
           , bgplTVarEvent :: TVar EventAusführen
           , bgplScaleGeschwindigkeit :: Gtk.HScale
           , bgplAuswahlFahrtrichtung :: AuswahlWidget Fahrtrichtung
           } -> BGWidgets 'Pwm 'Lego
    BGWidgetsKonstanteSpannungLego
        :: { bgkl :: Bahngeschwindigkeit 'KonstanteSpannung 'Lego
           , bgklWidget :: Gtk.VBox
           , bgklFunctionBox :: Gtk.HBox
           , bgklHinzWS
                 :: CheckButtonWegstreckeHinzufügen Void (BGWidgets 'KonstanteSpannung 'Lego)
           , bgklHinzPL :: ( ButtonPlanHinzufügen (BGWidgets 'KonstanteSpannung 'Lego)
                           , ButtonPlanHinzufügen (GeschwindigkeitEither BGWidgets 'Lego)
                           )
           , bgklTVarSprache :: TVar (Maybe [Sprache -> IO ()])
           , bgklTVarEvent :: TVar EventAusführen
           , bgklAuswahlFahrstrom :: AuswahlWidget Word8
           , bgklAuswahlFahrtrichtung :: AuswahlWidget Fahrtrichtung
           } -> BGWidgets 'KonstanteSpannung 'Lego

deriving instance (Eq (ObjektTyp (BGWidgets g z))) => Eq (BGWidgets g z)

instance MitWidget (BGWidgets g z) where
    erhalteWidget :: BGWidgets g z -> Gtk.Widget
    erhalteWidget BGWidgetsPwmMärklin {bgpmWidget} = erhalteWidget bgpmWidget
    erhalteWidget BGWidgetsKonstanteSpannungMärklin {bgkmWidget} = erhalteWidget bgkmWidget
    erhalteWidget BGWidgetsPwmLego {bgplWidget} = erhalteWidget bgplWidget
    erhalteWidget BGWidgetsKonstanteSpannungLego {bgklWidget} = erhalteWidget bgklWidget

instance (WegstreckenElement (BGWidgets g z), PlanElement (BGWidgets g z))
    => WidgetsTyp (BGWidgets g z) where
    type ObjektTyp (BGWidgets g z) = Bahngeschwindigkeit g z

    erhalteObjektTyp :: BGWidgets g z -> Bahngeschwindigkeit g z
    erhalteObjektTyp BGWidgetsPwmMärklin {bgpm} = bgpm
    erhalteObjektTyp BGWidgetsKonstanteSpannungMärklin {bgkm} = bgkm
    erhalteObjektTyp BGWidgetsPwmLego {bgpl} = bgpl
    erhalteObjektTyp BGWidgetsKonstanteSpannungLego {bgkl} = bgkl

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => BGWidgets g z -> m ()
    entferneWidgets bgWidgets = do
        DynamischeWidgets {vBoxBahngeschwindigkeiten} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxBahngeschwindigkeiten bgWidgets
        entferneHinzufügenWegstreckeWidgets bgWidgets
        entferneHinzufügenPlanWidgets bgWidgets
        liftIO $ atomically $ writeTVar (tvarSprache bgWidgets) Nothing

    boxButtonEntfernen :: BGWidgets g z -> Gtk.Box
    boxButtonEntfernen BGWidgetsPwmMärklin {bgpmFunctionBox} = erhalteBox bgpmFunctionBox
    boxButtonEntfernen
        BGWidgetsKonstanteSpannungMärklin {bgkmFunctionBox} = erhalteBox bgkmFunctionBox
    boxButtonEntfernen BGWidgetsPwmLego {bgplFunctionBox} = erhalteBox bgplFunctionBox
    boxButtonEntfernen
        BGWidgetsKonstanteSpannungLego {bgklFunctionBox} = erhalteBox bgklFunctionBox

    tvarSprache :: BGWidgets g z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache BGWidgetsPwmMärklin {bgpmTVarSprache} = bgpmTVarSprache
    tvarSprache BGWidgetsKonstanteSpannungMärklin {bgkmTVarSprache} = bgkmTVarSprache
    tvarSprache BGWidgetsPwmLego {bgplTVarSprache} = bgplTVarSprache
    tvarSprache BGWidgetsKonstanteSpannungLego {bgklTVarSprache} = bgklTVarSprache

    tvarEvent :: BGWidgets g z -> TVar EventAusführen
    tvarEvent BGWidgetsPwmMärklin {bgpmTVarEvent} = bgpmTVarEvent
    tvarEvent BGWidgetsKonstanteSpannungMärklin {bgkmTVarEvent} = bgkmTVarEvent
    tvarEvent BGWidgetsPwmLego {bgplTVarEvent} = bgplTVarEvent
    tvarEvent BGWidgetsKonstanteSpannungLego {bgklTVarEvent} = bgklTVarEvent

instance WegstreckenElement (BGWidgets g 'Märklin) where
    getterWegstrecke
        :: Lens.Getter (BGWidgets g 'Märklin) (CheckButtonWegstreckeHinzufügen Void (BGWidgets g 'Märklin))
    getterWegstrecke = Lens.to erhalteCheckbuttonWegstrecke
        where
            erhalteCheckbuttonWegstrecke
                :: BGWidgets g 'Märklin
                -> CheckButtonWegstreckeHinzufügen Void (BGWidgets g 'Märklin)
            erhalteCheckbuttonWegstrecke BGWidgetsPwmMärklin {bgpmHinzWS} = bgpmHinzWS
            erhalteCheckbuttonWegstrecke
                BGWidgetsKonstanteSpannungMärklin {bgkmHinzWS} = bgkmHinzWS

    boxWegstrecke
        :: Bahngeschwindigkeit g 'Märklin
        -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (BGWidgets g 'Märklin))
    boxWegstrecke _bahngeschwindigkeit =
        Lens.to
        $ widgetHinzufügenGeschwindigkeitVariante
        . vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin

instance WegstreckenElement (BGWidgets g 'Lego) where
    getterWegstrecke
        :: Lens.Getter (BGWidgets g 'Lego) (CheckButtonWegstreckeHinzufügen Void (BGWidgets g 'Lego))
    getterWegstrecke = Lens.to erhalteCheckbuttonWegstrecke
        where
            erhalteCheckbuttonWegstrecke
                :: BGWidgets g 'Lego -> CheckButtonWegstreckeHinzufügen Void (BGWidgets g 'Lego)
            erhalteCheckbuttonWegstrecke BGWidgetsPwmLego {bgplHinzWS} = bgplHinzWS
            erhalteCheckbuttonWegstrecke BGWidgetsKonstanteSpannungLego {bgklHinzWS} = bgklHinzWS

    boxWegstrecke :: Bahngeschwindigkeit g 'Lego
                  -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (BGWidgets g 'Lego))
    boxWegstrecke _bahngeschwindigkeit =
        Lens.to
        $ widgetHinzufügenGeschwindigkeitVariante
        . vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego

instance MitWidget (GeschwindigkeitEither BGWidgets z) where
    erhalteWidget :: GeschwindigkeitEither BGWidgets z -> Gtk.Widget
    erhalteWidget = ausGeschwindigkeitEither erhalteWidget

instance WegstreckenElement (GeschwindigkeitEither BGWidgets 'Märklin) where
    getterWegstrecke
        :: Lens.Getter (GeschwindigkeitEither BGWidgets 'Märklin) (CheckButtonWegstreckeHinzufügen Void (GeschwindigkeitEither BGWidgets 'Märklin))
    getterWegstrecke =
        Lens.to
        $ ausGeschwindigkeitEither
        $ widgetHinzufügenGeschwindigkeitEither . Lens.view getterWegstrecke

    boxWegstrecke
        :: GeschwindigkeitEither Bahngeschwindigkeit 'Märklin
        -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (GeschwindigkeitEither BGWidgets 'Märklin))
    boxWegstrecke _bgWidgets = Lens.to $ vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin

instance WegstreckenElement (GeschwindigkeitEither BGWidgets 'Lego) where
    getterWegstrecke
        :: Lens.Getter (GeschwindigkeitEither BGWidgets 'Lego) (CheckButtonWegstreckeHinzufügen Void (GeschwindigkeitEither BGWidgets 'Lego))
    getterWegstrecke =
        Lens.to
        $ ausGeschwindigkeitEither
        $ widgetHinzufügenGeschwindigkeitEither . Lens.view getterWegstrecke

    boxWegstrecke
        :: GeschwindigkeitEither Bahngeschwindigkeit 'Lego
        -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (GeschwindigkeitEither BGWidgets 'Lego))
    boxWegstrecke _bgWidgets = Lens.to vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego

instance ( WegstreckenElement (BGWidgets 'Pwm z)
         , WegstreckenElement (BGWidgets 'KonstanteSpannung z)
         ) => WidgetsTyp (GeschwindigkeitEither BGWidgets z) where
    type ObjektTyp (GeschwindigkeitEither BGWidgets z) =
        GeschwindigkeitEither Bahngeschwindigkeit z

    erhalteObjektTyp
        :: GeschwindigkeitEither BGWidgets z -> GeschwindigkeitEither Bahngeschwindigkeit z
    erhalteObjektTyp (GeschwindigkeitPwm bg) = GeschwindigkeitPwm $ erhalteObjektTyp bg
    erhalteObjektTyp (GeschwindigkeitKonstanteSpannung bg) =
        GeschwindigkeitKonstanteSpannung $ erhalteObjektTyp bg

    entferneWidgets
        :: (MonadIO m, DynamischeWidgetsReader r m) => GeschwindigkeitEither BGWidgets z -> m ()
    entferneWidgets (GeschwindigkeitPwm bgWidgets) = entferneWidgets bgWidgets
    entferneWidgets (GeschwindigkeitKonstanteSpannung bgWidgets) = entferneWidgets bgWidgets

    boxButtonEntfernen :: GeschwindigkeitEither BGWidgets z -> Gtk.Box
    boxButtonEntfernen (GeschwindigkeitPwm bgWidgets) = boxButtonEntfernen bgWidgets
    boxButtonEntfernen (GeschwindigkeitKonstanteSpannung bgWidgets) = boxButtonEntfernen bgWidgets

    tvarSprache :: GeschwindigkeitEither BGWidgets z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (GeschwindigkeitPwm bgWidgets) = tvarSprache bgWidgets
    tvarSprache (GeschwindigkeitKonstanteSpannung bgWidgets) = tvarSprache bgWidgets

    tvarEvent :: GeschwindigkeitEither BGWidgets z -> TVar EventAusführen
    tvarEvent (GeschwindigkeitPwm bgWidgets) = tvarEvent bgWidgets
    tvarEvent (GeschwindigkeitKonstanteSpannung bgWidgets) = tvarEvent bgWidgets

instance WidgetsTyp (ZugtypEither (GeschwindigkeitEither BGWidgets)) where
    type ObjektTyp (ZugtypEither (GeschwindigkeitEither BGWidgets)) =
        ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)

    erhalteObjektTyp :: ZugtypEither (GeschwindigkeitEither BGWidgets)
                     -> ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)
    erhalteObjektTyp (ZugtypMärklin (GeschwindigkeitPwm bg)) =
        ZugtypMärklin $ GeschwindigkeitPwm $ erhalteObjektTyp bg
    erhalteObjektTyp (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bg)) =
        ZugtypMärklin $ GeschwindigkeitKonstanteSpannung $ erhalteObjektTyp bg
    erhalteObjektTyp (ZugtypLego (GeschwindigkeitPwm bg)) =
        ZugtypLego $ GeschwindigkeitPwm $ erhalteObjektTyp bg
    erhalteObjektTyp (ZugtypLego (GeschwindigkeitKonstanteSpannung bg)) =
        ZugtypLego $ GeschwindigkeitKonstanteSpannung $ erhalteObjektTyp bg

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m)
                    => ZugtypEither (GeschwindigkeitEither BGWidgets)
                    -> m ()
    entferneWidgets (ZugtypMärklin (GeschwindigkeitPwm bgWidgets)) = entferneWidgets bgWidgets
    entferneWidgets (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        entferneWidgets bgWidgets
    entferneWidgets (ZugtypLego (GeschwindigkeitPwm bgWidgets)) = entferneWidgets bgWidgets
    entferneWidgets (ZugtypLego (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        entferneWidgets bgWidgets

    boxButtonEntfernen :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Gtk.Box
    boxButtonEntfernen (ZugtypMärklin (GeschwindigkeitPwm bgWidgets)) =
        boxButtonEntfernen bgWidgets
    boxButtonEntfernen (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        boxButtonEntfernen bgWidgets
    boxButtonEntfernen (ZugtypLego (GeschwindigkeitPwm bgWidgets)) = boxButtonEntfernen bgWidgets
    boxButtonEntfernen (ZugtypLego (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        boxButtonEntfernen bgWidgets

    tvarSprache :: ZugtypEither (GeschwindigkeitEither BGWidgets)
                -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (ZugtypMärklin (GeschwindigkeitPwm bgWidgets)) = tvarSprache bgWidgets
    tvarSprache (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        tvarSprache bgWidgets
    tvarSprache (ZugtypLego (GeschwindigkeitPwm bgWidgets)) = tvarSprache bgWidgets
    tvarSprache (ZugtypLego (GeschwindigkeitKonstanteSpannung bgWidgets)) = tvarSprache bgWidgets

    tvarEvent :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> TVar EventAusführen
    tvarEvent (ZugtypMärklin (GeschwindigkeitPwm bgWidgets)) = tvarEvent bgWidgets
    tvarEvent (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bgWidgets)) = tvarEvent bgWidgets
    tvarEvent (ZugtypLego (GeschwindigkeitPwm bgWidgets)) = tvarEvent bgWidgets
    tvarEvent (ZugtypLego (GeschwindigkeitKonstanteSpannung bgWidgets)) = tvarEvent bgWidgets

instance WegstreckenElement (ZugtypEither (GeschwindigkeitEither BGWidgets)) where
    getterWegstrecke
        :: Lens.Getter (ZugtypEither (GeschwindigkeitEither BGWidgets)) (CheckButtonWegstreckeHinzufügen Void (ZugtypEither (GeschwindigkeitEither BGWidgets)))
    getterWegstrecke = Lens.to erhalteCheckbuttonWegstrecke
        where
            erhalteCheckbuttonWegstrecke
                :: ZugtypEither (GeschwindigkeitEither BGWidgets)
                -> CheckButtonWegstreckeHinzufügen Void (ZugtypEither (GeschwindigkeitEither BGWidgets))
            erhalteCheckbuttonWegstrecke (ZugtypMärklin bg) =
                widgetHinzufügenZugtypEither $ bg ^. getterWegstrecke
            erhalteCheckbuttonWegstrecke (ZugtypLego bg) =
                widgetHinzufügenZugtypEither $ bg ^. getterWegstrecke

    boxWegstrecke
        :: ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)
        -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (ZugtypEither (GeschwindigkeitEither BGWidgets)))
    boxWegstrecke (ZugtypMärklin _bgWidgets) =
        Lens.to
        $ widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
    boxWegstrecke (ZugtypLego _bgWidgets) =
        Lens.to
        $ widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego

instance PlanElement (BGWidgets b 'Märklin) where
    foldPlan
        :: Lens.Fold (BGWidgets b 'Märklin) (Maybe (ButtonPlanHinzufügen (BGWidgets b 'Märklin)))
    foldPlan = Lens.folding $ map Just . erhalteButtonPlanHinzufügen
        where
            erhalteButtonPlanHinzufügen
                :: BGWidgets b 'Märklin -> [ButtonPlanHinzufügen (BGWidgets b 'Märklin)]
            erhalteButtonPlanHinzufügen
                BGWidgetsPwmMärklin {bgpmHinzPL = (buttonSpezifisch, buttonAllgemein)} =
                [buttonSpezifisch, widgetHinzufügenGeschwindigkeitVariante buttonAllgemein]
            erhalteButtonPlanHinzufügen
                BGWidgetsKonstanteSpannungMärklin
                {bgkmHinzPL = (buttonSpezifisch, buttonAllgemein)} =
                [buttonSpezifisch, widgetHinzufügenGeschwindigkeitVariante buttonAllgemein]

    boxenPlan :: Bahngeschwindigkeit b 'Märklin
              -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (BGWidgets b 'Märklin))
    boxenPlan MärklinBahngeschwindigkeitPwm {} =
        Lens.folding
        $ \DynamischeWidgets { vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
                             , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin}
        -> [ vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
           , widgetHinzufügenGeschwindigkeitVariante
                 vBoxHinzufügenPlanBahngeschwindigkeitenMärklin]
    boxenPlan MärklinBahngeschwindigkeitKonstanteSpannung {} =
        Lens.folding
        $ \DynamischeWidgets { vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
                             , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin}
        -> [ vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
           , widgetHinzufügenGeschwindigkeitVariante
                 vBoxHinzufügenPlanBahngeschwindigkeitenMärklin]

instance PlanElement (BGWidgets b 'Lego) where
    foldPlan :: Lens.Fold (BGWidgets b 'Lego) (Maybe (ButtonPlanHinzufügen (BGWidgets b 'Lego)))
    foldPlan = Lens.folding $ map Just . erhalteButtonPlanHinzufügen
        where
            erhalteButtonPlanHinzufügen
                :: BGWidgets b 'Lego -> [ButtonPlanHinzufügen (BGWidgets b 'Lego)]
            erhalteButtonPlanHinzufügen
                BGWidgetsPwmLego {bgplHinzPL = (buttonSpezifisch, buttonAllgemein)} =
                [buttonSpezifisch, widgetHinzufügenGeschwindigkeitVariante buttonAllgemein]
            erhalteButtonPlanHinzufügen
                BGWidgetsKonstanteSpannungLego {bgklHinzPL = (buttonSpezifisch, buttonAllgemein)} =
                [buttonSpezifisch, widgetHinzufügenGeschwindigkeitVariante buttonAllgemein]

    boxenPlan :: Bahngeschwindigkeit b 'Lego
              -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (BGWidgets b 'Lego))
    boxenPlan LegoBahngeschwindigkeit {} =
        Lens.folding
        $ \DynamischeWidgets { vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
                             , vBoxHinzufügenPlanBahngeschwindigkeitenLego}
        -> [ vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
           , widgetHinzufügenGeschwindigkeitVariante vBoxHinzufügenPlanBahngeschwindigkeitenLego]

instance PlanElement (ZugtypEither (GeschwindigkeitEither BGWidgets)) where
    foldPlan :: Lens.Fold (ZugtypEither (GeschwindigkeitEither BGWidgets)) (Maybe (ButtonPlanHinzufügen (ZugtypEither (GeschwindigkeitEither BGWidgets))))
    foldPlan = Lens.folding $ \bgWidgets -> Just <$> ausZugtypEither buttonList bgWidgets
        where
            buttonList :: (GeschwindigkeitEither BGWidgets) z
                       -> [ButtonPlanHinzufügen (ZugtypEither (GeschwindigkeitEither BGWidgets))]
            buttonList
                (GeschwindigkeitPwm
                     BGWidgetsPwmMärklin {bgpmHinzPL = (buttonSpezifisch, buttonAllgemein)}) =
                widgetHinzufügenZugtypEither
                <$> [widgetHinzufügenGeschwindigkeitEither buttonSpezifisch, buttonAllgemein]
            buttonList
                (GeschwindigkeitPwm
                     BGWidgetsPwmLego {bgplHinzPL = (buttonSpezifisch, buttonAllgemein)}) =
                widgetHinzufügenZugtypEither
                <$> [widgetHinzufügenGeschwindigkeitEither buttonSpezifisch, buttonAllgemein]
            buttonList
                (GeschwindigkeitKonstanteSpannung
                     BGWidgetsKonstanteSpannungMärklin
                     {bgkmHinzPL = (buttonSpezifisch, buttonAllgemein)}) =
                widgetHinzufügenZugtypEither
                <$> [widgetHinzufügenGeschwindigkeitEither buttonSpezifisch, buttonAllgemein]
            buttonList
                (GeschwindigkeitKonstanteSpannung
                     BGWidgetsKonstanteSpannungLego
                     {bgklHinzPL = (buttonSpezifisch, buttonAllgemein)}) =
                widgetHinzufügenZugtypEither
                <$> [widgetHinzufügenGeschwindigkeitEither buttonSpezifisch, buttonAllgemein]

    boxenPlan :: ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)
              -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither (GeschwindigkeitEither BGWidgets)))
    boxenPlan (ZugtypMärklin (GeschwindigkeitPwm _bahngeschwindigkeit)) =
        Lens.folding
        $ \DynamischeWidgets
        { vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
        , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin} -> widgetHinzufügenZugtypEither
        <$> [ widgetHinzufügenGeschwindigkeitEither
                  vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
            , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin]
    boxenPlan (ZugtypMärklin (GeschwindigkeitKonstanteSpannung _bahngeschwindigkeit)) =
        Lens.folding
        $ \DynamischeWidgets
        { vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
        , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin} -> widgetHinzufügenZugtypEither
        <$> [ widgetHinzufügenGeschwindigkeitEither
                  vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
            , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin]
    boxenPlan (ZugtypLego (GeschwindigkeitPwm _bahngeschwindigkeit)) =
        Lens.folding
        $ \DynamischeWidgets
        { vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
        , vBoxHinzufügenPlanBahngeschwindigkeitenLego} -> widgetHinzufügenZugtypEither
        <$> [ widgetHinzufügenGeschwindigkeitEither
                  vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
            , vBoxHinzufügenPlanBahngeschwindigkeitenLego]
    boxenPlan (ZugtypLego (GeschwindigkeitKonstanteSpannung _bahngeschwindigkeit)) =
        Lens.folding
        $ \DynamischeWidgets
        { vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
        , vBoxHinzufügenPlanBahngeschwindigkeitenLego} -> widgetHinzufügenZugtypEither
        <$> [ widgetHinzufügenGeschwindigkeitEither
                  vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
            , vBoxHinzufügenPlanBahngeschwindigkeitenLego]

instance StreckenObjekt (BGWidgets g z) where
    anschlüsse :: BGWidgets g z -> Set Anschluss
    anschlüsse BGWidgetsPwmMärklin {bgpm} = anschlüsse bgpm
    anschlüsse BGWidgetsKonstanteSpannungMärklin {bgkm} = anschlüsse bgkm
    anschlüsse BGWidgetsPwmLego {bgpl} = anschlüsse bgpl
    anschlüsse BGWidgetsKonstanteSpannungLego {bgkl} = anschlüsse bgkl

    erhalteName :: BGWidgets g z -> Text
    erhalteName BGWidgetsPwmMärklin {bgpm} = erhalteName bgpm
    erhalteName BGWidgetsKonstanteSpannungMärklin {bgkm} = erhalteName bgkm
    erhalteName BGWidgetsPwmLego {bgpl} = erhalteName bgpl
    erhalteName BGWidgetsKonstanteSpannungLego {bgkl} = erhalteName bgkl

instance (ZugtypKlasse z) => ObjektElement (BGWidgets g z) where
    zuObjekt :: BGWidgets g z -> Objekt
    zuObjekt BGWidgetsPwmMärklin {bgpm} = zuObjekt bgpm
    zuObjekt BGWidgetsKonstanteSpannungMärklin {bgkm} = zuObjekt bgkm
    zuObjekt BGWidgetsPwmLego {bgpl} = zuObjekt bgpl
    zuObjekt BGWidgetsKonstanteSpannungLego {bgkl} = zuObjekt bgkl

instance Aeson.ToJSON (BGWidgets g z) where
    toJSON :: BGWidgets g z -> Aeson.Value
    toJSON BGWidgetsPwmMärklin {bgpm} = Aeson.toJSON bgpm
    toJSON BGWidgetsKonstanteSpannungMärklin {bgkm} = Aeson.toJSON bgkm
    toJSON BGWidgetsPwmLego {bgpl} = Aeson.toJSON bgpl
    toJSON BGWidgetsKonstanteSpannungLego {bgkl} = Aeson.toJSON bgkl

instance BahngeschwindigkeitKlasse BGWidgets where
    geschwindigkeit
        :: (I2CReader r m, PwmReader r m, MonadIO m) => BGWidgets 'Pwm z -> Word8 -> m ()
    geschwindigkeit BGWidgetsPwmMärklin {bgpm, bgpmScaleGeschwindigkeit, bgpmTVarEvent} wert = do
        eventAusführen bgpmTVarEvent $ geschwindigkeit bgpm wert
        liftIO
            $ ohneEvent bgpmTVarEvent
            $ Gtk.set bgpmScaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
    geschwindigkeit BGWidgetsPwmLego {bgpl, bgplScaleGeschwindigkeit, bgplTVarEvent} wert = do
        eventAusführen bgplTVarEvent $ geschwindigkeit bgpl wert
        liftIO
            $ ohneEvent bgplTVarEvent
            $ Gtk.set bgplScaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]

    fahrstrom :: (I2CReader r m, MonadIO m) => BGWidgets 'KonstanteSpannung z -> Word8 -> m ()
    fahrstrom BGWidgetsKonstanteSpannungMärklin {bgkm, bgkmAuswahlFahrstrom, bgkmTVarEvent} wert =
        do
            eventAusführen bgkmTVarEvent $ fahrstrom bgkm wert
            liftIO $ ohneEvent bgkmTVarEvent $ setzeAuswahl bgkmAuswahlFahrstrom wert
    fahrstrom BGWidgetsKonstanteSpannungLego {bgkl, bgklAuswahlFahrstrom, bgklTVarEvent} wert = do
        eventAusführen bgklTVarEvent $ fahrstrom bgkl wert
        liftIO $ ohneEvent bgklTVarEvent $ setzeAuswahl bgklAuswahlFahrstrom wert

    umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m) => BGWidgets b 'Märklin -> m ()
    umdrehen
        BGWidgetsPwmMärklin {bgpm, bgpmTVarEvent} = eventAusführen bgpmTVarEvent $ umdrehen bgpm
    umdrehen BGWidgetsKonstanteSpannungMärklin {bgkm, bgkmTVarEvent} =
        eventAusführen bgkmTVarEvent $ umdrehen bgkm

    fahrtrichtungEinstellen
        :: (I2CReader r m, PwmReader r m, MonadIO m) => BGWidgets b 'Lego -> Fahrtrichtung -> m ()
    fahrtrichtungEinstellen BGWidgetsPwmLego {bgpl, bgplAuswahlFahrtrichtung, bgplTVarEvent} wert =
        do
            eventAusführen bgplTVarEvent $ fahrtrichtungEinstellen bgpl wert
            liftIO $ ohneEvent bgplTVarEvent $ setzeAuswahl bgplAuswahlFahrtrichtung wert
    fahrtrichtungEinstellen
        BGWidgetsKonstanteSpannungLego {bgkl, bgklAuswahlFahrtrichtung, bgklTVarEvent}
        wert = do
        eventAusführen bgklTVarEvent $ fahrtrichtungEinstellen bgkl wert
        liftIO $ ohneEvent bgklTVarEvent $ setzeAuswahl bgklAuswahlFahrtrichtung wert

-- | 'Bahngeschwindigkeit' darstellen und zum Status hinzufügen
bahngeschwindigkeitPackNew
    :: ( ObjektGuiReader m
       , MonadIO m
       , ZugtypKlasse z
       , GeschwindigkeitEitherKlasse g
       , WegstreckenElement (BGWidgets g z)
       , PlanElement (BGWidgets g z)
       )
    => Bahngeschwindigkeit g z
    -> MStatusGuiT m (BGWidgets g z)
bahngeschwindigkeitPackNew bahngeschwindigkeit = do
    DynamischeWidgets {vBoxBahngeschwindigkeiten} <- erhalteDynamischeWidgets
    (tvarSprache, tvarEvent) <- liftIO $ do
        tvarSprache <- newTVarIO $ Just []
        tvarEvent <- newTVarIO EventAusführen
        pure (tvarSprache, tvarEvent)
    let justTVarSprache = Just tvarSprache
    -- Widget erstellen
    vBox <- liftIO $ boxPackWidgetNewDefault vBoxBahngeschwindigkeiten $ Gtk.vBoxNew False 0
    namePackNew vBox bahngeschwindigkeit
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    bgWidgets <- geschwindigkeitsWidgetsPackNew
        vBox
        bahngeschwindigkeit
        vBoxAnschlüsse
        tvarSprache
        tvarEvent
    fließendPackNew vBoxAnschlüsse bahngeschwindigkeit justTVarSprache
    buttonEntfernenPackNew bgWidgets
        $ entfernenBahngeschwindigkeit
        $ zuZugtypEither
        $ zuGeschwindigkeitEither bgWidgets
    -- Widgets merken
    ausführenBefehl
        $ Hinzufügen
        $ OBahngeschwindigkeit
        $ zuZugtypEither
        $ zuGeschwindigkeitEither bgWidgets
    pure bgWidgets
    where
        hinzufügenWidgetsPackNew
            :: ( ObjektGuiReader m
               , MonadIO m
               , WegstreckenElement (BGWidgets g z)
               , PlanElement (BGWidgets g z)
               , ZugtypKlasse z
               )
            => Bahngeschwindigkeit g z
            -> TVar (Maybe [Sprache -> IO ()])
            -> m ( CheckButtonWegstreckeHinzufügen Void (BGWidgets g z)
                 , ButtonPlanHinzufügen (BGWidgets g z)
                 , ButtonPlanHinzufügen (GeschwindigkeitEither BGWidgets z)
                 )
        hinzufügenWidgetsPackNew bahngeschwindigkeit tvarSprache = do
            dynamischeWidgets <- erhalteDynamischeWidgets
            hinzufügenWidgetWegstrecke
                <- hinzufügenWidgetWegstreckePackNew bahngeschwindigkeit tvarSprache
            hinzufügenWidgetPlanSpezifisch <- hinzufügenWidgetPlanPackNew
                (fromJust $ Lens.firstOf (boxenPlan bahngeschwindigkeit) dynamischeWidgets)
                bahngeschwindigkeit
                tvarSprache
            hinzufügenWidgetPlanAllgemein <- widgetHinzufügenGeschwindigkeitEither
                <$> hinzufügenWidgetPlanPackNew
                    (fromJust $ Lens.lastOf (boxenPlan bahngeschwindigkeit) dynamischeWidgets)
                    bahngeschwindigkeit
                    tvarSprache
            pure
                ( hinzufügenWidgetWegstrecke
                , hinzufügenWidgetPlanSpezifisch
                , hinzufügenWidgetPlanAllgemein
                )

        geschwindigkeitsWidgetsPackNew
            :: (ObjektGuiReader m, MonadIO m)
            => Gtk.VBox
            -> Bahngeschwindigkeit g z
            -> ScrollbaresWidget Gtk.VBox
            -> TVar (Maybe [Sprache -> IO ()])
            -> TVar EventAusführen
            -> m (BGWidgets g z)
        geschwindigkeitsWidgetsPackNew
            box
            bahngeschwindigkeit@MärklinBahngeschwindigkeitPwm {bgmpGeschwindigkeitsPin}
            vBoxAnschlüsse
            bgpmTVarSprache
            bgpmTVarEvent = do
            boxPackWidgetNewDefault vBoxAnschlüsse
                $ pinNew (Just bgpmTVarSprache) Language.geschwindigkeit bgmpGeschwindigkeitsPin
            bgpmFunctionBox <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
            bgpmScaleGeschwindigkeit
                <- hScaleGeschwindigkeitPackNew bgpmFunctionBox bahngeschwindigkeit bgpmTVarEvent
            buttonUmdrehenPackNew bgpmFunctionBox bahngeschwindigkeit bgpmTVarSprache bgpmTVarEvent
            -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
            (bgpmHinzWS, hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                <- hinzufügenWidgetsPackNew bahngeschwindigkeit bgpmTVarSprache
            pure
                BGWidgetsPwmMärklin
                { bgpm = bahngeschwindigkeit
                , bgpmWidget = box
                , bgpmFunctionBox
                , bgpmHinzWS
                , bgpmHinzPL = (hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                , bgpmTVarSprache
                , bgpmTVarEvent
                , bgpmScaleGeschwindigkeit
                }
        geschwindigkeitsWidgetsPackNew
            box
            bahngeschwindigkeit@MärklinBahngeschwindigkeitKonstanteSpannung
            {bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss}
            vBoxAnschlüsse
            bgkmTVarSprache
            bgkmTVarEvent = do
            let justTVarSprache = Just bgkmTVarSprache
            let erstelleFahrstromAnschlussWidget
                    :: (MonadIO m, SpracheGuiReader r m) => Natural -> Anschluss -> m Natural
                erstelleFahrstromAnschlussWidget i anschluss = do
                    boxPackWidgetNewDefault vBoxAnschlüsse
                        $ anschlussNew justTVarSprache (Language.fahrstrom <> anzeige i) anschluss
                    pure $ succ i
            foldM erstelleFahrstromAnschlussWidget 1 bgmkFahrstromAnschlüsse
            bgkmFunctionBox <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
            bgkmAuswahlFahrstrom <- auswahlFahrstromPackNew
                bgkmFunctionBox
                bahngeschwindigkeit
                (fromIntegral $ length bgmkFahrstromAnschlüsse)
                bgkmTVarSprache
                bgkmTVarEvent
            boxPackWidgetNewDefault vBoxAnschlüsse
                $ anschlussNew justTVarSprache Language.umdrehen bgmkUmdrehenAnschluss
            buttonUmdrehenPackNew bgkmFunctionBox bahngeschwindigkeit bgkmTVarSprache bgkmTVarEvent
            -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
            (bgkmHinzWS, hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                <- hinzufügenWidgetsPackNew bahngeschwindigkeit bgkmTVarSprache
            pure
                BGWidgetsKonstanteSpannungMärklin
                { bgkm = bahngeschwindigkeit
                , bgkmWidget = box
                , bgkmFunctionBox
                , bgkmHinzWS
                , bgkmHinzPL = (hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                , bgkmTVarSprache
                , bgkmTVarEvent
                , bgkmAuswahlFahrstrom
                }
        geschwindigkeitsWidgetsPackNew
            box
            bahngeschwindigkeit@LegoBahngeschwindigkeit
            {bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss}
            vBoxAnschlüsse
            bgplTVarSprache
            bgplTVarEvent = do
            let justTVarSprache = Just bgplTVarSprache
            boxPackWidgetNewDefault vBoxAnschlüsse
                $ pinNew justTVarSprache Language.geschwindigkeit bglGeschwindigkeitsPin
            bgplFunctionBox <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
            bgplScaleGeschwindigkeit
                <- hScaleGeschwindigkeitPackNew bgplFunctionBox bahngeschwindigkeit bgplTVarEvent
            boxPackWidgetNewDefault vBoxAnschlüsse
                $ anschlussNew justTVarSprache Language.fahrtrichtung bglFahrtrichtungsAnschluss
            bgplAuswahlFahrtrichtung <- auswahlFahrtrichtungEinstellenPackNew
                bgplFunctionBox
                bahngeschwindigkeit
                bgplTVarSprache
                bgplTVarEvent
            -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
            (bgplHinzWS, hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                <- hinzufügenWidgetsPackNew bahngeschwindigkeit bgplTVarSprache
            pure
                BGWidgetsPwmLego
                { bgpl = bahngeschwindigkeit
                , bgplWidget = box
                , bgplFunctionBox
                , bgplHinzWS
                , bgplHinzPL = (hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                , bgplTVarSprache
                , bgplTVarEvent
                , bgplScaleGeschwindigkeit
                , bgplAuswahlFahrtrichtung
                }

class BGWidgetsKlasse (bg :: GeschwindigkeitVariante -> Zugtyp -> Type) where
    enthalteneBahngeschwindigkeiten :: (GeschwindigkeitEitherKlasse g)
                                    => bg g z
                                    -> Set (GeschwindigkeitEither Bahngeschwindigkeit z)

instance BGWidgetsKlasse Bahngeschwindigkeit where
    enthalteneBahngeschwindigkeiten :: (GeschwindigkeitEitherKlasse g)
                                    => Bahngeschwindigkeit g z
                                    -> Set (GeschwindigkeitEither Bahngeschwindigkeit z)
    enthalteneBahngeschwindigkeiten = Set.singleton . zuGeschwindigkeitEither

instance BGWidgetsKlasse BGWidgets where
    enthalteneBahngeschwindigkeiten :: (GeschwindigkeitEitherKlasse g)
                                    => BGWidgets g z
                                    -> Set (GeschwindigkeitEither Bahngeschwindigkeit z)
    enthalteneBahngeschwindigkeiten
        BGWidgetsPwmMärklin {bgpm} = Set.singleton $ GeschwindigkeitPwm bgpm
    enthalteneBahngeschwindigkeiten BGWidgetsKonstanteSpannungMärklin {bgkm} =
        Set.singleton $ GeschwindigkeitKonstanteSpannung bgkm
    enthalteneBahngeschwindigkeiten
        BGWidgetsPwmLego {bgpl} = Set.singleton $ GeschwindigkeitPwm bgpl
    enthalteneBahngeschwindigkeiten BGWidgetsKonstanteSpannungLego {bgkl} =
        Set.singleton $ GeschwindigkeitKonstanteSpannung bgkl

-- | Füge 'Scale' zum einstellen der Geschwindigkeit zur Box hinzu
hScaleGeschwindigkeitPackNew
    :: forall b bg m z.
    ( MitBox b
    , BahngeschwindigkeitKlasse bg
    , BGWidgetsKlasse bg
    , ObjektGuiReader m
    , MonadIO m
    , ZugtypKlasse z
    )
    => b
    -> bg 'Pwm z
    -> TVar EventAusführen
    -> m Gtk.HScale
hScaleGeschwindigkeitPackNew box bahngeschwindigkeit tvarEventAusführen = do
    statusVar <- erhalteStatusVar :: m StatusVarGui
    objektReader <- ask
    liftIO $ do
        scale <- boxPackWidgetNew box PackGrow paddingDefault positionDefault
            $ widgetShowNew
            $ Gtk.hScaleNewWithRange 0 (fromIntegral (maxBound :: Word8)) 1
        Gtk.widgetSetSizeRequest scale 100 (-1)
        Gtk.on scale Gtk.valueChanged $ eventAusführen tvarEventAusführen $ do
            wert <- floor <$> Gtk.get scale Gtk.rangeValue
            flip runReaderT objektReader $ flip auswertenStatusVarMStatusT statusVar $ do
                ausführenAktion $ Geschwindigkeit bahngeschwindigkeit wert
                -- Widgets synchronisieren
                bahngeschwindigkeiten <- getBahngeschwindigkeiten
                liftIO $ forM_ bahngeschwindigkeiten $ flip bgWidgetsSynchronisieren wert
                wegstrecken <- getWegstrecken
                liftIO $ forM_ wegstrecken $ flip wsWidgetsSynchronisieren wert
        pure scale
    where
        bgWidgetsSynchronisieren
            :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Word8 -> IO ()
        bgWidgetsSynchronisieren
            (ZugtypMärklin
                 (GeschwindigkeitPwm
                      BGWidgetsPwmMärklin {bgpm, bgpmTVarEvent, bgpmScaleGeschwindigkeit}))
            wert
            | elem (ZugtypMärklin $ GeschwindigkeitPwm bgpm)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgpmTVarEvent
                $ Gtk.set bgpmScaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitPwm
                      BGWidgetsPwmLego {bgpl, bgplTVarEvent, bgplScaleGeschwindigkeit}))
            wert
            | elem (ZugtypLego $ GeschwindigkeitPwm bgpl)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgplTVarEvent
                $ Gtk.set bgplScaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
        bgWidgetsSynchronisieren _bgWidgets _wert = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither WSWidgets -> Word8 -> IO ()
        wsWidgetsSynchronisieren
            (ZugtypMärklin
                 WSWidgets { ws = Wegstrecke {wsBahngeschwindigkeiten}
                           , wsTVarEvent
                           , wsScaleGeschwindigkeit = Just scaleGeschwindigkeit})
            wert
            | Set.isSubsetOf (Set.map zuZugtypEither wsBahngeschwindigkeiten)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent wsTVarEvent
                $ Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
        wsWidgetsSynchronisieren
            (ZugtypLego
                 WSWidgets { ws = Wegstrecke {wsBahngeschwindigkeiten}
                           , wsTVarEvent
                           , wsScaleGeschwindigkeit = Just scaleGeschwindigkeit})
            wert
            | Set.isSubsetOf (Set.map zuZugtypEither wsBahngeschwindigkeiten)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent wsTVarEvent
                $ Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
        wsWidgetsSynchronisieren _wsWidget _wert = pure ()

-- | Füge 'AuswahlWidget' zum einstellen des Fahrstroms zur Box hinzu
auswahlFahrstromPackNew
    :: forall b bg m z.
    ( MitBox b
    , BahngeschwindigkeitKlasse bg
    , BGWidgetsKlasse bg
    , ObjektGuiReader m
    , MonadIO m
    , ZugtypKlasse z
    )
    => b
    -> bg 'KonstanteSpannung z
    -> Word8
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> m (AuswahlWidget Word8)
auswahlFahrstromPackNew box bahngeschwindigkeit maxWert tvarSprachwechsel tvarEventAusführen = do
    statusVar <- erhalteStatusVar :: m StatusVarGui
    objektReader <- ask
    auswahlWidget <- boxPackWidgetNewDefault box
        $ widgetShowNew
        $ (if maxWert < 5
               then auswahlRadioButtonNew
               else auswahlComboBoxNew)
            (NonEmpty.fromList $ [maxWert, pred maxWert .. 0])
            (Just tvarSprachwechsel)
            Language.fahrstrom
    setzeAuswahl auswahlWidget 0
    beiAuswahl auswahlWidget $ \wert -> eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ flip auswertenStatusVarMStatusT statusVar
        $ do
            ausführenAktion $ Fahrstrom bahngeschwindigkeit wert
            -- Widgets synchronisieren
            bahngeschwindigkeiten <- getBahngeschwindigkeiten
            liftIO $ forM_ bahngeschwindigkeiten $ flip bgWidgetsSynchronisieren wert
            wegstrecken <- getWegstrecken
            liftIO $ forM_ wegstrecken $ flip wsWidgetsSynchronisieren wert
    pure auswahlWidget
    where
        bgWidgetsSynchronisieren
            :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Word8 -> IO ()
        bgWidgetsSynchronisieren
            (ZugtypMärklin
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgetsKonstanteSpannungMärklin
                      {bgkm, bgkmTVarEvent, bgkmAuswahlFahrstrom}))
            wert
            | elem (ZugtypMärklin $ GeschwindigkeitKonstanteSpannung bgkm)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgkmTVarEvent $ setzeAuswahl bgkmAuswahlFahrstrom wert
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgetsKonstanteSpannungLego {bgkl, bgklTVarEvent, bgklAuswahlFahrstrom}))
            wert
            | elem (ZugtypLego $ GeschwindigkeitKonstanteSpannung bgkl)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgklTVarEvent $ setzeAuswahl bgklAuswahlFahrstrom wert
        bgWidgetsSynchronisieren _bgWidgets _wert = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither WSWidgets -> Word8 -> IO ()
        wsWidgetsSynchronisieren
            (ZugtypMärklin
                 WSWidgets { ws = Wegstrecke {wsBahngeschwindigkeiten}
                           , wsTVarEvent
                           , wsAuswahlFahrstrom = Just auswahlFahrstrom})
            wert
            | Set.isSubsetOf (Set.map zuZugtypEither wsBahngeschwindigkeiten)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent wsTVarEvent $ setzeAuswahl auswahlFahrstrom wert
        wsWidgetsSynchronisieren
            (ZugtypLego
                 WSWidgets { ws = Wegstrecke {wsBahngeschwindigkeiten}
                           , wsTVarEvent
                           , wsAuswahlFahrstrom = Just auswahlFahrstrom})
            wert
            | Set.isSubsetOf (Set.map zuZugtypEither wsBahngeschwindigkeiten)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent wsTVarEvent $ setzeAuswahl auswahlFahrstrom wert
        wsWidgetsSynchronisieren _wsWidget _wert = pure ()

-- | Füge 'Gtk.Button' zum 'umdrehen' zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonUmdrehenPackNew
    :: forall b bg g m.
    ( MitBox b
    , BahngeschwindigkeitKlasse bg
    , BGWidgetsKlasse bg
    , STWidgetsKlasse (bg g 'Märklin)
    , ObjektGuiReader m
    , MonadIO m
    , GeschwindigkeitEitherKlasse g
    )
    => b
    -> bg g 'Märklin
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> m Gtk.Button
buttonUmdrehenPackNew box bahngeschwindigkeit tvarSprachwechsel tvarEventAusführen = do
    statusVar <- erhalteStatusVar :: m StatusVarGui
    objektReader <- ask
    boxPackWidgetNewDefault box
        $ buttonNewWithEventLabel (Just tvarSprachwechsel) Language.umdrehen
        $ eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ flip auswertenStatusVarMStatusT statusVar
        $ do
            ausführenAktion $ Umdrehen bahngeschwindigkeit
            -- Widgets synchronisieren
            bahngeschwindigkeiten <- getBahngeschwindigkeiten
            liftIO $ forM_ bahngeschwindigkeiten $ bgWidgetsSynchronisieren
            streckenabschnitte <- getStreckenabschnitte
            liftIO $ forM_ streckenabschnitte $ stWidgetsSynchronisieren
            wegstrecken <- getWegstrecken
            liftIO $ forM_ wegstrecken $ wsWidgetsSynchronisieren
    where
        bgWidgetsSynchronisieren :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> IO ()
        bgWidgetsSynchronisieren
            (ZugtypMärklin
                 (GeschwindigkeitPwm
                      BGWidgetsPwmMärklin {bgpm, bgpmTVarEvent, bgpmScaleGeschwindigkeit}))
            | elem (ZugtypMärklin $ GeschwindigkeitPwm bgpm)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgpmTVarEvent $ Gtk.set bgpmScaleGeschwindigkeit [Gtk.rangeValue := 0]
        bgWidgetsSynchronisieren
            (ZugtypMärklin
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgetsKonstanteSpannungMärklin
                      {bgkm, bgkmTVarEvent, bgkmAuswahlFahrstrom}))
            | elem (ZugtypMärklin $ GeschwindigkeitKonstanteSpannung bgkm)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgkmTVarEvent $ setzeAuswahl bgkmAuswahlFahrstrom 0
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitPwm
                      BGWidgetsPwmLego {bgpl, bgplTVarEvent, bgplScaleGeschwindigkeit}))
            | elem (ZugtypLego $ GeschwindigkeitPwm bgpl)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgplTVarEvent $ Gtk.set bgplScaleGeschwindigkeit [Gtk.rangeValue := 0]
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgetsKonstanteSpannungLego {bgkl, bgklTVarEvent, bgklAuswahlFahrstrom}))
            | elem (ZugtypLego $ GeschwindigkeitKonstanteSpannung bgkl)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgklTVarEvent $ setzeAuswahl bgklAuswahlFahrstrom 0
        bgWidgetsSynchronisieren _bgWidgets = pure ()

        stWidgetsSynchronisieren :: STWidgets -> IO ()
        stWidgetsSynchronisieren STWidgets {st, stTVarEvent, stTogglebuttonStrom}
            | elem st $ enthalteneStreckenabschnitte bahngeschwindigkeit =
                ohneEvent stTVarEvent
                $ Gtk.set stTogglebuttonStrom [Gtk.toggleButtonActive := True]
        stWidgetsSynchronisieren _stWidgets = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither WSWidgets -> IO ()
        wsWidgetsSynchronisieren (ZugtypMärklin wsWidgets) = wsWidgetsSynchronisierenAux wsWidgets
        wsWidgetsSynchronisieren (ZugtypLego wsWidgets) = wsWidgetsSynchronisierenAux wsWidgets

        wsWidgetsSynchronisierenAux :: (ZugtypKlasse z) => WSWidgets z -> IO ()
        wsWidgetsSynchronisierenAux
            WSWidgets { ws = Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte}
                      , wsTVarEvent
                      , wsToggleButtonStrom
                      , wsScaleGeschwindigkeit
                      , wsAuswahlFahrstrom} = do
            case wsToggleButtonStrom of
                (Just toggleButtonStrom)
                    | Set.isSubsetOf wsStreckenabschnitte
                        $ enthalteneStreckenabschnitte bahngeschwindigkeit -> ohneEvent wsTVarEvent
                        $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := True]
                _otherwise -> pure ()
            let istBahngeschwindigkeitTeilmenge =
                    Set.isSubsetOf (Set.map zuZugtypEither wsBahngeschwindigkeiten)
                    $ Set.map zuZugtypEither
                    $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit
            case wsScaleGeschwindigkeit of
                (Just scaleGeschwindigkeit)
                    | istBahngeschwindigkeitTeilmenge -> ohneEvent wsTVarEvent
                        $ Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := 0]
                _otherwise -> pure ()
            case wsAuswahlFahrstrom of
                (Just auswahlFahrstrom)
                    | istBahngeschwindigkeitTeilmenge
                        -> ohneEvent wsTVarEvent $ setzeAuswahl auswahlFahrstrom 0
                _otherwise -> pure ()

-- | Füge 'AuswahlWidget' zum Fahrtrichtung einstellen zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlFahrtrichtungEinstellenPackNew
    :: forall b bg g m.
    ( MitBox b
    , BahngeschwindigkeitKlasse bg
    , BGWidgetsKlasse bg
    , ObjektGuiReader m
    , MonadIO m
    , GeschwindigkeitEitherKlasse g
    )
    => b
    -> bg g 'Lego
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> m (AuswahlWidget Fahrtrichtung)
auswahlFahrtrichtungEinstellenPackNew box bahngeschwindigkeit tvarSprachwechsel tvarEventAusführen =
    do
        statusVar <- erhalteStatusVar :: m StatusVarGui
        objektReader <- ask
        auswahlFahrtrichtung <- boxPackWidgetNewDefault box
            $ boundedEnumAuswahlRadioButtonNew
                Vorwärts
                (Just tvarSprachwechsel)
                Language.fahrtrichtung
        beiAuswahl auswahlFahrtrichtung $ \fahrtrichtung -> eventAusführen tvarEventAusführen $ do
            flip runReaderT objektReader $ flip auswertenStatusVarMStatusT statusVar $ do
                ausführenAktion $ FahrtrichtungEinstellen bahngeschwindigkeit fahrtrichtung
                -- Widgets synchronisieren
                bahngeschwindigkeiten <- getBahngeschwindigkeiten
                liftIO $ forM_ bahngeschwindigkeiten $ flip bgWidgetsSynchronisieren fahrtrichtung
                wegstrecken <- getWegstrecken
                liftIO $ forM_ wegstrecken $ flip wsWidgetsSynchronisieren fahrtrichtung
        pure auswahlFahrtrichtung
    where
        bgWidgetsSynchronisieren
            :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Fahrtrichtung -> IO ()
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitPwm
                      BGWidgetsPwmLego
                      {bgpl, bgplTVarEvent, bgplScaleGeschwindigkeit, bgplAuswahlFahrtrichtung}))
            fahrtrichtung
            | elem (ZugtypLego $ GeschwindigkeitPwm bgpl)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgplTVarEvent $ do
                    Gtk.set bgplScaleGeschwindigkeit [Gtk.rangeValue := 0]
                    setzeAuswahl bgplAuswahlFahrtrichtung fahrtrichtung
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgetsKonstanteSpannungLego
                      {bgkl, bgklTVarEvent, bgklAuswahlFahrstrom, bgklAuswahlFahrtrichtung}))
            fahrtrichtung
            | elem (ZugtypLego $ GeschwindigkeitKonstanteSpannung bgkl)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgklTVarEvent $ do
                    setzeAuswahl bgklAuswahlFahrstrom 0
                    setzeAuswahl bgklAuswahlFahrtrichtung fahrtrichtung
        bgWidgetsSynchronisieren _bgWidgets _wert = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither WSWidgets -> Fahrtrichtung -> IO ()
        wsWidgetsSynchronisieren
            (ZugtypLego
                 WSWidgets { ws = Wegstrecke {wsBahngeschwindigkeiten}
                           , wsTVarEvent
                           , wsScaleGeschwindigkeit
                           , wsAuswahlFahrstrom
                           , wsAuswahlFahrtrichtung})
            fahrtrichtung
            | Set.isSubsetOf (Set.map zuZugtypEither wsBahngeschwindigkeiten)
                $ Set.map zuZugtypEither
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent wsTVarEvent $ do
                    case wsScaleGeschwindigkeit of
                        (Just scaleGeschwindigkeit)
                            -> Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := 0]
                        Nothing -> pure ()
                    case wsAuswahlFahrstrom of
                        (Just auswahlFahrstrom) -> setzeAuswahl auswahlFahrstrom 0
                        Nothing -> pure ()
                    case wsAuswahlFahrtrichtung of
                        (Just auswahlFahrtrichtung)
                            -> setzeAuswahl auswahlFahrtrichtung fahrtrichtung
                        Nothing -> pure ()
        wsWidgetsSynchronisieren _wsWidget _wert = pure ()

-- ** Streckenabschnitt
-- | 'Streckenabschnitt' mit zugehörigen Widgets
data STWidgets =
    STWidgets
    { st :: Streckenabschnitt
    , stWidget :: Gtk.VBox
    , stFunctionBox :: Gtk.HBox
    , stHinzWS :: CheckButtonWegstreckeHinzufügen Void STWidgets
    , stHinzPL :: ButtonPlanHinzufügen STWidgets
    , stTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , stTVarEvent :: TVar EventAusführen
    , stTogglebuttonStrom :: Gtk.ToggleButton
    }
    deriving (Eq)

instance MitWidget STWidgets where
    erhalteWidget :: STWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . stWidget

instance WidgetsTyp STWidgets where
    type ObjektTyp STWidgets = Streckenabschnitt

    erhalteObjektTyp :: STWidgets -> Streckenabschnitt
    erhalteObjektTyp = st

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => STWidgets -> m ()
    entferneWidgets stWidgets@STWidgets {stTVarSprache} = do
        DynamischeWidgets {vBoxStreckenabschnitte} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxStreckenabschnitte stWidgets
        entferneHinzufügenWegstreckeWidgets stWidgets
        entferneHinzufügenPlanWidgets stWidgets
        liftIO $ atomically $ writeTVar stTVarSprache Nothing

    boxButtonEntfernen :: STWidgets -> Gtk.Box
    boxButtonEntfernen = erhalteBox . stFunctionBox

    tvarSprache :: STWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = stTVarSprache

    tvarEvent :: STWidgets -> TVar EventAusführen
    tvarEvent = stTVarEvent

instance WegstreckenElement STWidgets where
    getterWegstrecke :: Lens.Getter STWidgets (CheckButtonWegstreckeHinzufügen Void STWidgets)
    getterWegstrecke = Lens.to stHinzWS

    boxWegstrecke :: Streckenabschnitt
                  -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen STWidgets)
    boxWegstrecke _stWidgets = Lens.to vBoxHinzufügenWegstreckeStreckenabschnitte

instance PlanElement STWidgets where
    foldPlan :: Lens.Fold STWidgets (Maybe (ButtonPlanHinzufügen STWidgets))
    foldPlan = Lens.folding $ (: []) . Just . stHinzPL

    boxenPlan :: Streckenabschnitt -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen STWidgets)
    boxenPlan _stWidgets = Lens.to vBoxHinzufügenPlanStreckenabschnitte

instance StreckenObjekt STWidgets where
    anschlüsse :: STWidgets -> Set Anschluss
    anschlüsse STWidgets {st} = anschlüsse st

    erhalteName :: STWidgets -> Text
    erhalteName STWidgets {st} = erhalteName st

instance Aeson.ToJSON STWidgets where
    toJSON :: STWidgets -> Aeson.Value
    toJSON (STWidgets {st}) = Aeson.toJSON st

instance StreckenabschnittKlasse STWidgets where
    strom :: (I2CReader r m, MonadIO m) => STWidgets -> Strom -> m ()
    strom STWidgets {st, stTogglebuttonStrom, stTVarEvent} wert = do
        eventAusführen stTVarEvent $ strom st wert
        liftIO
            $ ohneEvent stTVarEvent
            $ Gtk.set stTogglebuttonStrom [Gtk.toggleButtonActive := (wert == Fließend)]

-- | 'Streckenabschnitt' darstellen und zum Status hinzufügen
streckenabschnittPackNew
    :: forall m. (ObjektGuiReader m, MonadIO m) => Streckenabschnitt -> MStatusGuiT m STWidgets
streckenabschnittPackNew streckenabschnitt@Streckenabschnitt {stromAnschluss} = do
    DynamischeWidgets
        {vBoxStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte} <- erhalteDynamischeWidgets
    (stTVarSprache, stTVarEvent) <- liftIO $ do
        stTVarSprache <- newTVarIO $ Just []
        stTVarEvent <- newTVarIO EventAusführen
        pure (stTVarSprache, stTVarEvent)
    let justTVarSprache = Just stTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget
        <- hinzufügenWidgetWegstreckePackNew streckenabschnitt stTVarSprache
    hinzufügenPlanWidget <- hinzufügenWidgetPlanPackNew
        vBoxHinzufügenPlanStreckenabschnitte
        streckenabschnitt
        stTVarSprache
    -- Widget erstellen
    vBox <- boxPackWidgetNewDefault vBoxStreckenabschnitte $ liftIO $ Gtk.vBoxNew False 0
    namePackNew vBox streckenabschnitt
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    boxPackWidgetNewDefault vBoxAnschlüsse
        $ anschlussNew justTVarSprache Language.strom stromAnschluss
    stFunctionBox <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
    stTogglebuttonStrom
        <- toggleButtonStromPackNew stFunctionBox streckenabschnitt stTVarSprache stTVarEvent
    fließendPackNew vBoxAnschlüsse streckenabschnitt justTVarSprache
    let stWidgets =
            STWidgets
            { st = streckenabschnitt
            , stWidget = vBox
            , stFunctionBox
            , stHinzPL = hinzufügenPlanWidget
            , stHinzWS = hinzufügenWegstreckeWidget
            , stTVarSprache
            , stTVarEvent
            , stTogglebuttonStrom
            }
    buttonEntfernenPackNew stWidgets $ entfernenStreckenabschnitt stWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OStreckenabschnitt stWidgets
    pure stWidgets

class STWidgetsKlasse st where
    enthalteneStreckenabschnitte :: st -> Set Streckenabschnitt

instance STWidgetsKlasse Streckenabschnitt where
    enthalteneStreckenabschnitte :: Streckenabschnitt -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = Set.singleton

instance STWidgetsKlasse STWidgets where
    enthalteneStreckenabschnitte :: STWidgets -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = Set.singleton . st

instance STWidgetsKlasse (Bahngeschwindigkeit g z) where
    enthalteneStreckenabschnitte :: Bahngeschwindigkeit g z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = const Set.empty

-- | Füge 'Gtk.ToggleButton' zum einstellen des Stroms zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
toggleButtonStromPackNew
    :: forall m b s.
    (ObjektGuiReader m, MonadIO m, MitBox b, StreckenabschnittKlasse s, STWidgetsKlasse s)
    => b
    -> s
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> m Gtk.ToggleButton
toggleButtonStromPackNew box streckenabschnitt tvarSprachwechsel tvarEventAusführen = do
    statusVar <- erhalteStatusVar :: m StatusVarGui
    objektReader <- ask
    boxPackWidgetNewDefault box
        $ toggleButtonNewWithEventLabel (Just tvarSprachwechsel) Language.strom
        $ \an -> eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ flip auswertenStatusVarMStatusT statusVar
        $ do
            let fließend =
                    if an
                        then Fließend
                        else Gesperrt
            ausführenAktion $ Strom streckenabschnitt fließend
            -- Widgets synchronisieren
            streckenabschnitte <- getStreckenabschnitte
            liftIO $ forM_ streckenabschnitte $ flip stWidgetsSynchronisieren fließend
            wegstrecken <- getWegstrecken
            liftIO $ forM_ wegstrecken $ flip wsWidgetsSynchronisieren fließend
    where
        stWidgetsSynchronisieren :: STWidgets -> Strom -> IO ()
        stWidgetsSynchronisieren STWidgets {st, stTVarEvent, stTogglebuttonStrom} fließend
            | elem st $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent stTVarEvent
                $ Gtk.set stTogglebuttonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        stWidgetsSynchronisieren _stWidgets _fließend = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither WSWidgets -> Strom -> IO ()
        wsWidgetsSynchronisieren
            (ZugtypMärklin
                 WSWidgets { ws = Wegstrecke {wsStreckenabschnitte}
                           , wsTVarEvent
                           , wsToggleButtonStrom = Just toggleButtonStrom})
            fließend
            | Set.isSubsetOf wsStreckenabschnitte $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent wsTVarEvent
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        wsWidgetsSynchronisieren
            (ZugtypLego
                 WSWidgets { ws = Wegstrecke {wsStreckenabschnitte}
                           , wsTVarEvent
                           , wsToggleButtonStrom = Just toggleButtonStrom})
            fließend
            | Set.isSubsetOf wsStreckenabschnitte $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent wsTVarEvent
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        wsWidgetsSynchronisieren _wsWidget _fließend = pure ()

-- ** Weiche
-- | 'Weiche' mit zugehörigen Widgets
data WEWidgets (z :: Zugtyp) =
    WEWidgets
    { we :: Weiche z
    , weWidget :: Gtk.VBox
    , weFunctionBox :: Gtk.HBox
    , weHinzWS :: CheckButtonWegstreckeHinzufügen Richtung (WEWidgets z)
    , weHinzPL :: WeichePlanHinzufügenWidgets z
    , weTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , weTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

-- | Widgets zum Hinzufügen einer 'Weiche' zu einem 'Plan'
data WeichePlanHinzufügenWidgets z =
    WeichePlanHinzufügenWidgets
    { gerade :: Maybe (ButtonPlanHinzufügen (WEWidgets z))
    , kurve :: Maybe (ButtonPlanHinzufügen (WEWidgets z))
    , links :: Maybe (ButtonPlanHinzufügen (WEWidgets z))
    , rechts :: Maybe (ButtonPlanHinzufügen (WEWidgets z))
    }
    deriving (Eq)

instance MitWidget (WEWidgets z) where
    erhalteWidget :: WEWidgets z -> Gtk.Widget
    erhalteWidget = erhalteWidget . weWidget

instance (WegstreckenElement (WEWidgets z), PlanElement (WEWidgets z))
    => WidgetsTyp (WEWidgets z) where
    type ObjektTyp (WEWidgets z) = Weiche z

    erhalteObjektTyp :: WEWidgets z -> Weiche z
    erhalteObjektTyp = we

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => WEWidgets z -> m ()
    entferneWidgets weWidgets@WEWidgets {weTVarSprache} = do
        DynamischeWidgets {vBoxWeichen} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxWeichen weWidgets
        entferneHinzufügenWegstreckeWidgets weWidgets
        entferneHinzufügenPlanWidgets weWidgets
        liftIO $ atomically $ writeTVar weTVarSprache Nothing

    boxButtonEntfernen :: WEWidgets z -> Gtk.Box
    boxButtonEntfernen = erhalteBox . weFunctionBox

    tvarSprache :: WEWidgets z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = weTVarSprache

    tvarEvent :: WEWidgets z -> TVar EventAusführen
    tvarEvent = weTVarEvent

instance WidgetsTyp (ZugtypEither WEWidgets) where
    type ObjektTyp (ZugtypEither WEWidgets) = ZugtypEither Weiche

    erhalteObjektTyp :: ZugtypEither WEWidgets -> ZugtypEither Weiche
    erhalteObjektTyp = mapZugtypEither we

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => ZugtypEither WEWidgets -> m ()
    entferneWidgets (ZugtypMärklin weWidgets) = entferneWidgets weWidgets
    entferneWidgets (ZugtypLego weWidgets) = entferneWidgets weWidgets

    boxButtonEntfernen :: ZugtypEither WEWidgets -> Gtk.Box
    boxButtonEntfernen (ZugtypMärklin weWidgets) = boxButtonEntfernen weWidgets
    boxButtonEntfernen (ZugtypLego weWidgets) = boxButtonEntfernen weWidgets

    tvarSprache :: ZugtypEither WEWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (ZugtypMärklin weWidgets) = tvarSprache weWidgets
    tvarSprache (ZugtypLego weWidgets) = tvarSprache weWidgets

    tvarEvent :: ZugtypEither WEWidgets -> TVar EventAusführen
    tvarEvent (ZugtypMärklin weWidgets) = tvarEvent weWidgets
    tvarEvent (ZugtypLego weWidgets) = tvarEvent weWidgets

instance WegstreckenElement (WEWidgets 'Märklin) where
    type CheckButtonAuswahl (WEWidgets 'Märklin) = Richtung

    getterWegstrecke
        :: Lens.Getter (WEWidgets 'Märklin) (CheckButtonWegstreckeHinzufügen Richtung (WEWidgets 'Märklin))
    getterWegstrecke = Lens.to weHinzWS

    boxWegstrecke :: Weiche 'Märklin
                  -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (WEWidgets 'Märklin))
    boxWegstrecke _weWidgets = Lens.to vBoxHinzufügenWegstreckeWeichenMärklin

instance WegstreckenElement (WEWidgets 'Lego) where
    type CheckButtonAuswahl (WEWidgets 'Lego) = Richtung

    getterWegstrecke
        :: Lens.Getter (WEWidgets 'Lego) (CheckButtonWegstreckeHinzufügen Richtung (WEWidgets 'Lego))
    getterWegstrecke = Lens.to weHinzWS

    boxWegstrecke :: Weiche 'Lego
                  -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (WEWidgets 'Lego))
    boxWegstrecke _weWidgets = Lens.to vBoxHinzufügenWegstreckeWeichenLego

instance WegstreckenElement (ZugtypEither WEWidgets) where
    type CheckButtonAuswahl (ZugtypEither WEWidgets) = Richtung

    getterWegstrecke
        :: Lens.Getter (ZugtypEither WEWidgets) (CheckButtonWegstreckeHinzufügen Richtung (ZugtypEither WEWidgets))
    getterWegstrecke = Lens.to $ ausZugtypEither $ widgetHinzufügenZugtypEither . weHinzWS

    boxWegstrecke
        :: ZugtypEither Weiche
        -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (ZugtypEither WEWidgets))
    boxWegstrecke (ZugtypMärklin _weWidgets) =
        Lens.to $ widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeWeichenMärklin
    boxWegstrecke (ZugtypLego _weWidgets) =
        Lens.to $ widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeWeichenLego

instance PlanElement (WEWidgets 'Märklin) where
    foldPlan
        :: Lens.Fold (WEWidgets 'Märklin) (Maybe (ButtonPlanHinzufügen (WEWidgets 'Märklin)))
    foldPlan =
        Lens.folding
        $ \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
        -> [gerade, kurve, links, rechts]

    boxenPlan :: Weiche 'Märklin
              -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WEWidgets 'Märklin))
    boxenPlan _weWidgets =
        Lens.folding
        $ (??)
            [ vBoxHinzufügenPlanWeichenGeradeMärklin
            , vBoxHinzufügenPlanWeichenKurveMärklin
            , vBoxHinzufügenPlanWeichenLinksMärklin
            , vBoxHinzufügenPlanWeichenRechtsMärklin]

instance PlanElement (WEWidgets 'Lego) where
    foldPlan :: Lens.Fold (WEWidgets 'Lego) (Maybe (ButtonPlanHinzufügen (WEWidgets 'Lego)))
    foldPlan =
        Lens.folding
        $ \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
        -> [gerade, kurve, links, rechts]

    boxenPlan :: Weiche 'Lego -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WEWidgets 'Lego))
    boxenPlan _weWidgets =
        Lens.folding
        $ (??)
            [ vBoxHinzufügenPlanWeichenGeradeLego
            , vBoxHinzufügenPlanWeichenKurveLego
            , vBoxHinzufügenPlanWeichenLinksLego
            , vBoxHinzufügenPlanWeichenRechtsLego]

instance PlanElement (ZugtypEither WEWidgets) where
    foldPlan :: Lens.Fold (ZugtypEither WEWidgets) (Maybe (ButtonPlanHinzufügen (ZugtypEither WEWidgets)))
    foldPlan =
        Lens.folding
        $ ausZugtypEither
        $ \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
        -> fmap widgetHinzufügenZugtypEither <$> [gerade, kurve, links, rechts]

    boxenPlan :: ZugtypEither Weiche
              -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither WEWidgets))
    boxenPlan (ZugtypMärklin _weiche) =
        Lens.folding
        $ fmap widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWeichenGeradeMärklin
            , vBoxHinzufügenPlanWeichenKurveMärklin
            , vBoxHinzufügenPlanWeichenLinksMärklin
            , vBoxHinzufügenPlanWeichenRechtsMärklin]
    boxenPlan (ZugtypLego _weiche) =
        Lens.folding
        $ fmap widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWeichenGeradeLego
            , vBoxHinzufügenPlanWeichenKurveLego
            , vBoxHinzufügenPlanWeichenLinksLego
            , vBoxHinzufügenPlanWeichenRechtsLego]

instance StreckenObjekt (WEWidgets z) where
    anschlüsse :: WEWidgets z -> Set Anschluss
    anschlüsse WEWidgets {we} = anschlüsse we

    erhalteName :: WEWidgets z -> Text
    erhalteName WEWidgets {we} = erhalteName we

instance Aeson.ToJSON (WEWidgets z) where
    toJSON :: WEWidgets z -> Aeson.Value
    toJSON WEWidgets {we} = Aeson.toJSON we

instance WeicheKlasse (WEWidgets z) where
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => WEWidgets z -> Richtung -> m ()
    stellen WEWidgets {we} = stellen we

    erhalteRichtungen :: WEWidgets z -> NonEmpty Richtung
    erhalteRichtungen WEWidgets {we} = erhalteRichtungen we

-- | 'Weiche' darstellen und zum Status hinzufügen
weichePackNew :: forall m z.
              ( ObjektGuiReader m
              , MonadIO m
              , ZugtypKlasse z
              , WegstreckenElement (WEWidgets z)
              , PlanElement (WEWidgets z)
              )
              => Weiche z
              -> MStatusGuiT m (WEWidgets z)
weichePackNew weiche = do
    dynamischeWidgets@DynamischeWidgets {vBoxWeichen} <- erhalteDynamischeWidgets
    (weTVarSprache, weTVarEvent) <- liftIO $ do
        weTVarSprache <- newTVarIO $ Just []
        weTVarEvent <- newTVarIO EventAusführen
        pure (weTVarSprache, weTVarEvent)
    let justTVarSprache = Just weTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckeRichtungPackNew
        weiche
        (erhalteRichtungen weiche)
        weTVarSprache
    let [boxGerade, boxKurve, boxLinks, boxRechts] = dynamischeWidgets ^.. boxenPlan weiche
    hinzufügenPlanWidgetGerade <- if hatRichtung weiche Gerade
        then Just <$> hinzufügenWidgetPlanPackNew boxGerade weiche weTVarSprache
        else pure Nothing
    hinzufügenPlanWidgetKurve <- if hatRichtung weiche Kurve
        then Just <$> hinzufügenWidgetPlanPackNew boxKurve weiche weTVarSprache
        else pure Nothing
    hinzufügenPlanWidgetLinks <- if hatRichtung weiche Links
        then Just <$> hinzufügenWidgetPlanPackNew boxLinks weiche weTVarSprache
        else pure Nothing
    hinzufügenPlanWidgetRechts <- if hatRichtung weiche Rechts
        then Just <$> hinzufügenWidgetPlanPackNew boxRechts weiche weTVarSprache
        else pure Nothing
    let hinzufügenPlanWidget =
            WeichePlanHinzufügenWidgets
            { gerade = hinzufügenPlanWidgetGerade
            , kurve = hinzufügenPlanWidgetKurve
            , links = hinzufügenPlanWidgetLinks
            , rechts = hinzufügenPlanWidgetRechts
            }
    -- Widget erstellen
    (vBox, weFunctionBox) <- liftIO $ do
        vBox <- boxPackWidgetNewDefault vBoxWeichen $ Gtk.vBoxNew False 0
        weFunctionBox <- boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
        pure (vBox, weFunctionBox)
    let weWidgets =
            WEWidgets
            { we = weiche
            , weWidget = vBox
            , weFunctionBox
            , weHinzWS = hinzufügenWegstreckeWidget
            , weHinzPL = hinzufügenPlanWidget
            , weTVarSprache
            , weTVarEvent
            }
    namePackNew weFunctionBox weiche
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse
            <- boxPackWidgetNew weFunctionBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    richtungsButtonsPackNew weWidgets weFunctionBox vBoxAnschlüsse
    fließendPackNew vBoxAnschlüsse weiche justTVarSprache
    buttonEntfernenPackNew weWidgets $ entfernenWeiche $ zuZugtypEither weWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OWeiche $ zuZugtypEither weWidgets
    pure weWidgets
    where
        richtungsButtonsPackNew
            :: forall m z.
            (MonadIO m, ObjektGuiReader m)
            => WEWidgets z
            -> Gtk.HBox
            -> ScrollbaresWidget Gtk.VBox
            -> m ()
        richtungsButtonsPackNew
            WEWidgets {we = MärklinWeiche {wemRichtungsAnschlüsse}, weTVarSprache, weTVarEvent}
            box
            vBoxAktionen = do
            statusVar <- erhalteStatusVar :: m StatusVarGui
            objektReader <- ask
            forM_ wemRichtungsAnschlüsse $ \(richtung, anschluss) -> do
                let justTVarSprache = Just weTVarSprache
                    anzeigeRichtung = anzeige richtung
                boxPackWidgetNewDefault box
                    $ buttonNewWithEventLabel justTVarSprache anzeigeRichtung
                    $ eventAusführen weTVarEvent
                    $ flip runReaderT objektReader
                    $ ausführenStatusVarAktion (Stellen weiche richtung) statusVar
                boxPackWidgetNewDefault vBoxAktionen
                    $ anschlussNew justTVarSprache anzeigeRichtung anschluss
        richtungsButtonsPackNew
            WEWidgets { we = LegoWeiche {welRichtungsPin, welRichtungen = (richtung1, richtung2)}
                      , weTVarSprache
                      , weTVarEvent}
            box
            vBoxAktionen = void $ do
            statusVar <- erhalteStatusVar :: m StatusVarGui
            objektReader <- ask
            let justTVar = Just weTVarSprache
            boxPackWidgetNewDefault vBoxAktionen
                $ pinNew justTVar Language.richtung welRichtungsPin
            boxPackWidgetNewDefault box
                $ buttonNewWithEventLabel justTVar (anzeige richtung1)
                $ eventAusführen weTVarEvent
                $ flip runReaderT objektReader
                $ ausführenStatusVarAktion (Stellen weiche richtung1) statusVar
            boxPackWidgetNewDefault box
                $ buttonNewWithEventLabel justTVar (anzeige richtung2)
                $ eventAusführen weTVarEvent
                $ flip runReaderT objektReader
                $ ausführenStatusVarAktion (Stellen weiche richtung2) statusVar

-- ** Kupplung
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

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => KUWidgets -> m ()
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

    boxWegstrecke :: Kupplung -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen KUWidgets)
    boxWegstrecke _kuWidgets = Lens.to vBoxHinzufügenWegstreckeKupplungen

instance PlanElement KUWidgets where
    foldPlan :: Lens.Fold KUWidgets (Maybe (ButtonPlanHinzufügen KUWidgets))
    foldPlan = Lens.to $ Just . kuHinzPL

    boxenPlan :: Kupplung -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen KUWidgets)
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

-- ** Wegstrecke
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

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => WSWidgets z -> m ()
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

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => ZugtypEither WSWidgets -> m ()
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

    entferneWidgets
        :: (MonadIO m, DynamischeWidgetsReader r m) => GeschwindigkeitPhantom WSWidgets g z -> m ()
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

    boxenPlan :: Wegstrecke 'Märklin
              -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WSWidgets 'Märklin))
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

    boxenPlan :: Wegstrecke 'Lego
              -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WSWidgets 'Lego))
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

    boxenPlan :: ZugtypEither Wegstrecke
              -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither WSWidgets))
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

instance BGWidgetsKlasse (GeschwindigkeitPhantom Wegstrecke) where
    enthalteneBahngeschwindigkeiten :: (GeschwindigkeitEitherKlasse g)
                                    => GeschwindigkeitPhantom Wegstrecke g z
                                    -> Set (GeschwindigkeitEither Bahngeschwindigkeit z)
    enthalteneBahngeschwindigkeiten (GeschwindigkeitPhantom Wegstrecke {wsBahngeschwindigkeiten}) =
        wsBahngeschwindigkeiten

instance BGWidgetsKlasse (GeschwindigkeitPhantom WSWidgets) where
    enthalteneBahngeschwindigkeiten :: (GeschwindigkeitEitherKlasse g)
                                    => GeschwindigkeitPhantom WSWidgets g z
                                    -> Set (GeschwindigkeitEither Bahngeschwindigkeit z)
    enthalteneBahngeschwindigkeiten
        (GeschwindigkeitPhantom WSWidgets {ws = Wegstrecke {wsBahngeschwindigkeiten}}) =
        wsBahngeschwindigkeiten

instance STWidgetsKlasse (Wegstrecke z) where
    enthalteneStreckenabschnitte :: Wegstrecke z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = wsStreckenabschnitte

instance STWidgetsKlasse (WSWidgets z) where
    enthalteneStreckenabschnitte :: WSWidgets z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte = wsStreckenabschnitte . ws

instance STWidgetsKlasse (GeschwindigkeitPhantom Wegstrecke g z) where
    enthalteneStreckenabschnitte :: GeschwindigkeitPhantom Wegstrecke g z -> Set Streckenabschnitt
    enthalteneStreckenabschnitte (GeschwindigkeitPhantom Wegstrecke {wsStreckenabschnitte}) =
        wsStreckenabschnitte

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
            eitherFahrtrichtungWidget <- case zuZugtypEither wegstrecke of
                (ZugtypMärklin wsMärklin) -> Left
                    <$> buttonUmdrehenPackNew
                        functionBox
                        (GeschwindigkeitPhantom wsMärklin
                         :: GeschwindigkeitPhantom Wegstrecke 'Pwm 'Märklin)
                        wsTVarSprache
                        wsTVarEvent
                (ZugtypLego wsLego) -> Right
                    <$> auswahlFahrtrichtungEinstellenPackNew
                        functionBox
                        (GeschwindigkeitPhantom wsLego
                         :: GeschwindigkeitPhantom Wegstrecke 'Pwm 'Lego)
                        wsTVarSprache
                        wsTVarEvent
            pure (maybeScale, maybeAuswahlFahrstrom, rightToMaybe eitherFahrtrichtungWidget)
    wsToggleButtonStrom <- if null wsStreckenabschnitte
        then pure Nothing
        else do
            boxPackWidgetNewDefault vBoxExpander
                $ labelSpracheNew justTVarSprache
                $ Language.streckenabschnitte
                <:> fromJust (foldl appendName Nothing wsStreckenabschnitte)
            Just <$> toggleButtonStromPackNew functionBox wegstrecke wsTVarSprache wsTVarEvent
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

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => PLWidgets -> m ()
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

    boxenPlan :: Plan -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen PLWidgets)
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
