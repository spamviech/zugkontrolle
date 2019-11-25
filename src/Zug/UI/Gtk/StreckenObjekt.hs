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
{-# LANGUAGE CPP #-}

{-|
Description : Erstelle zusammengesetzte 

Allgemeine Hilfsfunktionen zum erstellen neuer Widgets
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.StreckenObjekt () where
#else
module Zug.UI.Gtk.StreckenObjekt (
    -- ** Spezifisches StreckenObjekt darstellen
    BGWidgets(), STWidgets(), WEWidgets(), KUWidgets(), WSWidgets(), PLWidgets(), WidgetsTyp(..),
    bahngeschwindigkeitPackNew, streckenabschnittPackNew, weichePackNew, kupplungPackNew, wegstreckePackNew, planPackNew,
    -- * Verwaltung des aktuellen Zustands
    DynamischeWidgets(..), DynamischeWidgetsReader(..), StatusGui, StatusReader(..),
    ObjektGui, BefehlGui, IOStatusGui, MStatusGui, MStatusGuiT,
    -- * Hinzufügen zu einem Plan/einer Wegstrecke
    WidgetHinzufügen(), HinzufügenZiel(..),
    CheckButtonWegstreckeHinzufügen, WegstreckeCheckButton(), WegstreckeCheckButtonVoid,
    KategorieText(..), Kategorie(..),
    BoxWegstreckeHinzufügen, boxWegstreckeHinzufügenNew,
    ButtonPlanHinzufügen, BoxPlanHinzufügen, boxPlanHinzufügenNew,
    widgetHinzufügenZugtypEither, widgetHinzufügenRegistrierterCheckButtonVoid,
    widgetHinzufügenAktuelleAuswahl, widgetHinzufügenToggled,
    widgetHinzufügenContainerRemoveJust, widgetHinzufügenBoxPackNew, widgetHinzufügenContainerGefüllt,
    foldWegstreckeHinzufügen, WegstreckenElement(..), entferneHinzufügenWegstreckeWidgets,
    PlanElement(..), entferneHinzufügenPlanWidgets) where

-- Bibliotheken
import Control.Applicative (ZipList(..))
import Control.Concurrent.STM (atomically, TMVar, putTMVar)
import Control.Lens ((^.), (^..), (??))
import qualified Control.Lens as Lens
import Control.Monad (void, unless, forM_)
import Control.Monad.Reader (MonadReader(..), asks, runReaderT)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.Aeson as Aeson
import Data.Foldable (Foldable(..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Void (Void)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (StreckenObjekt(..), Anschluss(), PwmReader(), I2CReader(),
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..),
                    Streckenabschnitt(..), StreckenabschnittKlasse(..),
                    Weiche(..), WeicheKlasse(..),
                    Kupplung(..), KupplungKlasse(..),
                    Wegstrecke(..), WegstreckeKlasse(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), ausZugtypEither, mapZugtypEither,
                    Fahrtrichtung(..), Strom(..), Richtung(..))
import qualified Zug.Language as Language
import Zug.Language (Sprache(), Anzeige(..), (<^>), (<:>), (<°>))
import Zug.Menge (ausFoldable)
import Zug.Objekt (ObjektAllgemein(..), ObjektElement(..), Objekt)
import Zug.Plan (PlanKlasse(..), Plan(..), AusführendReader(),
                AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionWeiche(..),
                AktionKupplung(..), AktionWegstrecke(..))
import Zug.UI.Base (StatusAllgemein(..), IOStatusAllgemein, MStatusAllgemein, MStatusAllgemeinT,
                    MitStatus(..), StatusReader(..),
                    AusführenMöglich(..), ReaderFamilie, ObjektReader, TVarMaps(..), MitTVarMaps(..),
                    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen,
                    auswertenTMVarIOStatus, ausführenMöglich, entfernenBahngeschwindigkeit,
                    entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung,
                    entfernenWegstrecke, entfernenPlan)
import Zug.UI.Befehl (BefehlAllgemein(..), ausführenTMVarBefehl, ausführenTMVarAktion)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledTMVar, registrierterCheckButtonNew,
                                        RegistrierterCheckButton, MitRegistrierterCheckButton(..),
                                        registrierterCheckButtonToggled)
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, containerRemoveJust, boxPackWidgetNew,
                                    boxPackWidgetNewDefault, paddingDefault, positionDefault,
                                    Packing(..), Position(..), dialogEval,
                                    buttonNewWithEventLabel, namePackNew, widgetShowNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), mitContainerRemove, MitBox(..), MitRange(..))
import Zug.UI.Gtk.Anschluss (anschlussNew)
import Zug.UI.Gtk.Auswahl (AuswahlWidget(), aktuelleAuswahl, auswahlRadioButtonNew,  MitAuswahlWidget(..))
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)

-- * Sammel-Typ um dynamische Widgets zu speichern
-- | Sammel-Typ spezialiert auf Gui-Typen
type ObjektGui = ObjektAllgemein BGWidgets STWidgets WEWidgets KUWidgets WSWidgets PLWidgets
-- | Befehl spezialiert auf Gui-Typen
type BefehlGui = BefehlAllgemein ObjektGui
-- | Zustands-Typ der Zustands-Monade spezialisiert auf Gui-Typen
type StatusGui = StatusAllgemein ObjektGui
-- | Zustands-Monaden-Transformer spezialisiert auf Gui-Typen in der IO-Monade
type IOStatusGui a = IOStatusAllgemein ObjektGui a
-- | Reine Zustands-Monade spezialiert auf Gui-Typen
type MStatusGui a = MStatusAllgemein ObjektGui a
-- | Zustands-Monaden-Transformer spezialiert auf Gui-Typen
type MStatusGuiT m a = MStatusAllgemeinT m ObjektGui a

-- | Auswahlmöglichkeiten zu 'WidgetHinzufügen'
data HinzufügenZiel = HinzufügenWegstrecke | HinzufügenPlan

-- | Widget zum Hinzufügen einer Wegstrecke/eines Plans
newtype WidgetHinzufügen (e :: HinzufügenZiel) (w :: Type) (a :: Type) = WidgetHinzufügen {widgetHinzufügen :: w}
    deriving (Eq)

instance (MitWidget w) => MitWidget (WidgetHinzufügen e w a) where
    erhalteWidget :: WidgetHinzufügen e w a -> Gtk.Widget
    erhalteWidget = erhalteWidget . widgetHinzufügen

instance (MitRegistrierterCheckButton w) => MitRegistrierterCheckButton (WidgetHinzufügen e w a) where
    erhalteRegistrierterCheckButton :: WidgetHinzufügen e w a -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton = erhalteRegistrierterCheckButton . widgetHinzufügen

-- | Konvertiere ein 'WidgetHinzufügen' in das zugehörige 'ZugtypEither'-Äquivalent
widgetHinzufügenZugtypEither :: WidgetHinzufügen e w (a z) -> WidgetHinzufügen e w (ZugtypEither a)
widgetHinzufügenZugtypEither = WidgetHinzufügen . widgetHinzufügen

-- | Konvertiere ein 'WidgetHinzufügen'-'MitRegistrierterCheckButton' in den zugehörigen 'Void'-Typ
widgetHinzufügenRegistrierterCheckButtonVoid :: (MitRegistrierterCheckButton r) =>
    WidgetHinzufügen e r a  -> WidgetHinzufügen e RegistrierterCheckButton Void
widgetHinzufügenRegistrierterCheckButtonVoid = WidgetHinzufügen . erhalteRegistrierterCheckButton . widgetHinzufügen

-- | Erhalte die aktuelle Auswahl des inkludierten 'MitAuswahlWidget'
widgetHinzufügenAktuelleAuswahl :: (Eq b, MitAuswahlWidget w b, MonadIO m) => WidgetHinzufügen e w a -> m b
widgetHinzufügenAktuelleAuswahl = aktuelleAuswahl . erhalteAuswahlWidget . widgetHinzufügen

-- | Überprüfe, ob der inkludierte 'MitRegistrierterCheckButton' aktuell gedrückt ist.
widgetHinzufügenToggled :: (MitRegistrierterCheckButton t, MonadIO m) => WidgetHinzufügen e t a -> m Bool
widgetHinzufügenToggled = registrierterCheckButtonToggled . erhalteRegistrierterCheckButton . widgetHinzufügen

-- | Entferne ein 'WidgetHinzufügen' (falls vorhanden) aus dem zugehörigen Container
widgetHinzufügenContainerRemoveJust :: (MitContainer c, MitWidget w, MonadIO m) =>
    WidgetHinzufügen e c a -> Maybe (WidgetHinzufügen e w a) -> m ()
widgetHinzufügenContainerRemoveJust c w = containerRemoveJust (widgetHinzufügen c) $ widgetHinzufügen <$> w

-- | Füge ein 'WidgetHinzufügen' zu einer zugehörigen Box hinzu
widgetHinzufügenBoxPackNew :: (MitBox b, MitWidget w, MonadIO m) =>
    WidgetHinzufügen e b a -> m w -> m (WidgetHinzufügen e w a)
widgetHinzufügenBoxPackNew b = fmap WidgetHinzufügen . boxPackWidgetNewDefault (widgetHinzufügen b)

-- | Teste ob ein 'WidgetHinzufügen'-'MitContainer' mindestens ein Element enthält.
widgetHinzufügenContainerGefüllt :: (MitContainer w, MonadIO m) => WidgetHinzufügen e w a -> m Bool
widgetHinzufügenContainerGefüllt
    = liftIO . fmap ((> 1) . length) . Gtk.containerGetChildren . erhalteContainer . widgetHinzufügen

-- | Text mit Typ-Annotation
newtype KategorieText a = KategorieText {kategorieText :: Sprache -> Text}
-- | Label für 'BoxWegstreckeHinzufügen'/'BoxPlanHinzufügen'
class Kategorie a where
    kategorie :: KategorieText a
instance Kategorie (BGWidgets z) where
    kategorie :: KategorieText (BGWidgets z)
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
type CheckButtonWegstreckeHinzufügen e a = WidgetHinzufügen 'HinzufügenWegstrecke (WegstreckeCheckButton e) a
-- | Box zur Auswahl der 'Wegstrecke'n-Elemente
type BoxWegstreckeHinzufügen a = WidgetHinzufügen 'HinzufügenWegstrecke (ScrollbaresWidget Gtk.VBox) a

-- | Erstelle eine neue 'BoxWegstreckeHinzufügen'
boxWegstreckeHinzufügenNew :: forall a m. (Kategorie a, MonadIO m) => m (BoxWegstreckeHinzufügen a)
boxWegstreckeHinzufügenNew = liftIO $ fmap WidgetHinzufügen $ scrollbaresWidgetNew $ do
    box <- Gtk.vBoxNew False 0
    boxPackWidgetNewDefault box $ Gtk.labelNew $ Just $ kategorieText (kategorie :: KategorieText a)
    pure box

-- | 'RegistrierterCheckButton', potentiell mit zusätlicher Richtungsauswahl
data WegstreckeCheckButton e where
    WegstreckeCheckButton :: {
        wcbvRegistrierterCheckButton :: RegistrierterCheckButton}
            -> WegstreckeCheckButton Void
    WegstreckeCheckButtonRichtung :: {
        wcbrWidget :: Gtk.Widget,
        wcbrRegistrierterCheckButton :: RegistrierterCheckButton,
        wcbrRichtungsAuswahl :: AuswahlWidget Richtung}
            -> WegstreckeCheckButton Richtung

deriving instance (Eq e) => Eq (WegstreckeCheckButton e)

instance MitWidget (WegstreckeCheckButton e) where
    erhalteWidget :: WegstreckeCheckButton e -> Gtk.Widget
    erhalteWidget   WegstreckeCheckButton {wcbvRegistrierterCheckButton}    = erhalteWidget wcbvRegistrierterCheckButton
    erhalteWidget   WegstreckeCheckButtonRichtung {wcbrWidget}              = wcbrWidget

instance MitRegistrierterCheckButton (WegstreckeCheckButton e) where
    erhalteRegistrierterCheckButton :: WegstreckeCheckButton e -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton
        WegstreckeCheckButton {wcbvRegistrierterCheckButton}
            = wcbvRegistrierterCheckButton
    erhalteRegistrierterCheckButton
        WegstreckeCheckButtonRichtung {wcbrRegistrierterCheckButton}
            = wcbrRegistrierterCheckButton

instance MitAuswahlWidget (WegstreckeCheckButton Richtung) Richtung where
    erhalteAuswahlWidget :: WegstreckeCheckButton Richtung -> AuswahlWidget Richtung
    erhalteAuswahlWidget WegstreckeCheckButtonRichtung {wcbrRichtungsAuswahl} = wcbrRichtungsAuswahl

-- | Button zum hinzufügen eines Plans
type ButtonPlanHinzufügen a = WidgetHinzufügen 'HinzufügenPlan Gtk.Button a
-- | Box zum hinzufügen eines Plans
type BoxPlanHinzufügen a = WidgetHinzufügen 'HinzufügenPlan (ScrollbaresWidget Gtk.VBox) a

-- | Erstelle eine neue 'BoxPlanHinzufügen'
boxPlanHinzufügenNew :: forall a m. (Kategorie a, MonadIO m) => m (BoxPlanHinzufügen a)
boxPlanHinzufügenNew = liftIO $ fmap WidgetHinzufügen $ scrollbaresWidgetNew $ do
    box <- Gtk.vBoxNew False 0
    boxPackWidgetNewDefault box $ Gtk.labelNew $ Just $ kategorieText (kategorie :: KategorieText a)
    pure box

-- | Sammlung aller Widgets, welche während der Laufzeit benötigt werden.
data DynamischeWidgets = DynamischeWidgets {
    vBoxBahngeschwindigkeiten :: ScrollbaresWidget Gtk.VBox,
    vBoxStreckenabschnitte :: ScrollbaresWidget Gtk.VBox,
    vBoxWeichen ::ScrollbaresWidget  Gtk.VBox,
    vBoxKupplungen :: ScrollbaresWidget Gtk.VBox,
    vBoxWegstrecken :: ScrollbaresWidget Gtk.VBox,
    vBoxPläne :: ScrollbaresWidget Gtk.VBox,
    vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin :: BoxWegstreckeHinzufügen (BGWidgets 'Märklin),
    vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego :: BoxWegstreckeHinzufügen (BGWidgets 'Lego),
    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin :: BoxPlanHinzufügen (BGWidgets 'Märklin),
    vBoxHinzufügenPlanBahngeschwindigkeitenLego :: BoxPlanHinzufügen (BGWidgets 'Lego),
    vBoxHinzufügenWegstreckeStreckenabschnitte :: BoxWegstreckeHinzufügen STWidgets,
    vBoxHinzufügenPlanStreckenabschnitte :: BoxPlanHinzufügen STWidgets,
    vBoxHinzufügenWegstreckeWeichenMärklin :: BoxWegstreckeHinzufügen (WEWidgets 'Märklin),
    vBoxHinzufügenWegstreckeWeichenLego :: BoxWegstreckeHinzufügen (WEWidgets 'Lego),
    vBoxHinzufügenPlanWeichenGeradeMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin),
    vBoxHinzufügenPlanWeichenKurveMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin),
    vBoxHinzufügenPlanWeichenLinksMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin),
    vBoxHinzufügenPlanWeichenRechtsMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin),
    vBoxHinzufügenPlanWeichenGeradeLego :: BoxPlanHinzufügen (WEWidgets 'Lego),
    vBoxHinzufügenPlanWeichenKurveLego :: BoxPlanHinzufügen (WEWidgets 'Lego),
    vBoxHinzufügenPlanWeichenLinksLego :: BoxPlanHinzufügen (WEWidgets 'Lego),
    vBoxHinzufügenPlanWeichenRechtsLego :: BoxPlanHinzufügen (WEWidgets 'Lego),
    vBoxHinzufügenWegstreckeKupplungen :: BoxWegstreckeHinzufügen KUWidgets,
    vBoxHinzufügenPlanKupplungen :: BoxPlanHinzufügen KUWidgets,
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin),
    vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin),
    vBoxHinzufügenPlanWegstreckenKupplungMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin),
    vBoxHinzufügenPlanWegstreckenMärklin :: BoxPlanHinzufügen (WSWidgets 'Märklin),
    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego :: BoxPlanHinzufügen (WSWidgets 'Lego),
    vBoxHinzufügenPlanWegstreckenStreckenabschnittLego :: BoxPlanHinzufügen (WSWidgets 'Lego),
    vBoxHinzufügenPlanWegstreckenKupplungLego :: BoxPlanHinzufügen (WSWidgets 'Lego),
    vBoxHinzufügenPlanWegstreckenLego :: BoxPlanHinzufügen (WSWidgets 'Lego),
    vBoxHinzufügenPlanPläne :: BoxPlanHinzufügen PLWidgets,
    progressBarPlan :: Gtk.ProgressBar,
    windowMain :: Gtk.Window,
    fortfahrenWennToggledWegstrecke :: FortfahrenWennToggledTMVar StatusGui WegstreckeCheckButtonVoid,
    tmvarPlanObjekt :: TMVar (Maybe Objekt)}

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

type instance ReaderFamilie ObjektGui = (TVarMaps, DynamischeWidgets, TMVar StatusGui)

instance MitTVarMaps (TVarMaps, DynamischeWidgets, TMVar StatusGui) where
    tvarMaps :: (TVarMaps, DynamischeWidgets, TMVar StatusGui) -> TVarMaps
    tvarMaps (tvarMaps, _dynamischeWidgets, _tmvarStatus) = tvarMaps
instance MitDynamischeWidgets (TVarMaps, DynamischeWidgets, TMVar StatusGui) where
    dynamischeWidgets :: (TVarMaps, DynamischeWidgets, TMVar StatusGui) -> DynamischeWidgets
    dynamischeWidgets (_tvarMaps, dynamischeWidgets, _tmvarStatus) = dynamischeWidgets
instance MitStatus (TVarMaps, DynamischeWidgets, TMVar StatusGui) ObjektGui where
    status :: (TVarMaps, DynamischeWidgets, TMVar StatusGui) -> TMVar StatusGui
    status (_tvarMaps, _dynamischeWidgets, tmvarStatus) = tmvarStatus

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

-- | Klasse für Gui-Darstellung von Typen, die zur Erstellung einer 'Wegstrecke' verwendet werden.
class (WidgetsTyp s) => WegstreckenElement s where
    -- | Auswahl-Typ beim erstellen einer Wegstrecke
    type CheckButtonAuswahl s
    type instance CheckButtonAuswahl s = Void
    -- | Getter auf 'RegistrierterCheckButton', ob 'StreckenObjekt' zu einer 'Wegstrecke' hinzugefügt werden soll
    getterWegstrecke :: Lens.Getter s (CheckButtonWegstreckeHinzufügen (CheckButtonAuswahl s) s)
    -- | Assoziierte 'BoxWegstreckeHinzufügen', in der 'MitRegistrierterCheckButton' gepackt ist.
    boxWegstrecke :: ObjektTyp s -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen s)

-- | Entferne 'Widget's zum Hinzufügen zu einer 'Wegstrecke' aus der entsprechenden Box
entferneHinzufügenWegstreckeWidgets :: forall s r m. (WegstreckenElement s, DynamischeWidgetsReader r m, MonadIO m) => s -> m ()
entferneHinzufügenWegstreckeWidgets wegsteckenElement = do
    box <- Lens.view (boxWegstrecke $ erhalteObjektTyp wegsteckenElement) <$> erhalteDynamischeWidgets
        :: m (BoxWegstreckeHinzufügen s)
    widgetHinzufügenContainerRemoveJust box $ Just $ wegsteckenElement ^. getterWegstrecke

-- | Typ-unspezifischer 'RegistrierterCheckButton' zum hinzufügen einer Wegstrecke
type WegstreckeCheckButtonVoid = WidgetHinzufügen 'HinzufügenWegstrecke RegistrierterCheckButton Void

-- | Alle 'RegistrierterCheckButton' zum hinzufügen einer Wegstrecke im aktuellen 'StatusGui'
foldWegstreckeHinzufügen :: Lens.Fold StatusGui WegstreckeCheckButtonVoid
foldWegstreckeHinzufügen = Lens.folding registrierteCheckButtons
    where
        registrierteCheckButtons :: StatusGui -> [WegstreckeCheckButtonVoid]
        registrierteCheckButtons status
            = map
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
entferneHinzufügenPlanWidgets :: forall s r m. (PlanElement s, DynamischeWidgetsReader r m, MonadIO m) => s -> m ()
entferneHinzufügenPlanWidgets planElement = do
    boxenPlan <- Lens.toListOf (boxenPlan $ erhalteObjektTyp planElement) <$> erhalteDynamischeWidgets
        :: m [BoxPlanHinzufügen s]
    sequence_ $ widgetHinzufügenContainerRemoveJust <$> ZipList boxenPlan <*> ZipList (planElement ^.. foldPlan)

-- | Neuen Entfernen-Knopf an das Ende der zugehörigen 'Box' hinzufügen.
-- Beim drücken werden 'entferneWidgets' und die übergebene 'IOStatusGui'-Aktion ausgeführt.
buttonEntfernenPackNew :: (WidgetsTyp w, ObjektReader ObjektGui m, MonadIO m) =>
    w -> IOStatusGui () -> m Gtk.Button
buttonEntfernenPackNew w entfernenAktion = do
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    liftIO $ boxPackWidgetNew (boxButtonEntfernen w) PackNatural paddingDefault End $
        buttonNewWithEventLabel Language.entfernen $
            flip runReaderT objektReader $ do
                auswertenTMVarIOStatus entfernenAktion tmvarStatus
                entferneWidgets w

-- ** Widget mit Name und CheckButton erstellen
-- | Erzeuge einen 'RegistrierterCheckButton' mit einem 'Label' für den Namen.
hinzufügenWidgetWegstreckePackNew :: forall o r m.
    (DynamischeWidgetsReader r m, StreckenObjekt (ObjektTyp o), WegstreckenElement o, MonadIO m) =>
        ObjektTyp o -> m (CheckButtonWegstreckeHinzufügen Void o)
hinzufügenWidgetWegstreckePackNew objekt = do
    dynamischeWidgets@DynamischeWidgets {fortfahrenWennToggledWegstrecke} <- erhalteDynamischeWidgets
    let box = dynamischeWidgets ^. boxWegstrecke objekt :: BoxWegstreckeHinzufügen o
    widgetHinzufügenBoxPackNew box $ WegstreckeCheckButton <$>
        registrierterCheckButtonNew (erhalteName objekt) fortfahrenWennToggledWegstrecke
-- | Erzeuge einen 'RegistrierterCheckButton' mit einem 'Label' für den Namen und einem 'AuswahlWidget' für die übergebenen 'Richtung'en.
hinzufügenWidgetWegstreckeRichtungPackNew :: forall o r m.
    (DynamischeWidgetsReader r m, StreckenObjekt (ObjektTyp o), WegstreckenElement o, MonadIO m) =>
    ObjektTyp o -> NonEmpty Richtung -> m (CheckButtonWegstreckeHinzufügen Richtung o)
hinzufügenWidgetWegstreckeRichtungPackNew objekt richtungen = do
    dynamischeWidgets@DynamischeWidgets {fortfahrenWennToggledWegstrecke} <- erhalteDynamischeWidgets
    let box = dynamischeWidgets ^. boxWegstrecke objekt :: BoxWegstreckeHinzufügen o
    liftIO $ widgetHinzufügenBoxPackNew box $ do
            hBox <- Gtk.hBoxNew False 0
            wcbrRegistrierterCheckButton <- boxPackWidgetNewDefault hBox $
                registrierterCheckButtonNew (erhalteName objekt) fortfahrenWennToggledWegstrecke
            wcbrRichtungsAuswahl <- boxPackWidgetNewDefault hBox $ auswahlRadioButtonNew richtungen ""
            pure WegstreckeCheckButtonRichtung {
                wcbrWidget = erhalteWidget hBox,
                wcbrRegistrierterCheckButton,
                wcbrRichtungsAuswahl}

-- | Füge einen Knopf mit dem Namen zur Box hinzu. Beim drücken wird die 'TMVar' mit dem Objekt gefüllt.
hinzufügenWidgetPlanPackNew ::
    (DynamischeWidgetsReader r m, StreckenObjekt (ObjektTyp o), ObjektElement (ObjektTyp o), MonadIO m) =>
        BoxPlanHinzufügen o -> ObjektTyp o -> m (ButtonPlanHinzufügen o)
hinzufügenWidgetPlanPackNew box objekt = do
    DynamischeWidgets {tmvarPlanObjekt} <- erhalteDynamischeWidgets
    widgetHinzufügenBoxPackNew box $ buttonNewWithEventLabel (erhalteName objekt) $
        atomically $ putTMVar tmvarPlanObjekt $ Just $ zuObjekt objekt

-- * Darstellung von Streckenobjekten
-- ** Bahngeschwindigkeit
-- | 'Bahngeschwindigkeit' mit zugehörigen Widgets
data BGWidgets (z :: Zugtyp)
    = BGWidgets {
        bg :: Bahngeschwindigkeit z,
        bgWidget :: Gtk.HBox,
        bgHinzWS :: CheckButtonWegstreckeHinzufügen Void (BGWidgets z),
        bgHinzPL :: ButtonPlanHinzufügen (BGWidgets z)}

deriving instance (Eq (ObjektTyp (BGWidgets z))) => Eq (BGWidgets z)

instance MitWidget (BGWidgets z) where
    erhalteWidget :: BGWidgets z -> Gtk.Widget
    erhalteWidget = erhalteWidget . bgWidget

instance (WegstreckenElement (BGWidgets z), PlanElement (BGWidgets z)) => WidgetsTyp (BGWidgets z) where
    type ObjektTyp (BGWidgets z) = Bahngeschwindigkeit z
    erhalteObjektTyp :: BGWidgets z -> Bahngeschwindigkeit z
    erhalteObjektTyp = bg
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => BGWidgets z -> m ()
    entferneWidgets bgWidgets = do
        DynamischeWidgets {vBoxBahngeschwindigkeiten} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxBahngeschwindigkeiten bgWidgets
        entferneHinzufügenWegstreckeWidgets bgWidgets
        entferneHinzufügenPlanWidgets bgWidgets
    boxButtonEntfernen :: BGWidgets z -> Gtk.Box
    boxButtonEntfernen = erhalteBox . bgWidget

instance WidgetsTyp (ZugtypEither BGWidgets) where
    type ObjektTyp (ZugtypEither BGWidgets) = ZugtypEither Bahngeschwindigkeit
    erhalteObjektTyp :: ZugtypEither BGWidgets -> ZugtypEither Bahngeschwindigkeit
    erhalteObjektTyp = mapZugtypEither bg
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => ZugtypEither BGWidgets -> m ()
    entferneWidgets (ZugtypMärklin bgWidgets)   = entferneWidgets bgWidgets
    entferneWidgets (ZugtypLego bgWidgets)      = entferneWidgets bgWidgets
    boxButtonEntfernen :: ZugtypEither BGWidgets -> Gtk.Box
    boxButtonEntfernen  (ZugtypMärklin bgWidgets)   = boxButtonEntfernen bgWidgets
    boxButtonEntfernen  (ZugtypLego bgWidgets)      = boxButtonEntfernen bgWidgets

instance WegstreckenElement (BGWidgets 'Märklin) where
    getterWegstrecke :: Lens.Getter (BGWidgets 'Märklin) (CheckButtonWegstreckeHinzufügen Void (BGWidgets 'Märklin))
    getterWegstrecke = Lens.to bgHinzWS
    boxWegstrecke :: Bahngeschwindigkeit 'Märklin -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (BGWidgets 'Märklin))
    boxWegstrecke _bgWidgets = Lens.to vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
instance WegstreckenElement (BGWidgets 'Lego) where
    getterWegstrecke :: Lens.Getter (BGWidgets 'Lego) (CheckButtonWegstreckeHinzufügen Void (BGWidgets 'Lego))
    getterWegstrecke = Lens.to bgHinzWS
    boxWegstrecke :: Bahngeschwindigkeit 'Lego -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (BGWidgets 'Lego))
    boxWegstrecke _bgWidgets = Lens.to vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
instance WegstreckenElement (ZugtypEither BGWidgets) where
    getterWegstrecke :: Lens.Getter (ZugtypEither BGWidgets) (CheckButtonWegstreckeHinzufügen Void (ZugtypEither BGWidgets))
    getterWegstrecke = Lens.to $ ausZugtypEither $ widgetHinzufügenZugtypEither . bgHinzWS
    boxWegstrecke :: ZugtypEither Bahngeschwindigkeit -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (ZugtypEither BGWidgets))
    boxWegstrecke (ZugtypMärklin _bgWidgets) = Lens.to $
        widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
    boxWegstrecke (ZugtypLego _bgWidgets) = Lens.to $
        widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego

instance PlanElement (BGWidgets 'Märklin) where
    foldPlan :: Lens.Fold (BGWidgets 'Märklin) (Maybe (ButtonPlanHinzufügen (BGWidgets 'Märklin)))
    foldPlan = Lens.to $ Just . bgHinzPL
    boxenPlan :: Bahngeschwindigkeit 'Märklin -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (BGWidgets 'Märklin))
    boxenPlan _bgWidgets = Lens.to vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
instance PlanElement (BGWidgets 'Lego) where
    foldPlan :: Lens.Fold (BGWidgets 'Lego) (Maybe (ButtonPlanHinzufügen (BGWidgets 'Lego)))
    foldPlan = Lens.to $ Just . bgHinzPL
    boxenPlan :: Bahngeschwindigkeit 'Lego -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (BGWidgets 'Lego))
    boxenPlan _bgWidgets = Lens.to vBoxHinzufügenPlanBahngeschwindigkeitenLego
instance PlanElement (ZugtypEither BGWidgets) where
    foldPlan :: Lens.Fold (ZugtypEither BGWidgets) (Maybe (ButtonPlanHinzufügen (ZugtypEither BGWidgets)))
    foldPlan = Lens.to $ ausZugtypEither $ Just . widgetHinzufügenZugtypEither . bgHinzPL
    boxenPlan :: ZugtypEither Bahngeschwindigkeit -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither BGWidgets))
    boxenPlan   (ZugtypMärklin _bahngeschwindigkeit)    = Lens.to $
        widgetHinzufügenZugtypEither . vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
    boxenPlan   (ZugtypLego _bahngeschwindigkeit)   = Lens.to $
        widgetHinzufügenZugtypEither . vBoxHinzufügenPlanBahngeschwindigkeitenLego

instance StreckenObjekt (BGWidgets z) where
    anschlüsse :: BGWidgets z -> [Anschluss]
    anschlüsse = anschlüsse . bg
    erhalteName :: BGWidgets z -> Text
    erhalteName = erhalteName . bg

instance (ZugtypKlasse z) => ObjektElement (BGWidgets z) where
    zuObjekt :: BGWidgets z -> Objekt
    zuObjekt = zuObjekt . bg

instance Aeson.ToJSON (BGWidgets z) where
    toJSON :: BGWidgets z -> Aeson.Value
    toJSON = Aeson.toJSON . bg

instance BahngeschwindigkeitKlasse BGWidgets where
    geschwindigkeit :: (PwmReader r m, MonadIO m) => BGWidgets z -> Natural -> m ()
    geschwindigkeit = geschwindigkeit . bg
    umdrehen :: (PwmReader r m, MonadIO m) => BGWidgets 'Märklin -> m ()
    umdrehen = umdrehen . bg
    fahrtrichtungEinstellen :: (PwmReader r m, MonadIO m) => BGWidgets 'Lego -> Fahrtrichtung -> m ()
    fahrtrichtungEinstellen = fahrtrichtungEinstellen . bg

-- | 'Bahngeschwindigkeit' darstellen und zum Status hinzufügen
bahngeschwindigkeitPackNew ::
    (ObjektReader ObjektGui m, MonadIO m, ZugtypKlasse z,
    WegstreckenElement (BGWidgets z), PlanElement (BGWidgets z)) =>
        Bahngeschwindigkeit z -> m (BGWidgets z)
bahngeschwindigkeitPackNew bahngeschwindigkeit = do
    tmvarStatus <- erhalteStatus
    dynamischeWidgets@DynamischeWidgets {vBoxBahngeschwindigkeiten} <- erhalteDynamischeWidgets
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWidgetWegstrecke <- hinzufügenWidgetWegstreckePackNew bahngeschwindigkeit
    let boxPlan = head $ dynamischeWidgets ^.. boxenPlan bahngeschwindigkeit
    hinzufügenWidgetPlan <- hinzufügenWidgetPlanPackNew boxPlan bahngeschwindigkeit
    -- Widget erstellen
    hBox <- liftIO $ boxPackWidgetNewDefault vBoxBahngeschwindigkeiten $ Gtk.hBoxNew False 0
    let bgWidgets = BGWidgets {
        bg = bahngeschwindigkeit,
        bgWidget = hBox,
        bgHinzWS = hinzufügenWidgetWegstrecke,
        bgHinzPL = hinzufügenWidgetPlan}
    namePackNew hBox bahngeschwindigkeit
    boxPackWidgetNewDefault hBox $ anschlussNew Language.geschwindigkeit $ getGeschwindigkeitsAnschluss bahngeschwindigkeit
    hScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew hBox bgWidgets
    fahrtrichtungsWidgetsPackNew hBox bahngeschwindigkeit hScaleGeschwindigkeit
    fließendPackNew hBox bahngeschwindigkeit
    buttonEntfernenPackNew bgWidgets $ entfernenBahngeschwindigkeit $ zuZugtypEither bgWidgets
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OBahngeschwindigkeit $ zuZugtypEither bgWidgets) tmvarStatus
    pure bgWidgets
        where
            getGeschwindigkeitsAnschluss :: Bahngeschwindigkeit z -> Anschluss
            getGeschwindigkeitsAnschluss
                MärklinBahngeschwindigkeit {bgmGeschwindigkeitsAnschluss}
                    = bgmGeschwindigkeitsAnschluss
            getGeschwindigkeitsAnschluss
                LegoBahngeschwindigkeit {bglGeschwindigkeitsAnschluss}
                    = bglGeschwindigkeitsAnschluss
            fahrtrichtungsWidgetsPackNew :: (ObjektReader ObjektGui m, MonadIO m, MitBox b, MitRange r) =>
                b -> Bahngeschwindigkeit z -> r -> m ()
            fahrtrichtungsWidgetsPackNew
                box
                bgMärklin@MärklinBahngeschwindigkeit {}
                range
                    = void $ buttonUmdrehenPackNew box bgMärklin range
            fahrtrichtungsWidgetsPackNew
                box
                bgLego@LegoBahngeschwindigkeit {bglFahrtrichtungsAnschluss}
                range
                    = void $ do
                        liftIO $ boxPackWidgetNewDefault box $
                            anschlussNew Language.fahrtrichtung bglFahrtrichtungsAnschluss
                        togglebuttonFahrtrichtungEinstellenPackNew box bgLego range

-- | Füge 'Scale' zum einstellen der Geschwindigkeit zur Box hinzu
hScaleGeschwindigkeitPackNew ::
    (MitBox b, BahngeschwindigkeitKlasse bg, ObjektReader ObjektGui m, MonadIO m) =>
        b -> bg z -> m Gtk.HScale
hScaleGeschwindigkeitPackNew box bahngeschwindigkeit = do
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    liftIO $ do
        scale <- boxPackWidgetNew box PackGrow paddingDefault positionDefault $ widgetShowNew $
            Gtk.hScaleNewWithRange 0 100 1
        Gtk.on scale Gtk.valueChanged $ do
            wert <- Gtk.get scale Gtk.rangeValue
            flip runReaderT objektReader $
                ausführenTMVarAktion (Geschwindigkeit bahngeschwindigkeit $ floor wert) tmvarStatus
        pure scale

-- | Füge 'Gtk.Button' zum umdrehen/ zur Box hinzu
buttonUmdrehenPackNew :: (MitBox b, BahngeschwindigkeitKlasse bg, MitRange r, ObjektReader ObjektGui m, MonadIO m) =>
    b -> bg 'Märklin -> r -> m Gtk.Button
buttonUmdrehenPackNew box bahngeschwindigkeit rangeGeschwindigkeit = do
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    liftIO $ do
        boxPackWidgetNewDefault box $ buttonNewWithEventLabel Language.umdrehen $ do
            Gtk.set (erhalteRange rangeGeschwindigkeit) [Gtk.rangeValue := 0]
            flip runReaderT objektReader $
                ausführenTMVarAktion (Umdrehen bahngeschwindigkeit) tmvarStatus
-- | Füge 'Gtk.ToggleButton' zum Fahrtrichtung einstellen zur Box hinzu
togglebuttonFahrtrichtungEinstellenPackNew ::
    (MitBox b, BahngeschwindigkeitKlasse bg, MitRange r, ObjektReader ObjektGui m, MonadIO m) =>
        b -> bg 'Lego -> r -> m Gtk.ToggleButton
togglebuttonFahrtrichtungEinstellenPackNew box bahngeschwindigkeit rangeGeschwindigkeit = do
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    liftIO $ do
        toggleButton <- boxPackWidgetNewDefault box $ Gtk.toggleButtonNewWithLabel (Language.umdrehen :: Text)
        Gtk.on toggleButton Gtk.toggled $ do
            Gtk.set (erhalteRange rangeGeschwindigkeit) [Gtk.rangeValue := 0]
            vorwärts <- Gtk.get toggleButton Gtk.toggleButtonActive
            let fahrtrichtung = if vorwärts then Vorwärts else Rückwärts
            flip runReaderT objektReader $
                ausführenTMVarAktion (FahrtrichtungEinstellen bahngeschwindigkeit fahrtrichtung) tmvarStatus
        pure toggleButton

-- ** Streckenabschnitt
-- | 'Streckenabschnitt' mit zugehörigen Widgets
data STWidgets
    = STWidgets {
        st :: Streckenabschnitt,
        stWidget :: Gtk.HBox,
        stHinzWS :: CheckButtonWegstreckeHinzufügen Void STWidgets,
        stHinzPL :: ButtonPlanHinzufügen STWidgets}
    deriving (Eq)

instance MitWidget STWidgets where
    erhalteWidget :: STWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . stWidget

instance WidgetsTyp STWidgets where
    type ObjektTyp STWidgets = Streckenabschnitt
    erhalteObjektTyp :: STWidgets -> Streckenabschnitt
    erhalteObjektTyp = st
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => STWidgets -> m ()
    entferneWidgets stWidgets = do
        DynamischeWidgets {vBoxStreckenabschnitte} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxStreckenabschnitte stWidgets
        entferneHinzufügenWegstreckeWidgets stWidgets
        entferneHinzufügenPlanWidgets stWidgets
    boxButtonEntfernen :: STWidgets -> Gtk.Box
    boxButtonEntfernen = erhalteBox . stWidget

instance WegstreckenElement STWidgets where
    getterWegstrecke :: Lens.Getter STWidgets (CheckButtonWegstreckeHinzufügen Void STWidgets)
    getterWegstrecke = Lens.to stHinzWS
    boxWegstrecke :: Streckenabschnitt -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen STWidgets)
    boxWegstrecke _stWidgets = Lens.to vBoxHinzufügenWegstreckeStreckenabschnitte

instance PlanElement STWidgets where
    foldPlan :: Lens.Fold STWidgets (Maybe (ButtonPlanHinzufügen STWidgets))
    foldPlan = Lens.folding $ (:[]) . Just . stHinzPL
    boxenPlan :: Streckenabschnitt -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen STWidgets)
    boxenPlan _stWidgets = Lens.to vBoxHinzufügenPlanStreckenabschnitte

instance StreckenObjekt STWidgets where
    anschlüsse :: STWidgets -> [Anschluss]
    anschlüsse STWidgets {st} = anschlüsse st
    erhalteName :: STWidgets -> Text
    erhalteName STWidgets {st} = erhalteName st

instance Aeson.ToJSON STWidgets where
    toJSON :: STWidgets -> Aeson.Value
    toJSON (STWidgets {st}) = Aeson.toJSON st

instance StreckenabschnittKlasse STWidgets where
    strom :: (I2CReader r m, MonadIO m) => STWidgets -> Strom -> m ()
    strom STWidgets {st} = strom st

-- | 'Streckenabschnitt' darstellen und zum Status hinzufügen
streckenabschnittPackNew :: (ObjektReader ObjektGui m, MonadIO m) =>
    Streckenabschnitt -> m STWidgets
streckenabschnittPackNew streckenabschnitt@Streckenabschnitt {stromAnschluss} = do
    tmvarStatus <- erhalteStatus
    DynamischeWidgets {
        vBoxStreckenabschnitte,
        vBoxHinzufügenPlanStreckenabschnitte}
            <- erhalteDynamischeWidgets
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckePackNew streckenabschnitt
    hinzufügenPlanWidget <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanStreckenabschnitte streckenabschnitt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxStreckenabschnitte $ liftIO $ Gtk.hBoxNew False 0
    namePackNew hBox streckenabschnitt
    boxPackWidgetNewDefault hBox $ anschlussNew Language.strom stromAnschluss
    toggleButtonStromPackNew hBox streckenabschnitt
    fließendPackNew hBox streckenabschnitt
    let stWidgets = STWidgets {st = streckenabschnitt, stWidget = hBox, stHinzPL = hinzufügenPlanWidget, stHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackNew stWidgets $ entfernenStreckenabschnitt stWidgets
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OStreckenabschnitt stWidgets) tmvarStatus
    pure stWidgets

-- | Füge 'Gtk.ToggleButton' zum einstellen des Stroms zur Box hinzu
toggleButtonStromPackNew :: (ObjektReader ObjektGui m, MonadIO m, MitBox b, StreckenabschnittKlasse s) =>
    b -> s -> m Gtk.ToggleButton
toggleButtonStromPackNew box streckenabschnitt = do
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    liftIO $ do
        toggleButton <- boxPackWidgetNewDefault box $ Gtk.toggleButtonNewWithLabel (Language.strom :: Text)
        Gtk.on toggleButton Gtk.toggled $ do
            an <- Gtk.get toggleButton Gtk.toggleButtonActive
            let fließend = if an then Fließend else Gesperrt
            flip runReaderT objektReader $
                ausführenTMVarAktion (Strom streckenabschnitt fließend) tmvarStatus
        pure toggleButton

-- ** Weiche
-- | 'Weiche' mit zugehörigen Widgets
data WEWidgets (z :: Zugtyp)
    = WEWidgets {
        we :: Weiche z,
        weWidget :: Gtk.HBox,
        weHinzWS :: CheckButtonWegstreckeHinzufügen Richtung (WEWidgets z),
        weHinzPL :: WeichePlanHinzufügenWidgets z}
    deriving (Eq)

-- | Widgets zum Hinzufügen einer 'Weiche' zu einem 'Plan'
data WeichePlanHinzufügenWidgets z
    = WeichePlanHinzufügenWidgets {
        gerade :: Maybe (ButtonPlanHinzufügen (WEWidgets z)),
        kurve :: Maybe (ButtonPlanHinzufügen (WEWidgets z)),
        links :: Maybe (ButtonPlanHinzufügen (WEWidgets z)),
        rechts :: Maybe (ButtonPlanHinzufügen (WEWidgets z))}
    deriving (Eq)

instance MitWidget (WEWidgets z) where
    erhalteWidget :: WEWidgets z -> Gtk.Widget
    erhalteWidget = erhalteWidget . weWidget

instance (WegstreckenElement (WEWidgets z), PlanElement (WEWidgets z)) => WidgetsTyp (WEWidgets z) where
    type ObjektTyp (WEWidgets z) = Weiche z
    erhalteObjektTyp :: WEWidgets z -> Weiche z
    erhalteObjektTyp = we
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => WEWidgets z -> m ()
    entferneWidgets weWidgets = do
        DynamischeWidgets {vBoxWeichen} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxWeichen weWidgets
        entferneHinzufügenWegstreckeWidgets weWidgets
        entferneHinzufügenPlanWidgets weWidgets
    boxButtonEntfernen :: WEWidgets z -> Gtk.Box
    boxButtonEntfernen = erhalteBox . weWidget

instance WidgetsTyp (ZugtypEither WEWidgets) where
    type ObjektTyp (ZugtypEither WEWidgets) = ZugtypEither Weiche
    erhalteObjektTyp :: ZugtypEither WEWidgets -> ZugtypEither Weiche
    erhalteObjektTyp = mapZugtypEither we
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => ZugtypEither WEWidgets -> m ()
    entferneWidgets (ZugtypMärklin weWidgets)   = entferneWidgets weWidgets
    entferneWidgets (ZugtypLego weWidgets)      = entferneWidgets weWidgets
    boxButtonEntfernen :: ZugtypEither WEWidgets -> Gtk.Box
    boxButtonEntfernen  (ZugtypMärklin weWidgets)   = boxButtonEntfernen weWidgets
    boxButtonEntfernen  (ZugtypLego weWidgets)      = boxButtonEntfernen weWidgets

instance WegstreckenElement (WEWidgets 'Märklin) where
    type CheckButtonAuswahl (WEWidgets 'Märklin) = Richtung
    getterWegstrecke :: Lens.Getter (WEWidgets 'Märklin) (CheckButtonWegstreckeHinzufügen Richtung (WEWidgets 'Märklin))
    getterWegstrecke = Lens.to weHinzWS
    boxWegstrecke :: Weiche 'Märklin -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (WEWidgets 'Märklin))
    boxWegstrecke _weWidgets = Lens.to vBoxHinzufügenWegstreckeWeichenMärklin
instance WegstreckenElement (WEWidgets 'Lego) where
    type CheckButtonAuswahl (WEWidgets 'Lego) = Richtung
    getterWegstrecke :: Lens.Getter (WEWidgets 'Lego) (CheckButtonWegstreckeHinzufügen Richtung (WEWidgets 'Lego))
    getterWegstrecke = Lens.to weHinzWS
    boxWegstrecke :: Weiche 'Lego -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (WEWidgets 'Lego))
    boxWegstrecke _weWidgets = Lens.to vBoxHinzufügenWegstreckeWeichenLego
instance WegstreckenElement (ZugtypEither WEWidgets) where
    type CheckButtonAuswahl (ZugtypEither WEWidgets) = Richtung
    getterWegstrecke :: Lens.Getter (ZugtypEither WEWidgets) (CheckButtonWegstreckeHinzufügen Richtung (ZugtypEither WEWidgets))
    getterWegstrecke = Lens.to $ ausZugtypEither $ widgetHinzufügenZugtypEither . weHinzWS
    boxWegstrecke :: ZugtypEither Weiche -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (ZugtypEither WEWidgets))
    boxWegstrecke (ZugtypMärklin _weWidgets) = Lens.to $
        widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeWeichenMärklin
    boxWegstrecke (ZugtypLego _weWidgets) = Lens.to $
        widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeWeichenLego

instance PlanElement (WEWidgets 'Märklin) where
    foldPlan :: Lens.Fold (WEWidgets 'Märklin) (Maybe (ButtonPlanHinzufügen (WEWidgets 'Märklin)))
    foldPlan = Lens.folding $
        \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
            -> [gerade, kurve, links, rechts]
    boxenPlan :: Weiche 'Märklin -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WEWidgets 'Märklin))
    boxenPlan _weWidgets
        = Lens.folding $ (??) [
            vBoxHinzufügenPlanWeichenGeradeMärklin,
            vBoxHinzufügenPlanWeichenKurveMärklin,
            vBoxHinzufügenPlanWeichenLinksMärklin,
            vBoxHinzufügenPlanWeichenRechtsMärklin]
instance PlanElement (WEWidgets 'Lego) where
    foldPlan :: Lens.Fold (WEWidgets 'Lego) (Maybe (ButtonPlanHinzufügen (WEWidgets 'Lego)))
    foldPlan = Lens.folding $
        \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
            -> [gerade, kurve, links, rechts]
    boxenPlan :: Weiche 'Lego -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WEWidgets 'Lego))
    boxenPlan _weWidgets
        = Lens.folding $ (??) [
            vBoxHinzufügenPlanWeichenGeradeLego,
            vBoxHinzufügenPlanWeichenKurveLego,
            vBoxHinzufügenPlanWeichenLinksLego,
            vBoxHinzufügenPlanWeichenRechtsLego]
instance PlanElement (ZugtypEither WEWidgets) where
    foldPlan :: Lens.Fold (ZugtypEither WEWidgets) (Maybe (ButtonPlanHinzufügen (ZugtypEither WEWidgets)))
    foldPlan = Lens.folding $ ausZugtypEither $
        \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
            -> fmap widgetHinzufügenZugtypEither <$> [gerade, kurve, links, rechts]
    boxenPlan :: ZugtypEither Weiche -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither WEWidgets))
    boxenPlan   (ZugtypMärklin _weiche) = Lens.folding $ fmap widgetHinzufügenZugtypEither . (??) [
        vBoxHinzufügenPlanWeichenGeradeMärklin,
        vBoxHinzufügenPlanWeichenKurveMärklin,
        vBoxHinzufügenPlanWeichenLinksMärklin,
        vBoxHinzufügenPlanWeichenRechtsMärklin]
    boxenPlan   (ZugtypLego _weiche)    = Lens.folding $ fmap widgetHinzufügenZugtypEither . (??) [
        vBoxHinzufügenPlanWeichenGeradeLego,
        vBoxHinzufügenPlanWeichenKurveLego,
        vBoxHinzufügenPlanWeichenLinksLego,
        vBoxHinzufügenPlanWeichenRechtsLego]

instance StreckenObjekt (WEWidgets z) where
    anschlüsse :: WEWidgets z -> [Anschluss]
    anschlüsse WEWidgets {we} = anschlüsse we
    erhalteName :: WEWidgets z -> Text
    erhalteName WEWidgets {we} = erhalteName we

instance Aeson.ToJSON (WEWidgets z) where
    toJSON :: WEWidgets z -> Aeson.Value
    toJSON WEWidgets {we} = Aeson.toJSON we

instance WeicheKlasse (WEWidgets z) where
    stellen :: (PwmReader r m, MonadIO m) => WEWidgets z -> Richtung -> m ()
    stellen WEWidgets {we} = stellen we
    erhalteRichtungen :: WEWidgets z -> NonEmpty Richtung
    erhalteRichtungen WEWidgets {we} = erhalteRichtungen we

-- | 'Weiche' darstellen und zum Status hinzufügen
weichePackNew ::
    (ObjektReader ObjektGui m, MonadIO m, ZugtypKlasse z,
    WegstreckenElement (WEWidgets z) , PlanElement (WEWidgets z)) =>
        Weiche z -> m (WEWidgets z)
weichePackNew weiche = do
    tmvarStatus <- erhalteStatus
    dynamischeWidgets@DynamischeWidgets {vBoxWeichen} <- erhalteDynamischeWidgets
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckeRichtungPackNew weiche $ erhalteRichtungen weiche
    let [boxGerade, boxKurve, boxLinks, boxRechts] = dynamischeWidgets ^.. boxenPlan weiche
    hinzufügenPlanWidgetGerade <- if hatRichtung weiche Gerade
        then Just <$> hinzufügenWidgetPlanPackNew boxGerade weiche
        else pure Nothing
    hinzufügenPlanWidgetKurve <- if hatRichtung weiche Kurve
        then Just <$> hinzufügenWidgetPlanPackNew boxKurve weiche
        else pure Nothing
    hinzufügenPlanWidgetLinks <- if hatRichtung weiche Links
        then Just <$> hinzufügenWidgetPlanPackNew boxLinks weiche
        else pure Nothing
    hinzufügenPlanWidgetRechts <- if hatRichtung weiche Rechts
        then Just <$> hinzufügenWidgetPlanPackNew boxRechts weiche
        else pure Nothing
    let hinzufügenPlanWidget = WeichePlanHinzufügenWidgets {
        gerade = hinzufügenPlanWidgetGerade,
        kurve = hinzufügenPlanWidgetKurve,
        links = hinzufügenPlanWidgetLinks,
        rechts = hinzufügenPlanWidgetRechts}
    -- Widget erstellen
    hBox <- liftIO $ boxPackWidgetNewDefault vBoxWeichen $ Gtk.hBoxNew False 0
    namePackNew hBox weiche
    richtungsButtonsPackNew weiche hBox
    fließendPackNew hBox weiche
    let weWidgets = WEWidgets {
        we = weiche,
        weWidget = hBox,
        weHinzWS = hinzufügenWegstreckeWidget,
        weHinzPL = hinzufügenPlanWidget}
    buttonEntfernenPackNew weWidgets $ entfernenWeiche $ zuZugtypEither weWidgets
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OWeiche $ zuZugtypEither weWidgets) tmvarStatus
    pure weWidgets
        where
            richtungsButtonsPackNew :: (MitBox b, MonadIO m, StatusReader r m, ObjektReader ObjektGui m) => Weiche z -> b -> m ()
            richtungsButtonsPackNew
                MärklinWeiche {wemRichtungsAnschlüsse}
                box
                    = do
                        tmvarStatus <- erhalteStatus
                        objektReader <- ask
                        forM_ wemRichtungsAnschlüsse $
                            \(richtung, anschluss) ->
                                boxPackWidgetNewDefault box $
                                    buttonNewWithEventLabel (showText richtung <:> showText anschluss) $
                                        flip runReaderT objektReader $
                                            ausführenTMVarAktion (Stellen weiche richtung) tmvarStatus
            richtungsButtonsPackNew
                LegoWeiche {welRichtungsAnschluss, welRichtungen = (richtung1, richtung2)}
                box
                    = void $ do
                        tmvarStatus <- erhalteStatus
                        objektReader <- ask
                        boxPackWidgetNewDefault box $ anschlussNew Language.richtung welRichtungsAnschluss
                        boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung1) $
                            flip runReaderT objektReader $
                                ausführenTMVarAktion (Stellen weiche richtung1) tmvarStatus
                        boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung2) $
                            flip runReaderT objektReader $
                                ausführenTMVarAktion (Stellen weiche richtung2) tmvarStatus

-- ** Kupplung
-- | 'Kupplung' mit zugehörigen Widgets
data KUWidgets
    = KUWidgets {
        ku :: Kupplung,
        kuWidget :: Gtk.HBox,
        kuHinzWS :: CheckButtonWegstreckeHinzufügen Void KUWidgets,
        kuHinzPL :: ButtonPlanHinzufügen KUWidgets}
    deriving (Eq)

instance MitWidget KUWidgets where
    erhalteWidget :: KUWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . kuWidget

instance WidgetsTyp KUWidgets where
    type ObjektTyp KUWidgets = Kupplung
    erhalteObjektTyp :: KUWidgets -> Kupplung
    erhalteObjektTyp = ku
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => KUWidgets -> m ()
    entferneWidgets kuWidgets = do
        DynamischeWidgets {vBoxKupplungen} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxKupplungen kuWidgets
        entferneHinzufügenWegstreckeWidgets kuWidgets
        entferneHinzufügenPlanWidgets kuWidgets
    boxButtonEntfernen :: KUWidgets -> Gtk.Box
    boxButtonEntfernen = erhalteBox . kuWidget

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
    anschlüsse :: KUWidgets -> [Anschluss]
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
kupplungPackNew :: (ObjektReader ObjektGui m, MonadIO m) =>
    Kupplung -> m KUWidgets
kupplungPackNew kupplung@Kupplung {kupplungsAnschluss} = do
    tmvarStatus <- erhalteStatus
    DynamischeWidgets {
        vBoxKupplungen,
        vBoxHinzufügenPlanKupplungen}
            <- erhalteDynamischeWidgets
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckePackNew kupplung
    hinzufügenPlanWidget <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanKupplungen kupplung
    -- Widget erstellen
    hBox <- liftIO $ boxPackWidgetNewDefault vBoxKupplungen $ Gtk.hBoxNew False 0
    namePackNew hBox kupplung
    boxPackWidgetNewDefault hBox $ anschlussNew Language.kupplung kupplungsAnschluss
    buttonKuppelnPackNew hBox kupplung
    fließendPackNew hBox kupplung
    let kuWidgets = KUWidgets {
        ku = kupplung,
        kuWidget = hBox,
        kuHinzPL = hinzufügenPlanWidget,
        kuHinzWS = hinzufügenWegstreckeWidget}
    buttonEntfernenPackNew kuWidgets $ entfernenKupplung kuWidgets
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OKupplung kuWidgets) tmvarStatus
    pure kuWidgets

-- | Füge 'Gtk.Button' zum kuppeln zur Box hinzu
buttonKuppelnPackNew :: (MitBox b, KupplungKlasse k, ObjektReader ObjektGui m, MonadIO m) =>
    b -> k -> m Gtk.Button
buttonKuppelnPackNew box kupplung = do
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    boxPackWidgetNewDefault box $ buttonNewWithEventLabel Language.kuppeln $
        flip runReaderT objektReader $
            ausführenTMVarAktion (Kuppeln kupplung) tmvarStatus

-- ** Wegstrecke
-- | 'Wegstrecke' mit zugehörigen Widgets
data WSWidgets (z :: Zugtyp)
    = WSWidgets {
        ws :: Wegstrecke z,
        wsWidget :: Gtk.Frame,
        wsFunktionBox :: Gtk.Box,
        wsHinzPL :: WegstreckePlanHinzufügenWidget z}
    deriving (Eq)

-- | Widget zum Hinzufügen einer 'Wegstrecke' zu einem 'Plan'
data WegstreckePlanHinzufügenWidget (z :: Zugtyp)
    = WegstreckePlanHinzufügenWidget {
        bahngeschwindigkeit :: Maybe (ButtonPlanHinzufügen (WSWidgets z)),
        streckenabschnitt :: Maybe (ButtonPlanHinzufügen (WSWidgets z)),
        kupplung :: Maybe (ButtonPlanHinzufügen (WSWidgets z)),
        wegstrecke :: Maybe (ButtonPlanHinzufügen (WSWidgets z))}
    deriving (Eq)

instance MitWidget (WSWidgets z) where
    erhalteWidget :: WSWidgets z -> Gtk.Widget
    erhalteWidget = erhalteWidget . wsWidget

instance (PlanElement (WSWidgets z)) => WidgetsTyp (WSWidgets z) where
    type ObjektTyp (WSWidgets z) = Wegstrecke z
    erhalteObjektTyp :: WSWidgets z -> Wegstrecke z
    erhalteObjektTyp = ws
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => WSWidgets z -> m ()
    entferneWidgets wsWidgets = do
        DynamischeWidgets {vBoxWegstrecken} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxWegstrecken wsWidgets
        entferneHinzufügenPlanWidgets wsWidgets
    boxButtonEntfernen :: WSWidgets z -> Gtk.Box
    boxButtonEntfernen = wsFunktionBox

instance WidgetsTyp (ZugtypEither WSWidgets) where
    type ObjektTyp (ZugtypEither WSWidgets) = ZugtypEither Wegstrecke
    erhalteObjektTyp :: ZugtypEither WSWidgets -> ZugtypEither Wegstrecke
    erhalteObjektTyp = mapZugtypEither ws
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => ZugtypEither WSWidgets -> m ()
    entferneWidgets (ZugtypMärklin wsWidgets)   = entferneWidgets wsWidgets
    entferneWidgets (ZugtypLego wsWidgets)      = entferneWidgets wsWidgets
    boxButtonEntfernen :: ZugtypEither WSWidgets -> Gtk.Box
    boxButtonEntfernen  (ZugtypMärklin wsWidgets)   = boxButtonEntfernen wsWidgets
    boxButtonEntfernen  (ZugtypLego wsWidgets)      = boxButtonEntfernen wsWidgets

instance PlanElement (WSWidgets 'Märklin) where
    foldPlan :: Lens.Fold (WSWidgets 'Märklin) (Maybe (ButtonPlanHinzufügen (WSWidgets 'Märklin)))
    foldPlan = Lens.folding $ (??) [bahngeschwindigkeit, streckenabschnitt, kupplung, wegstrecke] . wsHinzPL
    boxenPlan :: Wegstrecke 'Märklin -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WSWidgets 'Märklin))
    boxenPlan _wsWidgets = Lens.folding $ (??) [
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
        vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
        vBoxHinzufügenPlanWegstreckenKupplungMärklin,
        vBoxHinzufügenPlanWegstreckenMärklin]
instance PlanElement (WSWidgets 'Lego) where
    foldPlan :: Lens.Fold (WSWidgets 'Lego) (Maybe (ButtonPlanHinzufügen (WSWidgets 'Lego)))
    foldPlan = Lens.folding $ (??) [bahngeschwindigkeit, streckenabschnitt, kupplung, wegstrecke] . wsHinzPL
    boxenPlan :: Wegstrecke 'Lego -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WSWidgets 'Lego))
    boxenPlan _wsWidgets = Lens.folding $ (??) [
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
        vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
        vBoxHinzufügenPlanWegstreckenKupplungLego,
        vBoxHinzufügenPlanWegstreckenLego]
instance PlanElement (ZugtypEither WSWidgets) where
    foldPlan :: Lens.Fold (ZugtypEither WSWidgets) (Maybe (ButtonPlanHinzufügen (ZugtypEither WSWidgets)))
    foldPlan = Lens.folding $ ausZugtypEither $
        map (fmap widgetHinzufügenZugtypEither) .
        (??) [bahngeschwindigkeit, streckenabschnitt, kupplung, wegstrecke] .
        wsHinzPL
    boxenPlan :: ZugtypEither Wegstrecke -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither WSWidgets))
    boxenPlan   (ZugtypMärklin _wegstrecke) = Lens.folding $ map widgetHinzufügenZugtypEither . (??) [
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
        vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
        vBoxHinzufügenPlanWegstreckenKupplungMärklin,
        vBoxHinzufügenPlanWegstreckenMärklin]
    boxenPlan   (ZugtypLego _wegstrecke)    = Lens.folding $ map widgetHinzufügenZugtypEither . (??) [
        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
        vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
        vBoxHinzufügenPlanWegstreckenKupplungLego,
        vBoxHinzufügenPlanWegstreckenLego]

instance StreckenObjekt (WSWidgets z) where
    anschlüsse :: WSWidgets z -> [Anschluss]
    anschlüsse WSWidgets {ws} = anschlüsse ws
    erhalteName :: WSWidgets z -> Text
    erhalteName WSWidgets {ws} = erhalteName ws

instance Aeson.ToJSON (WSWidgets z) where
    toJSON :: WSWidgets z -> Aeson.Value
    toJSON WSWidgets {ws} = Aeson.toJSON ws

instance BahngeschwindigkeitKlasse WSWidgets where
    geschwindigkeit :: (PwmReader r m, MonadIO m) => WSWidgets z -> Natural -> m ()
    geschwindigkeit WSWidgets {ws} = geschwindigkeit ws
    umdrehen :: (PwmReader r m, MonadIO m) => WSWidgets 'Märklin -> m ()
    umdrehen WSWidgets {ws} = umdrehen ws
    fahrtrichtungEinstellen :: (PwmReader r m, MonadIO m) => WSWidgets 'Lego -> Fahrtrichtung -> m ()
    fahrtrichtungEinstellen WSWidgets {ws} = fahrtrichtungEinstellen ws

instance StreckenabschnittKlasse (WSWidgets z) where
    strom :: (I2CReader r m, MonadIO m) => WSWidgets z -> Strom -> m ()
    strom WSWidgets {ws} = strom ws

instance KupplungKlasse (WSWidgets z) where
    kuppeln :: (I2CReader r m, MonadIO m) => WSWidgets z -> m ()
    kuppeln WSWidgets {ws} = kuppeln ws

instance WegstreckeKlasse (WSWidgets z) where
    einstellen :: (PwmReader r m, MonadIO m) => WSWidgets z -> m ()
    einstellen WSWidgets {ws} = einstellen ws

-- | 'Wegstrecke' darstellen
wegstreckePackNew :: (ObjektReader ObjektGui m, MonadIO m, ZugtypKlasse z, PlanElement (WSWidgets z)) =>
    Wegstrecke z -> m (WSWidgets z)
wegstreckePackNew
    wegstrecke@Wegstrecke {
        wsBahngeschwindigkeiten,
        wsStreckenabschnitte,
        wsWeichenRichtungen,
        wsKupplungen}
            = do
                tmvarStatus <- erhalteStatus
                objektReader <- ask
                dynamischeWidgets@DynamischeWidgets {vBoxWegstrecken} <- erhalteDynamischeWidgets
                -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
                let
                    [boxBahngeschwindigkeit, boxStreckenabschnitt, boxKupplung, boxWegstrecke]
                        = dynamischeWidgets ^.. boxenPlan wegstrecke
                hinzufügenPlanWidgetBG  <- if null wsBahngeschwindigkeiten
                    then pure Nothing
                    else Just <$> hinzufügenWidgetPlanPackNew
                        boxBahngeschwindigkeit
                        wegstrecke
                hinzufügenPlanWidgetST <- if null wsStreckenabschnitte
                    then pure Nothing
                    else Just <$> hinzufügenWidgetPlanPackNew
                        boxStreckenabschnitt
                        wegstrecke
                hinzufügenPlanWidgetKU <- if null wsKupplungen
                    then pure Nothing
                    else Just <$> hinzufügenWidgetPlanPackNew
                        boxKupplung
                        wegstrecke
                hinzufügenPlanWidgetWS <- if null wsWeichenRichtungen
                    then pure Nothing
                    else Just <$> hinzufügenWidgetPlanPackNew
                        boxWegstrecke
                        wegstrecke
                let hinzufügenPlanWidget = WegstreckePlanHinzufügenWidget {
                    bahngeschwindigkeit = hinzufügenPlanWidgetBG,
                    streckenabschnitt = hinzufügenPlanWidgetST,
                    kupplung = hinzufügenPlanWidgetKU,
                    wegstrecke = hinzufügenPlanWidgetWS}
                -- Widget erstellen
                frame <- liftIO $ boxPackWidgetNewDefault vBoxWegstrecken Gtk.frameNew
                vBox <- liftIO $ containerAddWidgetNew frame $ Gtk.vBoxNew False 0
                namePackNew vBox wegstrecke
                expander <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.expanderNew (Language.wegstreckenElemente :: Text)
                vBoxExpander <- liftIO $ containerAddWidgetNew expander $ Gtk.vBoxNew False 0
                functionBox <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
                unless (null wsBahngeschwindigkeiten) $ void $ do
                    liftIO $ boxPackWidgetNewDefault vBoxExpander $ Gtk.labelNew $ Just $
                        Language.bahngeschwindigkeiten <:> foldl appendName "" wsBahngeschwindigkeiten
                    hScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew functionBox wegstrecke
                    case zuZugtypEither wegstrecke of
                        (ZugtypMärklin wsMärklin)
                            -> void $ buttonUmdrehenPackNew
                                functionBox
                                wsMärklin
                                hScaleGeschwindigkeit
                        (ZugtypLego wsLego)
                            -> void $ togglebuttonFahrtrichtungEinstellenPackNew
                                functionBox
                                wsLego
                                hScaleGeschwindigkeit
                unless (null wsStreckenabschnitte) $ void $ do
                    liftIO $ boxPackWidgetNewDefault vBoxExpander $ Gtk.labelNew $ Just $
                        Language.streckenabschnitte <:> foldl appendName "" wsStreckenabschnitte
                    toggleButtonStromPackNew functionBox wegstrecke
                unless (null wsWeichenRichtungen) $ void $ do
                    liftIO $ boxPackWidgetNewDefault vBoxExpander $ Gtk.labelNew $ Just $
                        Language.weichen <:>
                        foldl (\acc (weiche, richtung) -> appendName acc weiche <°> showText richtung) "" wsWeichenRichtungen
                    boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.einstellen $
                        flip runReaderT objektReader $
                            ausführenTMVarAktion (Einstellen wegstrecke) tmvarStatus
                unless (null wsKupplungen) $ void $ do
                    liftIO $ boxPackWidgetNewDefault vBoxExpander $ Gtk.labelNew $ Just $
                        Language.kupplungen <:> foldl appendName "" wsKupplungen
                    buttonKuppelnPackNew functionBox wegstrecke
                let wsWidgets = WSWidgets {
                        ws = wegstrecke,
                        wsWidget = frame,
                        wsFunktionBox = erhalteBox functionBox,
                        wsHinzPL = hinzufügenPlanWidget}
                buttonEntfernenPackNew wsWidgets $ entfernenWegstrecke $ zuZugtypEither wsWidgets
                -- Widgets merken
                ausführenTMVarBefehl (Hinzufügen $ OWegstrecke $ zuZugtypEither wsWidgets) tmvarStatus
                pure wsWidgets
    where
        appendName :: (StreckenObjekt o) => Text -> o -> Text
        appendName ""       objekt = erhalteName objekt
        appendName string   objekt = string <^> erhalteName objekt

-- ** Plan
-- | 'Plan' mit zugehörigen Widgets
data PLWidgets
    = PLWidgets {
        pl :: Plan,
        plWidget :: Gtk.Frame,
        plFunktionBox :: Gtk.Box,
        plHinzPL :: ButtonPlanHinzufügen PLWidgets}
    deriving (Eq)

instance MitWidget PLWidgets where
    erhalteWidget :: PLWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . plWidget

instance WidgetsTyp PLWidgets where
    type ObjektTyp PLWidgets = Plan
    erhalteObjektTyp :: PLWidgets -> Plan
    erhalteObjektTyp = pl
    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => PLWidgets -> m ()
    entferneWidgets plWidgets = do
        DynamischeWidgets {vBoxPläne} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxPläne plWidgets
    boxButtonEntfernen :: PLWidgets -> Gtk.Box
    boxButtonEntfernen = plFunktionBox
    
instance PlanElement PLWidgets where
    foldPlan :: Lens.Fold PLWidgets (Maybe (ButtonPlanHinzufügen PLWidgets))
    foldPlan = Lens.to $ Just . plHinzPL
    boxenPlan :: Plan -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen PLWidgets)
    boxenPlan _kuWidgets = Lens.to vBoxHinzufügenPlanPläne

instance StreckenObjekt PLWidgets where
    anschlüsse :: PLWidgets -> [Anschluss]
    anschlüsse PLWidgets {pl} = anschlüsse pl
    erhalteName :: PLWidgets -> Text
    erhalteName PLWidgets {pl} = erhalteName pl

instance Aeson.ToJSON PLWidgets where
    toJSON :: PLWidgets -> Aeson.Value
    toJSON (PLWidgets {pl}) = Aeson.toJSON pl

instance PlanKlasse PLWidgets where
    ausführenPlan :: (AusführendReader r m, MonadIO m) => PLWidgets -> (Natural -> IO ()) -> IO () -> m ()
    ausführenPlan PLWidgets {pl} = ausführenPlan pl

-- | 'Plan' darstellen
planPackNew :: (ObjektReader ObjektGui m, MonadIO m) => Plan -> m PLWidgets
planPackNew plan@Plan {plAktionen} = do
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    DynamischeWidgets {vBoxPläne, progressBarPlan, windowMain, vBoxHinzufügenPlanPläne} <- erhalteDynamischeWidgets
    -- Widget erstellen
    frame <- liftIO $ boxPackWidgetNewDefault vBoxPläne $ Gtk.frameNew
    vBox <- liftIO $ containerAddWidgetNew frame $ Gtk.vBoxNew False 0
    namePackNew vBox plan
    expander <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.expanderNew $ Language.aktionen <:> show (length plAktionen)
    vBoxExpander <- liftIO $ containerAddWidgetNew expander $ Gtk.vBoxNew False 0
    liftIO $ forM_ plAktionen $ boxPackWidgetNewDefault vBoxExpander . Gtk.labelNew . Just . show
    functionBox <- liftIO $ boxPackWidgetNewDefault vBox Gtk.hButtonBoxNew
    buttonAusführen <- liftIO $ boxPackWidgetNewDefault functionBox $ Gtk.buttonNewWithLabel (Language.ausführen :: Text)
    buttonAbbrechen <- liftIO $ boxPackWidgetNewDefault functionBox $ Gtk.buttonNewWithLabel (Language.ausführenAbbrechen :: Text)
    liftIO $ Gtk.widgetHide buttonAbbrechen
    dialogGesperrt <- liftIO $ Gtk.messageDialogNew (Just windowMain) [] Gtk.MessageError Gtk.ButtonsOk ("" :: Text)
    liftIO $ Gtk.set dialogGesperrt [Gtk.windowTitle := (Language.aktionGesperrt :: Text)]
    liftIO $ Gtk.on buttonAusführen Gtk.buttonActivated $ flip runReaderT objektReader $
        auswertenTMVarIOStatus (ausführenMöglich plan) tmvarStatus >>= \case
            AusführenMöglich                -> void $ do
                liftIO $ do
                    Gtk.widgetHide buttonAusführen
                    Gtk.widgetShow buttonAbbrechen
                ausführenTMVarBefehl (Ausführen plan anzeigeAktion abschlussAktion) tmvarStatus
                where
                    anzeigeAktion :: Natural -> IO ()
                    anzeigeAktion wert = Gtk.set progressBarPlan [
                        Gtk.progressBarFraction := (fromIntegral wert) / (fromIntegral $ length plAktionen)]
                    abschlussAktion :: IO ()
                    abschlussAktion = do
                        Gtk.widgetShow buttonAusführen
                        Gtk.widgetHide buttonAbbrechen
            WirdAusgeführt                  -> error "Ausführen in GTK-UI erneut gestartet."
            (AnschlüsseBelegt anschlüsse)   -> liftIO $ do
                Gtk.set dialogGesperrt 
                    [Gtk.messageDialogText := Just (Language.ausführenGesperrt $ show $ ausFoldable anschlüsse)]
                void $ dialogEval dialogGesperrt
    liftIO $ Gtk.on buttonAbbrechen Gtk.buttonActivated $ do
        flip runReaderT objektReader $
            ausführenTMVarBefehl (AusführenAbbrechen plan) tmvarStatus
        Gtk.widgetShow buttonAusführen
        Gtk.widgetHide buttonAbbrechen
    plHinzPL <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanPläne plan
    let plWidgets = PLWidgets {pl = plan, plWidget = frame, plFunktionBox = erhalteBox functionBox, plHinzPL}
    buttonEntfernenPackNew plWidgets $ entfernenPlan plWidgets
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OPlan plWidgets) tmvarStatus
    pure plWidgets
#endif