{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

{-|
Description : Erstelle zusammengesetzte Widgets.

Allgemeine Hilfsfunktionen zum erstellen neuer Widgets
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Widgets () where
#else
module Zug.UI.Gtk.Widgets (
    -- * Allgemeine Widget-Funktionen
    widgetShowNew, containerAddWidgetNew, boxPackWidgetNew, notebookAppendPageNew, containerRemoveJust, widgetShowIf,
    dialogEval, boxPack, boxPackDefault, boxPackWidgetNewDefault,
    Packing(..), packingDefault, Padding(..), paddingDefault, positionDefault, Position(..),
    -- *`* Scrollbare Widgets
    scrolledWidgetNew, scrolledWidgetPackNew, scrolledWidgetAddNew, scrolledWidgedNotebookAppendPageNew,
    -- ** Knopf erstellen
    buttonNewWithEvent, buttonNewWithEventLabel, buttonNewWithEventMnemonic,
    -- ** Pin darstellen
    pinLabelNew, pinSpinBoxNew, nameEntryPackNew,
    -- ** Spezifisches StreckenObjekt darstellen
    bahngeschwindigkeitPackNew, streckenabschnittPackNew, weichePackNew, kupplungPackNew, wegstreckePackNew, planPackNew,
    BahngeschwindigkeitWidget, StreckenabschnittWidget, WeicheWidget, KupplungWidget, WegstreckeWidget, PlanWidget,
    BahngeschwindigkeitWidgetHinzufügenWegstrecke, StreckenabschnittWidgetHinzufügenWegstrecke,
    WeicheWidgetHinzufügenWegstrecke, KupplungWidgetHinzufügenWegstrecke,
    BahngeschwindigkeitWidgetHinzufügenPlan, StreckenabschnittWidgetHinzufügenPlan,
    WeicheWidgetHinzufügenPlan, KupplungWidgetHinzufügenPlan, WegstreckeWidgetHinzufügenPlan,
    -- * Verwaltung des aktuellen Zustands
    DynamischeWidgets(..), WidgetReader(..), StatusGui, StatusReader(..), ObjektGui, BefehlGui, IOStatusGui, MStatusGui,
    MStatusGuiT, BGWidgets(), STWidgets(), WEWidgets(), KUWidgets(), WSWidgets(), PLWidgets(),
    traversalHinzufügenWegstrecke, WegstreckenElement(..), getterRichtungsRadioButtons,
    PlanElement(..), entferneHinzufügenPlanWidgets) where

-- Bibliotheken
import Control.Applicative (ZipList(..))
import Control.Concurrent.STM (atomically, TMVar, putTMVar, TVar)
import Control.Lens (Traversal', Lens', Getter, Fold, (%%~), (^.), (^..), (.~), Field1(..), Field2(..), Field3(..))
import qualified Control.Lens as Lens
import Control.Monad (void, unless)
import Control.Monad.Reader (MonadReader(), asks, runReaderT)
import Control.Monad.State (State, StateT)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.Aeson as Aeson
import Data.Foldable (Foldable(..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (StreckenObjekt(..), StreckenAtom(..), Anschluss(), PwmReader(), I2CReader(),
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..),
                    Streckenabschnitt(..), StreckenabschnittKlasse(..),
                    Weiche(..), WeicheKlasse(..),
                    Kupplung(..), KupplungKlasse(..),
                    Wegstrecke(..), WegstreckeKlasse(..))
import Zug.Klassen (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), mapZugtypEither, Fahrtrichtung(..), Strom(..), Richtung(..))
import qualified Zug.Language as Language
import Zug.Language ((<^>), (<->), (<:>), (<°>), addMnemonic, showText)
import Zug.Menge (Menge, ausFoldable)
import Zug.Objekt (ObjektAllgemein(..), ObjektElement(..), Objekt)
import Zug.Plan (PlanKlasse(..), Plan(..), Ausführend(), AusführendReader(),
                AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionWeiche(..),
                AktionKupplung(..), AktionWegstrecke(..))
import Zug.UI.Base (StatusAllgemein(..), IOStatusAllgemein, MStatusAllgemein, MStatusAllgemeinT,
                    AusführenMöglich(..), ReaderFamilie,
                    TVarMaps(..), TVarMapsReader(..), MitTVarMaps(..),
                    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, wegstrecken, pläne,
                    auswertenTMVarIOStatus, ausführenMöglich, entfernenBahngeschwindigkeit,
                    entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung,
                    entfernenWegstrecke, entfernenPlan)
import Zug.UI.Befehl (BefehlAllgemein(..), ausführenTMVarBefehl, ausführenTMVarAktion)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggled, FortfahrenWennToggledTMVar, registrierterCheckButtonNew,
                                        RegistrierterCheckButton, MitRegistrierterCheckButton(..))
import Zug.UI.Gtk.Hilfsfunktionen (containerRemoveJust, packingDefault, paddingDefault, positionDefault,
                                    boxPackWidgetNew, boxPackWidgetNewDefault, Packing(..), Padding(..), Position(..),
                                    buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), mitContainerRemove, MitBox(..), MitRange(..),
                                    MitButton(), MitCheckButton(), MitToggleButton())
import Zug.UI.Gtk.Widgets.Anschluss (anschlussNew)
import Zug.UI.Gtk.Widgets.Auswahl (AuswahlWidget, auswahlRadioButtonNew, aktuelleAuswahl)
import Zug.UI.Gtk.Widgets.ScrollbaresWidget ()

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

-- | Box zur Auswahl der 'Wegstrecke'n-Elemente
newtype WidgetWegstreckeHinzufügen w a
    = WidgetWegstreckeHinzufügen {widgetWegstreckeHinzufügen :: w}
        deriving (Eq, MitWidget, MitContainer, MitBox, MitButton, MitToggleButton, MitCheckButton)

type BoxWegstreckeHinzufügen a = WidgetWegstreckeHinzufügen Gtk.VBox a
type CheckButtonWegstreckeHinzufügen a = WidgetWegstreckeHinzufügen WegstreckeCheckButton a

-- | 'RegistrierterCheckButton', potentiell mit zusätlicher Richtungsauswahl
data WegstreckeCheckButton
    = WegstreckeCheckButton {
        wcbRegistrierterCheckButton :: RegistrierterCheckButton}
    | WegstreckeCheckButtonRichtung {
        wcbWidget :: Gtk.Widget,
        wcbRegistrierterCheckButton :: RegistrierterCheckButton,
        wcbRichtungsAuswahl :: AuswahlWidget Richtung}
    deriving (Eq)

instance MitWidget WegstreckeCheckButton where
    erhalteWidget :: WegstreckeCheckButton -> Gtk.Widget
    erhalteWidget   WegstreckeCheckButton {wcbRegistrierterCheckButton} = erhalteWidget wcbRegistrierterCheckButton
    erhalteWidget   WegstreckeCheckButtonRichtung {wcbWidget}           = wcbWidget

instance MitRegistrierterCheckButton WegstreckeCheckButton where
    erhalteRegistrierterCheckButton :: WegstreckeCheckButton -> RegistrierterCheckButton
    erhalteRegistrierterCheckButton = wcbRegistrierterCheckButton

-- | Box zur Auswahl der 'Plan'-Aktionen
newtype WidgetPlanHinzufügen w a
    = WidgetPlanHinzufügen {widgetPlanHinzufügen :: w}
        deriving (Eq, MitWidget, MitContainer, MitBox, MitButton, MitToggleButton)

type BoxPlanHinzufügen a = WidgetPlanHinzufügen Gtk.VBox a
type ButtonPlanHinzufügen a = WidgetPlanHinzufügen Gtk.Button a

-- | Sammlung aller Widgets, welche während der Laufzeit benötigt werden.
data DynamischeWidgets = DynamischeWidgets {
    vBoxBahngeschwindigkeiten :: Gtk.VBox,
    vBoxStreckenabschnitte :: Gtk.VBox,
    vBoxWeichen :: Gtk.VBox,
    vBoxKupplungen :: Gtk.VBox,
    vBoxWegstrecken :: Gtk.VBox,
    vBoxPläne :: Gtk.VBox,
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
    progressBarPlan :: Gtk.ProgressBar,
    windowMain :: Gtk.Window,
    fortfahrenWennToggledWegstrecke :: FortfahrenWennToggledTMVar StatusGui WegstreckeCheckButton,
    tmvarPlanObjekt :: TMVar (Maybe Objekt)}

-- | Klasse für Typen mit 'DynamischeWidgets'
class MitDynamischeWidgets r where
    dynamischeWidgets :: r -> DynamischeWidgets
-- | Abkürzung für Funktionen, die 'DynamischeWidgets' benötigen
class (MonadReader r m) => DynamischeWidgetsReader r m where
    erhalteDynamischeWidgets :: m DynamischeWidgets
instance (MonadReader r m, MitDynamischeWidgets r) => DynamischeWidgetsReader r m where
    erhalteDynamischeWidgets :: m DynamischeWidgets
    erhalteDynamischeWidgets = asks dynamischeWidgets

-- | Abkürzung für Funktionen, die den aktuell in einer 'TMVar' gespeicherten 'StatusAllgemein' benötigen.
class (MonadReader r m) => StatusReader r m where
    erhalteStatus :: m (TMVar StatusGui)

type instance ReaderFamilie ObjektGui = (DynamischeWidgets, TVarMaps)

instance MitTVarMaps (DynamischeWidgets, TVarMaps) where
    tvarMaps :: (DynamischeWidgets, TVarMaps) -> TVarMaps
    tvarMaps = snd
instance MitDynamischeWidgets (DynamischeWidgets, TVarMaps) where
    dynamischeWidgets :: (DynamischeWidgets, TVarMaps) -> DynamischeWidgets
    dynamischeWidgets = fst

-- | Klasse für Gui-Darstellung von Typen, die zur Erstellung einer 'Wegstrecke' verwendet werden.
class WegstreckenElement s where
    -- | Linse auf 'RegistrierterCheckButton', ob 'StreckenObjekt' zu einer 'Wegstrecke' hinzugefügt werden soll
    getterWegstrecke :: Getter s (CheckButtonWegstreckeHinzufügen s)
    -- | Assoziierte 'BoxWegstreckeHinzufügen', in der 'MitRegistrierterCheckButton' gepackt ist.
    boxWegstrecke :: s -> Getter DynamischeWidgets (BoxWegstreckeHinzufügen s)

-- | Entferne 'Widget's zum Hinzufügen zu einer 'Wegstrecke' aus der entsprechenden Box
entferneHinzufügenWegstreckeWidgets :: (WegstreckenElement s, DynamischeWidgetsReader r m, MonadIO m) => s -> m ()
entferneHinzufügenWegstreckeWidgets wegsteckenElement = do
    box <- Lens.view (boxWegstrecke wegsteckenElement) <$> erhalteDynamischeWidgets
    mitContainerRemove box $ wegsteckenElement ^. getterWegstrecke

instance (WegstreckenElement (s 'Märklin), WegstreckenElement (s 'Lego)) => WegstreckenElement (ZugtypEither s) where
    getterWegstrecke :: Getter (ZugtypEither s) (CheckButtonWegstreckeHinzufügen (ZugtypEither s))
    getterWegstrecke = Lens.to vonFunktion
        where
            vonFunktion :: (WegstreckenElement (s 'Märklin), WegstreckenElement (s 'Lego)) =>
                ZugtypEither s -> (CheckButtonWegstreckeHinzufügen (ZugtypEither s))
            vonFunktion
                (ZugtypMärklin s)
                    = WidgetWegstreckeHinzufügen $ widgetWegstreckeHinzufügen $ s ^. getterWegstrecke
            vonFunktion
                (ZugtypLego s)
                    = WidgetWegstreckeHinzufügen $ widgetWegstreckeHinzufügen $ s ^. getterWegstrecke
    boxWegstrecke :: ZugtypEither s -> Getter DynamischeWidgets (BoxWegstreckeHinzufügen (ZugtypEither s))
    boxWegstrecke   (ZugtypMärklin s)   = boxWegstrecke s . Lens.to (WidgetWegstreckeHinzufügen . widgetWegstreckeHinzufügen)
    boxWegstrecke   (ZugtypLego s)      = boxWegstrecke s . Lens.to (WidgetWegstreckeHinzufügen . widgetWegstreckeHinzufügen)

-- | Klasse für Gui-Darstellungen von Typen, die zur Erstellung eines 'Plan's verwendet werden.
class PlanElement s where
    -- | Faltung auf 'Gtk.Button's (falls vorhanden), welches 'StreckenObjekt' für eine 'Aktion' verwendet werden soll
    foldPlan :: Fold s (Maybe (ButtonPlanHinzufügen s))
    -- | Aller assoziierten 'BoxPlanHinzufügen', in denen jeweiliger 'ButtonPlanHinzufügen' gepackt ist.
    -- Die Reihenfolge muss zum Ergebnis von 'foldPlan' passen.
    -- Wird für 'entferneHinzufügenPlanWidgets' benötigt.
    boxenPlan :: s -> Fold DynamischeWidgets (BoxPlanHinzufügen s)

-- | Entferne 'Widget's zum 'Plan' erstellen aus den entsprechenden 'Box'en.
entferneHinzufügenPlanWidgets :: (PlanElement s, DynamischeWidgetsReader r m, MonadIO m) => s -> m ()
entferneHinzufügenPlanWidgets planElement = do
    boxenPlan <- Lens.toListOf (boxenPlan planElement) <$> erhalteDynamischeWidgets
    sequence_ $ containerRemoveJust <$> ZipList boxenPlan <*> ZipList (planElement ^.. foldPlan)

instance (PlanElement (s 'Märklin), PlanElement (s 'Lego)) => PlanElement (ZugtypEither s) where
    foldPlan :: Fold (ZugtypEither s) (Maybe (ButtonPlanHinzufügen (ZugtypEither s)))
    foldPlan = Lens.folding erhalteListe
        where
            erhalteListe :: (PlanElement (s 'Märklin), PlanElement (s 'Lego)) =>
                ZugtypEither s -> [Maybe (ButtonPlanHinzufügen (ZugtypEither s))]
            erhalteListe
                (ZugtypMärklin s)
                    = fmap (WidgetPlanHinzufügen . widgetPlanHinzufügen) <$> Lens.toListOf foldPlan s
            erhalteListe
                (ZugtypLego s)
                    = fmap (WidgetPlanHinzufügen . widgetPlanHinzufügen) <$> Lens.toListOf foldPlan s
    boxenPlan :: ZugtypEither s -> Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither s))
    boxenPlan = Lens.folding . erhalteListe
        where
            erhalteListe :: (PlanElement (s 'Märklin), PlanElement (s 'Lego)) =>
                ZugtypEither s -> DynamischeWidgets -> [BoxPlanHinzufügen (ZugtypEither s)]
            erhalteListe
                (ZugtypMärklin s)
                    = fmap (WidgetPlanHinzufügen . widgetPlanHinzufügen) . Lens.toListOf (boxenPlan s)
            erhalteListe
                (ZugtypLego s)
                    = fmap (WidgetPlanHinzufügen . widgetPlanHinzufügen) . Lens.toListOf (boxenPlan s)

-- type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s
-- | 'Traversal'' über alle 'CheckButton's zum Hinzufügen einer 'Wegstrecke'
traversalHinzufügenWegstrecke :: Traversal' StatusGui RegistrierterCheckButton
traversalHinzufügenWegstrecke f status = Status <$>
    traverseList f (status ^. bahngeschwindigkeiten) <*>
    traverseList f (status ^. streckenabschnitte) <*>
    traverseList f (status ^. weichen) <*>
    traverseList f (status ^. kupplungen) <*>
    pure (status ^. wegstrecken) <*>
    pure (status ^. pläne)
        where
            traverseList :: (Applicative f, WegstreckenElement s) =>
                (RegistrierterCheckButton -> f RegistrierterCheckButton) -> [s] -> f [s]
            traverseList f list = traverse . getterWegstrecke %%~ f $ list

-- | Entfernen-Knopf zu 'Box' hinzufügen. Beim drücken werden /removeActionGui/ und /removeAction/ ausgeführt.
buttonEntfernenPack :: (MitBox b, StatusReader r m, TVarMapsReader r m, DynamischeWidgetsReader r m, MonadIO m) =>
    b -> IO () -> IOStatusGui () -> m Gtk.Button
buttonEntfernenPack box removeActionGui removeAction = do
    tvarMaps <- erhalteTVarMaps
    dynamischeWidgets <- erhalteDynamischeWidgets
    tmvarStatus <- erhalteStatus
    liftIO $ boxPackWidgetNew box PackNatural paddingDefault End $
        buttonNewWithEventLabel Language.entfernen $ do
            runReaderT (auswertenTMVarIOStatus removeAction tmvarStatus) (dynamischeWidgets, tvarMaps)
            removeActionGui

-- | Entfernen-Knopf zu Box hinzufügen. Beim drücken wird /parent/ aus der /box/ entfernt und die 'IOStatusGui'-Aktion ausgeführt.
buttonEntfernenPackSimple ::
    (MitBox b, MitContainer c, StatusReader r m, TVarMapsReader r m, DynamischeWidgetsReader r m, MonadIO m) =>
        b -> c -> IOStatusGui () -> m Gtk.Button
buttonEntfernenPackSimple box parent = buttonEntfernenPack box $ mitContainerRemove parent box

-- ** Widget mit Name und CheckButton erstellen
-- | Füge einen 'RegistrierterCheckButton' mit einem 'Label' für den Namen zur Box hinzu.
hinzufügenWidgetWegstreckeNew ::
    (StreckenObjekt o, MonadIO m) =>
        o -> Maybe (NonEmpty Richtung) -> FortfahrenWennToggledTMVar StatusGui WegstreckeCheckButton -> m WegstreckeCheckButton
hinzufügenWidgetWegstreckeNew
    objekt
    Nothing
    fortfahrenWennToggled
        = liftIO $ do
            wcbRegistrierterCheckButton <- registrierterCheckButtonNew (erhalteName objekt) fortfahrenWennToggled
            pure WegstreckeCheckButton {wcbRegistrierterCheckButton}
hinzufügenWidgetWegstreckeNew
    objekt
    (Just richtungen)
    fortfahrenWennToggled
        = liftIO $ do
            hBox <- Gtk.hBoxNew False 0
            wcbRegistrierterCheckButton <- boxPackWidgetNewDefault hBox $
                registrierterCheckButtonNew (erhalteName objekt) fortfahrenWennToggled
            wcbRichtungsAuswahl <- boxPackWidgetNewDefault hBox $ auswahlRadioButtonNew richtungen
            pure WegstreckeCheckButtonRichtung {wcbRegistrierterCheckButton, wcbRichtungsAuswahl}

-- | Füge einen Knopf mit dem Namen zur Box hinzu. Beim drücken wird die 'TMVar' mit dem Objekt gefüllt.
hinzufügenWidgetPlanNew :: (DynamischeWidgetsReader r m, MitWidget o, StreckenObjekt o, ObjektElement o, MonadIO m) =>
    BoxPlanHinzufügen o -> o -> m Gtk.Button
hinzufügenWidgetPlanNew box objekt = do
    DynamischeWidgets {tmvarPlanObjekt} <- erhalteDynamischeWidgets
    boxPackWidgetNewDefault box $ buttonNewWithEventLabel (erhalteName objekt) $
        atomically $ putTMVar tmvarPlanObjekt $ Just $ zuObjekt objekt

-- * Darstellung von Streckenobjekten
-- | 'Bahngeschwindigkeit' darstellen und zum Status hinzufügen
bahngeschwindigkeitPackNew ::
    (StatusReader r m, DynamischeWidgetsReader r m, MonadIO m) =>
        Bahngeschwindigkeit z -> m (BGWidgets z)
bahngeschwindigkeitPackNew bahngeschwindigkeit = do
    tmvarStatus <- erhalteStatus
    dynamischeWidgets@DynamischeWidgets {
        vBoxBahngeschwindigkeiten,
        vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin,
        vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego,
        vBoxHinzufügenPlanBahngeschwindigkeitenLego,
        vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
        fortfahrenWennToggledWegstrecke,
        tmvarPlanObjekt}
            <- erhalteDynamischeWidgets
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidgetMärklin <- hinzufügenWidgetWegstreckeNew bahngeschwindigkeit vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin fortfahrenWennToggledWegstrecke
    hinzufügenWegstreckeWidgetLego <- hinzufügenWidgetWegstreckeNew bahngeschwindigkeit vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew vBoxHinzufügenPlanBahngeschwindigkeiten (OBahngeschwindigkeit bahngeschwindigkeit) tmvarPlanObjekt
    hinzufügenPlanWidgetZT <- case zugtyp bahngeschwindigkeit of
        Lego
            -> Left <$> hinzufügenWidgetPlanNew vBoxHinzufügenPlanBahngeschwindigkeitenLego (OBahngeschwindigkeit bahngeschwindigkeit) tmvarPlanObjekt
        Märklin
            -> Right <$> hinzufügenWidgetPlanNew vBoxHinzufügenPlanBahngeschwindigkeitenMärklin (OBahngeschwindigkeit bahngeschwindigkeit) tmvarPlanObjekt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxBahngeschwindigkeiten $ hBoxNew False 0
    nameLabelPackNew hBox bahngeschwindigkeit
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.geschwindigkeit $ getGeschwindigkeitsPin bahngeschwindigkeit
    hScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew hBox bahngeschwindigkeit tmvarStatus
    fahrtrichtungsPinLabelPackNew hBox bahngeschwindigkeit
    buttonUmdrehenPackNew hBox bahngeschwindigkeit hScaleGeschwindigkeit tmvarStatus
    labelFließendValuePackNew hBox bahngeschwindigkeit
    let bgWidgets = BGWidgets {bg=bahngeschwindigkeit, bgWidget=hBox, bgHinzPL=(hinzufügenPlanWidget, hinzufügenPlanWidgetZT), bgHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxBahngeschwindigkeiten (entfernenBahngeschwindigkeit bgWidgets >> liftIO (boxWegstrecke bgWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets bgWidgets dynamischeWidgets)) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OBahngeschwindigkeit bgWidgets) tmvarStatus
    pure bgWidgets
        where
            getGeschwindigkeitsAnschluss :: Bahngeschwindigkeit z -> Anschluss
            getGeschwindigkeitsAnschluss    MärklinBahngeschwindigkeit {bgmGeschwindigkeitsAnschluss}   = bgmGeschwindigkeitsAnschluss
            getGeschwindigkeitsAnschluss    LegoBahngeschwindigkeit {bglGeschwindigkeitsAnschluss}      = bglGeschwindigkeitsAnschluss
            fahrtrichtungsAnschlussLabelPackNew :: (MitBox b) => b -> Bahngeschwindigkeit z -> IO ()
            fahrtrichtungsAnschlussLabelPackNew
                box
                LegoBahngeschwindigkeit {bglFahrtrichtungsAnschluss}
                    = void $ boxPackWidgetNewDefault box $ anschlussNew Language.fahrtrichtung bglFahrtrichtungsAnschluss
            fahrtrichtungsAnschlussLabelPackNew
                _box
                MärklinBahngeschwindigkeit {}
                    = pure ()
-- | 'Bahngeschwindigkeit' mit zugehörigen Widgets
data BGWidgets (z :: Zugtyp)
    = BGWidgets {
        bg :: Bahngeschwindigkeit z,
        bgWidget :: Gtk.HBox,
        bgHinzWS :: CheckButtonWegstreckeHinzufügen (BGWidgets z),
        bgHinzPL :: ButtonPlanHinzufügen (BGWidgets z)}
    deriving (Eq)

instance WegstreckenElement (BGWidgets 'Märklin) where
    getterWegstrecke :: Lens' (BGWidgets 'Märklin) RegistrierterCheckButton
    getterWegstrecke = Lens.lens bgHinzWS (\bg v -> bg {bgHinzWS = v})
    boxWegstrecke :: (DynamischeWidgetsReader r m, MonadIO m) =>
        BGWidgets 'Märklin -> m (BoxWegstreckeHinzufügen (BGWidgets 'Märklin))
    boxWegstrecke _bgWidgets = vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin <$> erhalteDynamischeWidgets
instance WegstreckenElement (BGWidgets 'Lego) where
    getterWegstrecke :: Lens' (BGWidgets 'Lego) RegistrierterCheckButton
    getterWegstrecke = (Lens.lens bgHinzWS (\bg v -> bg {bgHinzWS=v})) . _2
    boxWegstrecke :: (DynamischeWidgetsReader r m, MonadIO m) =>
        BGWidgets 'Lego -> m (BoxWegstreckeHinzufügen (BGWidgets 'Lego))
    boxWegstrecke _bgWidgets = vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego <$> erhalteDynamischeWidgets

instance PlanElement (BGWidgets 'Märklin) where
    foldPlan :: Fold (BGWidgets 'Märklin) (Maybe (ButtonPlanHinzufügen (BGWidgets 'Märklin)))
    foldPlan = Lens.folding $ (: []) . bgHinzPL
    boxenPlan :: (DynamischeWidgetsReader r m, Monad m) => BGWidgets 'Märklin -> m (ZipList (BoxPlanHinzufügen (BGWidgets 'Märklin)))
    boxenPlan _bgWidgets = vBoxHinzufügenPlanBahngeschwindigkeitenMärklin <$> erhalteDynamischeWidgets
instance PlanElement (BGWidgets 'Lego) where
    foldPlan :: Fold (BGWidgets 'Lego) (Maybe (ButtonPlanHinzufügen (BGWidgets 'Lego)))
    foldPlan = Lens.folding $ (: []) . bgHinzPL
    boxenPlan :: (DynamischeWidgetsReader r m, Monad m) => BGWidgets 'Lego -> m (ZipList (BoxPlanHinzufügen (BGWidgets 'Lego)))
    boxenPlan _bgWidgets = vBoxHinzufügenPlanBahngeschwindigkeitenLego <$> erhalteDynamischeWidgets

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

-- | Füge 'Scale' zum einstellen der Geschwindigkeit zur Box hinzu
hScaleGeschwindigkeitPackNew :: (MitBox b, BahngeschwindigkeitKlasse bg, StatusReader s m, MonadIO m) =>
    b -> bg z -> m Gtk.HScale
hScaleGeschwindigkeitPackNew box bahngeschwindigkeit = do
    scale <- boxPackWidgetNew box PackGrow paddingDefault positionDefault $ widgetShowNew $ hScaleNewWithRange 0 100 1
    on scale valueChanged $ get scale rangeValue >>= \wert -> ausführenTMVarAktion (Geschwindigkeit bahngeschwindigkeit $ fromIntegral $ fromEnum wert) tmvarStatus
    pure scale

-- | Füge 'Gtk.Button' zum umdrehen zur Box hinzu
buttonUmdrehenPackNew :: (MitBox b, BahngeschwindigkeitKlasse bg, MitRange r, StatusReader s m, MonadIO m) =>
    b -> bg z -> r -> m (Either Gtk.Button Gtk.ToggleButton)
buttonUmdrehenPackNew box bahngeschwindigkeit rangeGeschwindigkeit = do
    set rangeGeschwindigkeit [rangeValue := 0]
    if (zugtyp bahngeschwindigkeit == Lego)
        then do
            toggleButton <- boxPackWidgetNewDefault box $ toggleButtonNewWithLabel (Language.umdrehen :: Text)
            on toggleButton toggled $ get toggleButton toggleButtonActive >>= \vorwärts -> ausführenTMVarAktion (Umdrehen bahngeschwindigkeit (Just $ if vorwärts then Vorwärts else Rückwärts)) tmvarStatus
            pure $ Right toggleButton
        else boxPackWidgetNewDefault box (buttonNewWithEventLabel Language.umdrehen $ ausführenTMVarAktion (Umdrehen bahngeschwindigkeit Nothing) tmvarStatus) >>= pure . Left

-- | 'Streckenabschnitt' darstellen und zum Status hinzufügen
streckenabschnittPackNew :: (StatusReader r m, DynamischeWidgetsReader r m, MonadIO m) =>
    Streckenabschnitt -> m STWidgets
streckenabschnittPackNew streckenabschnitt@Streckenabschnitt {stromAnschluss} = do
    tmvarStatus <- erhalteStatus
    DynamischeWidgets {
        vBoxStreckenabschnitte,
        vBoxHinzufügenWegstreckeStreckenabschnitte,
        vBoxHinzufügenPlanStreckenabschnitte,
        fortfahrenWennToggledWegstrecke,
        tmvarPlanObjekt}
            <- erhalteDynamischeWidgets
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckeNew streckenabschnitt vBoxHinzufügenWegstreckeStreckenabschnitte fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew vBoxHinzufügenPlanStreckenabschnitte (OStreckenabschnitt streckenabschnitt) tmvarPlanObjekt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxStreckenabschnitte $ hBoxNew False 0
    nameLabelPackNew hBox streckenabschnitt
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.strom stromPin
    toggleButtonStromPackNew hBox streckenabschnitt tmvarStatus
    labelFließendValuePackNew hBox streckenabschnitt
    let stWidgets = STWidgets {st=streckenabschnitt, stWidget=hBox, stHinzPL=hinzufügenPlanWidget, stHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxStreckenabschnitte (entfernenStreckenabschnitt stWidgets >> liftIO (boxWegstrecke stWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets stWidgets dynamischeWidgets)) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OStreckenabschnitt stWidgets) tmvarStatus
    pure hBox
-- | Äußerstes Widget zur Darstellung eines 'Streckenabschnitt's
type StreckenabschnittWidget = Gtk.HBox
-- | Widget zum Hinzufügen eines 'Streckenabschnitt's zu einer 'Wegstrecke'
type StreckenabschnittWidgetHinzufügenWegstrecke = (Gtk.HBox, RegistrierterCheckButton)
-- | Widget zum Hinzufügen eines 'Streckenabschnitt's zu einem 'Plan'
type StreckenabschnittWidgetHinzufügenPlan = Gtk.Button
-- | 'Streckenabschnitt' mit zugehörigen Widgets
data STWidgets
    = STWidgets {
        st :: Streckenabschnitt,
        stWidget :: StreckenabschnittWidget,
        stHinzPL :: StreckenabschnittWidgetHinzufügenPlan,
        stHinzWS :: StreckenabschnittWidgetHinzufügenWegstrecke}
            deriving (Eq)

instance WegstreckenElement STWidgets where
    getterWegstrecke :: Lens' STWidgets RegistrierterCheckButton
    getterWegstrecke = (Lens.lens stHinzWS (\st v -> st {stHinzWS=v})) . _2
    boxWegstrecke :: (DynamischeWidgetsReader r m, MonadIO m) => STWidgets -> m ()
    boxWegstrecke _stWidgets = vBoxHinzufügenWegstreckeStreckenabschnitte <$> erhalteDynamischeWidgets

instance PlanElement STWidgets where
    foldPlan :: Fold STWidgets (Maybe Gtk.Button)
    foldPlan = Lens.folding $ (:[]) . Just . stHinzPL
    boxenPlan :: STWidgets -> DynamischeWidgets -> ZipList Gtk.VBox
    boxenPlan _stWidgets = ZipList . (: []) . vBoxHinzufügenPlanStreckenabschnitte <$> erhalteDynamischeWidgets

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

-- | Füge 'Gtk.ToggleButton' zum einstellen des Stroms zur Box hinzu
toggleButtonStromPackNew :: (StatusReader r m, MonadIO m, MitBox b, StreckenabschnittKlasse s) =>
    b -> s -> m Gtk.ToggleButton
toggleButtonStromPackNew box streckenabschnitt = do
    tmvarStatus <- erhalteStatus
    toggleButton <- boxPackWidgetNewDefault box $ toggleButtonNewWithLabel (Language.strom :: Text)
    on toggleButton toggled $ do
        an <- get toggleButton toggleButtonActive
        ausführenTMVarAktion (Strom streckenabschnitt $ if an then Fließend else Gesperrt) tmvarStatus
    pure toggleButton

-- | 'Weiche' darstellen und zum Status hinzufügen
weichePackNew :: (StatusReader r m, DynamischeWidgetsReader r m, MonadIO m) =>
    Weiche z -> m (WEWidgets z)
weichePackNew weiche = do
    tmvarStatus <- erhalteStatus
    dynamischeWidgets@DynamischeWidgets {
        vBoxWeichen,
        vBoxHinzufügenWegstreckeWeichenMärklin,
        vBoxHinzufügenWegstreckeWeichenLego,
        vBoxHinzufügenPlanWeichenGeradeMärklin,
        vBoxHinzufügenPlanWeichenKurveMärklin,
        vBoxHinzufügenPlanWeichenLinksMärklin,
        vBoxHinzufügenPlanWeichenRechtsMärklin,
        vBoxHinzufügenPlanWeichenGeradeLego,
        vBoxHinzufügenPlanWeichenKurveLego,
        vBoxHinzufügenPlanWeichenLinksLego,
        vBoxHinzufügenPlanWeichenRechtsLego,
        fortfahrenWennToggledWegstrecke,
        tmvarPlanObjekt}
            <- erhalteDynamischeWidgets
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- do
        hBoxHinzufügen <- boxPackWidgetNewDefault vBoxHinzufügenWegstreckeWeichen $ hBoxNew False 0
        checkButton <- boxPackWidgetNewDefault hBoxHinzufügen checkButtonNew
        boxPackWidgetNewDefault hBoxHinzufügen $ labelNew $ Just $ erhalteName weiche
        richtungsRadioButtons <- do
            let (h:|t) = erhalteRichtungen weiche
            hRichtungRadioButton <- boxPackWidgetNewDefault hBoxHinzufügen (radioButtonNewWithLabel $ show h) >>= \radioButton -> pure (h, radioButton)
            tRichtungRadioButtons <- mapM (\richtung -> boxPackWidgetNewDefault hBoxHinzufügen (radioButtonNewWithLabelFromWidget (snd hRichtungRadioButton) $ show richtung) >>= \radioButton -> pure (richtung, radioButton)) t
            pure $ hRichtungRadioButton :| tRichtungRadioButtons
        registrierterCheckButton <- registrieren checkButton fortfahrenWennToggledWegstrecke traversalHinzufügenWegstrecke
        pure (hBoxHinzufügen, registrierterCheckButton, richtungsRadioButtons)
    hinzufügenPlanWidgetGerade  <- if hatRichtung weiche Gerade then hinzufügenWidgetPlanNew vBoxHinzufügenPlanWeichenGerade (OWeiche weiche) tmvarPlanObjekt >>= pure . Just else pure Nothing
    hinzufügenPlanWidgetKurve   <- if hatRichtung weiche Kurve  then hinzufügenWidgetPlanNew vBoxHinzufügenPlanWeichenKurve  (OWeiche weiche) tmvarPlanObjekt >>= pure . Just else pure Nothing
    hinzufügenPlanWidgetLinks   <- if hatRichtung weiche Links  then hinzufügenWidgetPlanNew vBoxHinzufügenPlanWeichenLinks  (OWeiche weiche) tmvarPlanObjekt >>= pure . Just else pure Nothing
    hinzufügenPlanWidgetRechts  <- if hatRichtung weiche Rechts then hinzufügenWidgetPlanNew vBoxHinzufügenPlanWeichenRechts (OWeiche weiche) tmvarPlanObjekt >>= pure . Just else pure Nothing
    let hinzufügenPlanWidget = (hinzufügenPlanWidgetGerade, hinzufügenPlanWidgetKurve, hinzufügenPlanWidgetLinks, hinzufügenPlanWidgetRechts)
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxWeichen $ hBoxNew False 0
    nameLabelPackNew hBox weiche
    richtungsButtonsPackNew weiche hBox
    labelFließendValuePackNew hBox weiche
    let weWidgets = WEWidgets {we=weiche, weWidget=hBox, weHinzPL=hinzufügenPlanWidget, weHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxWeichen (entfernenWeiche weWidgets >> liftIO (boxWegstrecke weWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets weWidgets dynamischeWidgets)) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OWeiche weWidgets) tmvarStatus
    pure hBox
        where
            richtungsButtonsPackNew :: (MitBox b) => Weiche -> b -> IO ()
            richtungsButtonsPackNew
                MärklinWeiche {wemRichtungsAnschlüsse}
                box
                    = mapM_
                        (\(richtung, anschluss) ->
                            boxPackWidgetNewDefault box $
                                buttonNewWithEventLabel (showText richtung <:> showText anschluss) $
                                    ausführenTMVarAktion (Stellen weiche richtung) tmvarStatus)
                        wemRichtungsAnschlüsse
            richtungsButtonsPackNew
                LegoWeiche {welRichtungsAnschluss, welRichtungen = (richtung1, richtung2)}
                box
                    = void $ do
                        boxPackWidgetNewDefault box $ anschlussNew Language.richtung bglRichtungsAnschluss
                        boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung1) $
                            ausführenTMVarAktion (Stellen weiche richtung1) tmvarStatus
                        boxPackWidgetNewDefault box $ buttonNewWithEventLabel (showText richtung2) $
                            ausführenTMVarAktion (Stellen weiche richtung2) tmvarStatus
-- | Widget zum Hinzufügen einer 'Weiche' zu einer 'Wegstrecke'
type WeicheWidgetHinzufügenWegstrecke = (Gtk.HBox, RegistrierterCheckButton, NonEmpty (Richtung, Gtk.RadioButton))
-- | Widget zum Hinzufügen einer 'Weiche' zu einem 'Plan'
type WeicheWidgetHinzufügenPlan = (Maybe Gtk.Button, Maybe Gtk.Button, Maybe Gtk.Button, Maybe Gtk.Button)
-- | 'Weiche' mit zugehörigen Widgets
data WEWidgets (z :: Zugtyp)
    = WEWidgets {
        we :: Weiche z,
        weWidget :: Gtk.HBox,
        weHinzWS :: WeicheWidgetHinzufügenWegstrecke,
        weHinzPL :: WeicheWidgetHinzufügenPlan}
    deriving (Eq)

-- | Erhalte 'Gtk.RadioButton's zum wählen der Richtungen einer Lego-Weiche
getterRichtungsRadioButtons :: Getter (WEWidgets z) (NonEmpty (Richtung, Gtk.RadioButton))
getterRichtungsRadioButtons = Lens.to $ \weWidgets -> (weHinzWS weWidgets) ^. _3

instance WegstreckenElement (WEWidgets 'Märklin) where
    getterWegstrecke :: Lens' (WEWidgets 'Märklin) RegistrierterCheckButton
    getterWegstrecke = (Lens.lens weHinzWS (\we v -> we {weHinzWS=v}))
    boxWegstrecke :: (DynamischeWidgetsReader r m, MonadIO m) =>
        WEWidgets 'Märklin -> m ()
    boxWegstrecke _weWidgets = vBoxHinzufügenWegstreckeWeichenMärklin <$> erhalteDynamischeWidgets
instance WegstreckenElement (WEWidgets 'Lego) where
    getterWegstrecke :: Lens' (WEWidgets 'Lego) RegistrierterCheckButton
    getterWegstrecke = (Lens.lens weHinzWS (\we v -> we {weHinzWS=v})) . _2
    boxWegstrecke :: (DynamischeWidgetsReader r m, MonadIO m) =>
        WEWidgets 'Lego -> m ()
    boxWegstrecke _weWidgets = vBoxHinzufügenWegstreckeWeichenLego <$> erhalteDynamischeWidgets

instance PlanElement (WEWidgets 'Märklin) where
    foldPlan :: Fold (WEWidgets 'Märklin) (Maybe Gtk.Button)
    foldPlan = Lens.folding $ \(WEWidgets {weHinzPL=(a, b, c, d)}) -> [a, b, c, d]
    boxenPlan :: (DynamischeWidgetsReader r m, Monad m) =>
        WEWidgets 'Märklin -> m (ZipList Gtk.Box)
    boxenPlan _weWidgets = do
        DynamischeWidgets {
            vBoxHinzufügenPlanWeichenGeradeMärklin,
            vBoxHinzufügenPlanWeichenKurveMärklin,
            vBoxHinzufügenPlanWeichenLinksMärklin,
            vBoxHinzufügenPlanWeichenRechtsMärklin}
                <- erhalteDynamischeWidgets
        pure $ ZipList [
            vBoxHinzufügenPlanWeichenGeradeMärklin,
            vBoxHinzufügenPlanWeichenKurveMärklin,
            vBoxHinzufügenPlanWeichenLinkMärklins,
            vBoxHinzufügenPlanWeichenRechtsMärklin]
instance PlanElement (WEWidgets 'Lego) where
    foldPlan :: Fold (WEWidgets 'Lego) (Maybe Gtk.Button)
    foldPlan = Lens.folding $ \(WEWidgets {weHinzPL=(a, b, c, d)}) -> [a, b, c, d]
    boxenPlan :: (DynamischeWidgetsReader r m, Monad m) =>
        WEWidgets 'Lego -> m (ZipList Gtk.Box)
    boxenPlan _weWidgets = do
        DynamischeWidgets {
            vBoxHinzufügenPlanWeichenGeradeLego,
            vBoxHinzufügenPlanWeichenKurveLego,
            vBoxHinzufügenPlanWeichenLinksLego,
            vBoxHinzufügenPlanWeichenRechtsLego}
                <- erhalteDynamischeWidgets
        pure $ ZipList [
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
    toJSON :: WEWidgets -> Aeson.Value
    toJSON WEWidgets {we} = Aeson.toJSON we

instance WeicheKlasse (WEWidgets z) where
    stellen :: (PwmReader r m, MonadIO m) => WEWidgets z -> Richtung -> m ()
    stellen WEWidgets {we} = stellen we
    erhalteRichtungen :: WEWidgets z -> NonEmpty Richtung
    erhalteRichtungen WEWidgets {we} = erhalteRichtungen we

-- | 'Kupplung' darstellen und zum Status hinzufügen
kupplungPackNew :: (StatusReader r m, DynamischeWidgetsReader r m, MonadIO m) =>
    Kupplung -> m KUWidgets
kupplungPackNew kupplung@Kupplung {kupplungsAnschluss} = do
    tmvarStatus <- erhalteStatus
    dynamischeWidgets@DynamischeWidgets {
        vBoxKupplungen,
        vBoxHinzufügenWegstreckeKupplungen,
        vBoxHinzufügenPlanKupplungen,
        fortfahrenWennToggledWegstrecke,
        tmvarPlanObjekt}
            <- erhalteDynamischeWidgets
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckeNew kupplung vBoxHinzufügenWegstreckeKupplungen fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget <- hinzufügenWidgetPlanNew vBoxHinzufügenPlanKupplungen (OKupplung kupplung) tmvarPlanObjekt
    -- Widget erstellen
    hBox <- boxPackWidgetNewDefault vBoxKupplungen $ hBoxNew False 0
    nameLabelPackNew hBox kupplung
    boxPackWidgetNewDefault hBox $ pinLabelNew Language.kupplung kupplungsPin
    buttonKuppelnPackNew hBox kupplung tmvarStatus
    labelFließendValuePackNew hBox kupplung
    let kuWidgets = KUWidgets {ku=kupplung, kuWidget=hBox, kuHinzPL=hinzufügenPlanWidget, kuHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxKupplungen (entfernenKupplung kuWidgets >> liftIO (boxWegstrecke kuWidgets dynamischeWidgets >> entferneHinzufügenPlanWidgets kuWidgets dynamischeWidgets)) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OKupplung kuWidgets) tmvarStatus
    pure hBox
-- | 'Kupplung' mit zugehörigen Widgets
data KUWidgets
    = KUWidgets {
        ku :: Kupplung,
        kuWidget :: Gtk.HBox,
        kuHinzWS :: CheckButtonWegstreckeHinzufügen KUWidgets,
        kuHinzPL :: ButtonPlanHinzufügen KUWidgets}
    deriving (Eq)

instance WegstreckenElement KUWidgets where
    getterWegstrecke :: Lens' KUWidgets RegistrierterCheckButton
    getterWegstrecke = (Lens.lens kuHinzWS (\ku v -> ku {kuHinzWS=v}))
    boxWegstrecke :: (DynamischeWidgetsReader r m, MonadIO m) => KUWidgets -> m ()
    boxWegstrecke _kuWidgets = vBoxHinzufügenWegstreckeKupplungen <$> erhalteDynamischeWidgets

instance PlanElement KUWidgets where
    foldPlan :: Fold KUWidgets (Maybe Gtk.Button)
    foldPlan = Lens.folding $ (: []) . Just . kuHinzPL
    boxenPlan :: (DynamischeWidgetsReader r m) => KUWidgets -> m (ZipList Gtk.VBox)
    boxenPlan _kuWidgets = vBoxHinzufügenPlanKupplungen <$> erhalteDynamischeWidgets

instance StreckenObjekt KUWidgets where
    anschlüsse :: KUWidgets -> [Anschluss]
    anschlüsse KUWidgets {ku} = anschlüsse ku
    erhalteName :: KUWidgets -> Text
    erhalteName KUWidgets {ku} = erhalteName ku

instance Aeson.ToJSON KUWidgets where
    toJSON :: KUWidgets -> Aeson.Value
    toJSON KUWidgets {ku} = Aeson.toJSON ku

instance KupplungKlasse KUWidgets where
    kuppeln :: (PwmReader r m, MonadIO m) => KUWidgets -> m ()
    kuppeln KUWidgets {ku} = kuppeln ku

-- | Füge 'Gtk.Button' zum kuppeln zur Box hinzu
buttonKuppelnPackNew :: (MitBox b, KupplungKlasse k, StatusReader r m, MonadIO m) => b -> k -> m Gtk.Button
buttonKuppelnPackNew box kupplung = do
    tmvarStatus <- erhalteStatus
    boxPackWidgetNewDefault box $ buttonNewWithEventLabel Language.kuppeln $
        ausführenTMVarAktion (Kuppeln kupplung) tmvarStatus

-- | 'Wegstrecke' darstellen
wegstreckePackNew :: (StatusReader r m, DynamischeWidgetsReader r m, MonadIO m) => Wegstrecke z -> m (WSWidgets z)
wegstreckePackNew
    wegstrecke@Wegstrecke {
        wsBahngeschwindigkeiten,
        wsStreckenabschnitte,
        wsWeichenRichtungen,
        wsKupplungen}
            = do
                tmvarStatus <- erhalteStatus
                dynamischeWidgets@DynamischeWidgets {
                    vBoxWegstrecken,
                    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
                    vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
                    vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
                    vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
                    vBoxHinzufügenPlanWegstreckenKupplungMärklin,
                    vBoxHinzufügenPlanWegstreckenKupplungLego,
                    vBoxHinzufügenPlanWegstreckenMärklin,
                    vBoxHinzufügenPlanWegstreckenLego,
                    tmvarPlanObjekt}
                        <- erhalteDynamischeWidgets
                -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
                hinzufügenPlanWidgetBG  <- if null wsBahngeschwindigkeiten  then pure Nothing else do
                    hinzufügenPlanWidgetBG <- hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit        (OWegstrecke wegstrecke) tmvarPlanObjekt
                    hinzufügenPlanWidgetBGZ <- case zugtyp wegstrecke of
                        Lego
                            -> Left <$> hinzufügenWidgetPlanNew
                                vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
                                (OWegstrecke wegstrecke)
                                tmvarPlanObjekt
                        Märklin
                            -> Right <$> hinzufügenWidgetPlanNew
                                vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                                (OWegstrecke wegstrecke)
                                tmvarPlanObjekt
                    pure $ Just (hinzufügenPlanWidgetBG, hinzufügenPlanWidgetBGZ)
                hinzufügenPlanWidgetST  <- if null wsStreckenabschnitte     then pure Nothing else hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenStreckenabschnitt          (OWegstrecke wegstrecke) tmvarPlanObjekt >>= pure . Just
                hinzufügenPlanWidgetWE  <- if null wsWeichenRichtungen      then pure Nothing else hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenWeiche                     (OWegstrecke wegstrecke) tmvarPlanObjekt >>= pure . Just
                hinzufügenPlanWidgetKU  <- if null wsKupplungen             then pure Nothing else hinzufügenWidgetPlanNew vBoxHinzufügenPlanWegstreckenKupplung                   (OWegstrecke wegstrecke) tmvarPlanObjekt >>= pure . Just
                let hinzufügenPlanWidget = (hinzufügenPlanWidgetBG, hinzufügenPlanWidgetST, hinzufügenPlanWidgetWE, hinzufügenPlanWidgetKU)
                -- Widget erstellen
                frame <- boxPackWidgetNewDefault vBoxWegstrecken frameNew
                vBox <- containerAddWidgetNew frame $ vBoxNew False 0
                nameLabelPackNew vBox wegstrecke
                expander <- boxPackWidgetNewDefault vBox $ expanderNew (Language.wegstreckenElemente :: Text)
                vBoxExpander <- containerAddWidgetNew expander $ vBoxNew False 0
                functionBox <- boxPackWidgetNewDefault vBox $ hBoxNew False 0
                unless (null wsBahngeschwindigkeiten) $ void $ do
                    boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.bahngeschwindigkeiten <:> foldl appendName "" wsBahngeschwindigkeiten
                    hScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew functionBox wegstrecke tmvarStatus
                    buttonUmdrehenPackNew functionBox wegstrecke hScaleGeschwindigkeit tmvarStatus
                unless (null wsStreckenabschnitte) $ void $ do
                    boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.streckenabschnitte <:> foldl appendName "" wsStreckenabschnitte
                    toggleButtonStromPackNew functionBox wegstrecke tmvarStatus
                unless (null wsWeichenRichtungen) $ void $ do
                    boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.weichen <:> foldl (\acc (weiche, richtung) -> appendName acc weiche <°> showText richtung) "" wsWeichenRichtungen
                    boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.einstellen $ ausführenTMVarAktion (Einstellen wegstrecke) tmvarStatus
                unless (null wsKupplungen) $ void $ do
                    boxPackWidgetNewDefault vBoxExpander $ labelNew $ Just $ Language.kupplungen <:> foldl appendName "" wsKupplungen
                    buttonKuppelnPackNew functionBox wegstrecke tmvarStatus
                let wsWidgets = WSWidgets {ws=wegstrecke, wsWidget=frame, wsHinzPL=hinzufügenPlanWidget}
                buttonEntfernenPack functionBox (containerRemove vBoxWegstrecken frame >> entferneHinzufügenPlanWidgets wsWidgets dynamischeWidgets) (entfernenWegstrecke wsWidgets) tmvarStatus
                -- Widgets merken
                ausführenTMVarBefehl (Hinzufügen $ OWegstrecke wsWidgets) tmvarStatus
                pure frame
    where
        appendName :: (StreckenObjekt o) => Text -> o -> Text
        appendName ""       objekt = erhalteName objekt
        appendName string   objekt = string <^> erhalteName objekt
-- | Äußerstes Widget zur Darstellung einer 'Wegstrecke'
type WegstreckeWidget = Gtk.Frame
-- | Widget zum Hinzufügen einer 'Wegstrecke' zu einem 'Plan'
type WegstreckeWidgetHinzufügenPlan
    = (Maybe (Gtk.Button, Either Gtk.Button Gtk.Button), Maybe Gtk.Button, Maybe Gtk.Button, Maybe Gtk.Button)
-- | 'Wegstrecke' mit zugehörigen Widgets
data WSWidgets (z :: Zugtyp)
    = WSWidgets {
        ws :: Wegstrecke z,
        wsWidget :: WegstreckeWidget,
        wsHinzPL :: WegstreckeWidgetHinzufügenPlan}
            deriving (Eq)

instance PlanElement (WSWidgets z) where
    foldPlan :: Fold (WSWidgets z) (Maybe Gtk.Button)
    foldPlan = Lens.folding $ \(WSWidgets {wsHinzPL=(bgs, st, we, ku)}) -> bgButtons bgs <> [st, we, ku]
        where
            bgButtons :: Maybe (Gtk.Button, Either Gtk.Button Gtk.Button) -> [Maybe Gtk.Button]
            bgButtons (Nothing)                 = [Nothing, Nothing, Nothing]
            bgButtons (Just (bg, (Left bgL)))   = [Just bg, Just bgL, Nothing]
            bgButtons (Just (bg, (Right bgM)))  = [Just bg, Nothing, Just bgM]
    boxenPlan :: (DynamischeWidgetsReader r m, Monad m) => WSWidgets -> m (ZipList Gtk.VBox)
    boxenPlan _wsWidgets = do
        DynamischeWidgets {
            vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
            vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
            vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
            vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
            vBoxHinzufügenPlanWegstreckenKupplungMärklin,
            vBoxHinzufügenPlanWegstreckenKupplungLego,
            vBoxHinzufügenPlanWegstreckenMärklin,
            vBoxHinzufügenPlanWegstreckenLego}
                <- erhalteDynamischeWidgets
        pure $ ZipList [vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego, vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin, vBoxHinzufügenPlanWegstreckenStreckenabschnitt, vBoxHinzufügenPlanWegstreckenWeiche, vBoxHinzufügenPlanWegstreckenKupplung]

instance StreckenObjekt (WSWidgets z) where
    anschlüsse :: WSWidgets z -> [Anschluss]
    anschlüsse WSWidgets {ws} = anschlüsse ws
    erhalteName :: WSWidgets z -> Text
    erhalteName WSWidgets {ws} = erhalteName ws

instance Aeson.ToJSON (WSWidgets z) where
    toJSON :: WSWidgets z -> Aeson.Value
    toJSON (WSWidgets {ws}) = Aeson.toJSON ws

instance BahngeschwindigkeitKlasse WSWidgets where
    geschwindigkeit :: (PwmReader r m, MonadIO m) => WSWidgets -> Natural -> m ()
    geschwindigkeit (WSWidgets {ws}) = geschwindigkeit ws
    umdrehen :: (PwmReader r m, MonadIO m) => WSWidgets -> Maybe Fahrtrichtung -> m ()
    umdrehen (WSWidgets {ws}) = umdrehen ws

instance StreckenabschnittKlasse (WSWidgets z) where
    strom :: (I2CReader r m, MonadIO m) => WSWidgets z -> Strom -> m ()
    strom (WSWidgets {ws}) = strom ws

instance KupplungKlasse (WSWidgets z) where
    kuppeln :: (I2CReader r m, MonadIO m) => (WSWidgets z) -> m ()
    kuppeln (WSWidgets {ws}) = kuppeln ws

instance WegstreckeKlasse (WSWidgets z) where
    einstellen :: (PwmReader r m, MonadIO m) => WSWidgets z -> m ()
    einstellen (WSWidgets {ws}) = einstellen ws

-- | 'Plan' darstellen
planPackNew :: (StatusReader r m, DynamischeWidgetsReader r m, MonadIO m) => Plan -> m PlanWidget
planPackNew plan@Plan {plAktionen} = do
    tmvarStatus <- erhalteStatus
    DynamischeWidgets {vBoxPläne, progressBarPlan, windowMain} <- erhalteDynamischeWidgets
    -- Widget erstellen
    frame <- boxPackWidgetNewDefault vBoxPläne $ frameNew
    vBox <- containerAddWidgetNew frame $ vBoxNew False 0
    nameLabelPackNew vBox plan
    expander <- boxPackWidgetNewDefault vBox $ expanderNew $ Language.aktionen <:> show (length plAktionen)
    vBoxExpander <- containerAddWidgetNew expander $ vBoxNew False 0
    mapM_ ((boxPackWidgetNewDefault vBoxExpander) . labelNew . Just . show) plAktionen
    functionBox <- boxPackWidgetNewDefault vBox hButtonBoxNew
    buttonAusführen <- boxPackWidgetNewDefault functionBox $ buttonNewWithLabel (Language.ausführen :: Text)
    buttonAbbrechen <- boxPackWidgetNewDefault functionBox $ buttonNewWithLabel (Language.ausführenAbbrechen :: Text)
    widgetHide buttonAbbrechen
    dialogGesperrt <- messageDialogNew (Just windowMain) [] MessageError ButtonsOk ("" :: Text)
    set dialogGesperrt [windowTitle := (Language.aktionGesperrt :: Text)]
    on buttonAusführen buttonActivated $ do
        auswertenTMVarIOStatus (ausführenMöglich plan) tmvarStatus >>= \case
            (AusführenMöglich)  -> do
                widgetHide buttonAusführen
                widgetShow buttonAbbrechen
                void $ ausführenTMVarBefehl (Ausführen plan (\wert -> set progressBarPlan [progressBarFraction := (toEnum $ fromIntegral wert) / (toEnum $ length plAktionen)]) $ widgetShow buttonAusführen >> widgetHide buttonAbbrechen) tmvarStatus
            (WirdAusgeführt)    -> error "Ausführen in GTK-UI erneut gestartet."
            (PinsBelegt pins)   -> do
                set dialogGesperrt [messageDialogText := Just (Language.ausführenGesperrt $ show $ ausFoldable pins)]
                void $ dialogEval dialogGesperrt
    on buttonAbbrechen buttonActivated $ do
        ausführenTMVarBefehl (AusführenAbbrechen plan) tmvarStatus
        widgetShow buttonAusführen
        widgetHide buttonAbbrechen
    let plWidgets = PLWidgets {pl=plan, plWidget=frame}
    buttonEntfernenPack functionBox (containerRemove vBoxPläne frame) (entfernenPlan plWidgets) tmvarStatus
    -- Widgets merken
    ausführenTMVarBefehl (Hinzufügen $ OPlan plWidgets) tmvarStatus
    pure frame
-- | Äußerstes Widget zur Darstellung eines 'Plan's
type PlanWidget = Gtk.Frame
-- | 'Plan' mit zugehörigen Widgets
data PLWidgets
    = PLWidgets {
        pl :: Plan,
        plWidget :: PlanWidget}
            deriving (Eq)

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
#endif