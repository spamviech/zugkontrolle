{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ScrollbaresWidget(), scrollbaresWidgetNew, scrollbaresWidgetAddNew,
    scrollbaresWidgetPackNew, scrollbaresWidgetNotebookAppendPageNew,
    -- ** Knopf erstellen
    buttonNewWithEvent, buttonNewWithEventLabel, buttonNewWithEventMnemonic,
    -- ** Auswahl-Widgets
    AuswahlWidget(), aktuelleAuswahl, auswahlRadioButtonNew, auswahlComboBoxNew,
    boundedEnumAuswahlRadioButtonNew, boundedEnumAuswahlComboBoxNew,
    MitAuswahlWidget(..), mitAuswahlWidget, auswahlWidget,
    -- ** Anschluss darstellen
    AnschlussWidget(), anschlussNew, AnschlussAuswahlWidget(), anschlussAuswahlNew, aktuellerAnschluss,
    -- ** Name darstellen
    namePackNew,
    -- ** Spezifisches StreckenObjekt darstellen
    BGWidgets(), STWidgets(), WEWidgets(), KUWidgets(), WSWidgets(), PLWidgets(),
    bahngeschwindigkeitPackNew, streckenabschnittPackNew, weichePackNew, kupplungPackNew, wegstreckePackNew, planPackNew,
    -- * Verwaltung des aktuellen Zustands
    DynamischeWidgets(..), DynamischeWidgetsReader(..), StatusGui, StatusReader(..),
    ObjektGui, BefehlGui, IOStatusGui, MStatusGui, MStatusGuiT,
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
import Zug.Klassen (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), ausZugtypEither, mapZugtypEither,
                    Fahrtrichtung(..), Strom(..), Richtung(..))
import qualified Zug.Language as Language
import Zug.Language ((<^>), (<:>), (<°>), showText)
import Zug.Menge (ausFoldable)
import Zug.Objekt (ObjektAllgemein(..), ObjektElement(..), Objekt, Phantom(..), ObjektKlasse(..))
import Zug.Plan (PlanKlasse(..), Plan(..), AusführendReader(),
                AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionWeiche(..),
                AktionKupplung(..), AktionWegstrecke(..))
import Zug.UI.Base (StatusAllgemein(..), IOStatusAllgemein, MStatusAllgemein, MStatusAllgemeinT,
                    AusführenMöglich(..), ReaderFamilie, ObjektReader,
                    TVarMaps(..), TVarMapsReader(..), MitTVarMaps(..),
                    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, wegstrecken, pläne,
                    auswertenTMVarIOStatus, ausführenMöglich, entfernenBahngeschwindigkeit,
                    entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung,
                    entfernenWegstrecke, entfernenPlan)
import Zug.UI.Befehl (BefehlAllgemein(..), ausführenTMVarBefehl, ausführenTMVarAktion)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledTMVar, registrierterCheckButtonNew,
                                        RegistrierterCheckButton, MitRegistrierterCheckButton(..))
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, containerRemoveJust, boxPackWidgetNew, boxPackWidgetNewDefault,
                                    packingDefault, paddingDefault, positionDefault, boxPackDefault, boxPack,
                                    Packing(..), Padding(..), Position(..), dialogEval,
                                    buttonNewWithEventLabel, namePackNew, widgetShowNew, widgetShowIf,
                                    notebookAppendPageNew, buttonNewWithEvent, buttonNewWithEventMnemonic)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitContainer(..), mitContainerRemove, MitBox(..), MitRange(..),
                                    MitButton(), MitCheckButton(), MitToggleButton())
import Zug.UI.Gtk.Widgets.Anschluss (AnschlussWidget(), anschlussNew,
                                        AnschlussAuswahlWidget(), anschlussAuswahlNew, aktuellerAnschluss)
import Zug.UI.Gtk.Widgets.Auswahl (AuswahlWidget(), aktuelleAuswahl, auswahlRadioButtonNew, auswahlComboBoxNew,
                                    boundedEnumAuswahlRadioButtonNew, boundedEnumAuswahlComboBoxNew,
                                    MitAuswahlWidget(..), mitAuswahlWidget, auswahlWidget)
import Zug.UI.Gtk.Widgets.Fliessend (fließendPackNew)
import Zug.UI.Gtk.Widgets.ScrollbaresWidget (ScrollbaresWidget(), scrollbaresWidgetNew, scrollbaresWidgetAddNew,
                                            scrollbaresWidgetPackNew, scrollbaresWidgetNotebookAppendPageNew)

-- | 'Phantom' spezialisiert auf 'ObjektGui'
phantomGui :: Phantom ObjektGui
phantomGui = Phantom

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
        deriving (Eq, MitWidget, MitContainer, MitBox, MitButton, MitToggleButton, MitCheckButton, MitRegistrierterCheckButton)

instance (MitAuswahlWidget w e) => MitAuswahlWidget (WidgetWegstreckeHinzufügen w a) e where
    erhalteAuswahlWidget :: WidgetWegstreckeHinzufügen w a -> AuswahlWidget e
    erhalteAuswahlWidget = erhalteAuswahlWidget . widgetWegstreckeHinzufügen

type BoxWegstreckeHinzufügen a = WidgetWegstreckeHinzufügen Gtk.VBox a
type CheckButtonWegstreckeHinzufügen e a = WidgetWegstreckeHinzufügen (WegstreckeCheckButton e) a

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
    fortfahrenWennToggledWegstrecke :: FortfahrenWennToggledTMVar StatusGui RegistrierterCheckButton,
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

-- | Klasse für Typen mit dem in einer 'TMVar' gespeicherten 'StatusGui'
class MitStatus r where
    status :: r -> TMVar StatusGui
instance MitStatus (TMVar StatusGui) where
    status :: TMVar StatusGui -> TMVar StatusGui
    status = id
-- | Abkürzung für Funktionen, die den in einer 'TMVar' gespeicherten 'StatusGui' benötigen.
class (MonadReader r m) => StatusReader r m where
    erhalteStatus :: m (TMVar StatusGui)
instance (MonadReader r m, MitStatus r) => StatusReader r m where
    erhalteStatus :: m (TMVar StatusGui)
    erhalteStatus = asks status

type instance ReaderFamilie ObjektGui = (TVarMaps, DynamischeWidgets, TMVar StatusGui)

instance MitTVarMaps (TVarMaps, DynamischeWidgets, TMVar StatusGui) where
    tvarMaps :: (TVarMaps, DynamischeWidgets, TMVar StatusGui) -> TVarMaps
    tvarMaps (tvarMaps, _dynamischeWidgets, _tmvarStatus) = tvarMaps
instance MitDynamischeWidgets (TVarMaps, DynamischeWidgets, TMVar StatusGui) where
    dynamischeWidgets :: (TVarMaps, DynamischeWidgets, TMVar StatusGui) -> DynamischeWidgets
    dynamischeWidgets (_tvarMaps, dynamischeWidgets, _tmvarStatus) = dynamischeWidgets
instance MitStatus (TVarMaps, DynamischeWidgets, TMVar StatusGui) where
    status :: (TVarMaps, DynamischeWidgets, TMVar StatusGui) -> TMVar StatusGui
    status (_tvarMaps, _dynamischeWidgets, tmvarStatus) = tmvarStatus

-- | Klasse für Widgets-Repräsentation von Objekt-Typen
class WidgetsTyp s where
    -- | Assoziierter 'Objekt'-Typ
    type ObjektTyp s
    -- | Erhalte den eingebetteten 'ObjektTyp'
    erhalteObjektTyp :: s -> ObjektTyp s

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
    mitContainerRemove box $ wegsteckenElement ^. getterWegstrecke

-- type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s
foldWegstreckeHinzufügen :: Lens.Fold StatusGui RegistrierterCheckButton
foldWegstreckeHinzufügen = Lens.folding registrierteCheckButtons
    where
        registrierteCheckButtons :: StatusGui -> [RegistrierterCheckButton]
        registrierteCheckButtons status
            = map (erhalteRegistrierterCheckButton . Lens.view getterWegstrecke) (status ^. bahngeschwindigkeiten)
            ++ map (erhalteRegistrierterCheckButton . Lens.view getterWegstrecke) (status ^. streckenabschnitte)
            ++ map (erhalteRegistrierterCheckButton . Lens.view getterWegstrecke) (status ^. weichen)
            ++ map (erhalteRegistrierterCheckButton . Lens.view getterWegstrecke) (status ^. kupplungen)

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
    sequence_ $ containerRemoveJust <$> ZipList boxenPlan <*> ZipList (planElement ^.. foldPlan)

-- | Entfernen-Knopf zu 'Box' hinzufügen. Beim drücken werden /removeActionGui/ und /removeAction/ ausgeführt.
buttonEntfernenPack :: (MitBox b, StatusReader r m, TVarMapsReader r m, DynamischeWidgetsReader r m, MonadIO m) =>
    b -> IO () -> IOStatusGui () -> m Gtk.Button
buttonEntfernenPack box removeActionGui removeAction = do
    tvarMaps <- erhalteTVarMaps
    dynamischeWidgets <- erhalteDynamischeWidgets
    tmvarStatus <- erhalteStatus
    liftIO $ boxPackWidgetNew box PackNatural paddingDefault End $
        buttonNewWithEventLabel Language.entfernen $ do
            runReaderT (auswertenTMVarIOStatus removeAction tmvarStatus) (tvarMaps, dynamischeWidgets, tmvarStatus)
            removeActionGui

-- | Entfernen-Knopf zu Box hinzufügen. Beim drücken wird /parent/ aus der /box/ entfernt und die 'IOStatusGui'-Aktion ausgeführt.
buttonEntfernenPackSimple ::
    (MitBox b, MitContainer c, StatusReader r m, TVarMapsReader r m, DynamischeWidgetsReader r m, MonadIO m) =>
        b -> c -> IOStatusGui () -> m Gtk.Button
buttonEntfernenPackSimple box parent = buttonEntfernenPack box $ mitContainerRemove parent box

-- ** Widget mit Name und CheckButton erstellen
-- | Erzeuge einen 'RegistrierterCheckButton' mit einem 'Label' für den Namen.
hinzufügenWidgetWegstreckePackNew :: forall o r m.
    (DynamischeWidgetsReader r m, StreckenObjekt (ObjektTyp o), WegstreckenElement o, MonadIO m) =>
        ObjektTyp o -> m (CheckButtonWegstreckeHinzufügen Void o)
hinzufügenWidgetWegstreckePackNew objekt = do
    dynamischeWidgets@DynamischeWidgets {fortfahrenWennToggledWegstrecke} <- erhalteDynamischeWidgets
    let box = dynamischeWidgets ^. boxWegstrecke objekt :: BoxWegstreckeHinzufügen o
    fmap (WidgetWegstreckeHinzufügen . WegstreckeCheckButton) $ boxPackWidgetNewDefault box $
        registrierterCheckButtonNew (erhalteName objekt) fortfahrenWennToggledWegstrecke
-- | Erzeuge einen 'RegistrierterCheckButton' mit einem 'Label' für den Namen und einem 'AuswahlWidget' für die übergebenen 'Richtung'en.
hinzufügenWidgetWegstreckeRichtungPackNew :: forall o r m.
    (DynamischeWidgetsReader r m, StreckenObjekt (ObjektTyp o), WegstreckenElement o, MonadIO m) =>
    ObjektTyp o -> NonEmpty Richtung -> m (CheckButtonWegstreckeHinzufügen Richtung o)
hinzufügenWidgetWegstreckeRichtungPackNew objekt richtungen = do
    dynamischeWidgets@DynamischeWidgets {fortfahrenWennToggledWegstrecke} <- erhalteDynamischeWidgets
    let box = dynamischeWidgets ^. boxWegstrecke objekt :: BoxWegstreckeHinzufügen o
    liftIO $ do
        hBox <- Gtk.hBoxNew False 0
        wcbrRegistrierterCheckButton <- boxPackWidgetNewDefault hBox $
            registrierterCheckButtonNew (erhalteName objekt) fortfahrenWennToggledWegstrecke
        wcbrRichtungsAuswahl <- boxPackWidgetNewDefault hBox $ auswahlRadioButtonNew richtungen ""
        let wegstreckeCheckButton = WidgetWegstreckeHinzufügen WegstreckeCheckButtonRichtung {
            wcbrWidget = erhalteWidget hBox,
            wcbrRegistrierterCheckButton,
            wcbrRichtungsAuswahl}
        boxPackDefault box wegstreckeCheckButton
        pure wegstreckeCheckButton

-- | Füge einen Knopf mit dem Namen zur Box hinzu. Beim drücken wird die 'TMVar' mit dem Objekt gefüllt.
hinzufügenWidgetPlanPackNew ::
    (DynamischeWidgetsReader r m, StreckenObjekt (ObjektTyp o), ObjektElement (ObjektTyp o), MonadIO m) =>
        BoxPlanHinzufügen o -> ObjektTyp o -> m (ButtonPlanHinzufügen o)
hinzufügenWidgetPlanPackNew box objekt = do
    DynamischeWidgets {tmvarPlanObjekt} <- erhalteDynamischeWidgets
    fmap WidgetPlanHinzufügen $ boxPackWidgetNewDefault box $ buttonNewWithEventLabel (erhalteName objekt) $
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

instance WidgetsTyp (BGWidgets z) where
    type ObjektTyp (BGWidgets z) = Bahngeschwindigkeit z
    erhalteObjektTyp :: BGWidgets z -> Bahngeschwindigkeit z
    erhalteObjektTyp = bg

instance WidgetsTyp (ZugtypEither BGWidgets) where
    type ObjektTyp (ZugtypEither BGWidgets) = ZugtypEither Bahngeschwindigkeit
    erhalteObjektTyp :: ZugtypEither BGWidgets -> ZugtypEither Bahngeschwindigkeit
    erhalteObjektTyp = mapZugtypEither bg

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
    getterWegstrecke = Lens.to $ ausZugtypEither $ WidgetWegstreckeHinzufügen . widgetWegstreckeHinzufügen . bgHinzWS
    boxWegstrecke :: ZugtypEither Bahngeschwindigkeit -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (ZugtypEither BGWidgets))
    boxWegstrecke (ZugtypMärklin _bgWidgets) = Lens.to $
        WidgetWegstreckeHinzufügen . widgetWegstreckeHinzufügen . vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
    boxWegstrecke (ZugtypLego _bgWidgets) = Lens.to $
        WidgetWegstreckeHinzufügen . widgetWegstreckeHinzufügen . vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego

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
    foldPlan = Lens.to $ ausZugtypEither $ Just . WidgetPlanHinzufügen . widgetPlanHinzufügen . bgHinzPL
    boxenPlan :: ZugtypEither Bahngeschwindigkeit -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither BGWidgets))
    boxenPlan (ZugtypMärklin _bgWidgets) = Lens.to $
        WidgetPlanHinzufügen . widgetPlanHinzufügen . vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
    boxenPlan (ZugtypLego _bgWidgets) = Lens.to $
        WidgetPlanHinzufügen . widgetPlanHinzufügen . vBoxHinzufügenPlanBahngeschwindigkeitenLego

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
    buttonEntfernenPackSimple hBox vBoxBahngeschwindigkeiten $ do
        entfernenBahngeschwindigkeit $ zuZugtypEither bgWidgets
        entferneHinzufügenWegstreckeWidgets bgWidgets
        entferneHinzufügenPlanWidgets bgWidgets
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
                        liftIO $ boxPackWidgetNewDefault box $ anschlussNew Language.fahrtrichtung bglFahrtrichtungsAnschluss
                        buttonFahrtrichtungEinstellenPackNew box bgLego range

-- | Füge 'Scale' zum einstellen der Geschwindigkeit zur Box hinzu
hScaleGeschwindigkeitPackNew ::
    (MitBox b, BahngeschwindigkeitKlasse bg, ObjektReader ObjektGui m, MonadIO m) =>
        b -> bg z -> m Gtk.HScale
hScaleGeschwindigkeitPackNew box bahngeschwindigkeit = do
    tmvarStatus <- erhalteStatus
    objektReader <- ask
    liftIO $ do
        scale <- boxPackWidgetNew box PackGrow paddingDefault positionDefault $ widgetShowNew $ Gtk.hScaleNewWithRange 0 100 1
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
-- | Füge 'Gtk.Button' zum Fahrtrichtung einstellen zur Box hinzu
buttonFahrtrichtungEinstellenPackNew ::
    (MitBox b, BahngeschwindigkeitKlasse bg, MitRange r, ObjektReader ObjektGui m, MonadIO m) =>
        b -> bg 'Lego -> r -> m Gtk.ToggleButton
buttonFahrtrichtungEinstellenPackNew box bahngeschwindigkeit rangeGeschwindigkeit = do
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

instance WidgetsTyp STWidgets where
    type ObjektTyp STWidgets = Streckenabschnitt
    erhalteObjektTyp :: STWidgets -> Streckenabschnitt
    erhalteObjektTyp = st

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
    let stWidgets = STWidgets {st=streckenabschnitt, stWidget=hBox, stHinzPL=hinzufügenPlanWidget, stHinzWS=hinzufügenWegstreckeWidget}
    buttonEntfernenPackSimple hBox vBoxStreckenabschnitte $ do
        entfernenStreckenabschnitt stWidgets
        entferneHinzufügenWegstreckeWidgets stWidgets
        entferneHinzufügenPlanWidgets stWidgets
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

instance WidgetsTyp (WEWidgets z) where
    type ObjektTyp (WEWidgets z) = Weiche z
    erhalteObjektTyp :: WEWidgets z -> Weiche z
    erhalteObjektTyp = we

instance WidgetsTyp (ZugtypEither WEWidgets) where
    type ObjektTyp (ZugtypEither WEWidgets) = ZugtypEither Weiche
    erhalteObjektTyp :: ZugtypEither WEWidgets -> ZugtypEither Weiche
    erhalteObjektTyp = mapZugtypEither we

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
    getterWegstrecke = Lens.to $ ausZugtypEither $ WidgetWegstreckeHinzufügen . widgetWegstreckeHinzufügen . weHinzWS
    boxWegstrecke :: ZugtypEither Weiche -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen (ZugtypEither WEWidgets))
    boxWegstrecke (ZugtypMärklin _weWidgets) = Lens.to $
        WidgetWegstreckeHinzufügen . widgetWegstreckeHinzufügen . vBoxHinzufügenWegstreckeWeichenMärklin
    boxWegstrecke (ZugtypLego _weWidgets) = Lens.to $
        WidgetWegstreckeHinzufügen . widgetWegstreckeHinzufügen . vBoxHinzufügenWegstreckeWeichenLego

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
            -> fmap (WidgetPlanHinzufügen . widgetPlanHinzufügen) <$> [gerade, kurve, links, rechts]
    boxenPlan :: ZugtypEither Weiche -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (ZugtypEither WEWidgets))
    boxenPlan (ZugtypMärklin _bgWidgets) = Lens.folding $ fmap (WidgetPlanHinzufügen . widgetPlanHinzufügen) . (??) [
        vBoxHinzufügenPlanWeichenGeradeMärklin,
        vBoxHinzufügenPlanWeichenKurveMärklin,
        vBoxHinzufügenPlanWeichenLinksMärklin,
        vBoxHinzufügenPlanWeichenRechtsMärklin]
    boxenPlan (ZugtypLego _bgWidgets) = Lens.folding $ fmap (WidgetPlanHinzufügen . widgetPlanHinzufügen) . (??) [
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
    buttonEntfernenPackSimple hBox vBoxWeichen $ do
        entfernenWeiche $ zuZugtypEither weWidgets
        entferneHinzufügenWegstreckeWidgets $ zuZugtypEither weWidgets
        entferneHinzufügenPlanWidgets $ zuZugtypEither weWidgets
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

instance WidgetsTyp KUWidgets where
    type ObjektTyp KUWidgets = Kupplung
    erhalteObjektTyp :: KUWidgets -> Kupplung
    erhalteObjektTyp = ku

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
    buttonEntfernenPackSimple hBox vBoxKupplungen $ do
        entfernenKupplung kuWidgets
        entferneHinzufügenWegstreckeWidgets kuWidgets
        entferneHinzufügenPlanWidgets kuWidgets
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

instance WidgetsTyp (WSWidgets z) where
    type ObjektTyp (WSWidgets z) = Wegstrecke z
    erhalteObjektTyp :: WSWidgets z -> Wegstrecke z
    erhalteObjektTyp = ws

instance WidgetsTyp (ZugtypEither WSWidgets) where
    type ObjektTyp (ZugtypEither WSWidgets) = ZugtypEither Wegstrecke
    erhalteObjektTyp :: ZugtypEither WSWidgets -> ZugtypEither Wegstrecke
    erhalteObjektTyp = mapZugtypEither ws

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
    boxenPlan :: Wegstrecke z -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen (WSWidgets 'Lego))
    boxenPlan _wsWidgets = Lens.folding $ (??) [
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
wegstreckePackNew :: (StatusReader r m, DynamischeWidgetsReader r m, MonadIO m, ZugtypKlasse z) =>
    Wegstrecke z -> m (WSWidgets z)
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
                hinzufügenPlanWidgetBG  <- if null wsBahngeschwindigkeiten
                    then pure Nothing
                    else Just <$> _hinzufügenWidgetPlanPackNew
                        _vBoxHinzufügenPlanWegstreckenBahngeschwindigkeit
                        (OWegstrecke $ zuZugtypEither wegstrecke)
                        tmvarPlanObjekt
                hinzufügenPlanWidgetST <- if null wsStreckenabschnitte
                    then pure Nothing
                    else Just <$> _hinzufügenWidgetPlanPackNew
                        _vBoxHinzufügenPlanWegstreckenStreckenabschnitt
                        (OWegstrecke $ zuZugtypEither wegstrecke)
                        tmvarPlanObjekt
                hinzufügenPlanWidgetKU <- if null wsKupplungen
                    then pure Nothing
                    else Just <$> _hinzufügenWidgetPlanPackNew
                        _vBoxHinzufügenPlanWegstreckenKupplung
                        (OWegstrecke $ zuZugtypEither wegstrecke)
                        tmvarPlanObjekt
                hinzufügenPlanWidgetWS <- if null wsWeichenRichtungen
                    then pure Nothing
                    else Just <$> _hinzufügenWidgetPlanPackNew
                        _vBoxHinzufügenPlanWegstreckenWeiche
                        (OWegstrecke $ zuZugtypEither wegstrecke)
                        tmvarPlanObjekt
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
                unless (null wsBahngeschwindigkeiten) $ liftIO $ void $ do
                    boxPackWidgetNewDefault vBoxExpander $ Gtk.labelNew $ Just $
                        Language.bahngeschwindigkeiten <:> foldl appendName "" wsBahngeschwindigkeiten
                    hScaleGeschwindigkeit <- _hScaleGeschwindigkeitPackNew functionBox wegstrecke
                    _buttonUmdrehenPackNew functionBox wegstrecke hScaleGeschwindigkeit tmvarStatus
                unless (null wsStreckenabschnitte) $ void $ do
                    liftIO $ boxPackWidgetNewDefault vBoxExpander $ Gtk.labelNew $ Just $
                        Language.streckenabschnitte <:> foldl appendName "" wsStreckenabschnitte
                    _toggleButtonStromPackNew functionBox wegstrecke
                unless (null wsWeichenRichtungen) $ void $ do
                    liftIO $ boxPackWidgetNewDefault vBoxExpander $ Gtk.labelNew $ Just $
                        Language.weichen <:>
                        foldl (\acc (weiche, richtung) -> appendName acc weiche <°> showText richtung) "" wsWeichenRichtungen
                    boxPackWidgetNewDefault functionBox $ buttonNewWithEventLabel Language.einstellen $
                        _ausführenTMVarAktion (Einstellen wegstrecke) tmvarStatus
                unless (null wsKupplungen) $ liftIO $ void $ do
                    boxPackWidgetNewDefault vBoxExpander $ Gtk.labelNew $ Just $
                        Language.kupplungen <:> foldl appendName "" wsKupplungen
                    _buttonKuppelnPackNew functionBox wegstrecke
                let wsWidgets = WSWidgets {
                        ws = wegstrecke,
                        wsWidget = frame,
                        wsHinzPL = hinzufügenPlanWidget}
                _buttonEntfernenPack
                    functionBox
                    (Gtk.containerRemove vBoxWegstrecken frame >> _entferneHinzufügenPlanWidgets wsWidgets)
                    (_entfernenWegstrecke $ zuZugtypEither wsWidgets)
                -- Widgets merken
                _ausführenTMVarBefehl (Hinzufügen $ OWegstrecke $ zuZugtypEither wsWidgets) tmvarStatus
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
        plWidget :: Gtk.Frame}
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

-- | 'Plan' darstellen
planPackNew :: (StatusReader r m, DynamischeWidgetsReader r m, MonadIO m) => Plan -> m PLWidgets
planPackNew plan@Plan {plAktionen} = do
    tmvarStatus <- _erhalteStatus
    DynamischeWidgets {vBoxPläne, progressBarPlan, windowMain} <- _erhalteDynamischeWidgets
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
    liftIO $ Gtk.on buttonAusführen Gtk.buttonActivated $ do
        _auswertenTMVarIOStatus (_ausführenMöglich plan) tmvarStatus >>= \case
            AusführenMöglich                -> do
                Gtk.widgetHide buttonAusführen
                Gtk.widgetShow buttonAbbrechen
                void $ _ausführenTMVarBefehl (Ausführen plan anzeigeAktion abschlussAktion) tmvarStatus
                where
                    anzeigeAktion :: Natural -> IO ()
                    anzeigeAktion wert = Gtk.set progressBarPlan [
                        Gtk.progressBarFraction := (fromIntegral wert) / (fromIntegral $ length plAktionen)]
                    abschlussAktion :: IO ()
                    abschlussAktion = do
                        Gtk.widgetShow buttonAusführen
                        Gtk.widgetHide buttonAbbrechen
            WirdAusgeführt                  -> error "Ausführen in GTK-UI erneut gestartet."
            (AnschlüsseBelegt anschlüsse)   -> do
                Gtk.set dialogGesperrt 
                    [Gtk.messageDialogText := Just (Language.ausführenGesperrt $ show $ ausFoldable anschlüsse)]
                void $ dialogEval dialogGesperrt
    liftIO $ Gtk.on buttonAbbrechen Gtk.buttonActivated $ do
        _ausführenTMVarBefehl (AusführenAbbrechen plan) tmvarStatus
        Gtk.widgetShow buttonAusführen
        Gtk.widgetHide buttonAbbrechen
    let plWidgets = PLWidgets {pl = plan, plWidget = frame}
    _buttonEntfernenPack functionBox (Gtk.containerRemove vBoxPläne frame) (_entfernenPlan plWidgets)
    -- Widgets merken
    _ausführenTMVarBefehl (Hinzufügen $ OPlan plWidgets) tmvarStatus
    pure plWidgets
#endif