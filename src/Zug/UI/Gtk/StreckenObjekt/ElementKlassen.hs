{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zug.UI.Gtk.StreckenObjekt.ElementKlassen
  ( -- * Wegstrecken-Element
    WegstreckenElement(..)
  , WegstreckeCheckButtonVoid
  , MitFortfahrenWennToggledWegstrecke(..)
  , FortfahrenWenToggledWegstreckeReader(..)
  , hinzufügenWidgetWegstreckePackNew
  , hinzufügenWidgetWegstreckeRichtungPackNew
  , entferneHinzufügenWegstreckeWidgets
  , foldWegstreckeHinzufügen
      -- * Plan-Element
  , PlanElement(..)
  , MitTMVarPlanObjekt(..)
  , TMVarPlanObjektReader(..)
  , hinzufügenWidgetPlanPackNew
  , entferneHinzufügenPlanWidgets
  ) where

import Control.Applicative (ZipList(..))
import Control.Concurrent.STM (atomically, TVar, TMVar, putTMVar)
import Control.Lens ((^.), (^..))
import qualified Control.Lens as Lens
import Control.Monad.Reader.Class (MonadReader(ask), asks)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List.NonEmpty (NonEmpty())
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(erhalteName))
import Zug.Enums (Richtung(), ZugtypEither(), GeschwindigkeitEither)
import Zug.Language (Sprache())
import Zug.Objekt (Objekt, ObjektElement(zuObjekt), ObjektKlasse(..))
import Zug.UI.Base
       (StatusAllgemein(), bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, kontakte)
import Zug.UI.Gtk.Auswahl (auswahlRadioButtonNew)
import Zug.UI.Gtk.FortfahrenWennToggled
       (FortfahrenWennToggledVar, RegistrierterCheckButton, registrierterCheckButtonNew)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader())
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen
       (WidgetHinzufügen, HinzufügenZiel(..), BoxWegstreckeHinzufügen
      , CheckButtonWegstreckeHinzufügen, WegstreckeCheckButton(..), BoxPlanHinzufügen
      , ButtonPlanHinzufügen, widgetHinzufügenBoxPackNew, widgetHinzufügenContainerRemoveJust
      , widgetHinzufügenRegistrierterCheckButtonVoid)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp (WidgetsTyp(..), WidgetsTypReader)
import Zug.UI.StatusVar (StatusVar())

-- | Klasse für Gui-Darstellung von Typen, die zur Erstellung einer 'Wegstrecke' verwendet werden.
class (WidgetsTyp s) => WegstreckenElement s where
    -- | Auswahl-Typ beim erstellen einer Wegstrecke
    type CheckButtonAuswahl s

    type CheckButtonAuswahl s = Void

    -- | Getter auf 'RegistrierterCheckButton', ob 'StreckenObjekt' zu einer 'Wegstrecke' hinzugefügt werden soll
    getterWegstrecke :: Lens.Getter s (CheckButtonWegstreckeHinzufügen (CheckButtonAuswahl s) s)

    -- | Assoziierte 'BoxWegstreckeHinzufügen', in der 'MitRegistrierterCheckButton' gepackt ist.
    boxWegstrecke
        :: (ReaderConstraint s r) => ObjektTyp s -> Lens.Getter r (BoxWegstreckeHinzufügen s)

class MitFortfahrenWennToggledWegstrecke r o where
    fortfahrenWennToggledWegstrecke
        :: r
        -> FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid

class (MonadReader r m, MitFortfahrenWennToggledWegstrecke r o)
    => FortfahrenWenToggledWegstreckeReader r o m | m -> r where
    erhalteFortfahrenWennToggledWegstrecke
        :: m (FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid)

instance (MonadReader r m, MitFortfahrenWennToggledWegstrecke r o)
    => FortfahrenWenToggledWegstreckeReader r o m where
    erhalteFortfahrenWennToggledWegstrecke = asks fortfahrenWennToggledWegstrecke

-- | Erzeuge einen 'RegistrierterCheckButton' mit einem 'Label' für den Namen.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
hinzufügenWidgetWegstreckePackNew
    :: forall s r m.
    ( FortfahrenWenToggledWegstreckeReader r s m
    , WidgetsTypReader r s m
    , SpracheGuiReader r m
    , StreckenObjekt (ObjektTyp s)
    , WegstreckenElement s
    , MonadIO m
    )
    => ObjektTyp s
    -> TVar (Maybe [Sprache -> IO ()])
    -> m (CheckButtonWegstreckeHinzufügen Void s)
hinzufügenWidgetWegstreckePackNew objekt tvar = do
    fortfahrenWennToggledWegstrecke <- erhalteFortfahrenWennToggledWegstrecke
        :: m (FortfahrenWennToggledVar (StatusAllgemein s) (StatusVar s) WegstreckeCheckButtonVoid)
    reader <- ask
    let box = reader ^. boxWegstrecke objekt :: BoxWegstreckeHinzufügen s
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
    :: forall s r m.
    ( FortfahrenWenToggledWegstreckeReader r s m
    , WidgetsTypReader r s m
    , SpracheGuiReader r m
    , StreckenObjekt (ObjektTyp s)
    , WegstreckenElement s
    , MonadIO m
    )
    => ObjektTyp s
    -> NonEmpty Richtung
    -> TVar (Maybe [Sprache -> IO ()])
    -> m (CheckButtonWegstreckeHinzufügen Richtung s)
hinzufügenWidgetWegstreckeRichtungPackNew objekt richtungen tvar = do
    fortfahrenWennToggledWegstrecke <- erhalteFortfahrenWennToggledWegstrecke
        :: m (FortfahrenWennToggledVar (StatusAllgemein s) (StatusVar s) WegstreckeCheckButtonVoid)
    reader <- ask
    let box = reader ^. boxWegstrecke objekt :: BoxWegstreckeHinzufügen s
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

-- | Entferne 'Widget's zum Hinzufügen zu einer 'Wegstrecke' aus der entsprechenden Box
entferneHinzufügenWegstreckeWidgets
    :: forall s r m. (WegstreckenElement s, WidgetsTypReader r s m, MonadIO m) => s -> m ()
entferneHinzufügenWegstreckeWidgets wegsteckenElement = do
    box <- Lens.view (boxWegstrecke $ erhalteObjektTyp wegsteckenElement) <$> ask
        :: m (BoxWegstreckeHinzufügen s)
    widgetHinzufügenContainerRemoveJust box $ Just $ wegsteckenElement ^. getterWegstrecke

-- | Typ-unspezifischer 'RegistrierterCheckButton' zum hinzufügen einer Wegstrecke
type WegstreckeCheckButtonVoid =
    WidgetHinzufügen 'HinzufügenWegstrecke RegistrierterCheckButton Void

-- | Alle 'RegistrierterCheckButton' zum hinzufügen einer Wegstrecke im aktuellen 'StatusGui'
foldWegstreckeHinzufügen
    :: ( WegstreckenElement (ZugtypEither (GeschwindigkeitEither (BG o)))
       , WegstreckenElement (ST o)
       , WegstreckenElement (ZugtypEither (WE o))
       , WegstreckenElement (KU o)
       , WegstreckenElement (KO o)
       )
    => Lens.Fold (StatusAllgemein o) WegstreckeCheckButtonVoid
foldWegstreckeHinzufügen = Lens.folding registrierteCheckButtons
    where
        registrierteCheckButtons
            :: ( WegstreckenElement (ZugtypEither (GeschwindigkeitEither (BG o)))
               , WegstreckenElement (ST o)
               , WegstreckenElement (ZugtypEither (WE o))
               , WegstreckenElement (KU o)
               , WegstreckenElement (KO o)
               )
            => StatusAllgemein o
            -> [WegstreckeCheckButtonVoid]
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
            ++ map
                (widgetHinzufügenRegistrierterCheckButtonVoid . Lens.view getterWegstrecke)
                (status ^. kontakte)

-- | Klasse für Gui-Darstellungen von Typen, die zur Erstellung eines 'Plan's verwendet werden.
class (WidgetsTyp s) => PlanElement s where
    -- | Faltung auf 'Gtk.Button's (falls vorhanden), welches 'StreckenObjekt' für eine 'Aktion' verwendet werden soll
    foldPlan :: Lens.Fold s (Maybe (ButtonPlanHinzufügen s))

    -- | Aller assoziierten 'BoxPlanHinzufügen', in denen jeweiliger 'ButtonPlanHinzufügen' gepackt ist.
    -- Die Reihenfolge muss zum Ergebnis von 'foldPlan' passen.
    -- Wird für 'entferneHinzufügenPlanWidgets' benötigt.
    boxenPlan :: (ReaderConstraint s r) => ObjektTyp s -> Lens.Fold r (BoxPlanHinzufügen s)

class MitTMVarPlanObjekt r where
    tmvarPlanObjekt :: r -> TMVar (Maybe Objekt)

class (MonadReader r m, MitTMVarPlanObjekt r) => TMVarPlanObjektReader r m | m -> r where
    erhalteTMVarPlanObjekt :: m (TMVar (Maybe Objekt))

instance (MonadReader r m, MitTMVarPlanObjekt r) => TMVarPlanObjektReader r m where
    erhalteTMVarPlanObjekt = asks tmvarPlanObjekt

-- | Füge einen Knopf mit dem Namen zur Box hinzu. Beim drücken wird die 'TMVar' mit dem Objekt gefüllt.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
hinzufügenWidgetPlanPackNew
    :: ( TMVarPlanObjektReader r m
       , SpracheGuiReader r m
       , StreckenObjekt (ObjektTyp o)
       , ObjektElement (ObjektTyp o)
       , MonadIO m
       )
    => BoxPlanHinzufügen o
    -> ObjektTyp o
    -> TVar (Maybe [Sprache -> IO ()])
    -> m (ButtonPlanHinzufügen o)
hinzufügenWidgetPlanPackNew box objekt tvar = do
    tmvarPlanObjekt <- erhalteTMVarPlanObjekt
    widgetHinzufügenBoxPackNew box
        $ buttonNewWithEventLabel (Just tvar) (const $ erhalteName objekt)
        $ atomically
        $ putTMVar tmvarPlanObjekt
        $ Just
        $ zuObjekt objekt

-- | Entferne 'Widget's zum 'Plan' erstellen aus den entsprechenden 'Box'en.
entferneHinzufügenPlanWidgets
    :: forall s r m. (PlanElement s, WidgetsTypReader r s m, MonadIO m) => s -> m ()
entferneHinzufügenPlanWidgets planElement = do
    boxenPlan <- Lens.toListOf (boxenPlan $ erhalteObjektTyp planElement) <$> ask
        :: m [BoxPlanHinzufügen s]
    sequence_
        $ widgetHinzufügenContainerRemoveJust <$> ZipList boxenPlan
        <*> ZipList (planElement ^.. foldPlan)