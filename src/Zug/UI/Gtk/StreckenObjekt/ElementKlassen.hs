{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#endif

module Zug.UI.Gtk.StreckenObjekt.ElementKlassen
  (
#ifdef ZUGKONTROLLEGUI
    -- * Wegstrecken-Element
    WegstreckenElement(..)
  , WegstreckeCheckButtonVoid
  , MitFortfahrenWennToggledWegstrecke(..)
  , FortfahrenWennToggledWegstreckeReader(..)
  , hinzufügenWidgetWegstreckePackNew
  , hinzufügenWidgetWegstreckeRichtungPackNew
  , entferneHinzufügenWegstreckeWidgets
  , checkButtonsWegstreckeHinzufügen
      -- * Plan-Element
  , PlanElement(..)
  , MitTMVarPlanObjekt(..)
  , TMVarPlanObjektReader(..)
  , hinzufügenWidgetPlanPackNew
  , entferneHinzufügenPlanWidgets
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Applicative (ZipList(..))
import Control.Concurrent.STM (atomically, TMVar, putTMVar)
import Control.Monad.Reader.Class (MonadReader(ask, reader))
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List.NonEmpty (NonEmpty())
import qualified Data.Text as Text
import Data.Void (Void)
import qualified GI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(erhalteName))
import Zug.Enums (Richtung(), ZugtypEither(), GeschwindigkeitEither)
import Zug.Objekt (Objekt, ObjektElement(zuObjekt, ObjektTyp, zuObjektTyp), ObjektKlasse(..))
import Zug.UI.Base
       (StatusAllgemein(), bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, kontakte)
import Zug.UI.Gtk.Auswahl (auswahlRadioButtonNew)
import Zug.UI.Gtk.FortfahrenWennToggled
       (FortfahrenWennToggledVar, RegistrierterCheckButton, registrierterCheckButtonNew)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufuegen
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

    -- | 'RegistrierterCheckButton' im 'Zug.UI.Gtk.AssistantHinzufuegen.AssistantHinzufügen'.
    -- Bestimmt ob ein 'StreckenObjekt' zu einer 'Wegstrecke' hinzugefügt werden soll.
    checkButtonWegstrecke :: s -> CheckButtonWegstreckeHinzufügen (CheckButtonAuswahl s) s

    -- | Assoziierte 'BoxWegstreckeHinzufügen', in der 'MitRegistrierterCheckButton' gepackt ist.
    boxWegstrecke :: (ReaderConstraint s r) => ObjektTyp s -> r -> BoxWegstreckeHinzufügen s

class MitFortfahrenWennToggledWegstrecke r o where
    fortfahrenWennToggledWegstrecke
        :: r
        -> FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid

class (MonadReader r m, MitFortfahrenWennToggledWegstrecke r o)
    => FortfahrenWennToggledWegstreckeReader r o m | m -> r where
    erhalteFortfahrenWennToggledWegstrecke
        :: m (FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid)
    erhalteFortfahrenWennToggledWegstrecke = reader fortfahrenWennToggledWegstrecke

instance (MonadReader r m, MitFortfahrenWennToggledWegstrecke r o)
    => FortfahrenWennToggledWegstreckeReader r o m

-- | Erzeuge einen 'RegistrierterCheckButton' mit einem 'Label' für den Namen.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
hinzufügenWidgetWegstreckePackNew
    :: forall s o r m.
    ( SpracheGuiReader r m
    , WidgetsTypReader r s m
    , StreckenObjekt (ObjektTyp s)
    , WegstreckenElement s
    , MonadIO m
    )
    => ObjektTyp s
    -> TVarSprachewechselAktionen
    -> FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid
    -> m (CheckButtonWegstreckeHinzufügen Void s)
hinzufügenWidgetWegstreckePackNew objekt tvar fortfahrenWennToggled = do
    box <- boxWegstrecke objekt <$> ask :: m (BoxWegstreckeHinzufügen s)
    widgetHinzufügenBoxPackNew box
        $ WegstreckeCheckButton
        <$> registrierterCheckButtonNew
            (Just tvar)
            (const $ erhalteName objekt)
            fortfahrenWennToggled

-- | Erzeuge einen 'RegistrierterCheckButton'.
-- Dieser enthält ein 'Label' für den Namen und einem 'AuswahlWidget' für die übergebenen 'Richtung'en.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
hinzufügenWidgetWegstreckeRichtungPackNew
    :: forall s o r m.
    ( SpracheGuiReader r m
    , WidgetsTypReader r s m
    , StreckenObjekt (ObjektTyp s)
    , WegstreckenElement s
    , MonadIO m
    )
    => ObjektTyp s
    -> NonEmpty Richtung
    -> TVarSprachewechselAktionen
    -> FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid
    -> m (CheckButtonWegstreckeHinzufügen Richtung s)
hinzufügenWidgetWegstreckeRichtungPackNew objekt richtungen tvar fortfahrenWennToggled = do
    box <- boxWegstrecke objekt <$> ask :: m (BoxWegstreckeHinzufügen s)
    let justTVar = Just tvar
    widgetHinzufügenBoxPackNew box $ do
        hBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
        wcbrWidget <- erhalteWidget hBox
        wcbrRegistrierterCheckButton <- boxPackWidgetNewDefault hBox
            $ registrierterCheckButtonNew
                justTVar
                (const $ erhalteName objekt)
                fortfahrenWennToggled
        wcbrRichtungsAuswahl <- boxPackWidgetNewDefault hBox
            $ auswahlRadioButtonNew richtungen justTVar
            $ const Text.empty
        pure
            WegstreckeCheckButtonRichtung
            { wcbrWidget
            , wcbrRegistrierterCheckButton
            , wcbrRichtungsAuswahl
            }

-- | Entferne 'Widget's zum Hinzufügen zu einer 'Wegstrecke' aus der entsprechenden Box
entferneHinzufügenWegstreckeWidgets
    :: forall s r m. (WegstreckenElement s, WidgetsTypReader r s m, MonadIO m) => s -> m ()
entferneHinzufügenWegstreckeWidgets wegsteckenElement = do
    box <- boxWegstrecke (zuObjektTyp wegsteckenElement) <$> ask :: m (BoxWegstreckeHinzufügen s)
    widgetHinzufügenContainerRemoveJust box $ Just $ checkButtonWegstrecke wegsteckenElement

-- | Typ-unspezifischer 'RegistrierterCheckButton' zum hinzufügen einer Wegstrecke
type WegstreckeCheckButtonVoid =
    WidgetHinzufügen 'HinzufügenWegstrecke RegistrierterCheckButton Void

-- | Alle 'RegistrierterCheckButton' zum hinzufügen einer Wegstrecke im aktuellen 'StatusGui'.
checkButtonsWegstreckeHinzufügen
    :: ( WegstreckenElement (ZugtypEither (GeschwindigkeitEither (BG o)))
       , WegstreckenElement (ST o)
       , WegstreckenElement (ZugtypEither (WE o))
       , WegstreckenElement (KU o)
       , WegstreckenElement (KO o)
       )
    => StatusAllgemein o
    -> [WegstreckeCheckButtonVoid]
checkButtonsWegstreckeHinzufügen status =
    map
        (widgetHinzufügenRegistrierterCheckButtonVoid . checkButtonWegstrecke)
        (bahngeschwindigkeiten status)
    ++ map
        (widgetHinzufügenRegistrierterCheckButtonVoid . checkButtonWegstrecke)
        (streckenabschnitte status)
    ++ map (widgetHinzufügenRegistrierterCheckButtonVoid . checkButtonWegstrecke) (weichen status)
    ++ map
        (widgetHinzufügenRegistrierterCheckButtonVoid . checkButtonWegstrecke)
        (kupplungen status)
    ++ map
        (widgetHinzufügenRegistrierterCheckButtonVoid . checkButtonWegstrecke)
        (kontakte status)

-- | Klasse für Gui-Darstellungen von Typen, die zur Erstellung eines 'Plan's verwendet werden.
class (WidgetsTyp s) => PlanElement s where
    -- | 'Gtk.Button's (falls vorhanden) im 'Zug.UI.Gtk.AssistantHinzufuegen.AssistantHinzufügen'.
    -- Bestimmt welches 'StreckenObjekt' für eine 'Aktion' verwendet werden soll.
    buttonsPlan :: s -> [Maybe (ButtonPlanHinzufügen s)]

    -- | Aller assoziierten 'BoxPlanHinzufügen', in denen jeweiliger 'ButtonPlanHinzufügen' gepackt ist.
    -- Die Reihenfolge muss zum Ergebnis von 'buttonsPlan' passen.
    -- Wird für 'entferneHinzufügenPlanWidgets' benötigt.
    boxenPlan :: (ReaderConstraint s r) => ObjektTyp s -> r -> [BoxPlanHinzufügen s]

class MitTMVarPlanObjekt r where
    tmvarPlanObjekt :: r -> TMVar (Maybe Objekt)

class (MonadReader r m, MitTMVarPlanObjekt r) => TMVarPlanObjektReader r m | m -> r where
    erhalteTMVarPlanObjekt :: m (TMVar (Maybe Objekt))
    erhalteTMVarPlanObjekt = reader tmvarPlanObjekt

instance (MonadReader r m, MitTMVarPlanObjekt r) => TMVarPlanObjektReader r m

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
    -> TVarSprachewechselAktionen
    -> m (ButtonPlanHinzufügen o)
hinzufügenWidgetPlanPackNew box objekt tvar = do
    readerTMVarPlanObjekt <- erhalteTMVarPlanObjekt
    widgetHinzufügenBoxPackNew box
        $ buttonNewWithEventLabel (Just tvar) (const $ erhalteName objekt)
        $ atomically
        $ putTMVar readerTMVarPlanObjekt
        $ Just
        $ zuObjekt objekt

-- | Entferne 'Widget's zum 'Plan' erstellen aus den entsprechenden 'Box'en.
entferneHinzufügenPlanWidgets
    :: forall s r m. (PlanElement s, WidgetsTypReader r s m, MonadIO m) => s -> m ()
entferneHinzufügenPlanWidgets planElement = do
    boxenPlanHinzufügen <- boxenPlan (zuObjektTyp planElement) <$> ask :: m [BoxPlanHinzufügen s]
    sequence_
        $ widgetHinzufügenContainerRemoveJust <$> ZipList boxenPlanHinzufügen
        <*> ZipList (buttonsPlan planElement)
#endif
--
