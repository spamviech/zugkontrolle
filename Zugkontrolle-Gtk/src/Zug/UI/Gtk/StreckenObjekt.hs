{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description : Erstelle zusammengesetzte Widgets.

Allgemeine Hilfsfunktionen zum erstellen neuer Widgets
-}
module Zug.UI.Gtk.StreckenObjekt
  ( -- * Spezifisches StreckenObjekt darstellen
    -- ** Bahngeschwindigkeit
    BGWidgets()
  , bahngeschwindigkeitPackNew
  , BGWidgetsBoxen(..)
    -- ** Streckenabschnitt
  , STWidgets()
  , streckenabschnittPackNew
  , STWidgetsBoxen(..)
    -- ** Weiche
  , WEWidgets()
  , weichePackNew
  , WEWidgetsBoxen(..)
    -- ** Kupplung
  , KUWidgets()
  , kupplungPackNew
  , KUWidgetsBoxen(..)
    -- ** Kontakt
  , KOWidgets()
  , kontaktPackNew
  , KOWidgetsBoxen(..)
    -- ** Wegstrecke
  , WSWidgets()
  , wegstreckePackNew
  , WSWidgetsBoxen(..)
    -- ** Plan
  , PLWidgets()
  , PlanGui
  , AktionGui
  , planPackNew
  , planGui
  , PLWidgetsBoxen(..)
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
    -- * Widgets für den Hinzufügen-Dialog
  , WidgetsTyp(..)
  , WidgetHinzufügen()
    -- ** Wegstrecke
  , WegstreckenElement(..)
  , BoxWegstreckeHinzufügen
  , boxWegstreckeHinzufügenNew
  , WegstreckeCheckButton()
  , WegstreckeCheckButtonVoid
  , checkButtonsWegstreckeHinzufügen
    -- ** Plan
  , PlanElement(..)
  , BoxPlanHinzufügen
  , boxPlanHinzufügenNew
  , widgetHinzufügenToggled
  , widgetHinzufügenSetToggled
  , widgetHinzufügenAktuelleAuswahl
  , widgetHinzufügenSetzeAuswahl
  ) where

import Control.Concurrent.STM (atomically, TMVar)
import Control.Monad.Reader.Class (MonadReader(), asks)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Version (Version())
import qualified GI.Gtk as Gtk

import Zug.Objekt (Objekt)
import Zug.Options (MitVersion(..))
import Zug.UI.Base (StatusAllgemein, MStatusAllgemeinT, MStatusAllgemein, IOStatusAllgemein
                  , ObjektReader(), ReaderFamilie, TVarMaps(), MitTVarMaps(..), sprache)
import Zug.UI.Befehl (BefehlAllgemein())
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledVar)
import Zug.UI.Gtk.SpracheGui (SpracheGui, MitSpracheGui(..))
import Zug.UI.Gtk.StreckenObjekt.BGWidgets
       (BGWidgets, bahngeschwindigkeitPackNew, BGWidgetsBoxen(..), MitBGWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (MitFortfahrenWennToggledWegstrecke(..), MitTMVarPlanObjekt(..), WegstreckenElement(..)
      , WegstreckeCheckButtonVoid, checkButtonsWegstreckeHinzufügen, PlanElement(..))
import Zug.UI.Gtk.StreckenObjekt.KOWidgets
       (KOWidgets, kontaktPackNew, KOWidgetsBoxen(..), MitKOWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.KUWidgets
       (KUWidgets, kupplungPackNew, KUWidgetsBoxen(..), MitKUWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.PLWidgets
       (PLWidgets, PlanGui, AktionGui, planPackNew, planGui, PLWidgetsBoxen(..)
      , MitPLWidgetsBoxen(..), MitWindowMain(..), ObjektGui)
import Zug.UI.Gtk.StreckenObjekt.STWidgets
       (STWidgets, streckenabschnittPackNew, STWidgetsBoxen(..), MitSTWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.WEWidgets
       (WEWidgets, weichePackNew, WEWidgetsBoxen(..), MitWEWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.WSWidgets
       (WSWidgets, wegstreckePackNew, WSWidgetsBoxen(..), MitWSWidgetsBoxen(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufuegen
       (WidgetHinzufügen(), BoxWegstreckeHinzufügen, boxWegstreckeHinzufügenNew
      , WegstreckeCheckButton(), BoxPlanHinzufügen, boxPlanHinzufügenNew, widgetHinzufügenToggled
      , widgetHinzufügenSetToggled, widgetHinzufügenAktuelleAuswahl, widgetHinzufügenSetzeAuswahl)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp (WidgetsTyp(..), MitAktionBearbeiten(..))
import Zug.UI.StatusVar (StatusVar, MitStatusVar(..), StatusVarReader(), tryReadStatusVar)

-- * Sammel-Typ um dynamische Widgets zu speichern
-- | Sammel-Typ spezialisiert auf Gui-Typen
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

-- | Sammlung aller Widgets, welche während der Laufzeit benötigt werden.
data DynamischeWidgets =
    DynamischeWidgets
    { dynWindowMain :: Gtk.Window
    , dynBGWidgetsBoxen :: BGWidgetsBoxen
    , dynSTWidgetsBoxen :: STWidgetsBoxen
    , dynWEWidgetsBoxen :: WEWidgetsBoxen
    , dynKUWidgetsBoxen :: KUWidgetsBoxen
    , dynKOWidgetsBoxen :: KOWidgetsBoxen
    , dynWSWidgetsBoxen :: WSWidgetsBoxen
    , dynPLWidgetsBoxen :: PLWidgetsBoxen
    , dynFortfahrenWennToggledWegstrecke
          :: FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid
    , dynTMVarPlanObjekt :: TMVar (Maybe Objekt)
    , dynAktionBearbeiten :: Objekt -> IO ()
    }

-- | Klasse für Typen mit 'DynamischeWidgets'
class MitDynamischeWidgets r where
    dynamischeWidgets :: r -> DynamischeWidgets

instance MitDynamischeWidgets DynamischeWidgets where
    dynamischeWidgets :: DynamischeWidgets -> DynamischeWidgets
    dynamischeWidgets = id

instance MitWindowMain DynamischeWidgets where
    windowMain :: DynamischeWidgets -> Gtk.Window
    windowMain = dynWindowMain

instance MitAktionBearbeiten DynamischeWidgets where
    aktionBearbeiten :: DynamischeWidgets -> Objekt -> IO ()
    aktionBearbeiten = dynAktionBearbeiten

instance MitFortfahrenWennToggledWegstrecke DynamischeWidgets ObjektGui where
    fortfahrenWennToggledWegstrecke
        :: DynamischeWidgets
        -> FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid
    fortfahrenWennToggledWegstrecke = dynFortfahrenWennToggledWegstrecke

instance MitTMVarPlanObjekt DynamischeWidgets where
    tmvarPlanObjekt :: DynamischeWidgets -> TMVar (Maybe Objekt)
    tmvarPlanObjekt = dynTMVarPlanObjekt

instance MitBGWidgetsBoxen DynamischeWidgets where
    bgWidgetsBoxen :: DynamischeWidgets -> BGWidgetsBoxen
    bgWidgetsBoxen = dynBGWidgetsBoxen

instance MitSTWidgetsBoxen DynamischeWidgets where
    stWidgetsBoxen :: DynamischeWidgets -> STWidgetsBoxen
    stWidgetsBoxen = dynSTWidgetsBoxen

instance MitWEWidgetsBoxen DynamischeWidgets where
    weWidgetsBoxen :: DynamischeWidgets -> WEWidgetsBoxen
    weWidgetsBoxen = dynWEWidgetsBoxen

instance MitKUWidgetsBoxen DynamischeWidgets where
    kuWidgetsBoxen :: DynamischeWidgets -> KUWidgetsBoxen
    kuWidgetsBoxen = dynKUWidgetsBoxen

instance MitKOWidgetsBoxen DynamischeWidgets where
    koWidgetsBoxen :: DynamischeWidgets -> KOWidgetsBoxen
    koWidgetsBoxen = dynKOWidgetsBoxen

instance MitWSWidgetsBoxen DynamischeWidgets where
    wsWidgetsBoxen :: DynamischeWidgets -> WSWidgetsBoxen
    wsWidgetsBoxen = dynWSWidgetsBoxen

instance MitPLWidgetsBoxen DynamischeWidgets where
    plWidgetsBoxen :: DynamischeWidgets -> PLWidgetsBoxen
    plWidgetsBoxen = dynPLWidgetsBoxen

-- | Abkürzung für Funktionen, die 'DynamischeWidgets' benötigen
class (MonadReader r m, MitDynamischeWidgets r) => DynamischeWidgetsReader r m | m -> r where
    erhalteDynamischeWidgets :: m DynamischeWidgets
    erhalteDynamischeWidgets = asks dynamischeWidgets

instance (MonadReader r m, MitDynamischeWidgets r) => DynamischeWidgetsReader r m

type ReaderGui = (TVarMaps, DynamischeWidgets, StatusVar ObjektGui, Version)

type instance ReaderFamilie ObjektGui = ReaderGui

instance MitTVarMaps ReaderGui where
    tvarMaps :: ReaderGui -> TVarMaps
    tvarMaps (maps, _dynamischeWidgets, _tmvarStatus, _version) = maps

instance MitDynamischeWidgets ReaderGui where
    dynamischeWidgets :: ReaderGui -> DynamischeWidgets
    dynamischeWidgets (_tvarMaps, dynWidgets, _tmvarStatus, _version) = dynWidgets

instance MitStatusVar ReaderGui ObjektGui where
    statusVar :: ReaderGui -> StatusVar ObjektGui
    statusVar (_tvarMaps, _dynamischeWidgets, var, _version) = var

instance MitSpracheGui ReaderGui where
    spracheGui :: (MonadIO m) => ReaderGui -> m SpracheGui
    spracheGui (_tvarMaps, _dynamischeWidgets, var, _version) = readSpracheGui var

instance MitVersion ReaderGui where
    version :: ReaderGui -> Version
    version (_tvarMaps, _dynamischeWidgets, _var, v) = v

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitWindowMain r where
    windowMain :: r -> Gtk.Window
    windowMain = dynWindowMain . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitAktionBearbeiten r where
    aktionBearbeiten :: r -> Objekt -> IO ()
    aktionBearbeiten = aktionBearbeiten . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r)
    => MitFortfahrenWennToggledWegstrecke r ObjektGui where
    fortfahrenWennToggledWegstrecke
        :: r -> FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid
    fortfahrenWennToggledWegstrecke = dynFortfahrenWennToggledWegstrecke . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitTMVarPlanObjekt r where
    tmvarPlanObjekt :: r -> TMVar (Maybe Objekt)
    tmvarPlanObjekt = dynTMVarPlanObjekt . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitBGWidgetsBoxen r where
    bgWidgetsBoxen :: r -> BGWidgetsBoxen
    bgWidgetsBoxen = dynBGWidgetsBoxen . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitSTWidgetsBoxen r where
    stWidgetsBoxen :: r -> STWidgetsBoxen
    stWidgetsBoxen = dynSTWidgetsBoxen . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitWEWidgetsBoxen r where
    weWidgetsBoxen :: r -> WEWidgetsBoxen
    weWidgetsBoxen = dynWEWidgetsBoxen . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitKUWidgetsBoxen r where
    kuWidgetsBoxen :: r -> KUWidgetsBoxen
    kuWidgetsBoxen = dynKUWidgetsBoxen . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitKOWidgetsBoxen r where
    koWidgetsBoxen :: r -> KOWidgetsBoxen
    koWidgetsBoxen = dynKOWidgetsBoxen . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitWSWidgetsBoxen r where
    wsWidgetsBoxen :: r -> WSWidgetsBoxen
    wsWidgetsBoxen = dynWSWidgetsBoxen . dynamischeWidgets

instance {-# OVERLAPPABLE #-}(MitDynamischeWidgets r) => MitPLWidgetsBoxen r where
    plWidgetsBoxen :: r -> PLWidgetsBoxen
    plWidgetsBoxen = dynPLWidgetsBoxen . dynamischeWidgets

-- | Lese die 'SpracheGui' aus einer 'StatusVarGui'.
readSpracheGui :: (MonadIO m) => StatusVarGui -> m SpracheGui
readSpracheGui = liftIO . fmap erhalteSprache . atomically . tryReadStatusVar
    where
        erhalteSprache :: Either StatusGui SpracheGui -> SpracheGui
        erhalteSprache (Left status) = sprache status
        erhalteSprache (Right sp) = sp
