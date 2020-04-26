{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
#endif

{-|
Description : Erstelle zusammengesetzte Widgets.

Allgemeine Hilfsfunktionen zum erstellen neuer Widgets
-}
module Zug.UI.Gtk.StreckenObjekt
  (
#ifdef ZUGKONTROLLEGUI
    -- * Spezifisches StreckenObjekt darstellen
    -- ** Bahngeschwindigkeit
    BGWidgets()
  , bahngeschwindigkeitPackNew
  , BGWidgetsBoxen(..)
  , MitBGWidgetsBoxen(..)
  , BGWidgetsBoxenReader(..)
    -- ** Streckenabschnitt
  , STWidgets()
  , streckenabschnittPackNew
  , STWidgetsBoxen(..)
  , MitSTWidgetsBoxen(..)
  , STWidgetsBoxenReader(..)
    -- ** Weiche
  , WEWidgets()
  , weichePackNew
  , WEWidgetsBoxen(..)
  , MitWEWidgetsBoxen(..)
  , WEWidgetsBoxenReader(..)
    -- ** Kupplung
  , KUWidgets()
  , kupplungPackNew
  , KUWidgetsBoxen(..)
  , MitKUWidgetsBoxen(..)
  , KUWidgetsBoxenReader(..)
    -- ** Kontakt
  , KOWidgets()
  , kontaktPackNew
  , KOWidgetsBoxen(..)
  , MitKOWidgetsBoxen(..)
  , KOWidgetsBoxenReader(..)
    -- ** Wegstrecke
  , WSWidgets()
  , wegstreckePackNew
  , WSWidgetsBoxen(..)
  , MitWSWidgetsBoxen(..)
  , WSWidgetsBoxenReader(..)
    -- ** Plan
  , PLWidgets()
  , planPackNew
  , PLWidgetsBoxen(..)
  , MitPLWidgetsBoxen(..)
  , PLWidgetsBoxenReader(..)
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
  , MitWindowMain(..)
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
import Control.Concurrent.STM (atomically, TMVar)
import Control.Lens ((^.))
import Control.Monad.Reader.Class (MonadReader(), asks)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..), GeschwindigkeitEither())
import Zug.Objekt (ObjektKlasse(..), Objekt, ObjektAllgemein())
import Zug.UI.Base (StatusAllgemein, MStatusAllgemeinT, MStatusAllgemein, IOStatusAllgemein
                  , ObjektReader(), ReaderFamilie, TVarMaps(), MitTVarMaps(..), sprache)
import Zug.UI.Befehl (BefehlAllgemein())
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledVar)
import Zug.UI.Gtk.SpracheGui (SpracheGui, MitSpracheGui(..))
import Zug.UI.Gtk.StreckenObjekt.BGWidgets
       (BGWidgets, bahngeschwindigkeitPackNew, BGWidgetsBoxen(..), MitBGWidgetsBoxen(..), BGWidgetsBoxenReader(..))
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen (WegstreckeCheckButtonVoid)
import Zug.UI.Gtk.StreckenObjekt.KOWidgets
       (KOWidgets, kontaktPackNew, KOWidgetsBoxen(..), MitKOWidgetsBoxen(..), KOWidgetsBoxenReader(..))
import Zug.UI.Gtk.StreckenObjekt.KUWidgets
       (KUWidgets, kupplungPackNew, KUWidgetsBoxen(..), MitKUWidgetsBoxen(..), KUWidgetsBoxenReader(..))
import Zug.UI.Gtk.StreckenObjekt.PLWidgets
       (PLWidgets, planPackNew, PLWidgetsBoxen(..), MitPLWidgetsBoxen(..), PLWidgetsBoxenReader(..), MitWindowMain(..))
import Zug.UI.Gtk.StreckenObjekt.STWidgets
       (STWidgets, streckenabschnittPackNew, STWidgetsBoxen(..), MitSTWidgetsBoxen(..), STWidgetsBoxenReader(..))
import Zug.UI.Gtk.StreckenObjekt.WEWidgets
       (WEWidgets, weichePackNew, WEWidgetsBoxen(..), MitWEWidgetsBoxen(..), WEWidgetsBoxenReader(..))
import Zug.UI.Gtk.StreckenObjekt.WSWidgets
       (WSWidgets, wegstreckePackNew, WSWidgetsBoxen(..), MitWSWidgetsBoxen(..), WSWidgetsBoxenReader(..))
import Zug.UI.StatusVar (StatusVar, MitStatusVar(..), StatusVarReader(), tryReadStatusVar)

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
    }

-- | Klasse für Typen mit 'DynamischeWidgets'
class MitDynamischeWidgets r where
    dynamischeWidgets :: r -> DynamischeWidgets

instance MitDynamischeWidgets DynamischeWidgets where
    dynamischeWidgets :: DynamischeWidgets -> DynamischeWidgets
    dynamischeWidgets = id

-- | Abkürzung für Funktionen, die 'DynamischeWidgets' benötigen
class (MonadReader r m, MitDynamischeWidgets r) => DynamischeWidgetsReader r m | m -> r where
    erhalteDynamischeWidgets :: m DynamischeWidgets
    erhalteDynamischeWidgets = asks dynamischeWidgets

instance (MonadReader r m, MitDynamischeWidgets r) => DynamischeWidgetsReader r m

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

-- | Lese die 'SpracheGui' aus einer 'StatusVarGui'.
readSpracheGui :: (MonadIO m) => StatusVarGui -> m SpracheGui
readSpracheGui statusVar = liftIO $ atomically (tryReadStatusVar statusVar) >>= pure . \case
    (Left status) -> status ^. sprache
    (Right spracheGui) -> spracheGui
#endif
--
