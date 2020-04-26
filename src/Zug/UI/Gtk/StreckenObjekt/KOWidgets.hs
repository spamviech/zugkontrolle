{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Zug.UI.Gtk.StreckenObjekt.KOWidgets (KOWidgets, kontaktPackNew) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Trans (MonadIO())
import qualified Data.Aeson as Aeson
import Data.Void (Void)
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(..), Kontakt(..), KontaktKlasse(..))
import Zug.Language (Sprache())
import qualified Zug.Language as Language
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen
       (Kategorie(..), KategorieText(..), CheckButtonWegstreckeHinzufügen, ButtonPlanHinzufügen)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp (WidgetsTyp(..), EventAusführen(EventAusführen))

instance Kategorie KOWidgets where
    kategorie :: KategorieText KOWidgets
    kategorie = KategorieText Language.kontakte

data KOWidgets =
    KOWidgets
    { ko :: Kontakt
    , koWidget :: Gtk.VBox
    , koFunctionBox :: Gtk.HBox
    , koHinzWS :: CheckButtonWegstreckeHinzufügen Void KOWidgets
    , koHinzPL :: ButtonPlanHinzufügen KOWidgets
    , koTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , koTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

kontaktPackNew :: (MonadIO m) => Kontakt -> m KOWidgets
kontaktPackNew = _undefined --TODO

instance Aeson.ToJSON KOWidgets where
    toJSON :: KOWidgets -> Aeson.Value
    toJSON = _undefined --TODO