{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
#endif

{-|
Description : Erstellen eines Assistant zum Hinzufügen eines 'StreckenObjekt'es.
-}
module Zug.UI.Gtk.Fenster.AssistantHinzufuegen
  (
#ifdef ZUGKONTROLLEGUI
    assistantHinzufügenNew
  , AssistantHinzufügen()
  , hinzufügenErgebnis
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
-- Bibliotheken
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty())
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk

-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(), Streckenabschnitt(..)
                    , StreckenabschnittKlasse(), Weiche(..), Kupplung(..), KupplungKlasse()
                    , Wegstrecke(..), WegstreckeKlasse(), Wartezeit(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), Richtung(..)
                , unterstützteRichtungen, Fahrtrichtung(..), Strom(..))
import qualified Zug.Language as Language
import Zug.Language (Sprache(), MitSprache(..), Anzeige(..), (<:>))
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (Plan(..), Aktion(..), AktionWegstrecke(..), AktionBahngeschwindigkeit(..)
               , AktionStreckenabschnitt(..), AktionWeiche(..), AktionKupplung(..))
import Zug.UI.Gtk.Anschluss (AnschlussAuswahlWidget)
import Zug.UI.Gtk.Auswahl (AuswahlWidget)
import Zug.UI.Gtk.FortfahrenWennToggled (RegistrierterCheckButton)
import Zug.UI.Gtk.Hilfsfunktionen (NameAuswahlWidget)
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.StreckenObjekt
       (ObjektGui, StatusVarGui, DynamischeWidgets(..), DynamischeWidgetsReader(..)
      , BoxPlanHinzufügen, WegstreckenElement(..), WegstreckeCheckButton(), WidgetsTyp(..)
      , widgetHinzufügenToggled, widgetHinzufügenAktuelleAuswahl
      , widgetHinzufügenContainerGefüllt, BGWidgets, WEWidgets)
import Zug.UI.Gtk.ZugtypSpezifisch (zugtypSpezifischNew, zugtypSpezifischButtonNew)
import Zug.UI.StatusVar (readStatusVar, StatusVarReader(..))
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes)

-- | Seiten des Hinzufügen-'Assistant'
data AssistantHinzufügen =
    AssistantHinzufügen
    { window :: Gtk.Window
    , notebook :: Gtk.Notebook
    , indexAuswertfunktion :: [(Int, HinzufügenSeite)]
    }
    deriving (Eq)

instance MitWidget AssistantHinzufügen where
    erhalteWidget :: AssistantHinzufügen -> Gtk.Widget
    erhalteWidget = Gtk.toWidget . window

-- | Seiten des Hinzufügen-'Assistant'
data HinzufügenSeite
    = HinzufügenSeiteBahngeschwindigkeit
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , geschwindigkeitAuswahl :: AnschlussAuswahlWidget
            -- Lego
          , fahrtrichtungsAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteStreckenabschnitt
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , stromAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteWeiche
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
            -- Märklin
          , märklinRichtungsAuswahl
                :: NonEmpty (Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget)
            -- Lego
          , legoRichtungsAuswahl :: AnschlussAuswahlWidget
          , legoRichtungenAuswahl :: AuswahlWidget (Richtung, Richtung)
          }
    | HinzufügenSeiteKupplung
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , kupplungsAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteWegstrecke { widget :: Gtk.Widget, nameAuswahl :: NameAuswahlWidget }
    | HinzufügenSeitePlan
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , tvarAktionen :: TVar (Warteschlange Aktion)
          , checkButtonDauerschleife :: Gtk.CheckButton
          }
    deriving (Eq)

instance MitWidget HinzufügenSeite where
    erhalteWidget :: HinzufügenSeite -> Gtk.Widget
    erhalteWidget = Gtk.toWidget . vBox

-- | Erhalte das Ergebnis einer 'HinzufügenSeite'.
hinzufügenErgebnis
    :: (StatusVarReader r ObjektGui m, MonadIO m) => AssistantHinzufügen -> m Objekt
hinzufügenErgebnis assistantHinzufügen = _undefined --TODO

assistantHinzufügenNew :: (MonadIO m) => m AssistantHinzufügen
assistantHinzufügenNew = _undefined --TODO
#endif































