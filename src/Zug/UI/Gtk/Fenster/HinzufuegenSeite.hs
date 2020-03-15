{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE InstanceSigs #-}
#endif

module Zug.UI.Gtk.Fenster.HinzufuegenSeite
  (
#ifdef ZUGKONTROLLEGUI
    HinzufügenSeite(..)
  , seiteErgebnis
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
-- Bibliotheken
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty())
import qualified Graphics.UI.Gtk as Gtk

-- Abhängigkeit von anderen Modulen
import Zug.Enums (Richtung(), unterstützteRichtungen)
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
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes)

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

seiteErgebnis :: (MonadIO m) => HinzufügenSeite -> m Objekt
seiteErgebnis = _
#endif

