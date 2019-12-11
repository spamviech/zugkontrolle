{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Description : Alle durch ein UI unterstützten Befehle, inklusive der Implementierung.
-}
module Zug.UI.Befehl (
    -- * Klasse
    BefehlKlasse(..),
    -- * Typen
    Befehl, BefehlAllgemein(..),
    BefehlListe, BefehlListeAllgemein(..),
    UIBefehl, UIBefehlAllgemein(..)) where

-- Bibliotheken
import qualified Control.Monad.RWS as RWS
import Control.Monad.Trans (MonadIO(..))
import Control.Concurrent.STM (atomically, writeTVar, modifyTVar)
import Data.Aeson (ToJSON)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (pwmMapEmpty, i2cMapEmpty)
import Zug.Enums (Zugtyp(..))
import Zug.Language (Sprache(), MitSprache(..))
import Zug.Menge (entfernen)
import Zug.Objekt (ObjektKlasse(..), ObjektAllgemein(..), Objekt)
import Zug.Plan (PlanKlasse(..), Plan(), AusführendReader(..), Ausführend(..), AktionKlasse(..), Aktion())
import qualified Zug.UI.Save as Save
import Zug.UI.Base (
    StatusAllgemein(), Status, IOStatusAllgemein, MStatusAllgemeinT, ReaderFamilie,
    TVarMaps(..), MitTVarMaps(), TVarMapsReader(..), liftIOStatus,
    hinzufügenPlan, entfernenPlan,
    hinzufügenWegstrecke, entfernenWegstrecke,
    hinzufügenWeiche, entfernenWeiche,
    hinzufügenBahngeschwindigkeit, entfernenBahngeschwindigkeit,
    hinzufügenStreckenabschnitt, entfernenStreckenabschnitt,
    hinzufügenKupplung, entfernenKupplung,
    getSprache)

-- | Ausführen eines Befehls
class BefehlKlasse b o where
    -- | Gibt True zurück, falls das UI beendet werden soll
    ausführenBefehl :: (MonadIO m) => b o -> MStatusAllgemeinT m o Bool

-- | Unterstütze Befehle
data BefehlAllgemein o
    = UI
        (UIBefehlAllgemein o)
    | Hinzufügen
        o
    | Entfernen
        o
    | Speichern
        FilePath
    | Laden
        FilePath
        (Status -> IOStatusAllgemein o ())  -- ^ Erfolgsaktion
        (IOStatusAllgemein o ())            -- ^ Fehlerbehandlung
    | Ausführen
        Plan
        (Natural -> Sprache -> IO ())       -- ^ Fortschrittsanzeige
        (IO ())                             -- ^ Abschlussanzeige
    | AusführenAbbrechen
        Plan
    | AktionBefehl
        Aktion
-- | 'BefehlAllgemein' spezialisiert auf 'Objekt'
type Befehl = BefehlAllgemein Objekt

-- | UI-spezifische Befehle. Phantomtyp, um eine 'BefehlKlasse'-Instanz zu erhalten.
data UIBefehlAllgemein o = Beenden | Abbrechen
    deriving (Show)
-- | 'UIBefehlAllgemein' spezialisiert auf 'Objekt'
type UIBefehl = UIBefehlAllgemein Objekt

instance BefehlKlasse UIBefehlAllgemein o where
    ausführenBefehl :: (MonadIO m) => UIBefehlAllgemein o -> MStatusAllgemeinT m o Bool
    ausführenBefehl Beenden     = pure True
    ausführenBefehl Abbrechen   = pure False

instance (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o),
    Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
    Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o),
    MitSprache (SP o), MitTVarMaps (ReaderFamilie o)) =>
        BefehlKlasse BefehlAllgemein o where
    ausführenBefehl :: (MonadIO m) => BefehlAllgemein o -> MStatusAllgemeinT m o Bool
    ausführenBefehl befehl = ausführenBefehlAux befehl >> pure (istBeenden befehl)
        where
            istBeenden :: BefehlAllgemein o -> Bool
            istBeenden  (UI Beenden)    = True
            istBeenden  _befehl         = False
            ausführenBefehlAux :: forall o m. (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego),
                                    Eq (ST o), Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
                                    Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o),
                                    MitSprache (SP o), MitTVarMaps (ReaderFamilie o), MonadIO m)
                                        => BefehlAllgemein o -> MStatusAllgemeinT m o ()
            ausführenBefehlAux  (UI _uiAction)
                = pure ()
            ausführenBefehlAux  (Hinzufügen objekt)
                = case erhalteObjekt objekt of
                    (OPlan plan)                                -> hinzufügenPlan plan
                    (OWegstrecke wegstrecke)                    -> hinzufügenWegstrecke wegstrecke
                    (OWeiche weiche)                            -> hinzufügenWeiche weiche
                    (OBahngeschwindigkeit bahngeschwindigkeit)  -> hinzufügenBahngeschwindigkeit bahngeschwindigkeit
                    (OStreckenabschnitt streckenabschnitt)      -> hinzufügenStreckenabschnitt streckenabschnitt
                    (OKupplung kupplung)                        -> hinzufügenKupplung kupplung
            ausführenBefehlAux  (Entfernen objekt)
                = case erhalteObjekt objekt of
                    (OPlan plan)                                -> entfernenPlan plan
                    (OWegstrecke wegstrecke)                    -> entfernenWegstrecke wegstrecke
                    (OWeiche weiche)                            -> entfernenWeiche weiche
                    (OBahngeschwindigkeit bahngeschwindigkeit)  -> entfernenBahngeschwindigkeit bahngeschwindigkeit
                    (OStreckenabschnitt streckenabschnitt)      -> entfernenStreckenabschnitt streckenabschnitt
                    (OKupplung kupplung)                        -> entfernenKupplung kupplung
            ausführenBefehlAux  (Speichern dateipfad)
                = RWS.get >>= liftIO . flip Save.speichern dateipfad
            ausführenBefehlAux  (Laden dateipfad erfolgsAktion fehlerbehandlung)
                = do
                    mitSprache <- getSprache
                    reader <- RWS.ask
                    state0 <- RWS.get
                    let
                        erfolgsIO :: Status -> IO (StatusAllgemein o)
                        erfolgsIO statusNeu = fst <$> RWS.execRWST (erfolgsAktion statusNeu) reader state0
                    liftIO (flip leseSprache mitSprache $ Save.laden dateipfad erfolgsIO) >>= \case
                        Nothing             -> liftIOStatus fehlerbehandlung
                        (Just statusNeu)    -> do
                            TVarMaps {tvarPwmMap, tvarI2CMap} <- erhalteTVarMaps
                            liftIO $ do
                                atomically $ writeTVar tvarPwmMap pwmMapEmpty
                                atomically $ writeTVar tvarI2CMap i2cMapEmpty
                            RWS.put statusNeu
            ausführenBefehlAux  (Ausführen plan showAction endAktion)
                = do
                    mitSprache <- getSprache
                    ausführenPlan plan (leseSprache (flip showAction) mitSprache) endAktion
            ausführenBefehlAux  (AusführenAbbrechen plan)
                = do
                    tvarAusführend <- erhalteMengeAusführend
                    liftIO $ atomically $ modifyTVar tvarAusführend $ entfernen $ Ausführend plan
            ausführenBefehlAux  (AktionBefehl aktion)
                = ausführenAktion aktion

-- | Normale Listen von 'BefehlAllgemein' haben den falschen Kind um eine 'BefehlKlasse'-Instanz zu erhalten.
-- Der hier bereitgestellte ein newtype löst das Problem über einen Phantomtyp.
newtype BefehlListeAllgemein o = BefehlListe {getBefehlListe :: [BefehlAllgemein o]}
-- | 'BefehlListeAllgemein' spezialisiert auf 'Objekt'
type BefehlListe = BefehlListeAllgemein Objekt

instance (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o),
    Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
    Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o),
    MitSprache (SP o), MitTVarMaps (ReaderFamilie o)) =>
        BefehlKlasse BefehlListeAllgemein o where
    ausführenBefehl :: (MonadIO m) => BefehlListeAllgemein o -> MStatusAllgemeinT m o Bool
    ausführenBefehl (BefehlListe liste) = ausführenBefehlAux liste
        where
            ausführenBefehlAux  :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o),
                                    Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
                                    Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o),
                                    MitSprache (SP o), MitTVarMaps (ReaderFamilie o), MonadIO m)
                                        =>  [BefehlAllgemein o] -> MStatusAllgemeinT m o Bool
            ausführenBefehlAux []      = pure False
            ausführenBefehlAux (h:t)   = do
                ende <- ausführenBefehl h
                if ende
                    then pure True
                    else ausführenBefehlAux t