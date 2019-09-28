{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

{-|
Description : Alle durch ein UI unterstützten Befehle, inklusive der Implementierung.
-}
module Zug.UI.Befehl (
    -- * Klasse
    BefehlKlasse(..),
    -- * Typen
    Befehl, BefehlAllgemein(..),
    BefehlListe, BefehlListeAllgemein(..),
    UIBefehl, UIBefehlAllgemein(..),
    -- * Funktionen
    ausführenTMVarPlan, ausführenTMVarAktion, ausführenTMVarBefehl) where

-- Bibliotheken
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans (MonadIO(..))
import Control.Concurrent.STM (atomically, writeTVar, modifyTVar, TMVar)
import Data.Aeson (ToJSON)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (pwmMapEmpty, i2cMapEmpty)
import Zug.Klassen (Zugtyp(..))
import Zug.Menge (entfernen)
import Zug.Plan (ObjektKlasse(..), ObjektAllgemein(..), Objekt, PlanKlasse(..), Plan(),
                AusführendReader(..), Ausführend(..), AktionKlasse(..), Aktion())
import qualified Zug.UI.Save as Save
import Zug.UI.Base (StatusAllgemein(), Status, IOStatusAllgemein, ObjektReader, ReaderFamilie,
                    TVarMaps(..), MitTVarMaps(), TVarMapsReader(..),
                    auswertenTMVarIOStatus, liftIOFunction,
                    hinzufügenPlan, entfernenPlan,
                    hinzufügenWegstrecke, entfernenWegstrecke,
                    hinzufügenWeiche, entfernenWeiche,
                    hinzufügenBahngeschwindigkeit, entfernenBahngeschwindigkeit,
                    hinzufügenStreckenabschnitt, entfernenStreckenabschnitt,
                    hinzufügenKupplung, entfernenKupplung)

-- | Führe einen Plan mit einem in einer 'TMVar' gespeichertem Zustand aus
ausführenTMVarPlan :: (ObjektReader o m, MonadIO m, PlanKlasse (PL o), MitTVarMaps (ReaderFamilie o))
                            => PL o -> (Natural -> IO ()) -> IO () -> TMVar (StatusAllgemein o) -> m ()
ausführenTMVarPlan plan showAktion endAktion = auswertenTMVarIOStatus $ ausführenPlan plan showAktion endAktion

-- | Führe eine Aktion mit einem in einer 'TMVar' gespeichertem Zustand aus
ausführenTMVarAktion   :: (ObjektReader o m, MonadIO m, AktionKlasse a, MitTVarMaps (ReaderFamilie o))
                                => a -> TMVar (StatusAllgemein o) -> m ()
ausführenTMVarAktion aktion = auswertenTMVarIOStatus $ ausführenAktion aktion

-- | Führe einen Befehl mit einem in einer 'TMVar' gespeichertem Zustand aus
ausführenTMVarBefehl :: (ObjektReader o m, MonadIO m, BefehlKlasse b,
                        ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego),
                        Eq (ST o), Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
                        Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o),
                        MitTVarMaps (ReaderFamilie o))
                            => b o -> TMVar (StatusAllgemein o) -> m Bool
ausführenTMVarBefehl befehl = auswertenTMVarIOStatus $ ausführenBefehl befehl

-- | Ausführen eines Befehls
class BefehlKlasse b where
    -- | Gibt True zurück, falls das UI beendet werden soll
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o), Eq (PL o),
                        Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o), Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego),
                        MitTVarMaps (ReaderFamilie o))
                            => b o -> IOStatusAllgemein o Bool

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
        (Status -> IO (StatusAllgemein o))  -- ^ Erfolgsaktion
        (IOStatusAllgemein o ())            -- ^ Fehlerbehandlung
    | Ausführen
        Plan
        (Natural -> IO ())                  -- ^ Fortschrittsanzeige
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

instance BefehlKlasse UIBefehlAllgemein where
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o), Eq (PL o),
                        Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o), Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego),
                        MitTVarMaps (ReaderFamilie o))
                            => UIBefehlAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl Beenden     = pure True
    ausführenBefehl Abbrechen   = pure False

instance BefehlKlasse BefehlAllgemein where
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o), Eq (PL o),
                        Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o), Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego),
                        MitTVarMaps (ReaderFamilie o))
                            => BefehlAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl befehl = ausführenBefehlAux befehl >> pure (istBeenden befehl)
        where
            istBeenden :: BefehlAllgemein o -> Bool
            istBeenden  (UI Beenden)    = True
            istBeenden  _befehl         = False
            ausführenBefehlAux :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o),
                                    Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
                                    Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o),
                                    MitTVarMaps (ReaderFamilie o))
                                        => BefehlAllgemein o -> IOStatusAllgemein o ()
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
                = get >>= liftIOFunction (flip Save.speichern dateipfad)
            ausführenBefehlAux  (Laden dateipfad erfolgsAktion fehlerbehandlung)
                = liftIO (Save.laden dateipfad erfolgsAktion) >>= \case
                    Nothing             -> fehlerbehandlung
                    (Just statusNeu)  -> do
                        TVarMaps {tvarPwmMap, tvarI2CMap} <- erhalteTVarMaps
                        liftIO $ do
                            atomically $ writeTVar tvarPwmMap pwmMapEmpty
                            atomically $ writeTVar tvarI2CMap i2cMapEmpty
                        put statusNeu
            ausführenBefehlAux  (Ausführen plan showAction endAktion)
                = ausführenPlan plan showAction endAktion
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

instance BefehlKlasse BefehlListeAllgemein where
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o), Eq (PL o),
                        Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o), Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego),
                        MitTVarMaps (ReaderFamilie o))
                            => BefehlListeAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl (BefehlListe liste) = ausführenBefehlAux liste
        where
            ausführenBefehlAux  :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o),
                                    Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
                                    Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o),
                                    MitTVarMaps (ReaderFamilie o))
                                        =>  [BefehlAllgemein o] -> IOStatusAllgemein o Bool
            ausführenBefehlAux []      = pure False
            ausführenBefehlAux (h:t)   = do
                ende <- ausführenBefehl h
                if ende
                    then pure True
                    else ausführenBefehlAux t