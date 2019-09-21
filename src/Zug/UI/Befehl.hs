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
import Control.Monad.Trans (liftIO)
import Control.Monad.State (get, put)
import Control.Concurrent.STM (atomically, TVar, writeTVar, modifyTVar, TMVar)
import Data.Aeson (ToJSON)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (PwmMap, I2CMap, runPwmMapT, pwmMapEmpty, i2cMapEmpty)
import Zug.Klassen (Zugtyp(..))
import Zug.Menge (Menge, entfernen)
import Zug.Plan (ObjektKlasse(..), ObjektAllgemein(..), Objekt, PlanKlasse(), Plan(),
                Ausführend(..), AktionKlasse(), Aktion(), ausführenPlan, ausführenAktion)
import qualified Zug.UI.Save as Save
import Zug.UI.Base (StatusAllgemein(), Status, IOStatusAllgemein,
                    auswertenTMVarIOStatus, übergebeTVarMaps, liftIOFunction,
                    getTVarAusführend, getTVarMaps,
                    hinzufügenPlan, entfernenPlan,
                    hinzufügenWegstrecke, entfernenWegstrecke,
                    hinzufügenWeiche, entfernenWeiche,
                    hinzufügenBahngeschwindigkeit, entfernenBahngeschwindigkeit,
                    hinzufügenStreckenabschnitt, entfernenStreckenabschnitt,
                    hinzufügenKupplung, entfernenKupplung)

-- | Führe einen Plan mit einem in einer MVar gespeichertem Zustand aus
ausführenTMVarPlan :: (PlanKlasse (PL o))
            => PL o -> (Natural -> IO ()) -> IO () -> TMVar (StatusAllgemein o) -> IO ()
ausführenTMVarPlan plan showAktion endAktion tmvarStatus = do
    (tvarAusführend, tvarMaps) <- auswertenTMVarIOStatus getTVars tmvarStatus
    flip runPwmMapT tvarMaps $ ausführenPlan plan showAktion endAktion tvarAusführend
        where
            getTVars :: IOStatusAllgemein o (TVar (Menge Ausführend), (TVar PwmMap, TVar I2CMap))
            getTVars = do
                tvarAusführend <- getTVarAusführend
                tvarMaps <- getTVarMaps
                pure (tvarAusführend, tvarMaps)

-- | Führe eine Aktion mit einem in einer MVar gespeichertem Zustand aus
ausführenTMVarAktion   :: (AktionKlasse a)
                => a -> TMVar (StatusAllgemein o) -> IO ()
ausführenTMVarAktion aktion tmvarStatus = do
    tvarMaps <- auswertenTMVarIOStatus getTVarMaps tmvarStatus
    runPwmMapT (ausführenAktion aktion) tvarMaps

-- | Führe einen Befehl mit einem in einer MVar gespeichertem Zustand aus
ausführenTMVarBefehl :: (BefehlKlasse b, ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego),
                        Eq (ST o), Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
                        Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o))
                            => b o -> TMVar (StatusAllgemein o) -> IO Bool
ausführenTMVarBefehl befehl = auswertenTMVarIOStatus $ ausführenBefehl befehl

-- | Ausführen eines Befehls
class BefehlKlasse b where
    -- | Gibt True zurück, falls das UI beendet werden soll
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o), Eq (PL o),
                        Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o), Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego))
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
                        Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o), Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego))
                            => UIBefehlAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl Beenden     = pure True
    ausführenBefehl Abbrechen   = pure False

instance BefehlKlasse BefehlAllgemein where
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o), Eq (PL o),
                        Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o), Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego))
                            => BefehlAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl befehl = ausführenBefehlAux befehl >> pure (istBeenden befehl)
        where
            istBeenden :: BefehlAllgemein o -> Bool
            istBeenden  (UI Beenden)    = True
            istBeenden  _befehl         = False
            ausführenBefehlAux :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o),
                                    Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
                                    Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o))
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
                    (Just konstruktor)  -> do
                        (tvarPwmMap, tvarI2CMap) <- getTVarMaps
                        statusNeu <- liftIO $ do
                            atomically $ writeTVar tvarPwmMap pwmMapEmpty
                            atomically $ writeTVar tvarI2CMap i2cMapEmpty
                            konstruktor tvarPwmMap tvarI2CMap
                        put statusNeu
            ausführenBefehlAux  (Ausführen plan showAction endAktion)
                = do
                    tvarAusführend <- getTVarAusführend
                    übergebeTVarMaps $ ausführenPlan plan showAction endAktion tvarAusführend
            ausführenBefehlAux  (AusführenAbbrechen plan)
                = do
                    tvarAusführend <- getTVarAusführend
                    liftIO $ atomically $ modifyTVar tvarAusführend $ entfernen $ Ausführend plan
            ausführenBefehlAux  (AktionBefehl aktion)
                = übergebeTVarMaps $ ausführenAktion aktion

-- | Normale Listen von 'BefehlAllgemein' haben den falschen Kind um eine 'BefehlKlasse'-Instanz zu erhalten.
-- Der hier bereitgestellte ein newtype löst das Problem über einen Phantomtyp.
newtype BefehlListeAllgemein o = BefehlListe {getBefehlListe :: [BefehlAllgemein o]}
-- | 'BefehlListeAllgemein' spezialisiert auf 'Objekt'
type BefehlListe = BefehlListeAllgemein Objekt

instance BefehlKlasse BefehlListeAllgemein where
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o), Eq (PL o),
                        Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o), Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego))
                            => BefehlListeAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl (BefehlListe liste) = ausführenBefehlAux liste
        where
            ausführenBefehlAux  :: (ObjektKlasse o, ToJSON o, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego), Eq (ST o),
                                    Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego), Eq (KU o),
                                    Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego), Eq (PL o))
                                        =>  [BefehlAllgemein o] -> IOStatusAllgemein o Bool
            ausführenBefehlAux []      = pure False
            ausführenBefehlAux (h:t)   = do
                ende <- ausführenBefehl h
                if ende
                    then pure True
                    else ausführenBefehlAux t