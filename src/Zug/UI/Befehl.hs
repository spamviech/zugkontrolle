{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Zug.Anbindung (PwmMap, pwmMapEmpty, i2cMapEmpty)
import Zug.Menge (Menge, entfernen)
import Zug.Plan (ObjektKlasse(..), ObjektAllgemein(..), Objekt, PlanKlasse(), Plan(),
                Ausführend(..), AktionKlasse(), Aktion(), ausführenPlan, ausführenAktion)
import qualified Zug.UI.Save as Save
import Zug.UI.Base (StatusAllgemein(), Status, IOStatusAllgemein,
                    auswertenTMVarIOStatus, übergebeTVarPwmMap, liftIOFunction,
                    getTVarAusführend, getTVarPwmMap, getTVarI2CMap,
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
    (mvarAusführend, mvarPinMap) <- auswertenTMVarIOStatus getTVars tmvarStatus
    ausführenPlan plan showAktion endAktion mvarAusführend mvarPinMap
        where
            getTVars :: IOStatusAllgemein o (TVar (Menge Ausführend), TVar PwmMap)
            getTVars = do
                tvarAusführend <- getTVarAusführend
                tvarPwmMap <- getTVarPwmMap
                pure (tvarAusführend, tvarPwmMap)

-- | Führe eine Aktion mit einem in einer MVar gespeichertem Zustand aus
ausführenTMVarAktion   :: (AktionKlasse a)
                => a -> TMVar (StatusAllgemein o) -> IO ()
ausführenTMVarAktion aktion tmvarStatus = auswertenTMVarIOStatus getTVarPwmMap tmvarStatus >>= ausführenAktion aktion

-- | Führe einen Befehl mit einem in einer MVar gespeichertem Zustand aus
ausführenTMVarBefehl :: (BefehlKlasse b, ObjektKlasse o, ToJSON o, Eq (BG o) , Eq (ST o) , Eq (WE o) , Eq (KU o) , Eq (WS o) , Eq (PL o))
                    => b o -> TMVar (StatusAllgemein o) -> IO Bool
ausführenTMVarBefehl befehl = auswertenTMVarIOStatus $ ausführenBefehl befehl

-- | Ausführen eines Befehls
class BefehlKlasse b where
    -- | Gibt True zurück, falls das UI beendet werden soll
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq (BG o) , Eq (ST o) , Eq (WE o) , Eq (KU o) , Eq (WS o) , Eq (PL o))
                    => b o -> IOStatusAllgemein o Bool

-- | Unterstütze Befehle
data BefehlAllgemein o  = UI                    (UIBefehlAllgemein o)
                        | Hinzufügen            o
                        | Entfernen             o
                        | Speichern             FilePath
                        | Laden                 FilePath                (Status -> IO (StatusAllgemein o))  (IOStatusAllgemein o ())
                        | Ausführen             Plan                    (Natural -> IO ())                  (IO ())
                        | AusführenAbbrechen    Plan
                        | AktionBefehl          Aktion
-- | 'BefehlAllgemein' spezialisiert auf minimal spezialisierte Typen
type Befehl = BefehlAllgemein Objekt

-- | UI-spezifische Befehle. Phantomtyp, um eine 'BefehlKlasse'-Instanz zu erhalten.
data UIBefehlAllgemein o = Beenden | Abbrechen
                            deriving (Show)
-- | 'UIBefehlAllgemein' spezialisiert auf minimal benötigte Typen
type UIBefehl = UIBefehlAllgemein Objekt

instance BefehlKlasse UIBefehlAllgemein where
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq (BG o) , Eq (ST o) , Eq (WE o) , Eq (KU o) , Eq (WS o) , Eq (PL o))
                    => UIBefehlAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl Beenden     = pure True
    ausführenBefehl Abbrechen   = pure False

instance BefehlKlasse BefehlAllgemein where
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq (BG o) , Eq (ST o) , Eq (WE o) , Eq (KU o) , Eq (WS o) , Eq (PL o))
                    => BefehlAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl (UI Beenden)    = pure True
    ausführenBefehl (UI Abbrechen)  = pure False
    ausführenBefehl befehl          = ausführenBefehlAux befehl >> pure False
        where
            ausführenBefehlAux :: (ObjektKlasse o, ToJSON o, Eq (BG o) , Eq (ST o) , Eq (WE o) , Eq (KU o) , Eq (WS o) , Eq (PL o))
                                => BefehlAllgemein o -> IOStatusAllgemein o ()
            ausführenBefehlAux  (UI uiAction)
                = error $ "Vergessene UI-Aktion: " ++ show uiAction
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
                        tvarPwmMap <- getTVarPwmMap
                        tvarI2CMap <- getTVarI2CMap
                        statusNeu <- liftIO $ do
                            atomically $ writeTVar tvarPwmMap pwmMapEmpty
                            atomically $ writeTVar tvarI2CMap i2cMapEmpty
                            konstruktor tvarPwmMap tvarI2CMap
                        put statusNeu
            ausführenBefehlAux  (Ausführen plan showAction endAktion)
                = do
                    tvarAusführend <- getTVarAusführend
                    übergebeTVarPwmMap $ ausführenPlan plan showAction endAktion tvarAusführend
            ausführenBefehlAux  (AusführenAbbrechen plan)
                = do
                    tvarAusführend <- getTVarAusführend
                    liftIO $ atomically $ modifyTVar tvarAusführend $ entfernen $ Ausführend plan
            ausführenBefehlAux  (AktionBefehl aktion)
                = übergebeTVarPwmMap $ ausführenAktion aktion

-- | Normale Listen von 'BefehlAllgemein' haben den falschen Kind um eine 'BefehlKlasse'-Instanz zu erhalten. Daher wird ein newtype benötigt.
newtype BefehlListeAllgemein o = BefehlListe {getBefehlListe :: [BefehlAllgemein o]}
-- | 'BefehlListeAllgemein' spezialisiert auf minimal benötigte Typen
type BefehlListe = BefehlListeAllgemein Objekt

instance BefehlKlasse BefehlListeAllgemein where
    ausführenBefehl :: (ObjektKlasse o, ToJSON o, Eq (BG o) , Eq (ST o) , Eq (WE o) , Eq (KU o) , Eq (WS o) , Eq (PL o))
                    => BefehlListeAllgemein o -> IOStatusAllgemein o Bool
    ausführenBefehl (BefehlListe liste) = ausführenBefehlAux liste
        where
            ausführenBefehlAux  :: (ObjektKlasse o, ToJSON o , Eq (BG o) , Eq (ST o) , Eq (WE o) , Eq (KU o) , Eq (WS o) , Eq (PL o))
                                =>  [BefehlAllgemein o] -> IOStatusAllgemein o Bool
            ausführenBefehlAux []      = pure False
            ausführenBefehlAux (h:[])  = ausführenBefehl h
            ausführenBefehlAux (h:t)   = ausführenBefehl h >>= \ende -> if ende then pure True else ausführenBefehlAux t