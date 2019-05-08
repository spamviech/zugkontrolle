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
                    ausführenMVarPlan, ausführenMVarAktion, ausführenMVarBefehl) where

-- Bibliotheken
import Control.Monad.Trans (liftIO)
import Control.Monad.State (get, put)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, modifyMVar_)
import Data.Aeson (ToJSON)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung
import Zug.LinkedMVar
import Zug.Menge
import Zug.Plan
import qualified Zug.UI.Save as Save
import Zug.UI.Base

-- | Führe einen Plan mit einem in einer MVar gespeichertem Zustand aus
ausführenMVarPlan :: (PlanKlasse (PL o), LikeMVar lmvar)
            => PL o -> (Natural -> IO ()) -> IO () -> lmvar (StatusAllgemein o) -> IO ()
ausführenMVarPlan plan showAktion endAktion mvarStatus = do
    (mvarAusführend, mvarPinMap) <- auswertenMVarIOStatus getMVars mvarStatus
    ausführenPlan plan showAktion endAktion mvarAusführend mvarPinMap
        where
            getMVars :: IOStatusAllgemein o (MVar (Menge Ausführend), MVar PinMap)
            getMVars = do
                mvarAusführend <- getMVarAusführend
                mvarPinMap <- getMVarPinMap
                pure (mvarAusführend, mvarPinMap)

-- | Führe eine Aktion mit einem in einer MVar gespeichertem Zustand aus
ausführenMVarAktion   :: (AktionKlasse a, LikeMVar lmvar)
                => a -> lmvar (StatusAllgemein o) -> IO ()
ausführenMVarAktion aktion mvarStatus = auswertenMVarIOStatus getMVarPinMap mvarStatus >>= ausführenAktion aktion

-- | Führe einen Befehl mit einem in einer MVar gespeichertem Zustand aus
ausführenMVarBefehl :: (BefehlKlasse b, LikeMVar lmvar, ObjektKlasse o, ToJSON o, Eq (BG o) , Eq (ST o) , Eq (WE o) , Eq (KU o) , Eq (WS o) , Eq (PL o))
                    => b o -> lmvar (StatusAllgemein o) -> IO Bool
ausführenMVarBefehl befehl = auswertenMVarIOStatus $ ausführenBefehl befehl

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
            ausführenBefehlAux  (UI uiAction)       = error $ "Vergessene UI-Aktion: " ++ show uiAction
            ausführenBefehlAux  (Hinzufügen objekt) = case erhalteObjekt objekt of
                (OPlan plan)                                -> hinzufügenPlan plan
                (OWegstrecke wegstrecke)                    -> hinzufügenWegstrecke wegstrecke
                (OWeiche weiche)                            -> hinzufügenWeiche weiche
                (OBahngeschwindigkeit bahngeschwindigkeit)  -> hinzufügenBahngeschwindigkeit bahngeschwindigkeit
                (OStreckenabschnitt streckenabschnitt)      -> hinzufügenStreckenabschnitt streckenabschnitt
                (OKupplung kupplung)                        -> hinzufügenKupplung kupplung
            ausführenBefehlAux  (Entfernen objekt)  = case erhalteObjekt objekt of
                (OPlan plan)                                -> entfernenPlan plan
                (OWegstrecke wegstrecke)                    -> entfernenWegstrecke wegstrecke
                (OWeiche weiche)                            -> entfernenWeiche weiche
                (OBahngeschwindigkeit bahngeschwindigkeit)  -> entfernenBahngeschwindigkeit bahngeschwindigkeit
                (OStreckenabschnitt streckenabschnitt)      -> entfernenStreckenabschnitt streckenabschnitt
                (OKupplung kupplung)                        -> entfernenKupplung kupplung
            ausführenBefehlAux  (Speichern dateipfad)                                   = get >>= liftIOFunction (flip Save.speichern dateipfad)
            ausführenBefehlAux  (Laden dateipfad erfolgsAktion fehlerbehandlung)        = liftIO (Save.laden dateipfad erfolgsAktion) >>= \case
                (Nothing)                   -> fehlerbehandlung
                (Just konstruktor)          -> getMVarPinMap >>= \mvarPinMap -> liftIO (takeMVar mvarPinMap >> putMVar mvarPinMap pinMapEmpty >> konstruktor mvarPinMap) >>= put
            ausführenBefehlAux  (Ausführen plan showAction endAktion)                   = do
                mvarAusführend <- getMVarAusführend
                übergebeMVarPinMap $ ausführenPlan plan showAction endAktion mvarAusführend
            ausführenBefehlAux  (AusführenAbbrechen plan)                               = getMVarAusführend >>= \mvarAusführend -> liftIO $ modifyMVar_ mvarAusführend $ pure . entfernen (Ausführend plan)
            ausführenBefehlAux  (AktionBefehl aktion)                                   = übergebeMVarPinMap $ ausführenAktion aktion

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
            ausführenBefehlAux ([])    = pure False
            ausführenBefehlAux (h:[])  = ausführenBefehl h
            ausführenBefehlAux (h:t)   = ausführenBefehl h >>= \ende -> if ende then pure True else ausführenBefehlAux t