{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

{-|
Description : Alle durch ein UI unterstützten Befehle, inklusive der Implementierung.
-}
module Zug.UI.Befehl (
                    -- * Klasse
                    BefehlKlasse(..),
                    -- * Typen
                    Befehl, BefehlGeneral(..),
                    BefehlListe, BefehlListeGeneral(..),
                    UIBefehl, UIBefehlGeneral(..), Objekt, ObjektGeneral(..),
                    -- * Funktionen
                    runMVarPlan, runMVarAktion, runMVarBefehl) where

-- Bibliotheken
import Control.Monad.Trans
import Control.Monad.State
import Control.Concurrent
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Numeric.Natural
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar
import Zug.Klassen
import Zug.Anbindung
import Zug.Plan
import qualified Zug.UI.Save as Save
import Zug.UI.Base

-- | Führe einen Plan mit einem in einer MVar gespeichertem Zustand aus
runMVarPlan :: (PlanKlasse p, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, LikeMVar lmvar)
            => p -> (Natural -> IO ()) -> lmvar (StatusGeneral bg st we ku ws pl) -> IO ()
runMVarPlan plan showAction mvarStatus = evalMVarIOStatus getMVarPinMap mvarStatus >>= ausführenPlan plan showAction

-- | Führe eine Aktion mit einem in einer MVar gespeichertem Zustand aus
runMVarAktion   :: (PlanKlasse p, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, LikeMVar lmvar)
                => p -> lmvar (StatusGeneral bg st we ku ws pl) -> IO ()
runMVarAktion aktion mvarStatus = runMVarPlan aktion (\_ -> pure ()) mvarStatus

-- | Führe einen Befehl mit einem in einer MVar gespeichertem Zustand aus
runMVarBefehl   :: (BefehlKlasse b, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl, LikeMVar lmvar)
                => b bg st we ku ws pl -> lmvar (StatusGeneral bg st we ku ws pl) -> IO Bool
runMVarBefehl befehl mvarStatus = evalMVarIOStatus (ausführenBefehl befehl) mvarStatus

-- | Ausführen eines Befehls
class BefehlKlasse b where
    -- | Gibt True zurück, falls das UI beendet werden soll
    ausführenBefehl ::(BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                    => b bg st we ku ws pl -> IOStatusGeneral bg st we ku ws pl Bool

-- | Unterstütze Befehle
data BefehlGeneral bg st we ku ws pl    = UI            (UIBefehlGeneral bg st we ku ws pl)
                                        | Hinzufügen    (ObjektGeneral bg st we ku ws pl)
                                        | Entfernen     (ObjektGeneral bg st we ku ws pl)
                                        | Speichern     FilePath
                                        | Laden         FilePath                            (Status -> IO (StatusGeneral bg st we ku ws pl))  (IOStatusGeneral bg st we ku ws pl ())
                                        | Ausführen     (PlanGeneral bg st we ku ws)        (Natural -> IO ())
                                        | AktionBefehl  (AktionGeneral  bg st we ku ws)
-- | 'BefehlGeneral' spezialisiert auf minimal spezialisierte Typen
type Befehl = BefehlGeneral Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

-- | UI-spezifische Befehle. 6-facher Phantomtyp, um eine 'BefehlKlasse'-Instanz zu erhalten.
data UIBefehlGeneral bg st we ku ws pl = Beenden | Abbrechen
                                            deriving (Show)
-- | 'UIBefehlGeneral' spezialisiert auf minimal benötigte Typen
type UIBefehl = UIBefehlGeneral Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

-- | Sammel-Typen
data ObjektGeneral bg st we ku ws pl    = OPlan                 pl
                                        | OWegstrecke           ws
                                        | OWeiche               we
                                        | OBahngeschwindigkeit  bg
                                        | OStreckenabschnitt    st
                                        | OKupplung             ku
                                            deriving (Show)
-- | 'ObjektGeneral' spezialisiert auf minimal benötigte Typen
type Objekt = ObjektGeneral Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

instance (StreckenObjekt bg, StreckenObjekt st, StreckenObjekt we, StreckenObjekt ku, StreckenObjekt ws, StreckenObjekt pl) => StreckenObjekt (ObjektGeneral bg st we ku ws pl) where
    getName :: ObjektGeneral bg st we ku ws pl -> Text
    getName (OPlan pl)                  = getName pl
    getName (OWegstrecke ws)            = getName ws
    getName (OWeiche we)                = getName we
    getName (OBahngeschwindigkeit bg)   = getName bg
    getName (OStreckenabschnitt st)     = getName st
    getName (OKupplung ku)              = getName ku
    pins ::ObjektGeneral bg st we ku ws pl -> [Pin]
    pins (OPlan pl)                 = pins pl
    pins (OWegstrecke ws)           = pins ws
    pins (OWeiche we)               = pins we
    pins (OBahngeschwindigkeit bg)  = pins bg
    pins (OStreckenabschnitt st)    = pins st
    pins (OKupplung ku)             = pins ku
    zugtyp :: ObjektGeneral bg st we ku ws pl -> Zugtyp
    zugtyp (OPlan pl)                  = zugtyp pl
    zugtyp (OWegstrecke ws)            = zugtyp ws
    zugtyp (OWeiche we)                = zugtyp we
    zugtyp (OBahngeschwindigkeit bg)   = zugtyp bg
    zugtyp (OStreckenabschnitt st)     = zugtyp st
    zugtyp (OKupplung ku)              = zugtyp ku

instance BefehlKlasse UIBefehlGeneral where
    ausführenBefehl :: (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                    => UIBefehlGeneral bg st we ku ws pl -> IOStatusGeneral bg st we ku ws pl Bool
    ausführenBefehl Beenden     = pure True
    ausführenBefehl Abbrechen   = pure False

instance BefehlKlasse BefehlGeneral where
    ausführenBefehl :: (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                    => BefehlGeneral bg st we ku ws pl -> IOStatusGeneral bg st we ku ws pl Bool
    ausführenBefehl (UI Beenden)    = pure True
    ausführenBefehl (UI Abbrechen)  = pure False
    ausführenBefehl befehl          = ausführenBefehlAux befehl >> pure False
        where
            ausführenBefehlAux  :: (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                                => BefehlGeneral bg st we ku ws pl -> IOStatusGeneral bg st we ku ws pl ()
            ausführenBefehlAux  (UI uiAction)                                           = error $ "Vergessene UI-Aktion: " ++ show uiAction
            ausführenBefehlAux  (Hinzufügen (OPlan plan))                               = hinzufügenPlan plan
            ausführenBefehlAux  (Hinzufügen (OWegstrecke wegstrecke))                   = hinzufügenWegstrecke wegstrecke
            ausführenBefehlAux  (Hinzufügen (OWeiche weiche))                           = hinzufügenWeiche weiche
            ausführenBefehlAux  (Hinzufügen (OBahngeschwindigkeit bahngeschwindigkeit)) = hinzufügenBahngeschwindigkeit bahngeschwindigkeit
            ausführenBefehlAux  (Hinzufügen (OStreckenabschnitt streckenabschnitt))     = hinzufügenStreckenabschnitt streckenabschnitt
            ausführenBefehlAux  (Hinzufügen (OKupplung kupplung))                       = hinzufügenKupplung kupplung
            ausführenBefehlAux  (Entfernen (OPlan plan))                                = entfernenPlan plan
            ausführenBefehlAux  (Entfernen (OWegstrecke wegstrecke))                    = entfernenWegstrecke wegstrecke
            ausführenBefehlAux  (Entfernen (OWeiche weiche))                            = entfernenWeiche weiche
            ausführenBefehlAux  (Entfernen (OBahngeschwindigkeit bahngeschwindigkeit))  = entfernenBahngeschwindigkeit bahngeschwindigkeit
            ausführenBefehlAux  (Entfernen (OStreckenabschnitt streckenabschnitt))      = entfernenStreckenabschnitt streckenabschnitt
            ausführenBefehlAux  (Entfernen (OKupplung kupplung))                        = entfernenKupplung kupplung
            ausführenBefehlAux  (Speichern dateipfad)                                   = get >>= liftIOFunction (flip Save.save dateipfad)
            ausführenBefehlAux  (Laden dateipfad erfolgsAktion fehlerbehandlung)        = liftIO (Save.load dateipfad erfolgsAktion) >>= \case
                (Nothing)                   -> fehlerbehandlung
                (Just konstruktor)          -> getMVarPinMap >>= \mvarPinMap -> liftIO (takeMVar mvarPinMap >> putMVar mvarPinMap pinMapEmpty >> konstruktor mvarPinMap) >>= put
            ausführenBefehlAux  (Ausführen plan showAction)                             = passMVarPinMap $ ausführenPlan plan showAction
            ausführenBefehlAux  (AktionBefehl aktion)                                   = passMVarPinMap $ ausführenPlan aktion $ \_ -> pure ()

-- | Normale Listen von 'BefehlGeneral' haben den falschen Kind um eine 'BefehlKlasse'-Instanz zu erhalten. Daher wird ein newtype benötigt.
newtype BefehlListeGeneral bg st we ku ws pl = BefehlListe {getBefehlListe :: [BefehlGeneral bg st we ku ws pl]}
-- | 'BefehlListeGeneral' spezialisiert auf minimal benötigte Typen
type BefehlListe = BefehlListeGeneral Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

instance BefehlKlasse BefehlListeGeneral where
    ausführenBefehl ::(BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                    => BefehlListeGeneral bg st we ku ws pl -> IOStatusGeneral bg st we ku ws pl Bool
    ausführenBefehl (BefehlListe liste) = ausführenBefehlAux liste
        where
            ausführenBefehlAux  ::(BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                                => [BefehlGeneral bg st we ku ws pl] -> IOStatusGeneral bg st we ku ws pl Bool
            ausführenBefehlAux ([])    = pure False
            ausführenBefehlAux (h:[])  = ausführenBefehl h
            ausführenBefehlAux (h:t)   = ausführenBefehl h >>= \ende -> if ende then pure True else ausführenBefehlAux t