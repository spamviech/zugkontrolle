{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

{-|
Description : Alle durch ein UI unterstützten Befehle, inklusive der Implementierung.
-}
module Zug.UI.Befehl (
                    -- * Klasse
                    BefehlKlasse(..),
                    -- * Typen
                    Befehl, BefehlAllgemein(..),
                    BefehlListe, BefehlListeAllgemein(..),
                    UIBefehl, UIBefehlAllgemein(..), Objekt, ObjektAllgemein(..),
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
            => p -> (Natural -> IO ()) -> lmvar (StatusAllgemein bg st we ku ws pl) -> IO ()
runMVarPlan plan showAction mvarStatus = auswertenMVarIOStatus getMVarPinMap mvarStatus >>= ausführenPlan plan showAction

-- | Führe eine Aktion mit einem in einer MVar gespeichertem Zustand aus
runMVarAktion   :: (PlanKlasse p, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, LikeMVar lmvar)
                => p -> lmvar (StatusAllgemein bg st we ku ws pl) -> IO ()
runMVarAktion aktion = runMVarPlan aktion $ const $ pure ()

-- | Führe einen Befehl mit einem in einer MVar gespeichertem Zustand aus
runMVarBefehl   :: (BefehlKlasse b, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl, LikeMVar lmvar)
                => b bg st we ku ws pl -> lmvar (StatusAllgemein bg st we ku ws pl) -> IO Bool
runMVarBefehl befehl = auswertenMVarIOStatus $ ausführenBefehl befehl

-- | Ausführen eines Befehls
class BefehlKlasse b where
    -- | Gibt True zurück, falls das UI beendet werden soll
    ausführenBefehl ::(BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                    => b bg st we ku ws pl -> IOStatusAllgemein bg st we ku ws pl Bool

-- | Unterstütze Befehle
data BefehlAllgemein bg st we ku ws pl  = UI            (UIBefehlAllgemein bg st we ku ws pl)
                                        | Hinzufügen    (ObjektAllgemein bg st we ku ws pl)
                                        | Entfernen     (ObjektAllgemein bg st we ku ws pl)
                                        | Speichern     FilePath
                                        | Laden         FilePath                            (Status -> IO (StatusAllgemein bg st we ku ws pl))  (IOStatusAllgemein bg st we ku ws pl ())
                                        | Ausführen     (PlanAllgemein bg st we ku ws)        (Natural -> IO ())
                                        | AktionBefehl  (AktionAllgemein  bg st we ku ws)
-- | 'BefehlAllgemein' spezialisiert auf minimal spezialisierte Typen
type Befehl = BefehlAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

-- | UI-spezifische Befehle. 6-facher Phantomtyp, um eine 'BefehlKlasse'-Instanz zu erhalten.
data UIBefehlAllgemein bg st we ku ws pl = Beenden | Abbrechen
                                            deriving (Show)
-- | 'UIBefehlAllgemein' spezialisiert auf minimal benötigte Typen
type UIBefehl = UIBefehlAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

-- | Sammel-Typen
data ObjektAllgemein bg st we ku ws pl  = OPlan                 pl
                                        | OWegstrecke           ws
                                        | OWeiche               we
                                        | OBahngeschwindigkeit  bg
                                        | OStreckenabschnitt    st
                                        | OKupplung             ku
                                            deriving (Show)
-- | 'ObjektAllgemein' spezialisiert auf minimal benötigte Typen
type Objekt = ObjektAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

instance (StreckenObjekt bg, StreckenObjekt st, StreckenObjekt we, StreckenObjekt ku, StreckenObjekt ws, StreckenObjekt pl) => StreckenObjekt (ObjektAllgemein bg st we ku ws pl) where
    getName :: ObjektAllgemein bg st we ku ws pl -> Text
    getName (OPlan pl)                  = getName pl
    getName (OWegstrecke ws)            = getName ws
    getName (OWeiche we)                = getName we
    getName (OBahngeschwindigkeit bg)   = getName bg
    getName (OStreckenabschnitt st)     = getName st
    getName (OKupplung ku)              = getName ku
    pins ::ObjektAllgemein bg st we ku ws pl -> [Pin]
    pins (OPlan pl)                 = pins pl
    pins (OWegstrecke ws)           = pins ws
    pins (OWeiche we)               = pins we
    pins (OBahngeschwindigkeit bg)  = pins bg
    pins (OStreckenabschnitt st)    = pins st
    pins (OKupplung ku)             = pins ku
    zugtyp :: ObjektAllgemein bg st we ku ws pl -> Zugtyp
    zugtyp (OPlan pl)                  = zugtyp pl
    zugtyp (OWegstrecke ws)            = zugtyp ws
    zugtyp (OWeiche we)                = zugtyp we
    zugtyp (OBahngeschwindigkeit bg)   = zugtyp bg
    zugtyp (OStreckenabschnitt st)     = zugtyp st
    zugtyp (OKupplung ku)              = zugtyp ku

instance BefehlKlasse UIBefehlAllgemein where
    ausführenBefehl :: (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                    => UIBefehlAllgemein bg st we ku ws pl -> IOStatusAllgemein bg st we ku ws pl Bool
    ausführenBefehl Beenden     = pure True
    ausführenBefehl Abbrechen   = pure False

instance BefehlKlasse BefehlAllgemein where
    ausführenBefehl :: (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                    => BefehlAllgemein bg st we ku ws pl -> IOStatusAllgemein bg st we ku ws pl Bool
    ausführenBefehl (UI Beenden)    = pure True
    ausführenBefehl (UI Abbrechen)  = pure False
    ausführenBefehl befehl          = ausführenBefehlAux befehl >> pure False
        where
            ausführenBefehlAux  :: (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                                => BefehlAllgemein bg st we ku ws pl -> IOStatusAllgemein bg st we ku ws pl ()
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
            ausführenBefehlAux  (Ausführen plan showAction)                             = übergebeMVarPinMap $ ausführenPlan plan showAction
            ausführenBefehlAux  (AktionBefehl aktion)                                   = übergebeMVarPinMap $ ausführenPlan aktion $ \_ -> pure ()

-- | Normale Listen von 'BefehlAllgemein' haben den falschen Kind um eine 'BefehlKlasse'-Instanz zu erhalten. Daher wird ein newtype benötigt.
newtype BefehlListeAllgemein bg st we ku ws pl = BefehlListe {getBefehlListe :: [BefehlAllgemein bg st we ku ws pl]}
-- | 'BefehlListeAllgemein' spezialisiert auf minimal benötigte Typen
type BefehlListe = BefehlListeAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

instance BefehlKlasse BefehlListeAllgemein where
    ausführenBefehl ::(BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                    => BefehlListeAllgemein bg st we ku ws pl -> IOStatusAllgemein bg st we ku ws pl Bool
    ausführenBefehl (BefehlListe liste) = ausführenBefehlAux liste
        where
            ausführenBefehlAux  ::(BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws, Eq bg, Eq st, Eq we, Eq ku, Eq ws, Eq pl, ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl)
                                => [BefehlAllgemein bg st we ku ws pl] -> IOStatusAllgemein bg st we ku ws pl Bool
            ausführenBefehlAux ([])    = pure False
            ausführenBefehlAux (h:[])  = ausführenBefehl h
            ausführenBefehlAux (h:t)   = ausführenBefehl h >>= \ende -> if ende then pure True else ausführenBefehlAux t