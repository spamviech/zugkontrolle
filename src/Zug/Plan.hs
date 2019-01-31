{-# LANGUAGE NamedFieldPuns, InstanceSigs #-}

{-|
Description : Pläne sind nacheinander auszuführende Aktionen, welche mit StreckenObjekten möglich sind.

Jede Art von 'StreckenObjekt' ('Bahngeschwindigkeit', 'Streckenabschnitt', 'Weiche', 'Wegstrecke') unterstützt unterschiedliche Aktionen.
Ein 'Plan' ist eine Zusammenfassung mehrerer dieser Aktionen und Wartezeiten, welche nacheinander ausgeführt werden können.
-}
module Zug.Plan (
    -- * Allgemeine Datentypen
    PlanKlasse(..), Plan, PlanGeneral(..), Aktion, AktionGeneral(..),
    -- * Spezialisierte Aktionen
    AktionWeiche(..), AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionKupplung(..), AktionWegstrecke(..)) where

-- Bibliotheken
import Control.Concurrent
import Control.Monad (void, foldM)
import Data.Semigroup (Semigroup(..))
import Numeric.Natural
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen
import Zug.Anbindung
import qualified Zug.Language as Language
import Zug.Language ((<~>), (<^>), (<=>), (<:>), (<°>))

-- | Mitglieder dieser Klasse sind ausführbar (können in IO-Aktionen übersetzt werden).
-- Sie können selbst entscheiden, ob sie die mitgegebene Update-Funktion über den Fortschritt informieren, oder nicht.
class PlanKlasse a where
    ausführenPlan :: a -> (Natural -> IO ()) -> PinMapIO ()
    {-# MINIMAL ausführenPlan #-}

-- | Pläne: Benannte IO-Aktionen mit StreckenObjekten, bzw. Wartezeiten.
-- Die Update-Funktion wird mit Index der aktuellen Aktion vor dessen Ausführung aufgerufen.
data PlanGeneral bg st we ku ws = Plan {
    plName :: String,
    plAktionen :: [AktionGeneral bg st we ku ws]}
        deriving (Eq)

type Plan = PlanGeneral Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke

instance (Show bg, Show st, Show we, Show ku, Show ws, StreckenObjekt bg, StreckenObjekt st, StreckenObjekt we, StreckenObjekt ku, StreckenObjekt ws) => Show (PlanGeneral bg st we ku ws) where
    show :: PlanGeneral bg st we ku ws -> String
    show    (Plan {plName, plAktionen}) = Language.plan
                                        <:> Language.name <=> plName
                                        <^> Language.aktionen <=> show plAktionen

instance (Show bg, Show st, Show we, Show ku, Show ws, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws) => StreckenObjekt (PlanGeneral bg st we ku ws) where
    pins :: PlanGeneral bg st we ku ws -> [Pin]
    pins    (Plan {plAktionen}) = foldMap pins plAktionen
    zugtyp :: PlanGeneral bg st we ku ws -> Zugtyp
    zugtyp  (Plan {plAktionen}) = foldZugtyp plAktionen
    getName :: PlanGeneral bg st we ku ws -> String
    getName (Plan {plName})     = plName

instance (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws) => PlanKlasse (PlanGeneral bg st we ku ws) where
    ausführenPlan :: PlanGeneral bg st we ku ws -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Plan {plAktionen})   showAction    mvarPinMap  = void $ forkIO $ void $ foldM (\i aktion -> showAction i >> ausführenPlan aktion showAction mvarPinMap >> pure (succ i)) 0 plAktionen >> showAction (fromIntegral $ length plAktionen)

-- | Eine Aktion eines 'StreckenObjekt's oder eine Wartezeit.
-- Die Update-Funktion wird nicht aufgerufen.
data AktionGeneral bg st we ku ws   = Warten                Natural
                                    | AWegstrecke           (AktionWegstrecke ws)
                                    | AWeiche               (AktionWeiche we)
                                    | ABahngeschwindigkeit  (AktionBahngeschwindigkeit bg)
                                    | AStreckenabschnitt    (AktionStreckenabschnitt st)
                                    | AKupplung             (AktionKupplung ku)
                                        deriving (Eq)
type Aktion = AktionGeneral Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke

instance (Show bg, Show st, Show we, Show ku, Show ws, StreckenObjekt bg, StreckenObjekt st, StreckenObjekt we, StreckenObjekt ku, StreckenObjekt ws) => Show (AktionGeneral bg st we ku ws) where
    show :: AktionGeneral bg st we ku ws -> String
    show    (Warten time)               = Language.warten <:> show time <> Language.wartenEinheit
    show    (AWegstrecke a)             = Language.wegstrecke <~> show a
    show    (AWeiche a)                 = Language.weiche <~> show a
    show    (ABahngeschwindigkeit a)    = Language.bahngeschwindigkeit <~> show a
    show    (AStreckenabschnitt a)      = Language.streckenabschnitt <~> show a
    show    (AKupplung a)               = Language.kupplung <~> show a

instance (Show bg, Show st, Show we, Show ku, Show ws, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws) => StreckenObjekt (AktionGeneral bg st we ku ws) where
    pins :: AktionGeneral bg st we ku ws -> [Pin]
    pins    (Warten _time)              = []
    pins    (AWegstrecke w)             = pins w
    pins    (AWeiche w)                 = pins w
    pins    (ABahngeschwindigkeit b)    = pins b
    pins    (AStreckenabschnitt s)      = pins s
    pins    (AKupplung k)               = pins k
    zugtyp :: AktionGeneral bg st we ku ws -> Zugtyp
    zugtyp  (Warten _time)              = Undefiniert
    zugtyp  (AWegstrecke w)             = zugtyp w
    zugtyp  (AWeiche w)                 = zugtyp w
    zugtyp  (ABahngeschwindigkeit b)    = zugtyp b
    zugtyp  (AStreckenabschnitt s)      = zugtyp s
    zugtyp  (AKupplung k)               = zugtyp k
    getName :: AktionGeneral bg st we ku ws -> String
    getName = show

instance (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws) => PlanKlasse (AktionGeneral bg st we ku ws) where
    ausführenPlan :: AktionGeneral bg st we ku ws -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Warten time)                   _showAction   = \_ -> delayµs time
    ausführenPlan (AWegstrecke aktion)            showAction    = ausführenPlan aktion showAction
    ausführenPlan (AWeiche aktion)                showAction    = ausführenPlan aktion showAction
    ausführenPlan (ABahngeschwindigkeit aktion)   showAction    = ausführenPlan aktion showAction
    ausführenPlan (AStreckenabschnitt aktion)     showAction    = ausführenPlan aktion showAction
    ausführenPlan (AKupplung aktion)              showAction    = ausführenPlan aktion showAction

data AktionWegstrecke w = Einstellen w
                        | AWSBahngeschwindigkeit (AktionBahngeschwindigkeit w)
                        | AWSStreckenabschnitt (AktionStreckenabschnitt w)
                        | AWSKupplung (AktionKupplung w)
                            deriving (Eq)

instance (StreckenObjekt w)  => Show (AktionWegstrecke w) where
    show :: AktionWegstrecke w -> String
    show    (Einstellen wegstrecke)     = getName wegstrecke <°> Language.einstellen
    show    (AWSBahngeschwindigkeit a)  = show a
    show    (AWSStreckenabschnitt a)    = show a
    show    (AWSKupplung a)             = show a

instance (WegstreckeKlasse w, Show w) => StreckenObjekt (AktionWegstrecke w) where
    pins :: AktionWegstrecke w -> [Pin]
    pins    (Einstellen w)              = pins w
    pins    (AWSBahngeschwindigkeit w)  = pins w
    pins    (AWSStreckenabschnitt w)    = pins w
    pins    (AWSKupplung w)             = pins w
    zugtyp :: AktionWegstrecke w -> Zugtyp
    zugtyp  (Einstellen w)              = zugtyp w
    zugtyp  (AWSBahngeschwindigkeit w)  = zugtyp w
    zugtyp  (AWSStreckenabschnitt w)    = zugtyp w
    zugtyp  (AWSKupplung w)             = zugtyp w
    getName :: AktionWegstrecke w -> String
    getName = show

instance (WegstreckeKlasse w) => PlanKlasse (AktionWegstrecke w) where
    ausführenPlan :: AktionWegstrecke w -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Einstellen w)                  _showAction   = einstellen w
    ausführenPlan (AWSBahngeschwindigkeit aktion) showAction    = ausführenPlan aktion showAction
    ausführenPlan (AWSStreckenabschnitt aktion)   showAction    = ausführenPlan aktion showAction
    ausführenPlan (AWSKupplung aktion)            showAction    = ausführenPlan aktion showAction

data AktionWeiche w = Stellen w Richtung
                        deriving (Eq)

instance (StreckenObjekt w)  => Show (AktionWeiche w) where
    show :: AktionWeiche w -> String
    show    (Stellen w richtung)    = getName w <°> Language.stellen <=> show richtung

instance (WeicheKlasse w, Show w) => StreckenObjekt (AktionWeiche w) where
    pins :: AktionWeiche w -> [Pin]
    pins    (Stellen w _richtung)   = pins w
    zugtyp :: AktionWeiche w -> Zugtyp
    zugtyp  (Stellen w _richtung)   = zugtyp w
    getName :: AktionWeiche w -> String
    getName = show

instance (WeicheKlasse w) => PlanKlasse (AktionWeiche w) where
    ausführenPlan :: AktionWeiche w -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Stellen w richtung)    _showAction   = stellen w richtung

data AktionBahngeschwindigkeit b    = Geschwindigkeit b Natural
                                    | Umdrehen b (Maybe Fahrtrichtung)
                                        deriving (Eq)

instance (StreckenObjekt b) => Show (AktionBahngeschwindigkeit b) where
    show :: AktionBahngeschwindigkeit b -> String
    show    (Geschwindigkeit b wert)            = getName b <°> Language.geschwindigkeit <=> show wert
    show    (Umdrehen b (Just fahrtrichtung))   = getName b <°> Language.umdrehen <=> show fahrtrichtung
    show    (Umdrehen b Nothing)                = getName b <°> Language.umdrehen

instance (BahngeschwindigkeitKlasse b, Show b) => StreckenObjekt (AktionBahngeschwindigkeit b) where
    pins :: AktionBahngeschwindigkeit b -> [Pin]
    pins    (Geschwindigkeit b _wert)   = pins b
    pins    (Umdrehen b _maybe)         = pins b
    zugtyp :: AktionBahngeschwindigkeit b -> Zugtyp
    zugtyp  (Geschwindigkeit b _wert)   = zugtyp b
    zugtyp  (Umdrehen b _maybe)         = zugtyp b
    getName :: AktionBahngeschwindigkeit b -> String
    getName = show

instance (BahngeschwindigkeitKlasse b) => PlanKlasse (AktionBahngeschwindigkeit b) where
    ausführenPlan :: AktionBahngeschwindigkeit b -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Geschwindigkeit b wert)        _showAction   = geschwindigkeit b wert
    ausführenPlan (Umdrehen b maybeFahrtrichtung) _showAction   = umdrehen b maybeFahrtrichtung

data AktionStreckenabschnitt s   = Strom s Bool
                                    deriving (Eq)

instance (StreckenObjekt s) => Show (AktionStreckenabschnitt s) where
    show :: AktionStreckenabschnitt s -> String
    show    (Strom s True)  = getName s <°> Language.strom <=> Language.an
    show    (Strom s False) = getName s <°> Language.strom <=> Language.aus

instance (StreckenabschnittKlasse s, Show s) => StreckenObjekt (AktionStreckenabschnitt s) where
    pins :: AktionStreckenabschnitt s -> [Pin]
    pins    (Strom s _an)   = pins s
    zugtyp :: AktionStreckenabschnitt s -> Zugtyp
    zugtyp  (Strom s _an)   = zugtyp s
    getName :: AktionStreckenabschnitt s -> String
    getName = show

instance (StreckenabschnittKlasse s) => PlanKlasse (AktionStreckenabschnitt s) where
    ausführenPlan :: AktionStreckenabschnitt s -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Strom s an)    _showAction   = strom s an

data AktionKupplung k   = Kuppeln k
                            deriving (Eq)

instance (StreckenObjekt k) => Show (AktionKupplung k) where
    show :: AktionKupplung k -> String
    show    (Kuppeln k) = getName k <°> Language.kuppeln

instance (KupplungKlasse k, Show k) => StreckenObjekt (AktionKupplung k) where
    pins :: AktionKupplung k -> [Pin]
    pins    (Kuppeln k) = pins k
    zugtyp :: AktionKupplung k -> Zugtyp
    zugtyp  (Kuppeln k) = zugtyp k
    getName :: AktionKupplung k -> String
    getName = show

instance (KupplungKlasse k) => PlanKlasse (AktionKupplung k) where
    ausführenPlan :: AktionKupplung k -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Kuppeln k) _showAction   = kuppeln k

-- | Hilfsfunktion umd den Gesamt-Zugtyp zu erhalten
foldZugtyp :: (StreckenObjekt s) => [s] -> Zugtyp
foldZugtyp liste = foldr foldZugtypAux Undefiniert liste
    where
        foldZugtypAux :: (StreckenObjekt s) => s -> Zugtyp -> Zugtyp
        foldZugtypAux   s   (Undefiniert)   = zugtyp s
        foldZugtypAux   _s  zugtypWert      = zugtypWert