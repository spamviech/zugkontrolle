{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

{-|
Description : Pläne sind nacheinander auszuführende Aktionen, welche mit StreckenObjekten möglich sind.

Jede Art von 'StreckenObjekt' ('Bahngeschwindigkeit', 'Streckenabschnitt', 'Weiche', 'Wegstrecke') unterstützt unterschiedliche Aktionen.
Ein 'Plan' ist eine Zusammenfassung mehrerer dieser Aktionen und Wartezeiten, welche nacheinander ausgeführt werden können.
-}
module Zug.Plan (
    -- * Allgemeine Datentypen
    PlanKlasse(..), Plan, PlanAllgemein(..), Aktion, AktionAllgemein(..),
    -- * Spezialisierte Aktionen
    AktionWeiche(..), AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionKupplung(..), AktionWegstrecke(..)) where

-- Bibliotheken
import Control.Concurrent (forkIO)
import Control.Monad (void, foldM)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen
import Zug.Anbindung
import qualified Zug.Language as Language
import Zug.Language (showText, (<~>), (<^>), (<=>), (<:>), (<°>))

-- | Mitglieder dieser Klasse sind ausführbar (können in IO-Aktionen übersetzt werden).
-- Sie können selbst entscheiden, ob sie die mitgegebene Update-Funktion über den Fortschritt informieren, oder nicht.
class PlanKlasse a where
    ausführenPlan :: a -> (Natural -> IO ()) -> PinMapIO ()
    {-# MINIMAL ausführenPlan #-}

-- | Pläne: Benannte IO-Aktionen mit StreckenObjekten, bzw. Wartezeiten.
-- Die Update-Funktion wird mit Index der aktuellen Aktion vor dessen Ausführung aufgerufen.
data PlanAllgemein bg st we ku ws = Plan {
    plName :: Text,
    plAktionen :: [AktionAllgemein bg st we ku ws]}
        deriving (Eq)

-- | Spezialisierung eines 'PlanAllgemein' auf minimal benötigte Typen
type Plan = PlanAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke

instance (Show bg, Show st, Show we, Show ku, Show ws, StreckenObjekt bg, StreckenObjekt st, StreckenObjekt we, StreckenObjekt ku, StreckenObjekt ws) => Show (PlanAllgemein bg st we ku ws) where
    show :: PlanAllgemein bg st we ku ws -> String
    show    (Plan {plName, plAktionen}) = Language.plan
                                        <:> Language.name <=> unpack plName
                                        <^> Language.aktionen <=> show plAktionen

instance (Show bg, Show st, Show we, Show ku, Show ws, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws) => StreckenObjekt (PlanAllgemein bg st we ku ws) where
    pins :: PlanAllgemein bg st we ku ws -> [Pin]
    pins    (Plan {plAktionen}) = foldMap pins plAktionen
    zugtyp :: PlanAllgemein bg st we ku ws -> Zugtyp
    zugtyp  (Plan {plAktionen}) = findeZugtyp plAktionen
    erhalteName :: PlanAllgemein bg st we ku ws -> Text
    erhalteName (Plan {plName})     = plName

instance (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws) => PlanKlasse (PlanAllgemein bg st we ku ws) where
    ausführenPlan :: PlanAllgemein bg st we ku ws -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Plan {plAktionen})   showAktion    mvarPinMap  = void $ forkIO $ void $ foldM (\i aktion -> showAktion i >> ausführenPlan aktion showAktion mvarPinMap >> pure (succ i)) 0 plAktionen >> showAktion (fromIntegral $ length plAktionen)

-- | Eine Aktion eines 'StreckenObjekt's oder eine Wartezeit.
-- Die Update-Funktion wird nicht aufgerufen.
data AktionAllgemein bg st we ku ws   = Warten                Natural
                                    | AWegstrecke           (AktionWegstrecke ws)
                                    | AWeiche               (AktionWeiche we)
                                    | ABahngeschwindigkeit  (AktionBahngeschwindigkeit bg)
                                    | AStreckenabschnitt    (AktionStreckenabschnitt st)
                                    | AKupplung             (AktionKupplung ku)
                                        deriving (Eq)
-- | Spezialisierung einer 'AktionAllgemein' auf minimal benötigte Typen
type Aktion = AktionAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke

instance (Show bg, Show st, Show we, Show ku, Show ws, StreckenObjekt bg, StreckenObjekt st, StreckenObjekt we, StreckenObjekt ku, StreckenObjekt ws) => Show (AktionAllgemein bg st we ku ws) where
    show :: AktionAllgemein bg st we ku ws -> String
    show    (Warten time)               = Language.warten <:> show time <> Language.wartenEinheit
    show    (AWegstrecke a)             = Language.wegstrecke <~> show a
    show    (AWeiche a)                 = Language.weiche <~> show a
    show    (ABahngeschwindigkeit a)    = Language.bahngeschwindigkeit <~> show a
    show    (AStreckenabschnitt a)      = Language.streckenabschnitt <~> show a
    show    (AKupplung a)               = Language.kupplung <~> show a

instance (Show bg, Show st, Show we, Show ku, Show ws, BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws) => StreckenObjekt (AktionAllgemein bg st we ku ws) where
    pins :: AktionAllgemein bg st we ku ws -> [Pin]
    pins    (Warten _time)              = []
    pins    (AWegstrecke w)             = pins w
    pins    (AWeiche w)                 = pins w
    pins    (ABahngeschwindigkeit b)    = pins b
    pins    (AStreckenabschnitt s)      = pins s
    pins    (AKupplung k)               = pins k
    zugtyp :: AktionAllgemein bg st we ku ws -> Zugtyp
    zugtyp  (Warten _time)              = Undefiniert
    zugtyp  (AWegstrecke w)             = zugtyp w
    zugtyp  (AWeiche w)                 = zugtyp w
    zugtyp  (ABahngeschwindigkeit b)    = zugtyp b
    zugtyp  (AStreckenabschnitt s)      = zugtyp s
    zugtyp  (AKupplung k)               = zugtyp k
    erhalteName :: AktionAllgemein bg st we ku ws -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse bg, StreckenabschnittKlasse st, WeicheKlasse we, KupplungKlasse ku, WegstreckeKlasse ws) => PlanKlasse (AktionAllgemein bg st we ku ws) where
    ausführenPlan :: AktionAllgemein bg st we ku ws -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Warten time)                   _showAktion   = const $ warteµs time
    ausführenPlan (AWegstrecke aktion)            showAktion    = ausführenPlan aktion showAktion
    ausführenPlan (AWeiche aktion)                showAktion    = ausführenPlan aktion showAktion
    ausführenPlan (ABahngeschwindigkeit aktion)   showAktion    = ausführenPlan aktion showAktion
    ausführenPlan (AStreckenabschnitt aktion)     showAktion    = ausführenPlan aktion showAktion
    ausführenPlan (AKupplung aktion)              showAktion    = ausführenPlan aktion showAktion

-- | Bekannte Aktionen einer Wegstrecke
data AktionWegstrecke w = Einstellen w
                        | AWSBahngeschwindigkeit (AktionBahngeschwindigkeit w)
                        | AWSStreckenabschnitt (AktionStreckenabschnitt w)
                        | AWSKupplung (AktionKupplung w)
                            deriving (Eq)

instance (StreckenObjekt w)  => Show (AktionWegstrecke w) where
    show :: AktionWegstrecke w -> String
    show    (Einstellen wegstrecke)     = unpack $ erhalteName wegstrecke <°> Language.einstellen
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
    erhalteName :: AktionWegstrecke w -> Text
    erhalteName = showText

instance (WegstreckeKlasse w) => PlanKlasse (AktionWegstrecke w) where
    ausführenPlan :: AktionWegstrecke w -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Einstellen w)                  _showAktion   = einstellen w
    ausführenPlan (AWSBahngeschwindigkeit aktion) showAktion    = ausführenPlan aktion showAktion
    ausführenPlan (AWSStreckenabschnitt aktion)   showAktion    = ausführenPlan aktion showAktion
    ausführenPlan (AWSKupplung aktion)            showAktion    = ausführenPlan aktion showAktion

-- | Bekannte Aktionen einer Weiche
data AktionWeiche w = Stellen w Richtung
                        deriving (Eq)

instance (StreckenObjekt w)  => Show (AktionWeiche w) where
    show :: AktionWeiche w -> String
    show    (Stellen w richtung)    = unpack $ erhalteName w <°> Language.stellen <=> showText richtung

instance (WeicheKlasse w, Show w) => StreckenObjekt (AktionWeiche w) where
    pins :: AktionWeiche w -> [Pin]
    pins    (Stellen w _richtung)   = pins w
    zugtyp :: AktionWeiche w -> Zugtyp
    zugtyp  (Stellen w _richtung)   = zugtyp w
    erhalteName :: AktionWeiche w -> Text
    erhalteName = showText

instance (WeicheKlasse w) => PlanKlasse (AktionWeiche w) where
    ausführenPlan :: AktionWeiche w -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Stellen w richtung)    _showAktion   = stellen w richtung

-- | Aktionen einer Bahngeschwindigkeit
data AktionBahngeschwindigkeit b    = Geschwindigkeit b Natural
                                    | Umdrehen b (Maybe Fahrtrichtung)
                                        deriving (Eq)

instance (StreckenObjekt b) => Show (AktionBahngeschwindigkeit b) where
    show :: AktionBahngeschwindigkeit b -> String
    show    (Geschwindigkeit b wert)            = unpack $ erhalteName b <°> Language.geschwindigkeit <=> showText wert
    show    (Umdrehen b (Just fahrtrichtung))   = unpack $ erhalteName b <°> Language.umdrehen <=> showText fahrtrichtung
    show    (Umdrehen b Nothing)                = unpack $ erhalteName b <°> Language.umdrehen

instance (BahngeschwindigkeitKlasse b, Show b) => StreckenObjekt (AktionBahngeschwindigkeit b) where
    pins :: AktionBahngeschwindigkeit b -> [Pin]
    pins    (Geschwindigkeit b _wert)   = pins b
    pins    (Umdrehen b _maybe)         = pins b
    zugtyp :: AktionBahngeschwindigkeit b -> Zugtyp
    zugtyp  (Geschwindigkeit b _wert)   = zugtyp b
    zugtyp  (Umdrehen b _maybe)         = zugtyp b
    erhalteName :: AktionBahngeschwindigkeit b -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse b) => PlanKlasse (AktionBahngeschwindigkeit b) where
    ausführenPlan :: AktionBahngeschwindigkeit b -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Geschwindigkeit b wert)        _showAktion   = geschwindigkeit b wert
    ausführenPlan (Umdrehen b maybeFahrtrichtung) _showAktion   = umdrehen b maybeFahrtrichtung

-- | Aktionen eines Streckenabschnitts
data AktionStreckenabschnitt s   = Strom s Strom
                                    deriving (Eq)

instance (StreckenObjekt s) => Show (AktionStreckenabschnitt s) where
    show :: AktionStreckenabschnitt s -> String
    show    (Strom s Fließend)  = unpack $ erhalteName s <°> Language.strom <=> Language.an
    show    (Strom s Gesperrt)  = unpack $ erhalteName s <°> Language.strom <=> Language.aus

instance (StreckenabschnittKlasse s, Show s) => StreckenObjekt (AktionStreckenabschnitt s) where
    pins :: AktionStreckenabschnitt s -> [Pin]
    pins    (Strom s _an)   = pins s
    zugtyp :: AktionStreckenabschnitt s -> Zugtyp
    zugtyp  (Strom s _an)   = zugtyp s
    erhalteName :: AktionStreckenabschnitt s -> Text
    erhalteName = showText

instance (StreckenabschnittKlasse s) => PlanKlasse (AktionStreckenabschnitt s) where
    ausführenPlan :: AktionStreckenabschnitt s -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Strom s an)    _showAktion   = strom s an

-- | Aktionen einer Kupplung
data AktionKupplung k   = Kuppeln k
                            deriving (Eq)

instance (StreckenObjekt k) => Show (AktionKupplung k) where
    show :: AktionKupplung k -> String
    show    (Kuppeln k) = unpack $ erhalteName k <°> Language.kuppeln

instance (KupplungKlasse k, Show k) => StreckenObjekt (AktionKupplung k) where
    pins :: AktionKupplung k -> [Pin]
    pins    (Kuppeln k) = pins k
    zugtyp :: AktionKupplung k -> Zugtyp
    zugtyp  (Kuppeln k) = zugtyp k
    erhalteName :: AktionKupplung k -> Text
    erhalteName = showText

instance (KupplungKlasse k) => PlanKlasse (AktionKupplung k) where
    ausführenPlan :: AktionKupplung k -> (Natural -> IO ()) -> PinMapIO ()
    ausführenPlan (Kuppeln k) _showAktion   = kuppeln k

-- | Hilfsfunktion um den ersten nicht-'Undefiniert'en 'Zugtyp' zu erhalten
findeZugtyp :: (StreckenObjekt s) => [s] -> Zugtyp
findeZugtyp liste = foldr foldZugtypAux Undefiniert liste
    where
        foldZugtypAux :: (StreckenObjekt s) => s -> Zugtyp -> Zugtyp
        foldZugtypAux   s   (Undefiniert)   = zugtyp s
        foldZugtypAux   _s  zugtypWert      = zugtypWert