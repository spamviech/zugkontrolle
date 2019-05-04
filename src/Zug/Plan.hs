{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : Pläne sind nacheinander auszuführende Aktionen, welche mit StreckenObjekten möglich sind.

Jede Art von 'StreckenObjekt' ('Bahngeschwindigkeit', 'Streckenabschnitt', 'Weiche', 'Wegstrecke') unterstützt unterschiedliche Aktionen.
Ein 'Plan' ist eine Zusammenfassung mehrerer dieser Aktionen und Wartezeiten, welche nacheinander ausgeführt werden können.
-}
module Zug.Plan (
    -- * Allgemeine Datentypen
    PlanKlasse(..), Plan(..), AktionKlasse(..), Aktion(..), Objekt, ObjektAllgemein(..), ObjektKlasse(..), Ausführend(..),
    -- * Spezialisierte Aktionen
    AktionWeiche(..), AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionKupplung(..), AktionWegstrecke(..)) where

-- Bibliotheken
import Control.Concurrent (forkIO, MVar, modifyMVar_, readMVar)
import Control.Monad (void, when)
import Data.Kind
import Data.List (delete)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen
import Zug.Anbindung
import qualified Zug.Language as Language
import Zug.Language (showText, (<~>), (<^>), (<=>), (<:>), (<°>))

-- | Summen-Typ
data ObjektAllgemein bg st we ku ws pl  = OPlan                 pl
                                        | OWegstrecke           ws
                                        | OWeiche               we
                                        | OBahngeschwindigkeit  bg
                                        | OStreckenabschnitt    st
                                        | OKupplung             ku
                                            deriving (Show)
-- | 'ObjektAllgemein' spezialisiert auf minimal benötigte Typen
type Objekt = ObjektAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

-- | Typ lässt sich in den Summen-Typ 'ObjektAllgemein'
class ObjektKlasse o where
    -- | Bahngeschwindigkeit
    type BG o :: Type
    -- | Streckenabschnitt
    type ST o :: Type
    -- | Weiche
    type WE o :: Type
    -- | Kupplung
    type KU o :: Type
    -- | Wegstrecke
    type WS o :: Type
    -- | Plan
    type PL o :: Type
    -- | Mapping auf 'ObjektAllgemein'. Notwendig für Pattern-Matching.
    erhalteObjekt :: o -> ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (WS o) (PL o)

instance ObjektKlasse (ObjektAllgemein bg st we ku ws pl) where
    type BG (ObjektAllgemein bg st we ku ws pl) = bg
    type ST (ObjektAllgemein bg st we ku ws pl) = st
    type WE (ObjektAllgemein bg st we ku ws pl) = we
    type KU (ObjektAllgemein bg st we ku ws pl) = ku
    type WS (ObjektAllgemein bg st we ku ws pl) = ws
    type PL (ObjektAllgemein bg st we ku ws pl) = pl
    erhalteObjekt :: ObjektAllgemein bg st we ku ws pl -> ObjektAllgemein bg st we ku ws pl
    erhalteObjekt = id

instance (StreckenObjekt pl, StreckenObjekt bg, StreckenObjekt st, StreckenObjekt we, StreckenObjekt ku, StreckenObjekt ws) => StreckenObjekt (ObjektAllgemein bg st we ku ws pl) where
    erhalteName :: ObjektAllgemein bg st we ku ws pl -> Text
    erhalteName (OPlan pl)                  = erhalteName pl
    erhalteName (OWegstrecke ws)            = erhalteName ws
    erhalteName (OWeiche we)                = erhalteName we
    erhalteName (OBahngeschwindigkeit bg)   = erhalteName bg
    erhalteName (OStreckenabschnitt st)     = erhalteName st
    erhalteName (OKupplung ku)              = erhalteName ku
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

-- | Mitglieder dieser Klasse sind ausführbar (können in IO-Aktionen übersetzt werden).
-- Sie können selbst entscheiden, ob sie die mitgegebene Update-Funktion über den Fortschritt informieren, oder nicht.
-- Die Ausführung soll abgebrochen werden, sobald der Plan nicht mehr in der MVar-Liste vorhanden ist.
class PlanKlasse pl where
    ausführenPlan :: pl -> (Natural -> IO ()) -> MVar [Ausführend] -> PinMapIO ()
    {-# MINIMAL ausführenPlan #-}

-- | Pläne: Benannte IO-Aktionen mit StreckenObjekten, bzw. Wartezeiten.
-- Die Update-Funktion wird mit Index der aktuellen Aktion vor dessen Ausführung aufgerufen.
data Plan = Plan {
    plName :: Text,
    plAktionen :: [Aktion]}
        deriving (Eq)

-- | newtype für ausführende Pläne ('Plan')
newtype Ausführend = Ausführend Plan
                            deriving (Eq)

instance Show Plan where
    show :: Plan -> String
    show (Plan {plName, plAktionen})    = Language.plan
                                        <:> Language.name <=> unpack plName
                                        <^> Language.aktionen <=> show plAktionen

instance StreckenObjekt Plan where
    pins :: Plan -> [Pin]
    pins (Plan {plAktionen}) = foldMap pins plAktionen
    zugtyp :: Plan -> Zugtyp
    zugtyp (Plan {plAktionen}) = findeZugtyp plAktionen
    erhalteName :: Plan -> Text
    erhalteName (Plan {plName}) = plName

instance PlanKlasse Plan where
    ausführenPlan :: Plan -> (Natural -> IO ()) -> MVar [Ausführend] -> PinMapIO ()
    ausführenPlan plan@(Plan {plAktionen}) showAktion mvarAusführend mvarPinMap = void $ forkIO $ void $ do
        modifyMVar_ mvarAusführend $ pure . ((Ausführend plan) :)
        ausführenAux 0 plAktionen
        showAktion $ fromIntegral $ length plAktionen
            where
                ausführenAux :: Natural -> [Aktion] ->IO ()
                ausführenAux    _i  ([])    = modifyMVar_ mvarAusführend $ pure . delete (Ausführend plan)
                ausführenAux    i   (h:t)   = do
                    ausführend <- readMVar mvarAusführend
                    when (elem (Ausführend plan) ausführend) $ showAktion i >> ausführenAktion h mvarPinMap >> ausführenAux (succ i) t

-- | Mitglieder dieser Klasse sind ausführbar.
class AktionKlasse a where
    ausführenAktion :: a -> PinMapIO ()

-- | Eine Aktion eines 'StreckenObjekt's oder eine Wartezeit.
-- Die Update-Funktion wird nicht aufgerufen.
data Aktion = Warten                Natural
            | AWegstrecke           (AktionWegstrecke Wegstrecke)
            | AWeiche               (AktionWeiche Weiche)
            | ABahngeschwindigkeit  (AktionBahngeschwindigkeit Bahngeschwindigkeit)
            | AStreckenabschnitt    (AktionStreckenabschnitt Streckenabschnitt)
            | AKupplung             (AktionKupplung Kupplung)
                deriving (Eq)

instance Show Aktion where
    show :: Aktion -> String
    show    (Warten time)               = Language.warten <:> show time <> Language.wartenEinheit
    show    (AWegstrecke a)             = Language.wegstrecke <~> show a
    show    (AWeiche a)                 = Language.weiche <~> show a
    show    (ABahngeschwindigkeit a)    = Language.bahngeschwindigkeit <~> show a
    show    (AStreckenabschnitt a)      = Language.streckenabschnitt <~> show a
    show    (AKupplung a)               = Language.kupplung <~> show a

instance StreckenObjekt Aktion where
    pins :: Aktion -> [Pin]
    pins    (Warten _time)              = []
    pins    (AWegstrecke w)             = pins w
    pins    (AWeiche w)                 = pins w
    pins    (ABahngeschwindigkeit b)    = pins b
    pins    (AStreckenabschnitt s)      = pins s
    pins    (AKupplung k)               = pins k
    zugtyp :: Aktion -> Zugtyp
    zugtyp  (Warten _time)              = Undefiniert
    zugtyp  (AWegstrecke w)             = zugtyp w
    zugtyp  (AWeiche w)                 = zugtyp w
    zugtyp  (ABahngeschwindigkeit b)    = zugtyp b
    zugtyp  (AStreckenabschnitt s)      = zugtyp s
    zugtyp  (AKupplung k)               = zugtyp k
    erhalteName :: Aktion -> Text
    erhalteName = showText

instance AktionKlasse Aktion where
    ausführenAktion :: Aktion -> PinMapIO ()
    ausführenAktion (Warten time)                   = const $ warteµs time
    ausführenAktion (AWegstrecke aktion)            = ausführenAktion aktion
    ausführenAktion (AWeiche aktion)                = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeit aktion)   = ausführenAktion aktion
    ausführenAktion (AStreckenabschnitt aktion)     = ausführenAktion aktion
    ausführenAktion (AKupplung aktion)              = ausführenAktion aktion

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

instance (WegstreckeKlasse w) => AktionKlasse (AktionWegstrecke w) where
    ausführenAktion :: AktionWegstrecke w -> PinMapIO ()
    ausführenAktion (Einstellen w)                  = einstellen w
    ausführenAktion (AWSBahngeschwindigkeit aktion) = ausführenAktion aktion
    ausführenAktion (AWSStreckenabschnitt aktion)   = ausführenAktion aktion
    ausführenAktion (AWSKupplung aktion)            = ausführenAktion aktion

-- | Bekannte Aktionen einer Weiche
data AktionWeiche w = Stellen w Richtung
                        deriving (Eq)

instance (StreckenObjekt w)  => Show (AktionWeiche w) where
    show :: AktionWeiche w -> String
    show (Stellen w richtung) = unpack $ erhalteName w <°> Language.stellen <=> showText richtung

instance (WeicheKlasse w, Show w) => StreckenObjekt (AktionWeiche w) where
    pins :: AktionWeiche w -> [Pin]
    pins (Stellen w _richtung) = pins w
    zugtyp :: AktionWeiche w -> Zugtyp
    zugtyp (Stellen w _richtung) = zugtyp w
    erhalteName :: AktionWeiche w -> Text
    erhalteName = showText

instance (WeicheKlasse w) => AktionKlasse (AktionWeiche w) where
    ausführenAktion :: AktionWeiche w -> PinMapIO ()
    ausführenAktion (Stellen w richtung) = stellen w richtung

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

instance (BahngeschwindigkeitKlasse b) => AktionKlasse (AktionBahngeschwindigkeit b) where
    ausführenAktion :: AktionBahngeschwindigkeit b -> PinMapIO ()
    ausführenAktion (Geschwindigkeit b wert)          = geschwindigkeit b wert
    ausführenAktion (Umdrehen b maybeFahrtrichtung)   = umdrehen b maybeFahrtrichtung

-- | Aktionen eines Streckenabschnitts
data AktionStreckenabschnitt s = Strom s Strom
                                    deriving (Eq)

instance (StreckenObjekt s) => Show (AktionStreckenabschnitt s) where
    show :: AktionStreckenabschnitt s -> String
    show    (Strom s Fließend)  = unpack $ erhalteName s <°> Language.strom <=> Language.an
    show    (Strom s Gesperrt)  = unpack $ erhalteName s <°> Language.strom <=> Language.aus

instance (StreckenabschnittKlasse s, Show s) => StreckenObjekt (AktionStreckenabschnitt s) where
    pins :: AktionStreckenabschnitt s -> [Pin]
    pins (Strom s _an) = pins s
    zugtyp :: AktionStreckenabschnitt s -> Zugtyp
    zugtyp (Strom s _an) = zugtyp s
    erhalteName :: AktionStreckenabschnitt s -> Text
    erhalteName = showText

instance (StreckenabschnittKlasse s) => AktionKlasse (AktionStreckenabschnitt s) where
    ausführenAktion :: AktionStreckenabschnitt s -> PinMapIO ()
    ausführenAktion (Strom s an) = strom s an

-- | Aktionen einer Kupplung
data AktionKupplung k = Kuppeln k
                            deriving (Eq)

instance (StreckenObjekt k) => Show (AktionKupplung k) where
    show :: AktionKupplung k -> String
    show (Kuppeln k) = unpack $ erhalteName k <°> Language.kuppeln

instance (KupplungKlasse k, Show k) => StreckenObjekt (AktionKupplung k) where
    pins :: AktionKupplung k -> [Pin]
    pins (Kuppeln k) = pins k
    zugtyp :: AktionKupplung k -> Zugtyp
    zugtyp (Kuppeln k) = zugtyp k
    erhalteName :: AktionKupplung k -> Text
    erhalteName = showText

instance (KupplungKlasse k) => AktionKlasse (AktionKupplung k) where
    ausführenAktion :: AktionKupplung k -> PinMapIO ()
    ausführenAktion (Kuppeln k) = kuppeln k

-- | Hilfsfunktion um den ersten nicht-'Undefiniert'en 'Zugtyp' zu erhalten
findeZugtyp :: (StreckenObjekt s) => [s] -> Zugtyp
findeZugtyp liste = foldr foldZugtypAux Undefiniert liste
    where
        foldZugtypAux :: (StreckenObjekt s) => s -> Zugtyp -> Zugtyp
        foldZugtypAux   s   (Undefiniert)   = zugtyp s
        foldZugtypAux   _s  zugtypWert      = zugtypWert