{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Description : Pläne sind nacheinander auszuführende Aktionen, welche mit StreckenObjekten möglich sind.

Jede Art von 'StreckenObjekt' ('Bahngeschwindigkeit', 'Streckenabschnitt', 'Weiche', 'Wegstrecke') unterstützt unterschiedliche Aktionen.
Ein 'Plan' ist eine Zusammenfassung mehrerer dieser Aktionen und Wartezeiten, welche nacheinander ausgeführt werden können.
-}
module Zug.Plan (
    -- * Allgemeine Datentypen
    PlanKlasse(..), Plan(..), AktionKlasse(..), Aktion(..), Objekt, ObjektAllgemein(..), ObjektKlasse(..), Ausführend(..),
    ausBG, ausST, ausWE, ausKU, ausWS, ausPL, Phantom(..),
    -- * Spezialisierte Aktionen
    AktionWeiche(..), AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..), AktionKupplung(..), AktionWegstrecke(..)) where

-- Bibliotheken
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TVar, readTVarIO, modifyTVar)
import Control.Monad (void, when)
import Data.Kind (Type)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen (Zugtyp(..), Richtung(), Fahrtrichtung(), Strom(..))
import Zug.Anbindung (StreckenObjekt(..), Bahngeschwindigkeit(), BahngeschwindigkeitKlasse(..), Streckenabschnitt(), StreckenabschnittKlasse(..), Weiche(), WeicheKlasse(..), Kupplung(), KupplungKlasse(..), Wegstrecke(), WegstreckeKlasse(..), Pin(), PwmMapIO, warteµs)
import qualified Zug.Language as Language
import Zug.Language (showText, (<~>), (<^>), (<=>), (<:>), (<°>))
import Zug.Menge (Menge(), hinzufügen, entfernen)

-- | Summen-Typ
data ObjektAllgemein bg st we ku ws pl
    = OBahngeschwindigkeit bg
    | OStreckenabschnitt st
    | OKupplung ku
    | OWeiche we
    | OWegstrecke ws
    | OPlan pl
        deriving (Eq)
-- | 'ObjektAllgemein' spezialisiert auf minimal benötigte Typen
type Objekt = ObjektAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

instance (Show bg, Show st, Show we, Show ku, Show ws, Show pl) => Show (ObjektAllgemein bg st we ku ws pl) where
    show :: ObjektAllgemein bg st we ku ws pl -> String
    show    (OBahngeschwindigkeit bg)   = show bg
    show    (OStreckenabschnitt st)     = show st
    show    (OWeiche we)                = show we
    show    (OKupplung ku)              = show ku
    show    (OWegstrecke ws)            = show ws
    show    (OPlan pl)                  = show pl

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
    -- | Inverses Mapping auf 'ObjektAllgemein'. Ermöglicht Instanz-Nutzung von o.
    ausObjekt :: ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (WS o) (PL o) -> o

-- | Erzeuge 'ObjektKlasse' aus einer 'Bahngeschwindigkeit'. Die Liste ist nur zur Typ-Inferenz notwendig und wird nicht verwendet.
ausBG :: (ObjektKlasse o) => Phantom o -> BG o -> o
ausBG _ = ausObjekt . OBahngeschwindigkeit
-- | Erzeuge 'ObjektKlasse' aus einem 'Streckenabschnitt'. Die Liste ist nur zur Typ-Inferenz notwendig und wird nicht verwendet.
ausST :: (ObjektKlasse o) => Phantom o -> ST o -> o
ausST _ = ausObjekt . OStreckenabschnitt
-- | Erzeuge 'ObjektKlasse' aus einer 'Weiche'. Die Liste ist nur zur Typ-Inferenz notwendig und wird nicht verwendet.
ausWE :: (ObjektKlasse o) => Phantom o -> WE o -> o
ausWE _ = ausObjekt . OWeiche
-- | Erzeuge 'ObjektKlasse' aus einer 'Kupplung'. Die Liste ist nur zur Typ-Inferenz notwendig und wird nicht verwendet.
ausKU :: (ObjektKlasse o) => Phantom o -> KU o -> o
ausKU _ = ausObjekt . OKupplung
-- | Erzeuge 'ObjektKlasse' aus einer 'Wegstrecke'. Die Liste ist nur zur Typ-Inferenz notwendig und wird nicht verwendet.
ausWS :: (ObjektKlasse o) => Phantom o -> WS o -> o
ausWS _ = ausObjekt . OWegstrecke
-- | Erzeuge 'ObjektKlasse' aus einem 'Plan'. Die Liste ist nur zur Typ-Inferenz notwendig und wird nicht verwendet.
ausPL :: (ObjektKlasse o) => Phantom o -> PL o -> o
ausPL _ = ausObjekt . OPlan
-- | Wie 'Nothing' aus 'Maybe' o. Wird für Typ-Inferenz benötigt.
data Phantom o = Phantom

instance ObjektKlasse (ObjektAllgemein bg st we ku ws pl) where
    type BG (ObjektAllgemein bg st we ku ws pl) = bg
    type ST (ObjektAllgemein bg st we ku ws pl) = st
    type WE (ObjektAllgemein bg st we ku ws pl) = we
    type KU (ObjektAllgemein bg st we ku ws pl) = ku
    type WS (ObjektAllgemein bg st we ku ws pl) = ws
    type PL (ObjektAllgemein bg st we ku ws pl) = pl
    erhalteObjekt :: ObjektAllgemein bg st we ku ws pl -> ObjektAllgemein bg st we ku ws pl
    erhalteObjekt = id
    ausObjekt :: ObjektAllgemein bg st we ku ws pl -> ObjektAllgemein bg st we ku ws pl
    ausObjekt = id

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
-- Sie können selbst entscheiden, wann sie die mitgegebene Update-Funktion über den Fortschritt informieren.  
-- Nach der kompletten Ausführung soll der End-Aktion ausgeführt werden.  
-- Die Ausführung soll abgebrochen werden, sobald der Plan nicht mehr in der 'TVar'-Liste vorhanden ist.
class PlanKlasse pl where
    ausführenPlan :: pl -> (Natural -> IO ()) -> IO () -> TVar (Menge Ausführend) -> PwmMapIO ()
    {-# MINIMAL ausführenPlan #-}

-- | Pläne: Benannte IO-Aktionen mit StreckenObjekten, bzw. Wartezeiten.
-- Die Update-Funktion wird mit Index der aktuellen Aktion vor dessen Ausführung aufgerufen.
data Plan = Plan {
    plName :: Text,
    plAktionen :: [Aktion]}
        deriving (Eq)

-- | newtype für ausführende Pläne ('Plan')
newtype Ausführend = Ausführend Plan
                        deriving (Eq, StreckenObjekt)

instance Show Plan where
    show :: Plan -> String
    show (Plan {plName, plAktionen}) = Language.plan
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
    ausführenPlan :: Plan -> (Natural -> IO ()) -> IO () -> TVar (Menge Ausführend) -> PwmMapIO ()
    ausführenPlan plan@(Plan {plAktionen}) showAktion endAktion tvarAusführend tvarPwmMap = void $ forkIO $ void $ do
        atomically $ modifyTVar tvarAusführend $ hinzufügen (Ausführend plan)
        ausführenAux 0 plAktionen
        showAktion $ fromIntegral $ length plAktionen
        endAktion
            where
                ausführenAux :: Natural -> [Aktion] ->IO ()
                ausführenAux    _i  ([])    = atomically $ modifyTVar tvarAusführend $ entfernen (Ausführend plan)
                ausführenAux    i   (h:t)   = do
                    ausführend <- readTVarIO tvarAusführend
                    when (elem (Ausführend plan) ausführend) $ do
                        showAktion i
                        ausführenAktion h tvarPwmMap
                        ausführenAux (succ i) t

-- | Mitglieder dieser Klasse sind ausführbar.
class AktionKlasse a where
    ausführenAktion :: a -> PwmMapIO ()

-- | Eine Aktion eines 'StreckenObjekt's oder eine Wartezeit.
-- Die Update-Funktion wird nicht aufgerufen.
data Aktion = Warten
                Natural
            | AWegstrecke
                (AktionWegstrecke Wegstrecke)
            | AWeiche
                (AktionWeiche Weiche)
            | ABahngeschwindigkeit
                (AktionBahngeschwindigkeit Bahngeschwindigkeit)
            | AStreckenabschnitt
                (AktionStreckenabschnitt Streckenabschnitt)
            | AKupplung
                (AktionKupplung Kupplung)
                    deriving (Eq)

instance Show Aktion where
    show :: Aktion -> String
    show    (Warten time)                   = Language.warten <:> show time <> Language.wartenEinheit
    show    (AWegstrecke aktion)            = Language.wegstrecke <~> show aktion
    show    (AWeiche aktion)                = Language.weiche <~> show aktion
    show    (ABahngeschwindigkeit aktion)   = Language.bahngeschwindigkeit <~> show aktion
    show    (AStreckenabschnitt aktion)     = Language.streckenabschnitt <~> show aktion
    show    (AKupplung aktion)              = Language.kupplung <~> show aktion

instance StreckenObjekt Aktion where
    pins :: Aktion -> [Pin]
    pins    (Warten _zeit)                  = []
    pins    (AWegstrecke aktion)            = pins aktion
    pins    (AWeiche aktion)                = pins aktion
    pins    (ABahngeschwindigkeit aktion)   = pins aktion
    pins    (AStreckenabschnitt aktion)     = pins aktion
    pins    (AKupplung aktion)              = pins aktion
    zugtyp :: Aktion -> Zugtyp
    zugtyp  (Warten _zeit)                  = Undefiniert
    zugtyp  (AWegstrecke aktion)            = zugtyp aktion
    zugtyp  (AWeiche aktion)                = zugtyp aktion
    zugtyp  (ABahngeschwindigkeit aktion)   = zugtyp aktion
    zugtyp  (AStreckenabschnitt aktion)     = zugtyp aktion
    zugtyp  (AKupplung aktion)              = zugtyp aktion
    erhalteName :: Aktion -> Text
    erhalteName = showText

instance AktionKlasse Aktion where
    ausführenAktion :: Aktion -> PwmMapIO ()
    ausführenAktion (Warten time)                   = const $ warteµs time
    ausführenAktion (AWegstrecke aktion)            = ausführenAktion aktion
    ausführenAktion (AWeiche aktion)                = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeit aktion)   = ausführenAktion aktion
    ausführenAktion (AStreckenabschnitt aktion)     = ausführenAktion aktion
    ausführenAktion (AKupplung aktion)              = ausführenAktion aktion

-- | Bekannte 'Aktion'en einer 'Wegstrecke'
data AktionWegstrecke ws    = Einstellen
                                ws
                            | AWSBahngeschwindigkeit
                                (AktionBahngeschwindigkeit ws)
                            | AWSStreckenabschnitt
                                (AktionStreckenabschnitt ws)
                            | AWSKupplung
                                (AktionKupplung ws)
                                    deriving (Eq)

instance (StreckenObjekt ws)  => Show (AktionWegstrecke ws) where
    show :: AktionWegstrecke ws -> String
    show    (Einstellen wegstrecke)         = unpack $ erhalteName wegstrecke <°> Language.einstellen
    show    (AWSBahngeschwindigkeit aktion) = show aktion
    show    (AWSStreckenabschnitt aktion)   = show aktion
    show    (AWSKupplung aktion)            = show aktion

instance (WegstreckeKlasse ws, Show ws) => StreckenObjekt (AktionWegstrecke ws) where
    pins :: AktionWegstrecke ws -> [Pin]
    pins    (Einstellen ws)                 = pins ws
    pins    (AWSBahngeschwindigkeit aktion) = pins aktion
    pins    (AWSStreckenabschnitt aktion)   = pins aktion
    pins    (AWSKupplung aktion)            = pins aktion
    zugtyp :: AktionWegstrecke ws -> Zugtyp
    zugtyp  (Einstellen ws)                 = zugtyp ws
    zugtyp  (AWSBahngeschwindigkeit aktion) = zugtyp aktion
    zugtyp  (AWSStreckenabschnitt aktion)   = zugtyp aktion
    zugtyp  (AWSKupplung aktion)            = zugtyp aktion
    erhalteName :: AktionWegstrecke ws -> Text
    erhalteName = showText

instance (WegstreckeKlasse ws) => AktionKlasse (AktionWegstrecke ws) where
    ausführenAktion :: AktionWegstrecke ws -> PwmMapIO ()
    ausführenAktion (Einstellen ws)                 = einstellen ws
    ausführenAktion (AWSBahngeschwindigkeit aktion) = ausführenAktion aktion
    ausführenAktion (AWSStreckenabschnitt aktion)   = ausführenAktion aktion
    ausführenAktion (AWSKupplung aktion)            = ausführenAktion aktion

-- | Bekannte 'Aktion'en einer 'Weiche'
data AktionWeiche we = Stellen
                        we
                        Richtung
                            deriving (Eq)

instance (StreckenObjekt we)  => Show (AktionWeiche we) where
    show :: AktionWeiche we -> String
    show (Stellen we richtung) = unpack $ erhalteName we <°> Language.stellen <=> showText richtung

instance (WeicheKlasse we, Show we) => StreckenObjekt (AktionWeiche we) where
    pins :: AktionWeiche we -> [Pin]
    pins (Stellen we _richtung) = pins we
    zugtyp :: AktionWeiche we -> Zugtyp
    zugtyp (Stellen we _richtung) = zugtyp we
    erhalteName :: AktionWeiche we -> Text
    erhalteName = showText

instance (WeicheKlasse w) => AktionKlasse (AktionWeiche w) where
    ausführenAktion :: AktionWeiche w -> PwmMapIO ()
    ausführenAktion (Stellen we richtung) = stellen we richtung
-- | Aktionen einer Bahngeschwindigkeit
data AktionBahngeschwindigkeit bg   = Geschwindigkeit
                                        bg
                                        Natural
                                    | Umdrehen
                                        bg
                                        (Maybe Fahrtrichtung)
                                            deriving (Eq)

instance (StreckenObjekt bg) => Show (AktionBahngeschwindigkeit bg) where
    show :: AktionBahngeschwindigkeit bg -> String
    show    (Geschwindigkeit bg wert)           = unpack $ erhalteName bg <°> Language.geschwindigkeit <=> showText wert
    show    (Umdrehen bg (Just fahrtrichtung))  = unpack $ erhalteName bg <°> Language.umdrehen <=> showText fahrtrichtung
    show    (Umdrehen bg Nothing)               = unpack $ erhalteName bg <°> Language.umdrehen

instance (BahngeschwindigkeitKlasse bg, Show bg) => StreckenObjekt (AktionBahngeschwindigkeit bg) where
    pins :: AktionBahngeschwindigkeit bg -> [Pin]
    pins    (Geschwindigkeit bg _wert)  = pins bg
    pins    (Umdrehen bg _maybe)        = pins bg
    zugtyp :: AktionBahngeschwindigkeit bg -> Zugtyp
    zugtyp  (Geschwindigkeit bg _wert)  = zugtyp bg
    zugtyp  (Umdrehen bg _maybe)        = zugtyp bg
    erhalteName :: AktionBahngeschwindigkeit bg -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse bg) => AktionKlasse (AktionBahngeschwindigkeit bg) where
    ausführenAktion :: AktionBahngeschwindigkeit bg -> PwmMapIO ()
    ausführenAktion (Geschwindigkeit bg wert)           = geschwindigkeit bg wert
    ausführenAktion (Umdrehen bg maybeFahrtrichtung)    = umdrehen bg maybeFahrtrichtung

-- | Aktionen eines Streckenabschnitts
data AktionStreckenabschnitt st = Strom
                                    st
                                    Strom
                                        deriving (Eq)

instance (StreckenObjekt st) => Show (AktionStreckenabschnitt st) where
    show :: AktionStreckenabschnitt st -> String
    show    (Strom st Fließend)  = unpack $ erhalteName st <°> Language.strom <=> Language.an
    show    (Strom st Gesperrt)  = unpack $ erhalteName st <°> Language.strom <=> Language.aus

instance (StreckenabschnittKlasse st, Show st) => StreckenObjekt (AktionStreckenabschnitt st) where
    pins :: AktionStreckenabschnitt st -> [Pin]
    pins (Strom st _an) = pins st
    zugtyp :: AktionStreckenabschnitt st -> Zugtyp
    zugtyp (Strom st _an) = zugtyp st
    erhalteName :: AktionStreckenabschnitt st -> Text
    erhalteName = showText

instance (StreckenabschnittKlasse st) => AktionKlasse (AktionStreckenabschnitt st) where
    ausführenAktion :: AktionStreckenabschnitt st -> PwmMapIO ()
    ausführenAktion (Strom st an) = strom st an

-- | Aktionen einer Kupplung
data AktionKupplung ku = Kuppeln
                            ku
                                deriving (Eq)

instance (StreckenObjekt ku) => Show (AktionKupplung ku) where
    show :: AktionKupplung ku -> String
    show (Kuppeln ku) = unpack $ erhalteName ku <°> Language.kuppeln

instance (KupplungKlasse ku, Show ku) => StreckenObjekt (AktionKupplung ku) where
    pins :: AktionKupplung ku -> [Pin]
    pins (Kuppeln ku) = pins ku
    zugtyp :: AktionKupplung ku -> Zugtyp
    zugtyp (Kuppeln ku) = zugtyp ku
    erhalteName :: AktionKupplung ku -> Text
    erhalteName = showText

instance (KupplungKlasse ku) => AktionKlasse (AktionKupplung ku) where
    ausführenAktion :: AktionKupplung ku -> PwmMapIO ()
    ausführenAktion (Kuppeln ku) = kuppeln ku

-- | Hilfsfunktion um den ersten nicht-'Undefiniert'en 'Zugtyp' zu erhalten
findeZugtyp :: (StreckenObjekt s) => [s] -> Zugtyp
findeZugtyp liste = foldr foldZugtypAux Undefiniert liste
    where
        foldZugtypAux :: (StreckenObjekt s) => s -> Zugtyp -> Zugtyp
        foldZugtypAux   s   (Undefiniert)   = zugtyp s
        foldZugtypAux   _s  zugtypWert      = zugtypWert