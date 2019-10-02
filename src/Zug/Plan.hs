{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description : Pläne sind nacheinander auszuführende Aktionen, welche mit StreckenObjekten möglich sind.

Jede Art von 'StreckenObjekt' ('Bahngeschwindigkeit', 'Streckenabschnitt', 'Weiche', 'Wegstrecke') unterstützt unterschiedliche Aktionen.
Ein 'Plan' ist eine Zusammenfassung mehrerer dieser Aktionen und Wartezeiten, welche nacheinander ausgeführt werden können.
-}
module Zug.Plan (
    -- * Allgemeine Datentypen
    PlanKlasse(..), MitAusführend(..), AusführendReader(..), Plan(..), AktionKlasse(..), Aktion(..),
    Objekt, ObjektAllgemein(..), ObjektElement(..), ObjektKlasse(..), Ausführend(..),
    ausBG, ausST, ausWE, ausKU, ausWS, ausPL, Phantom(..),
    -- * Spezialisierte Aktionen
    AktionWeiche(..), AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..),
    AktionKupplung(..), AktionWegstrecke(..)) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, TVar, readTVarIO, modifyTVar)
import Control.Monad (void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (MonadIO(..))
import Data.Kind (Type)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen (Zugtyp(..), ZugtypEither(), ZugtypKlasse(..), Richtung(), Fahrtrichtung(), Strom(..))
import Zug.Anbindung (Anschluss(), StreckenObjekt(..),
                    PwmReader(..), I2CReader(..),
                    Bahngeschwindigkeit(), BahngeschwindigkeitKlasse(..),
                    Streckenabschnitt(), StreckenabschnittKlasse(..),
                    Weiche(), WeicheKlasse(..),
                    Kupplung(), KupplungKlasse(..),
                    Wegstrecke(), WegstreckeKlasse(..),
                    warte, Wartezeit(..))
import qualified Zug.Language as Language
import Zug.Language (showText, (<~>), (<^>), (<=>), (<:>), (<°>))
import Zug.Menge (Menge(), hinzufügen, entfernen)

-- | Summen-Typ
data ObjektAllgemein bg st we ku ws pl
    = OBahngeschwindigkeit (ZugtypEither bg)
    | OStreckenabschnitt st
    | OWeiche (ZugtypEither we)
    | OKupplung ku
    | OWegstrecke (ZugtypEither ws)
    | OPlan pl
-- | 'ObjektAllgemein' spezialisiert auf minimal benötigte Typen
type Objekt = ObjektAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

deriving instance (Eq st, Eq ku, Eq pl, Eq (bg 'Märklin), Eq (bg 'Lego),
                    Eq (we 'Märklin), Eq (we 'Lego), Eq (ws 'Märklin), Eq (ws 'Lego))
                    => Eq (ObjektAllgemein bg st we ku ws pl)

instance (Show (bg 'Märklin), Show (bg 'Lego), Show st, Show (we 'Märklin), Show (we 'Lego),
        Show ku, Show (ws 'Märklin), Show (ws 'Lego), Show pl) => Show (ObjektAllgemein bg st we ku ws pl) where
    show :: ObjektAllgemein bg st we ku ws pl -> String
    show    (OBahngeschwindigkeit bg)   = show bg
    show    (OStreckenabschnitt st)     = show st
    show    (OWeiche we)                = show we
    show    (OKupplung ku)              = show ku
    show    (OWegstrecke ws)            = show ws
    show    (OPlan pl)                  = show pl

-- | Klasse für Typen die sich in ein 'Objekt' transformieren lassen
class ObjektElement e where
    zuObjekt :: e -> Objekt

instance (ZugtypKlasse z) => ObjektElement (Bahngeschwindigkeit z) where
    zuObjekt :: Bahngeschwindigkeit z -> Objekt
    zuObjekt = OBahngeschwindigkeit . zuZugtypEither

instance ObjektElement Streckenabschnitt where
    zuObjekt :: Streckenabschnitt -> Objekt
    zuObjekt = OStreckenabschnitt

instance (ZugtypKlasse z) => ObjektElement (Weiche z) where
    zuObjekt :: Weiche z -> Objekt
    zuObjekt = OWeiche . zuZugtypEither

instance ObjektElement Kupplung where
    zuObjekt :: Kupplung -> Objekt
    zuObjekt = OKupplung

instance (ZugtypKlasse z) => ObjektElement (Wegstrecke z) where
    zuObjekt :: Wegstrecke z -> Objekt
    zuObjekt = OWegstrecke . zuZugtypEither

instance ObjektElement Plan where
    zuObjekt :: Plan -> Objekt
    zuObjekt = OPlan

-- | Typ lässt sich in den Summen-Typ 'ObjektAllgemein'
class ObjektKlasse o where
    -- | Bahngeschwindigkeit
    type BG o :: Zugtyp -> Type
    -- | Streckenabschnitt
    type ST o :: Type
    -- | Weiche
    type WE o :: Zugtyp -> Type
    -- | Kupplung
    type KU o :: Type
    -- | Wegstrecke
    type WS o :: Zugtyp -> Type
    -- | Plan
    type PL o :: Type
    -- | Mapping auf 'ObjektAllgemein'. Notwendig für Pattern-Matching.
    erhalteObjekt :: o -> ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (WS o) (PL o)
    -- | Inverses Mapping auf 'ObjektAllgemein'. Ermöglicht Instanz-Nutzung von o.
    ausObjekt :: ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (WS o) (PL o) -> o

-- | Erzeuge 'ObjektKlasse' aus einer 'Bahngeschwindigkeit'.
ausBG :: (ObjektKlasse o) => Phantom o -> ZugtypEither (BG o) -> o
ausBG _ = ausObjekt . OBahngeschwindigkeit
-- | Erzeuge 'ObjektKlasse' aus einem 'Streckenabschnitt'.
ausST :: (ObjektKlasse o) => Phantom o -> ST o -> o
ausST _ = ausObjekt . OStreckenabschnitt
-- | Erzeuge 'ObjektKlasse' aus einer 'Weiche'.
ausWE :: (ObjektKlasse o) => Phantom o -> ZugtypEither (WE o) -> o
ausWE _ = ausObjekt . OWeiche
-- | Erzeuge 'ObjektKlasse' aus einer 'Kupplung'.
ausKU :: (ObjektKlasse o) => Phantom o -> KU o -> o
ausKU _ = ausObjekt . OKupplung
-- | Erzeuge 'ObjektKlasse' aus einer 'Wegstrecke'.
ausWS :: (ObjektKlasse o) => Phantom o -> ZugtypEither (WS o) -> o
ausWS _ = ausObjekt . OWegstrecke
-- | Erzeuge 'ObjektKlasse' aus einem 'Plan'.
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

instance (StreckenObjekt pl, StreckenObjekt (bg 'Märklin), StreckenObjekt (bg 'Lego),
        StreckenObjekt st, StreckenObjekt ku, StreckenObjekt (we 'Märklin), StreckenObjekt (we 'Lego),
        StreckenObjekt (ws 'Märklin), StreckenObjekt (ws 'Lego))
            => StreckenObjekt (ObjektAllgemein bg st we ku ws pl) where
    erhalteName :: ObjektAllgemein bg st we ku ws pl -> Text
    erhalteName (OPlan pl)                  = erhalteName pl
    erhalteName (OWegstrecke ws)            = erhalteName ws
    erhalteName (OWeiche we)                = erhalteName we
    erhalteName (OBahngeschwindigkeit bg)   = erhalteName bg
    erhalteName (OStreckenabschnitt st)     = erhalteName st
    erhalteName (OKupplung ku)              = erhalteName ku
    anschlüsse ::ObjektAllgemein bg st we ku ws pl -> [Anschluss]
    anschlüsse  (OPlan pl)                  = anschlüsse pl
    anschlüsse  (OWegstrecke ws)            = anschlüsse ws
    anschlüsse  (OWeiche we)                = anschlüsse we
    anschlüsse  (OBahngeschwindigkeit bg)   = anschlüsse bg
    anschlüsse  (OStreckenabschnitt st)     = anschlüsse st
    anschlüsse  (OKupplung ku)              = anschlüsse ku

-- | Klasse für Typen mit den aktuell 'Ausführend'en Plänen
class MitAusführend r where
    mengeAusführend :: r -> TVar (Menge Ausführend)
-- | Abkürzung für Funktionen, die die aktuelle 'Ausführend'-'Menge' benötigen
class (PwmReader r m, MitAusführend r) => AusführendReader r m where
    -- | Erhalte die aktuelle 'Ausführend'-'Menge' aus der Umgebung.
    erhalteMengeAusführend :: m (TVar (Menge Ausführend))
    erhalteMengeAusführend = asks mengeAusführend
instance (PwmReader r m, MitAusführend r) => AusführendReader r m

-- | Mitglieder dieser Klasse sind ausführbar (können in IO-Aktionen übersetzt werden).  
-- Sie können selbst entscheiden, wann sie die mitgegebene Update-Funktion über den Fortschritt informieren.  
-- Nach der kompletten Ausführung soll der End-Aktion ausgeführt werden.  
-- Die Ausführung soll abgebrochen werden, sobald der Plan nicht mehr in der 'TVar'-'Menge' vorhanden ist.
class PlanKlasse pl where
    ausführenPlan :: (AusführendReader r m, MonadIO m) => pl -> (Natural -> IO ()) -> IO () -> m ()
    {-# MINIMAL ausführenPlan #-}

-- | Pläne: Benannte IO-Aktionen mit StreckenObjekten, bzw. Wartezeiten.
-- Die Update-Funktion wird mit Index der aktuellen Aktion vor dessen Ausführung aufgerufen.
data Plan = Plan {
    plName :: Text,
    plAktionen :: [Aktion]}
        deriving (Eq)

-- | newtype für ausführende Pläne ('Plan')
newtype Ausführend
    = Ausführend Plan
        deriving (Eq, StreckenObjekt)

instance Show Plan where
    show :: Plan -> String
    show (Plan {plName, plAktionen}) = Language.plan
                                    <:> Language.name <=> unpack plName
                                    <^> Language.aktionen <=> show plAktionen

instance StreckenObjekt Plan where
    anschlüsse :: Plan -> [Anschluss]
    anschlüsse (Plan {plAktionen}) = foldMap anschlüsse plAktionen
    erhalteName :: Plan -> Text
    erhalteName (Plan {plName}) = plName

instance PlanKlasse Plan where
    ausführenPlan :: (AusführendReader r m, MonadIO m) => Plan -> (Natural -> IO ()) -> IO () -> m ()
    ausführenPlan plan@(Plan {plAktionen}) showAktion endAktion = void $ forkI2CReader $ void $ do
        tvarAusführend <- erhalteMengeAusführend
        liftIO $ atomically $ modifyTVar tvarAusführend $ hinzufügen (Ausführend plan)
        ausführenAux 0 plAktionen
        liftIO $ do
            showAktion $ fromIntegral $ length plAktionen
            endAktion
        where
            ausführenAux :: (AusführendReader r m, MonadIO m) => Natural -> [Aktion] -> m ()
            ausführenAux    _i  []      = do
                tvarAusführend <- erhalteMengeAusführend
                liftIO $ atomically $ modifyTVar tvarAusführend $ entfernen $ Ausführend plan
            ausführenAux    i   (h:t)   = do
                tvarAusführend <- erhalteMengeAusführend
                ausführend <- liftIO $ readTVarIO tvarAusführend
                when (elem (Ausführend plan) ausführend) $ do
                    liftIO $ showAktion i
                    ausführenAktion h
                    ausführenAux (succ i) t

-- | Mitglieder dieser Klasse sind ausführbar.
class AktionKlasse a where
    ausführenAktion :: (PwmReader r m, MonadIO m) => a -> m ()

-- | Eine Aktion eines 'StreckenObjekt's oder eine Wartezeit.
-- Die Update-Funktion wird nicht aufgerufen.
data Aktion
    = Warten
        Wartezeit
    | AWegstreckeMärklin
        (AktionWegstrecke Wegstrecke 'Märklin)
    | AWegstreckeLego
        (AktionWegstrecke Wegstrecke 'Lego)
    | AWeiche
        (AktionWeiche (ZugtypEither Weiche))
    | ABahngeschwindigkeitMärklin
        (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Märklin)
    | ABahngeschwindigkeitLego
        (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Lego)
    | AStreckenabschnitt
        (AktionStreckenabschnitt Streckenabschnitt)
    | AKupplung
        (AktionKupplung Kupplung)
            deriving (Eq)

instance Show Aktion where
    show :: Aktion -> String
    show    (Warten time)                           = Language.warten <:> show time <> Language.wartenEinheit
    show    (AWegstreckeMärklin aktion)             = Language.wegstrecke <~> show aktion
    show    (AWegstreckeLego aktion)                = Language.wegstrecke <~> show aktion
    show    (AWeiche aktion)                        = Language.weiche <~> show aktion
    show    (ABahngeschwindigkeitMärklin aktion)    = Language.bahngeschwindigkeit <~> show aktion
    show    (ABahngeschwindigkeitLego aktion)       = Language.bahngeschwindigkeit <~> show aktion
    show    (AStreckenabschnitt aktion)             = Language.streckenabschnitt <~> show aktion
    show    (AKupplung aktion)                      = Language.kupplung <~> show aktion

instance StreckenObjekt Aktion where
    anschlüsse :: Aktion -> [Anschluss]
    anschlüsse  (Warten _zeit)                          = []
    anschlüsse  (AWegstreckeMärklin aktion)             = anschlüsse aktion
    anschlüsse  (AWegstreckeLego aktion)                = anschlüsse aktion
    anschlüsse  (AWeiche aktion)                        = anschlüsse aktion
    anschlüsse  (ABahngeschwindigkeitMärklin aktion)    = anschlüsse aktion
    anschlüsse  (ABahngeschwindigkeitLego aktion)       = anschlüsse aktion
    anschlüsse  (AStreckenabschnitt aktion)             = anschlüsse aktion
    anschlüsse  (AKupplung aktion)                      = anschlüsse aktion
    erhalteName :: Aktion -> Text
    erhalteName = showText

instance AktionKlasse Aktion where
    ausführenAktion :: (PwmReader r m, MonadIO m) => Aktion -> m ()
    ausführenAktion (Warten time)                           = warte time
    ausführenAktion (AWegstreckeMärklin aktion)             = ausführenAktion aktion
    ausführenAktion (AWegstreckeLego aktion)                = ausführenAktion aktion
    ausführenAktion (AWeiche aktion)                        = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitMärklin aktion)    = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitLego aktion)       = ausführenAktion aktion
    ausführenAktion (AStreckenabschnitt aktion)             = ausführenAktion aktion
    ausführenAktion (AKupplung aktion)                      = ausführenAktion aktion

-- | Bekannte 'Aktion'en einer 'Wegstrecke'
data AktionWegstrecke ws (z :: Zugtyp)
    = Einstellen
        (ws z)
    | AWSBahngeschwindigkeit
        (AktionBahngeschwindigkeit ws z)
    | AWSStreckenabschnitt
        (AktionStreckenabschnitt (ws z))
    | AWSKupplung
        (AktionKupplung (ws z))
            deriving (Eq)

instance (StreckenObjekt (ws z))  => Show (AktionWegstrecke ws z) where
    show :: AktionWegstrecke ws z -> String
    show    (Einstellen wegstrecke)         = unpack $ erhalteName wegstrecke <°> Language.einstellen
    show    (AWSBahngeschwindigkeit aktion) = show aktion
    show    (AWSStreckenabschnitt aktion)   = show aktion
    show    (AWSKupplung aktion)            = show aktion

instance (BahngeschwindigkeitKlasse ws, WegstreckeKlasse (ws z), Show (ws z)) => StreckenObjekt (AktionWegstrecke ws z) where
    anschlüsse :: AktionWegstrecke ws z -> [Anschluss]
    anschlüsse  (Einstellen ws)                 = anschlüsse ws
    anschlüsse  (AWSBahngeschwindigkeit aktion) = anschlüsse aktion
    anschlüsse  (AWSStreckenabschnitt aktion)   = anschlüsse aktion
    anschlüsse  (AWSKupplung aktion)            = anschlüsse aktion
    erhalteName :: AktionWegstrecke ws z -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse ws, WegstreckeKlasse (ws z)) => AktionKlasse (AktionWegstrecke ws z) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionWegstrecke ws z -> m ()
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
    anschlüsse :: AktionWeiche we -> [Anschluss]
    anschlüsse (Stellen we _richtung) = anschlüsse we
    erhalteName :: AktionWeiche we -> Text
    erhalteName = showText

instance (WeicheKlasse w) => AktionKlasse (AktionWeiche w) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionWeiche w -> m ()
    ausführenAktion (Stellen we richtung) = stellen we richtung

-- | Aktionen einer Bahngeschwindigkeit
data AktionBahngeschwindigkeit bg (z :: Zugtyp) where
    Geschwindigkeit ::
        bg z ->
        Natural ->
            AktionBahngeschwindigkeit bg z
    Umdrehen ::
        bg 'Märklin ->
            AktionBahngeschwindigkeit bg 'Märklin
    FahrtrichtungEinstellen ::
        bg 'Lego ->
        Fahrtrichtung ->
            AktionBahngeschwindigkeit bg 'Lego

deriving instance (Eq (bg z)) => (Eq (AktionBahngeschwindigkeit bg z))

instance (StreckenObjekt (bg z)) => Show (AktionBahngeschwindigkeit bg z) where
    show :: AktionBahngeschwindigkeit bg  z-> String
    show
        (Geschwindigkeit bg wert)
            = unpack $ erhalteName bg <°> Language.geschwindigkeit <=> showText wert
    show
        (Umdrehen bg)
            = unpack $ erhalteName bg <°> Language.umdrehen
    show
        (FahrtrichtungEinstellen bg fahrtrichtung)
            = unpack $ erhalteName bg <°> Language.umdrehen <=> showText fahrtrichtung

instance (BahngeschwindigkeitKlasse bg, Show (bg z), StreckenObjekt (bg 'Märklin), StreckenObjekt (bg 'Lego), StreckenObjekt (bg z))
            => StreckenObjekt (AktionBahngeschwindigkeit bg z) where
    anschlüsse :: AktionBahngeschwindigkeit bg z -> [Anschluss]
    anschlüsse  (Geschwindigkeit bg _wert)                  = anschlüsse bg
    anschlüsse  (Umdrehen bg)                               = anschlüsse bg
    anschlüsse  (FahrtrichtungEinstellen bg _fahrtrichtung) = anschlüsse bg
    erhalteName :: AktionBahngeschwindigkeit bg z -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse bg) => AktionKlasse (AktionBahngeschwindigkeit bg z) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionBahngeschwindigkeit bg z -> m ()
    ausführenAktion (Geschwindigkeit bg wert)                   = geschwindigkeit bg wert
    ausführenAktion (Umdrehen bg)                               = umdrehen bg
    ausführenAktion (FahrtrichtungEinstellen bg fahrtrichtung)  = fahrtrichtungEinstellen bg fahrtrichtung

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
    anschlüsse :: AktionStreckenabschnitt st -> [Anschluss]
    anschlüsse (Strom st _an) = anschlüsse st
    erhalteName :: AktionStreckenabschnitt st -> Text
    erhalteName = showText

instance (StreckenabschnittKlasse st) => AktionKlasse (AktionStreckenabschnitt st) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionStreckenabschnitt st -> m ()
    ausführenAktion (Strom st an) = strom st an

-- | Aktionen einer Kupplung
data AktionKupplung ku = Kuppeln
                            ku
                                deriving (Eq)

instance (StreckenObjekt ku) => Show (AktionKupplung ku) where
    show :: AktionKupplung ku -> String
    show (Kuppeln ku) = unpack $ erhalteName ku <°> Language.kuppeln

instance (KupplungKlasse ku, Show ku) => StreckenObjekt (AktionKupplung ku) where
    anschlüsse :: AktionKupplung ku -> [Anschluss]
    anschlüsse (Kuppeln ku) = anschlüsse ku
    erhalteName :: AktionKupplung ku -> Text
    erhalteName = showText

instance (KupplungKlasse ku) => AktionKlasse (AktionKupplung ku) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionKupplung ku -> m ()
    ausführenAktion (Kuppeln ku) = kuppeln ku