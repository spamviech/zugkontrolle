{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

{-|
Description : 'Either'-ähnlicher Datentyp für 'StreckenObjekt'-Typen.
-}
module Zug.Objekt (
    Objekt, ObjektAllgemein(..), ObjektElement(..), ObjektKlasse(..),
    ausBG, ausST, ausWE, ausKU, ausWS, ausPL, Phantom(..)) where

-- Bibliotheken
import Data.Kind (Type)
import Data.Text (Text)
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen (Zugtyp(..), ZugtypEither(), ZugtypKlasse(..))
import Zug.Anbindung (Anschluss(), StreckenObjekt(..),
                    Bahngeschwindigkeit(),
                    Streckenabschnitt(),
                    Weiche(),
                    Kupplung(), Wegstrecke())
import Zug.Plan (Plan(..))

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

-- | Klasse für Typen, die ein 'Objekt' enthalten
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