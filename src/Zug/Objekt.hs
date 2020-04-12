{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description : 'Either'-ähnlicher Datentyp für 'StreckenObjekt'-Typen.
-}
module Zug.Objekt (Objekt, ObjektAllgemein(..), ObjektElement(..), ObjektKlasse(..)) where

import Data.Kind (Type)
import Data.Set (Set)
import Data.Text (Text)

import Zug.Anbindung (Anschluss(), StreckenObjekt(..), Bahngeschwindigkeit(..), Streckenabschnitt()
                    , Weiche(), Kupplung(), Wegstrecke())
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), GeschwindigkeitVariante(..)
                , GeschwindigkeitEither(..))
import Zug.Language (Sprache())
import Zug.Plan (Plan(..))

-- | Summen-Typ
data ObjektAllgemein bg st we ku ws pl
    = OBahngeschwindigkeit (ZugtypEither (GeschwindigkeitEither bg))
    | OStreckenabschnitt st
    | OWeiche (ZugtypEither we)
    | OKupplung ku
    | OWegstrecke (ZugtypEither ws)
    | OPlan pl

-- | 'ObjektAllgemein' spezialisiert auf minimal benötigte Typen
type Objekt = ObjektAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

deriving instance ( Eq st
                  , Eq ku
                  , Eq pl
                  , Eq (bg 'Pwm 'Märklin)
                  , Eq (bg 'KonstanteSpannung 'Märklin)
                  , Eq (bg 'Pwm 'Lego)
                  , Eq (bg 'KonstanteSpannung 'Lego)
                  , Eq (we 'Märklin)
                  , Eq (we 'Lego)
                  , Eq (ws 'Märklin)
                  , Eq (ws 'Lego)
                  ) => Eq (ObjektAllgemein bg st we ku ws pl)

instance ( Show (bg 'Pwm 'Märklin)
         , Show (bg 'KonstanteSpannung 'Märklin)
         , Show (bg 'Pwm 'Lego)
         , Show (bg 'KonstanteSpannung 'Lego)
         , Show st
         , Show (we 'Märklin)
         , Show (we 'Lego)
         , Show ku
         , Show (ws 'Märklin)
         , Show (ws 'Lego)
         , Show pl
         ) => Show (ObjektAllgemein bg st we ku ws pl) where
    show :: ObjektAllgemein bg st we ku ws pl -> String
    show (OBahngeschwindigkeit bg) = show bg
    show (OStreckenabschnitt st) = show st
    show (OWeiche we) = show we
    show (OKupplung ku) = show ku
    show (OWegstrecke ws) = show ws
    show (OPlan pl) = show pl

-- | Klasse für Typen, die ein 'Objekt' enthalten
class ObjektElement e where
    zuObjekt :: e -> Objekt

instance (ZugtypKlasse z) => ObjektElement (Bahngeschwindigkeit g z) where
    zuObjekt :: Bahngeschwindigkeit g z -> Objekt
    zuObjekt bg@MärklinBahngeschwindigkeitPwm {} =
        OBahngeschwindigkeit $ ZugtypMärklin $ GeschwindigkeitPwm bg
    zuObjekt bg@MärklinBahngeschwindigkeitKonstanteSpannung {} =
        OBahngeschwindigkeit $ ZugtypMärklin $ GeschwindigkeitKonstanteSpannung bg
    zuObjekt
        bg@LegoBahngeschwindigkeit {} = OBahngeschwindigkeit $ ZugtypLego $ GeschwindigkeitPwm bg

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
    type BG o :: GeschwindigkeitVariante -> Zugtyp -> Type

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

    -- | Sprache
    type SP o :: Type

    -- | Mapping auf 'ObjektAllgemein'. Notwendig für Pattern-Matching.
    erhalteObjekt :: o -> ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (WS o) (PL o)

    -- | Inverses Mapping auf 'ObjektAllgemein'. Ermöglicht Instanz-Nutzung von o.
    ausObjekt :: ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (WS o) (PL o) -> o

instance ObjektKlasse Objekt where
    type BG Objekt = Bahngeschwindigkeit

    type ST Objekt = Streckenabschnitt

    type WE Objekt = Weiche

    type KU Objekt = Kupplung

    type WS Objekt = Wegstrecke

    type PL Objekt = Plan

    type SP Objekt = Sprache

    erhalteObjekt :: Objekt -> Objekt
    erhalteObjekt = id

    ausObjekt :: Objekt -> Objekt
    ausObjekt = id

instance ( StreckenObjekt pl
         , StreckenObjekt (bg 'Pwm 'Märklin)
         , StreckenObjekt (bg 'KonstanteSpannung 'Märklin)
         , StreckenObjekt (bg 'Pwm 'Lego)
         , StreckenObjekt (bg 'KonstanteSpannung 'Lego)
         , StreckenObjekt st
         , StreckenObjekt ku
         , StreckenObjekt (we 'Märklin)
         , StreckenObjekt (we 'Lego)
         , StreckenObjekt (ws 'Märklin)
         , StreckenObjekt (ws 'Lego)
         ) => StreckenObjekt (ObjektAllgemein bg st we ku ws pl) where
    erhalteName :: ObjektAllgemein bg st we ku ws pl -> Text
    erhalteName (OPlan pl) = erhalteName pl
    erhalteName (OWegstrecke ws) = erhalteName ws
    erhalteName (OWeiche we) = erhalteName we
    erhalteName (OBahngeschwindigkeit bg) = erhalteName bg
    erhalteName (OStreckenabschnitt st) = erhalteName st
    erhalteName (OKupplung ku) = erhalteName ku

    anschlüsse :: ObjektAllgemein bg st we ku ws pl -> Set Anschluss
    anschlüsse (OPlan pl) = anschlüsse pl
    anschlüsse (OWegstrecke ws) = anschlüsse ws
    anschlüsse (OWeiche we) = anschlüsse we
    anschlüsse (OBahngeschwindigkeit bg) = anschlüsse bg
    anschlüsse (OStreckenabschnitt st) = anschlüsse st
    anschlüsse (OKupplung ku) = anschlüsse ku