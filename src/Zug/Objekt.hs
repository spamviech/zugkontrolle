{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
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

import Zug.Anbindung (AnschlussEither(), StreckenObjekt(..), Bahngeschwindigkeit(..)
                    , Streckenabschnitt(), Weiche(), Kupplung(), Kontakt(), Wegstrecke())
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), GeschwindigkeitVariante(..)
                , GeschwindigkeitEither(..))
import Zug.Language (Anzeige(..), Sprache())
import Zug.Plan (Plan())

-- | Summen-Typ
data ObjektAllgemein bg st we ku ko ws pl
    = OBahngeschwindigkeit (ZugtypEither (GeschwindigkeitEither bg))
    | OStreckenabschnitt st
    | OWeiche (ZugtypEither we)
    | OKupplung ku
    | OKontakt ko
    | OWegstrecke (ZugtypEither ws)
    | OPlan pl

-- | 'ObjektAllgemein' spezialisiert auf minimal benötigte Typen
type Objekt =
    ObjektAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Kontakt Wegstrecke Plan

deriving instance ( Eq st
                  , Eq ku
                  , Eq pl
                  , Eq (bg 'Pwm 'Märklin)
                  , Eq (bg 'KonstanteSpannung 'Märklin)
                  , Eq (bg 'Pwm 'Lego)
                  , Eq (bg 'KonstanteSpannung 'Lego)
                  , Eq (we 'Märklin)
                  , Eq (we 'Lego)
                  , Eq ko
                  , Eq (ws 'Märklin)
                  , Eq (ws 'Lego)
                  ) => Eq (ObjektAllgemein bg st we ku ko ws pl)

instance ( Show (bg 'Pwm 'Märklin)
         , Show (bg 'KonstanteSpannung 'Märklin)
         , Show (bg 'Pwm 'Lego)
         , Show (bg 'KonstanteSpannung 'Lego)
         , Show st
         , Show (we 'Märklin)
         , Show (we 'Lego)
         , Show ku
         , Show ko
         , Show (ws 'Märklin)
         , Show (ws 'Lego)
         , Show pl
         ) => Show (ObjektAllgemein bg st we ku ko ws pl) where
    show :: ObjektAllgemein bg st we ku ko ws pl -> String
    show (OBahngeschwindigkeit bg) = show bg
    show (OStreckenabschnitt st) = show st
    show (OWeiche we) = show we
    show (OKupplung ku) = show ku
    show (OKontakt ko) = show ko
    show (OWegstrecke ws) = show ws
    show (OPlan pl) = show pl

instance ( Anzeige (bg 'Pwm 'Märklin)
         , Anzeige (bg 'KonstanteSpannung 'Märklin)
         , Anzeige (bg 'Pwm 'Lego)
         , Anzeige (bg 'KonstanteSpannung 'Lego)
         , Anzeige st
         , Anzeige (we 'Märklin)
         , Anzeige (we 'Lego)
         , Anzeige ku
         , Anzeige ko
         , Anzeige (ws 'Märklin)
         , Anzeige (ws 'Lego)
         , Anzeige pl
         ) => Anzeige (ObjektAllgemein bg st we ku ko ws pl) where
    anzeige :: ObjektAllgemein bg st we ku ko ws pl -> Sprache -> Text
    anzeige (OBahngeschwindigkeit bg) = anzeige bg
    anzeige (OStreckenabschnitt st) = anzeige st
    anzeige (OWeiche we) = anzeige we
    anzeige (OKupplung ku) = anzeige ku
    anzeige (OKontakt ko) = anzeige ko
    anzeige (OWegstrecke ws) = anzeige ws
    anzeige (OPlan pl) = anzeige pl

-- | Klasse für Typen, die ein 'Objekt' enthalten.
class ObjektElement e where
    type ObjektTyp e :: Type

    type ObjektTyp e = e
    zuObjektTyp :: e -> ObjektTyp e
    default zuObjektTyp :: (e ~ ObjektTyp e) => e -> ObjektTyp e
    zuObjektTyp = id

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

instance ObjektElement Kontakt where
    zuObjekt :: Kontakt -> Objekt
    zuObjekt = OKontakt

instance (ZugtypKlasse z) => ObjektElement (Wegstrecke z) where
    zuObjekt :: Wegstrecke z -> Objekt
    zuObjekt = OWegstrecke . zuZugtypEither

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

    -- | Kontakt
    type KO o :: Type

    -- | Wegstrecke
    type WS o :: Zugtyp -> Type

    -- | Plan
    type PL o :: Type

    -- | Sprache
    type SP o :: Type

    -- | Mapping auf 'ObjektAllgemein'. Notwendig für Pattern-Matching.
    erhalteObjekt :: o -> ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (KO o) (WS o) (PL o)

    -- | Inverses Mapping auf 'ObjektAllgemein'. Ermöglicht Instanz-Nutzung von o.
    ausObjekt :: ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (KO o) (WS o) (PL o) -> o

instance ObjektKlasse Objekt where
    type BG Objekt = Bahngeschwindigkeit

    type ST Objekt = Streckenabschnitt

    type WE Objekt = Weiche

    type KU Objekt = Kupplung

    type KO Objekt = Kontakt

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
         , StreckenObjekt (we 'Märklin)
         , StreckenObjekt (we 'Lego)
         , StreckenObjekt ku
         , StreckenObjekt ko
         , StreckenObjekt (ws 'Märklin)
         , StreckenObjekt (ws 'Lego)
         ) => StreckenObjekt (ObjektAllgemein bg st we ku ko ws pl) where
    erhalteName :: ObjektAllgemein bg st we ku ko ws pl -> Text
    erhalteName (OBahngeschwindigkeit bg) = erhalteName bg
    erhalteName (OStreckenabschnitt st) = erhalteName st
    erhalteName (OWeiche we) = erhalteName we
    erhalteName (OKupplung ku) = erhalteName ku
    erhalteName (OKontakt ko) = erhalteName ko
    erhalteName (OWegstrecke ws) = erhalteName ws
    erhalteName (OPlan pl) = erhalteName pl

    anschlüsse :: ObjektAllgemein bg st we ku ko ws pl -> Set AnschlussEither
    anschlüsse (OBahngeschwindigkeit bg) = anschlüsse bg
    anschlüsse (OStreckenabschnitt st) = anschlüsse st
    anschlüsse (OWeiche we) = anschlüsse we
    anschlüsse (OKupplung ku) = anschlüsse ku
    anschlüsse (OKontakt ko) = anschlüsse ko
    anschlüsse (OWegstrecke ws) = anschlüsse ws
    anschlüsse (OPlan pl) = anschlüsse pl