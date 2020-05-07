{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Zug.Objekt (Objekt, ObjektAllgemein(..), ObjektKlasse(..), ObjektElement(..)) where

import Data.Kind (Type)

import Zug.Anbindung
       (Bahngeschwindigkeit(), Streckenabschnitt(), Weiche(), Kupplung(), Kontakt(), Wegstrecke())
import Zug.Enums (Zugtyp(), ZugtypEither(), GeschwindigkeitVariante(), GeschwindigkeitEither())
import {-# SOURCE #-} Zug.Plan (Plan)

data ObjektAllgemein bg st we ku ko ws pl
    = OBahngeschwindigkeit (ZugtypEither (GeschwindigkeitEither bg))
    | OStreckenabschnitt st
    | OWeiche (ZugtypEither we)
    | OKupplung ku
    | OKontakt ko
    | OWegstrecke (ZugtypEither ws)
    | OPlan pl

instance Eq (ObjektAllgemein bg st we ku ko ws pl)

class ObjektKlasse o where
    type BG o :: GeschwindigkeitVariante -> Zugtyp -> Type

    type ST o :: Type

    type WE o :: Zugtyp -> Type

    type KU o :: Type

    type KO o :: Type

    type WS o :: Zugtyp -> Type

    type PL o :: Type

    type SP o :: Type

    erhalteObjekt :: o -> ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (KO o) (WS o) (PL o)

    ausObjekt :: ObjektAllgemein (BG o) (ST o) (WE o) (KU o) (KO o) (WS o) (PL o) -> o

class ObjektElement e where
    type ObjektTyp e :: Type
    zuObjektTyp :: e -> ObjektTyp e
    zuObjekt :: e -> Objekt

type Objekt =
    ObjektAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Kontakt Wegstrecke Plan