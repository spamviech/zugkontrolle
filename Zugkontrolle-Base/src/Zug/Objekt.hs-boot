{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Zug.Objekt (Objekt, ObjektAllgemein(..), ObjektKlasse(..), ObjektElement(..)) where

import Data.Kind (Type)

import Zug.Anbindung
       (Bahngeschwindigkeit(), Streckenabschnitt(), Weiche(), Kupplung(), Kontakt(), Wegstrecke())
import Zug.Enums (Zugtyp(..), ZugtypEither(), GeschwindigkeitVariante(..), GeschwindigkeitEither())
import {-# SOURCE #-} Zug.Plan (Plan)

data ObjektAllgemein bg st we ku ko ws pl
    = OBahngeschwindigkeit (ZugtypEither (GeschwindigkeitEither bg))
    | OStreckenabschnitt st
    | OWeiche (ZugtypEither we)
    | OKupplung ku
    | OKontakt ko
    | OWegstrecke (ZugtypEither ws)
    | OPlan pl

instance ( Eq st
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

    type ObjektTyp e = e
    zuObjektTyp :: e -> ObjektTyp e
    default zuObjektTyp :: (e ~ ObjektTyp e) => e -> ObjektTyp e
    zuObjektTyp = id

    zuObjekt :: e -> Objekt
    default zuObjekt :: (ObjektElement (ObjektTyp e)) => e -> Objekt
    zuObjekt = zuObjekt . zuObjektTyp

    {-# MINIMAL zuObjektTyp | zuObjekt #-}

type Objekt =
    ObjektAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Kontakt Wegstrecke Plan
