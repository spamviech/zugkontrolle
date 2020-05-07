{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Zug.Plan (PlanAllgemein, Plan) where

import Data.Kind (Type)

import Zug.Anbindung
       (Bahngeschwindigkeit(), Streckenabschnitt(), Weiche(), Kupplung(), Kontakt(), Wegstrecke())
import Zug.Enums (Zugtyp(), GeschwindigkeitVariante())

data PlanAllgemein
       (bg :: GeschwindigkeitVariante -> Zugtyp -> Type)
       (st :: Type)
       (we :: Zugtyp -> Type)
       (ku :: Type)
       (ko :: Type)
       (ws :: Zugtyp -> Type)

type Plan = PlanAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Kontakt Wegstrecke