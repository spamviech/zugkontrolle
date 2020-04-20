{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description : Typ-Klassen.
-}
module Zug.Anbindung.Klassen (StreckenObjekt(..), StreckenAtom(..)) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Zug.Anbindung.Anschluss (Anschluss(), Value(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(..), GeschwindigkeitVariante(..)
                , GeschwindigkeitEither(..), GeschwindigkeitPhantom(..), Strom(..))

-- | Klasse für Typen mit Name und Anschlüssen.
class StreckenObjekt s where
    anschlüsse :: s -> Set Anschluss
    erhalteName :: s -> Text
    {-# MINIMAL anschlüsse, erhalteName #-}

instance (StreckenObjekt (a 'Märklin), StreckenObjekt (a 'Lego))
    => StreckenObjekt (ZugtypEither a) where
    anschlüsse :: ZugtypEither a -> Set Anschluss
    anschlüsse (ZugtypMärklin a) = anschlüsse a
    anschlüsse (ZugtypLego a) = anschlüsse a

    erhalteName :: ZugtypEither a -> Text
    erhalteName (ZugtypMärklin a) = erhalteName a
    erhalteName (ZugtypLego a) = erhalteName a

instance (StreckenObjekt (bg 'Pwm z), StreckenObjekt (bg 'KonstanteSpannung z))
    => StreckenObjekt (GeschwindigkeitEither bg z) where
    anschlüsse :: GeschwindigkeitEither bg z -> Set Anschluss
    anschlüsse (GeschwindigkeitPwm bg) = anschlüsse bg
    anschlüsse (GeschwindigkeitKonstanteSpannung bg) = anschlüsse bg

    erhalteName :: GeschwindigkeitEither bg z -> Text
    erhalteName (GeschwindigkeitPwm bg) = erhalteName bg
    erhalteName (GeschwindigkeitKonstanteSpannung bg) = erhalteName bg

instance (StreckenObjekt (a z)) => StreckenObjekt (GeschwindigkeitPhantom a g z) where
    anschlüsse :: GeschwindigkeitPhantom a g z -> Set Anschluss
    anschlüsse (GeschwindigkeitPhantom a) = anschlüsse a

    erhalteName :: GeschwindigkeitPhantom a g z -> Text
    erhalteName (GeschwindigkeitPhantom a) = erhalteName a

instance (StreckenObjekt a) => StreckenObjekt (Maybe a) where
    anschlüsse :: Maybe a -> Set Anschluss
    anschlüsse (Just a) = anschlüsse a
    anschlüsse Nothing = Set.empty

    erhalteName (Just a) = erhalteName a
    erhalteName Nothing = Text.empty

-- | Eine Klasse für alle Typen, die direkt mit wiringPi interagieren.
class (StreckenObjekt s) => StreckenAtom s where
    fließend :: s -> Value
    fließend = erhalteValue Fließend

    gesperrt :: s -> Value
    gesperrt s = case fließend s of
        HIGH -> LOW
        LOW -> HIGH

    erhalteValue :: Strom -> s -> Value
    erhalteValue Fließend = fließend
    erhalteValue Gesperrt = gesperrt

    {-# MINIMAL fließend | erhalteValue #-}

instance (StreckenAtom (a 'Märklin), StreckenAtom (a 'Lego)) => StreckenAtom (ZugtypEither a) where
    fließend :: ZugtypEither a -> Value
    fließend (ZugtypMärklin a) = fließend a
    fließend (ZugtypLego a) = fließend a