{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

{-|
Description : Datentypen, welche bestimmte Eigenschaften (z.B. Richtung einer Weiche) repräsentieren.
-}
module Zug.Enums
  ( -- * Zugtyp
    Zugtyp(..)
  , ZugtypEither(..)
  , ZugtypKlasse(..)
  , mapZugtypEither
  , ausZugtypEither
  , unterstützteZugtypen
    -- * GeschwindigkeitVariante
  , GeschwindigkeitVariante(..)
  , GeschwindigkeitEither(..)
  , GeschwindigkeitEitherKlasse(..)
  , GeschwindigkeitPhantom(..)
  , mapGeschwindigkeitEither
  , ausGeschwindigkeitEither
  , unterstützteGeschwindigkeitVarianten
  , catPwm
  , catKonstanteSpannung
    -- * Richtung
  , Richtung(..)
  , unterstützteRichtungen
    -- ** Fahrtrichtung
  , Fahrtrichtung(..)
  , unterstützteFahrtrichtungen
    -- * Strom
  , Strom(..)
  , unterstützteStromeinstellungen
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Text (Text)

import Zug.Language (Anzeige(..), Sprache())
import qualified Zug.Language as Language

-- | Zugtyp eines Elements
data Zugtyp
    = Märklin
    | Lego
    deriving (Eq, Show, Bounded, Enum)

-- | 'Either'-Like Datentyp für 'Zugtyp'-Abhängige Datentypen
data ZugtypEither (a :: Zugtyp -> Type)
    = ZugtypMärklin (a 'Märklin)
    | ZugtypLego (a 'Lego)

deriving instance (Eq (a 'Märklin), Eq (a 'Lego)) => Eq (ZugtypEither a)

deriving instance (Show (a 'Märklin), Show (a 'Lego)) => Show (ZugtypEither a)

instance (Anzeige (a 'Märklin), Anzeige (a 'Lego)) => Anzeige (ZugtypEither a) where
    anzeige :: ZugtypEither a -> Sprache -> Text
    anzeige (ZugtypMärklin a) = anzeige a
    anzeige (ZugtypLego a) = anzeige a

-- | Klasse zur Extraktion aus 'ZugtypEither'
class ZugtypKlasse (z :: Zugtyp) where
    vonZugtypEither :: ZugtypEither a -> Maybe (a z)
    zuZugtypEither :: a z -> ZugtypEither a

instance ZugtypKlasse 'Märklin where
    vonZugtypEither :: ZugtypEither a -> Maybe (a 'Märklin)
    vonZugtypEither (ZugtypMärklin a) = Just a
    vonZugtypEither _zugtypEither = Nothing

    zuZugtypEither :: a 'Märklin -> ZugtypEither a
    zuZugtypEither = ZugtypMärklin

instance ZugtypKlasse 'Lego where
    vonZugtypEither :: ZugtypEither a -> Maybe (a 'Lego)
    vonZugtypEither (ZugtypLego a) = Just a
    vonZugtypEither _zugtypEither = Nothing

    zuZugtypEither :: a 'Lego -> ZugtypEither a
    zuZugtypEither = ZugtypLego

-- | Führe eine 'Zugtyp'-generische Funktion auf einem 'ZugtypEither' aus
mapZugtypEither :: (forall (z :: Zugtyp). a z -> b z) -> ZugtypEither a -> ZugtypEither b
mapZugtypEither f (ZugtypMärklin a) = ZugtypMärklin $ f a
mapZugtypEither f (ZugtypLego a) = ZugtypLego $ f a

-- | Erhalte das Ergebnis einer 'Zugtyp'-generischen Funktion aus einem 'ZugtypEither'
ausZugtypEither :: (forall (z :: Zugtyp). a z -> b) -> ZugtypEither a -> b
ausZugtypEither f (ZugtypMärklin a) = f a
ausZugtypEither f (ZugtypLego a) = f a

-- | Unterstützte 'Zugtyp'en
unterstützteZugtypen :: NonEmpty Zugtyp
unterstützteZugtypen = fromList [minBound .. maxBound]

instance Anzeige Zugtyp where
    -- | Anzeigen eines 'Zugtyp'
    anzeige :: Zugtyp -> Sprache -> Text
    anzeige Märklin = Language.märklin
    anzeige Lego = Language.lego

-- | Die Art, wie Geschwindigkeit eingestellt wird.
data GeschwindigkeitVariante
    = Pwm
    | KonstanteSpannung
    deriving (Show, Eq, Bounded, Enum)

data GeschwindigkeitEither bg (z :: Zugtyp)
    = GeschwindigkeitPwm (bg 'Pwm z)
    | GeschwindigkeitKonstanteSpannung (bg 'KonstanteSpannung z)

deriving instance (Eq (bg 'Pwm z), Eq (bg 'KonstanteSpannung z)) => Eq (GeschwindigkeitEither bg z)

deriving instance (Show (bg 'Pwm z), Show (bg 'KonstanteSpannung z))
    => Show (GeschwindigkeitEither bg z)

instance (Anzeige (bg 'Pwm z), Anzeige (bg 'KonstanteSpannung z))
    => Anzeige (GeschwindigkeitEither bg z) where
    anzeige :: GeschwindigkeitEither bg z -> Sprache -> Text
    anzeige (GeschwindigkeitPwm bg) = anzeige bg
    anzeige (GeschwindigkeitKonstanteSpannung bg) = anzeige bg

-- | Führe eine 'GeschwindigkeitVariante'-generische Funktion auf einem 'GeschwindigkeitEither' aus.
mapGeschwindigkeitEither :: (forall (g :: GeschwindigkeitVariante). a g z -> b g z)
                         -> GeschwindigkeitEither a z
                         -> GeschwindigkeitEither b z
mapGeschwindigkeitEither f (GeschwindigkeitPwm a) = GeschwindigkeitPwm $ f a
mapGeschwindigkeitEither f (GeschwindigkeitKonstanteSpannung a) =
    GeschwindigkeitKonstanteSpannung $ f a

-- | Erhalte das Ergebnis einer 'GeschwindigkeitVariante'-generischen Funktion aus einem 'GeschwindigkeitEither'.
ausGeschwindigkeitEither
    :: (forall (g :: GeschwindigkeitVariante). a g z -> b) -> GeschwindigkeitEither a z -> b
ausGeschwindigkeitEither f (GeschwindigkeitPwm a) = f a
ausGeschwindigkeitEither f (GeschwindigkeitKonstanteSpannung a) = f a

-- | Klasse zur Extraktion aus 'GeschwindigkeitEither'
class GeschwindigkeitEitherKlasse (g :: GeschwindigkeitVariante) where
    vonGeschwindigkeitEither :: GeschwindigkeitEither a z -> Maybe (a g z)
    zuGeschwindigkeitEither :: a g z -> GeschwindigkeitEither a z

instance GeschwindigkeitEitherKlasse 'Pwm where
    vonGeschwindigkeitEither :: GeschwindigkeitEither a z -> Maybe (a 'Pwm z)
    vonGeschwindigkeitEither (GeschwindigkeitPwm a) = Just a
    vonGeschwindigkeitEither _zugtypEither = Nothing

    zuGeschwindigkeitEither :: a 'Pwm z -> GeschwindigkeitEither a z
    zuGeschwindigkeitEither = GeschwindigkeitPwm

instance GeschwindigkeitEitherKlasse 'KonstanteSpannung where
    vonGeschwindigkeitEither :: GeschwindigkeitEither a z -> Maybe (a 'KonstanteSpannung z)
    vonGeschwindigkeitEither (GeschwindigkeitKonstanteSpannung a) = Just a
    vonGeschwindigkeitEither _zugtypEither = Nothing

    zuGeschwindigkeitEither :: a 'KonstanteSpannung z -> GeschwindigkeitEither a z
    zuGeschwindigkeitEither = GeschwindigkeitKonstanteSpannung

-- | Sammle alle 'PWM'-Typen
catPwm :: [GeschwindigkeitEither bg z] -> [bg 'Pwm z]
catPwm ls = [x | GeschwindigkeitPwm x <- ls]

-- | Sammle alle 'KonstanteSpannung'-Typen
catKonstanteSpannung :: [GeschwindigkeitEither bg z] -> [bg 'KonstanteSpannung z]
catKonstanteSpannung ls = [x | GeschwindigkeitKonstanteSpannung x <- ls]

-- | newtype-Wrapper um einen 'GeschwindigkeitVariante'-Phantomtyp hinzuzufügen.
--
-- Wird benötigt, um eine 'BahngeschwindigkeitKlasse'-Instanz von 'Wegstrecke' zu erstellen.
newtype GeschwindigkeitPhantom a (g :: GeschwindigkeitVariante) (z :: Zugtyp) =
    GeschwindigkeitPhantom (a z)

instance (Show (a z)) => Show (GeschwindigkeitPhantom a g z) where
    show :: GeschwindigkeitPhantom a g z -> String
    show (GeschwindigkeitPhantom a) = show a

instance (Eq (a z)) => Eq (GeschwindigkeitPhantom a g z) where
    (==) :: GeschwindigkeitPhantom a g z -> GeschwindigkeitPhantom a g z -> Bool
    (==) (GeschwindigkeitPhantom a) (GeschwindigkeitPhantom b) = a == b

instance (Anzeige (a z)) => Anzeige (GeschwindigkeitPhantom a g z) where
    anzeige :: GeschwindigkeitPhantom a g z -> Sprache -> Text
    anzeige (GeschwindigkeitPhantom a) = anzeige a

instance Anzeige GeschwindigkeitVariante where
    anzeige :: GeschwindigkeitVariante -> Sprache -> Text
    anzeige Pwm = Language.geschwindigkeitPwm
    anzeige KonstanteSpannung = Language.geschwindigkeitKonstanteSpannung

unterstützteGeschwindigkeitVarianten :: NonEmpty GeschwindigkeitVariante
unterstützteGeschwindigkeitVarianten = fromList [minBound .. maxBound]

-- | Richtung einer 'Weiche'
data Richtung
    = Gerade
    | Kurve
    | Links
    | Rechts
    deriving (Eq, Show, Bounded, Enum)

-- | Alle 'Richtung'en
unterstützteRichtungen :: NonEmpty Richtung
unterstützteRichtungen = fromList [minBound .. maxBound]

instance Anzeige Richtung where
    -- | Anzeigen einer 'Richtung'
    anzeige :: Richtung -> Sprache -> Text
    anzeige Gerade = Language.gerade
    anzeige Kurve = Language.kurve
    anzeige Links = Language.links
    anzeige Rechts = Language.rechts

-- | Fahrtrichtung auf einer Schiene
data Fahrtrichtung
    = Vorwärts
    | Rückwärts
    deriving (Eq, Show, Bounded, Enum)

-- | Alle 'Fahrtrichtung'en
unterstützteFahrtrichtungen :: NonEmpty Fahrtrichtung
unterstützteFahrtrichtungen = fromList [minBound .. maxBound]

instance Anzeige Fahrtrichtung where
    -- | Anzeigen einer 'Fahrtrichtung'
    anzeige :: Fahrtrichtung -> Sprache -> Text
    anzeige Vorwärts = Language.vorwärts
    anzeige Rückwärts = Language.rückwärts

-- | Zustand des einzustellenden Stroms
data Strom
    = Fließend
    | Gesperrt
    deriving (Eq, Show, Bounded, Enum)

instance Anzeige Strom where
    -- | Anzeigen von 'Strom'
    anzeige :: Strom -> Sprache -> Text
    anzeige Fließend = Language.fließend
    anzeige Gesperrt = Language.gesperrt

-- | Alle Einstellmöglichkeiten eines Stroms
unterstützteStromeinstellungen :: NonEmpty Strom
unterstützteStromeinstellungen = fromList [minBound .. maxBound]