{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , zugtyp
    -- * GeschwindigkeitVariante
  , GeschwindigkeitVariante(..)
  , GeschwindigkeitEither(..)
  , GeschwindigkeitKlasse(..)
  , GeschwindigkeitPhantom(..)
  , mapGeschwindigkeitEither
  , ausGeschwindigkeitEither
  , unterstützteGeschwindigkeitVarianten
  , catPwm
  , catKonstanteSpannung
  , geschwindigkeitVariante
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

import Control.Applicative (Alternative(..))
import Data.Aeson.Types as Aeson
import qualified Data.Foldable as Foldable
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Text (Text)

import qualified Zug.JSONStrings as JS
import Zug.Language (Anzeige(..), Sprache())
import qualified Zug.Language as Language

-- | Zugtyp eines Elements
data Zugtyp
    = Märklin
    | Lego
    deriving (Eq, Show, Bounded, Enum, Ord)

instance Anzeige Zugtyp where
    -- | Anzeigen eines 'Zugtyp'
    anzeige :: Zugtyp -> Sprache -> Text
    anzeige Märklin = Language.märklin
    anzeige Lego = Language.lego

-- | 'Either'-Like Datentyp für 'Zugtyp'-Abhängige Datentypen
data ZugtypEither (a :: Zugtyp -> Type)
    = ZugtypMärklin (a 'Märklin)
    | ZugtypLego (a 'Lego)

deriving instance (Eq (a 'Märklin), Eq (a 'Lego)) => Eq (ZugtypEither a)

deriving instance (Ord (a 'Märklin), Ord (a 'Lego)) => Ord (ZugtypEither a)

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

instance (Aeson.FromJSON (a 'Märklin), Aeson.FromJSON (a 'Lego))
    => Aeson.FromJSON (ZugtypEither a) where
    parseJSON :: Aeson.Value -> Aeson.Parser (ZugtypEither a)
    parseJSON value =
        (ZugtypMärklin <$> Aeson.parseJSON value) <|> (ZugtypLego <$> Aeson.parseJSON value)

instance (Aeson.ToJSON (a 'Märklin), Aeson.ToJSON (a 'Lego)) => Aeson.ToJSON (ZugtypEither a) where
    toJSON :: ZugtypEither a -> Aeson.Value
    toJSON (ZugtypMärklin a) = Aeson.toJSON a
    toJSON (ZugtypLego a) = Aeson.toJSON a

-- | Führe eine 'Zugtyp'-generische Funktion auf einem 'ZugtypEither' aus
mapZugtypEither :: (forall (z :: Zugtyp). a z -> b z) -> ZugtypEither a -> ZugtypEither b
mapZugtypEither f (ZugtypMärklin a) = ZugtypMärklin $ f a
mapZugtypEither f (ZugtypLego a) = ZugtypLego $ f a

-- | Erhalte das Ergebnis einer 'Zugtyp'-generischen Funktion aus einem 'ZugtypEither'
ausZugtypEither :: (forall (z :: Zugtyp). a z -> b) -> ZugtypEither a -> b
ausZugtypEither f (ZugtypMärklin a) = f a
ausZugtypEither f (ZugtypLego a) = f a

-- | Erhalte den assoziierten 'Zugtyp' als Wert.
zugtyp :: ZugtypEither a -> Zugtyp
zugtyp (ZugtypMärklin _a) = Märklin
zugtyp (ZugtypLego _a) = Lego

-- | Unterstützte 'Zugtyp'en
unterstützteZugtypen :: NonEmpty Zugtyp
unterstützteZugtypen = fromList [minBound .. maxBound]

-- | Die Art, wie Geschwindigkeit eingestellt wird.
data GeschwindigkeitVariante
    = Pwm
    | KonstanteSpannung
    deriving (Show, Eq, Bounded, Enum, Ord)

instance Anzeige GeschwindigkeitVariante where
    anzeige :: GeschwindigkeitVariante -> Sprache -> Text
    anzeige Pwm = Language.geschwindigkeitPwm
    anzeige KonstanteSpannung = Language.geschwindigkeitKonstanteSpannung

-- | 'Either'-Like Datentyp für 'GeschwindigkeitVariante'-Abhängige Datentypen
data GeschwindigkeitEither bg (z :: Zugtyp)
    = GeschwindigkeitPwm (bg 'Pwm z)
    | GeschwindigkeitKonstanteSpannung (bg 'KonstanteSpannung z)

deriving instance (Eq (bg 'Pwm z), Eq (bg 'KonstanteSpannung z)) => Eq (GeschwindigkeitEither bg z)

deriving instance (Ord (bg 'Pwm z), Ord (bg 'KonstanteSpannung z))
    => Ord (GeschwindigkeitEither bg z)

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
class GeschwindigkeitKlasse (g :: GeschwindigkeitVariante) where
    vonGeschwindigkeitEither :: GeschwindigkeitEither a z -> Maybe (a g z)
    zuGeschwindigkeitEither :: a g z -> GeschwindigkeitEither a z

instance GeschwindigkeitKlasse 'Pwm where
    vonGeschwindigkeitEither :: GeschwindigkeitEither a z -> Maybe (a 'Pwm z)
    vonGeschwindigkeitEither (GeschwindigkeitPwm a) = Just a
    vonGeschwindigkeitEither _zugtypEither = Nothing

    zuGeschwindigkeitEither :: a 'Pwm z -> GeschwindigkeitEither a z
    zuGeschwindigkeitEither = GeschwindigkeitPwm

instance GeschwindigkeitKlasse 'KonstanteSpannung where
    vonGeschwindigkeitEither :: GeschwindigkeitEither a z -> Maybe (a 'KonstanteSpannung z)
    vonGeschwindigkeitEither (GeschwindigkeitKonstanteSpannung a) = Just a
    vonGeschwindigkeitEither _zugtypEither = Nothing

    zuGeschwindigkeitEither :: a 'KonstanteSpannung z -> GeschwindigkeitEither a z
    zuGeschwindigkeitEither = GeschwindigkeitKonstanteSpannung

-- | Sammle alle 'PWM'-Typen
catPwm :: (Foldable t) => t (GeschwindigkeitEither bg z) -> [bg 'Pwm z]
catPwm ls = [x | GeschwindigkeitPwm x <- Foldable.toList ls]

-- | Sammle alle 'KonstanteSpannung'-Typen
catKonstanteSpannung :: (Foldable t) => t (GeschwindigkeitEither bg z) -> [bg 'KonstanteSpannung z]
catKonstanteSpannung ls = [x | GeschwindigkeitKonstanteSpannung x <- Foldable.toList ls]

-- | Erhalte die assoziierte 'GeschwindigkeitVariante' als Wert.
geschwindigkeitVariante :: GeschwindigkeitEither a z -> GeschwindigkeitVariante
geschwindigkeitVariante (GeschwindigkeitPwm _a) = Pwm
geschwindigkeitVariante (GeschwindigkeitKonstanteSpannung _a) = KonstanteSpannung

instance (Aeson.FromJSON (bg 'Pwm z), Aeson.FromJSON (bg 'KonstanteSpannung z))
    => Aeson.FromJSON (GeschwindigkeitEither bg z) where
    parseJSON :: Aeson.Value -> Aeson.Parser (GeschwindigkeitEither bg z)
    parseJSON value =
        (GeschwindigkeitPwm <$> Aeson.parseJSON value)
        <|> (GeschwindigkeitKonstanteSpannung <$> Aeson.parseJSON value)

instance (Aeson.ToJSON (bg 'Pwm z), Aeson.ToJSON (bg 'KonstanteSpannung z))
    => Aeson.ToJSON (GeschwindigkeitEither bg z) where
    toJSON :: GeschwindigkeitEither bg z -> Aeson.Value
    toJSON (GeschwindigkeitPwm bg) = Aeson.toJSON bg
    toJSON (GeschwindigkeitKonstanteSpannung bg) = Aeson.toJSON bg

-- | newtype-Wrapper um einen 'GeschwindigkeitVariante'-Phantomtyp hinzuzufügen.
--
-- Wird benötigt, um eine 'BahngeschwindigkeitKlasse'-Instanz von 'Wegstrecke' zu erstellen.
newtype GeschwindigkeitPhantom a (g :: GeschwindigkeitVariante) (z :: Zugtyp) =
    GeschwindigkeitPhantom (a z)
    deriving (Eq, Ord)

instance (Show (a z)) => Show (GeschwindigkeitPhantom a g z) where
    show :: GeschwindigkeitPhantom a g z -> String
    show (GeschwindigkeitPhantom a) = show a

instance (Anzeige (a z)) => Anzeige (GeschwindigkeitPhantom a g z) where
    anzeige :: GeschwindigkeitPhantom a g z -> Sprache -> Text
    anzeige (GeschwindigkeitPhantom a) = anzeige a

-- | Alle 'GeschwindigkeitVariante'n.
unterstützteGeschwindigkeitVarianten :: NonEmpty GeschwindigkeitVariante
unterstützteGeschwindigkeitVarianten = fromList [minBound .. maxBound]

-- | Richtung einer 'Weiche'
data Richtung
    = Gerade
    | Kurve
    | Links
    | Rechts
    deriving (Eq, Show, Bounded, Enum, Ord)

-- | Alle 'Richtung'en.
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
    deriving (Eq, Show, Bounded, Enum, Ord)

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
    deriving (Eq, Show, Bounded, Enum, Ord)

instance Anzeige Strom where
    -- | Anzeigen von 'Strom'
    anzeige :: Strom -> Sprache -> Text
    anzeige Fließend = Language.fließend
    anzeige Gesperrt = Language.gesperrt

-- | Alle Einstellmöglichkeiten eines Stroms
unterstützteStromeinstellungen :: NonEmpty Strom
unterstützteStromeinstellungen = fromList [minBound .. maxBound]

instance FromJSON Richtung where
    parseJSON :: Value -> Parser Richtung
    parseJSON = JS.findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON Richtung where
    toJSON :: Richtung -> Value
    toJSON Links = String JS.links
    toJSON Rechts = String JS.rechts
    toJSON Gerade = String JS.gerade
    toJSON Kurve = String JS.kurve

instance FromJSON Zugtyp where
    parseJSON :: Value -> Parser Zugtyp
    parseJSON v = JS.findeÜbereinstimmendenWert [minBound .. maxBound] v

instance ToJSON Zugtyp where
    toJSON :: Zugtyp -> Value
    toJSON Märklin = String JS.märklin
    toJSON Lego = String JS.lego

instance FromJSON Fahrtrichtung where
    parseJSON :: Value -> Parser Fahrtrichtung
    parseJSON = JS.findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON Fahrtrichtung where
    toJSON :: Fahrtrichtung -> Value
    toJSON Vorwärts = String JS.vorwärts
    toJSON Rückwärts = String JS.rückwärts

instance FromJSON Strom where
    parseJSON :: Value -> Parser Strom
    parseJSON = JS.findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON Strom where
    toJSON :: Strom -> Value
    toJSON Fließend = String JS.fließend
    toJSON Gesperrt = String JS.gesperrt
