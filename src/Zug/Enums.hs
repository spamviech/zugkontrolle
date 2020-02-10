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
  ( Zugtyp(..)
  , ZugtypEither(..)
  , ZugtypKlasse(..)
  , mapZugtypEither
  , ausZugtypEither
  , unterstützteZugtypen
  , Richtung(..)
  , unterstützteRichtungen
  , Fahrtrichtung(..)
  , unterstützteFahrtrichtungen
  , Strom(..)
  , unterstützteStromeinstellungen
  ) where

-- Bibliotheken
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Text (Text)

-- Abhängigkeiten von anderen Modulen
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
mapZugtypEither :: (forall z. (ZugtypKlasse z) => a z -> b z) -> ZugtypEither a -> ZugtypEither b
mapZugtypEither f (ZugtypMärklin a) = ZugtypMärklin $ f a
mapZugtypEither f (ZugtypLego a) = ZugtypLego $ f a

-- | Erhalte das Ergebnis einer 'Zugtyp'-generischen Funktion aus einem 'ZugtypEither'
ausZugtypEither :: (forall z. a z -> b) -> ZugtypEither a -> b
ausZugtypEither f (ZugtypMärklin a) = f a
ausZugtypEither f (ZugtypLego a) = f a

-- | Unterstützte 'Zugtyp'en
unterstützteZugtypen :: NonEmpty Zugtyp
unterstützteZugtypen = fromList [minBound .. maxBound]

     -- | Anzeigen eines 'Zugtyp'
instance Anzeige Zugtyp where
    anzeige :: Zugtyp -> Sprache -> Text
    anzeige Märklin = Language.märklin
    anzeige Lego = Language.lego

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

     -- | Anzeigen einer 'Richtung'
instance Anzeige Richtung where
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

     -- | Anzeigen einer 'Fahrtrichtung'
instance Anzeige Fahrtrichtung where
    anzeige :: Fahrtrichtung -> Sprache -> Text
    anzeige Vorwärts = Language.vorwärts
    anzeige Rückwärts = Language.rückwärts

-- | Zustand des einzustellenden Stroms
data Strom
    = Fließend
    | Gesperrt
    deriving (Eq, Show, Bounded, Enum)

     -- | Anzeigen von 'Strom'
instance Anzeige Strom where
    anzeige :: Strom -> Sprache -> Text
    anzeige Fließend = Language.fließend
    anzeige Gesperrt = Language.gesperrt

-- | Alle Einstellmöglichkeiten eines Stroms
unterstützteStromeinstellungen :: NonEmpty Strom
unterstützteStromeinstellungen = fromList [minBound .. maxBound]