{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

{-|
Description : Datentypen, welche bestimmte Eigenschaften (z.B. Richtung einer Weiche) repräsentieren.
-}
module Zug.Klassen where

-- Bibliotheken
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..), fromList)
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language

-- | Zugtyp eines Elements
data Zugtyp = Märklin | Lego
                deriving (Eq, Bounded, Enum)

-- | 'Either'-Like Datentyp für 'Zugtyp'-Abhängige Datantypen
data ZugtypEither (a :: Zugtyp -> Type)
    = ZugtypMärklin (a 'Märklin)
    | ZugtypLego (a 'Lego)
deriving instance (Eq (a 'Märklin), Eq (a 'Lego)) => Eq (ZugtypEither a)
instance (Show (a 'Märklin), Show (a 'Lego)) => Show (ZugtypEither a) where
    show :: ZugtypEither a -> String
    show    (ZugtypMärklin a)   = show a
    show    (ZugtypLego a)      = show a

-- | Klasse zur Extraktion aus 'ZugtypEither'
class ZugtypKlasse (z :: Zugtyp) where
    vonZugtypEither :: ZugtypEither a -> Maybe (a z)
    zuZugtypEither :: a z -> ZugtypEither a

instance ZugtypKlasse 'Märklin where
    vonZugtypEither :: ZugtypEither a -> Maybe (a 'Märklin)
    vonZugtypEither (ZugtypMärklin a)   = Just a
    vonZugtypEither _zugtypEither       = Nothing
    zuZugtypEither :: a 'Märklin -> ZugtypEither a
    zuZugtypEither = ZugtypMärklin

instance ZugtypKlasse 'Lego where
    vonZugtypEither :: ZugtypEither a -> Maybe (a 'Lego)
    vonZugtypEither (ZugtypLego a)  = Just a
    vonZugtypEither _zugtypEither   = Nothing
    zuZugtypEither :: a 'Lego -> ZugtypEither a
    zuZugtypEither = ZugtypLego

-- | Führe eine 'Zugtyp'-generische Funktion auf einem 'ZugtypEither' aus
mapZugtypEither :: (forall z. a z -> b z) -> ZugtypEither a -> ZugtypEither b
mapZugtypEither f   (ZugtypMärklin a)   = ZugtypMärklin $ f a
mapZugtypEither f   (ZugtypLego a)      = ZugtypLego $ f a

-- | Erhalte das Ergebnis einer 'Zugtyp'-generischen Funktion aus einem 'ZugtypEither'
ausZugtypEither :: (forall z. a z -> b) -> ZugtypEither a -> b
ausZugtypEither f   (ZugtypMärklin a)   = f a
ausZugtypEither f   (ZugtypLego a)      = f a

-- | Unterstützte 'Zugtyp'en
unterstützteZugtypen :: NonEmpty Zugtyp
unterstützteZugtypen = fromList $ [minBound..maxBound]

-- | Anzeigen eines 'Zugtyp'
instance Show Zugtyp where
    show :: Zugtyp -> String
    show Märklin        = Language.märklin
    show Lego           = Language.lego

-- | Richtung einer 'Weiche'
data Richtung = Gerade | Kurve | Links | Rechts
                    deriving (Eq, Bounded, Enum)

-- | Alle 'Richtung'en
unterstützteRichtungen :: NonEmpty Richtung
unterstützteRichtungen = fromList [minBound..maxBound]

-- | Anzeigen einer 'Richtung'
instance Show Richtung where
    show :: Richtung -> String
    show Gerade = Language.gerade
    show Kurve  = Language.kurve
    show Links  = Language.links
    show Rechts = Language.rechts

-- | Fahrtrichtung auf einer Schiene
data Fahrtrichtung = Vorwärts | Rückwärts
                        deriving (Eq, Bounded, Enum)

-- | Alle 'Fahrtrichtung'en
unterstützteFahrtrichtungen :: NonEmpty Fahrtrichtung
unterstützteFahrtrichtungen = fromList [minBound..maxBound]

-- | Anzeigen einer 'Fahrtrichtung'
instance Show Fahrtrichtung where
    show :: Fahrtrichtung -> String
    show Vorwärts   = Language.vorwärts
    show Rückwärts  = Language.rückwärts

-- | Zustand des einzustellenden Stroms
data Strom = Fließend | Gesperrt
                deriving (Eq, Bounded, Enum)

-- | Anzeigen von 'Strom'
instance Show Strom where
    show :: Strom -> String
    show    Fließend    = Language.fließend
    show    Gesperrt    = Language.gesperrt

-- | Alle Einstellmöglichkeiten eines Stroms
unterstützteStromeinstellungen :: NonEmpty Strom
unterstützteStromeinstellungen = fromList [minBound..maxBound]