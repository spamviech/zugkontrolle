{-# LANGUAGE InstanceSigs #-}

{-|
Description : Datentypen, welche bestimmte Eigenschaften (z.B. Richtung einer Weiche) repräsentieren.
-}
module Zug.Klassen where

-- Bibliotheken
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty(..), fromList)
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language

-- | Zugtyp eines Elements (sofern ein Unterschied besteht)
data Zugtyp = Undefiniert | Märklin | Lego
                deriving (Eq, Bounded, Enum)

-- | Unterstützte 'Zugtyp'en
unterstützteZugtypen :: NonEmpty Zugtyp
unterstützteZugtypen = fromList $ delete Undefiniert [minBound..maxBound]

-- | Anzeigen eines 'Zugtyp'
instance Show Zugtyp where
    show :: Zugtyp -> String
    show Undefiniert    = Language.undefiniert
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

instance Show Strom where
    show :: Strom -> String
    show    (Fließend)  = Language.fließend
    show    (Gesperrt)  = Language.gesperrt

-- | Alle Einstellmöglichkeiten eines Stroms
unterstützteStromeinstellungen :: NonEmpty Strom
unterstützteStromeinstellungen = fromList [minBound..maxBound]