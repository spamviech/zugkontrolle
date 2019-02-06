{-# LANGUAGE InstanceSigs #-}

{-|
Description : Datentypen, welche bestimmte Eigenschaften (z.B. Richtung einer Weiche) repräsentieren.
-}
module Zug.Klassen where

-- Bibliotheken
import Data.List.NonEmpty (NonEmpty(..), fromList)
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language

data Zugtyp = Undefiniert | Märklin | Lego
                deriving (Eq)

unterstützteZugtypen :: NonEmpty Zugtyp
unterstützteZugtypen = Märklin:|Lego:[]

data Richtung = Gerade | Kurve | Links | Rechts
                    deriving (Eq, Bounded, Enum)

unterstützteRichtungen :: NonEmpty Richtung
unterstützteRichtungen = fromList [minBound..maxBound]

data Fahrtrichtung = Vorwärts | Rückwärts
                        deriving (Eq, Bounded, Enum)

unterstützteFahrtrichtungen :: NonEmpty Fahrtrichtung
unterstützteFahrtrichtungen = fromList [minBound..maxBound]

-- Instanz-Deklarationen
instance Show Zugtyp where
    show :: Zugtyp -> String
    show Undefiniert    = Language.undefiniert
    show Märklin        = Language.märklin
    show Lego           = Language.lego

instance Show Richtung where
    show :: Richtung -> String
    show Gerade = Language.gerade
    show Kurve  = Language.kurve
    show Links  = Language.links
    show Rechts = Language.rechts

instance Show Fahrtrichtung where
    show :: Fahrtrichtung -> String
    show Vorwärts   = Language.vorwärts
    show Rückwärts  = Language.rückwärts