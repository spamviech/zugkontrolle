{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description : Datentypen, welche bestimmte Eigenschaften (z.B. Richtung einer Weiche) repräsentieren.
-}
module Zug.Klassen where

-- Bibliotheken
import Data.List.NonEmpty (NonEmpty(..), fromList)
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language

-- | Zugtyp eines Elements
data Zugtyp = Märklin | Lego
                deriving (Eq, Bounded, Enum)

-- | 'Either'-Like Datentyp für 'Zugtyp'-Abhängige Datantypen
data ZugtypEither a
    = ZugtypMärklin (a 'Märklin)
    | ZugtypLego (a 'Lego)
deriving instance (Eq (a 'Märklin), Eq (a 'Lego)) => Eq (ZugtypEither a)
instance (Show (a 'Märklin), Show (a 'Lego)) => Show (ZugtypEither a) where
    show :: ZugtypEither a -> String
    show    (ZugtypMärklin a)   = show a
    show    (ZugtypLego a)      = show a

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