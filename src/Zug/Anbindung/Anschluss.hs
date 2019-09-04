{-# LANGUAGE InstanceSigs #-}

{-|
Description: Stellt einen Summentyp mit allen unterstützten Anschlussmöglichkeiten zur Verfügung.
-}
module Zug.Anbindung.Anschluss (Anschluss(..), Value(..), anschlussWrite, anschlussRead) where

-- Bibliotheken
import Control.Applicative (Alternative(..))
import System.Hardware.WiringPi (Pin(..), Value(..), digitalWrite, digitalRead)
import Text.Read (Read(..), ReadPrec, readListPrecDefault)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung.PCF8574 (PCF8574Port(..), pcf8574PortWrite, pcf8574PortRead, I2CMapIO)

-- | Alle unterstützten Anschlussmöglichkeiten
data Anschluss
    = AnschlussPin Pin
    | AnschlussPCF8574Port PCF8574Port
        deriving (Eq, Ord)

instance Show Anschluss where
    show :: Anschluss -> String
    show    (AnschlussPin pin)          = show pin
    show    (AnschlussPCF8574Port port) = show port

instance Read Anschluss where
    readPrec :: ReadPrec Anschluss
    readPrec = (AnschlussPin <$> readPrec) <|> (AnschlussPCF8574Port <$> readPrec)
    readListPrec :: ReadPrec [Anschluss] 
    readListPrec = readListPrecDefault

-- | Schreibe einen 'Value' in einen Anschlussmöglichkeit
anschlussWrite :: Anschluss -> Value -> I2CMapIO ()
anschlussWrite (AnschlussPin pin)           = \value _tvarI2CMap -> digitalWrite pin value
anschlussWrite (AnschlussPCF8574Port port)  = pcf8574PortWrite port

-- | Lese einen 'Value' aus einem 'Anschluss'
anschlussRead :: Anschluss -> I2CMapIO Value
anschlussRead   (AnschlussPin pin)          = const $ digitalRead pin
anschlussRead   (AnschlussPCF8574Port port) = pcf8574PortRead port