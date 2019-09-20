{-# LANGUAGE InstanceSigs #-}

{-|
Description: Stellt einen Summentyp mit allen unterstützten Anschlussmöglichkeiten zur Verfügung.
-}
module Zug.Anbindung.Anschluss (
    -- * Anschluss-Datentyp
    Anschluss(..), vonPin, zuPin, zuPinGpio, vonPinGpio, vonPCF8574Port, zuPCF8574Port,
    -- * Schreibe/Lese-Aktionen
    Value(..), anschlussWrite, anschlussRead, I2CMap, I2CMapT, i2cMapEmpty, runI2CMapT, forkI2CMapT) where

-- Bibliotheken
import Control.Applicative (Alternative(..))
import Control.Monad.Trans (MonadIO(..))
import System.Hardware.WiringPi (Pin(..), Value(..), Mode(..), digitalWrite, digitalRead, pinToBcmGpio, pinMode)
import Text.Read (Read(..), ReadPrec, readListPrecDefault)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung.PCF8574 (PCF8574Port(..), pcf8574PortWrite, pcf8574PortRead, I2CMap, I2CMapT, i2cMapEmpty, runI2CMapT, forkI2CMapT)

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

-- | Konvertiere einen 'Pin' in einen 'Anschluss'
vonPin :: Pin -> Anschluss
vonPin = AnschlussPin

-- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'Pin'
zuPin :: Anschluss -> Maybe Pin
zuPin   (AnschlussPin pin)  = Just pin
zuPin   _anschluss          = Nothing

-- | Konvertiere einen 'Integral' in einen 'AnschlussPin'
vonPinGpio :: (Integral n) => n -> Anschluss
vonPinGpio = vonPin . Gpio . fromIntegral

-- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'Num'. Der Wert entspricht der GPIO-Nummerirung.
zuPinGpio :: (Num n) => Anschluss -> Maybe n
zuPinGpio anschluss = do
    pin <- zuPin anschluss
    gpio <- pinToBcmGpio pin
    pure $ fromIntegral gpio

-- | Konvertiere einen 'PCF8574Port' in einen 'Anschluss'.
vonPCF8574Port :: PCF8574Port -> Anschluss
vonPCF8574Port = AnschlussPCF8574Port

-- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'PCF8574Port'.
zuPCF8574Port :: Anschluss -> Maybe PCF8574Port
zuPCF8574Port   (AnschlussPCF8574Port port) = Just port
zuPCF8574Port   _anschluss                  = Nothing

-- | Schreibe einen 'Value' in einen Anschlussmöglichkeit
anschlussWrite :: Anschluss -> Value -> I2CMapT IO ()
anschlussWrite (AnschlussPin pin)           = liftIO . (pinMode pin OUTPUT >>) . digitalWrite pin
anschlussWrite (AnschlussPCF8574Port port)  = pcf8574PortWrite port

-- | Lese einen 'Value' aus einem 'Anschluss'
anschlussRead :: Anschluss -> I2CMapT IO Value
anschlussRead   (AnschlussPin pin)          = liftIO $ pinMode pin INPUT >> digitalRead pin
anschlussRead   (AnschlussPCF8574Port port) = pcf8574PortRead port