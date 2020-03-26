{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description: Stellt einen Summentyp mit allen unterstützten Anschlussmöglichkeiten zur Verfügung.
-}
module Zug.Anbindung.Anschluss
  ( -- * Anschluss-Datentyp
    Anschluss(..)
  , PCF8574Port(..)
  , PCF8574(..)
  , PCF8574Variant(..)
  , vonPin
  , zuPin
  , zuPinGpio
  , vonPinGpio
  , vonPCF8574Port
  , zuPCF8574Port
    -- * Schreibe/Lese-Aktionen
  , Value(..)
  , anschlussWrite
  , anschlussRead
  , I2CMap
  , i2cMapEmpty
  , MitI2CMap(..)
  , I2CReader(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import System.Hardware.WiringPi
       (Pin(..), Value(..), Mode(..), digitalWrite, digitalRead, pinToBcmGpio, pinMode)
import Text.Read (Read(..), ReadPrec, readListPrecDefault)

import Zug.Anbindung.PCF8574 (PCF8574Port(..), PCF8574(..), PCF8574Variant(..), pcf8574PortWrite
                            , pcf8574PortRead, I2CMap, i2cMapEmpty, MitI2CMap(..), I2CReader(..))
import Zug.Language (Anzeige(..), Sprache(), showText)

-- | Alle unterstützten Anschlussmöglichkeiten
data Anschluss
    = AnschlussPin Pin
    | AnschlussPCF8574Port PCF8574Port
    deriving (Eq, Show, Ord)

instance Anzeige Anschluss where
    anzeige :: Anschluss -> Sprache -> Text
    anzeige (AnschlussPin pin) = const $ showText pin
    anzeige (AnschlussPCF8574Port port) = anzeige port

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
zuPin (AnschlussPin pin) = Just pin
zuPin _anschluss = Nothing

-- | Konvertiere einen 'Integral' in einen 'AnschlussPin'
vonPinGpio :: (Integral n) => n -> Anschluss
vonPinGpio = vonPin . Gpio . fromIntegral

-- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'Num'.
-- Der Wert entspricht der GPIO-Nummerierung. Invalide Werte werden auf 0 normiert.
-- Nothing wird dementsprechend nur bei einer anderen Anschlussart zurückgegeben.
zuPinGpio :: (Num n) => Anschluss -> Maybe n
zuPinGpio (AnschlussPin pin) = Just $ case pinToBcmGpio pin of
    (Just gpio) -> fromIntegral gpio
    Nothing -> 0
zuPinGpio _anschluss = Nothing

-- | Konvertiere einen 'PCF8574Port' in einen 'Anschluss'.
vonPCF8574Port :: PCF8574Port -> Anschluss
vonPCF8574Port = AnschlussPCF8574Port

-- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'PCF8574Port'.
zuPCF8574Port :: Anschluss -> Maybe PCF8574Port
zuPCF8574Port (AnschlussPCF8574Port port) = Just port
zuPCF8574Port _anschluss = Nothing

-- | Schreibe einen 'Value' in einen Anschlussmöglichkeit
anschlussWrite :: (I2CReader r m, MonadIO m) => Anschluss -> Value -> m ()
anschlussWrite (AnschlussPin pin) = liftIO . (pinMode pin OUTPUT >>) . digitalWrite pin
anschlussWrite (AnschlussPCF8574Port port) = pcf8574PortWrite port

-- | Lese einen 'Value' aus einem 'Anschluss'
anschlussRead :: (I2CReader r m, MonadIO m) => Anschluss -> m Value
anschlussRead (AnschlussPin pin) = liftIO $ pinMode pin INPUT >> digitalRead pin
anschlussRead (AnschlussPCF8574Port port) = pcf8574PortRead port