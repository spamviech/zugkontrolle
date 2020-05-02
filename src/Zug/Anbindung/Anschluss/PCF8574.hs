{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description : Funktionen zur Verwendung eines PCF8574 über die I2C-Schnittstelle
-}
module Zug.Anbindung.Anschluss.PCF8574
  ( -- * Map über aktuelle I2C-Kanäle
    I2CMap
  , i2cMapEmpty
  , MitI2CMap(..)
  , I2CReader(..)
    -- * I2CAddresse des PCF8574
  , PCF8574(..)
  , PCF8574Variant(..)
  , Value(..)
  , MitInterruptPin(..)
  , PCF8574Klasse(..)
    -- * Read-/Write-Aktionen
  , pcf8574Write
  , pcf8574Read
  , BitValue(..)
  , emptyBitValue
  , fullBitValue
  , toBitValue
    -- ** Auf bestimmten Port spezialisierte Funktionen
  , PCF8574Port(..)
  , pcf8574PortWrite
  , pcf8574MultiPortWrite
  , pcf8574Gruppieren
  , pcf8574PortRead
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..))
import Data.Bits (bit, (.|.), (.&.), testBit, complement)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word8)
import System.Hardware.WiringPi (Value(..), Pin(Gpio), pinToBcmGpio)
import Text.ParserCombinators.ReadP (ReadP())
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (Read(..), ReadPrec, lexP)
import Text.Read.Lex (numberToInteger, Lexeme(..))

import Zug.Anbindung.Anschluss.I2C
       (I2CMap, i2cMapEmpty, MitI2CMap(..), I2CReader(..), I2CAddress(..), i2cWrite, i2cWriteAdjust
      , i2cRead, BitValue(..), emptyBitValue, fullBitValue)
import Zug.Language (Anzeige(..), Sprache())
import qualified Zug.Language as Language

{-
-- | Alle Möglichkeiten, die Adresse eines PCF8574 einzustellen
addressMöglichkeiten :: [(Value, Value, Value)]
addressMöglichkeiten = (,,) <$> [minBound..maxBound] <*> [minBound..maxBound] <*> [minBound..maxBound]
-}
-- | kleinste 'I2CAddress' eines /PCF8574/.
minI2CAddress :: PCF8574Variant -> I2CAddress
minI2CAddress VariantNormal = I2CAddress $ bit 5
minI2CAddress VariantA = I2CAddress $ bit 5 .|. bit 4 .|. bit 3

-- | Berechne die 'I2CAddress' eines /PCF8574/ anhand der variablen Address-Bits.
toI2CAddress :: PCF8574 i -> I2CAddress
toI2CAddress = toI2CAddressAux . ohneInterruptPin
    where
        toI2CAddressAux :: PCF8574 'OhneInterruptPin -> I2CAddress
        toI2CAddressAux PCF8574 {variant, a0, a1, a2} =
            I2CAddress
            $ addFirstIfHigh a2 (bit 2)
            $ addFirstIfHigh a1 (bit 1)
            $ addFirstIfHigh a0 (bit 0)
            $ fromI2CAddress
            $ minI2CAddress variant

        addFirstIfHigh :: Value -> Word8 -> Word8 -> Word8
        addFirstIfHigh HIGH a b = a + b
        addFirstIfHigh LOW _a b = b

-- | Variante des /PCF8574/ (beeinflusst 'I2CAddress').
data PCF8574Variant
    = VariantNormal
    | VariantA
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance Anzeige PCF8574Variant where
    anzeige :: PCF8574Variant -> Sprache -> Text
    anzeige VariantNormal = Language.normal
    anzeige VariantA = Language.a

-- | Besitzt ein 'Anschluss' einen InterruptPin?
data MitInterruptPin
    = MitInterruptPin
    | OhneInterruptPin
    deriving (Show, Eq)

-- | Variante und variable Adress-Bits eines /PCF8574/.
data PCF8574 (i :: MitInterruptPin) where
    PCF8574 :: { variant :: PCF8574Variant, a0, a1, a2 :: Value } -> PCF8574 'OhneInterruptPin
    PCF8574InterruptPin
        :: { iVariant :: PCF8574Variant, iA0, iA1, iA2 :: Value, interruptPin :: Pin }
        -> PCF8574 'MitInterruptPin

class PCF8574Klasse p where
    ohneInterruptPin :: p i -> p 'OhneInterruptPin

instance PCF8574Klasse PCF8574 where
    ohneInterruptPin :: PCF8574 i -> PCF8574 'OhneInterruptPin
    ohneInterruptPin p@PCF8574 {} = p
    ohneInterruptPin PCF8574InterruptPin {iVariant, iA0, iA1, iA2} = PCF8574 iVariant iA0 iA1 iA2

deriving instance Eq (PCF8574 i)

deriving instance Ord (PCF8574 i)

instance Show (PCF8574 i) where
    show :: PCF8574 i -> String
    show PCF8574 {variant, a0, a1, a2} =
        "PCF8574"
        ++ (if variant == VariantA
                then "A"
                else "")
        ++ ['-', showAddress a0, showAddress a1, showAddress a2]
        where
            showAddress :: Value -> Char
            showAddress HIGH = 'H'
            showAddress LOW = 'L'
    show pcf8574@PCF8574InterruptPin {interruptPin} =
        show (ohneInterruptPin pcf8574)
        ++ '-' : 'I' : show (fromMaybe 0 $ pinToBcmGpio interruptPin)

instance Anzeige (PCF8574 i)

instance Read (PCF8574 'OhneInterruptPin) where
    readPrec :: ReadPrec (PCF8574 'OhneInterruptPin)
    readPrec = do
        variant <- ReadPrec.lift $ do
            parsePCF8574
            variant <- ReadP.option VariantNormal parseA
            ReadP.char '-'
            pure variant
        PCF8574 variant <$> parseValue <*> parseValue <*> parseValue

instance Read (PCF8574 'MitInterruptPin) where
    readPrec :: ReadPrec (PCF8574 'MitInterruptPin)
    readPrec = do
        variant <- ReadPrec.lift $ do
            parsePCF8574
            variant <- ReadP.option VariantNormal parseA
            ReadP.char '-'
            pure variant
        PCF8574InterruptPin variant <$> parseValue
            <*> parseValue
            <*> parseValue
            <*> (ReadPrec.lift (ReadP.char 'I' <|> ReadP.char 'i') *> (Gpio <$> readPrec))

parsePCF8574 :: ReadP ()
parsePCF8574 = void $ do
    ReadP.char 'P' <|> ReadP.char 'p'
    ReadP.char 'C' <|> ReadP.char 'c'
    ReadP.char 'F' <|> ReadP.char 'F'
    ReadP.char '8'
    ReadP.char '5'
    ReadP.char '7'
    ReadP.char '4'

parseA :: ReadP PCF8574Variant
parseA = ReadP.get >>= \case
    'A' -> pure VariantA
    _fail -> ReadP.pfail

parseValue :: ReadPrec Value
parseValue = ReadPrec.get >>= \case
    'H' -> pure HIGH
    'L' -> pure LOW
    _fail -> ReadPrec.pfail

-- | Umwandeln des Anschluss-Ports in den entsprechenden 'BitValue'.
toBitValue :: Word8 -> BitValue
toBitValue port = BitValue $ bit $ fromIntegral $ min port $ pred numPorts

-- | Wert eines /PCF8574/ komplett setzten.
pcf8574Write :: (I2CReader r m, MonadIO m) => PCF8574 i -> BitValue -> m ()
pcf8574Write pcf8574 = i2cWrite $ toI2CAddress pcf8574

-- | Wert eines /PCF8574/ auslesen.
pcf8574Read :: (I2CReader r m, MonadIO m) => PCF8574 i -> m BitValue
pcf8574Read pcf8574 = i2cRead $ toI2CAddress pcf8574

-- | Anzahl IO-Ports eines /PCF8574/.
numPorts :: Word8
numPorts = 8

-- | Verwende Port eines /PCF8574/.
data PCF8574Port (i :: MitInterruptPin) = PCF8574Port { pcf8574 :: PCF8574 i, port :: Word8 }
    deriving (Eq, Ord)

instance Show (PCF8574Port i) where
    show :: PCF8574Port i -> String
    show PCF8574Port {pcf8574, port} = show pcf8574 ++ ('-' : show port)

instance Anzeige (PCF8574Port i)

instance (Read (PCF8574 i)) => Read (PCF8574Port i) where
    readPrec :: ReadPrec (PCF8574Port i)
    readPrec = PCF8574Port <$> readPrec <*> parsePort
        where
            parsePort :: ReadPrec Word8
            parsePort = do
                ReadPrec.lift $ ReadP.char '-'
                (Number number) <- lexP
                case numberToInteger number >>= Just . fromInteger of
                    (Just n)
                        | (n >= 0) || (n < numPorts) -> pure n
                    _fail -> ReadPrec.pfail

instance PCF8574Klasse PCF8574Port where
    ohneInterruptPin :: PCF8574Port i -> PCF8574Port 'OhneInterruptPin
    ohneInterruptPin p@PCF8574Port {pcf8574} = p { pcf8574 = ohneInterruptPin pcf8574 }

-- | Wert einzelner Ports eines /PCF8574/ setzen.
pcf8574PortWrite :: (I2CReader r m, MonadIO m) => PCF8574Port i -> Value -> m ()
pcf8574PortWrite PCF8574Port {pcf8574, port} value =
    i2cWriteAdjust (toI2CAddress pcf8574) bitValueFunktion
    where
        bitValueFunktion :: BitValue -> BitValue
        bitValueFunktion oldBitValue
            | value == HIGH = oldBitValue .|. toBitValue port
            | otherwise = oldBitValue .&. complement (toBitValue port)

-- | Wert mehrerer Ports eines /PCF8574/ setzen.
pcf8574MultiPortWrite :: (I2CReader r m, MonadIO m) => PCF8574 i -> [Word8] -> Value -> m ()
pcf8574MultiPortWrite pcf8574 ports value = i2cWriteAdjust (toI2CAddress pcf8574) bitValueFunktion
    where
        bitValueFunktion :: BitValue -> BitValue
        bitValueFunktion oldBitValue = foldl (portFunktion value) oldBitValue ports

        portFunktion :: Value -> BitValue -> Word8 -> BitValue
        portFunktion HIGH bitValue port = bitValue .|. toBitValue port
        portFunktion LOW bitValue port = bitValue .&. complement (toBitValue port)

-- | Sortiere eine Liste von 'PCF8574Port's nach ihren 'PCF8574'.
pcf8574Gruppieren :: [PCF8574Port i] -> Map (PCF8574 i) [Word8]
pcf8574Gruppieren = foldl (\portsMap PCF8574Port {pcf8574, port}
                           -> Map.insertWith (++) pcf8574 [port] portsMap) Map.empty

-- | Wert eines einzelnen Ports eines /PCF8574/ auslesen.
pcf8574PortRead :: (I2CReader r m, MonadIO m) => (PCF8574Port i) -> m Value
pcf8574PortRead PCF8574Port {pcf8574, port} = do
    fullBitValue <- pcf8574Read pcf8574
    pure
        $ if testBit fullBitValue $ fromIntegral port
            then HIGH
            else LOW