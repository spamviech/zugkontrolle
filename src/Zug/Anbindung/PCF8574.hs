{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

{-|
Description : Funktionen zur Verwendung eines PCF8574 über die I2C-Schnittstelle
-}
module Zug.Anbindung.PCF8574 (
        -- * I2CAddresse des PCF8574
        PCF8574(..), PCF8574Variant(..), Value(..),
        -- * Read-/Write-Aktionen
        pcf8574Write, pcf8574Read,
        BitValue(..), toBitValue,
        -- ** Auf bestimmten Port spezailierte Funktionen
        PCF8574Port(..), pcf8574PortWrite, pcf8574PortRead) where

-- Bibliotheken
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative (Alternative(..))
import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically)
import Data.Bits
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Word
import System.Hardware.WiringPi
import Text.Read (Read(..), ReadPrec, lexP, readListPrecDefault)
import Text.Read.Lex (numberToInteger, Lexeme(..))
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.ParserCombinators.ReadP (ReadP())
import qualified Text.ParserCombinators.ReadP as ReadP

-- | 'FileHandle' und aktuell gesetzter 'BitValue' eines I2C-Kanals
tvarI2CKanäle :: TVar (Map I2CAddress (FileHandle, BitValue))
tvarI2CKanäle = unsafePerformIO $ newTVarIO Map.empty

-- | Stelle sicher, dass eine 'I2CAddress' registriert ist und gebe ihre aktuellen Werte zurück.
i2cKanalLookup :: I2CAddress -> IO (FileHandle, BitValue)
i2cKanalLookup i2cAddress = do
    i2cKanäle <- readTVarIO tvarI2CKanäle
    case Map.lookup i2cAddress i2cKanäle of
        Nothing         -> do
            fileHandle <- c_wiringPiI2CSetup $ fromIntegral $ fromI2CAddress i2cAddress
            let current = (FileHandle fileHandle, fullBitValue)
            atomically $ writeTVar tvarI2CKanäle $ Map.insert i2cAddress current i2cKanäle
            pure current
        (Just current)  -> pure current

{-
-- | Alle Möglichkeiten, die Addresse eines PCF8574 einzustellen
addressMöglichkeiten :: [(Value, Value, Value)]
addressMöglichkeiten = (,,) <$> [minBound..maxBound] <*> [minBound..maxBound] <*> [minBound..maxBound]
-}

-- | kleinste 'I2CAddress' eines /PCF8574/
minI2CAddress :: PCF8574Variant -> I2CAddress
minI2CAddress VariantNormal = I2CAddress $ bit 5
minI2CAddress VariantA      = I2CAddress $ bit 5 .|. bit 4 .|. bit 3

-- | I2C-Adresse eines /PCF8574/
newtype I2CAddress = I2CAddress {fromI2CAddress :: Word8}
                        deriving (Eq, Ord)

instance Show I2CAddress where
    show :: I2CAddress -> String
    show = show . fromI2CAddress

-- Memoisieren?
-- | Berechne die 'I2CAddress' eines /PCF8574/ anhand der variablen Address-Bits.
toI2CAddress :: PCF8574 -> I2CAddress
toI2CAddress (PCF8574 {variant, a0, a1, a2})
    = I2CAddress
        $ addFirstIfHigh a2 (bit 2)
        $ addFirstIfHigh a1 (bit 1)
        $ addFirstIfHigh a0 (bit 0)
        $ fromI2CAddress $ minI2CAddress variant
    where
        addFirstIfHigh :: Value -> Word8 -> Word8 -> Word8
        addFirstIfHigh  HIGH    a   b   = a + b
        addFirstIfHigh  LOW     _a  b   = b

-- | Variante des /PCF8574/ (beeinflusst 'I2CAddress')
data PCF8574Variant = VariantNormal | VariantA
                        deriving (Show, Read, Eq, Ord, Enum)

-- | Variante und variable Adress-Bits eines /PCF8574/
data PCF8574 = PCF8574 {variant :: PCF8574Variant, a0, a1, a2 :: Value}
                deriving (Eq)

instance Show PCF8574 where
    show :: PCF8574 -> String
    show (PCF8574 {variant, a0, a1, a2})
        = "PCF8574" ++ if (variant == VariantA) then "A" else "" ++
            showAddress a0 :
            showAddress a1 :
            showAddress a2 : []
        where
            showAddress :: Value -> Char
            showAddress HIGH    = 'H'
            showAddress LOW     = 'L'

instance Read PCF8574 where
    readPrec :: ReadPrec PCF8574
    readPrec = do
        variant <- ReadPrec.lift $ do
            ReadP.char 'P' <|> ReadP.char 'p'
            ReadP.char 'C' <|> ReadP.char 'c'
            ReadP.char 'F' <|> ReadP.char 'F'
            ReadP.char '8'
            ReadP.char '5'
            ReadP.char '7'
            ReadP.char '4'
            variant <- ReadP.option VariantNormal parseA
            ReadP.char '-'
            pure variant
        PCF8574 variant <$> parseValue <*> parseValue <*> parseValue
            where
                parseA :: ReadP PCF8574Variant
                parseA = ReadP.get >>= \case
                    'A'     -> pure VariantA
                    _fail   -> ReadP.pfail
                parseValue :: ReadPrec Value
                parseValue = ReadPrec.get >>= \case
                    'H'     -> pure HIGH
                    'L'     -> pure LOW
                    _fail   -> ReadPrec.pfail
    readListPrec :: ReadPrec [PCF8574] 
    readListPrec = readListPrecDefault

-- | Ein-/Ausgabe von 'pcf8574Write'/'pcf8574Read'
newtype BitValue = BitValue {fromBitValue::Word8}
                                deriving (Eq, Bits)

instance Show BitValue where
    show :: BitValue -> String
    show = show . fromBitValue

-- | Umwandeln des Anschluss-Ports in den entsprechenden 'BitValue'
toBitValue :: Word8 -> BitValue
toBitValue port = BitValue $ bit $ fromIntegral $ min port $ pred numPorts

-- | 'BitValue' with ever Bit set
fullBitValue :: BitValue
fullBitValue = complement zeroBits

-- | File Handle eines I2C-Channel
newtype FileHandle = FileHandle {fromFileHandle::CInt}

-- | Wert eines /PCF8574/ komplett setzten
pcf8574Write :: PCF8574 -> BitValue -> IO ()
pcf8574Write pcf8574 bitValue = do
    let i2cAddress = toI2CAddress pcf8574
    (fileHandle, _oldBitValue) <- i2cKanalLookup i2cAddress
    atomically $ modifyTVar tvarI2CKanäle $
        Map.adjust (\(fileHandle, _oldBitValue) -> (fileHandle, bitValue)) i2cAddress
    c_wiringPiI2CWrite (fromFileHandle fileHandle) (fromIntegral $ fromBitValue $ bitValue)

-- | Wert eines /PCF8574/ auslesen
pcf8574Read :: PCF8574 -> IO BitValue
pcf8574Read pcf8574 = do
    (fileHandle, _setBitValue) <- i2cKanalLookup $ toI2CAddress pcf8574
    c_wiringPiI2CRead (fromFileHandle fileHandle) >>= pure . BitValue . fromIntegral






-- | Anzahl IO-Ports eines /PCF8574/
numPorts :: Word8
numPorts = 8

-- | Verwende Port eines /PCF8574/
data PCF8574Port = PCF8574Port {pcf8574 :: PCF8574, port::Word8}
                    deriving (Eq)

instance Show PCF8574Port where
    show :: PCF8574Port -> String
    show (PCF8574Port {pcf8574, port}) = show pcf8574 ++ ('-' : show port)

instance Read PCF8574Port where
    readPrec :: ReadPrec PCF8574Port
    readPrec = PCF8574Port <$> (readPrec :: ReadPrec PCF8574) <*> parsePort
        where
            parsePort :: ReadPrec Word8
            parsePort = do
                ReadPrec.lift $ ReadP.char '-'
                (Number number) <- lexP
                case numberToInteger number >>= Just . fromInteger of
                    (Just n)    | (n >= 0) || (n < numPorts)
                        -> pure n
                    _fail
                        -> ReadPrec.pfail
    readListPrec :: ReadPrec [PCF8574Port] 
    readListPrec = readListPrecDefault

-- | Wert einzelner Ports eines /PCF8574/ setzen
pcf8574PortWrite :: PCF8574Port -> Value -> IO ()
pcf8574PortWrite (PCF8574Port {pcf8574, port}) value  = do
    let i2cAddress = toI2CAddress pcf8574
    (_fileHandle, oldBitValue) <- i2cKanalLookup i2cAddress
    let newBitValue
            | (value == HIGH)   = oldBitValue .|. toBitValue port
            | otherwise         = oldBitValue .&. complement (toBitValue port)
    pcf8574Write pcf8574 newBitValue

-- | Wert eines einzelnen Ports eines /PCF8574/ auslesen
pcf8574PortRead :: PCF8574Port -> IO Value
pcf8574PortRead (PCF8574Port {pcf8574, port}) = do
    fullBitValue <- pcf8574Read pcf8574
    pure $ if (testBit fullBitValue $ fromIntegral port) then HIGH else LOW





#if linux_HOST_OS
foreign import ccall "wiringPiI2CSetup" c_wiringPiI2CSetup :: CInt -> IO CInt
foreign import ccall "wiringPiI2CRead" c_wiringPiI2CRead :: CInt -> IO CInt
foreign import ccall "wiringPiI2CWrite" c_wiringPiI2CWrite :: CInt -> CInt -> IO ()
{-
foreign import ccall "wiringPiI2CReadReg8" c_wiringPiI2CReadReg8 :: CInt -> CInt -> IO CInt
foreign import ccall "wiringPiI2CReadReg16" c_wiringPiI2CReadReg16 :: CInt -> CInt -> IO CInt
foreign import ccall "wiringPiI2CWriteReg8" c_wiringPiI2CWriteReg8 :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "wiringPiI2CWriteReg16" c_wiringPiI2CWriteReg16 :: CInt -> CInt -> CInt -> IO CInt
-}
#else
-- wiringPi-Bibliothek nicht auf Windows vorhanden -> verwende stattdessen print-Befehle zum einfacheren Debugging
c_wiringPiI2CSetup :: CInt -> IO CInt
c_wiringPiI2CSetup  = \i2cAdresse -> putStrLn ("I2CSetup " ++ show i2cAdresse) >> pure (-1)
c_wiringPiI2CRead :: CInt -> IO CInt
c_wiringPiI2CRead   = \fileHandle -> putStrLn ("I2CRead " ++ show fileHandle) >> pure 0
c_wiringPiI2CWrite :: CInt -> CInt -> IO ()
c_wiringPiI2CWrite  = \fileHandle value -> putStrLn $ "I2CWrite " ++ show fileHandle ++ " -> " ++ show value
{-
c_wiringPiI2CReadReg8 :: CInt -> CInt -> IO CInt
c_wiringPiI2CReadReg8   fileHandle register         = putStrLn ("I2CReadReg8 " ++ show fileHandle ++ " r" ++ show register) >> pure 0
c_wiringPiI2CReadReg16 :: CInt -> CInt -> IO CInt
c_wiringPiI2CReadReg16  fileHandle register         = putStrLn ("I2CReadReg16 " ++ show fileHandle ++ " r" ++ show register) >> pure 0
c_wiringPiI2CWriteReg8 :: CInt -> CInt -> CInt -> IO ()
c_wiringPiI2CWriteReg8  fileHandle register value   = putStrLn $ "I2CWriteReg8 " ++ show fileHandle ++ " r" ++ show register ++ "->" ++ show value
c_wiringPiI2CWriteReg16 :: CInt -> CInt -> CInt -> IO ()
c_wiringPiI2CWriteReg16 fileHandle register value   = putStrLn $ "I2CWriteReg16 " ++ show fileHandle ++ " r" ++ show register ++ "->" ++ show value
-}
#endif
