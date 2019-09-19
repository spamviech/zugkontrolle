{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

{-|
Description : Funktionen zur Verwendung der I2C-Schnittstelle
-}
module Zug.Anbindung.I2C (
    -- * Map über aktuelle I2C-Kanäle
    I2CMap, I2CMapT, i2cMapEmpty, runI2CMapT, forkI2CMapT,
    -- * Read-/Write-Aktionen
    i2cWrite, i2cWriteAdjust, i2cRead, I2CAddress(..), BitValue(..)) where

import Foreign.C.Types (CInt)
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM (atomically, TVar, readTVarIO, writeTVar, modifyTVar)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO)
import Data.Bits (Bits, complement, zeroBits)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Word (Word8)

-- | 'FileHandle' und aktuell gesetzter 'BitValue' eines I2C-Kanals
type I2CMap = Map I2CAddress (FileHandle, BitValue)
-- | Abkürzung für Funktionen, die die aktuelle 'I2CMap' benötigen
type I2CMapT = ReaderT (TVar I2CMap)
-- | Leere 'I2CMap'
i2cMapEmpty :: I2CMap
i2cMapEmpty = Map.empty
-- | Führe eine 'I2CMapT'-Aktion in der angegebenen Umbebung aus.
runI2CMapT :: I2CMapT m a -> TVar I2CMap -> m a
runI2CMapT = runReaderT
-- | 'forkIO' in den 'I2CMapT'-Monaden Transformer gelifted; Die 'TVar' aus der aktuellen Umgebung wird übergeben.
forkI2CMapT :: I2CMapT IO () -> I2CMapT IO ThreadId
forkI2CMapT aktion = do
    tvarI2CMap <- ask
    liftIO $ forkIO $ flip runI2CMapT tvarI2CMap $ aktion

-- | Stelle sicher, dass eine 'I2CAddress' registriert ist und gebe ihre aktuellen Werte zurück.
i2cKanalLookup :: I2CAddress -> I2CMapT IO (FileHandle, BitValue)
i2cKanalLookup i2cAddress = do
    tvarI2CKanäle <- ask
    liftIO $ do
        i2cKanäle <- readTVarIO tvarI2CKanäle
        case Map.lookup i2cAddress i2cKanäle of
            Nothing         -> do
                fileHandle <- c_wiringPiI2CSetup $ fromIntegral $ fromI2CAddress i2cAddress
                let current = (FileHandle fileHandle, fullBitValue)
                atomically $ writeTVar tvarI2CKanäle $ Map.insert i2cAddress current i2cKanäle
                pure current
            (Just current)  -> pure current

-- | I2C-Adresse eines /PCF8574/
newtype I2CAddress = I2CAddress {fromI2CAddress :: Word8}
                        deriving (Eq, Ord)

instance Show I2CAddress where
    show :: I2CAddress -> String
    show = show . fromI2CAddress

-- | Ein-/Ausgabe von 'i2cWrite'/'i2cRead'
newtype BitValue = BitValue {fromBitValue::Word8}
                                deriving (Eq, Bits)

instance Show BitValue where
    show :: BitValue -> String
    show = show . fromBitValue

-- | 'BitValue' with ever Bit set
fullBitValue :: BitValue
fullBitValue = complement zeroBits

-- | Schreibe einen 'BitValue' in einen I2C-Kanal
i2cWrite :: I2CAddress -> BitValue -> I2CMapT IO ()
i2cWrite i2cAddress bitValue = do
    tvarI2CKanäle <- ask
    (fileHandle, _oldBitValue) <- i2cKanalLookup i2cAddress
    liftIO $ do
        atomically $ modifyTVar tvarI2CKanäle $
            Map.adjust (\(fileHandle, _oldBitValue) -> (fileHandle, bitValue)) i2cAddress
        c_wiringPiI2CWrite (fromFileHandle fileHandle) $ fromIntegral $ fromBitValue $ bitValue

-- | Ändere den geschriebenen 'BitValue' in einem I2C-Kanal.
-- Die aktuelle Ausgabe wird über der übergebenen Funktion angepasst und neu gesetzt.
i2cWriteAdjust :: I2CAddress -> (BitValue -> BitValue) -> I2CMapT IO ()
i2cWriteAdjust i2cAddress bitValueFunktion = do
    (_fileHandle, oldBitValue) <- i2cKanalLookup i2cAddress
    i2cWrite i2cAddress (bitValueFunktion oldBitValue)

-- | Lese den aktuellen Wert aus einem I2C-Kanal
i2cRead :: I2CAddress -> I2CMapT IO BitValue
i2cRead i2cAddress = do
    (fileHandle, _setBitValue) <- i2cKanalLookup i2cAddress
    liftIO $ do
        c_result <- c_wiringPiI2CRead (fromFileHandle fileHandle)
        pure $ BitValue $ fromIntegral c_result

-- | File Handle eines I2C-Channel
newtype FileHandle = FileHandle {fromFileHandle::CInt}

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