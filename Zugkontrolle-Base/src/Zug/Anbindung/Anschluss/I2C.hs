{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
#ifdef ZUGKONTROLLERASPI
{-# LANGUAGE ForeignFunctionInterface #-}
#endif

{-|
Description : Funktionen zur Verwendung der I2C-Schnittstelle
-}
module Zug.Anbindung.Anschluss.I2C
  ( -- * Map über aktuelle I2C-Kanäle
    I2CMap
  , I2CChannelStatus()
  , i2cMapEmpty
  , MitI2CMap(..)
  , I2CReader(..)
    -- * Read-/Write-Aktionen
  , i2cWrite
  , i2cWriteAdjust
  , i2cRead
  , I2CAddress(..)
  , BitValue(..)
  , emptyBitValue
  , fullBitValue
  , FileHandle()
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (atomically, retry, TVar, readTVar, writeTVar, modifyTVar)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT, asks)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Bits (Bits, complement, zeroBits)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))

import Zug.Util (forkIOSilent)

-- | Status eines I2C-Kanals (verhindern von Race-Conditions).
data I2CChannelStatus
    = I2CSetupInProgress
    | I2CChannelInUse
    | I2CChannelReady FileHandle BitValue

-- | 'FileHandle' und aktuell gesetzter 'BitValue' eines I2C-Kanals
type I2CMap = Map I2CAddress I2CChannelStatus

-- | Leere 'I2CMap'
i2cMapEmpty :: I2CMap
i2cMapEmpty = Map.empty

-- | Klasse für Typen mit der aktuellen 'I2CMap'
class MitI2CMap r where
    i2cMap :: r -> TVar I2CMap

-- | Abkürzung für Funktionen, die die aktuelle 'I2CMap' benötigen
class (MonadReader r m, MitI2CMap r) => I2CReader r m | m -> r where
    -- | Erhalte die aktuelle 'I2CMap' aus der Umgebung.
    erhalteI2CMap :: m (TVar I2CMap)
    erhalteI2CMap = asks i2cMap

    -- | 'forkIO' in die 'I2CReader'-Monade geliftet; Die aktuellen Umgebung soll übergeben werden.
    forkI2CReader :: (MonadIO m) => ReaderT r IO () -> m ThreadId
    forkI2CReader action = liftIO . forkIOSilent . void . runReaderT action =<< ask

instance (MonadReader r m, MitI2CMap r) => I2CReader r m

-- | Stelle sicher, dass eine 'I2CAddress' registriert ist und gebe ihre aktuellen Werte zurück.
i2cKanalLookup :: (I2CReader r m, MonadIO m) => I2CAddress -> m (FileHandle, BitValue)
i2cKanalLookup i2cAddress = do
    tvarI2CKanäle <- erhalteI2CMap
    liftIO $ do
        maybeI2CKanal <- atomically $ do
            i2cKanäle <- readTVar tvarI2CKanäle
            case Map.lookup i2cAddress i2cKanäle of
                (Just I2CSetupInProgress) -> retry
                (Just I2CChannelInUse) -> retry
                (Just (I2CChannelReady fileHandle bitValue)) -> pure $ Just (fileHandle, bitValue)
                Nothing -> do
                    writeTVar tvarI2CKanäle $ Map.insert i2cAddress I2CSetupInProgress i2cKanäle
                    pure Nothing
        case maybeI2CKanal of
            Nothing -> do
                fileHandle <- c_wiringPiI2CSetup $ fromIntegral $ fromI2CAddress i2cAddress
                atomically
                    $ modifyTVar tvarI2CKanäle
                    $ Map.insert i2cAddress
                    $ I2CChannelReady (FileHandle fileHandle) fullBitValue
                pure ((FileHandle fileHandle), fullBitValue)
            (Just kanal) -> pure kanal

-- | I2C-Adresse eines /PCF8574/
newtype I2CAddress = I2CAddress { fromI2CAddress :: Word8 }
    deriving (Eq, Ord)

instance Show I2CAddress where
    show :: I2CAddress -> String
    show = show . fromI2CAddress

-- | Ein-/Ausgabe von 'i2cWrite'/'i2cRead'
newtype BitValue = BitValue { fromBitValue :: Word8 }
    deriving (Eq, Bits)

instance Show BitValue where
    show :: BitValue -> String
    show = show . fromBitValue

-- | 'BitValue' with no Bit set.
emptyBitValue :: BitValue
emptyBitValue = zeroBits

-- | 'BitValue' with every Bit set.
fullBitValue :: BitValue
fullBitValue = complement zeroBits

-- | Schreibe einen 'BitValue' in einen I2C-Kanal
i2cWrite :: (I2CReader r m, MonadIO m) => I2CAddress -> BitValue -> m ()
i2cWrite i2cAddress bitValue = do
    tvarI2CKanäle <- erhalteI2CMap
    liftIO $ do
        maybeI2CKanal <- atomically $ do
            i2cKanäle <- readTVar tvarI2CKanäle
            case Map.lookup i2cAddress i2cKanäle of
                (Just I2CSetupInProgress) -> retry
                (Just I2CChannelInUse) -> retry
                (Just (I2CChannelReady fileHandle aktuellerBitValue)) -> do
                    modifyTVar tvarI2CKanäle $ Map.insert i2cAddress I2CChannelInUse
                    pure $ Just (fileHandle, aktuellerBitValue)
                Nothing -> do
                    modifyTVar tvarI2CKanäle $ Map.insert i2cAddress I2CSetupInProgress
                    pure Nothing
        fileHandle <- case maybeI2CKanal of
            Nothing -> do
                fileHandle <- c_wiringPiI2CSetup $ fromIntegral $ fromI2CAddress i2cAddress
                atomically $ modifyTVar tvarI2CKanäle $ Map.insert i2cAddress $ I2CChannelInUse
                pure (FileHandle fileHandle)
            (Just (fileHandle, _bitValue)) -> pure fileHandle
        c_wiringPiI2CWrite (fromFileHandle fileHandle) $ fromIntegral $ fromBitValue bitValue
        atomically
            $ modifyTVar tvarI2CKanäle
            $ Map.insert i2cAddress
            $ I2CChannelReady fileHandle bitValue

-- | Ändere den geschriebenen 'BitValue' in einem I2C-Kanal.
-- Die aktuelle Ausgabe wird über der übergebenen Funktion angepasst und neu gesetzt.
i2cWriteAdjust :: (I2CReader r m, MonadIO m) => I2CAddress -> (BitValue -> BitValue) -> m ()
i2cWriteAdjust i2cAddress bitValueFunktion = do
    tvarI2CKanäle <- erhalteI2CMap
    liftIO $ do
        maybeI2CKanal <- atomically $ do
            i2cKanäle <- readTVar tvarI2CKanäle
            case Map.lookup i2cAddress i2cKanäle of
                (Just I2CSetupInProgress) -> retry
                (Just I2CChannelInUse) -> retry
                (Just (I2CChannelReady fileHandle oldBitValue)) -> do
                    modifyTVar tvarI2CKanäle $ Map.insert i2cAddress $ I2CChannelInUse
                    pure $ Just (fileHandle, oldBitValue)
                Nothing -> do
                    writeTVar tvarI2CKanäle $ Map.insert i2cAddress I2CSetupInProgress i2cKanäle
                    pure Nothing
        (fileHandle, newBitValue) <- case maybeI2CKanal of
            Nothing -> do
                fileHandle <- c_wiringPiI2CSetup $ fromIntegral $ fromI2CAddress i2cAddress
                atomically $ modifyTVar tvarI2CKanäle $ Map.insert i2cAddress $ I2CChannelInUse
                pure (FileHandle fileHandle, bitValueFunktion fullBitValue)
            (Just (fileHandle, oldBitValue)) -> pure (fileHandle, bitValueFunktion oldBitValue)
        c_wiringPiI2CWrite (fromFileHandle fileHandle) $ fromIntegral $ fromBitValue newBitValue
        atomically
            $ modifyTVar tvarI2CKanäle
            $ Map.insert i2cAddress
            $ I2CChannelReady fileHandle newBitValue

-- | Lese den aktuellen Wert aus einem I2C-Kanal
i2cRead :: (I2CReader r m, MonadIO m) => I2CAddress -> m BitValue
i2cRead i2cAddress = do
    (fileHandle, _setBitValue) <- i2cKanalLookup i2cAddress
    liftIO $ do
        c_result <- c_wiringPiI2CRead (fromFileHandle fileHandle)
        pure $ BitValue $ fromIntegral c_result

-- | File Handle eines I2C-Channel
newtype FileHandle = FileHandle { fromFileHandle :: CInt }

#ifdef ZUGKONTROLLERASPI
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
c_wiringPiI2CSetup i2cAdresse = putStrLn ("I2CSetup " ++ show i2cAdresse) >> pure (-1)

c_wiringPiI2CRead :: CInt -> IO CInt
c_wiringPiI2CRead fileHandle = putStrLn ("I2CRead " ++ show fileHandle) >> pure 0

c_wiringPiI2CWrite :: CInt -> CInt -> IO ()
c_wiringPiI2CWrite fileHandle value =
    putStrLn $ "I2CWrite " ++ show fileHandle ++ " -> " ++ show value
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
--
