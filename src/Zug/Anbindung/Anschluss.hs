{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

{-|
Description: Stellt einen Summentyp mit allen unterstützten Anschlussmöglichkeiten zur Verfügung.
-}
module Zug.Anbindung.Anschluss
  ( -- * Anschluss-Datentyp
    Anschluss(..)
  , PCF8574Port(..)
  , PCF8574(..)
  , PCF8574Variant(..)
  , pcf8574Gruppieren
  , pcf8574MultiPortWrite
  , vonPin
  , zuPin
  , zuPinGpio
  , vonPinGpio
  , vonPCF8574Port
  , zuPCF8574Port
  , interruptPin
    -- * Schreibe/Lese-Aktionen
  , Value(..)
  , anschlussWrite
  , anschlussRead
  , I2CMap
  , i2cMapEmpty
  , MitI2CMap(..)
  , I2CReader(..)
    -- * Interrupt-basierte Aktionen
  , InterruptMap
  , interruptMapEmpty
  , MitInterruptMap(..)
  , InterruptReader(..)
  , beiÄnderung
  , IntEdge(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Concurrent (forkIO, ThreadId())
import Control.Concurrent.STM (TVar, readTVarIO, atomically, modifyTVar)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader(..), asks, ReaderT, runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Data.Bits (testBit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Hardware.WiringPi (Pin(..), Value(..), Mode(..), digitalWrite, digitalRead
                               , pinToBcmGpio, pinMode, IntEdge(..), wiringPiISR)
import Text.Read (Read(..), ReadPrec, readListPrecDefault)

import Zug.Anbindung.PCF8574
       (PCF8574Port(..), PCF8574(..), PCF8574Variant(..), pcf8574PortWrite, pcf8574Read
      , pcf8574PortRead, I2CMap, i2cMapEmpty, MitI2CMap(..), I2CReader(..), pcf8574Gruppieren
      , pcf8574MultiPortWrite, BitValue(..), emptyBitValue, fullBitValue)
import Zug.Language (Anzeige(..), Sprache(), showText)

-- | Alle unterstützten Anschlussmöglichkeiten
data Anschluss
    = AnschlussPin { pin :: Pin }
    | AnschlussPCF8574Port { pcf8574Port :: PCF8574Port, pcf8574InterruptPin :: Maybe Pin }
    deriving (Eq, Show, Ord)

instance Anzeige Anschluss where
    anzeige :: Anschluss -> Sprache -> Text
    anzeige AnschlussPin {pin} = const $ showText pin
    anzeige AnschlussPCF8574Port {pcf8574Port} = anzeige pcf8574Port

instance Read Anschluss where
    readPrec :: ReadPrec Anschluss
    readPrec =
        (AnschlussPin <$> readPrec)
        <|> (AnschlussPCF8574Port <$> readPrec <*> (Just <$> readPrec <|> pure Nothing))

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
-- 'Nothing' wird dementsprechend nur bei einer anderen Anschlussart zurückgegeben.
zuPinGpio :: (Num n) => Anschluss -> Maybe n
zuPinGpio (AnschlussPin pin) = Just $ case pinToBcmGpio pin of
    (Just gpio) -> fromIntegral gpio
    Nothing -> 0
zuPinGpio _anschluss = Nothing

-- | Konvertiere einen 'PCF8574Port' in einen 'Anschluss'.
vonPCF8574Port :: PCF8574Port -> Anschluss
vonPCF8574Port = flip AnschlussPCF8574Port Nothing

-- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'PCF8574Port'.
zuPCF8574Port :: Anschluss -> Maybe PCF8574Port
zuPCF8574Port AnschlussPCF8574Port {pcf8574Port} = Just pcf8574Port
zuPCF8574Port _anschluss = Nothing

-- | Schreibe einen 'Value' in einen Anschlussmöglichkeit
anschlussWrite :: (I2CReader r m, MonadIO m) => Anschluss -> Value -> m ()
anschlussWrite AnschlussPin {pin} = liftIO . (pinMode pin OUTPUT >>) . digitalWrite pin
anschlussWrite AnschlussPCF8574Port {pcf8574Port} = pcf8574PortWrite pcf8574Port

-- | Lese einen 'Value' aus einem 'Anschluss'
anschlussRead :: (I2CReader r m, MonadIO m) => Anschluss -> m Value
anschlussRead AnschlussPin {pin} = liftIO $ pinMode pin INPUT >> digitalRead pin
anschlussRead AnschlussPCF8574Port {pcf8574Port} = pcf8574PortRead pcf8574Port

-- | Erhalte den 'Pin', welche eine Änderung der eingehenden Spannung angibt.
interruptPin :: Anschluss -> Maybe Pin
interruptPin AnschlussPin {pin} = Just pin
interruptPin AnschlussPCF8574Port {pcf8574InterruptPin} = pcf8574InterruptPin

type InterruptMap = Map Pin ([(BitValue, BitValue) -> IO ()], BitValue)

-- | Leere 'InterruptMap'.
interruptMapEmpty :: InterruptMap
interruptMapEmpty = Map.empty

-- | Klasse für Typen mit der aktuellen 'InterruptMap'.
class MitInterruptMap r where
    interruptMap :: r -> TVar InterruptMap

-- | Abkürzung für Funktionen, die die aktuelle 'I2CMap' benötigen
class (MonadReader r m, MitInterruptMap r) => InterruptReader r m | m -> r where
    -- | Erhalte die aktuelle 'I2CMap' aus der Umgebung.
    erhalteInterruptMap :: m (TVar InterruptMap)
    erhalteInterruptMap = asks interruptMap

    -- | 'forkIO' in die 'InterruptReader'-Monade geliftet; Die aktuellen Umgebung soll übergeben werden.
    forkInterruptReader :: (MonadIO m) => ReaderT r IO () -> m ThreadId
    forkInterruptReader action = do
        reader <- ask
        liftIO $ forkIO $ void $ runReaderT action reader

instance (MonadReader r m, MitInterruptMap r) => InterruptReader r m

-- | Registriere ein Event für einen 'Anschluss'.
--
-- Diese Funktion hat nur für Anschlüsse mit 'interruptPin' einen Effekt.
beiÄnderung
    :: (InterruptReader r m, I2CReader r m, MonadIO m) => Anschluss -> IntEdge -> IO () -> m ()
beiÄnderung anschluss@(interruptPin -> Just pin) intEdge aktion = do
    reader <- ask
    tvarInterruptMap <- erhalteInterruptMap
    interruptMap <- liftIO $ readTVarIO tvarInterruptMap
    case Map.lookup pin interruptMap of
        (Just (aktionen, alterWert)) -> liftIO
            $ atomically
            $ modifyTVar tvarInterruptMap
            $ Map.insert pin
            $ (beiRichtigemBitValue anschluss intEdge aktion : aktionen, alterWert)
        Nothing -> do
            wert <- anschlussReadBitValue anschluss
            liftIO $ do
                wiringPiISR pin (verwendeteIntEdge anschluss)
                    $ runReaderT aktionenAusführen reader
                atomically
                    $ modifyTVar tvarInterruptMap
                    $ Map.insert pin ([beiRichtigemBitValue anschluss intEdge aktion], wert)
    where
        verwendeteIntEdge :: Anschluss -> IntEdge
        verwendeteIntEdge AnschlussPin {} = INT_EDGE_BOTH
        verwendeteIntEdge AnschlussPCF8574Port {} = INT_EDGE_FALLING

        beiRichtigemBitValue :: Anschluss -> IntEdge -> IO () -> (BitValue, BitValue) -> IO ()
        beiRichtigemBitValue AnschlussPin {} INT_EDGE_BOTH aktion _werte = aktion
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_BOTH
            aktion
            (wert, alterWert)
            | testBit wert port == testBit alterWert port = pure ()
            | otherwise = aktion
        beiRichtigemBitValue
            AnschlussPin {}
            INT_EDGE_FALLING
            aktion
            (fromBitValue -> wert, fromBitValue -> alterWert)
            | alterWert > wert = aktion
            | otherwise = pure ()
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_FALLING
            aktion
            (wert, alterWert)
            | testBit alterWert port && not (testBit wert port) = aktion
            | otherwise = pure ()
        beiRichtigemBitValue
            AnschlussPin {}
            INT_EDGE_RISING
            aktion
            (fromBitValue -> wert, fromBitValue -> alterWert)
            | alterWert < wert = aktion
            | otherwise = pure ()
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_RISING
            aktion
            (wert, alterWert)
            | not (testBit alterWert port) && testBit wert port = aktion
            | otherwise = pure ()
        beiRichtigemBitValue _anschluss INT_EDGE_SETUP _aktion _werte = pure ()

        anschlussReadBitValue :: (I2CReader r m, MonadIO m) => Anschluss -> m BitValue
        anschlussReadBitValue AnschlussPin {pin} = liftIO $ digitalRead pin >>= pure . \case
            LOW -> emptyBitValue
            HIGH -> fullBitValue
        anschlussReadBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {pcf8574}} = pcf8574Read pcf8574

        aktionenAusführen :: (InterruptReader r m, I2CReader r m, MonadIO m) => m ()
        aktionenAusführen = do
            tvarInterruptMap <- erhalteInterruptMap
            interruptMap <- liftIO $ readTVarIO tvarInterruptMap
            wert <- anschlussReadBitValue anschluss
            case Map.lookup pin interruptMap of
                (Just (aktionen, alterWert)) -> liftIO $ mapM_ ($ (wert, alterWert)) aktionen
                Nothing -> pure ()
beiÄnderung _anschluss _intEdge _aktion = pure ()