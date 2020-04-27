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
  , Pin(..)
  , PCF8574Port(..)
  , PCF8574(..)
  , PCF8574Variant(..)
  , pcf8574Gruppieren
  , pcf8574MultiPortWrite
    -- * Typ-Klasse
  , AnschlussKlasse(..)
  , Value(..)
  , alleValues
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
  , warteAufÄnderung
  , IntEdge(..)
  , EventBehalten(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Concurrent (forkIO, ThreadId())
import Control.Concurrent.STM
       (atomically, retry, newTVarIO, readTVar, writeTVar, TMVar, takeTMVar, putTMVar)
import Control.Monad (void, forM, foldM)
import Control.Monad.Reader (MonadReader(..), asks, ReaderT, runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Data.Bits (testBit)
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Hardware.WiringPi (Pin(..), Value(..), Mode(..), digitalWrite, digitalRead
                               , pinToBcmGpio, pinMode, IntEdge(..), wiringPiISR)
import Text.Read (Read(..), ReadPrec, readListPrecDefault)

import Zug.Anbindung.Anschluss.PCF8574
       (PCF8574Port(..), PCF8574(..), PCF8574Variant(..), pcf8574PortWrite, pcf8574Read
      , pcf8574PortRead, I2CMap, i2cMapEmpty, MitI2CMap(..), I2CReader(..), pcf8574Gruppieren
      , pcf8574MultiPortWrite, BitValue(..), emptyBitValue, fullBitValue)
import Zug.Language (Anzeige(..), Sprache(), showText)

-- | Alle Möglichen Werte von 'Value'.
alleValues :: NonEmpty Value
alleValues = NonEmpty.fromList [minBound .. maxBound]

-- | Alle unterstützten Anschlussmöglichkeiten
data Anschluss
    = AnschlussPin { pin :: Pin }
    | AnschlussPCF8574Port { pcf8574Port :: PCF8574Port }
    deriving (Eq, Show, Ord)

instance Anzeige Anschluss where
    anzeige :: Anschluss -> Sprache -> Text
    anzeige AnschlussPin {pin} = const $ showText pin
    anzeige AnschlussPCF8574Port {pcf8574Port} = anzeige pcf8574Port

instance Read Anschluss where
    readPrec :: ReadPrec Anschluss
    readPrec = (AnschlussPin <$> readPrec) <|> (AnschlussPCF8574Port <$> readPrec)

    readListPrec :: ReadPrec [Anschluss]
    readListPrec = readListPrecDefault

-- | Klasse für 'Anschluss'-Typen.
class AnschlussKlasse a where
    -- | Konvertiere in einen 'Anschluss'.
    zuAnschluss :: a -> Anschluss

    -- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'Pin'
    zuPin :: a -> Maybe Pin

    -- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'Num'.
    -- Der Wert entspricht der GPIO-Nummerierung. Invalide Werte werden auf 0 normiert.
    -- 'Nothing' wird dementsprechend nur bei einer anderen Anschlussart zurückgegeben.
    zuPinGpio :: (Num n) => a -> Maybe n
    zuPinGpio (zuPin -> Just pin) = Just $ case pinToBcmGpio pin of
        (Just gpio) -> fromIntegral gpio
        Nothing -> 0
    zuPinGpio _anschluss = Nothing

    -- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'PCF8574Port'.
    zuPCF8574Port :: a -> Maybe PCF8574Port

    -- | Schreibe einen 'Value' in einen Anschlussmöglichkeit.
    anschlussWrite :: (I2CReader r m, MonadIO m) => a -> Value -> m ()

    -- | Lese einen 'Value' aus einem 'Anschluss'.
    anschlussRead :: (I2CReader r m, MonadIO m) => a -> m Value

instance AnschlussKlasse Anschluss where
    zuAnschluss :: Anschluss -> Anschluss
    zuAnschluss = id

    zuPin :: Anschluss -> Maybe Pin
    zuPin AnschlussPin {pin} = Just pin
    zuPin _anschluss = Nothing

    zuPinGpio :: (Num n) => Anschluss -> Maybe n
    zuPinGpio (AnschlussPin pin) = Just $ case pinToBcmGpio pin of
        (Just gpio) -> fromIntegral gpio
        Nothing -> 0
    zuPinGpio _anschluss = Nothing

    zuPCF8574Port :: Anschluss -> Maybe PCF8574Port
    zuPCF8574Port AnschlussPCF8574Port {pcf8574Port} = Just pcf8574Port
    zuPCF8574Port _anschluss = Nothing

    anschlussWrite :: (I2CReader r m, MonadIO m) => Anschluss -> Value -> m ()
    anschlussWrite AnschlussPin {pin} = liftIO . (pinMode pin OUTPUT >>) . digitalWrite pin
    anschlussWrite AnschlussPCF8574Port {pcf8574Port} = pcf8574PortWrite pcf8574Port

    anschlussRead :: (I2CReader r m, MonadIO m) => Anschluss -> m Value
    anschlussRead AnschlussPin {pin} = liftIO $ pinMode pin INPUT >> digitalRead pin
    anschlussRead AnschlussPCF8574Port {pcf8574Port} = pcf8574PortRead pcf8574Port

instance AnschlussKlasse Pin where
    zuAnschluss :: Pin -> Anschluss
    zuAnschluss = AnschlussPin

    zuPin :: Pin -> Maybe Pin
    zuPin = Just

    zuPinGpio :: (Num n) => Pin -> Maybe n
    zuPinGpio pin = Just $ case pinToBcmGpio pin of
        (Just gpio) -> fromIntegral gpio
        Nothing -> 0

    zuPCF8574Port :: Pin -> Maybe PCF8574Port
    zuPCF8574Port = const Nothing

    anschlussWrite :: (I2CReader r m, MonadIO m) => Pin -> Value -> m ()
    anschlussWrite pin = liftIO . (pinMode pin OUTPUT >>) . digitalWrite pin

    anschlussRead :: (I2CReader r m, MonadIO m) => Pin -> m Value
    anschlussRead pin = liftIO $ pinMode pin INPUT >> digitalRead pin

instance AnschlussKlasse PCF8574Port where
    zuAnschluss :: PCF8574Port -> Anschluss
    zuAnschluss = AnschlussPCF8574Port

    zuPin :: PCF8574Port -> Maybe Pin
    zuPin = const Nothing

    zuPinGpio :: (Num n) => PCF8574Port -> Maybe n
    zuPinGpio = const Nothing

    zuPCF8574Port :: PCF8574Port -> Maybe PCF8574Port
    zuPCF8574Port = Just

    anschlussWrite :: (I2CReader r m, MonadIO m) => PCF8574Port -> Value -> m ()
    anschlussWrite = pcf8574PortWrite

    anschlussRead :: (I2CReader r m, MonadIO m) => PCF8574Port -> m Value
    anschlussRead = pcf8574PortRead

-- | Erhalte den 'Pin', welche eine Änderung der eingehenden Spannung angibt.
anschlussInterruptPin :: Anschluss -> Maybe Pin
anschlussInterruptPin AnschlussPin {pin} = Just pin
anschlussInterruptPin
    AnschlussPCF8574Port {pcf8574Port = PCF8574Port {pcf8574 = PCF8574 {interruptPin}}} =
    interruptPin

-- | Soll ein Event mehrfach ausgeführt werden?
data EventBehalten
    = EventBehalten
    | EventLöschen
    deriving (Show, Eq, Ord)

type InterruptMap = Map Pin ([(BitValue, BitValue) -> IO EventBehalten], BitValue)

-- | Leere 'InterruptMap'.
interruptMapEmpty :: InterruptMap
interruptMapEmpty = Map.empty

-- | Klasse für Typen mit der aktuellen 'InterruptMap'.
class MitInterruptMap r where
    interruptMap :: r -> TMVar InterruptMap

-- | Abkürzung für Funktionen, die die aktuelle 'I2CMap' benötigen
class (MonadReader r m, MitInterruptMap r) => InterruptReader r m | m -> r where
    -- | Erhalte die aktuelle 'I2CMap' aus der Umgebung.
    erhalteInterruptMap :: m (TMVar InterruptMap)
    erhalteInterruptMap = asks interruptMap

    -- | 'forkIO' in die 'InterruptReader'-Monade geliftet; Die aktuellen Umgebung soll übergeben werden.
    forkInterruptReader :: (MonadIO m) => ReaderT r IO () -> m ThreadId
    forkInterruptReader action = do
        reader <- ask
        liftIO $ forkIO $ void $ runReaderT action reader

instance (MonadReader r m, MitInterruptMap r) => InterruptReader r m

-- | Registriere ein Event für einen 'Anschluss'.
-- Wenn das Ergebnis der Aktion 'EventBehalten' ist wird sie beim nächsten Event erneut ausgeführt,
-- ansonsten ('EventLöschen') wird sie nur einmal ausgeführt.
--
-- Diese Funktion hat nur für Anschlüsse mit 'interruptPin' einen Effekt.
beiÄnderung :: (InterruptReader r m, I2CReader r m, MonadIO m)
             => Anschluss
             -> IntEdge
             -> IO EventBehalten
             -> m ()
beiÄnderung anschluss@(anschlussInterruptPin -> Just pin) intEdge aktion = do
    reader <- ask
    tmvarInterruptMap <- erhalteInterruptMap
    interruptMap <- liftIO $ atomically $ takeTMVar tmvarInterruptMap
    case Map.lookup pin interruptMap of
        (Just (aktionen, alterWert)) -> liftIO
            $ atomically
            $ putTMVar tmvarInterruptMap
            $ Map.insert
                pin
                (beiRichtigemBitValue anschluss intEdge aktion : aktionen, alterWert)
                interruptMap
        Nothing -> do
            wert <- anschlussReadBitValue anschluss
            liftIO $ do
                wiringPiISR pin (verwendeteIntEdge anschluss)
                    $ runReaderT aktionenAusführen reader
                atomically
                    $ putTMVar tmvarInterruptMap
                    $ Map.insert
                        pin
                        ([beiRichtigemBitValue anschluss intEdge aktion], wert)
                        interruptMap
    where
        verwendeteIntEdge :: Anschluss -> IntEdge
        verwendeteIntEdge AnschlussPin {} = INT_EDGE_BOTH
        verwendeteIntEdge AnschlussPCF8574Port {} = INT_EDGE_FALLING

        beiRichtigemBitValue
            :: Anschluss -> IntEdge -> IO EventBehalten -> (BitValue, BitValue) -> IO EventBehalten
        beiRichtigemBitValue AnschlussPin {} INT_EDGE_BOTH aktion _werte = aktion
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_BOTH
            aktion
            (wert, alterWert)
            | testBit wert port == testBit alterWert port = pure EventBehalten
            | otherwise = aktion
        beiRichtigemBitValue
            AnschlussPin {}
            INT_EDGE_FALLING
            aktion
            (fromBitValue -> wert, fromBitValue -> alterWert)
            | alterWert > wert = aktion
            | otherwise = pure EventBehalten
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_FALLING
            aktion
            (wert, alterWert)
            | testBit alterWert port && not (testBit wert port) = aktion
            | otherwise = pure EventBehalten
        beiRichtigemBitValue
            AnschlussPin {}
            INT_EDGE_RISING
            aktion
            (fromBitValue -> wert, fromBitValue -> alterWert)
            | alterWert < wert = aktion
            | otherwise = pure EventBehalten
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_RISING
            aktion
            (wert, alterWert)
            | not (testBit alterWert port) && testBit wert port = aktion
            | otherwise = pure EventBehalten
        beiRichtigemBitValue _anschluss INT_EDGE_SETUP _aktion _werte = pure EventBehalten

        anschlussReadBitValue :: (I2CReader r m, MonadIO m) => Anschluss -> m BitValue
        anschlussReadBitValue AnschlussPin {pin} = liftIO $ digitalRead pin >>= pure . \case
            LOW -> emptyBitValue
            HIGH -> fullBitValue
        anschlussReadBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {pcf8574}} = pcf8574Read pcf8574

        aktionenAusführen :: (InterruptReader r m, I2CReader r m, MonadIO m) => m ()
        aktionenAusführen = do
            tmvarInterruptMap <- erhalteInterruptMap
            wert <- anschlussReadBitValue anschluss
            liftIO $ do
                interruptMap <- atomically $ takeTMVar tmvarInterruptMap
                eintragAnpassen <- case Map.lookup pin interruptMap of
                    (Just (aktionen, alterWert)) -> do
                        verbliebeneAktionen
                            <- foldM (aktionAusführen (wert, alterWert)) [] aktionen
                        pure $ Map.insert pin (verbliebeneAktionen, wert)
                    Nothing -> pure id
                atomically $ putTMVar tmvarInterruptMap $ eintragAnpassen interruptMap

        aktionAusführen :: (BitValue, BitValue)
                         -> [(BitValue, BitValue)
                            -> IO EventBehalten]
                         -> ((BitValue, BitValue) -> IO EventBehalten)
                         -> IO [(BitValue, BitValue)
                               -> IO EventBehalten]
        aktionAusführen bitValues acc aktion = aktion bitValues >>= \case
            EventBehalten -> pure $ aktion : acc
            EventLöschen -> pure acc
beiÄnderung _anschluss _intEdge _aktion = pure ()

-- | Wurde ein Signal bei einem 'Kontakt' registriert.
data SignalErhalten
    = WarteAufSignal
    | SignalErhalten
    deriving (Show, Eq, Ord)

-- | Blockiere den aktuellen Thread, bis die spezifizierte 'IntEdge' bei einem der
-- übergebenen Anschlüsse auftritt.
-- Wird eine leere Liste übergeben wird der aktuelle Thread nicht blockiert.
--
-- Alle Einschränkungen von 'beiÄnderung' treffen auch auf diese Funktion zu.
warteAufÄnderung
    :: (InterruptReader r m, I2CReader r m, MonadIO m) => [(Anschluss, IntEdge)] -> m ()
warteAufÄnderung [] = pure ()
warteAufÄnderung anschlüsseIntEdge = do
    listeTVarSignal <- forM anschlüsseIntEdge $ \(anschluss, intEdge) -> do
        tvarSignal <- liftIO $ newTVarIO WarteAufSignal
        beiÄnderung anschluss intEdge $ do
            atomically $ writeTVar tvarSignal SignalErhalten
            pure EventLöschen
        pure tvarSignal
    liftIO $ atomically $ do
        listeSignalErhalten <- mapM readTVar listeTVarSignal
        if elem SignalErhalten listeSignalErhalten
            then pure ()
            else retry