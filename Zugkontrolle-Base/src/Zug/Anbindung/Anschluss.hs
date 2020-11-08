{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Stellt einen Summentyp mit allen unterstützten Anschlussmöglichkeiten zur Verfügung.
-}
module Zug.Anbindung.Anschluss
  ( -- * Anschluss-Datentyp
    Anschluss(..)
  , AnschlussEither(..)
  , Pin(..)
  , PCF8574Port(..)
  , PCF8574(..)
  , MitInterruptPin(..)
  , InterruptPinBenötigt(..)
  , InterruptPinKlasse()
  , PCF8574Klasse(..)
  , PCF8574Variant(..)
  , pcf8574Gruppieren
  , pcf8574MultiPortWrite
    -- ** Hilfsfunktionen
  , parseAnschlussEither
  , parseFließend
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
import Control.Concurrent (ThreadId())
import Control.Concurrent.STM
       (atomically, retry, newTVarIO, readTVar, writeTVar, TMVar, takeTMVar, putTMVar)
import Control.Monad (void, forM, unless, foldM)
import Control.Monad.Reader (MonadReader(..), asks, ReaderT, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Aeson.Types ((.:), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.Bits (testBit)
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import System.Hardware.WiringPi (Pin(..), Value(..), Mode(..), digitalWrite, digitalRead
                               , pinToBcmGpio, pinMode, IntEdge(..), wiringPiISR)
import Text.Read (Read(..), ReadPrec)

import Zug.Anbindung.Anschluss.PCF8574
       (PCF8574Port(..), PCF8574(..), PCF8574Variant(..), pcf8574PortWrite, pcf8574Read
      , pcf8574PortRead, I2CMap, i2cMapEmpty, MitI2CMap(..), I2CReader(..), pcf8574Gruppieren
      , pcf8574MultiPortWrite, BitValue(..), emptyBitValue, fullBitValue, MitInterruptPin(..)
      , PCF8574Klasse(..))
import qualified Zug.JSONStrings as JS
import Zug.Language (Anzeige(..), Sprache(), showText)
import Zug.Util (forkIOSilent, isRaspi)

-- | Wird ein Interrupt-Pin für den Anschluss benötigt?
data InterruptPinBenötigt
    = InterruptPinBenötigt
    | InterruptPinEgal
    deriving (Show, Eq)

-- | Alle Möglichen Werte von 'Value'.
alleValues :: NonEmpty Value
alleValues = NonEmpty.fromList [minBound .. maxBound]

-- | Alle unterstützten Anschlussmöglichkeiten.
data Anschluss (i :: MitInterruptPin) where
    AnschlussPin :: { pin :: Pin } -> Anschluss 'MitInterruptPin
    AnschlussPCF8574Port :: { pcf8574Port :: PCF8574Port i } -> Anschluss i

deriving instance Eq (Anschluss i)

deriving instance Show (Anschluss i)

deriving instance Ord (Anschluss i)

instance Anzeige (Anschluss i) where
    anzeige :: Anschluss i -> Sprache -> Text
    anzeige AnschlussPin {pin} = const $ showText pin
    anzeige AnschlussPCF8574Port {pcf8574Port} = anzeige pcf8574Port

instance Read (Anschluss 'MitInterruptPin) where
    readPrec :: ReadPrec (Anschluss 'MitInterruptPin)
    readPrec = (AnschlussPin <$> readPrec) <|> (AnschlussPCF8574Port <$> readPrec)

instance Read (Anschluss 'OhneInterruptPin) where
    readPrec :: ReadPrec (Anschluss 'OhneInterruptPin)
    readPrec = AnschlussPCF8574Port <$> readPrec

-- | Klasse für 'Anschluss'-Typen.
class AnschlussKlasse a where
    -- | Konvertiere in einen 'Anschluss'.
    zuAnschluss :: a -> AnschlussEither

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
    zuPCF8574Port :: a -> Maybe (PCF8574Port 'OhneInterruptPin)

    -- | Konvertiere (wenn möglich) einen 'Anschluss' in einen 'PCF8574Port' mit Interrupt-Pin.
    zuPCF8574PortInterruptPin :: a -> Maybe (PCF8574Port 'MitInterruptPin)

    -- | Schreibe einen 'Value' in einen Anschlussmöglichkeit.
    anschlussWrite :: (I2CReader r m, MonadIO m) => a -> Value -> m ()

    -- | Lese einen 'Value' aus einem 'Anschluss'.
    anschlussRead :: (I2CReader r m, MonadIO m) => a -> m Value

instance (InterruptPinKlasse i) => AnschlussKlasse (Anschluss i) where
    zuAnschluss :: Anschluss i -> AnschlussEither
    zuAnschluss = zuAnschlussEither

    zuPin :: Anschluss i -> Maybe Pin
    zuPin AnschlussPin {pin} = Just pin
    zuPin _anschluss = Nothing

    zuPinGpio :: (Num n) => Anschluss i -> Maybe n
    zuPinGpio (AnschlussPin pin) = Just $ case pinToBcmGpio pin of
        (Just gpio) -> fromIntegral gpio
        Nothing -> 0
    zuPinGpio _anschluss = Nothing

    zuPCF8574Port :: Anschluss i -> Maybe (PCF8574Port 'OhneInterruptPin)
    zuPCF8574Port AnschlussPCF8574Port {pcf8574Port} = Just $ ohneInterruptPin pcf8574Port
    zuPCF8574Port _anschluss = Nothing

    zuPCF8574PortInterruptPin :: Anschluss i -> Maybe (PCF8574Port 'MitInterruptPin)
    zuPCF8574PortInterruptPin
        AnschlussPCF8574Port
        {pcf8574Port = pcf8574Port@PCF8574Port {pcf8574 = PCF8574InterruptPin {}}} =
        Just pcf8574Port
    zuPCF8574PortInterruptPin _anschluss = Nothing

    anschlussWrite :: (I2CReader r m, MonadIO m) => Anschluss i -> Value -> m ()
    anschlussWrite AnschlussPin {pin} = liftIO . (pinMode pin OUTPUT >>) . digitalWrite pin
    anschlussWrite AnschlussPCF8574Port {pcf8574Port} = pcf8574PortWrite pcf8574Port

    anschlussRead :: (I2CReader r m, MonadIO m) => Anschluss i -> m Value
    anschlussRead AnschlussPin {pin} = liftIO $ pinMode pin INPUT >> digitalRead pin
    anschlussRead AnschlussPCF8574Port {pcf8574Port} = pcf8574PortRead pcf8574Port

instance AnschlussKlasse Pin where
    zuAnschluss :: Pin -> AnschlussEither
    zuAnschluss = AnschlussMit . AnschlussPin

    zuPin :: Pin -> Maybe Pin
    zuPin = Just

    zuPinGpio :: (Num n) => Pin -> Maybe n
    zuPinGpio pin = Just $ case pinToBcmGpio pin of
        (Just gpio) -> fromIntegral gpio
        Nothing -> 0

    zuPCF8574Port :: Pin -> Maybe (PCF8574Port 'OhneInterruptPin)
    zuPCF8574Port = const Nothing

    zuPCF8574PortInterruptPin :: Pin -> Maybe (PCF8574Port 'MitInterruptPin)
    zuPCF8574PortInterruptPin = const Nothing

    anschlussWrite :: (I2CReader r m, MonadIO m) => Pin -> Value -> m ()
    anschlussWrite pin = liftIO . (pinMode pin OUTPUT >>) . digitalWrite pin

    anschlussRead :: (I2CReader r m, MonadIO m) => Pin -> m Value
    anschlussRead pin = liftIO $ pinMode pin INPUT >> digitalRead pin

instance (InterruptPinKlasse i) => AnschlussKlasse (PCF8574Port i) where
    zuAnschluss :: PCF8574Port i -> AnschlussEither
    zuAnschluss = zuAnschlussEither . AnschlussPCF8574Port

    zuPin :: PCF8574Port i -> Maybe Pin
    zuPin = const Nothing

    zuPinGpio :: (Num n) => PCF8574Port i -> Maybe n
    zuPinGpio = const Nothing

    zuPCF8574Port :: PCF8574Port i -> Maybe (PCF8574Port 'OhneInterruptPin)
    zuPCF8574Port = Just . ohneInterruptPin

    zuPCF8574PortInterruptPin :: PCF8574Port i -> Maybe (PCF8574Port 'MitInterruptPin)
    zuPCF8574PortInterruptPin
        pcf8574Port@PCF8574Port {pcf8574 = PCF8574InterruptPin {}} = Just pcf8574Port
    zuPCF8574PortInterruptPin _pcf8574Port = Nothing

    anschlussWrite :: (I2CReader r m, MonadIO m) => PCF8574Port i -> Value -> m ()
    anschlussWrite = pcf8574PortWrite

    anschlussRead :: (I2CReader r m, MonadIO m) => PCF8574Port i -> m Value
    anschlussRead = pcf8574PortRead

-- | 'Anschluss' ohne Typ-Information über 'MitInterruptPin'.
data AnschlussEither
    = AnschlussMit (Anschluss 'MitInterruptPin)
    | AnschlussOhne (Anschluss 'OhneInterruptPin)
    deriving (Eq, Ord)

instance Show AnschlussEither where
    show :: AnschlussEither -> String
    show (AnschlussMit a) = show a
    show (AnschlussOhne a) = show a

instance Read AnschlussEither where
    readPrec :: ReadPrec AnschlussEither
    readPrec = (AnschlussMit <$> readPrec) <|> (AnschlussOhne <$> readPrec)

instance Anzeige AnschlussEither where
    anzeige :: AnschlussEither -> Sprache -> Text
    anzeige (AnschlussMit a) = anzeige a
    anzeige (AnschlussOhne a) = anzeige a

instance AnschlussKlasse AnschlussEither where
    zuAnschluss :: AnschlussEither -> AnschlussEither
    zuAnschluss = id

    zuPin :: AnschlussEither -> Maybe Pin
    zuPin (AnschlussMit a) = zuPin a
    zuPin (AnschlussOhne a) = zuPin a

    zuPinGpio :: (Num n) => AnschlussEither -> Maybe n
    zuPinGpio (AnschlussMit a) = zuPinGpio a
    zuPinGpio (AnschlussOhne a) = zuPinGpio a

    zuPCF8574Port :: AnschlussEither -> Maybe (PCF8574Port 'OhneInterruptPin)
    zuPCF8574Port (AnschlussMit a) = zuPCF8574Port a
    zuPCF8574Port (AnschlussOhne a) = zuPCF8574Port a

    zuPCF8574PortInterruptPin :: AnschlussEither -> Maybe (PCF8574Port 'MitInterruptPin)
    zuPCF8574PortInterruptPin (AnschlussMit a) = zuPCF8574PortInterruptPin a
    zuPCF8574PortInterruptPin (AnschlussOhne a) = zuPCF8574PortInterruptPin a

    anschlussWrite :: (I2CReader r m, MonadIO m) => AnschlussEither -> Value -> m ()
    anschlussWrite (AnschlussMit a) = anschlussWrite a
    anschlussWrite (AnschlussOhne a) = anschlussWrite a

    anschlussRead :: (I2CReader r m, MonadIO m) => AnschlussEither -> m Value
    anschlussRead (AnschlussMit a) = anschlussRead a
    anschlussRead (AnschlussOhne a) = anschlussRead a

class InterruptPinKlasse (i :: MitInterruptPin) where
    zuAnschlussEither :: Anschluss i -> AnschlussEither

instance InterruptPinKlasse 'MitInterruptPin where
    zuAnschlussEither :: Anschluss 'MitInterruptPin -> AnschlussEither
    zuAnschlussEither = AnschlussMit

instance InterruptPinKlasse 'OhneInterruptPin where
    zuAnschlussEither :: Anschluss 'OhneInterruptPin -> AnschlussEither
    zuAnschlussEither = AnschlussOhne

-- | Erhalte den 'Pin', welche eine Änderung der eingehenden Spannung angibt.
anschlussInterruptPin :: Anschluss 'MitInterruptPin -> Pin
anschlussInterruptPin AnschlussPin {pin} = pin
anschlussInterruptPin
    AnschlussPCF8574Port {pcf8574Port = PCF8574Port {pcf8574 = PCF8574InterruptPin {interruptPin}}} =
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
    forkInterruptReader action = liftIO . forkIOSilent . void . runReaderT action =<< ask

instance (MonadReader r m, MitInterruptMap r) => InterruptReader r m

-- | Registriere ein Event für einen 'Anschluss'.
-- Wenn das Ergebnis der Aktion 'EventBehalten' ist wird sie beim nächsten Event erneut ausgeführt,
-- ansonsten ('EventLöschen') wird sie nur einmal ausgeführt.
--
-- Diese Funktion hat nur für Anschlüsse mit 'interruptPin' einen Effekt.
beiÄnderung :: (InterruptReader r m, I2CReader r m, MonadIO m)
             => Anschluss 'MitInterruptPin
             -> IntEdge
             -> IO EventBehalten
             -> m ()
beiÄnderung anschluss intEdge aktion = do
    readerWert <- ask
    tmvarInterruptMap <- erhalteInterruptMap
    aktuelleInterruptMap <- liftIO $ atomically $ takeTMVar tmvarInterruptMap
    case Map.lookup interruptPin aktuelleInterruptMap of
        (Just (aktionen, alterWert)) -> liftIO
            $ atomically
            $ putTMVar tmvarInterruptMap
            $ Map.insert
                interruptPin
                (beiRichtigemBitValue anschluss intEdge : aktionen, alterWert)
                aktuelleInterruptMap
        Nothing -> do
            wert <- anschlussReadBitValue anschluss
            liftIO $ do
                pinMode interruptPin INPUT
                if isRaspi
                    then wiringPiISR interruptPin (verwendeteIntEdge anschluss)
                        $ runReaderT aktionenAusführen readerWert
                    else 
                        -- wiringPiISR führt auf Windows zu einem Auswerten von 'undefined'
                        putStrLn
                        $ "wiringPiISR "
                        ++ show interruptPin
                        ++ " "
                        ++ show (verwendeteIntEdge anschluss)
                atomically
                    $ putTMVar tmvarInterruptMap
                    $ Map.insert
                        interruptPin
                        ([beiRichtigemBitValue anschluss intEdge], wert)
                        aktuelleInterruptMap
    where
        interruptPin :: Pin
        interruptPin = anschlussInterruptPin anschluss

        verwendeteIntEdge :: Anschluss 'MitInterruptPin -> IntEdge
        verwendeteIntEdge AnschlussPin {} = INT_EDGE_BOTH
        verwendeteIntEdge AnschlussPCF8574Port {} = INT_EDGE_FALLING

        beiRichtigemBitValue
            :: Anschluss 'MitInterruptPin -> IntEdge -> (BitValue, BitValue) -> IO EventBehalten
        beiRichtigemBitValue AnschlussPin {} INT_EDGE_BOTH _werte = aktion
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_BOTH
            (wert, alterWert)
            | testBit wert port == testBit alterWert port = pure EventBehalten
            | otherwise = aktion
        beiRichtigemBitValue
            AnschlussPin {}
            INT_EDGE_FALLING
            (fromBitValue -> wert, fromBitValue -> alterWert)
            | alterWert > wert = aktion
            | otherwise = pure EventBehalten
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_FALLING
            (wert, alterWert)
            | testBit alterWert port && not (testBit wert port) = aktion
            | otherwise = pure EventBehalten
        beiRichtigemBitValue
            AnschlussPin {}
            INT_EDGE_RISING
            (fromBitValue -> wert, fromBitValue -> alterWert)
            | alterWert < wert = aktion
            | otherwise = pure EventBehalten
        beiRichtigemBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {port = (fromIntegral -> port)}}
            INT_EDGE_RISING
            (wert, alterWert)
            | not (testBit alterWert port) && testBit wert port = aktion
            | otherwise = pure EventBehalten
        beiRichtigemBitValue _anschluss INT_EDGE_SETUP _werte = pure EventBehalten

        anschlussReadBitValue
            :: (I2CReader r m, MonadIO m) => Anschluss 'MitInterruptPin -> m BitValue
        anschlussReadBitValue AnschlussPin {pin} = liftIO $ toBitValue <$> digitalRead pin
            where
                toBitValue :: Value -> BitValue
                toBitValue LOW = emptyBitValue
                toBitValue HIGH = fullBitValue
        anschlussReadBitValue
            AnschlussPCF8574Port {pcf8574Port = PCF8574Port {pcf8574}} = pcf8574Read pcf8574

        aktionenAusführen :: (InterruptReader r m, I2CReader r m, MonadIO m) => m ()
        aktionenAusführen = do
            tmvarInterruptMap <- erhalteInterruptMap
            wert <- anschlussReadBitValue anschluss
            liftIO $ do
                iMap <- atomically $ takeTMVar tmvarInterruptMap
                eintragAnpassen <- case Map.lookup interruptPin iMap of
                    (Just (aktionen, alterWert)) -> do
                        verbliebeneAktionen
                            <- foldM (aktionAusführen (wert, alterWert)) [] aktionen
                        pure $ Map.insert interruptPin (verbliebeneAktionen, wert)
                    Nothing -> pure id
                atomically $ putTMVar tmvarInterruptMap $ eintragAnpassen iMap

        aktionAusführen :: (BitValue, BitValue)
                         -> [(BitValue, BitValue)
                            -> IO EventBehalten]
                         -> ((BitValue, BitValue) -> IO EventBehalten)
                         -> IO
                             [(BitValue, BitValue)
                             -> IO EventBehalten]
        aktionAusführen bitValues acc akt = eventAnhängen <$> akt bitValues
            where
                eventAnhängen :: EventBehalten -> [(BitValue, BitValue) -> IO EventBehalten]
                eventAnhängen EventBehalten = akt : acc
                eventAnhängen EventLöschen = acc

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
warteAufÄnderung :: (InterruptReader r m, I2CReader r m, MonadIO m)
                  => [(Anschluss 'MitInterruptPin, IntEdge)]
                  -> m ()
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
        unless (elem SignalErhalten listeSignalErhalten) retry

-- JSON-Instanz-Deklarationen für Anschluss
-- Dabei wird eine Rückwärtskompatibilität zu Versionen < 1.0.1.0 berücksichtigt.
-- Bei diesen war nur ein Pin-Anschluss erlaubt, wodurch ein Anschluss nur durch eine Zahl gespeichert wurde.
instance Aeson.FromJSON AnschlussEither where
    parseJSON :: Aeson.Value -> Aeson.Parser AnschlussEither
    parseJSON v = (AnschlussMit <$> Aeson.parseJSON v) <|> (AnschlussOhne <$> Aeson.parseJSON v)

instance Aeson.ToJSON AnschlussEither where
    toJSON :: AnschlussEither -> Aeson.Value
    toJSON (AnschlussMit a) = Aeson.toJSON a
    toJSON (AnschlussOhne a) = Aeson.toJSON a

-- | Parse einen 'AnschlussEither'.
--
-- Dabei wird eine Rückwärtskompatibilität zu Versionen < 1.0.1.0 berücksichtigt.
-- Bei diesen war nur ein 'Pin'-Anschluss erlaubt, weshalb die JSON-Felder anders hießen.
parseAnschlussEither :: Aeson.Object -> Text -> Text -> Aeson.Parser AnschlussEither
parseAnschlussEither v anschlussJS pinJS =
    (v .: anschlussJS) <|> (AnschlussMit . AnschlussPin . Gpio <$> v .: pinJS)

instance Aeson.FromJSON (Anschluss 'MitInterruptPin) where
    parseJSON :: Aeson.Value -> Aeson.Parser (Anschluss 'MitInterruptPin)
    parseJSON (Aeson.Object v) =
        (AnschlussPin . Gpio <$> v .: JS.pin) <|> (AnschlussPCF8574Port <$> v .: JS.port)
    parseJSON (Aeson.Number pin) = pure $ AnschlussPin $ Gpio $ floor pin
    parseJSON _value = empty

instance Aeson.FromJSON (Anschluss 'OhneInterruptPin) where
    parseJSON :: Aeson.Value -> Aeson.Parser (Anschluss 'OhneInterruptPin)
    parseJSON (Aeson.Object v) = AnschlussPCF8574Port <$> v .: JS.port
    parseJSON _value = empty

instance Aeson.ToJSON (Anschluss i) where
    toJSON :: Anschluss i -> Aeson.Value
    toJSON anschluss@AnschlussPin {} =
        Aeson.object [JS.pin .= (fromJust $ zuPinGpio anschluss :: Int)]
    toJSON AnschlussPCF8574Port {pcf8574Port} = Aeson.object [JS.port .= pcf8574Port]

-- | Parse das Fließend-Feld.
-- Dabei wird eine Rückwärtskompatibilität zu Versionen <1.0.0.14 berücksichtigt.
-- Bei diesen wurde intern immer 'HIGH' angenommen.
parseFließend :: Aeson.Object -> Aeson.Parser Value
parseFließend v = (v .: JS.fließend) <|> pure HIGH
