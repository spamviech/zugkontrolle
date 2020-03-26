{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description : PWM-Implementierung über Software.

Pulswellen-Modulation (PWM) ermöglicht es, die Ausgangsspannung über Software zu regulieren.
Dieses Modul bietet eine Implementierung in Software an.
Im Gegensatz zu Hardware-basierter PWM, die nur von einigen Pins des Raspberry Pi unterstützt wird, ist diese mit allen Pins möglich.
-}
module Zug.Anbindung.SoftwarePWM
  ( -- * Map über aktuell laufende PWM-Ausgabe
    PwmMap
  , pwmMapEmpty
  , MitPwmMap(..)
  , PwmReader(..)
    -- * Aufruf der PWM-Funktionen
  , pwmSoftwareSetzteWert
  , pwmGrenze
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM (TVar, modifyTVar, readTVar, writeTVar, readTVarIO, atomically)
import Control.Monad (void, when)
import Control.Monad.Reader (ReaderT, asks, MonadReader(ask), runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (isNothing)
import Numeric.Natural (Natural)
import System.Hardware.WiringPi (PwmValue(), Value(..), Pin(), digitalWrite, Mode(OUTPUT), pinMode)

import Zug.Anbindung.Wartezeit (warte, Wartezeit(..), differenz, multiplizieren, dividieren)

-- | Welche Pins haben aktuell Software-PWM
type PwmMap = Map Pin (PwmValue, Natural)

-- | Leere 'PwmMap'
pwmMapEmpty :: PwmMap
pwmMapEmpty = Map.empty

-- | Klasse für Typen mit der aktellen 'PwmMap'
class MitPwmMap r where
    pwmMap :: r -> TVar PwmMap

-- | Abkürzung für Funktionen, die die aktuelle 'PwmMap' benötigen
class (MonadReader r m, MitPwmMap r) => PwmReader r m | m -> r where
    -- | Erhalte die aktuelle 'PwmMap' aus der Umgebung.
    erhaltePwmMap :: m (TVar PwmMap)
    erhaltePwmMap = asks pwmMap

    -- | 'forkIO' in die 'PwmReader'-Monade geliftet; Die aktuellen Umgebung soll übergeben werden.
    forkPwmReader :: (MonadIO m) => ReaderT r IO () -> m ThreadId
    forkPwmReader action = do
        reader <- ask
        liftIO $ forkIO $ void $ runReaderT action reader

instance (MonadReader r m, MitPwmMap r) => PwmReader r m

-- | Maximaler Range-Wert der PWM-Funktion
pwmGrenze :: PwmValue
pwmGrenze = 1024

-- | Erhalte Zeit in µs, die der Strom während einer PWM-Periode (an, aus) ist.
pwmBerechneZeiten :: Natural -> PwmValue -> (Wartezeit, Wartezeit)
pwmBerechneZeiten pwmFrequenzHz pwmWert = (zeitAn, zeitAus)
    where
        zeitAn :: Wartezeit
        zeitAn =
            dividieren (multiplizieren pwmPeriodendauer $ fromIntegral pwmWert)
            $ fromIntegral pwmGrenze

        zeitAus :: Wartezeit
        zeitAus = differenz pwmPeriodendauer zeitAn

        µsInS :: Natural
        µsInS = 1000000

        pwmPeriodendauer :: Wartezeit
        pwmPeriodendauer = MikroSekunden $ div µsInS pwmFrequenzHz

-- | Nutze Haskell-Module um ein Software-generiertes PWM-Signal zu erzeugen
pwmSoftwareSetzteWert :: (PwmReader r m, MonadIO m) => Pin -> Natural -> PwmValue -> m ()
pwmSoftwareSetzteWert pin _pwmFrequency 0 = do
    tvarPwmMap <- erhaltePwmMap
    liftIO $ atomically $ modifyTVar tvarPwmMap $ Map.delete pin
pwmSoftwareSetzteWert pin pwmFrequenz pwmWert = do
    tvarPwmMap <- erhaltePwmMap
    pwmMapAlt <- liftIO $ atomically $ do
        pwmMapAlt <- readTVar tvarPwmMap
        writeTVar tvarPwmMap $ Map.insert pin (pwmWert, pwmFrequenz) pwmMapAlt
        pure pwmMapAlt
    -- Starte neuen Pwm-Thread, falls er noch nicht existiert
    when (isNothing $ Map.lookup pin pwmMapAlt)
        $ void
        $ forkPwmReader
        $ pwmSoftwareAnschlussMain pin

-- | PWM-Funktion für einen 'Anschluss'. Läuft in einem eigenem Thread.
--
-- Läuft so lange in einer Dauerschleife, bis der Wert für den betroffenen 'Anschluss' in der übergebenen 'TVar' 'PwmMap' nicht mehr vorkommt.
pwmSoftwareAnschlussMain :: (MitPwmMap r) => Pin -> ReaderT r IO ()
pwmSoftwareAnschlussMain pin = do
    tvarPwmMap <- erhaltePwmMap
    pwmMap <- liftIO $ readTVarIO tvarPwmMap
    case Map.lookup pin pwmMap of
        Nothing -> pure ()
        (Just (pwmWert, pwmFrequenz)) -> do
            liftIO $ do
                pinMode pin OUTPUT
                digitalWrite pin HIGH
                let (zeitAn, zeitAus) = pwmBerechneZeiten pwmFrequenz pwmWert
                warte zeitAn
                digitalWrite pin LOW
                warte zeitAus
            pwmSoftwareAnschlussMain pin