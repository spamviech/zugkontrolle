{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Description : PWM-Implementierung über Software.

Pulswellen-Modulation (PWM) ermöglicht es, die Ausgangsspannung über Software zu regulieren.
Dieses Modul bietet eine Implementierung in Software an.
Im Gegensatz zu Hardware-basierter PWM, die nur von einigen Pins des Raspberry Pi unterstützt wird, ist diese mit allen Pins möglich.
-}
module Zug.Anbindung.SoftwarePWM (
    -- * Map über aktuell laufende PWM-Ausgabe
    PwmMap, pwmMapEmpty, PwmReader(..),
    -- * Aufruf der PWM-Funktionen
    pwmSoftwareSetzteWert, pwmGrenze) where

-- Bibliotheken
import Control.Concurrent.STM (TVar, modifyTVar, readTVar, writeTVar, readTVarIO, atomically)
import Control.Monad (void, when)
import Control.Monad.Trans (MonadIO(..))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (isNothing)
import Numeric.Natural (Natural)
import System.Hardware.WiringPi (PwmValue(), Value(..))
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung.Anschluss (Anschluss(), anschlussWrite, I2CReader(..))
import Zug.Anbindung.Wartezeit (warte, Wartezeit(..), differenz, multiplizieren, dividieren)

-- | Welche Pins haben aktuell Software-PWM
type PwmMap = Map Anschluss (PwmValue, Natural)
-- | Leere 'PwmMap'
pwmMapEmpty :: PwmMap
pwmMapEmpty = Map.empty
-- | Abkürzung für Funktionen, die die aktuelle 'PwmMap' benötigen
class (I2CReader r m) => PwmReader r m | m -> r where
    -- | Erhalte die aktuelle 'PwmMap' aus der Umgebung.
    erhaltePwmMap :: m (TVar PwmMap)

-- | Maximaler Range-Wert der PWM-Funktion
pwmGrenze :: PwmValue
pwmGrenze = 1024

-- | Erhalte Zeit in µs, die der Strom während einer PWM-Periode (an, aus) ist.
pwmBerechneZeiten :: Natural -> PwmValue -> (Wartezeit, Wartezeit)
pwmBerechneZeiten pwmFrequenzHz pwmWert = (zeitAn, zeitAus)
    where
        zeitAn :: Wartezeit
        zeitAn = dividieren (multiplizieren pwmPeriodendauer $ fromIntegral pwmWert) $ fromIntegral pwmGrenze
        zeitAus :: Wartezeit
        zeitAus = differenz pwmPeriodendauer zeitAn
        µsInS :: Natural
        µsInS = 1000000
        pwmPeriodendauer :: Wartezeit
        pwmPeriodendauer = MikroSekunden $ div µsInS pwmFrequenzHz

-- | Nutze Haskell-Module um ein Software-generiertes PWM-Signal zu erzeugen
pwmSoftwareSetzteWert :: (PwmReader r m, MonadIO m) => Anschluss -> Natural -> PwmValue -> m ()
pwmSoftwareSetzteWert anschluss _pwmFrequency 0          = do
    tvarPwmMap <- erhaltePwmMap
    liftIO $ atomically $ modifyTVar tvarPwmMap $ Map.delete anschluss
pwmSoftwareSetzteWert anschluss pwmFrequenz   pwmWert    = do
    tvarPwmMap <- erhaltePwmMap
    pwmMapAlt <- liftIO $ atomically $ do
        pwmMapAlt <- readTVar tvarPwmMap
        writeTVar tvarPwmMap $ Map.insert anschluss (pwmWert, pwmFrequenz) pwmMapAlt
        pure pwmMapAlt
    -- Starte neuen Pwm-Thread, falls er noch nicht existiert
    when (isNothing $ Map.lookup anschluss pwmMapAlt) $ void $ forkI2CReader $ pwmSoftwareAnschlussMain anschluss
        where
            -- | PWM-Funktion für einen 'Anschluss'. Läuft in einem eigenem Thread.
            -- 
            -- Läuft so lange in einer Dauerschleife, bis der Wert für den betroffenen 'Anschluss' in der übergebenen 'TVar' 'PwmMap' nicht mehr vorkommt.
            pwmSoftwareAnschlussMain :: (PwmReader r m, MonadIO m) => Anschluss -> m ()
            pwmSoftwareAnschlussMain anschluss = do
                tvarPwmMap <- erhaltePwmMap
                pwmMap <- liftIO $ readTVarIO tvarPwmMap
                case Map.lookup anschluss pwmMap of
                    Nothing                         -> pure ()
                    (Just (pwmWert, pwmFrequenz))   -> do
                        anschlussWrite anschluss HIGH
                        let (zeitAn, zeitAus) = pwmBerechneZeiten pwmFrequenz pwmWert
                        warte zeitAn
                        anschlussWrite anschluss LOW
                        warte zeitAus
                        pwmSoftwareAnschlussMain anschluss