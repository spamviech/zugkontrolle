{-|
Description : PWM-Implementierung über Software.

Pulswellen-Modulation (PWM) ermöglicht es, die Ausgangsspannung über Software zu regulieren.
Dieses Modul bietet eine Implementierung in Software an.
Im Gegensatz zu Hardware-basierter PWM, die nur von einigen Pins des Raspberry Pi unterstützt wird, ist diese mit allen Pins möglich.
-}
module Zug.Anbindung.SoftwarePWM (
    -- * Map über aktuell laufende PWM-Ausgabe
    PwmMap, PwmMapIO, pwmMapEmpty,
    -- * Aufruf der PWM-Funktion und zugehörige Hilfsfunktionen
    pwmSoftwareSetzteWert, pwmGrenze, warteµs) where

-- Bibliotheken
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import System.Hardware.WiringPi (Pin(), PwmValue(), Value(..), Mode(..), pinMode, digitalWrite)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, modifyTVar, readTVar, writeTVar, readTVarIO, atomically)
import Control.Monad (void, when)
import Data.Maybe (isNothing)
import Numeric.Natural (Natural)

-- | Welche Pins haben aktuell Software-PWM
type PwmMap = Map Pin (PwmValue, Natural)
-- | Abkürzung für Funktionen, die die aktuelle 'PwmMap' benötigen
type PwmMapIO a = TVar PwmMap -> IO a
-- | Leere 'PwmMap'
pwmMapEmpty :: PwmMap
pwmMapEmpty = Map.empty

-- | Maximaler Range-Wert der PWM-Funktion
pwmGrenze :: PwmValue
pwmGrenze = 1024

-- | Erhalte Zeit in µs, die der Strom während einer PWM-Periode (an, aus) ist.
pwmBerechneZeiten :: Natural -> PwmValue -> (Natural, Natural)
pwmBerechneZeiten pwmFrequenzHz pwmWert = (zeitAnµs, zeitAusµs)
    where
        zeitAnµs :: Natural
        zeitAnµs = div (pwmPeriodendauerµs * (fromIntegral pwmWert)) (fromIntegral pwmGrenze)
        zeitAusµs :: Natural
        zeitAusµs = pwmPeriodendauerµs - zeitAnµs
        µsInS :: Natural
        µsInS = 1000000
        pwmPeriodendauerµs :: Natural
        pwmPeriodendauerµs = div µsInS pwmFrequenzHz

-- | Nutze Haskell-Module um ein Software-generiertes PWM-Signal zu erzeugen
pwmSoftwareSetzteWert :: Pin -> Natural -> PwmValue -> PwmMapIO ()
pwmSoftwareSetzteWert pin _pwmFrequency 0          tvarPwmMap = atomically $ modifyTVar tvarPwmMap $ Map.delete pin
pwmSoftwareSetzteWert pin pwmFrequenz   pwmWert    tvarPwmMap = do
    pwmMapAlt <- atomically $ do
        pwmMapAlt <- readTVar tvarPwmMap
        writeTVar tvarPwmMap $ Map.insert pin (pwmWert, pwmFrequenz) pwmMapAlt
        pure pwmMapAlt
    -- Starte neuen Pwm-Thread, falls er noch nicht existiert
    when (isNothing $ Map.lookup pin pwmMapAlt) $ void $ forkIO $ pwmSoftwarePinMain pin tvarPwmMap

-- | PWM-Funktion für einen 'Pin'. Sollte in einem eigenem Thread laufen.
-- 
-- Läuft so lange in einer Dauerschleife, bis der Wert für den betroffenen 'Pin' in der übergebenen 'TVar' 'PwmMap' nicht mehr vorkommt.
pwmSoftwarePinMain :: Pin -> PwmMapIO ()
pwmSoftwarePinMain pin tvarPwmMap = do
    pwmMap <- readTVarIO tvarPwmMap
    case Map.lookup pin pwmMap of
        Nothing                         -> pure ()
        (Just (pwmWert, pwmFrequenz))   -> do
            pinMode pin OUTPUT
            digitalWrite pin HIGH
            let (zeitAnµs, zeitAusµs) = pwmBerechneZeiten pwmFrequenz pwmWert
            warteµs zeitAnµs
            digitalWrite pin LOW
            warteµs zeitAusµs
            pwmSoftwarePinMain pin tvarPwmMap

-- | Warte mindestens das Argument in µs.
-- 
-- Die Wartezeit kann länger sein (bedingt durch 'threadDelay'), allerdings kommt es nicht zu einem divide-by-zero error für 0-Argumente.
warteµs :: Natural -> IO ()
warteµs time = when (time > 0) $ threadDelay $ fromIntegral time