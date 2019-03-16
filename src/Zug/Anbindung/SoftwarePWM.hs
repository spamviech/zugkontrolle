{-|
Description : PWM-Implementierung über Software.

Pulswellen-Modulation (PWM) ermöglicht es, die Ausgangsspannung über Software zu regulieren.
Dieses Modul bietet eine Implementierung in Software an.
Im Gegensatz zu Hardware-basierter PWM, die nur von einigen Pins des Raspberry Pi unterstützt wird, ist diese mit allen Pins möglich.
-}
module Zug.Anbindung.SoftwarePWM (
    -- * Map über aktuell laufende PWM-Ausgabe
    PinMap, PinMapIO, pinMapEmpty,
    -- * Aufruf der PWM-Funktion und zugehörige Hilfsfunktionen
    pwmWriteSoftware, pwmRange, delayµs) where

-- Bibliotheken
import qualified Data.Map.Strict as Map
import System.Hardware.WiringPi
import Control.Concurrent
import Control.Monad (void, when)
import Data.Maybe (isNothing)
import Numeric.Natural (Natural)

-- | Welche Pins haben aktuell Software-PWM
type PinMap = Map.Map Pin (PwmValue, Natural)
-- | Abkürzung für Funktionen, die die aktuelle PinMap benötigen
type PinMapIO a = MVar PinMap -> IO a
-- | Leere PinMap
pinMapEmpty :: PinMap
pinMapEmpty = Map.empty

-- | Maximaler Range-Wert der PWM-Funktion
pwmRange :: PwmValue
pwmRange        = 1024

-- | Erhalte Zeit, die der Strom während einer PWM-Periode (an, aus) ist.
pwmGetTimeµs :: Natural -> PwmValue -> (Natural, Natural)
pwmGetTimeµs    pwmFrequency    pwmValue    = (onTime, offTime)
    where
        onTime :: Natural
        onTime = div (pwmPeriodµs * (fromIntegral pwmValue)) (fromIntegral pwmRange)
        offTime :: Natural
        offTime = pwmPeriodµs - onTime
        µsInS :: Natural
        µsInS = 1000000
        pwmPeriodµs :: Natural
        pwmPeriodµs     = div µsInS pwmFrequency

-- | Nutze Haskell-Module um ein Software-generiertes PWM-Signal zu erzeugen
pwmWriteSoftware :: Pin -> Natural -> PwmValue -> PinMapIO ()
pwmWriteSoftware pin _pwmFrequency  0           mvarPinMap = do
    modifyMVar_ mvarPinMap $ pure . Map.delete pin
pwmWriteSoftware pin pwmFrequency   pwmValue    mvarPinMap = do
    pinMapAlt <- modifyMVar mvarPinMap $ \pinMapAlt -> pure (Map.insert pin (pwmValue, pwmFrequency) pinMapAlt, pinMapAlt)
    -- Starte neuen Pwm-Thread, falls er noch nicht existiert
    when (isNothing $ Map.lookup pin pinMapAlt) $ void $ forkIO $ runPWMSoftware pin mvarPinMap

-- | PWM-Funktion für einen 'Pin'. Sollte in einem eigenem Thread laufen.
runPWMSoftware :: Pin -> PinMapIO ()
runPWMSoftware pin mvarPinMap = do
    pinMapCurrent <- readMVar mvarPinMap
    case Map.lookup pin pinMapCurrent of
        (Nothing)                       -> pure ()
        (Just (pwmValue, pwmFrequency)) -> do
            pinMode pin OUTPUT
            digitalWrite pin HIGH
            let (onTime, offTime) = pwmGetTimeµs pwmFrequency pwmValue
            delayµs onTime
            digitalWrite pin LOW
            delayµs offTime
            runPWMSoftware pin mvarPinMap

-- | Warte mindestens das Argument in µs.
-- 
-- Die Wartezeit kann länger sein (bedingt durch 'threadDelay'), allerdings kommt es nicht zu einem divide-by-zero error für 0-Argumente.
delayµs :: Natural -> IO ()
delayµs time = when (time > 0) $ threadDelay $ fromIntegral time