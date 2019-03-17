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
    pwmSoftwareSetzteWert, pwmGrenze, warteµs) where

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
pwmGrenze :: PwmValue
pwmGrenze = 1024

-- | Erhalte Zeit in µs, die der Strom während einer PWM-Periode (an, aus) ist.
pwmBerechneZeiten :: Natural -> PwmValue -> (Natural, Natural)
pwmBerechneZeiten   pwmFrequenzHz   pwmWert = (zeitAnµs, zeitAusµs)
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
pwmSoftwareSetzteWert :: Pin -> Natural -> PwmValue -> PinMapIO ()
pwmSoftwareSetzteWert pin _pwmFrequency  0           mvarPinMap = modifyMVar_ mvarPinMap $ pure . Map.delete pin
pwmSoftwareSetzteWert pin pwmFrequenz   pwmWert    mvarPinMap = do
    pinMapAlt <- modifyMVar mvarPinMap $ \pinMapAlt -> pure (Map.insert pin (pwmWert, pwmFrequenz) pinMapAlt, pinMapAlt)
    -- Starte neuen Pwm-Thread, falls er noch nicht existiert
    when (isNothing $ Map.lookup pin pinMapAlt) $ void $ forkIO $ pwmSoftwarePinMain pin mvarPinMap

-- | PWM-Funktion für einen 'Pin'. Sollte in einem eigenem Thread laufen.
-- 
-- Läuft so lange in einer Dauerschleife, bis der Wert für den betroffenen 'Pin' in der übergebenen 'MVar' 'PinMap' nicht mehr vorkommt.
pwmSoftwarePinMain :: Pin -> PinMapIO ()
pwmSoftwarePinMain pin mvarPinMap = do
    pinMap <- readMVar mvarPinMap
    case Map.lookup pin pinMap of
        (Nothing)                       -> pure ()
        (Just (pwmWert, pwmFrequenz))   -> do
            pinMode pin OUTPUT
            digitalWrite pin HIGH
            let (zeitAnµs, zeitAusµs) = pwmBerechneZeiten pwmFrequenz pwmWert
            warteµs zeitAnµs
            digitalWrite pin LOW
            warteµs zeitAusµs
            pwmSoftwarePinMain pin mvarPinMap

-- | Warte mindestens das Argument in µs.
-- 
-- Die Wartezeit kann länger sein (bedingt durch 'threadDelay'), allerdings kommt es nicht zu einem divide-by-zero error für 0-Argumente.
warteµs :: Natural -> IO ()
warteµs time = when (time > 0) $ threadDelay $ fromIntegral time