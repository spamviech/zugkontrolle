{-|
Description : PWM-Implementierung über Software.

Pulswellen-Modulation (PWM) ermöglicht es, die Ausgangsspannung über Software zu regulieren.
Dieses Modul bietet eine Implementierung in Software an.
Im Gegensatz zu Hardware-basierter PWM, die nur von einigen Pins des Raspberry Pi unterstützt wird, ist diese mit allen Pins möglich.
-}
module Zug.Anbindung.SoftwarePWM (
    -- * Map über aktuell laufende PWM-Ausgabe
    PwmMap, PwmMapT, pwmMapEmpty, runPwmMapT, liftI2CMapT, forkPwmMapT,
    -- * Aufruf der PWM-Funktionen
    pwmSoftwareSetzteWert, pwmGrenze) where

-- Bibliotheken
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import System.Hardware.WiringPi (PwmValue(), Value(..))
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM (TVar, modifyTVar, readTVar, writeTVar, readTVarIO, atomically)
import Control.Monad (void, when)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO, lift)
import Data.Maybe (isNothing)
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung.Anschluss (Anschluss(), anschlussWrite, warteµs, I2CMap, I2CMapT, runI2CMapT)

-- | Welche Pins haben aktuell Software-PWM
type PwmMap = Map Anschluss (PwmValue, Natural)
-- | Abkürzung für Funktionen, die die aktuelle 'PwmMap' benötigen
type PwmMapT m a = ReaderT (TVar PwmMap) (I2CMapT m) a
-- | Leere 'PwmMap'
pwmMapEmpty :: PwmMap
pwmMapEmpty = Map.empty
-- | Führe eine 'I2CMapT'-Aktion in einer 'PwmMapT'-Umbegung aus.
liftI2CMapT :: (Monad m) => I2CMapT m a -> PwmMapT m a
liftI2CMapT = lift
-- | Führe eine 'PwmMapT'-Aktion in der angegebenen Umbebung aus
runPwmMapT :: PwmMapT m a -> (TVar PwmMap, TVar I2CMap) -> m a
runPwmMapT action (tvarPwmMap, tvarI2CMap) = flip runI2CMapT tvarI2CMap $ runReaderT action tvarPwmMap
-- | 'forkIO' in den 'PwmMapT'-Monaden Transformer gelifted; Die 'TVar's aus der aktuellen Umgebung werden übergeben.
forkPwmMapT :: PwmMapT IO () -> PwmMapT IO ThreadId
forkPwmMapT aktion = do
    tvarPwmMap <- ask
    tvarI2CMap <- liftI2CMapT ask
    liftIO $ forkIO $ flip runPwmMapT (tvarPwmMap, tvarI2CMap) $ aktion

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
pwmSoftwareSetzteWert :: Anschluss -> Natural -> PwmValue -> PwmMapT IO ()
pwmSoftwareSetzteWert anschluss _pwmFrequency 0          = do
    tvarPwmMap <- ask
    liftIO $ atomically $ modifyTVar tvarPwmMap $ Map.delete anschluss
pwmSoftwareSetzteWert anschluss pwmFrequenz   pwmWert    = do
    tvarPwmMap <- ask
    pwmMapAlt <- liftIO $ atomically $ do
        pwmMapAlt <- readTVar tvarPwmMap
        writeTVar tvarPwmMap $ Map.insert anschluss (pwmWert, pwmFrequenz) pwmMapAlt
        pure pwmMapAlt
    -- Starte neuen Pwm-Thread, falls er noch nicht existiert
    when (isNothing $ Map.lookup anschluss pwmMapAlt) $ void $ forkPwmMapT $ pwmSoftwareAnschlussMain anschluss
        where
            -- | PWM-Funktion für einen 'Anschluss'. Läuft in einem eigenem Thread.
            -- 
            -- Läuft so lange in einer Dauerschleife, bis der Wert für den betroffenen 'Anschluss' in der übergebenen 'TVar' 'PwmMap' nicht mehr vorkommt.
            pwmSoftwareAnschlussMain :: Anschluss -> PwmMapT IO ()
            pwmSoftwareAnschlussMain anschluss = do
                tvarPwmMap <- ask
                pwmMap <- liftIO $ readTVarIO tvarPwmMap
                case Map.lookup anschluss pwmMap of
                    Nothing                         -> pure ()
                    (Just (pwmWert, pwmFrequenz))   -> do
                        liftI2CMapT $ anschlussWrite anschluss HIGH
                        let (zeitAnµs, zeitAusµs) = pwmBerechneZeiten pwmFrequenz pwmWert
                        warteµs zeitAnµs
                        liftI2CMapT $ anschlussWrite anschluss LOW
                        warteµs zeitAusµs
                        pwmSoftwareAnschlussMain anschluss