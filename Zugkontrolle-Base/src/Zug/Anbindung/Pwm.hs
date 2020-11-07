{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description : Erzeuge ein Pwm-Signal auf einem Pin.
-}
module Zug.Anbindung.Pwm
  ( pwmSetzeWert
  , pwmServo
  , Pin(..)
  , PwmReader(..)
  , VersionReader()
  , PwmValueUnmodifiziert()
  , erhaltePwmWertVoll
  , erhaltePwmWertReduziert
  , pwmEingabeMaximal
  , pwmGrenze
  , pwmMöglich
  , clockMöglich
  ) where

import Control.Monad.Trans (MonadIO(..))
import Data.Word (Word8)
import Numeric.Natural (Natural)
import System.Hardware.WiringPi (pinMode, pwmSetRange, pwmWrite, Mode(PWM_OUTPUT))

import Zug.Anbindung.Anschluss (Pin(..), Value(..))
import Zug.Anbindung.Klassen (StreckenAtom(fließend))
import Zug.Anbindung.SoftwarePWM (PwmReader(..), PwmValue, pwmGrenze, pwmSoftwareSetzteWert)
import Zug.Options (Options(..), getOptions, PWM(..), VersionReader())

-- * Test-Funktionen, ob Anschluss bestimmte Funktionen unterstützen
-- | Unterstützt der 'Pin'
-- >'pinMode' pin 'PWM_OUTPUT'
pwmMöglich :: Pin -> Bool
pwmMöglich = flip elem ([Wpi 1, Wpi 23, Wpi 24, Wpi 26] :: [Pin])

-- | Unterstützt der 'Pin'
-- >'pinMode' pin 'GPIO_CLOCK'
clockMöglich :: Pin -> Bool
clockMöglich = flip elem ([Wpi 7, Wpi 21, Wpi 22, Wpi 29] :: [Pin])

-- * PWM-Funktion
-- | 'pwmWrite' mit alternativer Software-basierter PWM-Funktion
pwmSetzeWert :: (StreckenAtom s, PwmReader r m, VersionReader r m, MonadIO m)
             => s
             -> Pin
             -> PwmValueUnmodifiziert
             -> m ()
pwmSetzeWert s pin pwmValue = do
    Options {pwm} <- getOptions
    if (pwm == HardwarePWM) && pwmMöglich pin
        then liftIO $ do
            pinMode pin PWM_OUTPUT
            pwmSetRange pwmGrenze
            pwmWrite pin $ pwmValueModifiziert s pwmValue
        else pwmSoftwareSetzteWert pin pwmFrequenzHzNormal $ pwmValueModifiziert s pwmValue

-- | Erzeuge PWM-Funktion für einen Servo-Motor
--   Nutze SoftwarePWM für eine konstante Frequenz (sonst abhängig pwmGrenze und pwmValue)
pwmServo :: (StreckenAtom s, PwmReader r m, MonadIO m) => s -> Pin -> Natural -> m ()
pwmServo s pin =
    pwmSoftwareSetzteWert pin pwmFrequenzHzServo . pwmValueModifiziert s . erhaltePwmWertVoll

-- | newtype auf 'PwmValue' um ein noch bevorstehendes modifizieren bzgl. fließend-Value zu signalisieren
newtype PwmValueUnmodifiziert = PwmValueUnmodifiziert PwmValue

-- | Berechne den PWM-Wert abhängig davon, bei welchen 'Anschluss'-Output ('HIGH'/'LOW') der Strom fließt.
pwmValueModifiziert :: (StreckenAtom s) => s -> PwmValueUnmodifiziert -> PwmValue
pwmValueModifiziert s (PwmValueUnmodifiziert pwmValue) = case fließend s of
    HIGH -> pwmValue
    LOW -> pwmGrenze - pwmValue

-- ** Frequenzen
-- | 50 Hz Frequenz; Standard-Wert von Servo-Motoren
pwmFrequenzHzServo :: Natural
pwmFrequenzHzServo = 50

-- | Normale PWM-Frequenz
pwmFrequenzHzNormal :: Natural
pwmFrequenzHzNormal = 500

-- | Erhalte PWMValue ausgehend von einem Wert zwischen 0 und 'pwmEingabeMaximal'.
erhaltePwmWert :: (Integral i) => Natural -> i -> PwmValueUnmodifiziert
erhaltePwmWert pwmGrenzeMax wert = PwmValueUnmodifiziert $ fromIntegral ergebnis
    {-
            Verwende Natural um Fehler wegen zu kleinem Wertebereich zu vermeiden.
            Multipliziere zuerst alle Werte, bevor sie normalisiert werden um Rundungsfehler zu verhindern.
            Möglich, nachdem die Funktion nicht in Performance-kritischen Bereichen (und selten) aufgerufen wird.
            Effektivspannung skaliert wie die Wurzel des PwmValue.
            Der Eingabewert wird daher quadriert um linear mit der Effektivspannung zu skalieren.
        -}
        where
            ergebnis :: Natural
            ergebnis = div wertSkaliert (pwmEingabeMaximal * pwmEingabeMaximal)

            wertSkaliert :: Natural
            wertSkaliert = pwmGrenzeMax * wertBegrenzt * wertBegrenzt

            wertBegrenzt :: Natural
            wertBegrenzt = min pwmEingabeMaximal $ fromIntegral wert

-- | Maximaler Eingabewert für 'geschwindigkeit'.
pwmEingabeMaximal :: Natural
pwmEingabeMaximal = fromIntegral $ (maxBound :: Word8)

-- | Vollständige pwmGrenze als Natural.
pwmGrenzeVoll :: Natural
pwmGrenzeVoll = fromIntegral pwmGrenze

-- | Maximal erlaubter pwmGrenze um eine Effektivspannung von 16V zu erhalten.
pwmGrenzeReduziert :: Natural
pwmGrenzeReduziert =
    div (pwmGrenzeVoll * spannungFahrt * spannungFahrt) (spannungQuelle * spannungQuelle)
-- Effektivspannung skaliert wie die Wurzel des PwmValues.
    where
        spannungFahrt :: Natural
        spannungFahrt = 16

        spannungQuelle :: Natural
        spannungQuelle = 25

-- | Nutze komplette pwmGrenze
erhaltePwmWertVoll :: (Integral i) => i -> PwmValueUnmodifiziert
erhaltePwmWertVoll = erhaltePwmWert pwmGrenzeVoll

-- | Nutze einen reduzierten Bereich der pwmGrenze (maximal 16V von 24V Maximalspannung)
erhaltePwmWertReduziert :: (Integral i) => i -> PwmValueUnmodifiziert
erhaltePwmWertReduziert = erhaltePwmWert pwmGrenzeReduziert