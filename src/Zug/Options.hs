{-|
Description : Kommandozeilen-Optionen
-}
module Zug.Options (Options(..), getOptions, UI(..), PWM(..)) where

-- Bibliotheken
import Options.Applicative
import Data.Semigroup (Semigroup(..))
import System.Hardware.WiringPi

-- | Erhalte Kommandozeilen-Arguemente
getOptions :: IO Options
getOptions = execParser opts

-- | Unterstützte Kommandozeilen-Argumente
data Options = Options {
                    printCmd :: Bool,
                    ui :: UI,
                    load :: String,
                    pwm :: PWM,
                    fließend :: Value}
                        deriving Show

opts :: ParserInfo Options
opts = info (combinedOptions <**> helper) (fullDesc <> progDesc "Kontrolliere einzelne Wegstrecken-Elemente, oder fasse sie zu Wegstrecken zusammen und kontrolliere sie gemeinsam. Erstelle Pläne zur automatischen Kontrolle." <> header "Zugkontrolle - RaspberryPi-Anbindung einer Modelleisenbahn.")

combinedOptions :: Parser Options
combinedOptions = Options <$> printOpt <*> uiOpt <*> loadOpt <*> pwmOpt <*> fließendOpt

printOpt :: Parser Bool
printOpt = switch (long "print" <> short 'p' <> help "Verwende Konsolenausgabe anstelle von wiringPi.")

-- | Unterstützte Benutzer-Schnittstellen
data UI = GTK | Cmd
            deriving (Show, Read)

uiOpt :: Parser UI
uiOpt = option auto (
                long "ui" <>
                metavar "UI" <>
                showDefault <>
                value GTK <>
                help "Verwende UI als Benutzer-Schnittstelle.")

loadOpt :: Parser String
loadOpt = strOption (
                long "load" <>
                short 'l' <>
                metavar "DATEINAME" <>
                showDefault <>
                value "" <>
                help "Lade DATEINAME zum Programmstart.")

-- | Verwende Hardware-PWM wenn möglich?
data PWM = SoftwarePWM | HardwarePWM
                deriving (Show, Read)

pwmOpt :: Parser PWM
pwmOpt = option auto (
            long "pwm" <>
            metavar "PWMTYP" <>
            showDefault <>
            value HardwarePWM <>
            help "Verwende Hardware-PWM wenn möglich?")

fließendOpt :: Parser Value
fließendOpt = option auto (
                long "on" <>
                metavar "VALUE" <>
                showDefault <>
                value LOW <>
                help "Bei welchem Pin-Output fließt der Strom (HardwarePWM unbeeinflusst)?")