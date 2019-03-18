{-# LANGUAGE CPP #-}

{-|
Description : Kommandozeilen-Optionen
-}
module Zug.Options (Options(..), getOptions, UI(..), PWM(..)) where

-- Bibliotheken
import Options.Applicative
import Data.Semigroup (Semigroup(..))
import System.Hardware.WiringPi (Value(..))
-- Abhängigkeit von anderen Modulen
import Zug.Language ((<~>), (<:>))
import qualified Zug.Language as Language

-- | Erhalte Kommandozeilen-Arguemente
getOptions :: IO Options
getOptions = execParser optionen

-- | Unterstützte Kommandozeilen-Argumente
data Options = Options {
                    printCmd :: Bool,
                    ui :: UI,
                    load :: String,
                    pwm :: PWM,
                    fließend :: Value}
                        deriving Show

optionen :: ParserInfo Options
optionen = info
        (helper <*> versionOpt <*> kombinierteOptionen)
        (fullDesc <>
            progDesc "Kontrolliere einzelne StreckenObjekte, oder fasse sie zu Wegstrecken zusammen und kontrolliere sie gemeinsam. Erstelle Pläne zur automatischen Kontrolle." <>
            header "Zugkontrolle - RaspberryPi-Anbindung einer Modelleisenbahn.")

versionOpt :: Parser (a -> a)
versionOpt = infoOption (Language.zugkontrolle <~> "Version" <:> ZUGKONTROLLEVERSION) (long "version" <> short 'v' <> help "Zeige die aktuelle Version an.")

kombinierteOptionen :: Parser Options
kombinierteOptionen = Options <$> printOpt <*> uiOpt <*> ladeOpt <*> pwmOpt <*> fließendOpt

printOpt :: Parser Bool
printOpt = switch (long "print" <> short 'p' <> help "Verwende Konsolenausgabe anstelle von wiringPi.")

-- | Unterstützte Benutzer-Schnittstellen
data UI = GTK | Cmd
            deriving (Show, Read, Enum, Bounded)

uiOpt :: Parser UI
uiOpt = option auto (
                long "ui" <>
                metavar "UI" <>
                showDefault <>
                value GTK <>
                help ("Verwende UI=" ++ zeigeMöglichkeiten ([minBound..maxBound] :: [UI]) ++ " als Benutzer-Schnittstelle."))

ladeOpt :: Parser String
ladeOpt = strOption (
                long "load" <>
                short 'l' <>
                metavar "DATEINAME" <>
                showDefault <>
                value "" <>
                help "Lade DATEINAME zum Programmstart.")

-- | Verwende Hardware-PWM wenn möglich?
data PWM = SoftwarePWM | HardwarePWM
                deriving (Show, Read, Enum, Bounded)

pwmOpt :: Parser PWM
pwmOpt = option auto (
            long "pwm" <>
            metavar "PWMTYP" <>
            showDefault <>
            value SoftwarePWM <>
            help ("Verwende PWMTYP=" ++ zeigeMöglichkeiten ([minBound..maxBound] :: [PWM]) ++ " wenn möglich."))

fließendOpt :: Parser Value
fließendOpt = option auto (
                long "fließend" <>
                metavar "VALUE" <>
                showDefault <>
                value LOW <>
                help ("Bei welchem Pin-Output VALUE=" ++ zeigeMöglichkeiten ([minBound..maxBound] :: [Value]) ++ " fließt der Strom (PWM-Ausgabe unbeeinflusst)."))

-- | Hilfsfunktion um mögliche Optionen anzuzeigen
zeigeMöglichkeiten :: (Show a) => [a] -> String
zeigeMöglichkeiten  ([])    = ""
zeigeMöglichkeiten  (h:[])  = show h
zeigeMöglichkeiten  (h:t)   = show h ++ '|' : zeigeMöglichkeiten t