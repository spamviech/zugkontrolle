{-|
Description : Kommandozeilen-Optionen
-}
module Zug.Options (Options(..), getOptions, UI(..)) where

-- Bibliotheken
import Options.Applicative
import Data.Semigroup (Semigroup(..))

-- | Erhalte Kommandozeilen-Arguemente
getOptions :: IO Options
getOptions = execParser opts

-- | Unterstützte Kommandozeilen-Argumente
data Options = Options {
                    printCmd :: Bool,
                    ui :: UI,
                    load :: String}
                        deriving Show

-- | Unterstützte Benutzer-Schnittstellen
data UI = GTK | Cmd
            deriving (Show, Read)

opts :: ParserInfo Options
opts = info (combinedOptions <**> helper) (fullDesc <> progDesc "Kontrolliere einzelne Wegstrecken-Elemente, oder fasse sie zu Wegstrecken zusammen und kontrolliere sie gemeinsam. Erstelle Pläne zur automatischen Kontrolle (WiP)." <> header "Zugkontrolle - RaspberryPi-Anbindung einer Modelleisenbahn.")

combinedOptions :: Parser Options
combinedOptions = Options <$> printOpt <*> uiOpt <*> loadOpt

printOpt :: Parser Bool
printOpt = switch (long "print" <> short 'p' <> help "Verwende Konsolenausgabe anstelle von wiringPi.")

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