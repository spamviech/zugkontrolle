{-|
Description : Kommandozeilen-Optionen
-}
module Zug.Options
  ( Options(..)
  , getOptions
  , UI(..)
  , alleUI
  , PWM(..)
  , allePWMOptionen
  , Sprache(..)
  , alleSprachen
  ) where

-- Bibliotheken
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Semigroup (Semigroup(..))
import Data.Text (unpack)
import Options.Applicative
       (ParserInfo(), Parser(), execParser, info, helper, fullDesc, progDesc, header, infoOption
      , long, short, help, switch, option, auto, metavar, showDefault, value, strOption)

-- Abhängigkeit von anderen Modulen
import Zug.Language (Sprache(..), alleSprachen)
import qualified Zug.Language as Language

-- | Erhalte Kommandozeilen-Argumente
getOptions :: (MonadIO m) => m Options
getOptions = liftIO $ execParser optionen

-- | Unterstützte Kommandozeilen-Argumente
data Options =
    Options
    { printCmd :: Bool
    , ui :: UI
    , sprache :: Sprache
    , load :: String
    , pwm :: PWM
    }
    deriving (Show)

optionen :: ParserInfo Options
optionen =
    info
        (helper <*> versionOpt <*> kombinierteOptionen)
        (fullDesc
         <> progDesc
             "Kontrolliere einzelne StreckenObjekte, oder fasse sie zu Wegstrecken zusammen und kontrolliere sie gemeinsam. Erstelle Pläne zur automatischen Kontrolle."
         <> header "Zugkontrolle - RaspberryPi-Anbindung einer Modelleisenbahn.")

versionOpt :: Parser (a -> a)
versionOpt =
    infoOption
        ("Zugkontrolle Version: " ++ unpack Language.version)
        (long "version" <> short 'v' <> help "Zeige die aktuelle Version an.")

kombinierteOptionen :: Parser Options
kombinierteOptionen = Options <$> printOpt <*> uiOpt <*> spracheOpt <*> ladeOpt <*> pwmOpt

printOpt :: Parser Bool
printOpt =
    switch (long "print" <> short 'p' <> help "Verwende Konsolenausgabe anstelle von wiringPi.")

-- | Unterstützte Benutzer-Schnittstellen
data UI
    = Gtk
    | Cmd
    deriving (Show, Read, Enum, Bounded, Eq)

-- | Alle unterstützten UI-Optionen
alleUI :: [UI]
alleUI = [minBound .. maxBound]

uiOpt :: Parser UI
uiOpt =
    option
        auto
        (long "ui"
         <> metavar "UI"
         <> showDefault
         <> value Gtk
         <> help ("Verwende UI=" ++ zeigeMöglichkeiten alleUI ++ " als Benutzer-Schnittstelle."))

ladeOpt :: Parser String
ladeOpt =
    strOption
        (long "load"
         <> short 'l'
         <> metavar "DATEINAME"
         <> showDefault
         <> value ""
         <> help "Lade DATEINAME zum Programmstart.")

-- | Verwende Hardware-PWM wenn möglich?
data PWM
    = SoftwarePWM
    | HardwarePWM
    deriving (Show, Read, Enum, Bounded, Eq)

-- | Alle unterstützten PWM-Optionen
allePWMOptionen :: [PWM]
allePWMOptionen = [minBound .. maxBound]

pwmOpt :: Parser PWM
pwmOpt =
    option
        auto
        (long "pwm"
         <> metavar "PWMTYP"
         <> showDefault
         <> value SoftwarePWM
         <> help ("Verwende PWMTYP=" ++ zeigeMöglichkeiten allePWMOptionen ++ " wenn möglich."))

spracheOpt :: Parser Sprache
spracheOpt =
    option
        auto
        (long "sprache"
         <> metavar "SPRACHE"
         <> showDefault
         <> value Deutsch
         <> help
             ("Welche Sprache (" ++ zeigeMöglichkeiten alleSprachen ++ ") soll verwendet werden?"))

-- | Hilfsfunktion um mögliche Optionen anzuzeigen
zeigeMöglichkeiten :: (Show a) => [a] -> String
zeigeMöglichkeiten [] = ""
zeigeMöglichkeiten [h] = show h
zeigeMöglichkeiten (h:t) = show h ++ '|' : zeigeMöglichkeiten t