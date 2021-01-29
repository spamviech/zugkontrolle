{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description : Kommandozeilen-Optionen.
-}
module Zug.Options
  ( Options(..)
  , getOptions
  , VersionReader(..)
  , MitVersion(..)
  , UI(..)
  , alleUI
  , PWM(..)
  , allePWMOptionen
  , Sprache(..)
  , alleSprachen
  , GtkSeiten(..)
  , alleGtkSeiten
  ) where

import Control.Monad.Reader.Class (MonadReader(), asks)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Semigroup (Semigroup((<>)))
import Data.Text (unpack)
import Data.Version (Version)
import Options.Applicative
       (ParserInfo(), Parser(), execParser, info, helper, fullDesc, progDesc, header, infoOption
      , long, short, help, switch, option, auto, metavar, showDefault, value, strOption)

import Zug.Language (Sprache(..), alleSprachen)
import qualified Zug.Language as Language

class MitVersion r where
    version :: r -> Version

instance MitVersion Version where
    version :: Version -> Version
    version = id

class (MitVersion r) => VersionReader r m | m -> r where
    erhalteVersion :: m Version

instance (MitVersion r, MonadReader r m) => VersionReader r m where
    erhalteVersion :: m Version
    erhalteVersion = asks version

-- | Erhalte Kommandozeilen-Argumente.
getOptions :: (MonadIO m, VersionReader r m) => m Options
getOptions = do
    v <- erhalteVersion
    liftIO $ execParser $ optionen v

-- | Unterstützte Kommandozeilen-Argumente.
data Options =
    Options
    { printCmd :: Bool
    , ui :: UI
    , sprache :: Sprache
    , load :: FilePath
    , pwm :: PWM
    , gtkSeiten :: GtkSeiten
    }
    deriving (Show)

optionen :: Version -> ParserInfo Options
optionen v =
    info (helper <*> versionOpt v <*> kombinierteOptionen)
    $ fullDesc
    <> progDesc
        "Kontrolliere einzelne StreckenObjekte, oder fasse sie zu Wegstrecken zusammen und kontrolliere sie gemeinsam. Erstelle Pläne zur automatischen Kontrolle."
    <> header "Zugkontrolle - RaspberryPi-Anbindung einer Modelleisenbahn."

versionOpt :: Version -> Parser (a -> a)
versionOpt v =
    infoOption ("Zugkontrolle Version: " ++ unpack (Language.version v))
    $ long "version" <> short 'v' <> help "Zeige die aktuelle Version an."

kombinierteOptionen :: Parser Options
kombinierteOptionen =
    Options <$> printOpt <*> uiOpt <*> spracheOpt <*> ladeOpt <*> pwmOpt <*> gtkSeitenOpt

printOpt :: Parser Bool
printOpt =
    switch $ long "print" <> short 'p' <> help "Verwende Konsolenausgabe anstelle von wiringPi."

-- | Unterstützte Benutzer-Schnittstellen.
data UI
    = Gtk
    | Cmd
    deriving (Show, Read, Enum, Bounded, Eq)

-- | Alle unterstützten UI-Optionen.
alleUI :: [UI]
alleUI = [minBound .. maxBound]

uiOpt :: Parser UI
uiOpt =
    option auto
    $ long "ui"
    <> metavar "UI"
    <> showDefault
    <> value Gtk
    <> help ("Verwende UI=" ++ zeigeMöglichkeiten alleUI ++ " als Benutzer-Schnittstelle.")

ladeOpt :: Parser String
ladeOpt =
    strOption
    $ long "load"
    <> short 'l'
    <> metavar "DATEINAME"
    <> showDefault
    <> value ""
    <> help "Lade DATEINAME zum Programmstart."

-- | Verwende Hardware-PWM wenn möglich?
data PWM
    = SoftwarePWM
    | HardwarePWM
    deriving (Show, Read, Enum, Bounded, Eq)

-- | Alle unterstützten PWM-Optionen.
allePWMOptionen :: [PWM]
allePWMOptionen = [minBound .. maxBound]

pwmOpt :: Parser PWM
pwmOpt =
    option auto
    $ long "pwm"
    <> metavar "PWMTYP"
    <> showDefault
    <> value SoftwarePWM
    <> help ("Verwende PWMTYP=" ++ zeigeMöglichkeiten allePWMOptionen ++ " wenn möglich.")

spracheOpt :: Parser Sprache
spracheOpt =
    option auto
    $ long "sprache"
    <> metavar "SPRACHE"
    <> showDefault
    <> value Deutsch
    <> help ("Welche Sprache (" ++ zeigeMöglichkeiten alleSprachen ++ ") soll verwendet werden?")

-- | Soll im 'Gtk'-UI jede Kategorie eine eigene Seite bekommen?
data GtkSeiten
    = Einzelseiten
    | Sammelseiten
    deriving (Show, Read, Enum, Bounded, Eq)

-- | Alle unterstützen Seiten-Konfigurationen im Gtk-UI.
alleGtkSeiten :: [GtkSeiten]
alleGtkSeiten = [minBound .. maxBound]

gtkSeitenOpt :: Parser GtkSeiten
gtkSeitenOpt =
    option auto
    $ long "seiten"
    <> metavar "KONFIGURATION"
    <> showDefault
    <> value Sammelseiten
    <> help
        ("Soll im Gtk-UI jede Kategorie eine eigene Seite bekommen ("
         ++ zeigeMöglichkeiten alleGtkSeiten
         ++ ")? Diese Option hat keine Auswirkung, wenn dass Cmd-UI verwendet wird.")

-- | Hilfsfunktion um mögliche Optionen anzuzeigen.
zeigeMöglichkeiten :: (Show a) => [a] -> String
zeigeMöglichkeiten [] = ""
zeigeMöglichkeiten [h] = show h
zeigeMöglichkeiten (h:t) = show h ++ '|' : zeigeMöglichkeiten t
