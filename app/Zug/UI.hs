{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description : Auswahl des verwendeten UI-Funktion anhand der Kommandozeilen-Parameter
-}
module Zug.UI (main) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Text.IO as Text
-- Auto-generiertes Cabal-Modul
import qualified Paths_Zugkontrolle as Paths
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))

import qualified Zug.Language as Language
import Zug.Options (getOptions, Options(..), UI(..), PWM(SoftwarePWM, HardwarePWM), VersionReader())
#ifdef ZUGKONTROLLESILENCE
import qualified Zug.UI.Disabled.Cmd as Cmd
#endif
#ifndef ZUGKONTROLLEGUI
import qualified Zug.UI.Disabled.Gtk as Gtk
#endif
#ifndef ZUGKONTROLLESILENCE
import qualified Zug.UI.Cmd as Cmd
#endif
#ifdef ZUGKONTROLLEGUI
import qualified Zug.UI.Gtk as Gtk
#endif
import Zug.Util (isNonRaspiOrRoot)

-- | Wähle das per Kommandozeilen-Parameter gewählte Nutzer-Interface und starte dessen main loop.
--
-- Falls es nicht verfügbar ist, starte stattdessen den Cmd-UI main loop.
main :: IO ()
main = flip runReaderT Paths.version $ ausführenWennRoot $ do
    Options {ui} <- getOptions
    case ui of
        Gtk -> Gtk.main
        Cmd -> Cmd.main

ausführenWennRoot :: (VersionReader r m, MonadIO m) => m () -> m ()
ausführenWennRoot action = do
    (Options {pwm, sprache}) <- getOptions
    case pwm of
        SoftwarePWM -> action
        HardwarePWM -> do
            root <- isNonRaspiOrRoot
            if root
                then action
                else liftIO $ do
                    setSGR [SetColor Foreground Vivid Red]
                    Text.putStrLn $ Language.nichtRoot sprache
                    setSGR [Reset]
--
