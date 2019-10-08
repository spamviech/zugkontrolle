{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

{-|
Description : Auswahl des verwendeten UI-Funktion anhand der Kommandozeilen-Parameter
-}
module Zug.UI (main) where

#if linux_HOST_OS && arm_HOST_ARCH
#define ZUGKONTROLLERASPI 1
#endif

-- Abhängigkeiten von anderen Modulen
import Zug.Options (
    getOptions, Options(..), UI(..)
#ifdef ZUGKONTROLLERASPI
    , PWM(SoftwarePWM, HardwarePWM)
#endif
    )
import qualified Zug.UI.Cmd as Cmd
import qualified Zug.UI.Gtk as Gtk
#ifdef ZUGKONTROLLERASPI
-- Überprüfe, ob das Programm mit Root-Rechten aufgeführt wird
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))
import System.Posix.User (getRealUserID)
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language
#endif

-- | Wähle das per Kommandozeilen-Parameter gewählte Nutzer-Interface und starte dessen main loop.
-- 
-- Falls es nicht verfügbar ist, starte stattdessen den Cmd-UI main loop.
main :: IO ()
main = ausführenWennRoot $ do
    Options {ui} <- getOptions
    case ui of
        Gtk -> GTK.main
        Cmd -> Cmd.main

ausführenWennRoot :: IO () -> IO ()
#ifdef ZUGKONTROLLERASPI
ausführenWennRoot action = do
    (Options {pwm}) <- getOptions
    case pwm of
        SoftwarePWM -> action
        HardwarePWM -> do
            uid <- getRealUserID
            if (uid == 0)
                then action
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn Language.nichtRoot
                    setSGR [Reset]
#else
ausführenWennRoot action = action
#endif