{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

{-|
Description : Auswahl des verwendeten UI-Funktion anhand der Kommandozeilen-Parameter
-}
module Zug.UI (main) where

#if linux_HOST_OS && arm_HOST_ARCH
#define ZUGKONTROLLERASPI 1
#endif

-- Bibliotheken
import Zug.Options
import qualified Zug.UI.Cmd as Cmd
import qualified Zug.UI.GTK as GTK
#ifdef ZUGKONTROLLERASPI
-- Überprüfe, ob das Programm mit Root-Rechten aufgeführt wird
import System.Console.ANSI
import System.Posix.User
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language
#endif

-- | Wähle das per Kommandozeilen-Parameter gewählte Nutzer-Interface und starte dessen main loop.
-- 
-- Falls es nicht verfügbar ist, starte stattdessen den Cmd-UI main loop.
main :: IO ()
main = whenRoot $ do
    (Options {ui}) <- getOptions
    case ui of
        GTK -> GTK.main
        Cmd -> Cmd.main

whenRoot :: IO () -> IO ()
#ifdef ZUGKONTROLLERASPI
whenRoot action = do
    (Options {pwm}) <- getOptions
    case pwm of
        (SoftwarePWM)   -> action
        (HardwarePWM)   -> do
            uid <- getRealUserID
            if (uid == 0)
                then action
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn Language.nichtRoot
                    setSGR [Reset]
#else
whenRoot action = action
#endif