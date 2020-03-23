{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

{-|
Description : Auswahl des verwendeten UI-Funktion anhand der Kommandozeilen-Parameter
-}
module Zug.UI (main) where

#ifdef ZUGKONTROLLERASPI
import qualified Data.Text.IO as Text
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))
import System.Posix.User (getRealUserID)
#endif

import Zug.Anbindung (Wartezeit(MilliSekunden))
#ifdef ZUGKONTROLLERASPI
import qualified Zug.Language as Language
#endif
#ifdef ZUGKONTROLLERASPI
import Zug.Options (getOptions, Options(..), UI(..), PWM(SoftwarePWM, HardwarePWM))
#else
import Zug.Options (getOptions, Options(..), UI(..))
#endif
import qualified Zug.UI.Cmd as Cmd
import qualified Zug.UI.Gtk as Gtk

-- | Wähle das per Kommandozeilen-Parameter gewählte Nutzer-Interface und starte dessen main loop.
--
-- Falls es nicht verfügbar ist, starte stattdessen den Cmd-UI main loop.
main :: IO ()
main = ausführenWennRoot $ do
    Options {ui} <- getOptions
    case ui of
        Gtk -> Gtk.main i2cRefreshRate
        Cmd -> Cmd.main i2cRefreshRate

i2cRefreshRate :: Wartezeit
i2cRefreshRate = MilliSekunden 500

ausführenWennRoot :: IO () -> IO ()

#ifdef ZUGKONTROLLERASPI
ausführenWennRoot action = do
    (Options {pwm, sprache}) <- getOptions
    case pwm of
        SoftwarePWM -> action
        HardwarePWM -> do
            uid <- getRealUserID
            if (uid == 0)
                then action
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    Text.putStrLn $ Language.nichtRoot sprache
                    setSGR [Reset]
#else
ausführenWennRoot action = action
#endif
--