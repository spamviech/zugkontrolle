{-# LANGUAGE MonoLocalBinds #-}

{-|
Description : Erstelle GUI und starte den GTK-Main-Loop.
-}
module Zug.UI.Disabled.Gtk (main) where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))

import Zug.Language (Sprache(..))
import qualified Zug.Language as Language
import Zug.Options (VersionReader())
import qualified Zug.UI.Cmd as Cmd

-- | Gtk-main loop nicht verfügbar. Weiche auf Cmd-UI aus.
main :: (VersionReader r m, MonadIO m) => m ()
main = do
    liftIO $ putWarningLn Language.uiNichtUnterstützt
    Cmd.main

putWarningLn :: (Sprache -> Text) -> IO ()
putWarningLn warning = do
    setSGR [SetColor Foreground Vivid Red]
    Text.putStrLn $ warning Deutsch
    setSGR [Reset]
