{-# LANGUAGE MonoLocalBinds #-}

{-|
Description : Starte Main-Loop für Kommandozeilen-basiertes UI.
-}
module Zug.UI.Disabled.Cmd (main) where

import Control.Monad.Trans (MonadIO())

import Zug.Options (VersionReader())
import qualified Zug.UI.Gtk as Gtk

-- | Lade per Kommandozeile übergebenen Anfangszustand und führe den main loop aus.
main :: (VersionReader r m, MonadIO m) => m ()
main = Gtk.main
