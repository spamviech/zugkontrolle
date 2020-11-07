{-# LANGUAGE LambdaCase #-}

{-|
Description : Speichern und laden
-}
module Zug.UI.Save
  ( -- * Speichern & Laden
    speichern
  , laden
  ) where

import Data.Aeson.Types (ToJSON())
import Data.Yaml (encodeFile, decodeFileEither)
import System.Directory (doesFileExist)

import Zug.Language (Sprache())
import Zug.Objekt (ObjektKlasse())
import Zug.UI.Base (StatusAllgemein(..), Status)

-- | Speichere aktuellen Zustand in Datei.
speichern :: (ObjektKlasse o, ToJSON o) => StatusAllgemein o -> FilePath -> IO ()
speichern = flip encodeFile

-- | Lade Zustand aus Datei.
--
-- Dateifehler und nicht-existente Dateien geben 'Nothing' zurück.
-- Ansonsten wird ein aus einem 'Status' konstruierter Typ zurückgegeben.
laden :: FilePath -> (Status -> IO s) -> Sprache -> IO (Maybe s)
laden path fromStatus sp = do
    fileExists <- doesFileExist path
    if fileExists
        then decodeFileEither path >>= \case
            (Left _error) -> pure Nothing
            (Right f) -> fmap Just $ fromStatus $ f sp
        else pure Nothing
