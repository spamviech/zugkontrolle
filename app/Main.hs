module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs, withArgs)

import qualified Zug.UI as UI

main :: IO ()
main = do
    -- Wenn genau ein Kommandozeilenargument übergeben wurde, versuche es als Datei zu laden.
    -- Damit kann man das Programm durch ziehen einer Datei auf die Binary mit einem bestimmten Anfangszustand starten.
    args <- getArgs
    argModifier <- getArgModifier args
    withArgs argModifier UI.main
    where
        getArgModifier :: [String] -> IO [String]
        getArgModifier [singleArg] = do
            isFile <- doesFileExist singleArg
            pure
                $ if isFile
                    then ["--load", singleArg]
                    else [singleArg]
        getArgModifier args = pure args