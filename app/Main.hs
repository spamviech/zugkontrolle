{-# LANGUAGE NamedFieldPuns #-}

import System.Environment (getArgs, withArgs)
import qualified Zug.UI as UI

main :: IO ()
main = do
    -- Wenn nur ein Kommandozeilenargument übergeben wurde, welches nicht mit '-' beginnt, versuche es als Datei zu laden.
    -- Damit kann das Programm durch ziehen einer .json-Datei auf die Executable mit einem bestimmten Anfangszustand geladen werden.
    args <- getArgs
    argModifier args UI.main
        where
            argModifier :: [String] -> IO a -> IO a
            argModifier (('-':_arg):([]))   = id
            argModifier (arg:[])            = withArgs ["--load=" ++ arg]
            argModifier _args               = id