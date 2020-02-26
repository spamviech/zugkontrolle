module Main (main) where

-- Bibliotheken
import System.Environment (getArgs, withArgs)

-- Abhängigkeit von anderen Modulen
import qualified Zug.UI as UI

main :: IO ()
main = do
    -- Wenn genau ein Kommandozeilenargument übergeben wurde, versuche es als Datei zu laden.
    -- Damit kann man das Programm durch ziehen einer Datei auf die Binary mit einem bestimmten Anfangszustand starten.
    args <- getArgs
    argModifier args UI.main
    where
        argModifier :: [String] -> IO a -> IO a
        argModifier [filename] = withArgs ["--load", filename]
        argModifier _args = id