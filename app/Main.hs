{-# LANGUAGE NamedFieldPuns #-}

import System.Environment (getArgs, withArgs)
import Text.Regex.TDFA ((=~))
import qualified Zug.UI as UI

main :: IO ()
main = do
    -- Wenn nur ein Kommandozeilenargument übergeben wurde, welches nicht mit '-' beginnt, versuche es als Datei zu laden.
    -- Damit kann das Programm durch ziehen einer .json-Datei auf die Executable mit einem bestimmten Anfangszustand geladen werden.
    args <- getArgs
    argModifier args UI.main
        where
            argModifier :: [String] -> IO a -> IO a
            argModifier
                ['-' : _arg]
                    = id
            argModifier
                [filename]
                    | filename =~ linuxRegex
                        = withArgs $
                            "--load" :
                                (\(_before, _match, _after, submatches) -> submatches)
                                    (filename =~ linuxRegex :: (String, String, String, [String]))
                | otherwise
                    = withArgs ["--load", filename]
            argModifier
                _args
                    = id
            -- Drag & Drop nur über .desktop-Datei möglich (raspian, nautilus window manager)
            -- "'file:///home/pi/Desktop/Zugkontrolle-bin/Doppeloval.json' "
            linuxRegex :: String
            linuxRegex = "'file://(.+)' *"