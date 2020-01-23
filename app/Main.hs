{-# LANGUAGE NamedFieldPuns #-}

import System.Environment (getArgs, withArgs)
import Text.Regex.TDFA ((=~))
import qualified Zug.UI as UI

main :: IO ()
main = do
    -- Wenn genau ein Kommandozeilenargument übergeben wurde, versuche es als Datei zu laden.
    -- Damit kann man das Programm durch ziehen einer Datei auf die Binary mit einem bestimmten Anfangszustand starten.
    args <- getArgs
    argModifier args UI.main
        where
            argModifier :: [String] -> IO a -> IO a
            argModifier [filename]
                | filename =~ linuxRegex
                    = withArgs $
                        "--load" :
                            (\(_before, _match, _after, submatches) -> submatches)
                                (filename =~ linuxRegex :: (String, String, String, [String]))
                | otherwise
                    = withArgs ["--load", filename]
            argModifier _args
                = id
            -- Drag & Drop nur über .desktop-Datei möglich (raspian, nautilus window manager)
            -- "'file:///home/pi/Desktop/Zugkontrolle-bin/Doppeloval.json' "
            linuxRegex :: String
            linuxRegex = "'file://(.+)' *"