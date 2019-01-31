{-# LANGUAGE InstanceSigs, OverloadedStrings, DuplicateRecordFields #-}

-- Bibliotheken
import GHC.Exts(IsString(..))
import Data.Semigroup(Semigroup(..))
import qualified Data.Text as T
import Data.Text (Text, unpack)
-- Bessere Konsolenausgabe
import System.Console.ANSI
-- Abhängigkeiten von anderen Modulen
import Zug.UI.Befehl
import qualified Zug.UI.Language as Language
import Zug.UI.Language ((<~>))
import Zug.UI.Cmd ()
import Zug.UI.Cmd.Parser (QErgebnis(..), BefehlSofort(..), QBefehl(..), parser)
import Zug.UI.Cmd.Lexer (lexer)
import Zug.Anbindung
import Zug.Klassen

main :: IO ()
main = do
    parseBefehl
    parseQuery
    parseBefehlSofort
    parseQBefehl

-- Parser für Eingabe von vollständigen Befehlen testen
parseBefehl :: IO ()
parseBefehl = do
    testMany "Befehl" [
        (Language.beenden, QEBefehl (UI Beenden)),
        (Language.abbrechen, QEBefehl (UI Abbrechen)),
        (Language.hinzufügen <~> Language.abbrechen, QEBefehl (UI Abbrechen)),
        (Language.hinzufügen <~> Language.bahngeschwindigkeit <~> Language.abbrechen, QEBefehl (UI Abbrechen)),
        (Language.hinzufügen <~> Language.bahngeschwindigkeit <~> "Name" <~> Language.beenden, QEBefehl (UI Beenden)),
        (Language.hinzufügen <~> Language.bahngeschwindigkeit <~> "Name" <~> "3", (QEBefehl (Hinzufügen (OBahngeschwindigkeit (MärklinBahngeschwindigkeit {name="Name", geschwindigkeitsPin=toPin 3}))))),
        (Language.hinzufügen <~> Language.kupplung <~> "Name" <~> "22", (QEBefehl (Hinzufügen (OKupplung (Kupplung {name="Name", kupplungsPin=toPin 22}))))),
        (Language.hinzufügen <~> Language.streckenabschnitt <~> "Name" <~> "3245", (QEBefehl (Hinzufügen (OStreckenabschnitt (Streckenabschnitt {name="Name", stromPin=toPin 3245}))))),
        (Language.hinzufügen <~> Language.weiche <~> "Name" <~> "1" <~> Language.rechts <~> "2", (QEBefehl (Hinzufügen (OWeiche (MärklinWeiche {name="Name", richtungsPins=[(toPin 2, Rechts)]}))))),
        (Language.hinzufügen <~> Language.weiche <~> "Name" <~> "2" <~> Language.gerade <~> "2" <~> Language.kurve <~> "-4", (QEBefehl (Hinzufügen (OWeiche (MärklinWeiche {name="Name", richtungsPins=[(toPin (-4), Kurve), (toPin 2, Gerade)]}))))),
        ((T.take 4 Language.hinzufügen) <~> Language.weiche <~> "Name" <~> "2" <~> Language.gerade <~> "2" <~> Language.kurve <~> "-4", (QEBefehl (Hinzufügen (OWeiche (MärklinWeiche {name="Name", richtungsPins=[(toPin (-4), Kurve), (toPin 2, Gerade)]}))))),
        (Language.speichern <~> "Dateiname", (QEBefehl (Speichern "Dateiname")))]
    setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite Befehl not yet fully implemented" >> setSGR [Reset]


eqShow :: (Show a, Show b) => a -> b -> Bool
eqShow x y = (show x) == (show y)

infix 1 <==>
(<==>) :: (Show a) => a -> a -> Bool
(<==>) = eqShow

-- Parser für Eingabe Testen, die eine Statusabfrage benötigt
parseQuery :: IO ()
parseQuery = setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite Query not yet implemented" >> setSGR [Reset]

-- Parser für Eingabe Testen, die eine sofortige ausführung benötigt
parseBefehlSofort :: IO ()
parseBefehlSofort = setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite BefehlSofort not yet implemented" >> setSGR [Reset]

-- Parser für Eingabe von unvollständigen Befehlen testen
parseQBefehl :: IO ()
parseQBefehl = setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite QBefehl not yet implemented" >> setSGR [Reset]

testMany :: String -> [(Text, QErgebnis)] -> IO ()
testMany name testListe = putStrLn (name <> "\n" <> map (\_ -> '=') name) >> mapM_ (uncurry test) testListe

test :: Text -> QErgebnis -> IO ()
test eingabe sollErgebnis = do
    putStr "Eingabe:\t"
    setSGR [SetColor Foreground Dull Cyan]
    putStrLn $ unpack eingabe
    setSGR [Reset]
    putStr "Ergebnis:\t"
    setSGR [SetColor Foreground Dull Blue]
    let istErgebnis = (snd . (parser QBefehl) . lexer . T.words $ eingabe)
    putStrLn $ show istErgebnis
    setSGR [Reset]
    putStr "Erwartet:\t"
    setSGR [SetColor Foreground Dull Green]
    putStrLn $ show sollErgebnis
    setSGR [Reset]
    setSGR [SetColor Foreground Vivid White] >> if istErgebnis <==> sollErgebnis
        then setSGR [SetColor Background Dull Green] >> putStrLn "Richtig"
        else setSGR [SetColor Background Dull Red] >> putStrLn "Falsch"
    setSGR [Reset]
    putStrLn $ "------------------"

instance Show QErgebnis where
    show :: QErgebnis -> String
    show    (QEBefehl (UI Beenden))                                             = "Beenden"
    show    (QEBefehl (UI Abbrechen))                                           = "Abbrechen"
    show    (QEBefehl (Hinzufügen objekt))                                      = "Hinzufügen " <> show objekt
    show    (QEBefehl (Entfernen objekt))                                       = "Entfernen " <> show objekt
    show    (QEBefehl (Speichern dateipfad))                                    = "Speichern " <> dateipfad
    show    (QEBefehl (Laden dateipfad _fehlerbehandlung))                      = "Laden " <> dateipfad
    show    (QEBefehl (Ausführen plan))                                         = "Ausführen " <> show plan
    show    (QEBefehl (AktionBefehl aktion))                                    = "Aktion " <> show aktion
    show    (QEBefehlSofort (BSLaden dateipfad) eingabeRest)                    = "Laden " <> dateipfad <> "=>" <> show eingabeRest
    show    (QEBefehlQuery qObjektIOStatus eitherF fallback eingabeRest)        = "Query " <> show qObjektIOStatus <> konstruktor eitherF <> "=>" <> show eingabeRest
        where
            konstruktor :: Either a b -> String
            konstruktor (Left _qKonstruktor)    = " Konstruktor "
            konstruktor (Right _konstruktor)    = " qKonstruktor "
    show    (QEQBefehl qBefehl)                                                 = show qBefehl

instance Show Objekt where
    show :: Objekt -> String
    show    (OPlan plan)                                = show plan
    show    (OWegstrecke wegstrecke)                    = show wegstrecke
    show    (OWeiche weiche)                            = show weiche
    show    (OBahngeschwindigkeit bahngeschwindigkeit)  = show bahngeschwindigkeit
    show    (OStreckenabschnitt streckenabschnitt)      = show streckenabschnitt
    show    (OKupplung kupplung)                        = show kupplung