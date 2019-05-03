{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-
    Ein Großteil der Tests ist noch nicht fertig.
    Mir ist wegen Krankheit leider die Zeit ausgegangen.
-}

-- Bibliotheken
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup(Semigroup(..))
import qualified Data.Text as T
import Data.Text (Text, unpack)
-- Bessere Konsolenausgabe
import System.Console.ANSI
-- Test-Suite
import Test.Hspec
-- Abhängigkeiten von anderen Modulen
import Zug.Warteschlange
import Zug.LinkedMVar
import Zug.UI.Befehl
import qualified Zug.Language as Language
import Zug.Language ((<~>))
import Zug.UI.Cmd ()
import Zug.UI.Cmd.Parser (AnfrageErgebnis(..), BefehlSofort(..), AnfrageBefehl(..), parser)
import Zug.UI.Cmd.Lexer (lexer)
import Zug.Anbindung
import Zug.Plan
import Zug.Klassen

main :: IO ()
main = do
    seQueueTests
    linkedMVarTests
    parseBefehl
    parseQuery
    parseBefehlSofort
    parseQBefehl

-- Tests für Warteschlange
seQueueTests :: IO ()
seQueueTests = setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite Warteschlange not yet implemented" >> setSGR [Reset]

-- Tests für LinkedMVar
linkedMVarTests :: IO ()
linkedMVarTests = do
    -- LinkedMVar ohne Update-Funktion
    unlinked <- newUnlinkedMVar (0 :: Word)
    modifyLinkedMVar_ unlinked $ pure . succ
    unlinkedRes <- readLinkedMVar unlinked
    testResult "0 >>= modifyUnlinkedMVar_ unlinked $ pure . succ" unlinkedRes 1
    -- LinkedMVar mit pure als Update-Funktion
    linkedPure <- newLinkedMVar pure (0 :: Int)
    modifyLinkedMVar_ linkedPure $ pure . succ
    linkedPureRes <- readLinkedMVar linkedPure
    testResult "0 >>= modifyUnlinkedMVar_ linkedPure $ pure . succ" linkedPureRes 1
    -- LinkedMVar mit pure . const 0 als Update-Funktion
    linkedConst0 <- newLinkedMVar (pure . const 0) (0 :: Float)
    modifyLinkedMVar_ linkedConst0 $ pure . succ
    linkedConst0Res <- readLinkedMVar linkedConst0
    testResult "0 >>= modifyUnlinkedMVar_ linkedConst0 $ pure . succ" linkedConst0Res 0

-- Parser für Eingabe von vollständigen Befehlen testen
parseBefehl :: IO ()
parseBefehl = do
    testMany "Befehl" [
        (Language.beenden, AEBefehl (UI Beenden)),
        (Language.abbrechen, AEBefehl (UI Abbrechen)),
        (Language.hinzufügen <~> Language.abbrechen, AEBefehl (UI Abbrechen)),
        (Language.hinzufügen <~> Language.bahngeschwindigkeit <~> Language.abbrechen, AEBefehl (UI Abbrechen)),
        (Language.hinzufügen <~> Language.bahngeschwindigkeit <~> Language.märklin <~> "Name" <~> Language.beenden, AEBefehl (UI Beenden)),
        (Language.hinzufügen <~> Language.bahngeschwindigkeit <~> Language.märklin <~> "Name" <~> Language.high <~> "3", (AEBefehl (Hinzufügen (OBahngeschwindigkeit (MärklinBahngeschwindigkeit {bgName="Name", bgFließend=HIGH, geschwindigkeitsPin=zuPin 3}))))),
        (Language.hinzufügen <~> Language.kupplung <~> "Name" <~> Language.low <~> "22", (AEBefehl (Hinzufügen (OKupplung (Kupplung {kuName="Name", kuFließend=LOW, kupplungsPin=zuPin 22}))))),
        (Language.hinzufügen <~> Language.streckenabschnitt <~> "Name" <~> Language.low <~> "3245", (AEBefehl (Hinzufügen (OStreckenabschnitt (Streckenabschnitt {stName="Name", stFließend=LOW, stromPin=zuPin 3245}))))),
        (Language.hinzufügen <~> Language.weiche <~> Language.märklin <~> "Name" <~> Language.low <~> "1" <~> Language.rechts <~> "2", (AEBefehl (Hinzufügen (OWeiche (MärklinWeiche {weName="Name", weFließend=LOW, richtungsPins=(Rechts, zuPin 2):|[]}))))),
        (Language.hinzufügen <~> Language.weiche <~> Language.märklin <~> "Name" <~> Language.low <~> "2" <~> Language.gerade <~> "2" <~> Language.kurve <~> "4", (AEBefehl (Hinzufügen (OWeiche (MärklinWeiche {weName="Name", weFließend=LOW, richtungsPins=(Kurve, zuPin (4)):|(Gerade, zuPin 2):[]}))))),
        ((T.take 4 Language.hinzufügen) <~> Language.weiche <~> Language.märklin <~> "Name" <~> Language.low <~> "2" <~> Language.gerade <~> "2" <~> Language.kurve <~> "4", (AEBefehl (Hinzufügen (OWeiche (MärklinWeiche {weName="Name", weFließend=LOW, richtungsPins=(Kurve, zuPin (4)):|(Gerade, zuPin 2):[]}))))),
        (Language.speichern <~> "Dateiname", (AEBefehl (Speichern "Dateiname")))]
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
parseQBefehl = setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite ABefehl not yet implemented" >> setSGR [Reset]

testMany :: String -> [(Text, AnfrageErgebnis)] -> IO ()
testMany name testListe = putStrLn (name <> "\n" <> map (\_ -> '=') name) >> mapM_ (uncurry test) testListe

test :: Text -> AnfrageErgebnis -> IO ()
test eingabe sollErgebnis = do
    putStr "Eingabe:\t"
    setSGR [SetColor Foreground Dull Cyan]
    putStrLn $ unpack eingabe
    setSGR [Reset]
    putStr "Ergebnis:\t"
    setSGR [SetColor Foreground Dull Blue]
    let istErgebnis = (snd . (parser AnfrageBefehl) . lexer . T.words $ eingabe)
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

testResult :: (Show a, Eq a) => String -> a -> a -> IO ()
testResult eingabe ist soll = do
    putStr "Eingabe:\t"
    setSGR [SetColor Foreground Dull Cyan]
    putStrLn eingabe
    setSGR [Reset]
    putStr "Ergebnis:\t"
    setSGR [SetColor Foreground Dull Blue]
    putStrLn $ show ist
    setSGR [Reset]
    putStr "Erwartet:\t"
    setSGR [SetColor Foreground Dull Green]
    putStrLn $ show soll
    setSGR [Reset]
    setSGR [SetColor Foreground Vivid White] >> if ist == soll
        then setSGR [SetColor Background Dull Green] >> putStrLn "Richtig"
        else setSGR [SetColor Background Dull Red] >> putStrLn "Falsch"
    setSGR [Reset]
    putStrLn $ "------------------"

instance Show AnfrageErgebnis where
    show :: AnfrageErgebnis -> String
    show    (AEBefehl (UI Beenden))                                             = "Beenden"
    show    (AEBefehl (UI Abbrechen))                                           = "Abbrechen"
    show    (AEBefehl (Hinzufügen objekt))                                      = "Hinzufügen " <> show objekt
    show    (AEBefehl (Entfernen objekt))                                       = "Entfernen " <> show objekt
    show    (AEBefehl (Speichern dateipfad))                                    = "Speichern " <> dateipfad
    show    (AEBefehl (Laden dateipfad _erfolgsAktion _fehlerbehandlung))       = "Laden " <> dateipfad
    show    (AEBefehl (Ausführen plan _showAction))                             = "Ausführen " <> show plan
    show    (AEBefehl (AktionBefehl aktion))                                    = "Aktion " <> show aktion
    show    (AEBefehlSofort (BSLaden dateipfad) eingabeRest)                    = "Laden " <> dateipfad <> "=>" <> show eingabeRest
    show    (AEStatusAnfrage qObjektIOStatus eitherF _fallback eingabeRest)     = "Query " <> show qObjektIOStatus <> konstruktor eitherF <> "=>" <> show eingabeRest
        where
            konstruktor :: Either a b -> String
            konstruktor (Left _qKonstruktor)    = " Konstruktor "
            konstruktor (Right _konstruktor)    = " qKonstruktor "
    show    (AEAnfrageBefehl qBefehl)                                           = show qBefehl