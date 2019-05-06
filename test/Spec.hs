{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Zug.UI.Cmd.Parser (AnfrageErgebnis(..), BefehlSofort(..), AnfrageBefehl(..), parser, StatusAnfrageObjekt(..))
import Zug.UI.Cmd.Lexer (lexer)
import Zug.Anbindung
import Zug.Plan
import Zug.Klassen

main :: IO ()
main = hspec $ do
    warteschalngeTests
    linkedMVarTests
    parseBefehl
    parseQuery
    parseBefehlSofort
    parseQBefehl

-- Tests für Warteschlange
warteschalngeTests :: Spec
warteschalngeTests = runIO $ setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite Warteschlange not yet implemented" >> setSGR [Reset]

-- Tests für LinkedMVar
linkedMVarTests :: Spec
linkedMVarTests = do
    describe "LinkedMVar" $ do
        -- LinkedMVar ohne Update-Funktion
        unlinkedRes <- runIO $ do
            unlinked <- newUnlinkedMVar (0 :: Word)
            modifyLinkedMVar_ unlinked $ pure . succ
            readLinkedMVar unlinked
        it "0 >>= modifyUnlinkedMVar_ unlinked $ pure . succ" $
            unlinkedRes `shouldBe` 1
        -- LinkedMVar mit pure als Update-Funktion
        linkedPureRes <- runIO $ do
            linkedPure <- newLinkedMVar pure (0 :: Int)
            modifyLinkedMVar_ linkedPure $ pure . succ
            readLinkedMVar linkedPure
        it "0 >>= modifyUnlinkedMVar_ linkedPure $ pure . succ" $
            linkedPureRes `shouldBe` 1
        -- LinkedMVar mit pure . const 0 als Update-Funktion
        linkedConst0Res <- runIO $ do
            linkedConst0 <- newLinkedMVar (pure . const 0) (0 :: Float)
            modifyLinkedMVar_ linkedConst0 $ pure . succ
            readLinkedMVar linkedConst0
        it "0 >>= modifyUnlinkedMVar_ linkedConst0 $ pure . succ" $
            linkedConst0Res `shouldBe` 0

-- Parser für Eingabe von vollständigen Befehlen testen
parseBefehl :: Spec
parseBefehl = do
    describe "Befehl" $ mapM_ testeParserErgebnis [
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
    runIO $ setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite Befehl not yet fully implemented" >> setSGR [Reset]

{-
eqShow :: (Show a, Show b) => a -> b -> Bool
eqShow x y = (show x) == (show y)

infix 1 <==>
(<==>) :: (Show a) => a -> a -> Bool
(<==>) = eqShow
-}

-- Parser für Eingabe Testen, die eine Statusabfrage benötigt
parseQuery :: Spec
parseQuery = runIO $ setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite Query not yet implemented" >> setSGR [Reset]

-- Parser für Eingabe Testen, die eine sofortige ausführung benötigt
parseBefehlSofort :: Spec
parseBefehlSofort = runIO $ setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite BefehlSofort not yet implemented" >> setSGR [Reset]

-- Parser für Eingabe von unvollständigen Befehlen testen
parseQBefehl :: Spec
parseQBefehl = runIO $ setSGR [SetColor Foreground Dull Red] >> putStrLn "Test suite ABefehl not yet implemented" >> setSGR [Reset]

testeParserErgebnis :: (Text, AnfrageErgebnis) -> Spec
testeParserErgebnis (eingabe, sollErgebnis) = it (unpack eingabe) $ (snd . (parser AnfrageBefehl) . lexer . T.words) eingabe `shouldBe` sollErgebnis

instance Show AnfrageErgebnis where
    show :: AnfrageErgebnis -> String
    show    (AEBefehl (UI Beenden))                                             = "Beenden"
    show    (AEBefehl (UI Abbrechen))                                           = "Abbrechen"
    show    (AEBefehl (Hinzufügen objekt))                                      = "Hinzufügen " <> show objekt
    show    (AEBefehl (Entfernen objekt))                                       = "Entfernen " <> show objekt
    show    (AEBefehl (Speichern dateipfad))                                    = "Speichern " <> dateipfad
    show    (AEBefehl (Laden dateipfad _erfolgsAktion _fehlerbehandlung))       = "Laden " <> dateipfad
    show    (AEBefehl (Ausführen plan _showAction _endAktion))                  = "Ausführen " <> show plan
    show    (AEBefehl (AusführenAbbrechen plan))                                = "Ausführen abbrechen" <> show plan
    show    (AEBefehl (AktionBefehl aktion))                                    = "Aktion " <> show aktion
    show    (AEBefehlSofort (BSLaden dateipfad) eingabeRest)                    = "Laden " <> dateipfad <> "=>" <> show eingabeRest
    show    (AEBefehlSofort (BSAusführenMöglich plan) eingabeRest)              = "Ausführen abbrechen" <> show plan <> "=>" <> show eingabeRest
    show    (AEStatusAnfrage qObjektIOStatus konstruktor _fallback eingabeRest) = "Query " <> show qObjektIOStatus <> show (konstruktor testObjekt) <> "=>" <> show eingabeRest
        where
            testObjekt :: Objekt
            testObjekt = OPlan $ Plan {plName="Test-Objekt", plAktionen=[]}
    show    (AEAnfrageBefehl qBefehl)                                           = show qBefehl

deriving instance Eq (UIBefehlAllgemein o)
deriving instance Eq StatusAnfrageObjekt

instance Eq AnfrageErgebnis where
    (==) :: AnfrageErgebnis -> AnfrageErgebnis -> Bool
    (AEBefehl (UI u0))                          ==  (AEBefehl (UI u1))                          = u0 == u1
    (AEBefehl (Hinzufügen o0))                  ==  (AEBefehl (Hinzufügen o1))                  = o0 == o1
    (AEBefehl (Entfernen o0))                   ==  (AEBefehl (Entfernen o1))                   = o0 == o1
    (AEBefehl (Speichern p0))                   ==  (AEBefehl (Speichern p1))                   = p0 == p1
    (AEBefehl (Laden p0 _ _))                   ==  (AEBefehl (Laden p1 _ _))                   = p0 == p1
    (AEBefehl (Ausführen p0 _ _))               ==  (AEBefehl (Ausführen p1 _ _))               = p0 == p1
    (AEBefehl (AusführenAbbrechen p0))          ==  (AEBefehl (AusführenAbbrechen p1))          = p0 == p1
    (AEBefehl (AktionBefehl a0))                ==  (AEBefehl (AktionBefehl a1))                = a0 == a1
    (AEBefehlSofort (BSLaden p0) r0)            ==  (AEBefehlSofort (BSLaden p1) r1)            = p0 == p1 && r0 == r1
    (AEBefehlSofort (BSAusführenMöglich p0) r0) ==  (AEBefehlSofort (BSAusführenMöglich p1) r1) = p0 == p1 && r0 == r1
    (AEStatusAnfrage q0 k0 _ r0)                ==  (AEStatusAnfrage q1 k1 _ r1)                = q0 == q1 && (k0 t) == (k1 t) && r0 == r1
        where
            t :: Objekt
            t = OPlan $ Plan {plName="", plAktionen=[]}
    _ae0                                        ==  _ae1                                        = False