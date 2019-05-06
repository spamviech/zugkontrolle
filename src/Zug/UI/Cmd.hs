{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

{-|
Description : Starte Main-Loop für Kommandozeilen-basiertes UI.
-}
module Zug.UI.Cmd (main, mainStatus) where

-- Bibliotheken
import System.IO (hFlush, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (evalStateT, get, state, runState)
import Control.Concurrent.MVar (newMVar, readMVar)
-- Farbige Konsolenausgabe
import System.Console.ANSI
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language
import Zug.Language ((<~>), (<\>), (<=>), (<!>), (<:>), showText, fehlerhafteEingabe, toBefehlsString)
import Zug.Anbindung
import qualified Zug.UI.Save as Save
import Zug.Options
import Zug.Plan (Ausführend (..), Objekt)
import Zug.UI.Base
import Zug.UI.Befehl
import Zug.UI.Cmd.Lexer(EingabeTokenAllgemein(..), EingabeToken(..), lexer)
import Zug.UI.Cmd.Parser

-- | Lade per Kommandozeile übergebenen Anfangszustand und führe den main loop aus.
main :: IO ()
main = do
    -- Lade Datei angegeben in Kommandozeilenargument
    (Options {load=path}) <- getOptions
    Save.laden path pure >>= \case
        (Nothing)           -> auswertenLeererIOStatus mainStatus
        (Just konstruktor)  -> newMVar pinMapEmpty >>= konstruktor >>= evalStateT mainStatus

-- | main loop
mainStatus :: IOStatus ()
mainStatus = do
    ende <- do
        status <- get
        liftIO $ do
            setSGR [SetColor Foreground Dull Green]
            putStr $ "" <\> Language.zugkontrolle
            setSGR [Reset]
            putStrLn $ "" <~> ZUGKONTROLLEVERSION
            setSGR [SetColor Foreground Dull Cyan]
            putStrLn $ (const '-') <$> Language.zugkontrolle <~> ZUGKONTROLLEVERSION
            setSGR [Reset]
            putStrLn $ showText status
            setSGR [SetColor Foreground Dull Blue]
            putStrLn $ toBefehlsString Language.befehlAlle
            setSGR [Reset]
        promptS "\n" >>= statusParser . lexer
    unless ende mainStatus

-- | Gesammter Auswerte-Prozess
statusParser :: [EingabeTokenAllgemein] -> IOStatus Bool
statusParser eingabe = statusParserAux $ parser AnfrageBefehl eingabe
    where
        statusParserAux :: ([Befehl], AnfrageErgebnis)-> IOStatus Bool
        statusParserAux (befehle, qErgebnis) = ausführenBefehl (BefehlListe befehle) >> case qErgebnis of
                (AEBefehl befehl)                                                   -> ausführenBefehl befehl
                (AEBefehlSofort befehlSofort eingabeRest)                           -> do
                    ergebnis <- ausführenBefehlSofort befehlSofort
                    statusParserAux $ parser ergebnis eingabeRest
                (AEStatusAnfrage qObjektIOStatus konstruktor backup eingabeRest)    -> statusAnfrage qObjektIOStatus konstruktor backup eingabeRest
                (AEAnfrageBefehl (AnfrageBefehl))                                   -> pure False
                (AEAnfrageBefehl (ABUnbekannt AnfrageBefehl eingabe))               -> liftIO (T.putStrLn $ unbekanntShowText AnfrageBefehl eingabe) >> pure False
                (AEAnfrageBefehl (ABUnbekannt anfrage eingabe))                     -> liftIO (setSGR [SetColor Foreground Vivid Red] >> T.putStr (unbekanntShowText anfrage eingabe) >> setSGR [Reset]) >> promptS "" >>= statusParserAux.(parser anfrage).lexer
                (AEAnfrageBefehl anfrage)                                           -> do
                    case zeigeAnfrageOptionen anfrage of
                        (Nothing)               -> pure ()
                        (Just anfrageOptionen)  -> liftIO $ setSGR [SetColor Foreground Dull Blue] >> putStrLn anfrageOptionen >> setSGR [Reset]
                    promptS (zeigeAnfrage anfrage <:> "") >>= statusParserAux.(parser anfrage).lexer
        statusAnfrage :: StatusAnfrageObjekt -> (Objekt -> AnfrageErgebnis) -> AnfrageBefehl -> [EingabeTokenAllgemein] -> IOStatus Bool
        statusAnfrage qObjektIOStatus konstruktor backup eingabeRest = state (runState $ statusAnfrageObjekt qObjektIOStatus) >>= \case
            (Right objekt)  -> case konstruktor objekt of
                (AEBefehl befehl)
                    -> ausführenBefehl befehl >> statusParser eingabeRest
                (AEBefehlSofort befehl eingabeRest1)
                    -> statusParserAux ([], AEBefehlSofort befehl $ eingabeRest1 ++ eingabeRest)
                (AEStatusAnfrage qObjektIOStatus1 konstruktor1 backup1 eingabeRest1)
                    -> statusAnfrage qObjektIOStatus1 konstruktor1 backup1 $ eingabeRest1 ++ eingabeRest
                (AEAnfrageBefehl anfrage)
                    -> statusParserAux $ parser anfrage eingabe
            (Left anfrage)  -> promptS (zeigeAnfrageFehlgeschlagen anfrage $ erhalteEingabe anfrage <!> zeigeAnfrage anfrage <:> "") >>= statusParserAux.(parser backup).lexer
        erhalteEingabe :: StatusAnfrageObjekt -> Text
        erhalteEingabe  (SAOUnbekannt eingabe)                            = eingabe
        erhalteEingabe  (SAOPlan (EingabeToken {eingabe}))                = eingabe
        erhalteEingabe  (SAOWegstrecke (EingabeToken {eingabe}))          = eingabe
        erhalteEingabe  (SAOWeiche (EingabeToken {eingabe}))              = eingabe
        erhalteEingabe  (SAOBahngeschwindigkeit (EingabeToken {eingabe})) = eingabe
        erhalteEingabe  (SAOStreckenabschnitt (EingabeToken {eingabe}))   = eingabe
        erhalteEingabe  (SAOKupplung (EingabeToken {eingabe}))            = eingabe

-- | Ausführen eines Befehls, der sofort ausgeführt werden muss
ausführenBefehlSofort :: BefehlSofort -> IOStatus AnfrageBefehl
ausführenBefehlSofort   (BSLaden dateipfad)         = do
    ausführenBefehl $ Laden dateipfad pure $ fehlerhafteEingabeS $ Language.nichtGefundeneDatei <=> pack dateipfad
    pure AnfrageBefehl
ausführenBefehlSofort   (BSAusführenAbbrechen plan) = do
    mvarAusführend <- getMVarAusführend
    ausführend <- liftIO $ readMVar mvarAusführend
    pure $ (if elem (Ausführend plan) ausführend then ABAktionPlanAusführend else ABAktionPlan) plan

-- * Eingabe abfragen
prompt :: Text -> IO [Text]
prompt text = do
    T.putStr text
    hFlush stdout
    T.getLine >>= pure . T.words

promptS :: Text -> IOStatus [Text]
promptS = liftIOFunction prompt

fehlerhafteEingabeS :: Text -> IOStatus ()
fehlerhafteEingabeS = liftIOFunction fehlerhafteEingabe