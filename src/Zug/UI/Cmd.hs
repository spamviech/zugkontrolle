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
import Control.Monad (void, unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (evalStateT, get, state, runState)
import Control.Concurrent.MVar (newMVar)
-- Farbige Konsolenausgabe
import System.Console.ANSI
-- Abhängigkeiten von anderen Modulen
import qualified Zug.Language as Language
import Zug.Language ((<~>), (<\>), (<=>), (<!>), (<:>), showText, fehlerhafteEingabe, toBefehlsString)
import Zug.Anbindung
import qualified Zug.UI.Save as Save
import Zug.Options
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
statusParser    eingabe = (uncurry statusParserAux) $ parser AnfrageBefehl eingabe
    where
        statusParserAux :: [Befehl] -> AnfrageErgebnis-> IOStatus Bool
        statusParserAux befehle qErgebnis = ausführenBefehl (BefehlListe befehle) >> case qErgebnis of
                (AEBefehl befehl)                                               -> ausführenBefehl befehl
                (AEBefehlSofort befehlSofort eingabeRest)                       -> ausführenBefehlSofort befehlSofort >> statusParser eingabeRest
                (AEStatusAnfrage qObjektIOStatus eitherF backup eingabeRest)    -> state (runState $ statusAnfrageObjekt qObjektIOStatus) >>= \case
                    (Right objekt)  -> case eitherF of
                        (Right konstruktor)     -> ausführenBefehl (konstruktor objekt) >>= \beenden -> if beenden then pure True else statusParser eingabeRest
                        (Left qKonstruktor)     -> (uncurry statusParserAux) $ parser (qKonstruktor objekt) eingabeRest
                    (Left anfrage)    -> promptS (zeigeAnfrageFehlgeschlagen anfrage $ erhalteEingabe anfrage <!> zeigeAnfrage anfrage <:> "") >>= (uncurry statusParserAux).(parser backup).lexer
                        where
                            erhalteEingabe :: StatusAnfrageObjekt -> Text
                            erhalteEingabe  (SAOUnbekannt eingabe)                            = eingabe
                            erhalteEingabe  (SAOPlan (EingabeToken {eingabe}))                = eingabe
                            erhalteEingabe  (SAOWegstrecke (EingabeToken {eingabe}))          = eingabe
                            erhalteEingabe  (SAOWeiche (EingabeToken {eingabe}))              = eingabe
                            erhalteEingabe  (SAOBahngeschwindigkeit (EingabeToken {eingabe})) = eingabe
                            erhalteEingabe  (SAOStreckenabschnitt (EingabeToken {eingabe}))   = eingabe
                            erhalteEingabe  (SAOKupplung (EingabeToken {eingabe}))            = eingabe
                (AEAnfrageBefehl (AnfrageBefehl))                               -> pure False
                (AEAnfrageBefehl (ABUnbekannt AnfrageBefehl eingabe))           -> liftIO (T.putStrLn $ unbekanntShowText AnfrageBefehl eingabe) >> pure False
                (AEAnfrageBefehl (ABUnbekannt anfrage eingabe))                 -> liftIO (setSGR [SetColor Foreground Vivid Red] >> T.putStr (unbekanntShowText anfrage eingabe) >> setSGR [Reset]) >> promptS "" >>= (uncurry statusParserAux).(parser anfrage).lexer
                (AEAnfrageBefehl anfrage)                                       -> do
                    case zeigeAnfrageOptionen anfrage of
                        (Nothing)               -> pure ()
                        (Just anfrageOptionen)  -> liftIO $ setSGR [SetColor Foreground Dull Blue] >> putStrLn anfrageOptionen >> setSGR [Reset]
                    promptS (zeigeAnfrage anfrage <:> "") >>= (uncurry statusParserAux).(parser anfrage).lexer

-- | Ausführen eines Befehls, der sofort ausgeführt werden muss
ausführenBefehlSofort :: BefehlSofort -> IOStatus ()
ausführenBefehlSofort   (BSLaden dateipfad) = void $ ausführenBefehl (Laden dateipfad pure $ fehlerhafteEingabeS $ Language.nichtGefundeneDatei <=> pack dateipfad)

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