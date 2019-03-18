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
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Concurrent
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
    Save.load path pure >>= \case
        (Nothing)           -> evalEmptyIOStatus mainStatus
        (Just konstruktor)  -> newMVar pinMapEmpty >>= \mvarPinMap -> konstruktor mvarPinMap >>= evalStateT mainStatus

-- | main loop
mainStatus :: IOStatus ()
mainStatus = do
    ende <- get >>= \status -> do
        liftIO $ do
            setSGR [SetColor Foreground Dull Green]
            putStr $ "" <\> Language.zugkontrolle
            setSGR [Reset]
            putStr $ "" <~> ZUGKONTROLLEVERSION
            setSGR [SetColor Foreground Dull Cyan]
            putStrLn $ (const '-') <$> Language.zugkontrolle
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
                (AEBefehl befehl)                                           -> ausführenBefehl befehl
                (AEBefehlSofort befehlSofort eingabeRest)                   -> ausführenBefehlSofort befehlSofort >> statusParser eingabeRest
                (AEStatusAnfrage qObjektIOStatus eitherF backup eingabeRest)  -> state (runState $ statusAnfrageObjekt qObjektIOStatus) >>= \case
                    (Right objekt)  -> case eitherF of
                        (Right konstruktor)     -> ausführenBefehl (konstruktor objekt) >>= \beenden -> if beenden then pure True else statusParser eingabeRest
                        (Left qKonstruktor)     -> (uncurry statusParserAux) $ parser (qKonstruktor objekt) eingabeRest
                    (Left query)    -> promptS (zeigeAnfrageFehlgeschlagen query $ erhalteEingabe query <!> zeigeAnfrage query <:> "") >>= (uncurry statusParserAux).(parser backup).lexer
                        where
                            erhalteEingabe :: StatusAnfrageObjekt -> Text
                            erhalteEingabe  (SAOUnbekannt eingabe)                            = eingabe
                            erhalteEingabe  (SAOPlan (EingabeToken {eingabe}))                = eingabe
                            erhalteEingabe  (SAOWegstrecke (EingabeToken {eingabe}))          = eingabe
                            erhalteEingabe  (SAOWeiche (EingabeToken {eingabe}))              = eingabe
                            erhalteEingabe  (SAOBahngeschwindigkeit (EingabeToken {eingabe})) = eingabe
                            erhalteEingabe  (SAOStreckenabschnitt (EingabeToken {eingabe}))   = eingabe
                            erhalteEingabe  (SAOKupplung (EingabeToken {eingabe}))            = eingabe
                (AEAnfrageBefehl (AnfrageBefehl))                                 -> pure False
                (AEAnfrageBefehl (ABUnbekannt AnfrageBefehl eingabe))             -> liftIO (T.putStrLn $ unbekanntShowText AnfrageBefehl eingabe) >> pure False
                (AEAnfrageBefehl (ABUnbekannt query eingabe))                     -> liftIO (setSGR [SetColor Foreground Vivid Red] >> T.putStr (unbekanntShowText query eingabe) >> setSGR [Reset]) >> promptS "" >>= (uncurry statusParserAux).(parser query).lexer
                (AEAnfrageBefehl query)                                           -> do
                    case zeigeAnfrageOptionen query of
                        (Nothing)           -> pure ()
                        (Just queryOptions) -> liftIO $ setSGR [SetColor Foreground Dull Blue] >> putStrLn queryOptions >> setSGR [Reset]
                    promptS (zeigeAnfrage query <:> "") >>= (uncurry statusParserAux).(parser query).lexer

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