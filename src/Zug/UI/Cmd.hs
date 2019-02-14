{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

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
import Zug.Language ((<=>), (<!>), (<:>), showText, fehlerhafteEingabe, toBefehlsString)
import Zug.Anbindung
import qualified Zug.UI.Save as Save
import Zug.Options
import Zug.UI.Base
import Zug.UI.Befehl
import Zug.UI.Cmd.Lexer(AllgemeinesEingabeToken(..), EingabeToken(..), lexer)
import Zug.UI.Cmd.Parser

-- | main loop
main :: IO ()
main = do
    -- Lade Datei angegeben in Kommandozeilenargument
    (Options {load=path}) <- getOptions
    Save.load path pure >>= \case
        (Nothing)           -> evalEmptyIOStatus mainStatus
        (Just konstruktor)  -> newMVar pinMapEmpty >>= \mvarPinMap -> konstruktor mvarPinMap >>= evalStateT mainStatus

mainStatus :: IOStatus ()
mainStatus = do
    ende <- get >>= \status -> do
        liftIO $ do
            setSGR [SetColor Foreground Dull Green]
            putStrLn $ '\n':Language.zugkontrolle
            setSGR [SetColor Foreground Dull Cyan]
            putStrLn (map (\_ -> '-') Language.zugkontrolle)
            setSGR [Reset]
            putStrLn $ showText status
            setSGR [SetColor Foreground Dull Blue]
            putStrLn $ toBefehlsString Language.befehlAlle
            setSGR [Reset]
        promptS "\n" >>= statusParser.lexer
    unless ende mainStatus

-- | Gesammter Auswerte-Prozess
statusParser :: [AllgemeinesEingabeToken] -> IOStatus Bool
statusParser    eingabe = (uncurry statusParserAux) $ parser QBefehl eingabe
    where
        statusParserAux :: [Befehl] -> QErgebnis-> IOStatus Bool
        statusParserAux befehle qErgebnis = ausführenBefehl (BefehlListe befehle) >> case qErgebnis of
                (QEBefehl befehl)                                           -> ausführenBefehl befehl
                (QEBefehlSofort befehlSofort eingabeRest)                   -> ausführenBefehlSofort befehlSofort >> statusParser eingabeRest
                (QEBefehlQuery qObjektIOStatus eitherF backup eingabeRest)  -> state (runState $ statusQueryObjekt qObjektIOStatus) >>= \case
                    (Right objekt)  -> case eitherF of
                        (Right konstruktor)     -> ausführenBefehl (konstruktor objekt) >>= \beenden -> if beenden then pure True else statusParser eingabeRest
                        (Left qKonstruktor)     -> (uncurry statusParserAux) $ parser (qKonstruktor objekt) eingabeRest
                    (Left query)    -> promptS (getQueryFailed query $ getEingabe query <!> showQuery query <:> "") >>= (uncurry statusParserAux).(parser backup).lexer
                        where
                            getEingabe :: QObjektIOStatus -> Text
                            getEingabe  (QOIOSUnbekannt eingabe)                            = eingabe
                            getEingabe  (QOIOSPlan (EingabeToken {eingabe}))                = eingabe
                            getEingabe  (QOIOSWegstrecke (EingabeToken {eingabe}))          = eingabe
                            getEingabe  (QOIOSWeiche (EingabeToken {eingabe}))              = eingabe
                            getEingabe  (QOIOSBahngeschwindigkeit (EingabeToken {eingabe})) = eingabe
                            getEingabe  (QOIOSStreckenabschnitt (EingabeToken {eingabe}))   = eingabe
                            getEingabe  (QOIOSKupplung (EingabeToken {eingabe}))            = eingabe
                (QEQBefehl (QBefehl))                                       -> pure False
                (QEQBefehl (QBUnbekannt QBefehl eingabe))                   -> liftIO (T.putStrLn $ unbekanntShowText QBefehl eingabe) >> pure False
                (QEQBefehl (QBUnbekannt query eingabe))                     -> liftIO (setSGR [SetColor Foreground Vivid Red] >> T.putStr (unbekanntShowText query eingabe) >> setSGR [Reset]) >> promptS "" >>= (uncurry statusParserAux).(parser query).lexer
                (QEQBefehl query)                                           -> do
                    liftIO $ case getQueryOptions query of
                        (Nothing)           -> pure ()
                        (Just queryOptions) -> setSGR [SetColor Foreground Dull Blue] >> putStrLn queryOptions >> setSGR [Reset]
                    promptS (showQuery query <:> "") >>= (uncurry statusParserAux).(parser query).lexer

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