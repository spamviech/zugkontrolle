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
import Control.Monad (unless, void)
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.RWS (evalRWST)
import Control.Monad.State.Class (MonadState(..))
-- Farbige Konsolenausgabe
import System.Console.ANSI
-- Abhängigkeiten von anderen Modulen
import Zug.Enums (ZugtypKlasse())
import qualified Zug.Language as Language
import Zug.Language ((<~>), (<\>), (<=>), (<!>), (<:>), showText, fehlerhafteEingabe, toBefehlsString)
import qualified Zug.UI.Save as Save
import Zug.Options (Options(..), getOptions)
import Zug.Objekt (Objekt)
import Zug.UI.Base (IOStatus, auswertenLeererIOStatus, tvarMapsNeu,
                    AusführenMöglich(..), ausführenMöglich)
import Zug.UI.Befehl (BefehlAllgemein(..), Befehl, BefehlListeAllgemein(..), ausführenBefehl)
import Zug.UI.Cmd.Lexer(EingabeTokenAllgemein(..), lexer)
import Zug.UI.Cmd.Parser (AnfrageFortsetzung(..), AnfrageBefehl(..), Anfrage(..),
                        StatusAnfrageObjekt(..), statusAnfrageObjekt,
                        StatusAnfrageObjektZugtyp(..), statusAnfrageObjektZugtyp, ObjektZugtyp(..),
                        BefehlSofort(..), AnfrageNeu(..), parser,
                        unbekanntShowText, zeigeAnfrage, zeigeAnfrageOptionen, zeigeAnfrageFehlgeschlagen)

-- | Lade per Kommandozeile übergebenen Anfangszustand und führe den main loop aus.
main :: IO ()
main = do
    -- Lade Datei angegeben in Kommandozeilenargument
    Options {load = path} <- getOptions
    Save.laden path pure >>= \case
        Nothing
            -> auswertenLeererIOStatus mainStatus tvarMapsNeu
        (Just anfangsZustand)
            -> do
                tvarMaps <- tvarMapsNeu
                void $ evalRWST mainStatus tvarMaps anfangsZustand

-- | main loop
mainStatus :: IOStatus ()
mainStatus = do
    status <- get
    liftIO $ do
        setSGR [SetColor Foreground Dull Green]
        putStr $ "" <\> Language.zugkontrolle
        setSGR [Reset]
        putStrLn $ "" <~> Language.version
        setSGR [SetColor Foreground Dull Cyan]
        putStrLn $ map (const '-') $ Language.zugkontrolle <~> Language.version
        setSGR [Reset]
        putStrLn $ showText status
        setSGR [SetColor Foreground Dull Blue]
        putStrLn $ toBefehlsString Language.befehlAlle
        setSGR [Reset]
    ende <- promptS "\n" >>= statusParser . lexer
    unless ende mainStatus

-- | Gesammter Auswerte-Prozess
statusParser :: [EingabeTokenAllgemein] -> IOStatus Bool
statusParser = statusParserAux . parser AnfrageBefehl
    where
        statusParserAux ::
            ([Befehl], AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl), [EingabeTokenAllgemein], AnfrageBefehl) ->
                IOStatus Bool
        statusParserAux (befehle, fortsetzung, eingabeRest, backup)
            = ausführenBefehl (BefehlListe befehle) >> case fortsetzung of
                (AFErgebnis (Right befehl))
                    -> ausführenBefehl befehl
                (AFErgebnis (Left befehlSofort))
                    -> do
                        ergebnis <- ausführenBefehlSofort befehlSofort
                        statusParserAux $ parser ergebnis eingabeRest
                (AFStatusAnfrage aObjektIOStatus konstruktor)
                    -> statusAnfrage aObjektIOStatus konstruktor backup eingabeRest
                (AFStatusAnfrageMärklin aObjektIOStatus konstruktor)
                    -> statusAnfrageZugtyp aObjektIOStatus konstruktor backup eingabeRest
                (AFStatusAnfrageLego aObjektIOStatus konstruktor)
                    -> statusAnfrageZugtyp aObjektIOStatus konstruktor backup eingabeRest
                (AFZwischenwert AnfrageBefehl)
                    -> pure False
                (AFFehler eingabe)
                    -> do
                        liftIO $ do
                            setSGR [SetColor Foreground Vivid Red]
                            T.putStr $ unbekanntShowText backup eingabe
                            setSGR [Reset]
                        promptS "" >>= statusParserAux . parser backup . lexer
                (AFZwischenwert anfrage)
                    -> do
                        case zeigeAnfrageOptionen anfrage of
                            Nothing
                                -> pure ()
                            (Just anfrageOptionen)
                                -> liftIO $ do
                                    setSGR [SetColor Foreground Dull Blue]
                                    putStrLn anfrageOptionen
                                    setSGR [Reset]
                        promptS (zeigeAnfrage anfrage <:> "") >>= statusParserAux . parser anfrage . lexer
        statusAnfrage ::
            StatusAnfrageObjekt ->
            (Objekt -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)) ->
            AnfrageBefehl ->
            [EingabeTokenAllgemein] ->
                IOStatus Bool
        statusAnfrage aObjektIOStatus konstruktor backup eingabeRest
            = statusAnfrageObjekt aObjektIOStatus >>= statusAnfrageAux konstruktor backup eingabeRest
        statusAnfrageZugtyp :: (ZugtypKlasse z) =>
            StatusAnfrageObjektZugtyp z ->
            (ObjektZugtyp z -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)) ->
            AnfrageBefehl ->
            [EingabeTokenAllgemein] ->
                IOStatus Bool
        statusAnfrageZugtyp aObjektIOStatus konstruktor backup eingabeRest
            = statusAnfrageObjektZugtyp aObjektIOStatus >>= statusAnfrageAux konstruktor backup eingabeRest
        statusAnfrageAux ::
            (objekt -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)) ->
            AnfrageBefehl ->
            [EingabeTokenAllgemein]->
            (Either Text objekt) ->
                IOStatus Bool
        statusAnfrageAux
            konstruktor
            backup
            eingabeRest
            (Right objekt)
                = case konstruktor objekt of
                    ergebnis@(AFErgebnis _befehl)
                        -> statusParserAux ([], ergebnis, eingabeRest, backup)
                    (AFStatusAnfrage qObjektIOStatus1 konstruktor1)
                        -> statusAnfrage qObjektIOStatus1 konstruktor1 backup eingabeRest
                    (AFStatusAnfrageMärklin qObjektIOStatus1 konstruktor1)
                        -> statusAnfrageZugtyp qObjektIOStatus1 konstruktor1 backup eingabeRest
                    (AFStatusAnfrageLego qObjektIOStatus1 konstruktor1)
                        -> statusAnfrageZugtyp qObjektIOStatus1 konstruktor1 backup eingabeRest
                    (AFZwischenwert anfrage)
                        -> statusParserAux $ parser anfrage eingabeRest
                    fehler@(AFFehler _eingabe)
                        -> statusParserAux ([], fehler, eingabeRest, backup)
        statusAnfrageAux
            _konstruktor
            backup
            _eingabeRest
            (Left eingabe)
                = promptS (zeigeAnfrageFehlgeschlagen backup eingabe <!> zeigeAnfrage backup <:> "")
                    >>= statusParserAux . parser backup . lexer

-- | Ausführen eines Befehls, der sofort ausgeführt werden muss
ausführenBefehlSofort :: BefehlSofort -> IOStatus AnfrageBefehl
ausführenBefehlSofort   (BSLaden dateipfad)         = do
    ausführenBefehl $ Laden dateipfad pure $ fehlerhafteEingabeS $
        Language.nichtGefundeneDatei <=> pack dateipfad
    pure AnfrageBefehl
ausführenBefehlSofort   (BSAusführenMöglich plan) = ausführenMöglich plan >>= pure . \case
        AusführenMöglich                -> ABAktionPlan plan
        WirdAusgeführt                  -> ABAktionPlanAusführend plan Neu
        (AnschlüsseBelegt anschlüsse)   -> ABAktionPlanGesperrt plan Neu anschlüsse

-- * Eingabe abfragen
prompt :: Text -> IO [Text]
prompt text = do
    T.putStr text
    hFlush stdout
    T.getLine >>= pure . T.words

promptS :: Text -> IOStatus [Text]
promptS = liftIO . prompt

fehlerhafteEingabeS :: Text -> IOStatus ()
fehlerhafteEingabeS = liftIO . fehlerhafteEingabe