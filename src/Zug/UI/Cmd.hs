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
import Zug.Klassen (ZugtypKlasse())
import qualified Zug.Language as Language
import Zug.Language ((<~>), (<\>), (<=>), (<!>), (<:>), showText, fehlerhafteEingabe, toBefehlsString)
import qualified Zug.UI.Save as Save
import Zug.Options (Options(..), getOptions)
import Zug.Objekt (Objekt)
import Zug.UI.Base (IOStatus, auswertenLeererIOStatus, tvarMapsNeu,
                    AusführenMöglich(..), ausführenMöglich)
import Zug.UI.Befehl (BefehlAllgemein(..), Befehl, BefehlListeAllgemein(..), ausführenBefehl)
import Zug.UI.Cmd.Lexer(EingabeTokenAllgemein(..), EingabeToken(..), lexer)
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
        putStrLn $ "" <~> ZUGKONTROLLEVERSION
        setSGR [SetColor Foreground Dull Cyan]
        putStrLn $ (const '-') <$> Language.zugkontrolle <~> ZUGKONTROLLEVERSION
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
        statusParserAux (befehle, qErgebnis, eingabeRest, backup)
            = ausführenBefehl (BefehlListe befehle) >> case qErgebnis of
                (AFErgebnis (Right befehl))
                    -> ausführenBefehl befehl
                (AFErgebnis (Left befehlSofort))
                    -> do
                        ergebnis <- ausführenBefehlSofort befehlSofort
                        statusParserAux $ parser ergebnis eingabeRest
                (AFStatusAnfrage aObjektIOStatus konstruktor)
                    -> statusAnfrage aObjektIOStatus konstruktor _backup _eingabeRest
                (AFStatusAnfrageMärklin aObjektIOStatus konstruktor)
                    -> statusAnfrageZugtyp aObjektIOStatus konstruktor _backup _eingabeRest
                (AFStatusAnfrageLego aObjektIOStatus konstruktor)
                    -> statusAnfrageZugtyp aObjektIOStatus konstruktor _backup _eingabeRest
                (AFZwischenwert AnfrageBefehl)
                    -> pure False
                (AFFehler eingabe)
                    -> liftIO (T.putStrLn $ unbekanntShowText AnfrageBefehl eingabe) >> pure False
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
        statusAnfrageAux :: (Anfrage statusAnfrageObjekt) =>
            (objekt -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)) ->
            AnfrageBefehl ->
            [EingabeTokenAllgemein]->
            (AnfrageFortsetzung statusAnfrageObjekt objekt) ->
                IOStatus Bool
        statusAnfrageAux
            konstruktor
            backup
            eingabeRest
            (AFErgebnis objekt)
                = case konstruktor objekt of
                    (AFErgebnis (Right befehl))
                        -> ausführenBefehl befehl >> statusParser eingabeRest
                    (AFErgebnis (Left befehlSofort))
                        -> statusParserAux ([], AFErgebnis $ Left befehlSofort, eingabeRest, backup)
                    (AFStatusAnfrage qObjektIOStatus1 konstruktor1)
                        -> statusAnfrage qObjektIOStatus1 konstruktor1 backup eingabeRest
                    (AFStatusAnfrageMärklin qObjektIOStatus1 konstruktor1)
                        -> statusAnfrageZugtyp qObjektIOStatus1 konstruktor1 backup eingabeRest
                    (AFStatusAnfrageLego qObjektIOStatus1 konstruktor1)
                        -> statusAnfrageZugtyp qObjektIOStatus1 konstruktor1 backup eingabeRest
                    (AFZwischenwert anfrage)
                        -> statusParserAux $ parser anfrage eingabeRest
        statusAnfrageAux
            _konstruktor
            backup
            _eingabeRest
            (AFFehler eingabe)
                = promptS (zeigeAnfrageFehlgeschlagen backup eingabe <!> zeigeAnfrage backup <:> "")
                    >>= statusParserAux . parser backup . lexer

-- class ErhalteEingabe s where
--     erhalteEingabe :: s -> Text
-- instance ErhalteEingabe StatusAnfrageObjekt where
--     erhalteEingabe :: StatusAnfrageObjekt -> Text
--     erhalteEingabe  (SAOPlan EingabeToken {eingabe})                = eingabe
--     erhalteEingabe  (SAOWegstrecke EingabeToken {eingabe})          = eingabe
--     erhalteEingabe  (SAOWeiche EingabeToken {eingabe})              = eingabe
--     erhalteEingabe  (SAOBahngeschwindigkeit EingabeToken {eingabe}) = eingabe
--     erhalteEingabe  (SAOStreckenabschnitt EingabeToken {eingabe})   = eingabe
--     erhalteEingabe  (SAOKupplung EingabeToken {eingabe})            = eingabe
-- instance ErhalteEingabe (StatusAnfrageObjektZugtyp z) where
--     erhalteEingabe :: StatusAnfrageObjektZugtyp z -> Text
--     erhalteEingabe  (SAOZPlan EingabeToken {eingabe})                = eingabe
--     erhalteEingabe  (SAOZWegstrecke EingabeToken {eingabe})          = eingabe
--     erhalteEingabe  (SAOZWeiche EingabeToken {eingabe})              = eingabe
--     erhalteEingabe  (SAOZBahngeschwindigkeit EingabeToken {eingabe}) = eingabe
--     erhalteEingabe  (SAOZStreckenabschnitt EingabeToken {eingabe})   = eingabe
--     erhalteEingabe  (SAOZKupplung EingabeToken {eingabe})            = eingabe

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