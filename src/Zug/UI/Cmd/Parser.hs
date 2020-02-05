{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description : Verarbeiten von Token.

In diesem Modul werden sämtliche Umwandlungen vorgenommen, für die nicht die IO-Monade benötigt wird.
-}
module Zug.UI.Cmd.Parser
  ( -- * Auswerten einer Text-Eingabe
    parser
  , statusAnfrageObjekt
  , statusAnfrageObjektZugtyp
    -- * Ergebnis-Typen
  , AnfrageBefehl(..)
  , BefehlSofort(..)
  , AnfrageNeu(..)
  , StatusAnfrageObjekt(..)
  , StatusAnfrageObjektZugtyp(..)
  , ObjektZugtyp(..)
  , zuObjekt
  , AnfrageFortsetzung(..)
  , verwendeAnfrageFortsetzung
  , ($<<)
    -- ** Unvollständige StreckenObjekte
  , Anfrage(..)
  , MitAnfrage(..)
  , zeigeAnfrageFehlgeschlagenStandard
  , anzeigeMitAnfrage
  , anzeigeMitAnfrageFehlgeschlagen
  , unbekanntShowText
  , AnfragePlan(..)
  , AnfrageAktion(..)
  , AnfrageAktionWegstrecke(..)
  , AnfrageAktionWeiche(..)
  , AnfrageAktionBahngeschwindigkeit(..)
  , AnfrageAktionStreckenabschnitt(..)
  , AnfrageAktionKupplung(..)
  , AnfrageObjekt(..)
  , AnfrageBahngeschwindigkeit(..)
  , AnfrageStreckenabschnitt(..)
  , AnfrageWeiche(..)
  , AnfrageKupplung(..)
  , AnfrageWegstrecke(..)) where

-- Bibliotheken
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Numeric.Natural (Natural)

-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (Anschluss(..))
import Zug.Enums (Zugtyp(..))
import Zug.Language (Anzeige(..), Sprache(..), ($#), (<^>), (<:>), (<\>), toBefehlsString)
import qualified Zug.Language as Language
import qualified Zug.Menge as Menge
import Zug.Objekt (Objekt, ObjektAllgemein(..))
import Zug.Plan (Plan, Plan(..))
import Zug.UI.Befehl (Befehl, BefehlAllgemein(..), UIBefehlAllgemein(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Lexer (EingabeTokenAllgemein(..), EingabeToken(..), leeresToken)
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), MitAnfrage(..), anzeigeMitAnfrage, anzeigeMitAnfrageFehlgeschlagen, StatusAnfrageObjekt(..)
      , statusAnfrageObjekt, zuObjekt, StatusAnfrageObjektZugtyp(..), statusAnfrageObjektZugtyp, ObjektZugtyp(..)
      , unbekanntShowText, zeigeAnfrageFehlgeschlagenStandard, wähleBefehl, wähleErgebnis, AnfrageFortsetzung(..)
      , verwendeAnfrageFortsetzung, ($<<))
import Zug.UI.Cmd.Parser.Plan
       (AnfragePlan(..), AnfrageAktion(..), AnfrageAktionBahngeschwindigkeit(..), AnfrageAktionStreckenabschnitt(..)
      , AnfrageAktionWeiche(..), AnfrageAktionKupplung(..), AnfrageAktionWegstrecke(..))
import Zug.UI.Cmd.Parser.StreckenObjekt (AnfrageObjekt(..), AnfrageBahngeschwindigkeit(..), AnfrageStreckenabschnitt(..)
                                       , AnfrageWeiche(..), AnfrageKupplung(..), AnfrageWegstrecke(..))

-- | Auswerten von Befehlen, so weit es ohne Status-Informationen möglich ist
parser :: AnfrageBefehl
       -> [EingabeTokenAllgemein]
       -> ( [Befehl]
          , AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
          , [EingabeTokenAllgemein]
          , AnfrageBefehl)
parser = parserAux []
    where
        parserAux :: [Befehl]
                  -> AnfrageBefehl
                  -> [EingabeTokenAllgemein]
                  -> ( [Befehl]
                     , AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
                     , [EingabeTokenAllgemein]
                     , AnfrageBefehl)
        parserAux acc anfrage@AnfrageBefehl [] = parserErgebnisOk acc anfrage
        parserAux acc anfrage@(ABAktionPlanAusführend plan Neu) [] =
            parserErgebnis acc [] anfrage $ AFZwischenwert $ ABAktionPlanAusführend plan Alt
        parserAux acc anfrage@(ABAktionPlanGesperrt plan Neu pins) [] =
            parserErgebnis acc [] anfrage $ AFZwischenwert $ ABAktionPlanGesperrt plan Alt pins
        parserAux acc anfrage@(ABAktionPlanAusführend plan Alt) [] =
            parserErgebnis acc [] anfrage $ AFErgebnis $ Left $ BSAusführenMöglich plan
        parserAux acc anfrage@(ABAktionPlanGesperrt plan Alt _pins) [] =
            parserErgebnis acc [] anfrage $ AFErgebnis $ Left $ BSAusführenMöglich plan
        parserAux acc anfrage [] = parserErgebnis acc [] anfrage $ AFZwischenwert anfrage
        parserAux acc anfrage (TkBeenden:_t) = parserErgebnisOk (UI Beenden : acc) anfrage
        parserAux acc anfrage (TkAbbrechen:_t) = parserErgebnisOk (UI Abbrechen : acc) anfrage
        parserAux acc anfrage (Tk h:t) = case anfrageAktualisieren anfrage h of
            (AFErgebnis (Left befehlSofort)) -> parserErgebnis acc t anfrage $ AFErgebnis $ Left befehlSofort
            (AFErgebnis (Right befehl)) -> parserAux (befehl : acc) AnfrageBefehl t
            (AFZwischenwert aBefehl) -> parserAux acc aBefehl t
            (AFFehler eingabe) -> parserErgebnis acc [] anfrage $ AFFehler eingabe
            statusAnfrage@AFStatusAnfrage {} -> parserErgebnis acc t anfrage statusAnfrage
            statusAnfrageMärklin@AFStatusAnfrageMärklin {} -> parserErgebnis acc t anfrage statusAnfrageMärklin
            statusAnfrageLego@AFStatusAnfrageLego {} -> parserErgebnis acc t anfrage statusAnfrageLego

        -- | Ergebnis zurückgeben
        parserErgebnis :: [Befehl]
                       -> [EingabeTokenAllgemein]
                       -> AnfrageBefehl
                       -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
                       -> ( [Befehl]
                          , AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
                          , [EingabeTokenAllgemein]
                          , AnfrageBefehl)
        parserErgebnis acc eingabeRest backup anfrageFortsetzung =
            (reverse acc, anfrageFortsetzung, eingabeRest, backup)

        parserErgebnisOk :: [Befehl]
                         -> AnfrageBefehl
                         -> ( [Befehl]
                            , AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
                            , [EingabeTokenAllgemein]
                            , AnfrageBefehl)
        parserErgebnisOk befehle backup = parserErgebnis befehle [] backup $ AFZwischenwert AnfrageBefehl

-- | Befehle, die sofort in 'IO' ausgeführt werden müssen
data BefehlSofort
    = BSLaden FilePath
    | BSAusführenMöglich Plan
    deriving (Eq, Show)

-- | Ist eine 'Anfrage' das erste mal zu sehen
data AnfrageNeu
    = Neu
    | Alt
    deriving (Eq, Show)

-- | Unvollständige Befehle
data AnfrageBefehl
    = AnfrageBefehl
    | ABSprache
    | ABHinzufügen AnfrageObjekt
    | ABEntfernen
    | ABSpeichern
    | ABLaden
    | ABAktionPlan Plan
    | ABAktionPlanAusführend Plan AnfrageNeu
    | ABAktionPlanGesperrt Plan AnfrageNeu (NonEmpty Anschluss)
    | ABAktion AnfrageAktion
    | ABStatusAnfrage (EingabeToken -> StatusAnfrageObjekt)
                      (Objekt -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))
    | ABStatusAnfrageMärklin (EingabeToken -> StatusAnfrageObjektZugtyp 'Märklin)
                              (ObjektZugtyp 'Märklin -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))
    | ABStatusAnfrageLego (EingabeToken -> StatusAnfrageObjektZugtyp 'Lego)
                          (ObjektZugtyp 'Lego -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))

instance Show AnfrageBefehl where
    show :: AnfrageBefehl -> String
    show = Text.unpack . flip anzeige Deutsch

instance Anzeige AnfrageBefehl where
    anzeige :: AnfrageBefehl -> Sprache -> Text
    anzeige AnfrageBefehl = Language.befehl
    anzeige ABSprache = Language.sprache
    anzeige (ABHinzufügen anfrageObjekt) = Language.hinzufügen <^> anfrageObjekt
    anzeige ABEntfernen = Language.entfernen
    anzeige ABSpeichern = Language.speichern
    anzeige ABLaden = Language.laden
    anzeige (ABAktionPlan plan) = Language.aktion <^> plan
    anzeige (ABAktionPlanAusführend plan _neu) = Language.wirdAusgeführt $# plan
    anzeige (ABAktionPlanGesperrt plan _neu pins) =
        (Language.ausführenGesperrt $# anzeige $ Menge.ausFoldable pins) <\> plan
    anzeige (ABAktion anfrageAktion) = anzeige anfrageAktion
    anzeige (ABStatusAnfrage anfrageKonstruktor _eitherF) = anzeige $ anfrageKonstruktor leeresToken
    anzeige (ABStatusAnfrageMärklin anfrageKonstruktor _eitherF) = anzeige $ anfrageKonstruktor leeresToken
    anzeige (ABStatusAnfrageLego anfrageKonstruktor _eitherF) = anzeige $ anfrageKonstruktor leeresToken

instance Anfrage AnfrageBefehl where
    zeigeAnfrage :: AnfrageBefehl -> Sprache -> Text
    zeigeAnfrage AnfrageBefehl = Language.befehl
    zeigeAnfrage ABSprache = Language.sprache
    zeigeAnfrage (ABHinzufügen anfrageObjekt) = zeigeAnfrage anfrageObjekt
    zeigeAnfrage ABEntfernen = Language.objekt
    zeigeAnfrage ABSpeichern = Language.dateiname
    zeigeAnfrage ABLaden = Language.dateiname
    zeigeAnfrage (ABAktionPlan _plan) = Language.aktion
    zeigeAnfrage anfrage@(ABAktionPlanAusführend _plan _neu) = anfrage <^> Language.aktion
    zeigeAnfrage anfrage@(ABAktionPlanGesperrt _plan _neu _pins) = anfrage <^> Language.aktion
    zeigeAnfrage (ABAktion anfrageAktion) = zeigeAnfrage anfrageAktion
    zeigeAnfrage (ABStatusAnfrage anfrageKonstruktor _eitherF) = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrage (ABStatusAnfrageMärklin anfrageKonstruktor _eitherF) = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrage (ABStatusAnfrageLego anfrageKonstruktor _eitherF) = zeigeAnfrage $ anfrageKonstruktor leeresToken

    zeigeAnfrageOptionen :: AnfrageBefehl -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen (ABHinzufügen anfrageObjekt) = zeigeAnfrageOptionen anfrageObjekt
    zeigeAnfrageOptionen ABSprache = Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) Language.alleSprachen
    zeigeAnfrageOptionen (ABAktionPlan _plan) = Just $ toBefehlsString . Language.aktionPlan
    zeigeAnfrageOptionen (ABAktionPlanAusführend _plan _neu) = Just $ toBefehlsString . Language.aktionPlanAusführend
    zeigeAnfrageOptionen (ABAktionPlanGesperrt _plan _neu _pins) = Just $ toBefehlsString . Language.aktionPlanGesperrt
    zeigeAnfrageOptionen (ABAktion anfrageAktion) = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (ABStatusAnfrage anfrageKonstruktor _eitherF) =
        zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen (ABStatusAnfrageMärklin anfrageKonstruktor _eitherF) =
        zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen (ABStatusAnfrageLego anfrageKonstruktor _eitherF) =
        zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage (Either BefehlSofort Befehl) where
    type AnfrageTyp (Either BefehlSofort Befehl) = AnfrageBefehl

    -- | Auswerten eines Zwischenergebnisses fortsetzen
    anfrageAktualisieren ::
                         AnfrageBefehl -> EingabeToken -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
    anfrageAktualisieren AnfrageBefehl token@EingabeToken {eingabe} =
        wähleBefehl
            token
            [ (Lexer.Beenden, AFErgebnis $ Right $ UI Beenden)
            , (Lexer.Hinzufügen, AFZwischenwert $ ABHinzufügen AnfrageObjekt)
            , (Lexer.Entfernen, AFZwischenwert ABEntfernen)
            , (Lexer.Speichern, AFZwischenwert ABSpeichern)
            , (Lexer.Laden, AFZwischenwert ABLaden)
            , (Lexer.Plan, AFZwischenwert $ ABStatusAnfrage SAOPlan planWählen)
            , (Lexer.Wegstrecke, anfrageAktualisieren (ABAktion AnfrageAktion) token)
            , (Lexer.Weiche, anfrageAktualisieren (ABAktion AnfrageAktion) token)
            , (Lexer.Bahngeschwindigkeit, anfrageAktualisieren (ABAktion AnfrageAktion) token)
            , (Lexer.Streckenabschnitt, anfrageAktualisieren (ABAktion AnfrageAktion) token)
            , (Lexer.Kupplung, anfrageAktualisieren (ABAktion AnfrageAktion) token)]
        $ AFFehler eingabe
        where
            planWählen :: Objekt -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
            planWählen (OPlan plan) = AFErgebnis $ Left $ BSAusführenMöglich plan
            planWählen
                objekt = error $ "planWählen erwartet einen Plan. Stattdessen \"" ++ show objekt ++ "\" erhalten."
    anfrageAktualisieren ABSprache token =
        wähleErgebnis
            token
            [(Lexer.Deutsch, Right $ SprachWechsel Deutsch), (Lexer.Englisch, Right $ SprachWechsel Englisch)]
    anfrageAktualisieren (ABHinzufügen anfrageObjekt) token =
        (AFErgebnis . Right . Hinzufügen, ABHinzufügen) $<< anfrageAktualisieren anfrageObjekt token
    anfrageAktualisieren ABEntfernen token@EingabeToken {eingabe} = case anfrageObjektExistierend token of
        Nothing -> AFFehler eingabe
        (Just anfrageKonstruktor)
            -> AFZwischenwert $ ABStatusAnfrage anfrageKonstruktor $ AFErgebnis . Right . Entfernen
    anfrageAktualisieren ABSpeichern EingabeToken {eingabe} = AFErgebnis $ Right $ Speichern $ Text.unpack eingabe
    anfrageAktualisieren ABLaden EingabeToken {eingabe} = AFErgebnis $ Left $ BSLaden $ Text.unpack eingabe
    anfrageAktualisieren (ABAktionPlan plan@Plan {plAktionen}) token =
        wähleErgebnis token [(Lexer.Ausführen, Right $ Ausführen plan zeigeFortschritt $ pure ())]
        where
            zeigeFortschritt :: Natural -> Sprache -> IO ()
            zeigeFortschritt i =
                Text.putStrLn . (plan <:> (toEnum (fromIntegral i) / toEnum (length plAktionen) :: Double))
    anfrageAktualisieren (ABAktionPlanAusführend plan _neu) token =
        wähleErgebnis token [(Lexer.AusführenAbbrechen, Right $ AusführenAbbrechen plan)]
    anfrageAktualisieren (ABAktionPlanGesperrt _plan _neu _pins) token@EingabeToken {eingabe} =
        wähleBefehl token [] $ AFFehler eingabe
    anfrageAktualisieren (ABAktion anfrageAktion) token =
        (AFErgebnis . Right . AktionBefehl, ABAktion) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (ABStatusAnfrage anfrageKonstruktor konstruktor) token =
        AFStatusAnfrage (anfrageKonstruktor token) konstruktor
    anfrageAktualisieren (ABStatusAnfrageMärklin anfrageKonstruktor konstruktor) token =
        AFStatusAnfrageMärklin (anfrageKonstruktor token) konstruktor
    anfrageAktualisieren (ABStatusAnfrageLego anfrageKonstruktor konstruktor) token =
        AFStatusAnfrageLego (anfrageKonstruktor token) konstruktor

-- | Eingabe eines existierendes Objekts
anfrageObjektExistierend :: EingabeToken -> Maybe (EingabeToken -> StatusAnfrageObjekt)
anfrageObjektExistierend token@EingabeToken {} =
    wähleBefehl
        token
        [ (Lexer.Plan, Just SAOPlan)
        , (Lexer.Wegstrecke, Just SAOWegstrecke)
        , (Lexer.Weiche, Just SAOWeiche)
        , (Lexer.Bahngeschwindigkeit, Just SAOBahngeschwindigkeit)
        , (Lexer.Streckenabschnitt, Just SAOStreckenabschnitt)
        , (Lexer.Kupplung, Just SAOKupplung)]
        Nothing