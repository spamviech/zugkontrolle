{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
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
module Zug.UI.Cmd.Parser (
    -- * Auswerten einer Text-Eingabe
    parser, statusAnfrageObjekt, statusAnfrageObjektZugtyp,
    -- * Ergebnis-Typen
    AnfrageErgebnis(..), AnfrageBefehl(..), BefehlSofort(..), AnfrageNeu(..),
    StatusAnfrageObjekt(..), StatusAnfrageObjektZugtyp(..), ObjektZugtyp(..), zuObjekt,
    -- ** Unvollständige StreckenObjekte
    Anfrage(..), MitAnfrage(..), zeigeAnfrageFehlgeschlagenStandard,
    showMitAnfrage, showMitAnfrageFehlgeschlagen, unbekanntShowText,
    AnfragePlan(..), AnfrageAktion(..), AnfrageAktionWegstrecke(..), AnfrageAktionWeiche(..),
    AnfrageAktionBahngeschwindigkeit(..), AnfrageAktionStreckenabschnitt(..), AnfrageAktionKupplung(..),
    AnfrageObjekt(..), AnfrageBahngeschwindigkeit(..), AnfrageStreckenabschnitt(..),
    AnfrageWeiche(..), AnfrageKupplung(..), AnfrageWegstrecke(..)) where

-- Bibliotheken
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (Anschluss(..))
import Zug.Klassen (Zugtyp(..))
import qualified Zug.Language as Language
import Zug.Language ((<^>), (<:>), (<\>), showText, toBefehlsString)
import qualified Zug.Menge as Menge
import Zug.Objekt (Objekt, ObjektAllgemein(..))
import Zug.Plan (Plan, Plan(..))
import Zug.UI.Befehl (Befehl, BefehlAllgemein(..), UIBefehlAllgemein(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Lexer (EingabeTokenAllgemein(..), EingabeToken(..), leeresToken)
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), MitAnfrage(..), showMitAnfrage, showMitAnfrageFehlgeschlagen,
                                StatusAnfrageObjekt(..), statusAnfrageObjekt, zuObjekt,
                                StatusAnfrageObjektZugtyp(..), statusAnfrageObjektZugtyp, ObjektZugtyp(..),
                                wähleBefehl, unbekanntShowText, zeigeAnfrageFehlgeschlagenStandard)
import Zug.UI.Cmd.Parser.Plan (AnfragePlan(..), AnfrageAktion(..),
                                AnfrageAktionBahngeschwindigkeit(..), AnfrageAktionStreckenabschnitt(..),
                                AnfrageAktionWeiche(..), AnfrageAktionKupplung(..), AnfrageAktionWegstrecke(..))
import Zug.UI.Cmd.Parser.StreckenObjekt (AnfrageObjekt(..), AnfrageBahngeschwindigkeit(..), AnfrageStreckenabschnitt(..),
                                        AnfrageWeiche(..), AnfrageKupplung(..), AnfrageWegstrecke(..))

-- | Auswerten von Befehlen, so weit es ohne Status-Informationen möglich ist
parser :: AnfrageBefehl -> [EingabeTokenAllgemein] -> ([Befehl], AnfrageErgebnis)
parser = parserAux []
    where
        parserAux :: [Befehl] -> AnfrageBefehl -> [EingabeTokenAllgemein] -> ([Befehl], AnfrageErgebnis)
        parserAux
            acc
            AnfrageBefehl
            []
                = parserErgebnisOk acc
        parserAux
            acc
            (ABAktionPlanAusführend plan Neu)
            []
                = (reverse acc, AEAnfrageBefehl (ABAktionPlanAusführend plan Alt))
        parserAux
            acc
            (ABAktionPlanGesperrt plan Neu pins)
            []
                = (reverse acc, AEAnfrageBefehl (ABAktionPlanGesperrt plan Alt pins))
        parserAux
            acc
            (ABAktionPlanAusführend plan Alt)
            []
                = (reverse acc, AEBefehlSofort (BSAusführenMöglich plan) [])
        parserAux
            acc
            (ABAktionPlanGesperrt plan Alt _pins)
            []
                = (reverse acc, AEBefehlSofort (BSAusführenMöglich plan) [])
        parserAux
            acc
            anfrage
            []
                = (reverse acc, AEAnfrageBefehl anfrage)
        parserAux
            acc
            _anfrage
            (TkBeenden:_t)
                = parserErgebnisOk (UI Beenden:acc)
        parserAux
            acc
            _anfrage
            (TkAbbrechen:_t)
                = parserErgebnisOk (UI Abbrechen:acc)
        parserAux
            acc
            anfrage
            ((Tk h):t)
                = case anfrageBefehlAktualisieren anfrage h of
                    (AEAnfrageBefehl qFehler@(ABUnbekannt _ab _b))
                        -> parserErgebnis acc $ AEAnfrageBefehl qFehler
                    (AEAnfrageBefehl qBefehl)
                        -> parserAux acc qBefehl t
                    (AEBefehlSofort eingabe r)
                        -> parserErgebnis acc $ AEBefehlSofort eingabe $ r ++ t
                    (AEStatusAnfrage eingabe konstruktor anfrage r)
                        -> parserErgebnis acc $ AEStatusAnfrage eingabe konstruktor anfrage $ r ++ t
                    (AEStatusAnfrageMärklin eingabe konstruktor anfrage r)
                        -> parserErgebnis acc $ AEStatusAnfrageMärklin eingabe konstruktor anfrage $ r ++ t
                    (AEStatusAnfrageLego eingabe konstruktor anfrage r)
                        -> parserErgebnis acc $ AEStatusAnfrageLego eingabe konstruktor anfrage $ r ++ t
                    (AEBefehl befehl)
                        -> parserAux (befehl:acc) AnfrageBefehl t
        -- | Ergebnis zurückgeben
        parserErgebnis :: [Befehl] -> AnfrageErgebnis -> ([Befehl], AnfrageErgebnis)
        parserErgebnis acc anfrage = (reverse acc, anfrage)
        parserErgebnisOk :: [Befehl] -> ([Befehl], AnfrageErgebnis)
        parserErgebnisOk    []                  = parserErgebnis [] $ AEAnfrageBefehl AnfrageBefehl
        parserErgebnisOk    (befehl:befehle)    = parserErgebnis befehle $ AEBefehl befehl

-- ** Anfrage
-- | Rückgabe-Typen
data AnfrageErgebnis
    = AEBefehl
        Befehl
    | AEBefehlSofort
        BefehlSofort                                -- ^ Sofort auszuführender Befehl (z.B. IO-Aktion)
        [EingabeTokenAllgemein]                     -- ^ Nachfolge-Token
    | AEStatusAnfrage
        StatusAnfrageObjekt                         -- ^ Wonach wird gefragt?
        (Objekt -> AnfrageErgebnis)                 -- ^ Wozu wird das Objekt benötigt
        AnfrageBefehl                               -- ^ Backup-Befehl, falls die Anfrage fehlschlägt
        [EingabeTokenAllgemein]                     -- ^ Nachfolge-Token
    | AEStatusAnfrageMärklin
        (StatusAnfrageObjektZugtyp 'Märklin)        -- ^ Wonach wird gefragt?
        (ObjektZugtyp 'Märklin -> AnfrageErgebnis)  -- ^ Wozu wird das Objekt benötigt
        AnfrageBefehl                               -- ^ Backup-Befehl, falls die Anfrage fehlschlägt
        [EingabeTokenAllgemein]                     -- ^ Nachfolge-Token
    | AEStatusAnfrageLego
        (StatusAnfrageObjektZugtyp 'Lego)           -- ^ Wonach wird gefragt?
        (ObjektZugtyp 'Lego -> AnfrageErgebnis)     -- ^ Wozu wird das Objekt benötigt
        AnfrageBefehl                               -- ^ Backup-Befehl, falls die Anfrage fehlschlägt
        [EingabeTokenAllgemein]                     -- ^ Nachfolge-Token
    | AEAnfrageBefehl
        AnfrageBefehl

-- | Befehle, die sofort in 'IO' ausgeführt werden müssen
data BefehlSofort   = BSLaden               FilePath
                    | BSAusführenMöglich    Plan

-- | Ist eine 'Anfrage' das erste mal zu sehen
data AnfrageNeu = Neu | Alt
            deriving (Show, Eq)

-- | Unvollständige Befehle
data AnfrageBefehl
    = AnfrageBefehl
    | ABUnbekannt
        AnfrageBefehl
        Text
    | ABHinzufügen
        AnfrageObjekt
    | ABEntfernen
    | ABSpeichern
    | ABLaden
    | ABAktionPlan
        Plan
    | ABAktionPlanAusführend
        Plan
        AnfrageNeu
    | ABAktionPlanGesperrt
        Plan
        AnfrageNeu
        (NonEmpty Anschluss)
    | ABAktion
        AnfrageAktion
    | ABStatusAnfrage
        (EingabeToken -> StatusAnfrageObjekt)
        (Objekt -> AnfrageErgebnis)
    | ABStatusAnfrageMärklin
        (EingabeToken -> StatusAnfrageObjektZugtyp 'Märklin)
        (ObjektZugtyp 'Märklin -> AnfrageErgebnis)
    | ABStatusAnfrageLego
        (EingabeToken -> StatusAnfrageObjektZugtyp 'Lego)
        (ObjektZugtyp 'Lego -> AnfrageErgebnis)

instance Show AnfrageBefehl where
    show :: AnfrageBefehl -> String
    show
        AnfrageBefehl
            = Language.befehl
    show
        (ABUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show
        (ABHinzufügen anfrageObjekt)
            = Language.hinzufügen <^> showText anfrageObjekt
    show
        ABEntfernen
            = Language.entfernen
    show
        ABSpeichern
            = Language.speichern
    show
        ABLaden
            = Language.laden
    show
        (ABAktionPlan plan)
            = Language.aktion <^> showText plan
    show
        (ABAktionPlanAusführend plan _neu)
            = Language.wirdAusgeführt $ showText plan
    show
        (ABAktionPlanGesperrt plan _neu pins)
            = Language.ausführenGesperrt (show $ Menge.ausFoldable pins) <\> showText plan
    show
        (ABAktion anfrageAktion)
            = showText anfrageAktion
    show
        (ABStatusAnfrage anfrageKonstruktor _eitherF)
            = showText $ anfrageKonstruktor leeresToken
    show
        (ABStatusAnfrageMärklin anfrageKonstruktor _eitherF)
            = showText $ anfrageKonstruktor leeresToken
    show
        (ABStatusAnfrageLego anfrageKonstruktor _eitherF)
            = showText $ anfrageKonstruktor leeresToken

instance Anfrage AnfrageBefehl where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageBefehl -> s
    zeigeAnfrage
        AnfrageBefehl
            = Language.befehl
    zeigeAnfrage
        (ABUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        (ABHinzufügen anfrageObjekt)
            = zeigeAnfrage anfrageObjekt
    zeigeAnfrage
        ABEntfernen
            = Language.objekt
    zeigeAnfrage
        ABSpeichern
            = Language.dateiname
    zeigeAnfrage
        ABLaden
            = Language.dateiname
    zeigeAnfrage
        (ABAktionPlan _plan)
            = Language.aktion
    zeigeAnfrage
        anfrage@(ABAktionPlanAusführend _plan _neu)
            = showText anfrage <^> Language.aktion
    zeigeAnfrage
        anfrage@(ABAktionPlanGesperrt _plan _neu _pins)
            = showText anfrage <^> Language.aktion
    zeigeAnfrage
        (ABAktion anfrageAktion)
            = zeigeAnfrage anfrageAktion
    zeigeAnfrage
        (ABStatusAnfrage anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrage
        (ABStatusAnfrageMärklin anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrage
        (ABStatusAnfrageLego anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageBefehl -> Maybe s
    zeigeAnfrageOptionen
        (ABUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        (ABHinzufügen anfrageObjekt)
            = zeigeAnfrageOptionen anfrageObjekt
    zeigeAnfrageOptionen
        (ABAktionPlan _plan)
            = Just $ toBefehlsString Language.aktionPlan
    zeigeAnfrageOptionen
        (ABAktionPlanAusführend _plan _neu)
            = Just $ toBefehlsString Language.aktionPlanAusführend
    zeigeAnfrageOptionen
        (ABAktionPlanGesperrt _plan _neu _pins)
            = Just $ toBefehlsString Language.aktionPlanGesperrt
    zeigeAnfrageOptionen
        (ABAktion anfrageAktion)
            = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen
        (ABStatusAnfrage anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen
        (ABStatusAnfrageMärklin anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen
        (ABStatusAnfrageLego anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Auswerten eines Zwischenergebnisses fortsetzen
anfrageBefehlAktualisieren :: AnfrageBefehl -> EingabeToken -> AnfrageErgebnis
anfrageBefehlAktualisieren
    anfrage@(ABUnbekannt _anfrage _eingabe)
    _token
        = AEAnfrageBefehl anfrage
anfrageBefehlAktualisieren
    AnfrageBefehl
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Beenden              , AEBefehl $ UI Beenden),
            (Lexer.Hinzufügen           , AEAnfrageBefehl $ ABHinzufügen AnfrageObjekt),
            (Lexer.Entfernen            , AEAnfrageBefehl ABEntfernen),
            (Lexer.Speichern            , AEAnfrageBefehl ABSpeichern),
            (Lexer.Laden                , AEAnfrageBefehl ABLaden),
            (Lexer.Plan                 , AEAnfrageBefehl $ ABStatusAnfrage SAOPlan planWählen),
            (Lexer.Wegstrecke           , anfrageBefehlAktualisieren (ABAktion AnfrageAktion) token),
            (Lexer.Weiche               , anfrageBefehlAktualisieren (ABAktion AnfrageAktion) token),
            (Lexer.Bahngeschwindigkeit  , anfrageBefehlAktualisieren (ABAktion AnfrageAktion) token),
            (Lexer.Streckenabschnitt    , anfrageBefehlAktualisieren (ABAktion AnfrageAktion) token),
            (Lexer.Kupplung             , anfrageBefehlAktualisieren (ABAktion AnfrageAktion) token)]
            $ AEAnfrageBefehl $ ABUnbekannt AnfrageBefehl eingabe
                where
                    planWählen :: Objekt -> AnfrageErgebnis
                    planWählen (OPlan plan) = AEBefehlSofort (BSAusführenMöglich plan) []
                    planWählen  objekt      = error $
                        "planWählen aus anfrageBefehlAktualisieren erwartet einen Plan. Stattdessen \"" ++
                        show objekt ++
                        "\" erhalten."
anfrageBefehlAktualisieren
    anfrage@(ABHinzufügen anfrageObjekt)
    token
        = case anfrageAktualisieren anfrageObjekt token of
            (Left (AOUnbekannt anfrage eingabe))
                -> AEAnfrageBefehl $ ABUnbekannt (ABHinzufügen anfrage) eingabe
            (Left (AOStatusAnfrage statusAnfrage (Left anfrageKonstruktor)))
                -> AEStatusAnfrage statusAnfrage (AEAnfrageBefehl . ABHinzufügen . anfrageKonstruktor) anfrage []
            (Left (AOStatusAnfrage statusAnfrage (Right konstruktor)))
                -> AEStatusAnfrage statusAnfrage (AEBefehl . Hinzufügen . konstruktor) anfrage []
            (Left (AOStatusAnfrageMärklin statusAnfrageMärklin (Left anfrageKonstruktor)))
                -> AEStatusAnfrageMärklin
                    statusAnfrageMärklin
                    (AEAnfrageBefehl . ABHinzufügen . anfrageKonstruktor)
                    anfrage
                    []
            (Left (AOStatusAnfrageMärklin statusAnfrageMärklin (Right konstruktor)))
                -> AEStatusAnfrageMärklin statusAnfrageMärklin (AEBefehl . Hinzufügen . zuObjekt . konstruktor) anfrage []
            (Left (AOStatusAnfrageLego statusAnfrageLego (Left anfrageKonstruktor)))
                -> AEStatusAnfrageLego statusAnfrageLego (AEAnfrageBefehl . ABHinzufügen . anfrageKonstruktor) anfrage []
            (Left (AOStatusAnfrageLego statusAnfrageLego (Right konstruktor)))
                -> AEStatusAnfrageLego statusAnfrageLego (AEBefehl . Hinzufügen . zuObjekt . konstruktor) anfrage []
            -- Kein Wildcard-Pattern, damit neu hinzugefügte Konstruktoren nicht vergessen werden können
            (Left qObjekt1@AnfrageObjekt)
                -> AEAnfrageBefehl $ ABHinzufügen qObjekt1
            (Left qObjekt1@(AOBahngeschwindigkeit _anfrageBahngeschwindigkeit))
                -> AEAnfrageBefehl $ ABHinzufügen qObjekt1
            (Left qObjekt1@(AOStreckenabschnitt _anfrageStreckenabschnitt))
                -> AEAnfrageBefehl $ ABHinzufügen qObjekt1
            (Left qObjekt1@(AOWeiche _anfrageWeiche))
                -> AEAnfrageBefehl $ ABHinzufügen qObjekt1
            (Left qObjekt1@(AOKupplung _anfrageKupplung))
                -> AEAnfrageBefehl $ ABHinzufügen qObjekt1
            (Left qObjekt1@(AOWegstrecke _anfrageWegstrecke))
                -> AEAnfrageBefehl $ ABHinzufügen qObjekt1
            (Left qObjekt1@(AOPlan _anfragePlan))
                -> AEAnfrageBefehl $ ABHinzufügen qObjekt1
            (Right objekt)
                -> AEBefehl $ Hinzufügen objekt
anfrageBefehlAktualisieren
    ABEntfernen
    token@EingabeToken {eingabe}
        = case anfrageObjektExistierend token of
            Nothing
                -> AEAnfrageBefehl $ ABUnbekannt ABEntfernen eingabe
            (Just anfrageKonstruktor)
                -> AEAnfrageBefehl $ ABStatusAnfrage anfrageKonstruktor $ AEBefehl . Entfernen
    where
        -- | Eingabe eines existierendes Objekts
        anfrageObjektExistierend :: EingabeToken -> Maybe (EingabeToken -> StatusAnfrageObjekt)
        anfrageObjektExistierend  token@EingabeToken {} = wähleBefehl token [
            (Lexer.Plan                  , Just SAOPlan),
            (Lexer.Wegstrecke            , Just SAOWegstrecke),
            (Lexer.Weiche                , Just SAOWeiche),
            (Lexer.Bahngeschwindigkeit   , Just SAOBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , Just SAOStreckenabschnitt),
            (Lexer.Kupplung              , Just SAOKupplung)]
            Nothing
anfrageBefehlAktualisieren
    ABSpeichern
    EingabeToken {eingabe}
        = AEBefehl $ Speichern $ unpack eingabe
anfrageBefehlAktualisieren
    ABLaden
    EingabeToken {eingabe}
        = AEBefehlSofort (BSLaden $ unpack eingabe) []
anfrageBefehlAktualisieren
    anfrage@(ABAktionPlan plan@Plan {plAktionen})
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Ausführen, AEBefehl $ Ausführen plan zeigeFortschritt $ pure ())]
            $ AEAnfrageBefehl $ ABUnbekannt anfrage eingabe
                where
                    zeigeFortschritt :: Natural -> IO ()
                    zeigeFortschritt i = putStrLn $
                        showText plan <:>
                        showText (toEnum (fromIntegral i) / toEnum (length plAktionen) :: Double)
anfrageBefehlAktualisieren
    (ABAktionPlanAusführend plan _neu)
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.AusführenAbbrechen, AEBefehl $ AusführenAbbrechen plan)]
            $ AEAnfrageBefehl $ ABUnbekannt (ABAktionPlanAusführend plan Alt) eingabe
anfrageBefehlAktualisieren
    (ABAktionPlanGesperrt plan _neu pins)
    token@EingabeToken {eingabe}
        = wähleBefehl token [] $ AEAnfrageBefehl $ ABUnbekannt (ABAktionPlanGesperrt plan Alt pins) eingabe
anfrageBefehlAktualisieren
    anfrage@(ABAktion anfrageAktion)
    token
        = case anfrageAktualisieren anfrageAktion token of
            (Left (AAUnbekannt anfrage eingabe))
                -> AEAnfrageBefehl $ ABUnbekannt (ABAktion anfrage) eingabe
            (Left (AAStatusAnfrage objektStatusAnfrage (Left anfrageKonstruktor)))
                -> AEStatusAnfrage  objektStatusAnfrage (AEAnfrageBefehl . ABAktion . anfrageKonstruktor) anfrage []
            (Left (AAStatusAnfrage objektStatusAnfrage (Right konstruktor)))
                -> AEStatusAnfrage objektStatusAnfrage (AEBefehl . AktionBefehl . konstruktor) anfrage []
            (Left anfrageAktion)
                -> AEAnfrageBefehl $ ABAktion anfrageAktion
            (Right aktion)
                -> AEBefehl $ AktionBefehl aktion
anfrageBefehlAktualisieren
    anfrage@(ABStatusAnfrage anfrageKonstruktor eitherF)
    token
        = AEStatusAnfrage (anfrageKonstruktor token) eitherF anfrage []
anfrageBefehlAktualisieren
    anfrage@(ABStatusAnfrageMärklin anfrageKonstruktor eitherF)
    token
        = AEStatusAnfrageMärklin (anfrageKonstruktor token) eitherF anfrage []
anfrageBefehlAktualisieren
    anfrage@(ABStatusAnfrageLego anfrageKonstruktor eitherF)
    token
        = AEStatusAnfrageLego (anfrageKonstruktor token) eitherF anfrage []