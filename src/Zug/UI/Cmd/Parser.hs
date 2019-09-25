{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description : Verarbeiten von Token.

In diesem Modul werden sämtliche Umwandlungen vorgenommen, für die nicht die IO-Monade benötigt wird.
-}
module Zug.UI.Cmd.Parser (
    -- * Auswerten einer Text-Eingabe
    parser, statusAnfrageObjekt,
    -- * Ergebnis-Typen
    AnfrageErgebnis(..), AnfrageBefehl(..), BefehlSofort(..), StatusAnfrageObjekt(..), AnfrageNeu(..),
    -- ** Unvollständige StreckenObjekte
    Anfrage(..), AnfrageFamilie(..), zeigeAnfrageFehlgeschlagenStandard,
    showMitAnfrage, showMitAnfrageFehlgeschlagen, unbekanntShowText,
    AnfragePlan(..), AnfrageAktion(..), AnfrageAktionWegstrecke(..), AnfrageAktionWeiche(..),
    AnfrageAktionBahngeschwindigkeit(..), AnfrageAktionStreckenabschnitt(..), AnfrageAktionKupplung(..),
    AnfrageObjekt(..), AnfrageWegstrecke(..), AnfrageWeiche(..), AnfrageBahngeschwindigkeit(..),
    AnfrageStreckenabschnitt(..), AnfrageKupplung(..)) where

-- Bibliotheken
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
import System.Hardware.WiringPi (Value(..))
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (Anschluss(..), StreckenObjekt(..), alleValues, zuPin,
                    Wegstrecke(..), WegstreckeKlasse(),
                    Weiche(..), WeicheKlasse(..),
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(),
                    Streckenabschnitt(..), StreckenabschnittKlasse(),
                    Kupplung(..), KupplungKlasse())
import Zug.Klassen (Richtung(..), unterstützteRichtungen, Fahrtrichtung(..), unterstützteFahrtrichtungen,
                    Zugtyp(..), ZugtypEither(..), unterstützteZugtypen , Strom(..))
import qualified Zug.Language as Language
import Zug.Language ((<^>), (<=>), (<->), (<|>), (<:>), (<\>), showText, fehlerText, toBefehlsString)
import qualified Zug.Menge as Menge
import Zug.Plan (Objekt, Plan, ObjektAllgemein(..), Plan(..), Aktion(..),
                AktionWegstrecke(..), AktionWeiche(..), AktionBahngeschwindigkeit(..),
                AktionStreckenabschnitt(..), AktionKupplung(..))
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes)
import Zug.UI.Base (MStatus, getPläne, getWegstrecken, getWeichen, getBahngeschwindigkeiten,
                    getStreckenabschnitte, getKupplungen)
import Zug.UI.Befehl (Befehl, BefehlAllgemein(..), UIBefehlAllgemein(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Lexer (EingabeTokenAllgemein(..), EingabeToken(..), Token(), leeresToken)
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), AnfrageFamilie, showMitAnfrage, showMitAnfrageFehlgeschlagen,
                                StatusAnfrageObjekt(..), statusAnfrageObjekt,
                                wähleBefehl, wähleRichtung, unbekanntShowText)
import Zug.UI.Cmd.Parser.Plan (AnfragePlan(..), anfragePlanAktualisieren,
                                AnfrageAktion(..), anfrageAktionAktualisieren,
                                AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..),
                                AktionWeiche(..), AktionKupplung(..), AktionWegstrecke(..))
import Zug.UI.Cmd.Parser.StreckenObjekt (AnfrageObjekt(..), anfrageObjektAktualisieren)

-- | Auswerten von Befehlen, so weit es ohne Status-Informationen möglich ist
parser :: (Show (AnfrageFamilie (ZugtypEither Weiche)))
    => AnfrageBefehl -> [EingabeTokenAllgemein] -> ([Befehl], AnfrageErgebnis)
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
                = case anfrageAktualisieren anfrage h of
                    (AEAnfrageBefehl qFehler@(ABUnbekannt _ab _b))
                        -> parserErgebnis acc $ AEAnfrageBefehl qFehler
                    (AEAnfrageBefehl qBefehl)
                        -> parserAux acc qBefehl t
                    (AEBefehlSofort eingabe r)
                        -> parserErgebnis acc $ AEBefehlSofort eingabe $ r ++ t
                    (AEStatusAnfrage eingabe konstruktor anfrage r)
                        -> parserErgebnis acc $ AEStatusAnfrage eingabe konstruktor anfrage $ r ++ t
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
        BefehlSofort                -- ^ Sofort auszuführender Befehl (z.B. IO-Aktion)
        [EingabeTokenAllgemein]     -- ^ Nachfolge-Token
    | AEStatusAnfrage
        StatusAnfrageObjekt         -- ^ Wonach wird gefragt?
        (Objekt -> AnfrageErgebnis) -- ^ Wozu wird das Objekt benötigt
        AnfrageBefehl               -- ^ Backup-Befehl, falls die Anfrage fehlschlägt
        [EingabeTokenAllgemein]     -- ^ Nachfolge-Token
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

type instance AnfrageFamilie Befehl = AnfrageBefehl

instance (Show (AnfrageFamilie (ZugtypEither Weiche))) => Show AnfrageBefehl where
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
instance (Show (AnfrageFamilie (ZugtypEither Weiche))) => Anfrage AnfrageBefehl where
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
        _anfrage
            = Nothing

-- | Auswerten eines Zwischenergebnisses fortsetzen
anfrageAktualisieren :: (Show (AnfrageFamilie (ZugtypEither Weiche)))
    => AnfrageBefehl -> EingabeToken -> AnfrageErgebnis
anfrageAktualisieren
    anfrage@(ABUnbekannt _anfrage _eingabe)
    _token
        = AEAnfrageBefehl anfrage
anfrageAktualisieren
    AnfrageBefehl
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Beenden              , AEBefehl $ UI Beenden),
            (Lexer.Hinzufügen           , AEAnfrageBefehl $ ABHinzufügen AnfrageObjekt),
            (Lexer.Entfernen            , AEAnfrageBefehl ABEntfernen),
            (Lexer.Speichern            , AEAnfrageBefehl ABSpeichern),
            (Lexer.Laden                , AEAnfrageBefehl ABLaden),
            (Lexer.Plan                 , AEAnfrageBefehl $ ABStatusAnfrage SAOPlan planWählen),
            (Lexer.Wegstrecke           , anfrageAktualisieren (ABAktion AnfrageAktion) token),
            (Lexer.Weiche               , anfrageAktualisieren (ABAktion AnfrageAktion) token),
            (Lexer.Bahngeschwindigkeit  , anfrageAktualisieren (ABAktion AnfrageAktion) token),
            (Lexer.Streckenabschnitt    , anfrageAktualisieren (ABAktion AnfrageAktion) token),
            (Lexer.Kupplung             , anfrageAktualisieren (ABAktion AnfrageAktion) token)]
            $ AEAnfrageBefehl $ ABUnbekannt AnfrageBefehl eingabe
                where
                    planWählen :: Objekt -> AnfrageErgebnis
                    planWählen (OPlan plan) = AEBefehlSofort (BSAusführenMöglich plan) []
                    planWählen  objekt      = error $
                        "planWählen aus anfrageAktualisieren erwartet einen Plan. Stattdessen \"" ++
                        show objekt ++
                        "\" erhalten."
anfrageAktualisieren
    anfrage@(ABHinzufügen anfrageObjekt)
    token
        = case anfrageObjektAktualisieren anfrageObjekt token of
            (Left (AOUnbekannt anfrage eingabe))
                -> AEAnfrageBefehl $ ABUnbekannt (ABHinzufügen anfrage) eingabe
            (Left (AOStatusAnfrage statusanfrage (Right konstruktor)))
                -> AEStatusAnfrage statusanfrage (AEBefehl . Hinzufügen . konstruktor) anfrage []
            (Left (AOStatusAnfrage statusanfrage (Left anfrageKonstruktor)))
                -> AEStatusAnfrage statusanfrage (AEAnfrageBefehl . ABHinzufügen . anfrageKonstruktor) anfrage []
            (Left qObjekt1)
                -> AEAnfrageBefehl $ ABHinzufügen qObjekt1
            (Right objekt)
                -> AEBefehl $ Hinzufügen objekt
anfrageAktualisieren
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
        anfrageObjektExistierend  token@(EingabeToken {}) = wähleBefehl token [
            (Lexer.Plan                  , Just SAOPlan),
            (Lexer.Wegstrecke            , Just SAOWegstrecke),
            (Lexer.Weiche                , Just SAOWeiche),
            (Lexer.Bahngeschwindigkeit   , Just SAOBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , Just SAOStreckenabschnitt),
            (Lexer.Kupplung              , Just SAOKupplung)]
            Nothing
anfrageAktualisieren
    ABSpeichern
    EingabeToken {eingabe}
        = AEBefehl $ Speichern $ unpack eingabe
anfrageAktualisieren
    ABLaden
    EingabeToken {eingabe}
        = AEBefehlSofort (BSLaden $ unpack eingabe) []
anfrageAktualisieren
    anfrage@(ABAktionPlan plan@(Plan {plAktionen}))
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Ausführen, AEBefehl $ Ausführen plan zeigeFortschritt $ pure ())]
            $ AEAnfrageBefehl $ ABUnbekannt anfrage eingabe
                where
                    zeigeFortschritt :: Natural -> IO ()
                    zeigeFortschritt i = putStrLn $
                        showText plan <:>
                        showText (toEnum (fromIntegral i) / toEnum (length plAktionen) :: Double)
anfrageAktualisieren
    (ABAktionPlanAusführend plan _neu)
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.AusführenAbbrechen, AEBefehl $ AusführenAbbrechen plan)]
            $ AEAnfrageBefehl $ ABUnbekannt (ABAktionPlanAusführend plan Alt) eingabe
anfrageAktualisieren
    (ABAktionPlanGesperrt plan _neu pins)
    token@EingabeToken {eingabe}
        = wähleBefehl token [] $ AEAnfrageBefehl $ ABUnbekannt (ABAktionPlanGesperrt plan Alt pins) eingabe
anfrageAktualisieren
    anfrage@(ABAktion anfrageAktion)
    token
        = case anfrageAktionAktualisieren anfrageAktion token of
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
anfrageAktualisieren
    anfrage@(ABStatusAnfrage anfrageKonstruktor eitherF)
    token
        = AEStatusAnfrage (anfrageKonstruktor token) eitherF anfrage []