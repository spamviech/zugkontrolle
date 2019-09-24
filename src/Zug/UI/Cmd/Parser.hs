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
                                StatusAnfrageObjekt(..), statusAnfrageObjekt)

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
anfrageAktualisieren :: AnfrageBefehl -> EingabeToken -> AnfrageErgebnis
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

-- ** Objekt
-- | Unvollständige Objekte
data AnfrageObjekt
    = AnfrageObjekt
    | AOUnbekannt
        AnfrageObjekt   -- ^ Anfrage
        Text            -- ^ Eingabe
    | AOPlan
        AnfragePlan
    | AOWegstrecke
        (ZugtypEither AnfrageWegstrecke)
    | AOWeiche
        (ZugtypEither AnfrageWeiche)
    | AOBahngeschwindigkeit
        (ZugtypEither AnfrageBahngeschwindigkeit)
    | AOStreckenabschnitt
        AnfrageStreckenabschnitt
    | AOKupplung
        AnfrageKupplung
    | AOStatusAnfrage
        StatusAnfrageObjekt
        (Either (Objekt -> AnfrageObjekt) (Objekt -> Objekt))

type instance AnfrageFamilie Objekt = AnfrageObjekt

instance Show AnfrageObjekt where
    show :: AnfrageObjekt -> String
    show
        (AOUnbekannt anfrageObjekt eingabe)
            = unpack $ unbekanntShowText anfrageObjekt eingabe
    show
        AnfrageObjekt
            = Language.objekt
    show
        (AOPlan aPlan)
            = showText aPlan
    show
        (AOWegstrecke qWegstrecke)
            = showText qWegstrecke
    show
        (AOWeiche aWeiche)
            = showText aWeiche
    show
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = showText aBahngeschwindigkeit
    show
        (AOStreckenabschnitt aStreckenabschnitt)
            = showText aStreckenabschnitt
    show
        (AOKupplung aKupplung)
            = showText aKupplung
    show
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = showText objektStatusAnfrage
instance Anfrage AnfrageObjekt where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageObjekt -> s
    zeigeAnfrage
        (AOUnbekannt anfrageObjekt _eingabe)
            = zeigeAnfrage anfrageObjekt
    zeigeAnfrage
        (AnfrageObjekt)
            = Language.objekt
    zeigeAnfrage
        (AOPlan aPlan)
            = zeigeAnfrage aPlan
    zeigeAnfrage
        (AOWegstrecke qWegstrecke)
            = zeigeAnfrage qWegstrecke
    zeigeAnfrage
        (AOWeiche aWeiche)
            = zeigeAnfrage aWeiche
    zeigeAnfrage
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = zeigeAnfrage aBahngeschwindigkeit
    zeigeAnfrage
        (AOStreckenabschnitt aStreckenabschnitt)
            = zeigeAnfrage aStreckenabschnitt
    zeigeAnfrage
        (AOKupplung aKupplung)
            = zeigeAnfrage aKupplung
    zeigeAnfrage
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageObjekt -> Maybe s
    zeigeAnfrageOptionen
        (AOUnbekannt anfrageObjekt _eingabe)
            = zeigeAnfrageOptionen anfrageObjekt
    zeigeAnfrageOptionen
        AnfrageObjekt
            = Just $ toBefehlsString Language.befehlTypen
    zeigeAnfrageOptionen
        (AOPlan aPlan)
            = zeigeAnfrageOptionen aPlan
    zeigeAnfrageOptionen
        (AOWegstrecke qWegstrecke)
            = zeigeAnfrageOptionen qWegstrecke
    zeigeAnfrageOptionen
        (AOWeiche aWeiche)
            = zeigeAnfrageOptionen aWeiche
    zeigeAnfrageOptionen
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = zeigeAnfrageOptionen aBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AOStreckenabschnitt aStreckenabschnitt)
            = zeigeAnfrageOptionen aStreckenabschnitt
    zeigeAnfrageOptionen
        (AOKupplung aKupplung)
            = zeigeAnfrageOptionen aKupplung
    zeigeAnfrageOptionen
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage

-- | Bekannte Teil-Typen einer 'Wegstrecke'
data AnfrageWegstreckenElement
    = AWSEUnbekannt
        Text
    | AWSEWeiche
    | AWSEBahngeschwindigkeit
    | AWSEStreckenabschnitt
    | AWSEKupplung

-- | Eingabe eines Objekts
anfrageObjektAktualisieren :: AnfrageObjekt -> EingabeToken -> Either AnfrageObjekt Objekt
anfrageObjektAktualisieren
    qFehler@(AOUnbekannt _anfrage _eingabe)
    _token
        = Left qFehler
anfrageObjektAktualisieren
    anfrageObjekt@(AOStatusAnfrage _ _)
    _token
        = Left anfrageObjekt
anfrageObjektAktualisieren
    AnfrageObjekt
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Plan                  , Left $ AOPlan AnfragePlan),
            (Lexer.Wegstrecke            , Left $ AOWegstrecke AnfrageWegstrecke),
            (Lexer.Weiche                , Left $ AOWeiche AnfrageWeiche),
            (Lexer.Bahngeschwindigkeit   , Left $ AOBahngeschwindigkeit AnfrageBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , Left $ AOStreckenabschnitt AnfrageStreckenabschnitt),
            (Lexer.Kupplung              , Left $ AOKupplung AnfrageKupplung)]
            $ Left $ AOUnbekannt AnfrageObjekt eingabe
anfrageObjektAktualisieren
    (AOPlan aPlan)
    token
        = case anfragePlanAktualisieren aPlan token of
            (Left (APUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOPlan anfrage) eingabe1
            (Left (APlanIOStatus objektStatusAnfrage (Right konstruktor)))
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Right $
                    \objekt -> OPlan $ konstruktor objekt
            (Left (APlanIOStatus objektStatusAnfrage (Left anfrageKonstruktor)))
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Left $
                    \objekt -> AOPlan $ anfrageKonstruktor objekt
            (Left aPlan1)
                -> Left $ AOPlan aPlan1
            (Right plan)
                -> Right $ OPlan plan
anfrageObjektAktualisieren
    (AOWegstrecke aWegstrecke0)
    token
        = case anfrageWegstreckeAktualisieren aWegstrecke0 token of
            (Left (AWSUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOWegstrecke anfrage) eingabe1
            (Left (AWegstreckeRStatus objektStatusAnfrage (Right konstruktor)))
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Right $
                    \objekt -> OWegstrecke $ konstruktor objekt
            (Left (AWegstreckeRStatus objektStatusAnfrage (Left anfrageKonstruktor)))
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Left $
                    \objekt -> AOWegstrecke $ anfrageKonstruktor objekt
            (Left aWegstrecke1)
                -> Left $ AOWegstrecke aWegstrecke1
            (Right wegstrecke)
                -> Right $ OWegstrecke wegstrecke
anfrageObjektAktualisieren
    (AOWeiche aWeiche)
    token
        = case anfrageWeicheAktualisieren aWeiche token of
            (Left (AWEUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOWeiche anfrage) eingabe1
            (Left aWeiche1)
                -> Left $ AOWeiche aWeiche1
            (Right weiche)
                -> Right $ OWeiche weiche
anfrageObjektAktualisieren
    (AOBahngeschwindigkeit aBahngeschwindigkeit)
    token
        = case anfrageBahngeschwindigkeitAktualisieren aBahngeschwindigkeit token of
            (Left (ABGUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOBahngeschwindigkeit anfrage) eingabe1
            (Left aBahngeschwindigkeit1)
                -> Left $ AOBahngeschwindigkeit aBahngeschwindigkeit1
            (Right bahngeschwindigkeit)
                -> Right $ OBahngeschwindigkeit bahngeschwindigkeit
anfrageObjektAktualisieren
    (AOStreckenabschnitt aStreckenabschnitt)
    token
        = case anfrageStreckenabschnittAktualisieren aStreckenabschnitt token of
            (Left (ASTUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOStreckenabschnitt anfrage) eingabe1
            (Left aStreckenabschnitt1)
                -> Left $ AOStreckenabschnitt aStreckenabschnitt1
            (Right streckenabschnitt)
                -> Right $ OStreckenabschnitt streckenabschnitt
anfrageObjektAktualisieren
    (AOKupplung aKupplung)
    token
        = case anfrageKupplungAktualisieren aKupplung token of
            (Left (AKUUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOKupplung anfrage) eingabe1
            (Left aKupplung1)
                -> Left $ AOKupplung aKupplung1
            (Right kupplung)
                -> Right $ OKupplung kupplung

-- ** Plan
-- | Unvollständiger 'Plan'
data AnfragePlan
    = AnfragePlan
    | APUnbekannt
        AnfragePlan             -- ^ Anfrage
        Text                    -- ^ Eingabe
    | APlanName
        Text                    -- ^ Name
    | APlanNameAnzahl
        Text                    -- ^ Name
        Natural                 -- ^ Verbleibende Aktionen
        (Warteschlange Aktion)  -- ^ Bekannte Aktionen
        AnfrageAktion           -- ^ Nächste Aktion
    | APlanIOStatus
        StatusAnfrageObjekt
        (Either (Objekt -> AnfragePlan) (Objekt -> Plan))
    | APStatusAnfrage
        (EingabeToken -> StatusAnfrageObjekt)
        (Either (Objekt -> AnfragePlan) (Objekt -> Plan))

type instance AnfrageFamilie Plan = AnfragePlan

instance Show AnfragePlan where
    show :: AnfragePlan -> String
    show
        (APUnbekannt aPlan eingabe)
            = unpack $ unbekanntShowText aPlan eingabe
    show
        AnfragePlan
            = Language.plan
    show
        (APlanName name)
            = unpack $ Language.plan <^> Language.name <=> name
    show
        (APlanNameAnzahl name anzahl acc anfrageAktion)
            = unpack $ Language.plan
                <^> Language.name <=> name
                <^> Language.anzahl Language.aktionen <=> showText anzahl
                <^> showText acc
                <^> showText anfrageAktion
    show
        (APlanIOStatus objektStatusAnfrage _eitherKonstruktor)
            = Language.plan <^> showText objektStatusAnfrage
    show
        (APStatusAnfrage anfrageKonstruktor _eitherF)
            = Language.plan
                <^> Language.aktion <-> Language.objekt
                <^> showText (anfrageKonstruktor leeresToken)
instance Anfrage AnfragePlan where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfragePlan -> s
    zeigeAnfrage
        (APUnbekannt aPlan _eingabe)
            = zeigeAnfrage aPlan
    zeigeAnfrage
        AnfragePlan
            = Language.name
    zeigeAnfrage
        (APlanName _name)
            = Language.anzahl Language.aktionen
    zeigeAnfrage
        (APlanNameAnzahl _name _anzahl _acc anfrageAktion)
            = zeigeAnfrage anfrageAktion
    zeigeAnfrage
        (APlanIOStatus objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage
        (APStatusAnfrage anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfragePlan -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(APlanName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfragePlan -> Maybe s
    zeigeAnfrageOptionen
        (APUnbekannt aPlan _eingabe)
            = zeigeAnfrageOptionen aPlan
    zeigeAnfrageOptionen
        AnfragePlan
            = Nothing
    zeigeAnfrageOptionen
        (APlanName _name)
            = Nothing
    zeigeAnfrageOptionen
        (APlanNameAnzahl _name _anzahl _acc anfrageAktion)
            = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen
        (APlanIOStatus objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen
        (APStatusAnfrage anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken

-- | Eingabe eines Plans
anfragePlanAktualisieren :: AnfragePlan -> EingabeToken -> Either AnfragePlan Plan
anfragePlanAktualisieren
    AnfragePlan
    EingabeToken {eingabe}
        = Left $ APlanName eingabe
anfragePlanAktualisieren
    anfrage@(APlanName name)
    EingabeToken {eingabe, ganzzahl}
        = Left $ case ganzzahl of
            Nothing       -> APUnbekannt anfrage eingabe
            (Just anzahl)   -> APlanNameAnzahl name anzahl leer AnfrageAktion
anfragePlanAktualisieren
    (APlanNameAnzahl name anzahl acc anfrageAktion)
    token
        = case anfrageAktionAktualisieren anfrageAktion token of
            (Left (AAUnbekannt aAktion1 eingabe))
                -> Left $ APUnbekannt (APlanNameAnzahl name anzahl acc aAktion1) eingabe
            (Left (AAStatusAnfrage objektStatusAnfrage (Left anfrageKonstruktor)))
                -> Left $ APlanIOStatus objektStatusAnfrage $
                    Left $ \objekt -> APlanNameAnzahl name anzahl acc $ anfrageKonstruktor objekt
            (Left (AAStatusAnfrage objektStatusAnfrage (Right konstruktor)))
                -> Left $ APlanIOStatus objektStatusAnfrage $ if anzahl > 1
                    then Left $ \objekt -> APlanNameAnzahl name anzahl (anhängen (konstruktor objekt) acc) AnfrageAktion
                    else Right $ \objekt -> Plan {plName=name, plAktionen=toList $ anhängen (konstruktor objekt) acc}
            (Left AARückgängig)
                -> Left $ APlanNameAnzahl name (succ anzahl) (löscheLetztes acc) AnfrageAktion
            (Left aAktion1)
                -> Left $ APlanNameAnzahl name anzahl acc aAktion1
            (Right aktion)
                | anzahl > 1
                    -> Left $ APlanNameAnzahl name (pred anzahl) (anhängen aktion acc) AnfrageAktion
                | otherwise
                    -> Right $ Plan {plName=name, plAktionen=toList $ anhängen aktion acc}
        where
            löscheLetztes :: Warteschlange a -> Warteschlange a
            löscheLetztes warteschlange = case zeigeLetztes warteschlange of
                Leer            -> p
                (Gefüllt _l p)  -> p
anfragePlanAktualisieren
    (APStatusAnfrage anfrageKonstruktor eitherF)
    token
        = Left $ APlanIOStatus (anfrageKonstruktor token) eitherF
anfragePlanAktualisieren
    anfrage
    _token
        = Left anfrage

-- *** Aktion
-- | Unvollständige 'Aktion'
data AnfrageAktion
    = AnfrageAktion
    | AAUnbekannt
        AnfrageAktion   -- ^ Anfrage
        Text            -- ^ Eingabe
    | AARückgängig
    | AAWarten
    | AAWegstreckeMärklin
        (AnfrageAktionWegstrecke Wegstrecke 'Märklin)
    | AAWegstreckeLego
        (AnfrageAktionWegstrecke Wegstrecke 'Lego)
    | AAWeiche
        (AnfrageAktionWeiche (ZugtypEither Weiche))
    | AABahngeschwindigkeitMärklin
        (AnfrageAktionBahngeschwindigkeit Bahngeschwindigkeit 'Märklin)
    | AABahngeschwindigkeitLego
        (AnfrageAktionBahngeschwindigkeit Bahngeschwindigkeit 'Lego)
    | AAStreckenabschnitt
        (AnfrageAktionStreckenabschnitt Streckenabschnitt)
    | AAKupplung
        (AnfrageAktionKupplung Kupplung)
    | AAStatusAnfrage
        StatusAnfrageObjekt
        (Either (Objekt -> AnfrageAktion) (Objekt -> Aktion))
    | AAKlassifizierung
        (EingabeToken -> StatusAnfrageObjekt)
        (Either (Objekt -> AnfrageAktion) (Objekt -> Aktion))

type instance AnfrageFamilie Aktion = AnfrageAktion

instance Show AnfrageAktion where
    show :: AnfrageAktion -> String
    show
        (AAUnbekannt anfrageAktion eingabe)
            = unpack $ unbekanntShowText anfrageAktion eingabe
    show
        AnfrageAktion
            = Language.aktion
    show
        AARückgängig
            = Language.rückgängig
    show
        AAWarten
            = Language.aktion <^> Language.warten
    show
        (AAWegstreckeMärklin qAktionWegstrecke)
            = Language.aktion <^> showText qAktionWegstrecke
    show
        (AAWegstreckeLego qAktionWegstrecke)
            = Language.aktion <^> showText qAktionWegstrecke
    show
        (AAWeiche qAktionWeiche)
            = Language.aktion <^> showText qAktionWeiche
    show
        (AABahngeschwindigkeitMärklin qAktionBahngeschwindigkeit)
            = Language.aktion <^> showText qAktionBahngeschwindigkeit
    show
        (AABahngeschwindigkeitLego qAktionBahngeschwindigkeit)
            = Language.aktion <^> showText qAktionBahngeschwindigkeit
    show
        (AAStreckenabschnitt qAktionStreckenabschnitt)
            = Language.aktion <^> showText qAktionStreckenabschnitt
    show
        (AAKupplung qAktionKupplung)
            = Language.aktion <^> showText qAktionKupplung
    show
        (AAStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = Language.aktion <^> showText objektStatusAnfrage
    show
        (AAKlassifizierung anfrageKonstruktor _eitherF)
            = Language.aktion <-> Language.objekt <^> showText (anfrageKonstruktor leeresToken)
instance Anfrage AnfrageAktion where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktion -> s
    zeigeAnfrage
        (AAUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrage anfrageAktion
    zeigeAnfrage
        AnfrageAktion
            = Language.aktion
    zeigeAnfrage
        AARückgängig
            = Language.aktion
    zeigeAnfrage
        AAWarten
            = Language.zeit
    zeigeAnfrage
        (AAWegstreckeMärklin qAktionWegstrecke)
            = zeigeAnfrage qAktionWegstrecke
    zeigeAnfrage
        (AAWegstreckeLego qAktionWegstrecke)
            = zeigeAnfrage qAktionWegstrecke
    zeigeAnfrage
        (AAWeiche qAktionWeiche)
            = zeigeAnfrage qAktionWeiche
    zeigeAnfrage
        (AABahngeschwindigkeitMärklin qAktionBahngeschwindigkeit)
            = zeigeAnfrage qAktionBahngeschwindigkeit
    zeigeAnfrage
        (AABahngeschwindigkeitLego qAktionBahngeschwindigkeit)
            = zeigeAnfrage qAktionBahngeschwindigkeit
    zeigeAnfrage
        (AAStreckenabschnitt qAktionStreckenabschnitt)
            = zeigeAnfrage qAktionStreckenabschnitt
    zeigeAnfrage
        (AAKupplung qAktionKupplung)
            = zeigeAnfrage qAktionKupplung
    zeigeAnfrage
        (AAStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage
        (AAKlassifizierung anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageAktion -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@AAWarten
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktion -> Maybe s
    zeigeAnfrageOptionen
        (AAUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen
        AnfrageAktion
            = Just $ toBefehlsString Language.aktionGruppen
    zeigeAnfrageOptionen
        AARückgängig
            = Nothing
    zeigeAnfrageOptionen
        AAWarten
            = Nothing
    zeigeAnfrageOptionen
        (AAWegstreckeMärklin qAktionWegstrecke)
            = zeigeAnfrageOptionen qAktionWegstrecke
    zeigeAnfrageOptionen
        (AAWegstreckeLego qAktionWegstrecke)
            = zeigeAnfrageOptionen qAktionWegstrecke
    zeigeAnfrageOptionen
        (AAWeiche qAktionWeiche)
            = zeigeAnfrageOptionen qAktionWeiche
    zeigeAnfrageOptionen
        (AABahngeschwindigkeitMärklin qAktionBahngeschwindigkeit)
            = zeigeAnfrageOptionen qAktionBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AABahngeschwindigkeitLego qAktionBahngeschwindigkeit)
            = zeigeAnfrageOptionen qAktionBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AAStreckenabschnitt qAktionStreckenabschnitt)
            = zeigeAnfrageOptionen qAktionStreckenabschnitt
    zeigeAnfrageOptionen
        (AAKupplung qAktionKupplung)
            = zeigeAnfrageOptionen qAktionKupplung
    zeigeAnfrageOptionen
        (AAStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen
        (AAKlassifizierung anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken

-- | 'Aktion'-Klassifizierungen
data AnfrageAktionElement
    = AAEUnbekannt
        Text
    | AAERückgängig
    | AAEWarten
    | AAEWegstrecke
    | AAEWeiche
    | AAEBahngeschwindigkeit
    | AAEStreckenabschnitt
    | AAEKupplung

-- | Eingabe einer 'Aktion'
anfrageAktionAktualisieren :: AnfrageAktion -> EingabeToken -> Either AnfrageAktion Aktion
anfrageAktionAktualisieren
    AnfrageAktion
    token
        = Left $ case anfrageAktionElement token of
            (AAEUnbekannt eingabe)
                -> AAUnbekannt AnfrageAktion eingabe
            AAERückgängig
                -> AARückgängig
            AAEWarten
                -> AAWarten
            AAEWegstrecke
                -> AAKlassifizierung SAOWegstrecke $ Left $
                    \(OWegstrecke wegstrecke) -> AAWegstrecke $ AnfrageAktionWegstrecke wegstrecke
            AAEWeiche
                -> AAKlassifizierung SAOWeiche $ Left $ \(OWeiche weiche) -> AAWeiche $ AnfrageAktionWeiche weiche
            AAEBahngeschwindigkeit
                -> AAKlassifizierung SAOBahngeschwindigkeit $ Left $
                \(OBahngeschwindigkeit bahngeschwindigkeit)
                    -> AABahngeschwindigkeit $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
            AAEStreckenabschnitt
                -> AAKlassifizierung SAOStreckenabschnitt $ Left $
                    \(OStreckenabschnitt streckenabschnitt)
                        -> AAStreckenabschnitt $ AnfrageAktionStreckenabschnitt streckenabschnitt
            AAEKupplung
                -> AAKlassifizierung SAOKupplung $ Left $
                    \(OKupplung kupplung) -> AAKupplung $ AnfrageAktionKupplung kupplung
    where
        anfrageAktionElement :: EingabeToken -> AnfrageAktionElement
        anfrageAktionElement  token@EingabeToken {eingabe}  = wähleBefehl token [
            (Lexer.Rückgängig           , AAERückgängig),
            (Lexer.Warten               , AAEWarten),
            (Lexer.Wegstrecke           , AAEWegstrecke),
            (Lexer.Weiche               , AAEWeiche),
            (Lexer.Bahngeschwindigkeit  , AAEBahngeschwindigkeit),
            (Lexer.Streckenabschnitt    , AAEStreckenabschnitt),
            (Lexer.Kupplung             , AAEKupplung)]
            $ AAEUnbekannt eingabe
anfrageAktionAktualisieren
    _anfrage
    EingabeToken {möglichkeiten}
        | elem Lexer.Rückgängig möglichkeiten
            = Left AnfrageAktion
anfrageAktionAktualisieren
    AAWarten
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing   -> Left $ AAUnbekannt AAWarten eingabe
            (Just zeit) -> Right $ Warten zeit
anfrageAktionAktualisieren
    (AAKlassifizierung anfrageKonstruktor eitherF)
    token
        = Left $ AAStatusAnfrage (anfrageKonstruktor token) eitherF
anfrageAktionAktualisieren
    (AAWegstreckeMärklin anfrageAktion)
    token
        = case anfrageAktionWegstreckeAktualisieren anfrageAktion token of
            (Left (AAWSUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AAWegstrecke anfrage) eingabe
            (Left qAktionWegstrecke)
                -> Left $ AAWegstrecke qAktionWegstrecke
            (Right aktionWegstrecke)
                -> Right $ AWegstrecke aktionWegstrecke
anfrageAktionAktualisieren
    (AAWegstreckeLego anfrageAktion)
    token
        = case anfrageAktionWegstreckeAktualisieren anfrageAktion token of
            (Left (AAWSUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AAWegstrecke anfrage) eingabe
            (Left qAktionWegstrecke)
                -> Left $ AAWegstrecke qAktionWegstrecke
            (Right aktionWegstrecke)
                -> Right $ AWegstrecke aktionWegstrecke
anfrageAktionAktualisieren
    (AAWeiche anfrageAktion)
    token
        = case anfrageAktionWeicheAktualisieren anfrageAktion token of
            (Left (AAWUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AAWeiche anfrage) eingabe
            (Left qAktionWeiche)
                -> Left $ AAWeiche qAktionWeiche
            (Right aktionWeiche)
                -> Right $ AWeiche aktionWeiche
anfrageAktionAktualisieren
    (AABahngeschwindigkeitMärklin anfrageAktion)
    token
        = case anfrageAktionBahngeschwindigkeitAktualisieren anfrageAktion token of
            (Left (AABGUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AABahngeschwindigkeit anfrage) eingabe
            (Left qAktionBahngeschwindigkeit)
                -> Left $ AABahngeschwindigkeit qAktionBahngeschwindigkeit
            (Right aktionBahngeschwindigkeit)
                -> Right $ ABahngeschwindigkeit aktionBahngeschwindigkeit
anfrageAktionAktualisieren
    (AABahngeschwindigkeitLego anfrageAktion)
    token
        = case anfrageAktionBahngeschwindigkeitAktualisieren anfrageAktion token of
            (Left (AABGUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AABahngeschwindigkeit anfrage) eingabe
            (Left qAktionBahngeschwindigkeit)
                -> Left $ AABahngeschwindigkeit qAktionBahngeschwindigkeit
            (Right aktionBahngeschwindigkeit)
                -> Right $ ABahngeschwindigkeit aktionBahngeschwindigkeit
anfrageAktionAktualisieren
    (AAStreckenabschnitt anfrageAktion)
    token
        = case anfrageAktionStreckenabschnittAktualisieren anfrageAktion token of
            (Left (AASTUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AAStreckenabschnitt anfrage) eingabe
            (Left qAktionStreckenabschnitt)
                -> Left $ AAStreckenabschnitt qAktionStreckenabschnitt
            (Right aktionStreckenabschnitt)
                -> Right $ AStreckenabschnitt aktionStreckenabschnitt
anfrageAktionAktualisieren
    (AAKupplung anfrageAktion)
    token
        = case anfrageAktionKupplungAktualisieren anfrageAktion token of
            (Left (AAKUUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AAKupplung anfrage) eingabe
            (Left qAktionKupplung)
                -> Left $ AAKupplung qAktionKupplung
            (Right aktionKupplung)
                -> Right $ AKupplung aktionKupplung
anfrageAktionAktualisieren
    anfrage
    _token
        = Left anfrage

-- *** Wegstrecken-Aktion
-- | Unvollständige 'Aktion' einer 'Wegstrecke'
data AnfrageAktionWegstrecke w (z :: Zugtyp)
    = AnfrageAktionWegstrecke
        (w z)                                   -- ^ Wegstrecke
    | AAWSUnbekannt
        (AnfrageAktionWegstrecke w z)           -- ^ Anfrage
        Text                                    -- ^ Eingabe
    | AAWSBahngeschwindigkeit
        (AnfrageAktionBahngeschwindigkeit w z)
    | AAWSStreckenabschnitt
        (AnfrageAktionStreckenabschnitt (w z))
    | AAWSKupplung
        (AnfrageAktionKupplung (w z))

type instance AnfrageFamilie (AktionWegstrecke w z) = AnfrageAktionWegstrecke w z

instance (Show (AnfrageFamilie (w z)), Show (w z)) => Show (AnfrageAktionWegstrecke w z) where
    show :: AnfrageAktionWegstrecke aw w -> String
    show    (AnfrageAktionWegstrecke wegstrecke)      = Language.wegstrecke <=> showText wegstrecke
    show    (AAWSUnbekannt anfrageAktion eingabe)     = unpack $ unbekanntShowText anfrageAktion eingabe
    show    (AAWSBahngeschwindigkeit anfrageAktion)   = showText anfrageAktion
    show    (AAWSStreckenabschnitt anfrageAktion)     = showText anfrageAktion
    show    (AAWSKupplung anfrageAktion)              = showText anfrageAktion
instance Anfrage (AnfrageAktionWegstrecke w z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionWegstrecke w z -> s
    zeigeAnfrage    (AnfrageAktionWegstrecke _wegstrecke)     = Language.aktion
    zeigeAnfrage    (AAWSUnbekannt anfrageAktion _eingabe)    = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWSBahngeschwindigkeit anfrageAktion)   = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWSStreckenabschnitt anfrageAktion)     = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWSKupplung anfrageAktion)              = zeigeAnfrage anfrageAktion
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionWegstrecke w z -> Maybe s
    zeigeAnfrageOptionen (AnfrageAktionWegstrecke _wegstrecke)     = Just $ toBefehlsString Language.aktionWegstrecke
    zeigeAnfrageOptionen (AAWSUnbekannt anfrageAktion _eingabe)    = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSBahngeschwindigkeit anfrageAktion)   = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSStreckenabschnitt anfrageAktion)     = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSKupplung anfrageAktion)              = zeigeAnfrageOptionen anfrageAktion

-- | Eingabe einer Wegstrecken-Aktion
anfrageAktionWegstreckeAktualisieren :: (WegstreckeKlasse (w z))
    => AnfrageAktionWegstrecke w z -> EingabeToken -> Either (AnfrageAktionWegstrecke w z) (AktionWegstrecke w z)
anfrageAktionWegstreckeAktualisieren
    anfrage@(AAWSUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage
anfrageAktionWegstreckeAktualisieren
    anfrage@(AnfrageAktionWegstrecke wegstrecke)
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Einstellen       , Right $ Einstellen wegstrecke),
            (Lexer.Geschwindigkeit  , Left $ AAWSBahngeschwindigkeit $ AABGGeschwindigkeit wegstrecke),
            (Lexer.Umdrehen         , Left $ AAWSBahngeschwindigkeit $ AABGUmdrehen wegstrecke),
            (Lexer.Strom            , Left $ AAWSStreckenabschnitt $ AASTStrom wegstrecke),
            (Lexer.Kuppeln          , Right $ AWSKupplung $ Kuppeln wegstrecke)]
            $ Left $ AAWSUnbekannt anfrage eingabe
anfrageAktionWegstreckeAktualisieren
    (AAWSBahngeschwindigkeit qAktion0)
    token
        = case anfrageAktionBahngeschwindigkeitAktualisieren qAktion0 token of
            (Left (AABGUnbekannt anfrage eingabe))
                -> Left $ AAWSUnbekannt (AAWSBahngeschwindigkeit anfrage) eingabe
            (Left aAktion1)
                -> Left $ AAWSBahngeschwindigkeit aAktion1
            (Right aktion)
                -> Right $ AWSBahngeschwindigkeit aktion
anfrageAktionWegstreckeAktualisieren
    (AAWSStreckenabschnitt qAktion0)
    token
        = case anfrageAktionStreckenabschnittAktualisieren qAktion0 token of
            (Left (AASTUnbekannt anfrage eingabe))
                -> Left $ AAWSUnbekannt (AAWSStreckenabschnitt anfrage) eingabe
            (Left aAktion1)
                -> Left $ AAWSStreckenabschnitt aAktion1
            (Right aktion)
                -> Right $ AWSStreckenabschnitt aktion
anfrageAktionWegstreckeAktualisieren
    (AAWSKupplung qAktion0)
    token
        = case anfrageAktionKupplungAktualisieren qAktion0 token of
            (Left (AAKUUnbekannt anfrage eingabe))
                -> Left $ AAWSUnbekannt (AAWSKupplung anfrage) eingabe
            (Left aAktion1)
                -> Left $ AAWSKupplung aAktion1
            (Right aktion)
                -> Right $ AWSKupplung aktion

-- *** Weichen-Aktion
-- | Unvollständige 'Aktion' einer 'Weiche'
data AnfrageAktionWeiche w
    = AnfrageAktionWeiche
        w                           -- ^ Weiche
    | AAWUnbekannt
        (AnfrageAktionWeiche w)     -- ^ Anfrage
        Text                        -- ^ Eingabe
    | AAWStellen
        w                           -- ^ Weiche

type instance AnfrageFamilie (AktionWeiche w) = AnfrageAktionWeiche w

instance (Show (AnfrageFamilie w), Show w) => Show (AnfrageAktionWeiche w) where
    show :: AnfrageAktionWeiche w -> String
    show
        (AnfrageAktionWeiche weiche)
            = Language.weiche <=> showText weiche
    show
        (AAWUnbekannt anfrageAktion eingabe)
            = unpack $ unbekanntShowText anfrageAktion eingabe
    show
        (AAWStellen weiche)
            = Language.weiche <=> showText weiche <^> Language.stellen
instance Anfrage (AnfrageAktionWeiche w) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionWeiche w -> s
    zeigeAnfrage
        (AnfrageAktionWeiche _weiche)
            = Language.aktion
    zeigeAnfrage
        (AAWUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrage anfrageAktion
    zeigeAnfrage
        (AAWStellen _weiche)
            = Language.richtung
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionWeiche w -> Maybe s
    zeigeAnfrageOptionen
        (AnfrageAktionWeiche _weiche)
            = Just $ toBefehlsString Language.aktionWeiche
    zeigeAnfrageOptionen
        (AAWUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen
        (AAWStellen _weiche)
            = Just $ toBefehlsString $ NE.toList $ fmap showText unterstützteRichtungen

-- | Eingabe einer Weichen-Aktion
anfrageAktionWeicheAktualisieren :: (Show (AnfrageFamilie w), Show w, WeicheKlasse w)
    => AnfrageAktionWeiche w -> EingabeToken -> Either (AnfrageAktionWeiche w) (AktionWeiche w)
anfrageAktionWeicheAktualisieren
    anfrage@(AnfrageAktionWeiche weiche)
    token@EingabeToken {eingabe}
        = wähleBefehl token [(Lexer.Stellen  , Left $ AAWStellen weiche)] $
            Left $ AAWUnbekannt anfrage eingabe
anfrageAktionWeicheAktualisieren
    anfrage@(AAWStellen _weiche)
    token@EingabeToken {eingabe}
        = case wähleRichtung token of
            Nothing
                -> Left $ AAWUnbekannt anfrage eingabe
            (Just richtung)
                -> mitRichtung anfrage richtung
    where
        mitRichtung :: (Show (AnfrageFamilie w), Show w, WeicheKlasse w)
            => AnfrageAktionWeiche aw w -> Richtung -> Either (AnfrageAktionWeiche aw w) (AktionWeiche w)
        mitRichtung
            anfrage@(AAWStellen weiche)
            richtung
                | hatRichtung weiche richtung
                    = Right $ Stellen weiche richtung
                | otherwise
                    = Left $ AAWUnbekannt anfrage eingabe
        mitRichtung
            anfrage
            _richtung
                = error $ "mitRichtung mit unbekannter anfrage aufgerufen: " ++ show anfrage
anfrageAktionWeicheAktualisieren
    anfrage
    _token
        = Left anfrage

-- *** Bahngeschwindigkeit-Aktion
-- | Unvollständige 'Aktion' einer 'Bahngeschwindigkeit'
data AnfrageAktionBahngeschwindigkeit b (z :: Zugtyp) where
    AnfrageAktionBahngeschwindigkeit
        :: (b z)                                    -- ^ Bahngeschwindigkeit
            -> AnfrageAktionBahngeschwindigkeit b z
    AABGUnbekannt
        :: (AnfrageAktionBahngeschwindigkeit b z)   -- ^ Anfrage
        -> Text                                     -- ^ Eingabe
            -> AnfrageAktionBahngeschwindigkeit b z
    AABGGeschwindigkeit
        :: (b z)                                -- ^ Bahngeschwindigkeit
            -> AnfrageAktionBahngeschwindigkeit b z
    AABGFahrtrichtungEinstellen
        :: (b z)                                -- ^ Bahngeschwindigkeit
            -> AnfrageAktionBahngeschwindigkeit b 'Lego

type instance AnfrageFamilie (AktionBahngeschwindigkeit b z) = AnfrageAktionBahngeschwindigkeit b z

instance (Show (AnfrageFamilie (b z)), Show (b z)) => Show (AnfrageAktionBahngeschwindigkeit b z) where
    show :: AnfrageAktionBahngeschwindigkeit b z -> String
    show
        (AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
            = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit
    show
        (AABGUnbekannt anfrageAktion eingabe)
            = unpack $ unbekanntShowText anfrageAktion eingabe
    show
        (AABGGeschwindigkeit bahngeschwindigkeit)
            = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit <^> Language.geschwindigkeit
    show
        (AABGFahrtrichtungEinstellen bahngeschwindigkeit)
            = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit <^> Language.fahrtrichtungEinstellen
instance Anfrage (AnfrageAktionBahngeschwindigkeit b z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit b z -> s
    zeigeAnfrage
        (AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit)
            = Language.aktion
    zeigeAnfrage
        (AABGUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrage anfrageAktion
    zeigeAnfrage
        (AABGGeschwindigkeit _bahngeschwindigkeit)
            = Language.geschwindigkeit
    zeigeAnfrage
        (AABGFahrtrichtungEinstellen _bahngeschwindigkeit)
            = Language.fahrtrichtung
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit b z -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AABGGeschwindigkeit _bahngeschwindigkeit)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit b z -> Maybe s
    zeigeAnfrageOptionen
        (AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit)
            = Just $ toBefehlsString Language.aktionBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AABGUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen
        (AABGGeschwindigkeit _bahngeschwindigkeit)
            = Nothing
    zeigeAnfrageOptionen
        (AABGFahrtrichtungEinstellen _bahngeschwindigkeit)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteFahrtrichtungen

-- | Eingabe einer Bahngeschwindigkeit-Aktion
anfrageAktionBahngeschwindigkeitAktualisieren :: (BahngeschwindigkeitKlasse b)
    => AnfrageAktionBahngeschwindigkeit b z -> EingabeToken
        -> Either (AnfrageAktionBahngeschwindigkeit b z) (AktionBahngeschwindigkeit b z)
anfrageAktionBahngeschwindigkeitAktualisieren
    anfrage@(AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Geschwindigkeit  , Left $ AABGGeschwindigkeit bahngeschwindigkeit),
            (Lexer.Umdrehen         , if zugtyp bahngeschwindigkeit == Märklin
                    then Right $ Umdrehen bahngeschwindigkeit Nothing
                    else Left $ AABGUmdrehen bahngeschwindigkeit)]
            $ Left $ AABGUnbekannt anfrage eingabe
anfrageAktionBahngeschwindigkeitAktualisieren
    anfrage@(AABGGeschwindigkeit bahngeschwindigkeit)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AABGUnbekannt anfrage eingabe
            (Just wert)
                -> Right $ Geschwindigkeit bahngeschwindigkeit wert
anfrageAktionBahngeschwindigkeitAktualisieren
    anfrage@(AABGFahrtrichtungEinstellen bahngeschwindigkeit)
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Vorwärts , Right $ Umdrehen bahngeschwindigkeit $ Just Vorwärts),
            (Lexer.Rückwärts , Right $ Umdrehen bahngeschwindigkeit $ Just Rückwärts)]
            $ Left $ AABGUnbekannt anfrage eingabe
anfrageAktionBahngeschwindigkeitAktualisieren
    anfrage
    _token
        = Left $ anfrage

-- *** Streckenabschnitt-Aktion
-- | Unvollständige 'Aktion' eines 'Streckenabschnitt's
data AnfrageAktionStreckenabschnitt s
    = AnfrageAktionStreckenabschnitt
        s                                   -- ^ Streckenabschnitt
    | AASTUnbekannt
        (AnfrageAktionStreckenabschnitt s)  -- ^ Anfrage
        Text                                -- ^ Eingabe
    | AASTStrom
        s                                   -- ^ Streckenabschnitt

type instance AnfrageFamilie (AktionStreckenabschnitt s) = AnfrageAktionStreckenabschnitt s

instance (Show (AnfrageFamilie s), Show s) => Show (AnfrageAktionStreckenabschnitt s) where
    show :: AnfrageAktionStreckenabschnitt as s -> String
    show
        (AnfrageAktionStreckenabschnitt streckenabschnitt)
            = Language.streckenabschnitt <=> showText streckenabschnitt
    show
        (AASTUnbekannt anfrageAktion eingabe)
            = unpack $ unbekanntShowText anfrageAktion eingabe
    show
        (AASTStrom streckenabschnitt)
            = Language.streckenabschnitt <=> showText streckenabschnitt <^> Language.strom
instance Anfrage (AnfrageAktionStreckenabschnitt st) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionStreckenabschnitt qst st -> s
    zeigeAnfrage
        (AnfrageAktionStreckenabschnitt _streckenabschnitt)
            = Language.aktion
    zeigeAnfrage
        (AASTUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrage anfrageAktion
    zeigeAnfrage
        (AASTStrom _streckenabschnitt)
            = Language.fließend <|> Language.gesperrt
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionStreckenabschnitt st -> Maybe s
    zeigeAnfrageOptionen
        (AnfrageAktionStreckenabschnitt _streckenabschnitt)
            = Just $ toBefehlsString Language.aktionStreckenabschnitt
    zeigeAnfrageOptionen
        (AASTUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen
        (AASTStrom _streckenabschnitt)
            = Just $ toBefehlsString [Language.an, Language.aus]

-- | Eingabe einer Streckenabschnitt-Aktion
anfrageAktionStreckenabschnittAktualisieren :: (StreckenabschnittKlasse s)
    => AnfrageAktionStreckenabschnitt s -> EingabeToken
        -> Either (AnfrageAktionStreckenabschnitt s) (AktionStreckenabschnitt s)
anfrageAktionStreckenabschnittAktualisieren
    anfrage@(AnfrageAktionStreckenabschnitt streckenabschnitt)
    token@EingabeToken {eingabe}
        = wähleBefehl token [(Lexer.Strom, Left $ AASTStrom streckenabschnitt)] $
            Left $ AASTUnbekannt anfrage eingabe
anfrageAktionStreckenabschnittAktualisieren
    anfrage@(AASTStrom streckenabschnitt)
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Fließend , Right $ Strom streckenabschnitt Fließend),
            (Lexer.An       , Right $ Strom streckenabschnitt Fließend),
            (Lexer.Gesperrt , Right $ Strom streckenabschnitt Gesperrt),
            (Lexer.Aus      , Right $ Strom streckenabschnitt Gesperrt)]
            $ Left $ AASTUnbekannt anfrage eingabe
anfrageAktionStreckenabschnittAktualisieren
    anfrage
    _token
        = Left $ anfrage

-- *** KupplungAktion
-- | Unvollständige 'Aktion' einer 'Kupplung'
data AnfrageAktionKupplung k
    = AnfrageAktionKupplung
        k                           -- ^ Kupplung
    | AAKUUnbekannt
        (AnfrageAktionKupplung k)   -- ^ Anfrage
        Text                        -- ^ Eingabe

type instance AnfrageFamilie (AktionKupplung k) = AnfrageAktionKupplung k

instance (Show (AnfrageFamilie k), Show k) => Show (AnfrageAktionKupplung k) where
    show :: AnfrageAktionKupplung ak k -> String
    show
        (AnfrageAktionKupplung kupplung)
            = Language.kupplung <=> showText kupplung
    show
        (AAKUUnbekannt anfrageAktion eingabe)
            = unpack $ unbekanntShowText anfrageAktion eingabe
instance Anfrage (AnfrageAktionKupplung k) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionKupplung k -> s
    zeigeAnfrage
        (AnfrageAktionKupplung _kupplung)
            = Language.aktion
    zeigeAnfrage
        (AAKUUnbekannt anfrageAktion _eingabe)  = zeigeAnfrage anfrageAktion
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionKupplung k -> Maybe s
    zeigeAnfrageOptionen
        (AnfrageAktionKupplung _kupplung)
            = Just $ toBefehlsString Language.aktionKupplung
    zeigeAnfrageOptionen
        (AAKUUnbekannt anfrageAktion _eingabe)
            = zeigeAnfrageOptionen anfrageAktion

-- | Eingabe einer Kupplung-Aktion
anfrageAktionKupplungAktualisieren :: (KupplungKlasse k)
    => AnfrageAktionKupplung k -> EingabeToken -> Either (AnfrageAktionKupplung k) (AktionKupplung k)
anfrageAktionKupplungAktualisieren
    anfrage@(AnfrageAktionKupplung kupplung)
    token@EingabeToken {eingabe}
        = wähleBefehl token [(Lexer.Kuppeln, Right $ Kuppeln kupplung)] $
            Left $ AAKUUnbekannt anfrage eingabe
anfrageAktionKupplungAktualisieren
    anfrage
    _token
        = Left $ anfrage

-- ** Wegstrecke
-- | Unvollständige 'Wegstrecke'
data AnfrageWegstrecke (z :: Zugtyp)
    = AnfrageWegstrecke
    | AWSUnbekannt
        (AnfrageWegstrecke z)   -- ^ Anfrage
        Text                    -- ^ Eingabe
    | AWegstreckeName
        Text                    -- ^ Name
    | AWegstreckeNameAnzahl
        (Wegstrecke z)          -- ^ Akkumulator
        Natural                 -- ^ Anzahl zusätzliche Elemente
    | AWegstreckeNameAnzahlWeicheRichtung
        (Wegstrecke z)          -- ^ Akkumulator
        Natural                 -- ^ Anzahl zusätzliche Elemente
        (Weiche z)              -- ^ Weiche für Richtung-Angabe
    | AWegstreckeRStatus
        StatusAnfrageObjekt
        (Either (Objekt -> (AnfrageWegstrecke z)) (Objekt -> (Wegstrecke z)))
    | AWSStatusAnfrage
        (EingabeToken -> StatusAnfrageObjekt)
        (Either (Objekt -> (AnfrageWegstrecke z)) (Objekt -> (Wegstrecke z)))

type instance AnfrageFamilie (Wegstrecke z) = AnfrageWegstrecke z

instance Show (AnfrageWegstrecke z) where
    show :: AnfrageWegstrecke z -> String
    show
        (AWSUnbekannt qWegstrecke eingabe)
            = unpack $ unbekanntShowText qWegstrecke eingabe
    show
        AnfrageWegstrecke
            = unpack $ Language.wegstrecke
    show
        (AWegstreckeName name)
            = unpack $ Language.wegstrecke <^> Language.name <=> name
    show
        (AWegstreckeNameAnzahl acc anzahl)
            = unpack $ Language.wegstrecke
                <^> showText acc
                <^> Language.anzahl Language.wegstreckenElemente <=> showText anzahl
    show
        (AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche)
            = unpack $ Language.wegstrecke
                <^> showText acc
                <^> Language.anzahl Language.wegstreckenElemente <=> showText anzahl
                <^> showText weiche
    show
        (AWegstreckeRStatus objektStatusAnfrage _eitherKonstruktor)
            = Language.wegstrecke <^> showText objektStatusAnfrage
    show
        (AWSStatusAnfrage anfrageKonstruktor _eitherF)
            = Language.wegstreckenElement <^> showText (anfrageKonstruktor leeresToken)
instance Anfrage (AnfrageWegstrecke z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageWegstrecke z -> s
    zeigeAnfrage
        (AWSUnbekannt qWegstrecke _eingabe)
            = zeigeAnfrage qWegstrecke
    zeigeAnfrage
        AnfrageWegstrecke
            = Language.name
    zeigeAnfrage
        (AWegstreckeName _name)
            = Language.anzahl Language.wegstreckenElemente
    zeigeAnfrage
        (AWegstreckeNameAnzahl _acc _anzahl)
            = Language.wegstreckenElement
    zeigeAnfrage
        (AWegstreckeNameAnzahlWeicheRichtung _acc _anzahl _weiche)
            = Language.richtung
    zeigeAnfrage
        (AWegstreckeRStatus objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage
        (AWSStatusAnfrage anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageWegstrecke -> Maybe s
    zeigeAnfrageOptionen
        (AWSUnbekannt qWegstrecke _eingabe)
            = zeigeAnfrageOptionen qWegstrecke
    zeigeAnfrageOptionen
        (AWegstreckeNameAnzahl _acc _anzahl)
            = Just $ toBefehlsString Language.befehlWegstreckenElemente
    zeigeAnfrageOptionen
        (AWegstreckeNameAnzahlWeicheRichtung _acc _anzahl _weiche)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        (AWegstreckeRStatus objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen
        (AWSStatusAnfrage anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Eingabe einer Wegstrecke
anfrageWegstreckeAktualisieren :: (AnfrageWegstrecke z) -> EingabeToken -> Either (AnfrageWegstrecke z) (Wegstrecke z)
anfrageWegstreckeAktualisieren
    AnfrageWegstrecke
    EingabeToken {eingabe}
        = Left $ AWegstreckeName eingabe
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeName wsName)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AWSUnbekannt anfrage eingabe
            (Just anzahl)
                -> Left $ AWegstreckeNameAnzahl
                    Wegstrecke {
                        wsName,
                        wsBahngeschwindigkeiten = [],
                        wsStreckenabschnitte = [],
                        wsWeichenRichtungen = [],
                        wsKupplungen = []}
                    anzahl
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeNameAnzahl
        acc@(Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsKupplungen})
        anzahl)
    token
        = Left $ case anfrageWegstreckenElement token of
            AWSEWeiche
                -> AWSStatusAnfrage SAOWeiche $ Left $ anfrageWeicheAnhängen
            AWSEBahngeschwindigkeit
                -> AWSStatusAnfrage SAOBahngeschwindigkeit eitherObjektAnhängen
            AWSEStreckenabschnitt
                -> AWSStatusAnfrage SAOStreckenabschnitt eitherObjektAnhängen
            AWSEKupplung
                -> AWSStatusAnfrage SAOKupplung eitherObjektAnhängen
            (AWSEUnbekannt eingabe)
                -> AWSUnbekannt anfrage eingabe
    where
        anfrageWegstreckenElement :: EingabeToken -> AnfrageWegstreckenElement
        anfrageWegstreckenElement token@EingabeToken {eingabe} = wähleBefehl token [
            (Lexer.Weiche                , AWSEWeiche),
            (Lexer.Bahngeschwindigkeit   , AWSEBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , AWSEStreckenabschnitt),
            (Lexer.Kupplung              , AWSEKupplung)]
            $ AWSEUnbekannt eingabe
        eitherObjektAnhängen :: Either (Objekt -> (AnfrageWegstrecke z)) (Objekt -> (Wegstrecke z))
        eitherObjektAnhängen
            | anzahl > 1
                = Left anfrageObjektAnhängen
            | otherwise
                = Right objektAnhängen
        objektAnhängen :: Objekt -> (Wegstrecke z)
        objektAnhängen
            (OBahngeschwindigkeit bahngeschwindigkeit)
                = acc {wsBahngeschwindigkeiten=bahngeschwindigkeit:wsBahngeschwindigkeiten}
        objektAnhängen
            (OStreckenabschnitt streckenabschnitt)
                = acc {wsStreckenabschnitte=streckenabschnitt:wsStreckenabschnitte}
        objektAnhängen
            (OKupplung kupplung)
                = acc {wsKupplungen=kupplung:wsKupplungen}
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        objektAnhängen
            _objekt
                = acc
        anfrageObjektAnhängen :: Objekt -> (AnfrageWegstrecke z)
        anfrageObjektAnhängen objekt = AWegstreckeNameAnzahl (objektAnhängen objekt) $ pred anzahl
        anfrageWeicheAnhängen :: Objekt -> (AnfrageWegstrecke z)
        anfrageWeicheAnhängen (OWeiche weiche)  = AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        anfrageWeicheAnhängen _objekt           = anfrage
anfrageWegstreckeAktualisieren
    (AWSStatusAnfrage anfrageKonstruktor eitherF)
    token
        = Left $ AWegstreckeIOStatus (anfrageKonstruktor token) eitherF
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeNameAnzahlWeicheRichtung
        acc@(Wegstrecke {wsWeichenRichtungen})
        anzahl
        weiche)
    token@EingabeToken {eingabe}
        = case wähleRichtung token of
            Nothing
                -> Left $ AWSUnbekannt anfrage eingabe
            (Just richtung)
                -> eitherWeicheRichtungAnhängen richtung
    where
        eitherWeicheRichtungAnhängen :: Richtung -> Either (AnfrageWegstrecke z) (Wegstrecke z)
        eitherWeicheRichtungAnhängen richtung
            | anzahl > 1
                = Left $ qWeicheRichtungAnhängen richtung
            | otherwise
                = Right $ weicheRichtungAnhängen richtung
        qWeicheRichtungAnhängen :: Richtung -> (AnfrageWegstrecke z)
        qWeicheRichtungAnhängen richtung = AWegstreckeNameAnzahl (weicheRichtungAnhängen richtung) $ pred anzahl
        weicheRichtungAnhängen :: Richtung -> (Wegstrecke z)
        weicheRichtungAnhängen richtung = acc {wsWeichenRichtungen = (weiche, richtung) : wsWeichenRichtungen}
anfrageWegstreckeAktualisieren
    anfrage
    _token
        = Left anfrage

-- ** Weiche
-- | Unvollständige 'Weiche'
data AnfrageWeiche (z :: Zugtyp) where
    AnfrageWeiche
        :: AnfrageWeiche z
    AWEUnbekannt
        :: AnfrageWeiche z              -- ^ Anfrage
        -> Text                         -- ^ Eingabe
            -> AnfrageWeiche z
    ALegoWeiche
        :: AnfrageWeiche 'Lego
    ALegoWeicheName
        :: Text                         -- ^ Name
            -> AnfrageWeiche 'Lego
    ALegoWeicheNameFließend
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
            -> AnfrageWeiche 'Lego
    ALegoWeicheNameFließendRichtung1
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
        -> Richtung                     -- ^ Richtung1
            -> AnfrageWeiche 'Lego
    ALegoWeicheNameFließendRichtungen
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
        -> Richtung                     -- ^ Richtung1
        -> Richtung                     -- ^ Richtung2
            -> AnfrageWeiche 'Lego
    AMärklinWeiche
        :: AnfrageWeiche 'Märklin
    AMärklinWeicheName
        :: Text                         -- ^ Name
            -> AnfrageWeiche 'Märklin
    AMärklinWeicheNameFließend
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
            -> AnfrageWeiche 'Märklin
    AMärklinWeicheNameFließendAnzahl
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
        -> Natural                      -- ^ Verbleibende Richtungen
        -> [(Richtung, Anschluss)]      -- ^ RichtungsAnschlüsse
            -> AnfrageWeiche 'Märklin
    AMärklinWeicheNameFließendAnzahlRichtung
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
        -> Natural                      -- ^ Verbleibende Richtungen
        -> [(Richtung, Anschluss)]      -- ^ RichtungsAnschlüsse
        -> Richtung                     -- ^ nächste Richtung
            -> AnfrageWeiche 'Märklin

type instance AnfrageFamilie (Weiche z) = AnfrageWeiche z

instance Show (AnfrageWeiche z) where
    show :: AnfrageWeiche z -> String
    show
        AnfrageWeiche
            = Language.weiche
    show
        (AWEUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show
        ALegoWeiche
            = Language.lego <-> Language.weiche
    show
        (ALegoWeicheName name)
            = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name
    show
        (ALegoWeicheNameFließend name fließend)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
    show
        (ALegoWeicheNameFließendRichtung1 name fließend richtung1)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> showText richtung1
    show
        (ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> showText richtung1
                <^> showText richtung2
    show
        AMärklinWeiche
            = Language.märklin <-> Language.weiche
    show
        (AMärklinWeicheName name)
            = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name
    show
        (AMärklinWeicheNameFließend name fließend)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
    show
        (AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> Language.erwartet Language.richtungen <=> showText anzahl
                <^> showText acc
    show
        (AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> Language.erwartet Language.richtungen <=> showText anzahl
                <^> showText acc <> Language.richtung <=> showText richtung
instance Anfrage (AnfrageWeiche z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageWeiche z -> s
    zeigeAnfrage
        AnfrageWeiche
            = Language.zugtyp
    zeigeAnfrage
        (AWEUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        ALegoWeiche
            = Language.name
    zeigeAnfrage
        (ALegoWeicheName _name)
            = Language.fließendValue
    zeigeAnfrage
        (ALegoWeicheNameFließend _name _fließend)
            = Language.richtung
    zeigeAnfrage
        (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1)
            = Language.richtung
    zeigeAnfrage
        (ALegoWeicheNameFließendRichtungen _name _fließen _richtung1 _richtung2)
            = Language.anschluss
    zeigeAnfrage
        AMärklinWeiche
            = Language.name
    zeigeAnfrage
        (AMärklinWeicheName _name)
            = Language.fließendValue
    zeigeAnfrage
        (AMärklinWeicheNameFließend _name _fließend)
            = Language.anzahl Language.richtungen
    zeigeAnfrage
        (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc)
            = Language.richtung
    zeigeAnfrage
        (AMärklinWeicheNameFließendAnzahlRichtung _name _fließend _anzahl _acc _richtung)
            = Language.anschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageWeiche z -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(ALegoWeicheNameFließendRichtungen _name _fließend _richtung1 _richtung2)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(AMärklinWeicheNameFließend _name _fließend)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(AMärklinWeicheNameFließendAnzahlRichtung _name _fließend _anzahl _acc _richtung)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageWeiche z -> Maybe s
    zeigeAnfrageOptionen
        AnfrageWeiche
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen
        (AWEUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        (ALegoWeicheName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (ALegoWeicheNameFließend _name _fließend)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        (AMärklinWeicheName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Eingabe einer Weiche
anfrageWeicheAktualisieren :: AnfrageWeiche z -> EingabeToken -> Either (AnfrageWeiche z) (Weiche z)
anfrageWeicheAktualisieren
    AnfrageWeiche
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.Märklin  , AMärklinWeiche),
            (Lexer.Lego     , ALegoWeiche)]
            $ AWEUnbekannt AnfrageWeiche eingabe
anfrageWeicheAktualisieren
    ALegoWeiche
    EingabeToken {eingabe}
        = Left $ ALegoWeicheName eingabe
anfrageWeicheAktualisieren
    anfrage@(ALegoWeicheName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , ALegoWeicheNameFließend name HIGH),
            (Lexer.LOW  , ALegoWeicheNameFließend name LOW)]
            $ AWEUnbekannt anfrage eingabe
anfrageWeicheAktualisieren
    anfrage@(ALegoWeicheNameFließend name fließend)
    token@EingabeToken {eingabe}
        = Left $ case wähleRichtung token of
            Nothing
                -> AWEUnbekannt anfrage eingabe
            (Just richtung1)
                -> ALegoWeicheNameFließendRichtung1 name fließend richtung1
anfrageWeicheAktualisieren
    anfrage@(ALegoWeicheNameFließendRichtung1 name fließend richtung1)
    token@EingabeToken {eingabe}
        = Left $ case wähleRichtung token of
            Nothing
                -> AWEUnbekannt anfrage eingabe
            (Just richtung2)
                -> ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2
anfrageWeicheAktualisieren
    anfrage@(ALegoWeicheNameFließendRichtungen welName welFließend richtung1 richtung2)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AWEUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ LegoWeiche {
                    welName,
                    welFließend,
                    welRichtungsAnschluss = zuPin pin,
                    welRichtungen = (richtung1,richtung2)}
anfrageWeicheAktualisieren
    AMärklinWeiche
    EingabeToken {eingabe}
        = Left $ AMärklinWeicheName eingabe
anfrageWeicheAktualisieren
    anfrage@(AMärklinWeicheName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , AMärklinWeicheNameFließend name HIGH),
            (Lexer.LOW  , AMärklinWeicheNameFließend name LOW)]
            $ AWEUnbekannt anfrage eingabe
anfrageWeicheAktualisieren
    anfrage@(AMärklinWeicheNameFließend name fließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AWEUnbekannt anfrage eingabe
            (Just anzahl)
                -> Left $ AMärklinWeicheNameFließendAnzahl name fließend anzahl []
anfrageWeicheAktualisieren
    anfrage@(AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)
    token@EingabeToken {eingabe}
        = Left $ case wähleRichtung token of
            Nothing
                -> AWEUnbekannt anfrage eingabe
            (Just richtung)
                -> AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung
anfrageWeicheAktualisieren
    anfrage@(AMärklinWeicheNameFließendAnzahlRichtung wemName wemFließend anzahl acc richtung)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AWEUnbekannt anfrage eingabe
            (Just pin)
                | anzahl > 1
                    -> Left $ AMärklinWeicheNameFließendAnzahl
                        wemName
                        wemFließend
                        (pred anzahl)
                        ((richtung, zuPin pin) : acc)
                | otherwise
                    -> Right MärklinWeiche {
                        wemName,
                        wemFließend,
                        wemRichtungsAnschlüsse = (richtung, zuPin pin) :| acc}
anfrageWeicheAktualisieren
    anfrage@(AWEUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- ** Bahngeschwindigkeit
-- | Unvollständige 'Bahngeschwindigkeit'
data AnfrageBahngeschwindigkeit (z :: Zugtyp) where
    AnfrageBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit z
    ABGUnbekannt
        :: (AnfrageBahngeschwindigkeit z)               -- ^ Anfrage
        -> Text                                         -- ^ Eingabe
            -> AnfrageBahngeschwindigkeit z
    ALegoBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'Lego
    ALegoBahngeschwindigkeitName
        :: Text                                         -- ^ Name
            -> AnfrageBahngeschwindigkeit 'Lego
    ALegoBahngeschwindigkeitNameFließend
        :: Text                                         -- ^ Name
        -> Value                                        -- ^ Fließend
            -> AnfrageBahngeschwindigkeit 'Lego
    ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
        :: Text                                         -- ^ Name
        -> Value                                        -- ^ Fließend
        -> Anschluss                                    -- ^ Geschwindigkeit-Anschluss
            -> AnfrageBahngeschwindigkeit 'Lego
    AMärklinBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'Märklin
    AMärklinBahngeschwindigkeitName
        :: Text                                         -- ^ Name
            -> AnfrageBahngeschwindigkeit 'Märklin
    AMärklinBahngeschwindigkeitNameFließend
        :: Text                                         -- ^ Name
        -> Value                                        -- ^ Fließend
            -> AnfrageBahngeschwindigkeit 'Märklin

type instance AnfrageFamilie (Bahngeschwindigkeit z) = AnfrageBahngeschwindigkeit z

instance Show (AnfrageBahngeschwindigkeit z) where
    show :: AnfrageBahngeschwindigkeit z -> String
    show
        AnfrageBahngeschwindigkeit
            = Language.bahngeschwindigkeit
    show
        (ABGUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show
        ALegoBahngeschwindigkeit
            = Language.lego <-> Language.bahngeschwindigkeit
    show
        (ALegoBahngeschwindigkeitName name)
            = unpack $ Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    show
        (ALegoBahngeschwindigkeitNameFließend name fließend)
            = unpack $ Language.lego <-> Language.bahngeschwindigkeit
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
    show
        (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit name fließend pin)
            = unpack $ Language.lego <-> Language.bahngeschwindigkeit
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
                <^> Language.pin <=> showText pin
    show
        AMärklinBahngeschwindigkeit
            = Language.märklin <-> Language.bahngeschwindigkeit
    show
        (AMärklinBahngeschwindigkeitName name)
            = unpack $ Language.märklin <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    show
        (AMärklinBahngeschwindigkeitNameFließend name fließend)
            = unpack $ Language.märklin <-> Language.bahngeschwindigkeit
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
instance Anfrage (AnfrageBahngeschwindigkeit z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit z -> s
    zeigeAnfrage
        AnfrageBahngeschwindigkeit
            = Language.zugtyp
    zeigeAnfrage
        (ABGUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        ALegoBahngeschwindigkeit
            = Language.name
    zeigeAnfrage
        (ALegoBahngeschwindigkeitName _name)
            = Language.fließendValue
    zeigeAnfrage
        (ALegoBahngeschwindigkeitNameFließend _name _fließend)
            = Language.anschluss
    zeigeAnfrage
        (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit _name _fließend _pin)
            = Language.anschluss
    zeigeAnfrage
        AMärklinBahngeschwindigkeit
            = Language.name
    zeigeAnfrage
        (AMärklinBahngeschwindigkeitName _name)
            = Language.fließendValue
    zeigeAnfrage
        (AMärklinBahngeschwindigkeitNameFließend _name _fließend)
            = Language.anschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit z -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(ALegoBahngeschwindigkeitName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit _name _fließend _pin)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(AMärklinBahngeschwindigkeitName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit z -> Maybe s
    zeigeAnfrageOptionen
        AnfrageBahngeschwindigkeit
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen
        (ALegoBahngeschwindigkeitName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (AMärklinBahngeschwindigkeitName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (ABGUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Eingabe einer Bahngeschwindigkeit
anfrageBahngeschwindigkeitAktualisieren ::
    AnfrageBahngeschwindigkeit z ->
    EingabeToken ->
        Either (AnfrageBahngeschwindigkeit z) (Bahngeschwindigkeit z)
anfrageBahngeschwindigkeitAktualisieren
    AnfrageBahngeschwindigkeit
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.Märklin  , AMärklinBahngeschwindigkeit),
            (Lexer.Lego     , ALegoBahngeschwindigkeit)]
            $ ABGUnbekannt AnfrageBahngeschwindigkeit eingabe
anfrageBahngeschwindigkeitAktualisieren
    ALegoBahngeschwindigkeit
    EingabeToken {eingabe}
        = Left $ ALegoBahngeschwindigkeitName eingabe
anfrageBahngeschwindigkeitAktualisieren
    anfrage@(ALegoBahngeschwindigkeitName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , ALegoBahngeschwindigkeitNameFließend name HIGH),
            (Lexer.LOW  , ALegoBahngeschwindigkeitNameFließend name LOW)]
            $ ABGUnbekannt anfrage eingabe
anfrageBahngeschwindigkeitAktualisieren
    anfrage@(ALegoBahngeschwindigkeitNameFließend name fließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ ABGUnbekannt anfrage eingabe
            (Just pin)
                -> Left $ ALegoBahngeschwindigkeitNameFließendGeschwindigkeit name fließend $ zuPin pin
anfrageBahngeschwindigkeitAktualisieren
    anfrage@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit bglName bglFließend bglGeschwindigkeitsAnschluss)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ ABGUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ LegoBahngeschwindigkeit {
                    bglName,
                    bglFließend,
                    bglGeschwindigkeitsAnschluss,
                    bglFahrtrichtungsAnschluss = zuPin pin}
anfrageBahngeschwindigkeitAktualisieren
    AMärklinBahngeschwindigkeit
    EingabeToken {eingabe}
        = Left $ AMärklinBahngeschwindigkeitName eingabe
anfrageBahngeschwindigkeitAktualisieren
    anfrage@(AMärklinBahngeschwindigkeitName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , AMärklinBahngeschwindigkeitNameFließend name HIGH),
            (Lexer.LOW  , AMärklinBahngeschwindigkeitNameFließend name LOW)]
            $ ABGUnbekannt anfrage eingabe
anfrageBahngeschwindigkeitAktualisieren
    anfrage@(AMärklinBahngeschwindigkeitNameFließend bgmName bgmFließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ ABGUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ MärklinBahngeschwindigkeit {
                    bgmName,
                    bgmFließend,
                    bgmGeschwindigkeitsAnschluss = zuPin pin}
anfrageBahngeschwindigkeitAktualisieren
    anfrage@(ABGUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- ** Streckenabschnitt
-- | Unvollständiger 'Streckenabschnitt'
data AnfrageStreckenabschnitt
    = AnfrageStreckenabschnitt
    | ASTUnbekannt
        AnfrageStreckenabschnitt        -- ^ Anfrage
        Text                            -- ^ Eingabe
    | AStreckenabschnittName
        Text                            -- ^ Name
    | AStreckenabschnittNameFließend
        Text                            -- ^ Name
        Value                           -- ^ Fließend

type instance AnfrageFamilie Streckenabschnitt = AnfrageStreckenabschnitt

instance Show AnfrageStreckenabschnitt where
    show :: AnfrageStreckenabschnitt -> String
    show
        AnfrageStreckenabschnitt
            = Language.streckenabschnitt
    show
        (ASTUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show
        (AStreckenabschnittName name)
            = unpack $ Language.streckenabschnitt <^> Language.name <=> name
    show
        (AStreckenabschnittNameFließend name fließend)
            = unpack $ Language.streckenabschnitt
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
instance Anfrage AnfrageStreckenabschnitt where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> s
    zeigeAnfrage
        AnfrageStreckenabschnitt
            = Language.name
    zeigeAnfrage
        (ASTUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        (AStreckenabschnittName _name)
            = Language.fließendValue
    zeigeAnfrage
        (AStreckenabschnittNameFließend _name _fließend)
            = Language.anschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AStreckenabschnittName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> Maybe s
    zeigeAnfrageOptionen
        (AStreckenabschnittName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (ASTUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

    -- | Eingabe eines Streckenabschnitts
anfrageStreckenabschnittAktualisieren :: AnfrageStreckenabschnitt -> EingabeToken -> Either AnfrageStreckenabschnitt Streckenabschnitt
anfrageStreckenabschnittAktualisieren
    AnfrageStreckenabschnitt
    EingabeToken {eingabe}
        = Left $ AStreckenabschnittName eingabe
anfrageStreckenabschnittAktualisieren
    anfrage@(AStreckenabschnittName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , AStreckenabschnittNameFließend name HIGH),
            (Lexer.LOW  , AStreckenabschnittNameFließend name LOW)]
            $ ASTUnbekannt anfrage eingabe
anfrageStreckenabschnittAktualisieren
    anfrage@(AStreckenabschnittNameFließend stName stFließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ ASTUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ Streckenabschnitt {
                    stName,
                    stFließend,
                    stromAnschluss = zuPin pin}
anfrageStreckenabschnittAktualisieren
    anfrage@(ASTUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- ** Kupplung
-- | Unvollständige 'Kupplung'
data AnfrageKupplung
    = AnfrageKupplung
    | AKUUnbekannt
        AnfrageKupplung     -- ^ Anfrage
        Text                -- ^ Eingabe
    | AKupplungName
        Text                -- ^ Name
    | AKupplungNameFließend
        Text                -- ^ Name
        Value               -- ^ Fließend

type instance AnfrageFamilie Kupplung = AnfrageKupplung

instance Show AnfrageKupplung where
    show :: AnfrageKupplung -> String
    show 
        AnfrageKupplung
            = Language.kupplung
    show 
        (AKUUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show 
        (AKupplungName name)
            = unpack $ Language.kupplung <^> Language.name <=> name
    show 
        (AKupplungNameFließend name fließend)
            = unpack $ Language.kupplung <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
instance Anfrage AnfrageKupplung where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageKupplung -> s
    zeigeAnfrage
        AnfrageKupplung
            = Language.name
    zeigeAnfrage
        (AKUUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        (AKupplungName _name)
            = Language.fließendValue
    zeigeAnfrage
        (AKupplungNameFließend _name _fließend)
            = Language.anschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageKupplung -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AKupplungName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageKupplung -> Maybe s
    zeigeAnfrageOptionen
        (AKupplungName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (AKUUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Eingabe einer Kupplung
anfrageKupplungAktualisieren :: AnfrageKupplung -> EingabeToken -> Either AnfrageKupplung Kupplung
anfrageKupplungAktualisieren
    AnfrageKupplung
    EingabeToken {eingabe}
        = Left $ AKupplungName eingabe
anfrageKupplungAktualisieren
    anfrage@(AKupplungName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , AKupplungNameFließend name HIGH),
            (Lexer.LOW  , AKupplungNameFließend name LOW)]
            $ AKUUnbekannt anfrage eingabe
anfrageKupplungAktualisieren
    anfrage@(AKupplungNameFließend kuName kuFließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AKUUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ Kupplung {
                    kuName,
                    kuFließend,
                    kupplungsAnschluss = zuPin pin}
anfrageKupplungAktualisieren
    anfrage@(AKUUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- * Hilfs-Befehle
-- | Wähle aus möglichen Interpretationen der Eingabe die erste passende aus und gebe den daraus resultierenden Befehl zurück.
-- Falls keine Möglichkeit passend ist, gebe wird das Ersatz-Ergebnis zurückgegeben.
wähleBefehl :: EingabeToken -> [(Token, a)] -> a -> a
wähleBefehl
    _eingabe
    []
    ersatz
        = ersatz
wähleBefehl
    eingabe@(EingabeToken {möglichkeiten})
    ((befehl, ergebnis) : t)
    ersatz
        | elem befehl möglichkeiten
            = ergebnis
        | otherwise
            = wähleBefehl eingabe t ersatz

-- | Gebe (falls möglich) die zur Eingabe passende 'Richtung' zurück.
wähleRichtung :: EingabeToken -> Maybe Richtung
wähleRichtung token = wähleBefehl token [
    (Lexer.Gerade   , Just Gerade),
    (Lexer.Kurve    , Just Kurve),
    (Lexer.Links    , Just Links),
    (Lexer.Rechts   , Just Rechts)]
    Nothing

-- ** Text-Hilfsfunktionen
-- | Fehlerhafte Eingabe anzeigen
unbekanntShowText :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s -> s
unbekanntShowText a eingabe = fehlerText $ showMitAnfrageFehlgeschlagen a eingabe