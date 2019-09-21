{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

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
    Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, showMitAnfrage, showMitAnfrageFehlgeschlagen, unbekanntShowText,
    AnfragePlan(..), AnfrageAktion(..), AnfrageAktionWegstrecke(..), AnfrageAktionWeiche(..), AnfrageAktionBahngeschwindigkeit(..), AnfrageAktionStreckenabschnitt(..), AnfrageAktionKupplung(..),
    AnfrageObjekt(..), AnfrageWegstrecke(..), AnfrageWeiche(..), AnfrageBahngeschwindigkeit(..), AnfrageStreckenabschnitt(..), AnfrageKupplung(..)) where

-- Bibliotheken
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
import System.Hardware.WiringPi (Value(..))
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (Pin(), StreckenObjekt(..), alleValues, zuPin,
                    Wegstrecke(..), WegstreckeKlasse(),
                    Weiche(..), WeicheKlasse(..),
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(),
                    Streckenabschnitt(..), StreckenabschnittKlasse(),
                    Kupplung(..), KupplungKlasse())
import Zug.Klassen (Richtung(..), unterstützteRichtungen, Fahrtrichtung(..), unterstützteFahrtrichtungen,
                    Zugtyp(..), unterstützteZugtypen , Strom(..))
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

-- * Auswerten einer Text-Eingabe
-- ** Suchen eines Objekt im aktuellen 'StatusAllgemein'
-- | Ein Objekt aus dem aktuellen Status wird benötigt
data StatusAnfrageObjekt    = SAOUnbekannt              Text
                            | SAOPlan                   EingabeToken
                            | SAOWegstrecke             EingabeToken
                            | SAOWeiche                 EingabeToken
                            | SAOBahngeschwindigkeit    EingabeToken
                            | SAOStreckenabschnitt      EingabeToken
                            | SAOKupplung               EingabeToken

instance Show StatusAnfrageObjekt where
    show :: StatusAnfrageObjekt -> String
    show    anfrage@(SAOUnbekannt eingabe)    = unpack $ zeigeAnfrageFehlgeschlagen anfrage eingabe
    show    (SAOPlan _token)                  = Language.plan
    show    (SAOWegstrecke _token)            = Language.wegstrecke
    show    (SAOWeiche _token)                = Language.weiche
    show    (SAOBahngeschwindigkeit _token)   = Language.bahngeschwindigkeit
    show    (SAOStreckenabschnitt _token)     = Language.streckenabschnitt
    show    (SAOKupplung _token)              = Language.kupplung
instance Anfrage StatusAnfrageObjekt where
    zeigeAnfrage :: (IsString s, Semigroup s) => StatusAnfrageObjekt -> s
    zeigeAnfrage    (SAOUnbekannt _eingabe)           = Language.objekt
    zeigeAnfrage    (SAOPlan _token)                  = Language.indexOderName Language.plan
    zeigeAnfrage    (SAOWegstrecke _token)            = Language.indexOderName Language.wegstrecke
    zeigeAnfrage    (SAOWeiche _token)                = Language.indexOderName Language.weiche
    zeigeAnfrage    (SAOBahngeschwindigkeit _token)   = Language.indexOderName Language.bahngeschwindigkeit
    zeigeAnfrage    (SAOStreckenabschnitt _token)     = Language.indexOderName Language.streckenabschnitt
    zeigeAnfrage    (SAOKupplung _token)              = Language.indexOderName Language.kupplung
-- | Erhalte ein im Status existierendes Objekt
statusAnfrageObjekt :: StatusAnfrageObjekt -> MStatus (Either StatusAnfrageObjekt Objekt)
statusAnfrageObjekt
    anfrage@(SAOUnbekannt _eingabe0)
        = pure $ Left anfrage
statusAnfrageObjekt
    anfrage@(SAOPlan eingabe)
        = statusAnfrageObjektAux anfrage eingabe getPläne OPlan
statusAnfrageObjekt
    anfrage@(SAOWegstrecke eingabe)
        = statusAnfrageObjektAux anfrage eingabe getWegstrecken OWegstrecke
statusAnfrageObjekt
    anfrage@(SAOWeiche eingabe)
        = statusAnfrageObjektAux anfrage eingabe getWeichen OWeiche
statusAnfrageObjekt
    anfrage@(SAOBahngeschwindigkeit eingabe)
        = statusAnfrageObjektAux anfrage eingabe getBahngeschwindigkeiten OBahngeschwindigkeit
statusAnfrageObjekt
    anfrage@(SAOStreckenabschnitt eingabe)
        = statusAnfrageObjektAux anfrage eingabe getStreckenabschnitte OStreckenabschnitt
statusAnfrageObjekt
    anfrage@(SAOKupplung eingabe)
        = statusAnfrageObjektAux anfrage eingabe getKupplungen OKupplung

-- | Hilfsfunktion
statusAnfrageObjektAux :: (StreckenObjekt a)
    => StatusAnfrageObjekt
    -> EingabeToken
    -> MStatus [a]
    -> (a -> Objekt)
        -> MStatus (Either StatusAnfrageObjekt Objekt)
statusAnfrageObjektAux anfrage eingabe getFromStatus konstruktor = do
    objekte <- getFromStatus
    pure $ case findByNameOrIndex objekte eingabe of
        Nothing         -> Left anfrage
        (Just objekt)   -> Right $ konstruktor objekt

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
        (NonEmpty Pin)
    | ABAktion
        AnfrageAktion
    | ABStatusAnfrage
        (EingabeToken -> StatusAnfrageObjekt)
        (Objekt -> AnfrageErgebnis)

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
    token@(EingabeToken {eingabe})
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
    token@(EingabeToken {eingabe})
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
    token@(EingabeToken {eingabe})
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
    token@(EingabeToken {eingabe})
        = wähleBefehl token [
            (Lexer.AusführenAbbrechen, AEBefehl $ AusführenAbbrechen plan)]
            $ AEAnfrageBefehl $ ABUnbekannt (ABAktionPlanAusführend plan Alt) eingabe
anfrageAktualisieren
    (ABAktionPlanGesperrt plan _neu pins)
    token@(EingabeToken {eingabe})
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
        AnfrageObjekt
        Text
    | AOPlan
        AnfragePlan
    | AOWegstrecke
        AnfrageWegstrecke
    | AOWeiche
        AnfrageWeiche
    | AOBahngeschwindigkeit
        AnfrageBahngeschwindigkeit
    | AOStreckenabschnitt
        AnfrageStreckenabschnitt
    | AOKupplung
        AnfrageKupplung
    | AOStatusAnfrage
        StatusAnfrageObjekt
        (Either (Objekt -> AnfrageObjekt) (Objekt -> Objekt))

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
    token@(EingabeToken {eingabe})
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
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Right $ \objekt -> OPlan $ konstruktor objekt
            (Left (APlanIOStatus objektStatusAnfrage (Left anfrageKonstruktor)))
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Left $ \objekt -> AOPlan $ anfrageKonstruktor objekt
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
            (Left (AWegstreckeIOStatus objektStatusAnfrage (Right konstruktor)))
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Right $
                    \objekt -> OWegstrecke $ konstruktor objekt
            (Left (AWegstreckeIOStatus objektStatusAnfrage (Left anfrageKonstruktor)))
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
data AnfragePlan    = AnfragePlan
                    | APUnbekannt       AnfragePlan                             Text
                    | APlanName         Text
                    | APlanNameAnzahl   Text                                    Natural                                             (Warteschlange Aktion)    AnfrageAktion
                    | APlanIOStatus     StatusAnfrageObjekt                     (Either (Objekt -> AnfragePlan) (Objekt -> Plan))
                    | APStatusAnfrage   (EingabeToken -> StatusAnfrageObjekt)   (Either (Objekt -> AnfragePlan) (Objekt -> Plan))

instance Show AnfragePlan where
    show :: AnfragePlan -> String
    show    (APUnbekannt aPlan eingabe)                             = unpack $ unbekanntShowText aPlan eingabe
    show    (AnfragePlan)                                           = Language.plan
    show    (APlanName name)                                        = unpack $ Language.plan <^> Language.name <=> name
    show    (APlanNameAnzahl name anzahl acc anfrageAktion)         = unpack $ Language.plan <^> Language.name <=> name <^> Language.anzahl Language.aktionen <=> showText anzahl <^> showText acc <^> showText anfrageAktion
    show    (APlanIOStatus objektStatusAnfrage _eitherKonstruktor)  = Language.plan <^> showText objektStatusAnfrage
    show    (APStatusAnfrage anfrageKonstruktor _eitherF)           = Language.plan <^> Language.aktion <-> Language.objekt <^> showText (anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing})
instance Anfrage AnfragePlan where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfragePlan -> s
    zeigeAnfrage    (APUnbekannt aPlan _eingabe)                            = zeigeAnfrage aPlan
    zeigeAnfrage    (AnfragePlan)                                           = Language.name
    zeigeAnfrage    (APlanName _name)                                       = Language.anzahl Language.aktionen
    zeigeAnfrage    (APlanNameAnzahl _name _anzahl _acc anfrageAktion)      = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (APlanIOStatus objektStatusAnfrage _eitherKonstruktor)  = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage    (APStatusAnfrage anfrageKonstruktor _eitherF)           = zeigeAnfrage $ anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfragePlan -> s -> s
    zeigeAnfrageFehlgeschlagen  a@(APlanName _name) eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a                   eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfragePlan -> Maybe s
    zeigeAnfrageOptionen (APUnbekannt aPlan _eingabe)                           = zeigeAnfrageOptionen aPlan
    zeigeAnfrageOptionen (AnfragePlan)                                          = Nothing
    zeigeAnfrageOptionen (APlanName _name)                                      = Nothing
    zeigeAnfrageOptionen (APlanNameAnzahl _name _anzahl _acc anfrageAktion)     = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (APlanIOStatus objektStatusAnfrage _eitherKonstruktor) = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen (APStatusAnfrage anfrageKonstruktor _eitherF)          = zeigeAnfrageOptionen $ anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}

-- | Eingabe eines Plans
anfragePlanAktualisieren :: AnfragePlan -> EingabeToken -> Either AnfragePlan Plan
anfragePlanAktualisieren   (AnfragePlan)                                    (EingabeToken {eingabe})            = Left $ APlanName eingabe
anfragePlanAktualisieren   anfrage@(APlanName name)                         (EingabeToken {eingabe, ganzzahl})  = Left $ case ganzzahl of
    (Nothing)       -> APUnbekannt anfrage eingabe
    (Just anzahl)   -> APlanNameAnzahl name anzahl leer AnfrageAktion
anfragePlanAktualisieren   (APlanNameAnzahl name anzahl acc anfrageAktion)  token                               = case anfrageAktionAktualisieren anfrageAktion token of
    (Left (AAUnbekannt aAktion1 eingabe))                                   -> Left $ APUnbekannt (APlanNameAnzahl name anzahl acc aAktion1) eingabe
    (Left (AAStatusAnfrage objektStatusAnfrage (Left anfrageKonstruktor)))  -> Left $ APlanIOStatus objektStatusAnfrage $ Left $ \objekt -> APlanNameAnzahl name anzahl acc $ anfrageKonstruktor objekt
    (Left (AAStatusAnfrage objektStatusAnfrage (Right konstruktor)))        -> Left $ APlanIOStatus objektStatusAnfrage $ if anzahl > 1 then Left $ \objekt -> APlanNameAnzahl name anzahl (anhängen (konstruktor objekt) acc) AnfrageAktion else Right $ \objekt -> Plan {plName=name, plAktionen=toList $ anhängen (konstruktor objekt) acc}
    (Left AARückgängig)                                                     -> let prevAcc = case zeigeLetztes acc of {(Leer) -> leer; (Gefüllt _l p) -> p} in Left $ APlanNameAnzahl name (succ anzahl) prevAcc AnfrageAktion
    (Left aAktion1)                                                         -> Left $ APlanNameAnzahl name anzahl acc aAktion1
    (Right aktion)  | anzahl > 1                                            -> Left $ APlanNameAnzahl name (pred anzahl) (anhängen aktion acc) AnfrageAktion
                    | otherwise                                             -> Right $ Plan {plName=name, plAktionen=toList $ anhängen aktion acc}
anfragePlanAktualisieren   (APStatusAnfrage anfrageKonstruktor eitherF)     token                               = Left $ APlanIOStatus (anfrageKonstruktor token) eitherF
anfragePlanAktualisieren   anfrage                                          _token                              = Left anfrage

-- *** Aktion
-- | Unvollständige 'Aktion'
data AnfrageAktion  = AnfrageAktion
                    | AAUnbekannt           AnfrageAktion                                                                       Text
                    | AARückgängig
                    | AAWarten
                    | AAWegstrecke          (AnfrageAktionWegstrecke AnfrageWegstrecke Wegstrecke)
                    | AAWeiche              (AnfrageAktionWeiche AnfrageWeiche Weiche)
                    | AABahngeschwindigkeit (AnfrageAktionBahngeschwindigkeit AnfrageBahngeschwindigkeit Bahngeschwindigkeit)
                    | AAStreckenabschnitt   (AnfrageAktionStreckenabschnitt AnfrageStreckenabschnitt Streckenabschnitt)
                    | AAKupplung            (AnfrageAktionKupplung AnfrageKupplung Kupplung)
                    | AAStatusAnfrage       StatusAnfrageObjekt                                                                 (Either (Objekt -> AnfrageAktion) (Objekt -> Aktion))
                    | AAKlassifizierung     (EingabeToken -> StatusAnfrageObjekt)                                               (Either (Objekt -> AnfrageAktion) (Objekt -> Aktion))

instance Show AnfrageAktion where
    show :: AnfrageAktion -> String
    show    (AAUnbekannt anfrageAktion eingabe)                         = unpack $ unbekanntShowText anfrageAktion eingabe
    show    (AnfrageAktion)                                             = Language.aktion
    show    (AARückgängig)                                              = Language.rückgängig
    show    (AAWarten)                                                  = Language.aktion <^> Language.warten
    show    (AAWegstrecke qAktionWegstrecke)                            = Language.aktion <^> showText qAktionWegstrecke
    show    (AAWeiche qAktionWeiche)                                    = Language.aktion <^> showText qAktionWeiche
    show    (AABahngeschwindigkeit qAktionBahngeschwindigkeit)          = Language.aktion <^> showText qAktionBahngeschwindigkeit
    show    (AAStreckenabschnitt qAktionStreckenabschnitt)              = Language.aktion <^> showText qAktionStreckenabschnitt
    show    (AAKupplung qAktionKupplung)                                = Language.aktion <^> showText qAktionKupplung
    show    (AAStatusAnfrage objektStatusAnfrage _eitherKonstruktor)    = Language.aktion <^> showText objektStatusAnfrage
    show    (AAKlassifizierung anfrageKonstruktor _eitherF)             = Language.aktion <-> Language.objekt <^> showText (anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing})
instance Anfrage AnfrageAktion where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktion -> s
    zeigeAnfrage    (AAUnbekannt anfrageAktion _eingabe)                        = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AnfrageAktion)                                             = Language.aktion
    zeigeAnfrage    (AARückgängig)                                              = Language.aktion
    zeigeAnfrage    (AAWarten)                                                  = Language.zeit
    zeigeAnfrage    (AAWegstrecke qAktionWegstrecke)                            = zeigeAnfrage qAktionWegstrecke
    zeigeAnfrage    (AAWeiche qAktionWeiche)                                    = zeigeAnfrage qAktionWeiche
    zeigeAnfrage    (AABahngeschwindigkeit qAktionBahngeschwindigkeit)          = zeigeAnfrage qAktionBahngeschwindigkeit
    zeigeAnfrage    (AAStreckenabschnitt qAktionStreckenabschnitt)              = zeigeAnfrage qAktionStreckenabschnitt
    zeigeAnfrage    (AAKupplung qAktionKupplung)                                = zeigeAnfrage qAktionKupplung
    zeigeAnfrage    (AAStatusAnfrage objektStatusAnfrage _eitherKonstruktor)    = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage    (AAKlassifizierung anfrageKonstruktor _eitherF)             = zeigeAnfrage $ anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageAktion -> s -> s
    zeigeAnfrageFehlgeschlagen  a@(AAWarten)    eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a               eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktion -> Maybe s
    zeigeAnfrageOptionen (AAUnbekannt anfrageAktion _eingabe)                       = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AnfrageAktion)                                            = Just $ toBefehlsString Language.aktionGruppen
    zeigeAnfrageOptionen (AARückgängig)                                             = Nothing
    zeigeAnfrageOptionen (AAWarten)                                                 = Nothing
    zeigeAnfrageOptionen (AAWegstrecke qAktionWegstrecke)                           = zeigeAnfrageOptionen qAktionWegstrecke
    zeigeAnfrageOptionen (AAWeiche qAktionWeiche)                                   = zeigeAnfrageOptionen qAktionWeiche
    zeigeAnfrageOptionen (AABahngeschwindigkeit qAktionBahngeschwindigkeit)         = zeigeAnfrageOptionen qAktionBahngeschwindigkeit
    zeigeAnfrageOptionen (AAStreckenabschnitt qAktionStreckenabschnitt)             = zeigeAnfrageOptionen qAktionStreckenabschnitt
    zeigeAnfrageOptionen (AAKupplung qAktionKupplung)                               = zeigeAnfrageOptionen qAktionKupplung
    zeigeAnfrageOptionen (AAStatusAnfrage objektStatusAnfrage _eitherKonstruktor)   = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen (AAKlassifizierung anfrageKonstruktor _eitherF)            = zeigeAnfrageOptionen $ anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}

-- | 'Aktion'-Klassifizierungen
data AnfrageAktionElement   = AAEUnbekannt              Text
                            | AAERückgängig
                            | AAEWarten
                            | AAEWegstrecke
                            | AAEWeiche
                            | AAEBahngeschwindigkeit
                            | AAEStreckenabschnitt
                            | AAEKupplung

-- | Eingabe einer 'Aktion'
anfrageAktionAktualisieren :: AnfrageAktion -> EingabeToken -> Either AnfrageAktion Aktion
anfrageAktionAktualisieren (AnfrageAktion)                                  token                               = Left $ case anfrageAktionElement token of
    (AAEUnbekannt eingabe)      -> AAUnbekannt AnfrageAktion eingabe
    (AAERückgängig)             -> AARückgängig
    (AAEWarten)                 -> AAWarten
    (AAEWegstrecke)             -> AAKlassifizierung SAOWegstrecke $ Left $ \(OWegstrecke wegstrecke) -> AAWegstrecke $ AnfrageAktionWegstrecke wegstrecke
    (AAEWeiche)                 -> AAKlassifizierung SAOWeiche $ Left $ \(OWeiche weiche) -> AAWeiche $ AnfrageAktionWeiche weiche
    (AAEBahngeschwindigkeit)    -> AAKlassifizierung SAOBahngeschwindigkeit $ Left $ \(OBahngeschwindigkeit bahngeschwindigkeit) -> AABahngeschwindigkeit $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
    (AAEStreckenabschnitt)      -> AAKlassifizierung SAOStreckenabschnitt $ Left $ \(OStreckenabschnitt streckenabschnitt) -> AAStreckenabschnitt $ AnfrageAktionStreckenabschnitt streckenabschnitt
    (AAEKupplung)               -> AAKlassifizierung SAOKupplung $ Left $ \(OKupplung kupplung) -> AAKupplung $ AnfrageAktionKupplung kupplung
    where
        anfrageAktionElement :: EingabeToken -> AnfrageAktionElement
        anfrageAktionElement  token@(EingabeToken {eingabe})  = wähleBefehl token [
            (Lexer.Rückgängig           , AAERückgängig),
            (Lexer.Warten               , AAEWarten),
            (Lexer.Wegstrecke           , AAEWegstrecke),
            (Lexer.Weiche               , AAEWeiche),
            (Lexer.Bahngeschwindigkeit  , AAEBahngeschwindigkeit),
            (Lexer.Streckenabschnitt    , AAEStreckenabschnitt),
            (Lexer.Kupplung             , AAEKupplung)]
            $ AAEUnbekannt eingabe
anfrageAktionAktualisieren _anfrage                                         (EingabeToken {möglichkeiten})
    | elem Lexer.Rückgängig möglichkeiten                                                                       = Left AnfrageAktion
anfrageAktionAktualisieren (AAWarten)                                       (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ AAUnbekannt AAWarten eingabe
    (Just zeit) -> Right $ Warten zeit
anfrageAktionAktualisieren (AAKlassifizierung anfrageKonstruktor eitherF)   token                               = Left $ AAStatusAnfrage (anfrageKonstruktor token) eitherF
anfrageAktionAktualisieren (AAWegstrecke anfrageAktion)                     token                               = case anfrageAktionWegstreckeAktualisieren anfrageAktion token of
    (Left (AAWSUnbekannt anfrage eingabe))    -> Left $ AAUnbekannt (AAWegstrecke anfrage) eingabe
    (Left qAktionWegstrecke)                -> Left $ AAWegstrecke qAktionWegstrecke
    (Right aktionWegstrecke)                -> Right $ AWegstrecke aktionWegstrecke
anfrageAktionAktualisieren (AAWeiche anfrageAktion)                          token                               = case anfrageAktionWeicheAktualisieren anfrageAktion token of
    (Left (AAWUnbekannt anfrage eingabe)) -> Left $ AAUnbekannt (AAWeiche anfrage) eingabe
    (Left qAktionWeiche)                -> Left $ AAWeiche qAktionWeiche
    (Right aktionWeiche)                -> Right $ AWeiche aktionWeiche
anfrageAktionAktualisieren (AABahngeschwindigkeit anfrageAktion)            token                               = case anfrageAktionBahngeschwindigkeitAktualisieren anfrageAktion token of
    (Left (AABGUnbekannt anfrage eingabe))    -> Left $ AAUnbekannt (AABahngeschwindigkeit anfrage) eingabe
    (Left qAktionBahngeschwindigkeit)       -> Left $ AABahngeschwindigkeit qAktionBahngeschwindigkeit
    (Right aktionBahngeschwindigkeit)       -> Right $ ABahngeschwindigkeit aktionBahngeschwindigkeit
anfrageAktionAktualisieren (AAStreckenabschnitt anfrageAktion)              token                               = case anfrageAktionStreckenabschnittAktualisieren anfrageAktion token of
    (Left (AASTUnbekannt anfrage eingabe)) -> Left $ AAUnbekannt (AAStreckenabschnitt anfrage) eingabe
    (Left qAktionStreckenabschnitt)     -> Left $ AAStreckenabschnitt qAktionStreckenabschnitt
    (Right aktionStreckenabschnitt)     -> Right $ AStreckenabschnitt aktionStreckenabschnitt
anfrageAktionAktualisieren (AAKupplung anfrageAktion)                       token                               = case anfrageAktionKupplungAktualisieren anfrageAktion token of
    (Left (AAKUUnbekannt anfrage eingabe)) -> Left $ AAUnbekannt (AAKupplung anfrage) eingabe
    (Left qAktionKupplung)              -> Left $ AAKupplung qAktionKupplung
    (Right aktionKupplung)              -> Right $ AKupplung aktionKupplung
anfrageAktionAktualisieren anfrage                                          _token                              = Left anfrage

-- *** Wegstrecken-Aktion
-- | Unvollständige 'Aktion' einer 'Wegstrecke'
data AnfrageAktionWegstrecke aw w   = AnfrageAktionWegstrecke   w
                                    | AAWSUnbekannt             (AnfrageAktionWegstrecke aw w)          Text
                                    | AAWSBahngeschwindigkeit   (AnfrageAktionBahngeschwindigkeit aw w)
                                    | AAWSStreckenabschnitt     (AnfrageAktionStreckenabschnitt aw w)
                                    | AAWSKupplung              (AnfrageAktionKupplung aw w)

instance (Show aw, Show w) => Show (AnfrageAktionWegstrecke aw w) where
    show :: AnfrageAktionWegstrecke aw w -> String
    show    (AnfrageAktionWegstrecke wegstrecke)      = Language.wegstrecke <=> showText wegstrecke
    show    (AAWSUnbekannt anfrageAktion eingabe)     = unpack $ unbekanntShowText anfrageAktion eingabe
    show    (AAWSBahngeschwindigkeit anfrageAktion)   = showText anfrageAktion
    show    (AAWSStreckenabschnitt anfrageAktion)     = showText anfrageAktion
    show    (AAWSKupplung anfrageAktion)              = showText anfrageAktion
instance Anfrage (AnfrageAktionWegstrecke aw w) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionWegstrecke aw w -> s
    zeigeAnfrage    (AnfrageAktionWegstrecke _wegstrecke)     = Language.aktion
    zeigeAnfrage    (AAWSUnbekannt anfrageAktion _eingabe)    = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWSBahngeschwindigkeit anfrageAktion)   = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWSStreckenabschnitt anfrageAktion)     = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWSKupplung anfrageAktion)              = zeigeAnfrage anfrageAktion
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionWegstrecke aw w -> Maybe s
    zeigeAnfrageOptionen (AnfrageAktionWegstrecke _wegstrecke)     = Just $ toBefehlsString Language.aktionWegstrecke
    zeigeAnfrageOptionen (AAWSUnbekannt anfrageAktion _eingabe)    = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSBahngeschwindigkeit anfrageAktion)   = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSStreckenabschnitt anfrageAktion)     = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSKupplung anfrageAktion)              = zeigeAnfrageOptionen anfrageAktion

-- | Eingabe einer Wegstrecken-Aktion
anfrageAktionWegstreckeAktualisieren :: (WegstreckeKlasse w) => AnfrageAktionWegstrecke aw w -> EingabeToken -> Either (AnfrageAktionWegstrecke aw w) (AktionWegstrecke w)
anfrageAktionWegstreckeAktualisieren   anfrage@(AAWSUnbekannt _ _)                  _token                          = Left anfrage
anfrageAktionWegstreckeAktualisieren   anfrage@(AnfrageAktionWegstrecke wegstrecke) token@(EingabeToken {eingabe})  = wähleBefehl token [
    (Lexer.Einstellen       , Right $ Einstellen wegstrecke),
    (Lexer.Geschwindigkeit  , Left $ AAWSBahngeschwindigkeit $ AABGGeschwindigkeit wegstrecke),
    (Lexer.Umdrehen         , Left $ AAWSBahngeschwindigkeit $ AABGUmdrehen wegstrecke),
    (Lexer.Strom            , Left $ AAWSStreckenabschnitt $ AASTStrom wegstrecke),
    (Lexer.Kuppeln          , Right $ AWSKupplung $ Kuppeln wegstrecke)]
    $ Left $ AAWSUnbekannt anfrage eingabe
anfrageAktionWegstreckeAktualisieren   (AAWSBahngeschwindigkeit qAktion0)           token                           = case anfrageAktionBahngeschwindigkeitAktualisieren qAktion0 token of
    (Left (AABGUnbekannt anfrage eingabe))  -> Left $ AAWSUnbekannt (AAWSBahngeschwindigkeit anfrage) eingabe
    (Left aAktion1)                         -> Left $ AAWSBahngeschwindigkeit aAktion1
    (Right aktion)                          -> Right $ AWSBahngeschwindigkeit aktion
anfrageAktionWegstreckeAktualisieren   (AAWSStreckenabschnitt qAktion0)             token                           = case anfrageAktionStreckenabschnittAktualisieren qAktion0 token of
    (Left (AASTUnbekannt anfrage eingabe))  -> Left $ AAWSUnbekannt (AAWSStreckenabschnitt anfrage) eingabe
    (Left aAktion1)                         -> Left $ AAWSStreckenabschnitt aAktion1
    (Right aktion)                          -> Right $ AWSStreckenabschnitt aktion
anfrageAktionWegstreckeAktualisieren   (AAWSKupplung qAktion0)                      token                           = case anfrageAktionKupplungAktualisieren qAktion0 token of
    (Left (AAKUUnbekannt anfrage eingabe))  -> Left $ AAWSUnbekannt (AAWSKupplung anfrage) eingabe
    (Left aAktion1)                         -> Left $ AAWSKupplung aAktion1
    (Right aktion)                          -> Right $ AWSKupplung aktion

-- *** Weichen-Aktion
-- | Unvollständige 'Aktion' einer 'Weiche'
data AnfrageAktionWeiche aw w   = AnfrageAktionWeiche   w
                                | AAWUnbekannt          (AnfrageAktionWeiche aw w)  Text
                                | AAWStellen            w

instance (Show aw, Show w) => Show (AnfrageAktionWeiche aw w) where
    show :: AnfrageAktionWeiche aw w -> String
    show    (AnfrageAktionWeiche weiche)            = Language.weiche <=> showText weiche
    show    (AAWUnbekannt anfrageAktion eingabe)    = unpack $ unbekanntShowText anfrageAktion eingabe
    show    (AAWStellen weiche)                     = Language.weiche <=> showText weiche <^> Language.stellen
instance Anfrage (AnfrageAktionWeiche aw w) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionWeiche aw w -> s
    zeigeAnfrage    (AnfrageAktionWeiche _weiche)           = Language.aktion
    zeigeAnfrage    (AAWUnbekannt anfrageAktion _eingabe)   = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWStellen _weiche)                    = Language.richtung
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionWeiche aw w -> Maybe s
    zeigeAnfrageOptionen (AnfrageAktionWeiche _weiche)          = Just $ toBefehlsString Language.aktionWeiche
    zeigeAnfrageOptionen (AAWUnbekannt anfrageAktion _eingabe)  = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWStellen _weiche)                   = Just $ toBefehlsString $ NE.toList $ fmap showText unterstützteRichtungen

-- | Eingabe einer Weichen-Aktion
anfrageAktionWeicheAktualisieren :: (Show aw, Show w, WeicheKlasse w) => AnfrageAktionWeiche aw w -> EingabeToken -> Either (AnfrageAktionWeiche aw w) (AktionWeiche w)
anfrageAktionWeicheAktualisieren   anfrage@(AnfrageAktionWeiche weiche) token@(EingabeToken {eingabe})  = wähleBefehl token [(Lexer.Stellen  , Left $ AAWStellen weiche)] $ Left $ AAWUnbekannt anfrage eingabe
anfrageAktionWeicheAktualisieren   anfrage@(AAWStellen _weiche)         token@(EingabeToken {eingabe})  = case wähleRichtung token of
    (Nothing)       -> Left $ AAWUnbekannt anfrage eingabe
    (Just richtung) -> mitRichtung anfrage richtung
        where
            mitRichtung :: (Show aw, Show w, WeicheKlasse w) => AnfrageAktionWeiche aw w -> Richtung -> Either (AnfrageAktionWeiche aw w) (AktionWeiche w)
            mitRichtung  anfrage@(AAWStellen weiche)  richtung
                | hatRichtung weiche richtung                       = Right $ Stellen weiche richtung
                | otherwise                                         = Left $ AAWUnbekannt anfrage eingabe
            mitRichtung anfrage                       _richtung     = error $ "mitRichtung mit unbekannter anfrage aufgerufen: " ++ show anfrage
anfrageAktionWeicheAktualisieren   anfrage                              _token                          = Left anfrage

-- *** Bahngeschwindigkeit-Aktion
-- | Unvollständige 'Aktion' einer 'Bahngeschwindigkeit'
data AnfrageAktionBahngeschwindigkeit ab b  = AnfrageAktionBahngeschwindigkeit  b
                                            | AABGUnbekannt                     (AnfrageAktionBahngeschwindigkeit ab b) Text
                                            | AABGGeschwindigkeit               b
                                            | AABGUmdrehen                      b

instance (Show ab, Show b) => Show (AnfrageAktionBahngeschwindigkeit ab b) where
    show :: AnfrageAktionBahngeschwindigkeit ab b -> String
    show    (AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)  = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit
    show    (AABGUnbekannt anfrageAktion eingabe)                   = unpack $ unbekanntShowText anfrageAktion eingabe
    show    (AABGGeschwindigkeit bahngeschwindigkeit)               = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit <^> Language.geschwindigkeit
    show    (AABGUmdrehen bahngeschwindigkeit)                      = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit <^> Language.umdrehen
instance Anfrage (AnfrageAktionBahngeschwindigkeit ab b) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit ab b -> s
    zeigeAnfrage    (AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit) = Language.aktion
    zeigeAnfrage    (AABGUnbekannt anfrageAktion _eingabe)                  = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AABGGeschwindigkeit _bahngeschwindigkeit)              = Language.geschwindigkeit
    zeigeAnfrage    (AABGUmdrehen _bahngeschwindigkeit)                     = Language.fahrtrichtung
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit ab b -> s -> s
    zeigeAnfrageFehlgeschlagen  a@(AABGGeschwindigkeit _bahngeschwindigkeit)    eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen a                                                eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit bg b -> Maybe s
    zeigeAnfrageOptionen (AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit)    = Just $ toBefehlsString Language.aktionBahngeschwindigkeit
    zeigeAnfrageOptionen (AABGUnbekannt anfrageAktion _eingabe)                     = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AABGGeschwindigkeit _bahngeschwindigkeit)                 = Nothing
    zeigeAnfrageOptionen (AABGUmdrehen _bahngeschwindigkeit)                        = Just $ toBefehlsString $ map showText $ NE.toList unterstützteFahrtrichtungen

-- | Eingabe einer Bahngeschwindigkeit-Aktion
anfrageAktionBahngeschwindigkeitAktualisieren :: (BahngeschwindigkeitKlasse b) => AnfrageAktionBahngeschwindigkeit ab b -> EingabeToken -> Either (AnfrageAktionBahngeschwindigkeit ab b) (AktionBahngeschwindigkeit b)
anfrageAktionBahngeschwindigkeitAktualisieren  anfrage@(AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)   token@(EingabeToken {eingabe})      = wähleBefehl token [
    (Lexer.Geschwindigkeit  , Left $ AABGGeschwindigkeit bahngeschwindigkeit),
    (Lexer.Umdrehen         , if zugtyp bahngeschwindigkeit == Märklin then Right $ Umdrehen bahngeschwindigkeit Nothing else Left $ AABGUmdrehen bahngeschwindigkeit)]
    $ Left $ AABGUnbekannt anfrage eingabe
anfrageAktionBahngeschwindigkeitAktualisieren  anfrage@(AABGGeschwindigkeit bahngeschwindigkeit)                (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ AABGUnbekannt anfrage eingabe
    (Just wert) -> Right $ Geschwindigkeit bahngeschwindigkeit wert
anfrageAktionBahngeschwindigkeitAktualisieren  anfrage@(AABGUmdrehen bahngeschwindigkeit)                       token@(EingabeToken {eingabe})      = wähleBefehl token [
    (Lexer.Vorwärts , Right $ Umdrehen bahngeschwindigkeit $ Just Vorwärts),
    (Lexer.Rückwärts , Right $ Umdrehen bahngeschwindigkeit $ Just Rückwärts)]
    $ Left $ AABGUnbekannt anfrage eingabe
anfrageAktionBahngeschwindigkeitAktualisieren  anfrage                                                          _token                              = Left $ anfrage

-- *** Streckenabschnitt-Aktion
-- | Unvollständige 'Aktion' eines 'Streckenabschnitt's
data AnfrageAktionStreckenabschnitt as s    = AnfrageAktionStreckenabschnitt    s
                                            | AASTUnbekannt                     (AnfrageAktionStreckenabschnitt as s)   Text
                                            | AASTStrom                         s

instance (Show as, Show s) => Show (AnfrageAktionStreckenabschnitt as s) where
    show :: AnfrageAktionStreckenabschnitt as s -> String
    show    (AnfrageAktionStreckenabschnitt streckenabschnitt)  = Language.streckenabschnitt <=> showText streckenabschnitt
    show    (AASTUnbekannt anfrageAktion eingabe)               = unpack $ unbekanntShowText anfrageAktion eingabe
    show    (AASTStrom streckenabschnitt)                       = Language.streckenabschnitt <=> showText streckenabschnitt <^> Language.strom
instance Anfrage (AnfrageAktionStreckenabschnitt qst st) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionStreckenabschnitt qst st -> s
    zeigeAnfrage    (AnfrageAktionStreckenabschnitt _streckenabschnitt) = Language.aktion
    zeigeAnfrage    (AASTUnbekannt anfrageAktion _eingabe)              = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AASTStrom _streckenabschnitt)                      = Language.fließend <|> Language.gesperrt
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionStreckenabschnitt qst st -> Maybe s
    zeigeAnfrageOptionen (AnfrageAktionStreckenabschnitt _streckenabschnitt)    = Just $ toBefehlsString Language.aktionStreckenabschnitt
    zeigeAnfrageOptionen (AASTUnbekannt anfrageAktion _eingabe)                 = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AASTStrom _streckenabschnitt)                         = Just $ toBefehlsString [Language.an, Language.aus]

-- | Eingabe einer Streckenabschnitt-Aktion
anfrageAktionStreckenabschnittAktualisieren :: (StreckenabschnittKlasse s) => AnfrageAktionStreckenabschnitt as s -> EingabeToken -> Either (AnfrageAktionStreckenabschnitt as s) (AktionStreckenabschnitt s)
anfrageAktionStreckenabschnittAktualisieren anfrage@(AnfrageAktionStreckenabschnitt streckenabschnitt)  token@(EingabeToken {eingabe})  = wähleBefehl token [(Lexer.Strom, Left $ AASTStrom streckenabschnitt)] $ Left $ AASTUnbekannt anfrage eingabe
anfrageAktionStreckenabschnittAktualisieren anfrage@(AASTStrom streckenabschnitt)                       token@(EingabeToken {eingabe})  = wähleBefehl token [
    (Lexer.Fließend , Right $ Strom streckenabschnitt Fließend),
    (Lexer.An       , Right $ Strom streckenabschnitt Fließend),
    (Lexer.Gesperrt , Right $ Strom streckenabschnitt Gesperrt),
    (Lexer.Aus      , Right $ Strom streckenabschnitt Gesperrt)]
    $ Left $ AASTUnbekannt anfrage eingabe
anfrageAktionStreckenabschnittAktualisieren anfrage                                                     _token                          = Left $ anfrage

-- *** KupplungAktion
-- | Unvollständige 'Aktion' einer 'Kupplung'
data AnfrageAktionKupplung ak k = AnfrageAktionKupplung k
                                | AAKUUnbekannt             (AnfrageAktionKupplung ak k)    Text

instance (Show ak, Show k) => Show (AnfrageAktionKupplung ak k) where
    show :: AnfrageAktionKupplung ak k -> String
    show    (AnfrageAktionKupplung kupplung)        = Language.kupplung <=> showText kupplung
    show    (AAKUUnbekannt anfrageAktion eingabe)   = unpack $ unbekanntShowText anfrageAktion eingabe
instance Anfrage (AnfrageAktionKupplung ak k) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionKupplung ak k -> s
    zeigeAnfrage    (AnfrageAktionKupplung _kupplung)       = Language.aktion
    zeigeAnfrage    (AAKUUnbekannt anfrageAktion _eingabe)  = zeigeAnfrage anfrageAktion
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionKupplung ak k -> Maybe s
    zeigeAnfrageOptionen (AnfrageAktionKupplung _kupplung)      = Just $ toBefehlsString Language.aktionKupplung
    zeigeAnfrageOptionen (AAKUUnbekannt anfrageAktion _eingabe) = zeigeAnfrageOptionen anfrageAktion

-- | Eingabe einer Kupplung-Aktion
anfrageAktionKupplungAktualisieren :: (KupplungKlasse k) => AnfrageAktionKupplung ak k -> EingabeToken -> Either (AnfrageAktionKupplung ak k) (AktionKupplung k)
anfrageAktionKupplungAktualisieren anfrage@(AnfrageAktionKupplung kupplung) token@(EingabeToken {eingabe})  = wähleBefehl token [(Lexer.Kuppeln, Right $ Kuppeln kupplung)] $ Left $ AAKUUnbekannt anfrage eingabe
anfrageAktionKupplungAktualisieren anfrage                                  _token                          = Left $ anfrage

-- ** Wegstrecke
-- | Unvollständige 'Wegstrecke'
data AnfrageWegstrecke  = AnfrageWegstrecke
                        | AWSUnbekannt                          AnfrageWegstrecke                         Text
                        | AWegstreckeName                       Text
                        | AWegstreckeNameAnzahl                 Wegstrecke                              Natural
                        | AWegstreckeNameAnzahlWeicheRichtung   Wegstrecke                              Natural                                                         Weiche
                        | AWegstreckeIOStatus                   StatusAnfrageObjekt                     (Either (Objekt -> AnfrageWegstrecke) (Objekt -> Wegstrecke))
                        | AWSStatusAnfrage                      (EingabeToken -> StatusAnfrageObjekt)   (Either (Objekt -> AnfrageWegstrecke) (Objekt -> Wegstrecke))

instance Show AnfrageWegstrecke where
    show :: AnfrageWegstrecke -> String
    show    (AWSUnbekannt qWegstrecke eingabe)                              = unpack $ unbekanntShowText qWegstrecke eingabe
    show    (AnfrageWegstrecke)                                             = unpack $ Language.wegstrecke
    show    (AWegstreckeName name)                                          = unpack $ Language.wegstrecke <^> Language.name <=> name
    show    (AWegstreckeNameAnzahl acc anzahl)                              = unpack $ Language.wegstrecke <^> showText acc <^> Language.anzahl Language.wegstreckenElemente <=> showText anzahl
    show    (AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche)         = unpack $ Language.wegstrecke <^> showText acc <^> Language.anzahl Language.wegstreckenElemente <=> showText anzahl <^> showText weiche
    show    (AWegstreckeIOStatus objektStatusAnfrage _eitherKonstruktor)    = Language.wegstrecke <^> showText objektStatusAnfrage
    show    (AWSStatusAnfrage anfrageKonstruktor _eitherF)                  = Language.wegstreckenElement <^> showText (anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing})
instance Anfrage AnfrageWegstrecke where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageWegstrecke -> s
    zeigeAnfrage    (AWSUnbekannt qWegstrecke _eingabe)                             = zeigeAnfrage qWegstrecke
    zeigeAnfrage    (AnfrageWegstrecke)                                             = Language.name
    zeigeAnfrage    (AWegstreckeName _name)                                         = Language.anzahl Language.wegstreckenElemente
    zeigeAnfrage    (AWegstreckeNameAnzahl _acc _anzahl)                            = Language.wegstreckenElement
    zeigeAnfrage    (AWegstreckeNameAnzahlWeicheRichtung _acc _anzahl _weiche)      = Language.richtung
    zeigeAnfrage    (AWegstreckeIOStatus objektStatusAnfrage _eitherKonstruktor)    = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage    (AWSStatusAnfrage anfrageKonstruktor _eitherF)                  = zeigeAnfrage $ anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageWegstrecke -> Maybe s
    zeigeAnfrageOptionen (AWSUnbekannt qWegstrecke _eingabe)                            = zeigeAnfrageOptionen qWegstrecke
    zeigeAnfrageOptionen (AWegstreckeNameAnzahl _acc _anzahl)                           = Just $ toBefehlsString Language.befehlWegstreckenElemente
    zeigeAnfrageOptionen (AWegstreckeNameAnzahlWeicheRichtung _acc _anzahl _weiche)     = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen (AWegstreckeIOStatus objektStatusAnfrage _eitherKonstruktor)   = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen (AWSStatusAnfrage anfrageKonstruktor _eitherF)                 = zeigeAnfrageOptionen $ anfrageKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    zeigeAnfrageOptionen _anfrage                                                       = Nothing

-- | Eingabe einer Wegstrecke
anfrageWegstreckeAktualisieren :: AnfrageWegstrecke -> EingabeToken -> Either AnfrageWegstrecke Wegstrecke
anfrageWegstreckeAktualisieren (AnfrageWegstrecke)                                                                                  (EingabeToken {eingabe})            = Left $ AWegstreckeName eingabe
anfrageWegstreckeAktualisieren anfrage@(AWegstreckeName name)                                                                       (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)       -> Left $ AWSUnbekannt anfrage eingabe
    (Just anzahl)   -> Left $ AWegstreckeNameAnzahl (Wegstrecke {wsName=name, wsBahngeschwindigkeiten=[], wsStreckenabschnitte=[], wsWeichenRichtungen=[], wsKupplungen=[]}) anzahl
anfrageWegstreckeAktualisieren anfrage@(AWegstreckeNameAnzahl acc@(Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsKupplungen}) anzahl)   token                               = Left $ case anfrageWegstreckenElement token of
    (AWSEWeiche)                 -> AWSStatusAnfrage SAOWeiche $ Left $ anfrageWeicheAnhängen
    (AWSEBahngeschwindigkeit)    -> AWSStatusAnfrage SAOBahngeschwindigkeit eitherObjektAnhängen
    (AWSEStreckenabschnitt)      -> AWSStatusAnfrage SAOStreckenabschnitt eitherObjektAnhängen
    (AWSEKupplung)               -> AWSStatusAnfrage SAOKupplung eitherObjektAnhängen
    (AWSEUnbekannt eingabe)      -> AWSUnbekannt anfrage eingabe
    where
        anfrageWegstreckenElement :: EingabeToken -> AnfrageWegstreckenElement
        anfrageWegstreckenElement token@(EingabeToken {eingabe})  = wähleBefehl token [
            (Lexer.Weiche                , AWSEWeiche),
            (Lexer.Bahngeschwindigkeit   , AWSEBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , AWSEStreckenabschnitt),
            (Lexer.Kupplung              , AWSEKupplung)]
            $ AWSEUnbekannt eingabe
        eitherObjektAnhängen :: Either (Objekt -> AnfrageWegstrecke) (Objekt -> Wegstrecke)
        eitherObjektAnhängen = if anzahl > 1 then Left anfrageObjektAnhängen else Right objektAnhängen
        objektAnhängen :: Objekt -> Wegstrecke
        objektAnhängen  (OBahngeschwindigkeit bahngeschwindigkeit)  = acc {wsBahngeschwindigkeiten=bahngeschwindigkeit:wsBahngeschwindigkeiten}
        objektAnhängen  (OStreckenabschnitt streckenabschnitt)      = acc {wsStreckenabschnitte=streckenabschnitt:wsStreckenabschnitte}
        objektAnhängen  (OKupplung kupplung)                        = acc {wsKupplungen=kupplung:wsKupplungen}
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        objektAnhängen  _                                           = acc
        anfrageObjektAnhängen :: Objekt -> AnfrageWegstrecke
        anfrageObjektAnhängen objekt = AWegstreckeNameAnzahl (objektAnhängen objekt) $ pred anzahl
        anfrageWeicheAnhängen :: Objekt -> AnfrageWegstrecke
        anfrageWeicheAnhängen (OWeiche weiche)    = AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        anfrageWeicheAnhängen _                   = anfrage
anfrageWegstreckeAktualisieren (AWSStatusAnfrage anfrageKonstruktor eitherF)                                                        token                               = Left $ AWegstreckeIOStatus (anfrageKonstruktor token) eitherF
anfrageWegstreckeAktualisieren anfrage@(AWegstreckeNameAnzahlWeicheRichtung acc@(Wegstrecke {wsWeichenRichtungen}) anzahl weiche)   token@(EingabeToken {eingabe})      = case wähleRichtung token of
    (Nothing)       -> Left $ AWSUnbekannt anfrage eingabe
    (Just richtung) -> eitherWeicheRichtungAnhängen richtung
        where
            eitherWeicheRichtungAnhängen :: Richtung -> Either AnfrageWegstrecke Wegstrecke
            eitherWeicheRichtungAnhängen richtung = if anzahl > 1 then Left $ qWeicheRichtungAnhängen richtung else Right $ weicheRichtungAnhängen richtung
            qWeicheRichtungAnhängen :: Richtung -> AnfrageWegstrecke
            qWeicheRichtungAnhängen richtung = AWegstreckeNameAnzahl (weicheRichtungAnhängen richtung) $ pred anzahl
            weicheRichtungAnhängen :: Richtung -> Wegstrecke
            weicheRichtungAnhängen richtung = acc {wsWeichenRichtungen=(weiche, richtung):wsWeichenRichtungen}
anfrageWegstreckeAktualisieren anfrage                                                                                              _token                              = Left anfrage

-- ** Weiche
-- | Unvollständige 'Weiche'
data AnfrageWeiche  = AnfrageWeiche
                    | AWEUnbekannt                              AnfrageWeiche   Text
                    | ALegoWeiche
                    | ALegoWeicheName                           Text
                    | ALegoWeicheNameFließend                   Text            Value
                    | ALegoWeicheNameFließendRichtung1          Text            Value   Richtung
                    | ALegoWeicheNameFließendRichtungen         Text            Value   Richtung    Richtung
                    | AMärklinWeiche
                    | AMärklinWeicheName                        Text
                    | AMärklinWeicheNameFließend                Text            Value
                    | AMärklinWeicheNameFließendAnzahl          Text            Value   Natural   [(Richtung, Pin)]
                    | AMärklinWeicheNameFließendAnzahlRichtung  Text            Value   Natural   [(Richtung, Pin)]   Richtung

instance Show AnfrageWeiche where
    show :: AnfrageWeiche -> String
    show    (AnfrageWeiche)                                                                 = Language.weiche
    show    (AWEUnbekannt anfrage eingabe)                                                  = unpack $ unbekanntShowText anfrage eingabe
    show    (ALegoWeiche)                                                                   = Language.lego <-> Language.weiche
    show    (ALegoWeicheName name)                                                          = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name
    show    (ALegoWeicheNameFließend name fließend)                                         = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> showText fließend
    show    (ALegoWeicheNameFließendRichtung1 name fließend richtung1)                      = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> showText fließend <^> showText richtung1
    show    (ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2)           = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> showText fließend <^> showText richtung1 <^> showText richtung2
    show    (AMärklinWeiche)                                                                = Language.märklin <-> Language.weiche
    show    (AMärklinWeicheName name)                                                       = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name
    show    (AMärklinWeicheNameFließend name fließend)                                      = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> showText fließend
    show    (AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)                     = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> showText fließend <^> Language.erwartet Language.richtungen <=> showText anzahl <^> showText acc
    show    (AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung)    = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> showText fließend <^> Language.erwartet Language.richtungen <=> showText anzahl <^> showText acc <> Language.richtung <=> showText richtung
instance Anfrage AnfrageWeiche where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageWeiche -> s
    zeigeAnfrage    (AnfrageWeiche)                                                                     = Language.zugtyp
    zeigeAnfrage    (AWEUnbekannt anfrage _eingabe)                                                     = zeigeAnfrage anfrage
    zeigeAnfrage    (ALegoWeiche)                                                                       = Language.name
    zeigeAnfrage    (ALegoWeicheName _name)                                                             = Language.fließendValue
    zeigeAnfrage    (ALegoWeicheNameFließend _name _fließend)                                           = Language.richtung
    zeigeAnfrage    (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1)                       = Language.richtung
    zeigeAnfrage    (ALegoWeicheNameFließendRichtungen _name _fließen _richtung1 _richtung2)            = Language.pin
    zeigeAnfrage    (AMärklinWeiche)                                                                    = Language.name
    zeigeAnfrage    (AMärklinWeicheName _name)                                                          = Language.fließendValue
    zeigeAnfrage    (AMärklinWeicheNameFließend _name _fließend)                                        = Language.anzahl Language.richtungen
    zeigeAnfrage    (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc)                     = Language.richtung
    zeigeAnfrage    (AMärklinWeicheNameFließendAnzahlRichtung _name _fließend _anzahl _acc _richtung)   = Language.pin
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageWeiche -> s -> s
    zeigeAnfrageFehlgeschlagen  a@(ALegoWeicheNameFließendRichtungen _name _fließend _richtung1 _richtung2)         eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a@(AMärklinWeicheNameFließend _name _fließend)                                      eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a@(AMärklinWeicheNameFließendAnzahlRichtung _name _fließend _anzahl _acc _richtung) eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a                                                                                   eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageWeiche -> Maybe s
    zeigeAnfrageOptionen (AnfrageWeiche)                                                    = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen (AWEUnbekannt anfrage _eingabe)                                    = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen (ALegoWeicheName _name)                                            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen (ALegoWeicheNameFließend _name _fließend)                          = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1)      = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen (AMärklinWeicheName _name)                                         = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc)    = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen _anfrage                                                           = Nothing

-- | Eingabe einer Weiche
anfrageWeicheAktualisieren :: AnfrageWeiche -> EingabeToken -> Either AnfrageWeiche Weiche
anfrageWeicheAktualisieren (AnfrageWeiche)                                                                      token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.Märklin  , AMärklinWeiche),
    (Lexer.Lego     , ALegoWeiche)]
    $ AWEUnbekannt AnfrageWeiche eingabe
anfrageWeicheAktualisieren (ALegoWeiche)                                                                        (EingabeToken {eingabe})            = Left $ ALegoWeicheName eingabe
anfrageWeicheAktualisieren anfrage@(ALegoWeicheName name)                                                       token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.HIGH , ALegoWeicheNameFließend name HIGH),
    (Lexer.LOW  , ALegoWeicheNameFließend name LOW)]
    $ AWEUnbekannt anfrage eingabe
anfrageWeicheAktualisieren anfrage@(ALegoWeicheNameFließend name fließend)                                      token@(EingabeToken {eingabe})      = Left $ case wähleRichtung token of
    (Nothing)           -> AWEUnbekannt anfrage eingabe
    (Just richtung1)    -> ALegoWeicheNameFließendRichtung1 name fließend richtung1
anfrageWeicheAktualisieren anfrage@(ALegoWeicheNameFließendRichtung1 name fließend richtung1)                   token@(EingabeToken {eingabe})      = Left $ case wähleRichtung token of
    (Nothing)           -> AWEUnbekannt anfrage eingabe
    (Just richtung2)    -> ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2
anfrageWeicheAktualisieren anfrage@(ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2)        (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ AWEUnbekannt anfrage eingabe
    (Just pin)  -> Right $ LegoWeiche {weName=name, weFließend=fließend, richtungsPin=zuPin pin, richtungen=(richtung1,richtung2)}
anfrageWeicheAktualisieren (AMärklinWeiche)                                                                     (EingabeToken {eingabe})            = Left $ AMärklinWeicheName eingabe
anfrageWeicheAktualisieren anfrage@(AMärklinWeicheName name)                                                    token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.HIGH , AMärklinWeicheNameFließend name HIGH),
    (Lexer.LOW  , AMärklinWeicheNameFließend name LOW)]
    $ AWEUnbekannt anfrage eingabe
anfrageWeicheAktualisieren anfrage@(AMärklinWeicheNameFließend name fließend)                                   (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)       -> Left $ AWEUnbekannt anfrage eingabe
    (Just anzahl)   -> Left $ AMärklinWeicheNameFließendAnzahl name fließend anzahl []
anfrageWeicheAktualisieren anfrage@(AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)                  token@(EingabeToken {eingabe})      = Left $ case wähleRichtung token of
    (Nothing)       -> AWEUnbekannt anfrage eingabe
    (Just richtung) -> AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung
anfrageWeicheAktualisieren anfrage@(AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung) (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)           -> Left $ AWEUnbekannt anfrage eingabe
    (Just pin)
        | anzahl > 1    -> Left $ AMärklinWeicheNameFließendAnzahl name fließend (pred anzahl) $ (richtung, zuPin pin):acc
        | otherwise     -> Right MärklinWeiche {weName=name, weFließend=fließend, richtungsPins=(richtung, zuPin pin):|acc}
anfrageWeicheAktualisieren anfrage@(AWEUnbekannt _ _)                                                           _token                              = Left anfrage

-- ** Bahngeschwindigkeit
-- | Unvollständige 'Bahngeschwindigkeit'
data AnfrageBahngeschwindigkeit = AnfrageBahngeschwindigkeit
                                | ABGUnbekannt                                          AnfrageBahngeschwindigkeit  Text
                                | ALegoBahngeschwindigkeit
                                | ALegoBahngeschwindigkeitName                          Text
                                | ALegoBahngeschwindigkeitNameFließend                  Text                        Value
                                | ALegoBahngeschwindigkeitNameFließendGeschwindigkeit   Text                        Value   Pin
                                | AMärklinBahngeschwindigkeit
                                | AMärklinBahngeschwindigkeitName                       Text
                                | AMärklinBahngeschwindigkeitNameFließend               Text                        Value

instance Show AnfrageBahngeschwindigkeit where
    show :: AnfrageBahngeschwindigkeit -> String
    show    (AnfrageBahngeschwindigkeit)                                            = Language.bahngeschwindigkeit
    show    (ABGUnbekannt anfrage eingabe)                                          = unpack $ unbekanntShowText anfrage eingabe
    show    (ALegoBahngeschwindigkeit)                                              = Language.lego <-> Language.bahngeschwindigkeit
    show    (ALegoBahngeschwindigkeitName name)                                     = unpack $ Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    show    (ALegoBahngeschwindigkeitNameFließend name fließend)                    = unpack $ Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name <^> Language.fließendValue <=> showText fließend
    show    (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit name fließend pin) = unpack $ Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name <^> Language.fließendValue <=> showText fließend <^> Language.pin <=> showText pin
    show    (AMärklinBahngeschwindigkeit)                                           = Language.märklin <-> Language.bahngeschwindigkeit
    show    (AMärklinBahngeschwindigkeitName name)                                  = unpack $ Language.märklin <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    show    (AMärklinBahngeschwindigkeitNameFließend name fließend)                 = unpack $ Language.märklin <-> Language.bahngeschwindigkeit <^> Language.name <=> name <^> Language.fließendValue <=> showText fließend
instance Anfrage AnfrageBahngeschwindigkeit where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit -> s
    zeigeAnfrage    (AnfrageBahngeschwindigkeit)                                                = Language.zugtyp
    zeigeAnfrage    (ABGUnbekannt anfrage _eingabe)                                             = zeigeAnfrage anfrage
    zeigeAnfrage    (ALegoBahngeschwindigkeit)                                                  = Language.name
    zeigeAnfrage    (ALegoBahngeschwindigkeitName _name)                                        = Language.fließendValue
    zeigeAnfrage    (ALegoBahngeschwindigkeitNameFließend _name _fließend)                      = Language.pin
    zeigeAnfrage    (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit _name _fließend _pin)  = Language.pin
    zeigeAnfrage    (AMärklinBahngeschwindigkeit)                                               = Language.name
    zeigeAnfrage    (AMärklinBahngeschwindigkeitName _name)                                     = Language.fließendValue
    zeigeAnfrage    (AMärklinBahngeschwindigkeitNameFließend _name _fließend)                   = Language.pin
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit -> s -> s
    zeigeAnfrageFehlgeschlagen  a@(ALegoBahngeschwindigkeitName _name)                                          eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit _name _fließend _pin)    eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a@(AMärklinBahngeschwindigkeitName _name)                                       eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a                                                                               eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit -> Maybe s
    zeigeAnfrageOptionen (AnfrageBahngeschwindigkeit)               = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen (ALegoBahngeschwindigkeitName _name)       = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen (AMärklinBahngeschwindigkeitName _name)    = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen (ABGUnbekannt anfrage _eingabe)            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen _anfrage                                   = Nothing

-- | Eingabe einer Bahngeschwindigkeit
anfrageBahngeschwindigkeitAktualisieren :: AnfrageBahngeschwindigkeit -> EingabeToken -> Either AnfrageBahngeschwindigkeit Bahngeschwindigkeit
anfrageBahngeschwindigkeitAktualisieren    (AnfrageBahngeschwindigkeit)                                                                     token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.Märklin  , AMärklinBahngeschwindigkeit),
    (Lexer.Lego     , ALegoBahngeschwindigkeit)]
    $ ABGUnbekannt AnfrageBahngeschwindigkeit eingabe
anfrageBahngeschwindigkeitAktualisieren    (ALegoBahngeschwindigkeit)                                                                       (EingabeToken {eingabe})            = Left $ ALegoBahngeschwindigkeitName eingabe
anfrageBahngeschwindigkeitAktualisieren    anfrage@(ALegoBahngeschwindigkeitName name)                                                      token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.HIGH , ALegoBahngeschwindigkeitNameFließend name HIGH),
    (Lexer.LOW  , ALegoBahngeschwindigkeitNameFließend name LOW)]
    $ ABGUnbekannt anfrage eingabe
anfrageBahngeschwindigkeitAktualisieren    anfrage@(ALegoBahngeschwindigkeitNameFließend name fließend)                                     (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ ABGUnbekannt anfrage eingabe
    (Just pin)  -> Left $ ALegoBahngeschwindigkeitNameFließendGeschwindigkeit name fließend $ zuPin pin
anfrageBahngeschwindigkeitAktualisieren    anfrage@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit name fließend geschwindigkeitsPin)  (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ ABGUnbekannt anfrage eingabe
    (Just pin)  -> Right $ LegoBahngeschwindigkeit {bgName=name, bgFließend=fließend, geschwindigkeitsPin, fahrtrichtungsPin=zuPin pin}
anfrageBahngeschwindigkeitAktualisieren    (AMärklinBahngeschwindigkeit)                                                                    (EingabeToken {eingabe})            = Left $ AMärklinBahngeschwindigkeitName eingabe
anfrageBahngeschwindigkeitAktualisieren    anfrage@(AMärklinBahngeschwindigkeitName name)                                                   token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.HIGH , AMärklinBahngeschwindigkeitNameFließend name HIGH),
    (Lexer.LOW  , AMärklinBahngeschwindigkeitNameFließend name LOW)]
    $ ABGUnbekannt anfrage eingabe
anfrageBahngeschwindigkeitAktualisieren    anfrage@(AMärklinBahngeschwindigkeitNameFließend name fließend)                                 (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ ABGUnbekannt anfrage eingabe
    (Just pin)  -> Right $ MärklinBahngeschwindigkeit {bgName=name, bgFließend=fließend, geschwindigkeitsPin=zuPin pin}
anfrageBahngeschwindigkeitAktualisieren    anfrage@(ABGUnbekannt _ _)                                                                       _token                              = Left anfrage

-- ** Streckenabschnitt
-- | Unvollständiger 'Streckenabschnitt'
data AnfrageStreckenabschnitt   = AnfrageStreckenabschnitt
                                | ASTUnbekannt                      AnfrageStreckenabschnitt    Text
                                | AStreckenabschnittName            Text
                                | AStreckenabschnittNameFließend    Text                        Value

instance Show AnfrageStreckenabschnitt where
    show :: AnfrageStreckenabschnitt -> String
    show    (AnfrageStreckenabschnitt)                      = Language.streckenabschnitt
    show    (ASTUnbekannt anfrage eingabe)                  = unpack $ unbekanntShowText anfrage eingabe
    show    (AStreckenabschnittName name)                   = unpack $ Language.streckenabschnitt <^> Language.name <=> name
    show    (AStreckenabschnittNameFließend name fließend)  = unpack $ Language.streckenabschnitt <^> Language.name <=> name <^> Language.fließendValue <=> showText fließend
instance Anfrage AnfrageStreckenabschnitt where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> s
    zeigeAnfrage    (AnfrageStreckenabschnitt)                          = Language.name
    zeigeAnfrage    (ASTUnbekannt anfrage _eingabe)                     = zeigeAnfrage anfrage
    zeigeAnfrage    (AStreckenabschnittName _name)                      = Language.fließendValue
    zeigeAnfrage    (AStreckenabschnittNameFließend _name _fließend)    = Language.pin
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> s -> s
    zeigeAnfrageFehlgeschlagen  a@(AStreckenabschnittName _name)    eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a                                   eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> Maybe s
    zeigeAnfrageOptionen (AStreckenabschnittName _name)     = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen (ASTUnbekannt anfrage _eingabe)    = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen _anfrage                           = Nothing

    -- | Eingabe eines Streckenabschnitts
anfrageStreckenabschnittAktualisieren :: AnfrageStreckenabschnitt -> EingabeToken -> Either AnfrageStreckenabschnitt Streckenabschnitt
anfrageStreckenabschnittAktualisieren   (AnfrageStreckenabschnitt)                              (EingabeToken {eingabe})            = Left $ AStreckenabschnittName eingabe
anfrageStreckenabschnittAktualisieren   anfrage@(AStreckenabschnittName name)                   token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.HIGH , AStreckenabschnittNameFließend name HIGH),
    (Lexer.LOW  , AStreckenabschnittNameFließend name LOW)]
    $ ASTUnbekannt anfrage eingabe
anfrageStreckenabschnittAktualisieren   anfrage@(AStreckenabschnittNameFließend name fließend)  (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ ASTUnbekannt anfrage eingabe
    (Just pin)  -> Right $ Streckenabschnitt {stName=name, stFließend=fließend, stromPin=zuPin pin}
anfrageStreckenabschnittAktualisieren   anfrage@(ASTUnbekannt _ _)                              _token                              = Left anfrage

-- ** Kupplung
-- | Unvollständige 'Kupplung'
data AnfrageKupplung    = AnfrageKupplung
                        | AKUUnbekannt          AnfrageKupplung Text
                        | AKupplungName         Text
                        | AKupplungNameFließend Text            Value

instance Show AnfrageKupplung where
    show :: AnfrageKupplung -> String
    show    (AnfrageKupplung)                       = Language.kupplung
    show    (AKUUnbekannt anfrage eingabe)          = unpack $ unbekanntShowText anfrage eingabe
    show    (AKupplungName name)                    = unpack $ Language.kupplung <^> Language.name <=> name
    show    (AKupplungNameFließend name fließend)   = unpack $ Language.kupplung <^> Language.name <=> name <^> Language.fließendValue <=> showText fließend
instance Anfrage AnfrageKupplung where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageKupplung -> s
    zeigeAnfrage    (AnfrageKupplung)                       = Language.name
    zeigeAnfrage    (AKUUnbekannt anfrage _eingabe)         = zeigeAnfrage anfrage
    zeigeAnfrage    (AKupplungName _name)                   = Language.fließendValue
    zeigeAnfrage    (AKupplungNameFließend _name _fließend) = Language.pin
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageKupplung -> s -> s
    zeigeAnfrageFehlgeschlagen  a@(AKupplungName _name) eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen  a                       eingabe = zeigeAnfrageFehlgeschlagenStandard a eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageKupplung -> Maybe s
    zeigeAnfrageOptionen (AKupplungName _name)              = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen (AKUUnbekannt anfrage _eingabe)    = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen _anfrage                           = Nothing

-- | Eingabe einer Kupplung
anfrageKupplungAktualisieren :: AnfrageKupplung -> EingabeToken -> Either AnfrageKupplung Kupplung
anfrageKupplungAktualisieren    (AnfrageKupplung)                               (EingabeToken {eingabe})            = Left $ AKupplungName eingabe
anfrageKupplungAktualisieren    anfrage@(AKupplungName name)                    token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.HIGH , AKupplungNameFließend name HIGH),
    (Lexer.LOW  , AKupplungNameFließend name LOW)]
    $ AKUUnbekannt anfrage eingabe
anfrageKupplungAktualisieren    anfrage@(AKupplungNameFließend name fließend)   (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ AKUUnbekannt anfrage eingabe
    (Just pin)  -> Right $ Kupplung {kuName=name, kuFließend=fließend, kupplungsPin=zuPin pin}
anfrageKupplungAktualisieren    anfrage@(AKUUnbekannt _ _)                      _token                              = Left anfrage

-- * Klasse für unvollständige Befehle
-- | Unvollständige Befehle/Objekte stellen Funktionen bereit dem Nutzer angzuzeigen, was als nächstes zum vervollständigen benötigt wird.
class Anfrage a where
    zeigeAnfrage :: (IsString s, Semigroup s) => a -> s
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => a -> s -> s
    zeigeAnfrageFehlgeschlagen = zeigeAnfrageFehlgeschlagenStandard
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => a -> Maybe s
    zeigeAnfrageOptionen _anfrage = Nothing
    {-# MINIMAL zeigeAnfrage #-}

-- | Standard-Implementierung zum Anzeigen einer fehlgeschlagenen 'Anfrage'
zeigeAnfrageFehlgeschlagenStandard :: (Anfrage a, IsString s, Semigroup s) => a -> s -> s
zeigeAnfrageFehlgeschlagenStandard a eingabe = Language.unbekannt (zeigeAnfrage a) <=> eingabe

-- | Zeige ein unvollständiges Objekt, gefolgt von der nächsten Nachfrage an
showMitAnfrage :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s
showMitAnfrage a = showText a <^> zeigeAnfrage a

-- | Zeige Meldung für eine invalide Eingabe auf die Nachfrage einer 'Anfrage' an
showMitAnfrageFehlgeschlagen :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s -> s
showMitAnfrageFehlgeschlagen a eingabe = showText a <^> zeigeAnfrageFehlgeschlagen a eingabe

-- * Hilfs-Befehle
-- | Wähle aus möglichen Interpretationen der Eingabe die erste passende aus und gebe den daraus resultierenden Befehl zurück.
-- Falls keine Möglichkeit passend ist, gebe wird das Ersatz-Ergebnis zurückgegeben.
wähleBefehl :: EingabeToken -> [(Token, a)] -> a -> a
wähleBefehl _eingabe                                ([])                    ersatz  = ersatz
wähleBefehl eingabe@(EingabeToken {möglichkeiten})  ((befehl,ergebnis):t)   ersatz
    | elem befehl möglichkeiten                                                     = ergebnis
    | otherwise                                                                     = wähleBefehl eingabe t ersatz

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

-- | Element einer Liste anhand des Index oder Namens finden
findByNameOrIndex :: (StreckenObjekt a) => [a] -> EingabeToken -> Maybe a
findByNameOrIndex   liste   (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Just index)    | index >= 0, längerAls liste index     -> Just $ liste !! fromIntegral index
    _maybeIndex                                             -> listToMaybe $ filter ((== eingabe) . erhalteName) liste

-- | Prüft, ob eine Liste mindestens von der Länge i ist, ohne die komplette Länge zu berechnen
längerAls :: [a] -> Natural -> Bool
längerAls   ([])    i   = (i < 0)
längerAls   _liste  0   = True
längerAls   (_h:t)  i   = längerAls t $ pred i