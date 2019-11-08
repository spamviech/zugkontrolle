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
    AnfrageBefehl(..), BefehlSofort(..), AnfrageNeu(..),
    StatusAnfrageObjekt(..), StatusAnfrageObjektZugtyp(..), ObjektZugtyp(..), zuObjekt,
    AnfrageFortsetzung(..), verwendeAnfrageFortsetzung, ($<<),
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
import Data.Text (unpack)
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
                                unbekanntShowText, zeigeAnfrageFehlgeschlagenStandard,
                                wähleBefehl, wähleErgebnis,
                                AnfrageFortsetzung(..), verwendeAnfrageFortsetzung, ($<<))
import Zug.UI.Cmd.Parser.Plan (AnfragePlan(..), AnfrageAktion(..),
                                AnfrageAktionBahngeschwindigkeit(..), AnfrageAktionStreckenabschnitt(..),
                                AnfrageAktionWeiche(..), AnfrageAktionKupplung(..), AnfrageAktionWegstrecke(..))
import Zug.UI.Cmd.Parser.StreckenObjekt (AnfrageObjekt(..), AnfrageBahngeschwindigkeit(..), AnfrageStreckenabschnitt(..),
                                        AnfrageWeiche(..), AnfrageKupplung(..), AnfrageWegstrecke(..))

-- | Auswerten von Befehlen, so weit es ohne Status-Informationen möglich ist
parser ::
    AnfrageBefehl ->
    [EingabeTokenAllgemein] ->
        ([Befehl], AnfrageFortsetzung AnfrageBefehl (Either (BefehlSofort, [EingabeTokenAllgemein]) Befehl))
parser = parserAux []
    where
        parserAux ::
            [Befehl] ->
            AnfrageBefehl ->
            [EingabeTokenAllgemein] ->
                ([Befehl], AnfrageFortsetzung AnfrageBefehl (Either (BefehlSofort, [EingabeTokenAllgemein]) Befehl))
        parserAux
            acc
            AnfrageBefehl
            []
                = parserErgebnisOk acc
        parserAux
            acc
            (ABAktionPlanAusführend plan Neu)
            []
                = parserErgebnis acc $ AFZwischenwert (ABAktionPlanAusführend plan Alt)
        parserAux
            acc
            (ABAktionPlanGesperrt plan Neu pins)
            []
                = parserErgebnis acc $ AFZwischenwert (ABAktionPlanGesperrt plan Alt pins)
        parserAux
            acc
            (ABAktionPlanAusführend plan Alt)
            []
                = parserErgebnis acc $ AFErgebnis $ Left $ (BSAusführenMöglich plan, [])
        parserAux
            acc
            (ABAktionPlanGesperrt plan Alt _pins)
            []
                = parserErgebnis acc $ AFErgebnis $ Left $ (BSAusführenMöglich plan, [])
        parserAux
            acc
            anfrage
            []
                = parserErgebnis acc $ AFZwischenwert anfrage
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
                    (AFErgebnis (Left befehlSofort))
                        -> parserErgebnis acc $ AFErgebnis $ Left (befehlSofort, t)
                    (AFErgebnis (Right befehl))
                        -> parserAux (befehl : acc) AnfrageBefehl t
                    (AFZwischenwert aBefehl)
                        -> parserAux acc aBefehl t
                    (AFFehler anfrage eingabe)
                        -> parserErgebnis acc $ AFFehler anfrage eingabe
                    -- (AEStatusAnfrage eingabe konstruktor anfrage r)
                    --     -> parserErgebnis acc $ AEStatusAnfrage eingabe konstruktor anfrage $ r ++ t
                    -- (AEStatusAnfrageMärklin eingabe konstruktor anfrage r)
                    --     -> parserErgebnis acc $ AEStatusAnfrageMärklin eingabe konstruktor anfrage $ r ++ t
                    -- (AEStatusAnfrageLego eingabe konstruktor anfrage r)
                    --     -> parserErgebnis acc $ AEStatusAnfrageLego eingabe konstruktor anfrage $ r ++ t
        -- | Ergebnis zurückgeben
        parserErgebnis ::
            [Befehl] ->
            AnfrageFortsetzung AnfrageBefehl (Either (BefehlSofort, [EingabeTokenAllgemein]) Befehl) ->
                ([Befehl], AnfrageFortsetzung AnfrageBefehl (Either (BefehlSofort, [EingabeTokenAllgemein]) Befehl))
        parserErgebnis acc anfrageFortsetzung = (reverse acc, anfrageFortsetzung)
        parserErgebnisOk ::
            [Befehl] ->
                ([Befehl], AnfrageFortsetzung AnfrageBefehl (Either (BefehlSofort, [EingabeTokenAllgemein]) Befehl))
        parserErgebnisOk    []                  = parserErgebnis [] $ AFZwischenwert AnfrageBefehl
        parserErgebnisOk    (befehl : befehle)  = parserErgebnis befehle $ AFErgebnis $ Right befehl

-- ** Anfrage
-- -- | Rückgabe-Typen
-- data AnfrageMöglichkeiten
--     = AEBefehl
--         Befehl
--     | AEBefehlSofort
--         BefehlSofort                                -- ^ Sofort auszuführender Befehl (z.B. IO-Aktion)
--         [EingabeTokenAllgemein]                     -- ^ Nachfolge-Token
--     | AEStatusAnfrage
--         StatusAnfrageObjekt                         -- ^ Wonach wird gefragt?
--         (Objekt -> AnfrageMöglichkeiten)                 -- ^ Wozu wird das Objekt benötigt
--         AnfrageBefehl                               -- ^ Backup-Befehl, falls die Anfrage fehlschlägt
--         [EingabeTokenAllgemein]                     -- ^ Nachfolge-Token
--     | AEStatusAnfrageMärklin
--         (StatusAnfrageObjektZugtyp 'Märklin)        -- ^ Wonach wird gefragt?
--         (ObjektZugtyp 'Märklin -> AnfrageMöglichkeiten)  -- ^ Wozu wird das Objekt benötigt
--         AnfrageBefehl                               -- ^ Backup-Befehl, falls die Anfrage fehlschlägt
--         [EingabeTokenAllgemein]                     -- ^ Nachfolge-Token
--     | AEStatusAnfrageLego
--         (StatusAnfrageObjektZugtyp 'Lego)           -- ^ Wonach wird gefragt?
--         (ObjektZugtyp 'Lego -> AnfrageMöglichkeiten)     -- ^ Wozu wird das Objekt benötigt
--         AnfrageBefehl                               -- ^ Backup-Befehl, falls die Anfrage fehlschlägt
--         [EingabeTokenAllgemein]                     -- ^ Nachfolge-Token
--     | AEAnfrageBefehl
--         AnfrageBefehl

-- | Befehle, die sofort in 'IO' ausgeführt werden müssen
data BefehlSofort
    = BSLaden
        FilePath
    | BSAusführenMöglich
        Plan
    deriving (Eq, Show)

-- | Anfragen an den aktuellen 'Status'
data StatusAnfrage
    = StatusAnfrage
        -- | Wonach wird gefragt?
        StatusAnfrageObjekt
        -- | Wozu wird das Objekt benötigt
        (Objekt -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))
        -- | Backup-Befehl, falls die Anfrage fehlschlägt
        AnfrageBefehl
        -- | Nachfolge-Token
        [EingabeTokenAllgemein]
    | StatusAnfrageMärklin
        -- | Wonach wird gefragt?
        (StatusAnfrageObjektZugtyp 'Märklin)
        -- | Wozu wird das Objekt benötigt
        (ObjektZugtyp 'Märklin -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))
        -- | Backup-Befehl, falls die Anfrage fehlschlägt
        AnfrageBefehl
        -- | Nachfolge-Token
        [EingabeTokenAllgemein]
    | StatusAnfrageLego
        -- | Wonach wird gefragt?
        (StatusAnfrageObjektZugtyp 'Lego)
        -- | Wozu wird das Objekt benötigt
        (ObjektZugtyp 'Lego -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))
        -- | Backup-Befehl, falls die Anfrage fehlschlägt
        AnfrageBefehl
        -- | Nachfolge-Token
        [EingabeTokenAllgemein]

-- | Mögliche Ergebnisse von 'parser', die vollständig sind
data ParserMöglichkeiten
    = PMBefehl
        Befehl
    | PMBefehlSofort
        BefehlSofort
        [EingabeTokenAllgemein]
    | PMStatusAnfrage
        StatusAnfrage
        [EingabeTokenAllgemein]

-- | Ist eine 'Anfrage' das erste mal zu sehen
data AnfrageNeu = Neu | Alt
    deriving (Eq, Show)

-- | Unvollständige Befehle
data AnfrageBefehl
    = AnfrageBefehl
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
        (Objekt -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))
    | ABStatusAnfrageMärklin
        (EingabeToken -> StatusAnfrageObjektZugtyp 'Märklin)
        (ObjektZugtyp 'Märklin -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))
    | ABStatusAnfrageLego
        (EingabeToken -> StatusAnfrageObjektZugtyp 'Lego)
        (ObjektZugtyp 'Lego -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl))

instance Show AnfrageBefehl where
    show :: AnfrageBefehl -> String
    show
        AnfrageBefehl
            = Language.befehl
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

instance MitAnfrage (Either BefehlSofort Befehl) where
    type AnfrageTyp (Either BefehlSofort Befehl) = AnfrageBefehl
    -- | Auswerten eines Zwischenergebnisses fortsetzen
    anfrageAktualisieren ::
        AnfrageBefehl ->
        EingabeToken ->
            AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
    anfrageAktualisieren
        AnfrageBefehl
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Beenden              , AFErgebnis $ Right $ UI Beenden),
                (Lexer.Hinzufügen           , AFZwischenwert $ ABHinzufügen AnfrageObjekt),
                (Lexer.Entfernen            , AFZwischenwert ABEntfernen),
                (Lexer.Speichern            , AFZwischenwert ABSpeichern),
                (Lexer.Laden                , AFZwischenwert ABLaden),
                (Lexer.Plan                 , AFZwischenwert $ ABStatusAnfrage SAOPlan planWählen),
                (Lexer.Wegstrecke           , anfrageAktualisieren (ABAktion AnfrageAktion) token),
                (Lexer.Weiche               , anfrageAktualisieren (ABAktion AnfrageAktion) token),
                (Lexer.Bahngeschwindigkeit  , anfrageAktualisieren (ABAktion AnfrageAktion) token),
                (Lexer.Streckenabschnitt    , anfrageAktualisieren (ABAktion AnfrageAktion) token),
                (Lexer.Kupplung             , anfrageAktualisieren (ABAktion AnfrageAktion) token)]
                $ AFFehler AnfrageBefehl eingabe
                    where
                        planWählen :: Objekt -> AnfrageFortsetzung AnfrageBefehl (Either BefehlSofort Befehl)
                        planWählen (OPlan plan) = AFErgebnis $ Left $ BSAusführenMöglich plan
                        planWählen  objekt      = error $
                            "planWählen erwartet einen Plan. Stattdessen \"" ++
                            show objekt ++
                            "\" erhalten."
    anfrageAktualisieren
        (ABHinzufügen anfrageObjekt)
        token
            = (AFErgebnis . Right . Hinzufügen, ABHinzufügen) $<< anfrageAktualisieren anfrageObjekt token
    anfrageAktualisieren
        ABEntfernen
        token@EingabeToken {eingabe}
            = case anfrageObjektExistierend token of
                Nothing
                    -> AFFehler ABEntfernen eingabe
                (Just anfrageKonstruktor)
                    -> AFZwischenwert $ ABStatusAnfrage anfrageKonstruktor $ AFErgebnis . Right . Entfernen
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
    anfrageAktualisieren
        ABSpeichern
        EingabeToken {eingabe}
            = AFErgebnis $ Right $ Speichern $ unpack eingabe
    anfrageAktualisieren
        ABLaden
        EingabeToken {eingabe}
            = AFErgebnis $ Left $ BSLaden $ unpack eingabe
    anfrageAktualisieren
        anfrage@(ABAktionPlan plan@Plan {plAktionen})
        token
            = wähleErgebnis anfrage token [(Lexer.Ausführen, Right $ Ausführen plan zeigeFortschritt $ pure ())]
            where
                zeigeFortschritt :: Natural -> IO ()
                zeigeFortschritt i = putStrLn $
                    showText plan <:>
                    showText (toEnum (fromIntegral i) / toEnum (length plAktionen) :: Double)
    anfrageAktualisieren
        (ABAktionPlanAusführend plan _neu)
        token
            = wähleErgebnis (ABAktionPlanAusführend plan Alt) token [
                (Lexer.AusführenAbbrechen, Right $ AusführenAbbrechen plan)]
    anfrageAktualisieren
        (ABAktionPlanGesperrt plan _neu pins)
        token@EingabeToken {eingabe}
            = wähleBefehl token [] $ AFFehler (ABAktionPlanGesperrt plan Alt pins) eingabe
    anfrageAktualisieren
        (ABAktion anfrageAktion)
        token
            = (AFErgebnis . Right . AktionBefehl, ABAktion) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren
        anfrage@(ABStatusAnfrage anfrageKonstruktor eitherF)
        token
            = _AEStatusAnfrage (anfrageKonstruktor token) eitherF anfrage []
    anfrageAktualisieren
        anfrage@(ABStatusAnfrageMärklin anfrageKonstruktor eitherF)
        token
            = _AEStatusAnfrageMärklin (anfrageKonstruktor token) eitherF anfrage []
    anfrageAktualisieren
        anfrage@(ABStatusAnfrageLego anfrageKonstruktor eitherF)
        token
            = _AEStatusAnfrageLego (anfrageKonstruktor token) eitherF anfrage []