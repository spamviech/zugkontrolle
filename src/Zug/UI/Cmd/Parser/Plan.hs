{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

{-|
Description: Parsen von 'Plan' und 'Aktion'
-}
module Zug.UI.Cmd.Parser.Plan (
    AnfragePlan(..), anfragePlanAktualisieren,
    AnfrageAktion(..), anfrageAktionAktualisieren,
    AnfrageAktionWegstrecke(..), anfrageAktionWegstreckeAktualisieren,
    AktionWegstreckeZugtyp(..),
    AnfrageAktionBahngeschwindigkeit(..), anfrageAktionBahngeschwindigkeitAktualisieren,
    AktionBahngeschwindigkeitZugtyp(..),
    AnfrageAktionStreckenabschnitt(..), anfrageAktionStreckenabschnittAktualisieren,
    AnfrageAktionWeiche(..), anfrageAktionWeicheAktualisieren,
    AnfrageAktionKupplung(..), anfrageAktionKupplungAktualisieren) where

import Data.Foldable (Foldable(..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup())
import Data.String (IsString())
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (Wegstrecke(..), WegstreckeKlasse(..), Weiche(..), WeicheKlasse(..),
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..),
                    Streckenabschnitt(..), StreckenabschnittKlasse(..),
                    Kupplung(..), KupplungKlasse(..), Wartezeit(..))
import Zug.Klassen (Zugtyp(..), ZugtypEither(..), Richtung(..), unterstützteRichtungen, Strom(..),
                    Fahrtrichtung(..), unterstützteFahrtrichtungen)
import Zug.Language ((<^>), (<=>), (<->), (<|>), showText, toBefehlsString)
import qualified Zug.Language as Language
import Zug.Plan (ObjektAllgemein(..), Objekt, Plan(..), Aktion(..), AktionStreckenabschnitt(..),
                AktionWegstrecke(..), AktionBahngeschwindigkeit(..), AktionWeiche(..), AktionKupplung(..))
import Zug.Warteschlange (Warteschlange, leer, anhängen, zeigeLetztes, Anzeige(..))
import Zug.UI.Cmd.Lexer (EingabeToken(..), leeresToken)
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, AnfrageFamilie,
                                StatusAnfrageObjekt(..), unbekanntShowText, wähleBefehl, wähleRichtung)

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

instance (Show (AnfrageFamilie (Wegstrecke 'Märklin)), Show (AnfrageFamilie (Wegstrecke 'Lego)),
    Show (AnfrageFamilie (Bahngeschwindigkeit 'Märklin)), Show (AnfrageFamilie (Bahngeschwindigkeit 'Lego)),
    Show (AnfrageFamilie Streckenabschnitt), Show (AnfrageFamilie Kupplung),
    Show (AnfrageFamilie (ZugtypEither Weiche)))
        => Show AnfragePlan where
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
anfragePlanAktualisieren :: (Show (AnfrageFamilie (ZugtypEither Weiche)))
    => AnfragePlan -> EingabeToken -> Either AnfragePlan Plan
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
                    then Left $ \objekt ->
                        APlanNameAnzahl name anzahl (anhängen (konstruktor objekt) acc) AnfrageAktion
                    else Right $ \objekt ->
                        Plan {
                            plName = name,
                            plAktionen = toList $ anhängen (konstruktor objekt) acc}
            (Left AARückgängig)
                -> Left $ APlanNameAnzahl name (succ anzahl) (löscheLetztes acc) AnfrageAktion
            (Left aAktion1)
                -> Left $ APlanNameAnzahl name anzahl acc aAktion1
            (Right aktion)
                | anzahl > 1
                    -> Left $ APlanNameAnzahl name (pred anzahl) (anhängen aktion acc) AnfrageAktion
                | otherwise
                    -> Right $ Plan {
                        plName = name,
                        plAktionen = toList $ anhängen aktion acc}
        where
            löscheLetztes :: Warteschlange a -> Warteschlange a
            löscheLetztes warteschlange = case zeigeLetztes warteschlange of
                Leer
                    -> warteschlange
                (Gefüllt _l p)
                    -> p
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

instance (Show (AnfrageFamilie (Wegstrecke 'Märklin)), Show (AnfrageFamilie (Wegstrecke 'Lego)),
    Show (AnfrageFamilie (Bahngeschwindigkeit 'Märklin)), Show (AnfrageFamilie (Bahngeschwindigkeit 'Lego)),
    Show (AnfrageFamilie Streckenabschnitt), Show (AnfrageFamilie Kupplung),
    Show (AnfrageFamilie (ZugtypEither Weiche)))
        => Show AnfrageAktion where
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
anfrageAktionAktualisieren :: (Show (AnfrageFamilie (ZugtypEither Weiche)))
    => AnfrageAktion -> EingabeToken -> Either AnfrageAktion Aktion
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
                -> AAKlassifizierung SAOWegstrecke $ Left erhalteWegstreckeAktion
                where
                    erhalteWegstreckeAktion :: Objekt -> AnfrageAktion
                    erhalteWegstreckeAktion
                        (OWegstrecke (ZugtypMärklin wegstrecke))
                            = AAWegstreckeMärklin $ AnfrageAktionWegstrecke wegstrecke
                    erhalteWegstreckeAktion
                        (OWegstrecke (ZugtypLego wegstrecke))
                            = AAWegstreckeLego $ AnfrageAktionWegstrecke wegstrecke
                    erhalteWegstreckeAktion
                        _objekt
                            = AnfrageAktion
            AAEWeiche
                -> AAKlassifizierung SAOWeiche $ Left $ \(OWeiche weiche) -> AAWeiche $ AnfrageAktionWeiche weiche
            AAEBahngeschwindigkeit
                -> AAKlassifizierung SAOBahngeschwindigkeit $ Left erhalteBahngeschwindigkeitAktion
                where
                    erhalteBahngeschwindigkeitAktion :: Objekt -> AnfrageAktion
                    erhalteBahngeschwindigkeitAktion
                        (OBahngeschwindigkeit (ZugtypMärklin bahngeschwindigkeit))
                            = AABahngeschwindigkeitMärklin $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
                    erhalteBahngeschwindigkeitAktion
                        (OBahngeschwindigkeit (ZugtypLego bahngeschwindigkeit))
                            = AABahngeschwindigkeitLego $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
                    erhalteBahngeschwindigkeitAktion
                        _objekt
                            = AnfrageAktion
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
            Nothing
                -> Left $ AAUnbekannt AAWarten eingabe
            (Just zeit)
                -> Right $ Warten $ MikroSekunden zeit
anfrageAktionAktualisieren
    (AAKlassifizierung anfrageKonstruktor eitherF)
    token
        = Left $ AAStatusAnfrage (anfrageKonstruktor token) eitherF
anfrageAktionAktualisieren
    (AAWegstreckeMärklin anfrageAktion)
    token
        = case anfrageAktionWegstreckeAktualisieren anfrageAktion token of
            (Left (AAWSUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AAWegstreckeMärklin anfrage) eingabe
            (Left qAktionWegstrecke)
                -> Left $ AAWegstreckeMärklin qAktionWegstrecke
            (Right aktionWegstrecke)
                -> Right $ AWegstreckeMärklin aktionWegstrecke
anfrageAktionAktualisieren
    (AAWegstreckeLego anfrageAktion)
    token
        = case anfrageAktionWegstreckeAktualisieren anfrageAktion token of
            (Left (AAWSUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AAWegstreckeLego anfrage) eingabe
            (Left qAktionWegstrecke)
                -> Left $ AAWegstreckeLego qAktionWegstrecke
            (Right aktionWegstrecke)
                -> Right $ AWegstreckeLego aktionWegstrecke
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
                -> Left $ AAUnbekannt (AABahngeschwindigkeitMärklin anfrage) eingabe
            (Left qAktionBahngeschwindigkeit)
                -> Left $ AABahngeschwindigkeitMärklin qAktionBahngeschwindigkeit
            (Right aktionBahngeschwindigkeit)
                -> Right $ ABahngeschwindigkeitMärklin aktionBahngeschwindigkeit
anfrageAktionAktualisieren
    (AABahngeschwindigkeitLego anfrageAktion)
    token
        = case anfrageAktionBahngeschwindigkeitAktualisieren anfrageAktion token of
            (Left (AABGUnbekannt anfrage eingabe))
                -> Left $ AAUnbekannt (AABahngeschwindigkeitLego anfrage) eingabe
            (Left qAktionBahngeschwindigkeit)
                -> Left $ AABahngeschwindigkeitLego qAktionBahngeschwindigkeit
            (Right aktionBahngeschwindigkeit)
                -> Right $ ABahngeschwindigkeitLego aktionBahngeschwindigkeit
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
    show :: AnfrageAktionWegstrecke w z -> String
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

class AktionWegstreckeZugtyp (z :: Zugtyp) where
    wähleAktionWegstrecke ::
        AnfrageAktionWegstrecke ws z -> EingabeToken
            -> Either (AnfrageAktionWegstrecke ws z) (AktionWegstrecke ws z)

instance AktionWegstreckeZugtyp 'Märklin where
    wähleAktionWegstrecke ::
        AnfrageAktionWegstrecke ws 'Märklin -> EingabeToken
            -> Either (AnfrageAktionWegstrecke ws 'Märklin) (AktionWegstrecke ws 'Märklin)
    wähleAktionWegstrecke
        anfrage@(AnfrageAktionWegstrecke wegstrecke)
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Einstellen               , Right $ Einstellen wegstrecke),
                (Lexer.Geschwindigkeit          , Left $ AAWSBahngeschwindigkeit $ AABGGeschwindigkeit wegstrecke),
                (Lexer.Umdrehen                 , Right $ AWSBahngeschwindigkeit $ Umdrehen wegstrecke),
                (Lexer.Strom                    , Left $ AAWSStreckenabschnitt $ AASTStrom wegstrecke),
                (Lexer.Kuppeln                  , Right $ AWSKupplung $ Kuppeln wegstrecke)]
                $ Left $ AAWSUnbekannt anfrage eingabe
    wähleAktionWegstrecke
        anfrage
        EingabeToken {eingabe}
            = Left $ AAWSUnbekannt anfrage eingabe

instance AktionWegstreckeZugtyp 'Lego where
    wähleAktionWegstrecke ::
        AnfrageAktionWegstrecke ws 'Lego -> EingabeToken
            -> Either (AnfrageAktionWegstrecke ws 'Lego) (AktionWegstrecke ws 'Lego)
    wähleAktionWegstrecke
        anfrage@(AnfrageAktionWegstrecke wegstrecke)
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Einstellen               , Right $ Einstellen wegstrecke),
                (Lexer.Geschwindigkeit          , Left $ AAWSBahngeschwindigkeit $ AABGGeschwindigkeit wegstrecke),
                (Lexer.FahrtrichtungEinstellen  , Left $ AAWSBahngeschwindigkeit $ AABGFahrtrichtungEinstellen wegstrecke),
                (Lexer.Strom                    , Left $ AAWSStreckenabschnitt $ AASTStrom wegstrecke),
                (Lexer.Kuppeln                  , Right $ AWSKupplung $ Kuppeln wegstrecke)]
                $ Left $ AAWSUnbekannt anfrage eingabe
    wähleAktionWegstrecke
        anfrage
        EingabeToken {eingabe}
            = Left $ AAWSUnbekannt anfrage eingabe

-- | Eingabe einer Wegstrecken-Aktion
anfrageAktionWegstreckeAktualisieren :: (AktionWegstreckeZugtyp z, WegstreckeKlasse (w z),
    BahngeschwindigkeitKlasse w, AktionBahngeschwindigkeitZugtyp z)
        => AnfrageAktionWegstrecke w z
        -> EingabeToken
            -> Either (AnfrageAktionWegstrecke w z) (AktionWegstrecke w z)
anfrageAktionWegstreckeAktualisieren
    anfrage@(AAWSUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage
anfrageAktionWegstreckeAktualisieren
    anfrage@(AnfrageAktionWegstrecke _wegstrecke)
    token
        = wähleAktionWegstrecke anfrage token
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
            => AnfrageAktionWeiche w -> Richtung -> Either (AnfrageAktionWeiche w) (AktionWeiche w)
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
        :: (b z)                                    -- ^ Bahngeschwindigkeit
            -> AnfrageAktionBahngeschwindigkeit b z
    AABGFahrtrichtungEinstellen
        :: (b 'Lego)                                -- ^ Bahngeschwindigkeit
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

class AktionBahngeschwindigkeitZugtyp (z :: Zugtyp) where
    wähleAktionBahngeschwindigkeit ::
        AnfrageAktionBahngeschwindigkeit bg z -> EingabeToken
            -> Either (AnfrageAktionBahngeschwindigkeit bg z) (AktionBahngeschwindigkeit bg z)

instance AktionBahngeschwindigkeitZugtyp 'Märklin where
    wähleAktionBahngeschwindigkeit ::
        AnfrageAktionBahngeschwindigkeit bg 'Märklin -> EingabeToken
            -> Either (AnfrageAktionBahngeschwindigkeit bg 'Märklin) (AktionBahngeschwindigkeit bg 'Märklin)
    wähleAktionBahngeschwindigkeit
        anfrage@(AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Geschwindigkeit  , Left $ AABGGeschwindigkeit bahngeschwindigkeit),
                (Lexer.Umdrehen         , Right $ Umdrehen bahngeschwindigkeit)]
                $ Left $ AABGUnbekannt anfrage eingabe
    wähleAktionBahngeschwindigkeit
        anfrage
        EingabeToken {eingabe}
            = Left $ AABGUnbekannt anfrage eingabe

instance AktionBahngeschwindigkeitZugtyp 'Lego where
    wähleAktionBahngeschwindigkeit ::
        AnfrageAktionBahngeschwindigkeit bg 'Lego -> EingabeToken
            -> Either (AnfrageAktionBahngeschwindigkeit bg 'Lego) (AktionBahngeschwindigkeit bg 'Lego)
    wähleAktionBahngeschwindigkeit
        anfrage@(AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Geschwindigkeit          , Left $ AABGGeschwindigkeit bahngeschwindigkeit),
                (Lexer.FahrtrichtungEinstellen  , Left $ AABGFahrtrichtungEinstellen bahngeschwindigkeit)]
                $ Left $ AABGUnbekannt anfrage eingabe
    wähleAktionBahngeschwindigkeit
        anfrage
        EingabeToken {eingabe}
            = Left $ AABGUnbekannt anfrage eingabe

-- | Eingabe einer Bahngeschwindigkeit-Aktion
anfrageAktionBahngeschwindigkeitAktualisieren :: (BahngeschwindigkeitKlasse b, AktionBahngeschwindigkeitZugtyp z)
    => AnfrageAktionBahngeschwindigkeit b z -> EingabeToken
        -> Either (AnfrageAktionBahngeschwindigkeit b z) (AktionBahngeschwindigkeit b z)
anfrageAktionBahngeschwindigkeitAktualisieren
    anfrage@(AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit)
    token
        = wähleAktionBahngeschwindigkeit anfrage token
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
            (Lexer.Vorwärts , Right $ FahrtrichtungEinstellen bahngeschwindigkeit Vorwärts),
            (Lexer.Rückwärts , Right $ FahrtrichtungEinstellen bahngeschwindigkeit Rückwärts)]
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
    show :: AnfrageAktionStreckenabschnitt s -> String
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
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionStreckenabschnitt st -> s
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
    show :: AnfrageAktionKupplung k -> String
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