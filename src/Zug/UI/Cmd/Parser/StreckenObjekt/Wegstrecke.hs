{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.StreckenObjekt.Wegstrecke (AnfrageWegstrecke(..)) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)

import Zug.Anbindung (Wegstrecke(..), Weiche(), WeicheKlasse(hatRichtung))
import Zug.Enums
       (Zugtyp(..), ZugtypKlasse(), unterstützteZugtypen, Richtung(), unterstützteRichtungen)
import Zug.Language (Anzeige(..), Sprache(Deutsch), (<^>), (<=>), ($#), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..), leeresToken)
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), MitAnfrage(..), AnfrageZugtyp(..), FixerZugtyp, MitAnfrageZugtyp(..)
      , AnfrageZugtypKlasse(..), StatusAnfrageObjektZugtyp(..), ObjektZugtyp(..)
      , AnfrageFortsetzung(..), wähleBefehl, wähleRichtung)

-- | Unvollständige 'Wegstrecke'.
data AnfrageWegstrecke (z :: AnfrageZugtyp) where
    AnfrageWegstreckeZugtyp :: AnfrageWegstrecke 'AnfrageZugtyp
    AnfrageWegstrecke :: AnfrageWegstrecke z
    AWegstreckeName :: { awsName :: Text } -> AnfrageWegstrecke z
    AWegstreckeNameAnzahl :: { awsAkkumulator :: Wegstrecke (FixerZugtyp z), awsAnzahl :: Natural }
        -> AnfrageWegstrecke z
    AWegstreckeNameAnzahlWeicheRichtung
        :: { awsAkkumulator :: Wegstrecke (FixerZugtyp z)
           , awsAnzahl :: Natural
           , awsWeiche :: Weiche (FixerZugtyp z)
           } -> AnfrageWegstrecke z
    AWSStatusAnfrage
        :: { awsStatusAnfrageKonstruktor
                 :: EingabeToken -> StatusAnfrageObjektZugtyp (FixerZugtyp z)
           , awsEitherKonstruktor :: Either (ObjektZugtyp (FixerZugtyp z)
                                             -> AnfrageWegstrecke z) (ObjektZugtyp (FixerZugtyp z)
                                                                      -> Wegstrecke (FixerZugtyp z))
           } -> AnfrageWegstrecke z

instance Show (AnfrageWegstrecke z) where
    show :: AnfrageWegstrecke z -> String
    show = Text.unpack . flip anzeige Deutsch

instance Anzeige (AnfrageWegstrecke z) where
    anzeige :: AnfrageWegstrecke z -> Sprache -> Text
    anzeige AnfrageWegstreckeZugtyp = Language.wegstrecke
    anzeige AnfrageWegstrecke = Language.wegstrecke
    anzeige (AWegstreckeName name) = Language.wegstrecke <^> Language.name <=> name
    anzeige (AWegstreckeNameAnzahl acc anzahl) =
        Language.wegstrecke
        <^> acc
        <^> (Language.anzahl $# Language.wegstreckenElemente) <=> anzahl
    anzeige (AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche) =
        Language.wegstrecke
        <^> acc
        <^> (Language.anzahl $# Language.wegstreckenElemente) <=> anzahl <^> weiche
    anzeige AWSStatusAnfrage {awsStatusAnfrageKonstruktor} =
        Language.wegstreckenElement <^> awsStatusAnfrageKonstruktor leeresToken

instance Anfrage (AnfrageWegstrecke z) where
    zeigeAnfrage :: AnfrageWegstrecke z -> Sprache -> Text
    zeigeAnfrage AnfrageWegstreckeZugtyp = Language.zugtyp
    zeigeAnfrage AnfrageWegstrecke = Language.name
    zeigeAnfrage AWegstreckeName {} = Language.anzahl $# Language.wegstreckenElemente
    zeigeAnfrage AWegstreckeNameAnzahl {} = Language.wegstreckenElement
    zeigeAnfrage AWegstreckeNameAnzahlWeicheRichtung {} = Language.richtung
    zeigeAnfrage AWSStatusAnfrage {awsStatusAnfrageKonstruktor} =
        zeigeAnfrage $ awsStatusAnfrageKonstruktor leeresToken

    zeigeAnfrageOptionen :: AnfrageWegstrecke z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageWegstreckeZugtyp = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList unterstützteZugtypen
    zeigeAnfrageOptionen
        AWegstreckeNameAnzahl {} = Just $ toBefehlsString . Language.befehlWegstreckenElemente
    zeigeAnfrageOptionen AWegstreckeNameAnzahlWeicheRichtung {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList unterstützteRichtungen
    zeigeAnfrageOptionen AWSStatusAnfrage {awsStatusAnfrageKonstruktor} =
        zeigeAnfrageOptionen $ awsStatusAnfrageKonstruktor leeresToken
    zeigeAnfrageOptionen _anfrage = Nothing

-- | Bekannte Teil-Typen einer 'Wegstrecke'
data AnfrageWegstreckenElement
    = AWSEUnbekannt Text
    | AWSEBahngeschwindigkeit
    | AWSEStreckenabschnitt
    | AWSEWeiche
    | AWSEKupplung
    | AWSEKontakt

instance MitAnfrage (Wegstrecke 'Märklin) where
    type AnfrageTyp (Wegstrecke 'Märklin) = AnfrageWegstrecke 'AnfrageMärklin

    anfrageAktualisieren
        :: AnfrageWegstrecke 'AnfrageMärklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageWegstrecke 'AnfrageMärklin) (Wegstrecke 'Märklin)
    anfrageAktualisieren = anfrageWegstreckeAktualisieren

instance MitAnfrage (Wegstrecke 'Lego) where
    type AnfrageTyp (Wegstrecke 'Lego) = AnfrageWegstrecke 'AnfrageLego

    anfrageAktualisieren :: AnfrageWegstrecke 'AnfrageLego
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageWegstrecke 'AnfrageLego) (Wegstrecke 'Lego)
    anfrageAktualisieren = anfrageWegstreckeAktualisieren

-- | Eingabe einer Wegstrecke
anfrageWegstreckeAktualisieren
    :: forall z.
    (AnfrageZugtypKlasse z, ZugtypKlasse (FixerZugtyp z))
    => AnfrageWegstrecke z
    -> EingabeToken
    -> AnfrageFortsetzung (AnfrageWegstrecke z) (Wegstrecke (FixerZugtyp z))
anfrageWegstreckeAktualisieren anfrage@AnfrageWegstreckeZugtyp _token = AFZwischenwert anfrage
anfrageWegstreckeAktualisieren AnfrageWegstrecke EingabeToken {eingabe} =
    AFZwischenwert $ AWegstreckeName eingabe
anfrageWegstreckeAktualisieren (AWegstreckeName wsName) EingabeToken {eingabe, ganzzahl} =
    case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just 0) -> AFFehler eingabe
        (Just anzahl) -> AFZwischenwert
            $ AWegstreckeNameAnzahl
                Wegstrecke
                { wsName
                , wsBahngeschwindigkeiten = Set.empty
                , wsStreckenabschnitte = Set.empty
                , wsWeichenRichtungen = Set.empty
                , wsKupplungen = Set.empty
                , wsKontakte = Set.empty
                }
                anzahl
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeNameAnzahl acc anzahl)
    token@EingabeToken {eingabe} = case anfrageWegstreckenElement of
    (AWSEUnbekannt eingabeFehler) -> AFFehler eingabeFehler
    AWSEBahngeschwindigkeit
     -> AFZwischenwert $ AWSStatusAnfrage SAOZBahngeschwindigkeit $ eitherObjektAnhängen acc
    AWSEStreckenabschnitt
     -> AFZwischenwert $ AWSStatusAnfrage SAOZStreckenabschnitt $ eitherObjektAnhängen acc
    AWSEWeiche -> AFZwischenwert $ AWSStatusAnfrage SAOZWeiche $ Left anfrageWeicheAnhängen
    AWSEKupplung -> AFZwischenwert $ AWSStatusAnfrage SAOZKupplung $ eitherObjektAnhängen acc
    AWSEKontakt -> AFZwischenwert $ AWSStatusAnfrage SAOZKontakt $ eitherObjektAnhängen acc
    where
        anfrageWegstreckenElement :: AnfrageWegstreckenElement
        anfrageWegstreckenElement =
            wähleBefehl
                token
                [ (Lexer.Weiche, AWSEWeiche)
                , (Lexer.Bahngeschwindigkeit, AWSEBahngeschwindigkeit)
                , (Lexer.Streckenabschnitt, AWSEStreckenabschnitt)
                , (Lexer.Kupplung, AWSEKupplung)]
            $ AWSEUnbekannt eingabe

        eitherObjektAnhängen :: Wegstrecke (FixerZugtyp z)
                              -> Either (ObjektZugtyp (FixerZugtyp z)
                                         -> AnfrageWegstrecke z) (ObjektZugtyp (FixerZugtyp z)
                                                                  -> Wegstrecke (FixerZugtyp z))
        eitherObjektAnhängen wegstrecke
            | anzahl > 1 = Left $ anfrageObjektAnhängen wegstrecke
            | otherwise = Right $ objektAnhängen wegstrecke

        objektAnhängen :: Wegstrecke az -> ObjektZugtyp az -> Wegstrecke az
        objektAnhängen
            wegstrecke@Wegstrecke {wsBahngeschwindigkeiten}
            (OZBahngeschwindigkeit bahngeschwindigkeit) =
            wegstrecke
            { wsBahngeschwindigkeiten = Set.insert bahngeschwindigkeit wsBahngeschwindigkeiten
            }
        objektAnhängen
            wegstrecke@Wegstrecke {wsStreckenabschnitte}
            (OZStreckenabschnitt streckenabschnitt) =
            wegstrecke { wsStreckenabschnitte = Set.insert streckenabschnitt wsStreckenabschnitte }
        objektAnhängen wegstrecke@Wegstrecke {wsKupplungen} (OZKupplung kupplung) =
            wegstrecke { wsKupplungen = Set.insert kupplung wsKupplungen }
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        objektAnhängen wegstrecke objekt =
            error
            $ "Unbekanntes Objekt zum anhängen an Wegstrecke ("
            ++ show wegstrecke
            ++ ") erhalten: "
            ++ show objekt

        anfrageObjektAnhängen
            :: Wegstrecke (FixerZugtyp z) -> ObjektZugtyp (FixerZugtyp z) -> AnfrageWegstrecke z
        anfrageObjektAnhängen wegstrecke objekt =
            AWegstreckeNameAnzahl (objektAnhängen wegstrecke objekt) $ pred anzahl

        anfrageWeicheAnhängen :: ObjektZugtyp (FixerZugtyp z) -> AnfrageWegstrecke z
        anfrageWeicheAnhängen (OZWeiche weiche) =
            AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche
        anfrageWeicheAnhängen objekt =
            error
            $ "Unbekanntes Objekt zum anhängen einer Weiche an AnfrageWegstrecke ("
            ++ show anfrage
            ++ ") erhalten: "
            ++ show objekt
anfrageWegstreckeAktualisieren
    (AWSStatusAnfrage anfrageKonstruktor (Left zwischenwertKonstruktor))
    token = afStatusAnfrage (anfrageKonstruktor token) $ AFZwischenwert . zwischenwertKonstruktor
anfrageWegstreckeAktualisieren (AWSStatusAnfrage anfrageKonstruktor (Right konstruktor)) token =
    afStatusAnfrage (anfrageKonstruktor token) $ AFErgebnis . konstruktor
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeNameAnzahlWeicheRichtung Wegstrecke {} anzahl weiche)
    token@EingabeToken {eingabe} = case wähleRichtung token of
    (Just richtung)
        | hatRichtung weiche richtung -> eitherWeicheRichtungAnhängen anfrage richtung
    _otherwise -> AFFehler eingabe
    where
        eitherWeicheRichtungAnhängen
            :: AnfrageWegstrecke z
            -> Richtung
            -> AnfrageFortsetzung (AnfrageWegstrecke z) (Wegstrecke (FixerZugtyp z))
        eitherWeicheRichtungAnhängen anfrageWegstrecke richtung
            | anzahl > 1 = AFZwischenwert $ qWeicheRichtungAnhängen anfrageWegstrecke richtung
            | otherwise = AFErgebnis $ weicheRichtungAnhängen anfrageWegstrecke richtung

        qWeicheRichtungAnhängen :: AnfrageWegstrecke z -> Richtung -> AnfrageWegstrecke z
        qWeicheRichtungAnhängen anfrageWegstrecke richtung =
            AWegstreckeNameAnzahl (weicheRichtungAnhängen anfrageWegstrecke richtung)
            $ pred anzahl

        weicheRichtungAnhängen :: AnfrageWegstrecke z -> Richtung -> Wegstrecke (FixerZugtyp z)
        weicheRichtungAnhängen
            AWegstreckeNameAnzahlWeicheRichtung
            {awsAkkumulator = wegstrecke@Wegstrecke {wsWeichenRichtungen}, awsWeiche}
            richtung =
            wegstrecke
            { wsWeichenRichtungen = Set.insert (awsWeiche, richtung) wsWeichenRichtungen
            }
        weicheRichtungAnhängen anfrageWegstrecke _richtung =
            error
            $ "weicheRichtungAnhängen mit unerwarteter anfrageWegstrecke aufgerufen: "
            ++ show anfrageWegstrecke

instance MitAnfrageZugtyp AnfrageWegstrecke where
    anfrageMärklin :: AnfrageWegstrecke 'AnfrageMärklin
    anfrageMärklin = AnfrageWegstrecke

    anfrageLego :: AnfrageWegstrecke 'AnfrageLego
    anfrageLego = AnfrageWegstrecke
