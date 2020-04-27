{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.StreckenObjekt.Weiche (AnfrageWeiche(..)) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Numeric.Natural (Natural)

import Zug.Anbindung (Value(..), alleValues, Anschluss(), Pin(Gpio), Weiche(..))
import Zug.Enums (Zugtyp(..), unterstützteZugtypen, Richtung(), unterstützteRichtungen)
import Zug.Language (Anzeige(..), Sprache(), (<->), (<^>), (<=>), ($#), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), MitAnfrage(..), zeigeAnfrageFehlgeschlagenStandard, AnfrageZugtyp(..)
      , MitAnfrageZugtyp(..), AnfrageFortsetzung(..), wähleZwischenwert, ($<<), wähleRichtung)
import Zug.UI.Cmd.Parser.Anschluss (AnfrageAnschluss(AnfrageAnschluss))

-- | Unvollständige 'Weiche'.
data AnfrageWeiche (z :: AnfrageZugtyp) where
    AnfrageWeiche :: AnfrageWeiche 'AnfrageZugtyp
    AMärklinWeiche :: AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheName :: { awemName :: Text } -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließend :: { awemName :: Text, awemFließend :: Value }
        -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließendAnzahl
        :: { awemName :: Text
           , awemFließend :: Value
           , awemAnzahl :: Natural
           , awemRichtungsAnschlüsse :: [(Richtung, Anschluss)]
           } -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließendAnzahlRichtung
        :: { awemName :: Text
           , awemFließend :: Value
           , awemAnzahl :: Natural
           , awemRichtungsAnschlüsse :: [(Richtung, Anschluss)]
           , awemRichtung :: Richtung
           , awemAnfrageAnschluss :: AnfrageAnschluss
           } -> AnfrageWeiche 'AnfrageZugtypMärklin
    ALegoWeiche :: AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheName :: { awelName :: Text } -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließend :: { awelName :: Text, awelFließend :: Value }
        -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließendRichtung1
        :: { awelName :: Text, awelFließend :: Value, awelRichtung1 :: Richtung }
        -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließendRichtungen
        :: { awelName :: Text
           , awelFließend :: Value
           , awelRichtung1 :: Richtung
           , awelRichtung2 :: Richtung
           } -> AnfrageWeiche 'AnfrageZugtypLego

deriving instance Eq (AnfrageWeiche z)

deriving instance Show (AnfrageWeiche z)

instance Anzeige (AnfrageWeiche z) where
    anzeige :: AnfrageWeiche z -> Sprache -> Text
    anzeige AnfrageWeiche = Language.weiche
    anzeige AMärklinWeiche = Language.märklin <-> Language.weiche
    anzeige (AMärklinWeicheName name) =
        Language.lego <-> Language.weiche <^> Language.name <=> name
    anzeige (AMärklinWeicheNameFließend name fließend) =
        Language.lego
        <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> fließend
    anzeige (AMärklinWeicheNameFließendAnzahl name fließend anzahl acc) =
        Language.lego
        <-> Language.weiche
        <^> Language.name
        <=> name
        <^> Language.fließend
        <=> fließend <^> (Language.erwartet $# Language.richtungen) <=> anzahl <^> acc
    anzeige
        (AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung anschluss) =
        Language.lego
        <-> Language.weiche
        <^> Language.name
        <=> name
        <^> Language.fließend
        <=> fließend
        <^> (Language.erwartet $# Language.richtungen)
        <=> anzahl <^> acc <^> Language.richtung <=> richtung <^> Language.anschluss <=> anschluss
    anzeige ALegoWeiche = Language.lego <-> Language.weiche
    anzeige (ALegoWeicheName name) = Language.lego <-> Language.weiche <^> Language.name <=> name
    anzeige (ALegoWeicheNameFließend name fließend) =
        Language.lego
        <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> fließend
    anzeige (ALegoWeicheNameFließendRichtung1 name fließend richtung1) =
        Language.lego
        <-> Language.weiche
        <^> Language.name <=> name <^> Language.fließend <=> fließend <^> richtung1
    anzeige (ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2) =
        Language.lego
        <-> Language.weiche
        <^> Language.name <=> name <^> Language.fließend <=> fließend <^> richtung1 <^> richtung2

instance Anfrage (AnfrageWeiche z) where
    zeigeAnfrage :: AnfrageWeiche z -> Sprache -> Text
    zeigeAnfrage AnfrageWeiche = Language.zugtyp
    zeigeAnfrage AMärklinWeiche = Language.name
    zeigeAnfrage (AMärklinWeicheName _name) = Language.fließendValue
    zeigeAnfrage (AMärklinWeicheNameFließend _name _fließend) =
        Language.anzahl $# Language.richtungen
    zeigeAnfrage (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc) =
        Language.richtung
    zeigeAnfrage AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss} =
        zeigeAnfrage awemAnfrageAnschluss
    zeigeAnfrage ALegoWeiche = Language.name
    zeigeAnfrage ALegoWeicheName {} = Language.fließendValue
    zeigeAnfrage ALegoWeicheNameFließend {} = Language.richtung
    zeigeAnfrage ALegoWeicheNameFließendRichtung1 {} = Language.richtung
    zeigeAnfrage ALegoWeicheNameFließendRichtungen {} = Language.pin

    zeigeAnfrageFehlgeschlagen :: AnfrageWeiche z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@(AMärklinWeicheNameFließend _name _fließend) eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen awemAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage@ALegoWeicheNameFließendRichtungen {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageWeiche z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageWeiche = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList unterstützteZugtypen
    zeigeAnfrageOptionen (AMärklinWeicheName _name) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NonEmpty.toList unterstützteRichtungen
    zeigeAnfrageOptionen AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss} =
        zeigeAnfrageOptionen awemAnfrageAnschluss
    zeigeAnfrageOptionen (ALegoWeicheName _name) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen (ALegoWeicheNameFließend _name _fließend) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NonEmpty.toList unterstützteRichtungen
    zeigeAnfrageOptionen (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NonEmpty.toList unterstützteRichtungen
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage (Weiche 'Märklin) where
    type AnfrageTyp (Weiche 'Märklin) = AnfrageWeiche 'AnfrageZugtypMärklin

    -- | Eingabe einer 'Märklin'-'Weiche'
    anfrageAktualisieren
        :: AnfrageTyp (Weiche 'Märklin)
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageZugtypMärklin) (Weiche 'Märklin)
    anfrageAktualisieren AMärklinWeiche EingabeToken {eingabe} =
        AFZwischenwert $ AMärklinWeicheName eingabe
    anfrageAktualisieren (AMärklinWeicheName name) token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, AMärklinWeicheNameFließend name HIGH)
            , (Lexer.LOW, AMärklinWeicheNameFließend name LOW)]
    anfrageAktualisieren
        (AMärklinWeicheNameFließend name fließend)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just 0) -> AFFehler eingabe
        (Just anzahl)
            -> AFZwischenwert $ AMärklinWeicheNameFließendAnzahl name fließend anzahl []
    anfrageAktualisieren
        (AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)
        token@EingabeToken {eingabe} = case wähleRichtung token of
        Nothing -> AFFehler eingabe
        (Just richtung) -> AFZwischenwert
            $ AMärklinWeicheNameFließendAnzahlRichtung
                name
                fließend
                anzahl
                acc
                richtung
                AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(AMärklinWeicheNameFließendAnzahlRichtung
                     wemName
                     wemFließend
                     anzahl
                     acc
                     richtung
                     anfrageAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren anfrageAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss -> AnfrageWeiche 'AnfrageZugtypMärklin
            anfrageAnschlussVerwenden awemAnfrageAnschluss = anfrage { awemAnfrageAnschluss }

            anschlussVerwenden
                :: Anschluss
                -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageZugtypMärklin) (Weiche 'Märklin)
            anschlussVerwenden anschluss
                | anzahl > 1 =
                    AFZwischenwert
                    $ AMärklinWeicheNameFließendAnzahl
                        wemName
                        wemFließend
                        (pred anzahl)
                        ((richtung, anschluss) : acc)
                | otherwise =
                    AFErgebnis
                        MärklinWeiche
                        { wemName
                        , wemFließend
                        , wemRichtungsAnschlüsse = (richtung, anschluss) :| acc
                        }

instance MitAnfrage (Weiche 'Lego) where
    type AnfrageTyp (Weiche 'Lego) = AnfrageWeiche 'AnfrageZugtypLego

    -- | Eingabe einer 'Lego'-'Weiche'
    anfrageAktualisieren :: AnfrageTyp (Weiche 'Lego)
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageZugtypLego) (Weiche 'Lego)
    anfrageAktualisieren ALegoWeiche EingabeToken {eingabe} =
        AFZwischenwert $ ALegoWeicheName eingabe
    anfrageAktualisieren (ALegoWeicheName name) token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, ALegoWeicheNameFließend name HIGH)
            , (Lexer.LOW, ALegoWeicheNameFließend name LOW)]
    anfrageAktualisieren (ALegoWeicheNameFließend name fließend) token@EingabeToken {eingabe} =
        case wähleRichtung token of
            Nothing -> AFFehler eingabe
            (Just richtung1)
                -> AFZwischenwert $ ALegoWeicheNameFließendRichtung1 name fließend richtung1
    anfrageAktualisieren
        (ALegoWeicheNameFließendRichtung1 name fließend richtung1)
        token@EingabeToken {eingabe} = case wähleRichtung token of
        Nothing -> AFFehler eingabe
        (Just richtung2) -> AFZwischenwert
            $ ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2
    anfrageAktualisieren
        (ALegoWeicheNameFließendRichtungen welName welFließend richtung1 richtung2)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis
            $ LegoWeiche
            { welName
            , welFließend
            , welRichtungsPin = Gpio $ fromIntegral pin
            , welRichtungen = (richtung1, richtung2)
            }
        Nothing -> AFFehler eingabe

instance MitAnfrageZugtyp AnfrageWeiche where
    anfrageMärklin :: AnfrageWeiche 'AnfrageZugtypMärklin
    anfrageMärklin = AMärklinWeiche

    anfrageLego :: AnfrageWeiche 'AnfrageZugtypLego
    anfrageLego = ALegoWeiche
