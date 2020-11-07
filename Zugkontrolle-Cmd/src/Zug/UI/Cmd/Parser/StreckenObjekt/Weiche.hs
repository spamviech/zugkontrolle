{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.StreckenObjekt.Weiche (AnfrageWeiche(..)) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Numeric.Natural (Natural)

import Zug.Anbindung (Value(..), alleValues, AnschlussEither()
                    , InterruptPinBenötigt(InterruptPinEgal), Pin(Gpio), Weiche(..))
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
    AWeicheMärklin :: AnfrageWeiche 'AnfrageMärklin
    AWeicheMärklinName :: { awemName :: Text } -> AnfrageWeiche 'AnfrageMärklin
    AWeicheMärklinNameFließend :: { awemName :: Text, awemFließend :: Value }
        -> AnfrageWeiche 'AnfrageMärklin
    AWeicheMärklinNameFließendAnzahl
        :: { awemName :: Text
           , awemFließend :: Value
           , awemAnzahl :: Natural
           , awemRichtungsAnschlüsse :: [(Richtung, AnschlussEither)]
           } -> AnfrageWeiche 'AnfrageMärklin
    AWeicheMärklinNameFließendAnzahlRichtung
        :: { awemName :: Text
           , awemFließend :: Value
           , awemAnzahl :: Natural
           , awemRichtungsAnschlüsse :: [(Richtung, AnschlussEither)]
           , awemRichtung :: Richtung
           , awemAnfrageAnschluss :: AnfrageAnschluss 'InterruptPinEgal
           } -> AnfrageWeiche 'AnfrageMärklin
    AWeicheLego :: AnfrageWeiche 'AnfrageLego
    AWeicheLegoName :: { awelName :: Text } -> AnfrageWeiche 'AnfrageLego
    AWeicheLegoNameFließend :: { awelName :: Text, awelFließend :: Value }
        -> AnfrageWeiche 'AnfrageLego
    AWeicheLegoNameFließendRichtung1
        :: { awelName :: Text, awelFließend :: Value, awelRichtung1 :: Richtung }
        -> AnfrageWeiche 'AnfrageLego
    AWeicheLegoNameFließendRichtungen
        :: { awelName :: Text
           , awelFließend :: Value
           , awelRichtung1 :: Richtung
           , awelRichtung2 :: Richtung
           } -> AnfrageWeiche 'AnfrageLego

deriving instance Eq (AnfrageWeiche z)

deriving instance Show (AnfrageWeiche z)

instance Anzeige (AnfrageWeiche z) where
    anzeige :: AnfrageWeiche z -> Sprache -> Text
    anzeige AnfrageWeiche = Language.weiche
    anzeige AWeicheMärklin = Language.märklin <-> Language.weiche
    anzeige (AWeicheMärklinName name) =
        Language.lego <-> Language.weiche <^> Language.name <=> name
    anzeige (AWeicheMärklinNameFließend name fließend) =
        Language.lego
        <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> fließend
    anzeige (AWeicheMärklinNameFließendAnzahl name fließend anzahl acc) =
        Language.lego
        <-> Language.weiche
        <^> Language.name
        <=> name
        <^> Language.fließend
        <=> fließend <^> (Language.erwartet $# Language.richtungen) <=> anzahl <^> acc
    anzeige
        (AWeicheMärklinNameFließendAnzahlRichtung name fließend anzahl acc richtung anschluss) =
        Language.lego
        <-> Language.weiche
        <^> Language.name
        <=> name
        <^> Language.fließend
        <=> fließend
        <^> (Language.erwartet $# Language.richtungen)
        <=> anzahl <^> acc <^> Language.richtung <=> richtung <^> Language.anschluss <=> anschluss
    anzeige AWeicheLego = Language.lego <-> Language.weiche
    anzeige (AWeicheLegoName name) = Language.lego <-> Language.weiche <^> Language.name <=> name
    anzeige (AWeicheLegoNameFließend name fließend) =
        Language.lego
        <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> fließend
    anzeige (AWeicheLegoNameFließendRichtung1 name fließend richtung1) =
        Language.lego
        <-> Language.weiche
        <^> Language.name <=> name <^> Language.fließend <=> fließend <^> richtung1
    anzeige (AWeicheLegoNameFließendRichtungen name fließend richtung1 richtung2) =
        Language.lego
        <-> Language.weiche
        <^> Language.name <=> name <^> Language.fließend <=> fließend <^> richtung1 <^> richtung2

instance Anfrage (AnfrageWeiche z) where
    zeigeAnfrage :: AnfrageWeiche z -> Sprache -> Text
    zeigeAnfrage AnfrageWeiche = Language.zugtyp
    zeigeAnfrage AWeicheMärklin = Language.name
    zeigeAnfrage (AWeicheMärklinName _name) = Language.fließendValue
    zeigeAnfrage (AWeicheMärklinNameFließend _name _fließend) =
        Language.anzahl $# Language.richtungen
    zeigeAnfrage (AWeicheMärklinNameFließendAnzahl _name _fließend _anzahl _acc) =
        Language.richtung
    zeigeAnfrage AWeicheMärklinNameFließendAnzahlRichtung {awemAnfrageAnschluss} =
        zeigeAnfrage awemAnfrageAnschluss
    zeigeAnfrage AWeicheLego = Language.name
    zeigeAnfrage AWeicheLegoName {} = Language.fließendValue
    zeigeAnfrage AWeicheLegoNameFließend {} = Language.richtung
    zeigeAnfrage AWeicheLegoNameFließendRichtung1 {} = Language.richtung
    zeigeAnfrage AWeicheLegoNameFließendRichtungen {} = Language.pin

    zeigeAnfrageFehlgeschlagen :: AnfrageWeiche z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@(AWeicheMärklinNameFließend _name _fließend) eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        AWeicheMärklinNameFließendAnzahlRichtung {awemAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen awemAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage@AWeicheLegoNameFließendRichtungen {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageWeiche z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageWeiche = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList unterstützteZugtypen
    zeigeAnfrageOptionen (AWeicheMärklinName _name) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen (AWeicheMärklinNameFließendAnzahl _name _fließend _anzahl _acc) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NonEmpty.toList unterstützteRichtungen
    zeigeAnfrageOptionen AWeicheMärklinNameFließendAnzahlRichtung {awemAnfrageAnschluss} =
        zeigeAnfrageOptionen awemAnfrageAnschluss
    zeigeAnfrageOptionen (AWeicheLegoName _name) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen (AWeicheLegoNameFließend _name _fließend) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NonEmpty.toList unterstützteRichtungen
    zeigeAnfrageOptionen (AWeicheLegoNameFließendRichtung1 _name _fließend _richtung1) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NonEmpty.toList unterstützteRichtungen
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage (Weiche 'Märklin) where
    type AnfrageTyp (Weiche 'Märklin) = AnfrageWeiche 'AnfrageMärklin

    -- | Eingabe einer 'Märklin'-'Weiche'
    anfrageAktualisieren :: AnfrageTyp (Weiche 'Märklin)
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageMärklin) (Weiche 'Märklin)
    anfrageAktualisieren AWeicheMärklin EingabeToken {eingabe} =
        AFZwischenwert $ AWeicheMärklinName eingabe
    anfrageAktualisieren (AWeicheMärklinName name) token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, AWeicheMärklinNameFließend name HIGH)
            , (Lexer.LOW, AWeicheMärklinNameFließend name LOW)]
    anfrageAktualisieren
        (AWeicheMärklinNameFließend name fließend)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just 0) -> AFFehler eingabe
        (Just anzahl)
            -> AFZwischenwert $ AWeicheMärklinNameFließendAnzahl name fließend anzahl []
    anfrageAktualisieren
        (AWeicheMärklinNameFließendAnzahl name fließend anzahl acc)
        token@EingabeToken {eingabe} = case wähleRichtung token of
        Nothing -> AFFehler eingabe
        (Just richtung) -> AFZwischenwert
            $ AWeicheMärklinNameFließendAnzahlRichtung
                name
                fließend
                anzahl
                acc
                richtung
                AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(AWeicheMärklinNameFließendAnzahlRichtung
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
            anfrageAnschlussVerwenden
                :: AnfrageAnschluss 'InterruptPinEgal -> AnfrageWeiche 'AnfrageMärklin
            anfrageAnschlussVerwenden awemAnfrageAnschluss = anfrage { awemAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageMärklin) (Weiche 'Märklin)
            anschlussVerwenden anschluss
                | anzahl > 1 =
                    AFZwischenwert
                    $ AWeicheMärklinNameFließendAnzahl
                        wemName
                        wemFließend
                        (pred anzahl)
                        ((richtung, anschluss) : acc)
                | otherwise =
                    AFErgebnis
                        WeicheMärklin
                        { wemName
                        , wemFließend
                        , wemRichtungsAnschlüsse = (richtung, anschluss) :| acc
                        }

instance MitAnfrage (Weiche 'Lego) where
    type AnfrageTyp (Weiche 'Lego) = AnfrageWeiche 'AnfrageLego

    -- | Eingabe einer 'Lego'-'Weiche'
    anfrageAktualisieren :: AnfrageTyp (Weiche 'Lego)
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageLego) (Weiche 'Lego)
    anfrageAktualisieren AWeicheLego EingabeToken {eingabe} =
        AFZwischenwert $ AWeicheLegoName eingabe
    anfrageAktualisieren (AWeicheLegoName name) token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, AWeicheLegoNameFließend name HIGH)
            , (Lexer.LOW, AWeicheLegoNameFließend name LOW)]
    anfrageAktualisieren (AWeicheLegoNameFließend name fließend) token@EingabeToken {eingabe} =
        case wähleRichtung token of
            Nothing -> AFFehler eingabe
            (Just richtung1)
                -> AFZwischenwert $ AWeicheLegoNameFließendRichtung1 name fließend richtung1
    anfrageAktualisieren
        (AWeicheLegoNameFließendRichtung1 name fließend richtung1)
        token@EingabeToken {eingabe} = case wähleRichtung token of
        Nothing -> AFFehler eingabe
        (Just richtung2) -> AFZwischenwert
            $ AWeicheLegoNameFließendRichtungen name fließend richtung1 richtung2
    anfrageAktualisieren
        (AWeicheLegoNameFließendRichtungen welName welFließend richtung1 richtung2)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis
            $ WeicheLego
            { welName
            , welFließend
            , welRichtungsPin = Gpio $ fromIntegral pin
            , welRichtungen = (richtung1, richtung2)
            }
        Nothing -> AFFehler eingabe

instance MitAnfrageZugtyp AnfrageWeiche where
    anfrageMärklin :: AnfrageWeiche 'AnfrageMärklin
    anfrageMärklin = AWeicheMärklin

    anfrageLego :: AnfrageWeiche 'AnfrageLego
    anfrageLego = AWeicheLego
