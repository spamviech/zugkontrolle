{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.Anschluss (AnfrageAnschluss(..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import Zug.Anbindung (Anschluss(..), AnschlussKlasse(..), Pin(..), PCF8574(..), PCF8574Port(..)
                    , PCF8574Variant(..), Value(), alleValues)
import Zug.Language (Anzeige(..), Sprache(), (<->), (<^>), (<=>), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), MitAnfrage(..), AnfrageFortsetzung(..), zeigeAnfrageFehlgeschlagenStandard
      , wähleZwischenwert, wähleValue)

-- | Unvollständiger 'Anschluss'.
data AnfrageAnschluss
    = AnfrageAnschluss
    | APin
    | APCF8574Port
    | APCF8574PortVariant { aaVariante :: PCF8574Variant }
    | APCF8574PortVariantA0 { aaVariante :: PCF8574Variant, aaA0 :: Value }
    | APCF8574PortVariantA0A1 { aaVariante :: PCF8574Variant, aaA0 :: Value, aaA1 :: Value }
    | APCF8574PortVariantA0A1A2
          { aaVariante :: PCF8574Variant
          , aaA0 :: Value
          , aaA1 :: Value
          , aaA2 :: Value
          }
    deriving (Show, Eq)

instance Anzeige AnfrageAnschluss where
    anzeige :: AnfrageAnschluss -> Sprache -> Text
    anzeige AnfrageAnschluss = Language.anschluss
    anzeige APin = Language.anschluss <-> Language.pin
    anzeige APCF8574Port = Language.anschluss <-> Language.pcf8574Port
    anzeige (APCF8574PortVariant variante) =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> if variante == VariantNormal
            then Language.normal
            else Language.a
    anzeige (APCF8574PortVariantA0 variante a0) =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> (if variante == VariantNormal
                 then Language.normal
                 else Language.a)
        <^> Language.a0 <=> a0
    anzeige (APCF8574PortVariantA0A1 variante a0 a1) =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> (if variante == VariantNormal
                 then Language.normal
                 else Language.a)
        <^> Language.a0 <=> a0 <^> Language.a1 <=> a1
    anzeige (APCF8574PortVariantA0A1A2 variante a0 a1 a2) =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> (if variante == VariantNormal
                 then Language.normal
                 else Language.a)
        <^> Language.a0 <=> a0 <^> Language.a1 <=> a1 <^> Language.a2 <=> a2

instance Anfrage AnfrageAnschluss where
    zeigeAnfrage :: AnfrageAnschluss -> Sprache -> Text
    zeigeAnfrage AnfrageAnschluss = Language.anschluss
    zeigeAnfrage APin = Language.pin
    zeigeAnfrage APCF8574Port = Language.pcf8574 <-> Language.variante
    zeigeAnfrage (APCF8574PortVariant _variante) = Language.a0
    zeigeAnfrage (APCF8574PortVariantA0 _variante _a0) = Language.a1
    zeigeAnfrage (APCF8574PortVariantA0A1 _variante _a0 _a1) = Language.a2
    zeigeAnfrage (APCF8574PortVariantA0A1A2 _variante _a0 _a1 _a2) = Language.port

    zeigeAnfrageFehlgeschlagen :: AnfrageAnschluss -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@APin eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage@(APCF8574PortVariant _variante) eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen anfrage@(APCF8574PortVariantA0 _variante _a0) eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen anfrage@(APCF8574PortVariantA0A1 _variante _a0 _a2) eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen anfrage@(APCF8574PortVariantA0A1A2 _variante _a0 _a1 _a2) eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageAnschluss -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageAnschluss = Just $ toBefehlsString . \sprache
        -> map ($ sprache) [Language.pin, Language.pcf8574Port]
    zeigeAnfrageOptionen APCF8574Port = Just $ toBefehlsString . \sprache
        -> map ($ sprache) [Language.normal, Language.a]
    zeigeAnfrageOptionen (APCF8574PortVariant _variante) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen (APCF8574PortVariantA0 _variante _a0) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen (APCF8574PortVariantA0A1 _variante _a0 _a1) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage Anschluss where
    type AnfrageTyp Anschluss = AnfrageAnschluss

    -- | Eingabe eines 'Anschluss'
    anfrageAktualisieren
        :: AnfrageAnschluss -> EingabeToken -> AnfrageFortsetzung AnfrageAnschluss Anschluss
    anfrageAktualisieren AnfrageAnschluss token =
        wähleZwischenwert token [(Lexer.Pin, APin), (Lexer.PCF8574Port, APCF8574Port)]
    anfrageAktualisieren APin EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis $ zuAnschluss $ Gpio $ fromIntegral pin
        Nothing -> AFFehler eingabe
    anfrageAktualisieren APCF8574Port token =
        wähleZwischenwert
            token
            [ (Lexer.A, APCF8574PortVariant VariantA)
            , (Lexer.Normal, APCF8574PortVariant VariantNormal)]
    anfrageAktualisieren (APCF8574PortVariant variante) token@EingabeToken {eingabe} =
        case wähleValue token of
            (Just a0) -> AFZwischenwert $ APCF8574PortVariantA0 variante a0
            Nothing -> AFFehler eingabe
    anfrageAktualisieren (APCF8574PortVariantA0 variante a0) token@EingabeToken {eingabe} =
        case wähleValue token of
            (Just a1) -> AFZwischenwert $ APCF8574PortVariantA0A1 variante a0 a1
            Nothing -> AFFehler eingabe
    anfrageAktualisieren (APCF8574PortVariantA0A1 variante a0 a1) token@EingabeToken {eingabe} =
        case wähleValue token of
            (Just a2) -> AFZwischenwert $ APCF8574PortVariantA0A1A2 variante a0 a1 a2
            Nothing -> AFFehler eingabe
    anfrageAktualisieren
        (APCF8574PortVariantA0A1A2 variant a0 a1 a2)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just port) -> AFErgebnis
            $ AnschlussPCF8574Port
            $ PCF8574Port
            { pcf8574 = PCF8574 { variant, a0, a1, a2, interruptPin = Nothing }
            , port = fromIntegral port
            }
        Nothing -> AFFehler eingabe