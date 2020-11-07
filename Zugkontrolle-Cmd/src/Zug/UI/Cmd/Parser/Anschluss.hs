{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.Anschluss (AnfrageAnschluss(..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Word (Word8)

import Zug.Anbindung (Anschluss(..), AnschlussEither(..), MitInterruptPin(MitInterruptPin)
                    , InterruptPinBenötigt(..), AnschlussKlasse(..), Pin(..), PCF8574(..)
                    , PCF8574Port(..), PCF8574Variant(..), Value(), alleValues)
import Zug.Language (Anzeige(..), Sprache(), (<->), (<^>), (<=>), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), MitAnfrage(..), AnfrageFortsetzung(..), zeigeAnfrageFehlgeschlagenStandard
      , wähleZwischenwert, wähleValue)

-- | Unvollständiger 'Anschluss'.
data AnfrageAnschluss (i :: InterruptPinBenötigt) where
    AnfrageAnschluss :: AnfrageAnschluss i
    APin :: AnfrageAnschluss i
    APCF8574Port :: AnfrageAnschluss i
    APCF8574PortVariant :: { aaVariante :: PCF8574Variant } -> AnfrageAnschluss i
    APCF8574PortVariantA0 :: { aaVariante :: PCF8574Variant, aaA0 :: Value } -> AnfrageAnschluss i
    APCF8574PortVariantA0A1 :: { aaVariante :: PCF8574Variant, aaA0 :: Value, aaA1 :: Value }
        -> AnfrageAnschluss i
    APCF8574PortVariantA0A1A2
        :: { aaVariante :: PCF8574Variant, aaA0 :: Value, aaA1 :: Value, aaA2 :: Value }
        -> AnfrageAnschluss i
    APCF8574PortVariantA0A1A2Port
        :: { aaiVariante :: PCF8574Variant
           , aaiA0 :: Value
           , aaiA1 :: Value
           , aaiA2 :: Value
           , aaiPort :: Word8
           } -> AnfrageAnschluss 'InterruptPinBenötigt

deriving instance Show (AnfrageAnschluss i)

deriving instance Eq (AnfrageAnschluss i)

instance Anzeige (AnfrageAnschluss i) where
    anzeige :: AnfrageAnschluss i -> Sprache -> Text
    anzeige AnfrageAnschluss {} = Language.anschluss
    anzeige APin = Language.anschluss <-> Language.pin
    anzeige APCF8574Port {} = Language.anschluss <-> Language.pcf8574Port
    anzeige APCF8574PortVariant {aaVariante} =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> if aaVariante == VariantNormal
            then Language.normal
            else Language.a
    anzeige APCF8574PortVariantA0 {aaVariante, aaA0} =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> (if aaVariante == VariantNormal
                 then Language.normal
                 else Language.a)
        <^> Language.a0 <=> aaA0
    anzeige APCF8574PortVariantA0A1 {aaVariante, aaA0, aaA1} =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> (if aaVariante == VariantNormal
                 then Language.normal
                 else Language.a)
        <^> Language.a0 <=> aaA0 <^> Language.a1 <=> aaA1
    anzeige APCF8574PortVariantA0A1A2 {aaVariante, aaA0, aaA1, aaA2} =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> (if aaVariante == VariantNormal
                 then Language.normal
                 else Language.a)
        <^> Language.a0 <=> aaA0 <^> Language.a1 <=> aaA1 <^> Language.a2 <=> aaA2
    anzeige APCF8574PortVariantA0A1A2Port {aaiVariante, aaiA0, aaiA1, aaiA2, aaiPort} =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> (if aaiVariante == VariantNormal
                 then Language.normal
                 else Language.a)
        <^> Language.a0
        <=> aaiA0 <^> Language.a1 <=> aaiA1 <^> Language.a2 <=> aaiA2 <^> Language.port <=> aaiPort

instance Anfrage (AnfrageAnschluss i) where
    zeigeAnfrage :: AnfrageAnschluss i -> Sprache -> Text
    zeigeAnfrage AnfrageAnschluss {} = Language.anschluss
    zeigeAnfrage APin = Language.pin
    zeigeAnfrage APCF8574Port {} = Language.pcf8574 <-> Language.variante
    zeigeAnfrage APCF8574PortVariant {} = Language.a0
    zeigeAnfrage APCF8574PortVariantA0 {} = Language.a1
    zeigeAnfrage APCF8574PortVariantA0A1 {} = Language.a2
    zeigeAnfrage APCF8574PortVariantA0A1A2 {} = Language.port
    zeigeAnfrage APCF8574PortVariantA0A1A2Port {} = Language.interrupt <-> Language.pin

    zeigeAnfrageFehlgeschlagen :: AnfrageAnschluss i -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@APin eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage@APCF8574PortVariant {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen anfrage@APCF8574PortVariantA0 {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen anfrage@APCF8574PortVariantA0A1 {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen anfrage@APCF8574PortVariantA0A1A2 {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage@APCF8574PortVariantA0A1A2Port {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageAnschluss i -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageAnschluss {} = Just $ toBefehlsString . \sprache
        -> map ($ sprache) [Language.pin, Language.pcf8574Port]
    zeigeAnfrageOptionen APCF8574Port {} = Just $ toBefehlsString . \sprache
        -> map ($ sprache) [Language.normal, Language.a]
    zeigeAnfrageOptionen APCF8574PortVariant {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen APCF8574PortVariantA0 {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen APCF8574PortVariantA0A1 {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage AnschlussEither where
    type AnfrageTyp AnschlussEither = AnfrageAnschluss 'InterruptPinEgal

    -- | Eingabe eines 'Anschluss'
    anfrageAktualisieren :: AnfrageAnschluss 'InterruptPinEgal
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageAnschluss 'InterruptPinEgal) AnschlussEither
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
    anfrageAktualisieren APCF8574PortVariant {aaVariante} token@EingabeToken {eingabe} =
        case wähleValue token of
            (Just aaA0) -> AFZwischenwert $ APCF8574PortVariantA0 { aaVariante, aaA0 }
            Nothing -> AFFehler eingabe
    anfrageAktualisieren APCF8574PortVariantA0 {aaVariante, aaA0} token@EingabeToken {eingabe} =
        case wähleValue token of
            (Just aaA1) -> AFZwischenwert $ APCF8574PortVariantA0A1 { aaVariante, aaA0, aaA1 }
            Nothing -> AFFehler eingabe
    anfrageAktualisieren
        APCF8574PortVariantA0A1 {aaVariante, aaA0, aaA1}
        token@EingabeToken {eingabe} = case wähleValue token of
        (Just aaA2) -> AFZwischenwert $ APCF8574PortVariantA0A1A2 { aaVariante, aaA0, aaA1, aaA2 }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        APCF8574PortVariantA0A1A2 {aaVariante, aaA0, aaA1, aaA2}
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just port) -> AFErgebnis
            $ AnschlussOhne
            $ AnschlussPCF8574Port
            $ PCF8574Port
            { pcf8574 = PCF8574 { variant = aaVariante, a0 = aaA0, a1 = aaA1, a2 = aaA2 }
            , port = fromIntegral port
            }
        Nothing -> AFFehler eingabe

instance MitAnfrage (Anschluss 'MitInterruptPin) where
    type AnfrageTyp (Anschluss 'MitInterruptPin) = AnfrageAnschluss 'InterruptPinBenötigt

    -- | Eingabe eines 'Anschluss'
    anfrageAktualisieren
        :: AnfrageAnschluss 'InterruptPinBenötigt
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAnschluss 'InterruptPinBenötigt) (Anschluss 'MitInterruptPin)
    anfrageAktualisieren AnfrageAnschluss token =
        wähleZwischenwert token [(Lexer.Pin, APin), (Lexer.PCF8574Port, APCF8574Port)]
    anfrageAktualisieren APin EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis $ AnschlussPin $ Gpio $ fromIntegral pin
        Nothing -> AFFehler eingabe
    anfrageAktualisieren APCF8574Port token =
        wähleZwischenwert
            token
            [ (Lexer.A, APCF8574PortVariant VariantA)
            , (Lexer.Normal, APCF8574PortVariant VariantNormal)]
    anfrageAktualisieren APCF8574PortVariant {aaVariante} token@EingabeToken {eingabe} =
        case wähleValue token of
            (Just aaA0) -> AFZwischenwert $ APCF8574PortVariantA0 { aaVariante, aaA0 }
            Nothing -> AFFehler eingabe
    anfrageAktualisieren APCF8574PortVariantA0 {aaVariante, aaA0} token@EingabeToken {eingabe} =
        case wähleValue token of
            (Just aaA1) -> AFZwischenwert $ APCF8574PortVariantA0A1 { aaVariante, aaA0, aaA1 }
            Nothing -> AFFehler eingabe
    anfrageAktualisieren
        APCF8574PortVariantA0A1 {aaVariante, aaA0, aaA1}
        token@EingabeToken {eingabe} = case wähleValue token of
        (Just aaA2) -> AFZwischenwert $ APCF8574PortVariantA0A1A2 { aaVariante, aaA0, aaA1, aaA2 }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        (APCF8574PortVariantA0A1A2 aaiVariante aaiA0 aaiA1 aaiA2)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just port) -> AFZwischenwert
            $ APCF8574PortVariantA0A1A2Port
            { aaiVariante
            , aaiA0
            , aaiA1
            , aaiA2
            , aaiPort = fromIntegral port
            }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        (APCF8574PortVariantA0A1A2Port iVariant iA0 iA1 iA2 port)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis
            $ AnschlussPCF8574Port
            $ PCF8574Port
            { pcf8574 = PCF8574InterruptPin
                  { iVariant
                  , iA0
                  , iA1
                  , iA2
                  , interruptPin = Gpio $ fromIntegral pin
                  }
            , port
            }
        Nothing -> AFFehler eingabe
