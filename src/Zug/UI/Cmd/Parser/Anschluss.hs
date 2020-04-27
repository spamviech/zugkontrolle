{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.Anschluss (AnfrageAnschluss(..), MitInterruptPin(..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Word (Word8)

import Zug.Anbindung (Anschluss(..), AnschlussKlasse(..), Pin(..), PCF8574(..), PCF8574Port(..)
                    , PCF8574Variant(..), Value(), alleValues)
import Zug.Language (Anzeige(..), Sprache(), (<->), (<^>), (<=>), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), MitAnfrage(..), AnfrageFortsetzung(..), zeigeAnfrageFehlgeschlagenStandard
      , wähleZwischenwert, wähleValue)

-- | Wird ein Interrupt-Pin für den Anschluss benötigt?
data MitInterruptPin
    = InterruptPinBenötigt
    | InterruptPinEgal
    deriving (Show, Eq)

-- | Unvollständiger 'Anschluss'.
data AnfrageAnschluss
    = AnfrageAnschluss { mitInterruptPin :: MitInterruptPin }
    | APin
    | APCF8574Port { mitInterruptPin :: MitInterruptPin }
    | APCF8574PortVariant { mitInterruptPin :: MitInterruptPin, aaVariante :: PCF8574Variant }
    | APCF8574PortVariantA0
          { mitInterruptPin :: MitInterruptPin
          , aaVariante :: PCF8574Variant
          , aaA0 :: Value
          }
    | APCF8574PortVariantA0A1
          { mitInterruptPin :: MitInterruptPin
          , aaVariante :: PCF8574Variant
          , aaA0 :: Value
          , aaA1 :: Value
          }
    | APCF8574PortVariantA0A1A2
          { mitInterruptPin :: MitInterruptPin
          , aaVariante :: PCF8574Variant
          , aaA0 :: Value
          , aaA1 :: Value
          , aaA2 :: Value
          }
    | APCF8574PortVariantA0A1A2Port
          { aaVariante :: PCF8574Variant
          , aaA0 :: Value
          , aaA1 :: Value
          , aaA2 :: Value
          , aaPort :: Word8
          }
    deriving (Show, Eq)

instance Anzeige AnfrageAnschluss where
    anzeige :: AnfrageAnschluss -> Sprache -> Text
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
    anzeige APCF8574PortVariantA0A1A2Port {aaVariante, aaA0, aaA1, aaA2, aaPort} =
        Language.anschluss
        <-> Language.pcf8574Port
        <^> Language.variante
        <=> (if aaVariante == VariantNormal
                 then Language.normal
                 else Language.a)
        <^> Language.a0
        <=> aaA0 <^> Language.a1 <=> aaA1 <^> Language.a2 <=> aaA2 <^> Language.port <=> aaPort

instance Anfrage AnfrageAnschluss where
    zeigeAnfrage :: AnfrageAnschluss -> Sprache -> Text
    zeigeAnfrage AnfrageAnschluss {} = Language.anschluss
    zeigeAnfrage APin = Language.pin
    zeigeAnfrage APCF8574Port {} = Language.pcf8574 <-> Language.variante
    zeigeAnfrage APCF8574PortVariant {} = Language.a0
    zeigeAnfrage APCF8574PortVariantA0 {} = Language.a1
    zeigeAnfrage APCF8574PortVariantA0A1 {} = Language.a2
    zeigeAnfrage APCF8574PortVariantA0A1A2 {} = Language.port
    zeigeAnfrage APCF8574PortVariantA0A1A2Port {} = Language.interrupt <-> Language.pin

    zeigeAnfrageFehlgeschlagen :: AnfrageAnschluss -> Text -> Sprache -> Text
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

    zeigeAnfrageOptionen :: AnfrageAnschluss -> Maybe (Sprache -> Text)
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

instance MitAnfrage Anschluss where
    type AnfrageTyp Anschluss = AnfrageAnschluss

    -- | Eingabe eines 'Anschluss'
    anfrageAktualisieren
        :: AnfrageAnschluss -> EingabeToken -> AnfrageFortsetzung AnfrageAnschluss Anschluss
    anfrageAktualisieren AnfrageAnschluss {mitInterruptPin} token =
        wähleZwischenwert
            token
            [(Lexer.Pin, APin), (Lexer.PCF8574Port, APCF8574Port { mitInterruptPin })]
    anfrageAktualisieren APin EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis $ zuAnschluss $ Gpio $ fromIntegral pin
        Nothing -> AFFehler eingabe
    anfrageAktualisieren APCF8574Port {mitInterruptPin} token =
        wähleZwischenwert
            token
            [ (Lexer.A, APCF8574PortVariant mitInterruptPin VariantA)
            , (Lexer.Normal, APCF8574PortVariant mitInterruptPin VariantNormal)]
    anfrageAktualisieren
        APCF8574PortVariant {mitInterruptPin, aaVariante}
        token@EingabeToken {eingabe} = case wähleValue token of
        (Just aaA0) -> AFZwischenwert $ APCF8574PortVariantA0 { mitInterruptPin, aaVariante, aaA0 }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        APCF8574PortVariantA0 {mitInterruptPin, aaVariante, aaA0}
        token@EingabeToken {eingabe} = case wähleValue token of
        (Just aaA1)
            -> AFZwischenwert $ APCF8574PortVariantA0A1 { mitInterruptPin, aaVariante, aaA0, aaA1 }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        APCF8574PortVariantA0A1 {mitInterruptPin, aaVariante, aaA0, aaA1}
        token@EingabeToken {eingabe} = case wähleValue token of
        (Just aaA2) -> AFZwischenwert
            $ APCF8574PortVariantA0A1A2 { mitInterruptPin, aaVariante, aaA0, aaA1, aaA2 }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        APCF8574PortVariantA0A1A2
        {mitInterruptPin = InterruptPinBenötigt, aaVariante, aaA0, aaA1, aaA2}
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just port) -> AFZwischenwert
            $ APCF8574PortVariantA0A1A2Port
            { aaVariante
            , aaA0
            , aaA1
            , aaA2
            , aaPort = fromIntegral port
            }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        APCF8574PortVariantA0A1A2
        {mitInterruptPin = InterruptPinEgal, aaVariante, aaA0, aaA1, aaA2}
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just port) -> AFErgebnis
            $ AnschlussPCF8574Port
            $ PCF8574Port
            { pcf8574 = PCF8574
                  { variant = aaVariante
                  , a0 = aaA0
                  , a1 = aaA1
                  , a2 = aaA2
                  , interruptPin = Nothing
                  }
            , port = fromIntegral port
            }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        (APCF8574PortVariantA0A1A2Port variant a0 a1 a2 port)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis
            $ AnschlussPCF8574Port
            $ PCF8574Port
            { pcf8574 =
                  PCF8574 { variant, a0, a1, a2, interruptPin = Just $ Gpio $ fromIntegral pin }
            , port
            }
        Nothing -> AFFehler eingabe
