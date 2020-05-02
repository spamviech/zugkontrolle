{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.StreckenObjekt.Bahngeschwindigkeit (AnfrageBahngeschwindigkeit(..)) where

import Data.Foldable (Foldable(toList))
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup((<>)))
import Data.Text (Text)
import Data.Word (Word8)

import Zug.Anbindung (AnschlussEither(), InterruptPinBenötigt(InterruptPinEgal), Pin(Gpio)
                    , Value(..), alleValues, Bahngeschwindigkeit(..))
import Zug.Enums (Zugtyp(..), unterstützteZugtypen, GeschwindigkeitVariante(..))
import Zug.Language (Anzeige(..), Sprache(), (<->), (<^>), (<=>), (<~>), ($#), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, MitAnfrage(..), AnfrageZugtyp(..)
      , MitAnfrageZugtyp(..), AnfrageGeschwindigkeitVariante(..), AnfrageGeschwindigkeitEither(..)
      , AnfrageFortsetzung(..), wähleZwischenwert, ($<<))
import Zug.UI.Cmd.Parser.Anschluss (AnfrageAnschluss(AnfrageAnschluss))
import Zug.Warteschlange (Warteschlange)
import qualified Zug.Warteschlange as Warteschlange

-- | Unvollständige 'Bahngeschwindigkeit'.
data AnfrageBahngeschwindigkeit (g :: AnfrageGeschwindigkeitVariante) (z :: AnfrageZugtyp) where
    AnfrageBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageGeschwindigkeitVariante 'AnfrageZugtyp
    -- Märklin
    AMärklinBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageGeschwindigkeitVariante 'AnfrageZugtypMärklin
    -- Pwm, Märklin
    AMärklinBahngeschwindigkeitPwm :: AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNamePwm :: { abgmpName :: Text }
        -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNameFließendPwm :: { abgmpName :: Text, abgmpFließend :: Value }
        -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypMärklin
    -- Konstante Spannung, Märklin
    AMärklinBahngeschwindigkeitKonstanteSpannung
        :: AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNameKonstanteSpannung :: { abgmkName :: Text }
        -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNameFließendKonstanteSpannung
        :: { abgmkName :: Text, abgmkFließend :: Value }
        -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNameFließendFahrstromAnzahlKonstanteSpannung
        :: { abgmkName :: Text
           , abgmkFließend :: Value
           , abgmkFahrstromAnschlüsseAnzahl :: Word8
           , abgmkFahrstromAnschlüsseAkkumulator :: Warteschlange AnschlussEither
           , abgmkFahrstromAnfrageAnschluss :: AnfrageAnschluss 'InterruptPinEgal
           } -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNameFließendFahrstromKonstanteSpannung
        :: { abgmkName :: Text
           , abgmkFließend :: Value
           , abgmkFahrstromAnschlüsse :: NonEmpty AnschlussEither
           , abgmkUmdrehenAnfrageAnschluss :: AnfrageAnschluss 'InterruptPinEgal
           } -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin
    -- Pwm, Lego
    ALegoBahngeschwindigkeit :: AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitName :: { abglName :: Text }
        -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitNameFließend :: { abglName :: Text, abglFließend :: Value }
        -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
        :: { abglName :: Text
           , abglFließend :: Value
           , abglGeschwindigkeitsPin :: Pin
           , abglFahrtrichtungsAnfrageAnschluss :: AnfrageAnschluss 'InterruptPinEgal
           } -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego

deriving instance Eq (AnfrageBahngeschwindigkeit g z)

deriving instance Show (AnfrageBahngeschwindigkeit g z)

instance Anzeige (AnfrageBahngeschwindigkeit g z) where
    anzeige :: AnfrageBahngeschwindigkeit g z -> Sprache -> Text
    anzeige AnfrageBahngeschwindigkeit = Language.bahngeschwindigkeit
    anzeige
        AMärklinBahngeschwindigkeit = Language.bahngeschwindigkeit <-> Language.geschwindigkeitPwm
    anzeige AMärklinBahngeschwindigkeitPwm =
        Language.märklin <-> Language.geschwindigkeitPwm <-> Language.bahngeschwindigkeit
    anzeige (AMärklinBahngeschwindigkeitNamePwm name) =
        Language.märklin
        <-> Language.geschwindigkeitPwm
        <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    anzeige (AMärklinBahngeschwindigkeitNameFließendPwm name fließend) =
        Language.märklin
        <-> Language.bahngeschwindigkeit
        <-> Language.geschwindigkeitPwm
        <^> Language.name <=> name <^> Language.fließendValue <=> fließend
    anzeige AMärklinBahngeschwindigkeitKonstanteSpannung =
        Language.märklin
        <-> Language.geschwindigkeitKonstanteSpannung
        <-> Language.bahngeschwindigkeit
    anzeige (AMärklinBahngeschwindigkeitNameKonstanteSpannung name) =
        Language.märklin
        <-> Language.geschwindigkeitKonstanteSpannung
        <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    anzeige (AMärklinBahngeschwindigkeitNameFließendKonstanteSpannung name fließend) =
        Language.märklin
        <-> Language.bahngeschwindigkeit
        <-> Language.geschwindigkeitKonstanteSpannung
        <^> Language.name <=> name <^> Language.fließendValue <=> fließend
    anzeige
        (AMärklinBahngeschwindigkeitNameFließendFahrstromAnzahlKonstanteSpannung
             name
             fließend
             fahrstromAnzahl
             fahrstromAnschlüsse
             fahrstromAnfrageAnschluss) =
        Language.märklin
        <-> Language.bahngeschwindigkeit
        <-> Language.geschwindigkeitKonstanteSpannung
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend
        <^> Language.fahrstrom
        <-> Language.anschlüsse
        <~> (const "(" <> anzeige fahrstromAnzahl <> const ")")
        <=> fahrstromAnschlüsse <^> fahrstromAnfrageAnschluss
    anzeige
        (AMärklinBahngeschwindigkeitNameFließendFahrstromKonstanteSpannung
             name
             fließend
             fahrstromAnschlüsse
             umdrehenAnschluss) =
        Language.märklin
        <-> Language.bahngeschwindigkeit
        <-> Language.geschwindigkeitKonstanteSpannung
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend
        <^> Language.fahrstrom
        <-> Language.anschlüsse
        <=> fahrstromAnschlüsse <^> Language.umdrehen <-> Language.anschluss <=> umdrehenAnschluss
    anzeige ALegoBahngeschwindigkeit = Language.lego <-> Language.bahngeschwindigkeit
    anzeige (ALegoBahngeschwindigkeitName name) =
        Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    anzeige (ALegoBahngeschwindigkeitNameFließend name fließend) =
        Language.lego
        <-> Language.bahngeschwindigkeit
        <^> Language.name <=> name <^> Language.fließendValue <=> fließend
    anzeige
        (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
             name
             fließend
             geschwindigkeitsPin
             fahrtrichtungsAnschluss) =
        Language.lego
        <-> Language.bahngeschwindigkeit
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend
        <^> Language.geschwindigkeit
        <-> Language.pin
        <=> geschwindigkeitsPin
        <^> Language.fahrtrichtung <-> Language.anschluss <=> fahrtrichtungsAnschluss

instance Anfrage (AnfrageBahngeschwindigkeit g z) where
    zeigeAnfrage :: AnfrageBahngeschwindigkeit g z -> Sprache -> Text
    zeigeAnfrage AnfrageBahngeschwindigkeit = Language.zugtyp
    zeigeAnfrage AMärklinBahngeschwindigkeit = Language.geschwindigkeitVariante
    zeigeAnfrage AMärklinBahngeschwindigkeitPwm = Language.name
    zeigeAnfrage AMärklinBahngeschwindigkeitNamePwm {} = Language.fließendValue
    zeigeAnfrage AMärklinBahngeschwindigkeitNameFließendPwm {} = Language.pin
    zeigeAnfrage AMärklinBahngeschwindigkeitKonstanteSpannung = Language.name
    zeigeAnfrage AMärklinBahngeschwindigkeitNameKonstanteSpannung {} = Language.fließendValue
    zeigeAnfrage AMärklinBahngeschwindigkeitNameFließendKonstanteSpannung {} =
        Language.anzahl $# Language.fahrstrom <-> Language.anschlüsse
    zeigeAnfrage
        AMärklinBahngeschwindigkeitNameFließendFahrstromAnzahlKonstanteSpannung
        {abgmkFahrstromAnfrageAnschluss} = zeigeAnfrage abgmkFahrstromAnfrageAnschluss
    zeigeAnfrage
        AMärklinBahngeschwindigkeitNameFließendFahrstromKonstanteSpannung
        {abgmkUmdrehenAnfrageAnschluss} = zeigeAnfrage abgmkUmdrehenAnfrageAnschluss
    zeigeAnfrage ALegoBahngeschwindigkeit = Language.name
    zeigeAnfrage ALegoBahngeschwindigkeitName {} = Language.fließendValue
    zeigeAnfrage ALegoBahngeschwindigkeitNameFließend {} = Language.pin
    zeigeAnfrage
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss} =
        zeigeAnfrage abglFahrtrichtungsAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageBahngeschwindigkeit g z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AMärklinBahngeschwindigkeitNamePwm {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage@AMärklinBahngeschwindigkeitNameFließendPwm {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@AMärklinBahngeschwindigkeitNameKonstanteSpannung {}
        eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@AMärklinBahngeschwindigkeitNameFließendKonstanteSpannung {}
        eingabe = zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        AMärklinBahngeschwindigkeitNameFließendFahrstromAnzahlKonstanteSpannung
        {abgmkFahrstromAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen abgmkFahrstromAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        AMärklinBahngeschwindigkeitNameFließendFahrstromKonstanteSpannung
        {abgmkUmdrehenAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen abgmkUmdrehenAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage@ALegoBahngeschwindigkeitName {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage@ALegoBahngeschwindigkeitNameFließend {} eingabe =
        zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen abglFahrtrichtungsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageBahngeschwindigkeit g z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageBahngeschwindigkeit = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList unterstützteZugtypen
    zeigeAnfrageOptionen AMärklinBahngeschwindigkeit = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ [Pwm, KonstanteSpannung]
    zeigeAnfrageOptionen AMärklinBahngeschwindigkeitNamePwm {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen AMärklinBahngeschwindigkeitNameKonstanteSpannung {} =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen
        AMärklinBahngeschwindigkeitNameFließendFahrstromAnzahlKonstanteSpannung
        {abgmkFahrstromAnfrageAnschluss} = zeigeAnfrageOptionen abgmkFahrstromAnfrageAnschluss
    zeigeAnfrageOptionen
        AMärklinBahngeschwindigkeitNameFließendFahrstromKonstanteSpannung
        {abgmkUmdrehenAnfrageAnschluss} = zeigeAnfrageOptionen abgmkUmdrehenAnfrageAnschluss
    zeigeAnfrageOptionen ALegoBahngeschwindigkeitName {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss} =
        zeigeAnfrageOptionen abglFahrtrichtungsAnfrageAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrageZugtyp (AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit) where
    anfrageMärklin
        :: AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    anfrageMärklin = AnfrageGeschwindigkeitNothing AMärklinBahngeschwindigkeit

    anfrageLego :: AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    anfrageLego = AnfrageGeschwindigkeitPwm ALegoBahngeschwindigkeit

instance MitAnfrage (Bahngeschwindigkeit 'Pwm 'Märklin) where
    type AnfrageTyp (Bahngeschwindigkeit 'Pwm 'Märklin) =
        AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypMärklin

    -- | Eingabe einer 'Märklin'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypMärklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypMärklin) (Bahngeschwindigkeit 'Pwm 'Märklin)
    anfrageAktualisieren AMärklinBahngeschwindigkeitPwm EingabeToken {eingabe} =
        AFZwischenwert $ AMärklinBahngeschwindigkeitNamePwm eingabe
    anfrageAktualisieren AMärklinBahngeschwindigkeitNamePwm {abgmpName} token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, AMärklinBahngeschwindigkeitNameFließendPwm abgmpName HIGH)
            , (Lexer.LOW, AMärklinBahngeschwindigkeitNameFließendPwm abgmpName LOW)]
    anfrageAktualisieren
        (AMärklinBahngeschwindigkeitNameFließendPwm bgmpName bgmpFließend)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis
            $ MärklinBahngeschwindigkeitPwm
            { bgmpName
            , bgmpFließend
            , bgmpGeschwindigkeitsPin = Gpio $ fromIntegral pin
            }
        Nothing -> AFFehler eingabe

instance MitAnfrage (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin) where
    type AnfrageTyp (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin) =
        AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin

    -- | Eingabe einer 'Märklin'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin) (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
    anfrageAktualisieren AMärklinBahngeschwindigkeitKonstanteSpannung EingabeToken {eingabe} =
        AFZwischenwert $ AMärklinBahngeschwindigkeitNameKonstanteSpannung eingabe
    anfrageAktualisieren AMärklinBahngeschwindigkeitNameKonstanteSpannung {abgmkName} token =
        wähleZwischenwert
            token
            [ ( Lexer.HIGH
                  , AMärklinBahngeschwindigkeitNameFließendKonstanteSpannung abgmkName HIGH
                  )
            , (Lexer.LOW, AMärklinBahngeschwindigkeitNameFließendKonstanteSpannung abgmkName LOW)]
    anfrageAktualisieren
        (AMärklinBahngeschwindigkeitNameFließendKonstanteSpannung name fließend)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just 0) -> AFFehler eingabe
        (Just fahrstromAnzahl) -> AFZwischenwert
            $ AMärklinBahngeschwindigkeitNameFließendFahrstromAnzahlKonstanteSpannung
                name
                fließend
                (fromIntegral $ min (fromIntegral (maxBound :: Word8)) fahrstromAnzahl)
                Warteschlange.leer
                AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(AMärklinBahngeschwindigkeitNameFließendFahrstromAnzahlKonstanteSpannung
                     name
                     fließend
                     fahrstromAnzahl
                     fahrstromAkkumulator
                     fahrstromAnfrageAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren fahrstromAnfrageAnschluss token
        where
            anfrageAnschlussVerwenden
                :: AnfrageAnschluss 'InterruptPinEgal
                -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin
            anfrageAnschlussVerwenden
                abgmkFahrstromAnfrageAnschluss = anfrage { abgmkFahrstromAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin) (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
            anschlussVerwenden fahrstromAnschluss
                | fahrstromAnzahl > 1 =
                    AFZwischenwert
                    $ AMärklinBahngeschwindigkeitNameFließendFahrstromAnzahlKonstanteSpannung
                        name
                        fließend
                        (pred fahrstromAnzahl)
                        ergänzteAnschlüsse
                        AnfrageAnschluss
                | otherwise =
                    AFZwischenwert
                    $ AMärklinBahngeschwindigkeitNameFließendFahrstromKonstanteSpannung
                        name
                        fließend
                        (NonEmpty.fromList $ toList ergänzteAnschlüsse)
                        AnfrageAnschluss
                where
                    ergänzteAnschlüsse :: Warteschlange AnschlussEither
                    ergänzteAnschlüsse =
                        Warteschlange.anhängen fahrstromAnschluss fahrstromAkkumulator
    anfrageAktualisieren
        anfrage@(AMärklinBahngeschwindigkeitNameFließendFahrstromKonstanteSpannung
                     bgmkName
                     bgmkFließend
                     bgmkFahrstromAnschlüsse
                     umdrehenAnfrageAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren umdrehenAnfrageAnschluss token
        where
            anfrageAnschlussVerwenden
                :: AnfrageAnschluss 'InterruptPinEgal
                -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin
            anfrageAnschlussVerwenden
                abgmkUmdrehenAnfrageAnschluss = anfrage { abgmkUmdrehenAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypMärklin) (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
            anschlussVerwenden bgmkUmdrehenAnschluss =
                AFErgebnis
                    MärklinBahngeschwindigkeitKonstanteSpannung
                    { bgmkName
                    , bgmkFließend
                    , bgmkFahrstromAnschlüsse
                    , bgmkUmdrehenAnschluss
                    }

instance MitAnfrage (Bahngeschwindigkeit 'Pwm 'Lego) where
    type AnfrageTyp (Bahngeschwindigkeit 'Pwm 'Lego) =
        AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego

    -- | Eingabe einer 'Lego'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego) (Bahngeschwindigkeit 'Pwm 'Lego)
    anfrageAktualisieren ALegoBahngeschwindigkeit EingabeToken {eingabe} =
        AFZwischenwert $ ALegoBahngeschwindigkeitName eingabe
    anfrageAktualisieren ALegoBahngeschwindigkeitName {abglName} token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, ALegoBahngeschwindigkeitNameFließend abglName HIGH)
            , (Lexer.LOW, ALegoBahngeschwindigkeitNameFließend abglName LOW)]
    anfrageAktualisieren
        (ALegoBahngeschwindigkeitNameFließend name fließend)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFZwischenwert
            $ ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
                name
                fließend
                (Gpio $ fromIntegral pin)
                AnfrageAnschluss
        Nothing -> AFFehler eingabe
    anfrageAktualisieren
        anfrage@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
                     bglName
                     bglFließend
                     bglGeschwindigkeitsPin
                     fahrtrichtungsAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren fahrtrichtungsAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss 'InterruptPinEgal
                                      -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego
            anfrageAnschlussVerwenden
                abglFahrtrichtungsAnfrageAnschluss = anfrage { abglFahrtrichtungsAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageZugtypLego) (Bahngeschwindigkeit 'Pwm 'Lego)
            anschlussVerwenden bglFahrtrichtungsAnschluss =
                AFErgebnis
                    LegoBahngeschwindigkeit
                    { bglName
                    , bglFließend
                    , bglGeschwindigkeitsPin
                    , bglFahrtrichtungsAnschluss
                    }

instance MitAnfrage (Bahngeschwindigkeit 'KonstanteSpannung 'Lego) where
    type AnfrageTyp (Bahngeschwindigkeit 'KonstanteSpannung 'Lego) =
        AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypLego

    -- | Eingabe einer 'Lego'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypLego
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageZugtypLego) (Bahngeschwindigkeit 'KonstanteSpannung 'Lego)
    anfrageAktualisieren _unmöglich _token =
        error "anfrageAktualisieren mit Bahngeschwindigkeit 'KonstanteSpannung 'Lego aufgerufen!"