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
import qualified Data.Text as Text
import Data.Word (Word8)

import Zug.Anbindung
       (AnschlussEither(), InterruptPinBenötigt(InterruptPinEgal), Pin(Gpio), Value(..), alleValues
      , Bahngeschwindigkeit(..), GeschwindigkeitsAnschlüsse(..), FahrtrichtungsAnschluss(..))
import Zug.Enums (Zugtyp(..), unterstützteZugtypen, GeschwindigkeitVariante(..))
import Zug.Language (Anzeige(..), Sprache(), (<->), (<^>), (<=>), (<~>), ($#), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, MitAnfrage(..), AnfrageZugtyp(..)
      , MitAnfrageZugtyp(..), FixerZugtyp, AnfrageGeschwindigkeitVariante(..)
      , AnfrageGeschwindigkeitEither(..), FixeGeschwindigkeitVariante, AnfrageFortsetzung(..)
      , wähleZwischenwert, ($<<))
import Zug.UI.Cmd.Parser.Anschluss (AnfrageAnschluss(AnfrageAnschluss))
import Zug.Warteschlange (Warteschlange)
import qualified Zug.Warteschlange as Warteschlange

-- | Unvollständige 'GeschwindigkeitsAnschüsse'.
data AnfrageGeschwindigkeitsAnschlüsse (g :: GeschwindigkeitVariante) where
    AGeschwindigkeitsPin :: AnfrageGeschwindigkeitsAnschlüsse 'Pwm
    AFahrstromAnschlüsseAnzahl :: AnfrageGeschwindigkeitsAnschlüsse 'KonstanteSpannung
    AFahrstromAnschlüsse :: { fahrstromAnschlüsseAnzahl :: Word8
                             , fahrstromAnschlüsseAkkumulator :: Warteschlange AnschlussEither
                             , fahrstromAnfrageAnschluss :: AnfrageAnschluss 'InterruptPinEgal
                             } -> AnfrageGeschwindigkeitsAnschlüsse 'KonstanteSpannung

deriving instance Eq (AnfrageGeschwindigkeitsAnschlüsse g)

deriving instance Show (AnfrageGeschwindigkeitsAnschlüsse g)

instance Anzeige (AnfrageGeschwindigkeitsAnschlüsse g) where
    anzeige :: AnfrageGeschwindigkeitsAnschlüsse g -> Sprache -> Text
    anzeige AGeschwindigkeitsPin = const Text.empty
    anzeige AFahrstromAnschlüsseAnzahl = const Text.empty
    anzeige
        AFahrstromAnschlüsse
        {fahrstromAnschlüsseAnzahl, fahrstromAnschlüsseAkkumulator, fahrstromAnfrageAnschluss} =
        Language.fahrstrom
        <-> Language.anschlüsse
        <~> (const "(" <> anzeige fahrstromAnschlüsseAnzahl <> const ")")
        <=> fahrstromAnschlüsseAkkumulator <^> fahrstromAnfrageAnschluss

instance Anfrage (AnfrageGeschwindigkeitsAnschlüsse g) where
    zeigeAnfrage :: AnfrageGeschwindigkeitsAnschlüsse g -> Sprache -> Text
    zeigeAnfrage AGeschwindigkeitsPin = Language.pin
    zeigeAnfrage AFahrstromAnschlüsseAnzahl =
        Language.anzahl $# Language.fahrstrom <-> Language.anschlüsse
    zeigeAnfrage
        AFahrstromAnschlüsse {fahrstromAnfrageAnschluss} = zeigeAnfrage fahrstromAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageGeschwindigkeitsAnschlüsse g -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AGeschwindigkeitsPin eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage@AFahrstromAnschlüsseAnzahl eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen AFahrstromAnschlüsse {fahrstromAnfrageAnschluss} eingabe =
        zeigeAnfrageFehlgeschlagenStandard fahrstromAnfrageAnschluss eingabe

    zeigeAnfrageOptionen :: AnfrageGeschwindigkeitsAnschlüsse g -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AFahrstromAnschlüsse {fahrstromAnfrageAnschluss} =
        zeigeAnfrageOptionen fahrstromAnfrageAnschluss
    zeigeAnfrageOptionen _anfrageGeschwindigkeitsAnschlüsse = Nothing

instance MitAnfrage (GeschwindigkeitsAnschlüsse g) where
    type AnfrageTyp (GeschwindigkeitsAnschlüsse g) = AnfrageGeschwindigkeitsAnschlüsse g

    anfrageAktualisieren
        :: AnfrageGeschwindigkeitsAnschlüsse g
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageGeschwindigkeitsAnschlüsse g) (GeschwindigkeitsAnschlüsse g)
    anfrageAktualisieren AGeschwindigkeitsPin EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin)
            -> AFErgebnis $ GeschwindigkeitsPin { geschwindigkeitsPin = Gpio $ fromIntegral pin }
        Nothing -> AFFehler eingabe
    anfrageAktualisieren AFahrstromAnschlüsseAnzahl EingabeToken {eingabe, ganzzahl} =
        case ganzzahl of
            Nothing -> AFFehler eingabe
            (Just 0) -> AFFehler eingabe
            (Just fahrstromAnzahl) -> AFZwischenwert
                $ AFahrstromAnschlüsse
                { fahrstromAnschlüsseAnzahl =
                      (fromIntegral $ min (fromIntegral (maxBound :: Word8)) fahrstromAnzahl)
                , fahrstromAnschlüsseAkkumulator = Warteschlange.leer
                , fahrstromAnfrageAnschluss = AnfrageAnschluss
                }
    anfrageAktualisieren
        anfrage@AFahrstromAnschlüsse
        {fahrstromAnschlüsseAnzahl, fahrstromAnschlüsseAkkumulator, fahrstromAnfrageAnschluss}
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren fahrstromAnfrageAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss 'InterruptPinEgal
                                      -> AnfrageGeschwindigkeitsAnschlüsse 'KonstanteSpannung
            anfrageAnschlussVerwenden
                fahrstromAnfrageAnschluss = anfrage { fahrstromAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageGeschwindigkeitsAnschlüsse 'KonstanteSpannung) (GeschwindigkeitsAnschlüsse 'KonstanteSpannung)
            anschlussVerwenden fahrstromAnschluss
                | fahrstromAnschlüsseAnzahl > 1 =
                    AFZwischenwert
                    $ AFahrstromAnschlüsse
                    { fahrstromAnschlüsseAnzahl = pred fahrstromAnschlüsseAnzahl
                    , fahrstromAnschlüsseAkkumulator = ergänzteAnschlüsse
                    , fahrstromAnfrageAnschluss = AnfrageAnschluss
                    }
                | otherwise =
                    AFErgebnis
                    $ FahrstromAnschlüsse
                    { fahrstromAnschlüsse = NonEmpty.fromList $ toList ergänzteAnschlüsse
                    }
                where
                    ergänzteAnschlüsse :: Warteschlange AnschlussEither
                    ergänzteAnschlüsse =
                        Warteschlange.anhängen fahrstromAnschluss fahrstromAnschlüsseAkkumulator

-- | Unvollständiger 'FahrtrichtungsAnschluss'.
data AnfrageFahrtrichtungsAnschluss (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
    AUmdrehenAnschluss :: { umdrehenAnfrageAnschluss :: AnfrageAnschluss 'InterruptPinEgal }
        -> AnfrageFahrtrichtungsAnschluss 'KonstanteSpannung 'Märklin
    AFahrtrichtungsAnschluss
        :: { fahrtrichtungsAnfrageAnschluss :: AnfrageAnschluss 'InterruptPinEgal }
        -> AnfrageFahrtrichtungsAnschluss g 'Lego

deriving instance Eq (AnfrageFahrtrichtungsAnschluss g z)

deriving instance Show (AnfrageFahrtrichtungsAnschluss g z)

instance Anzeige (AnfrageFahrtrichtungsAnschluss g z) where
    anzeige :: AnfrageFahrtrichtungsAnschluss g z -> Sprache -> Text
    anzeige AUmdrehenAnschluss {umdrehenAnfrageAnschluss} =
        Language.umdrehen <-> Language.anschluss <=> umdrehenAnfrageAnschluss
    anzeige AFahrtrichtungsAnschluss {fahrtrichtungsAnfrageAnschluss} =
        Language.fahrtrichtung <-> Language.anschluss <=> fahrtrichtungsAnfrageAnschluss

instance Anfrage (AnfrageFahrtrichtungsAnschluss g z) where
    zeigeAnfrage :: AnfrageFahrtrichtungsAnschluss g z -> Sprache -> Text
    zeigeAnfrage
        AUmdrehenAnschluss {umdrehenAnfrageAnschluss} = zeigeAnfrage umdrehenAnfrageAnschluss
    zeigeAnfrage AFahrtrichtungsAnschluss {fahrtrichtungsAnfrageAnschluss} =
        zeigeAnfrage fahrtrichtungsAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageFahrtrichtungsAnschluss g z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen AUmdrehenAnschluss {umdrehenAnfrageAnschluss} =
        zeigeAnfrageFehlgeschlagen umdrehenAnfrageAnschluss
    zeigeAnfrageFehlgeschlagen AFahrtrichtungsAnschluss {fahrtrichtungsAnfrageAnschluss} =
        zeigeAnfrageFehlgeschlagen fahrtrichtungsAnfrageAnschluss

    zeigeAnfrageOptionen :: AnfrageFahrtrichtungsAnschluss g z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AUmdrehenAnschluss {umdrehenAnfrageAnschluss} =
        zeigeAnfrageOptionen umdrehenAnfrageAnschluss
    zeigeAnfrageOptionen AFahrtrichtungsAnschluss {fahrtrichtungsAnfrageAnschluss} =
        zeigeAnfrageOptionen fahrtrichtungsAnfrageAnschluss

instance MitAnfrage (FahrtrichtungsAnschluss g z) where
    type AnfrageTyp (FahrtrichtungsAnschluss g z) = AnfrageFahrtrichtungsAnschluss g z

    anfrageAktualisieren
        :: AnfrageFahrtrichtungsAnschluss g z
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageFahrtrichtungsAnschluss g z) (FahrtrichtungsAnschluss g z)
    anfrageAktualisieren anfrage@AUmdrehenAnschluss {umdrehenAnfrageAnschluss} token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren umdrehenAnfrageAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss 'InterruptPinEgal
                                      -> AnfrageFahrtrichtungsAnschluss 'KonstanteSpannung 'Märklin
            anfrageAnschlussVerwenden
                umdrehenAnfrageAnschluss = anfrage { umdrehenAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageFahrtrichtungsAnschluss 'KonstanteSpannung 'Märklin) (FahrtrichtungsAnschluss 'KonstanteSpannung 'Märklin)
            anschlussVerwenden
                umdrehenAnschluss = AFErgebnis UmdrehenAnschluss { umdrehenAnschluss }
    anfrageAktualisieren anfrage@AFahrtrichtungsAnschluss {fahrtrichtungsAnfrageAnschluss} token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren fahrtrichtungsAnfrageAnschluss token
        where
            anfrageAnschlussVerwenden
                :: AnfrageAnschluss 'InterruptPinEgal -> AnfrageFahrtrichtungsAnschluss g 'Lego
            anfrageAnschlussVerwenden
                fahrtrichtungsAnfrageAnschluss = anfrage { fahrtrichtungsAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageFahrtrichtungsAnschluss g 'Lego) (FahrtrichtungsAnschluss g 'Lego)
            anschlussVerwenden fahrtrichtungsAnschluss =
                AFErgebnis FahrtrichtungsAnschluss { fahrtrichtungsAnschluss }

-- | Unvollständige 'Bahngeschwindigkeit'.
data AnfrageBahngeschwindigkeit (g :: AnfrageGeschwindigkeitVariante) (z :: AnfrageZugtyp) where
    AnfrageBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageGeschwindigkeitVariante 'AnfrageZugtyp
    -- GeschwindigkeitVariante
    ABahngeschwindigkeitMärklin
        :: AnfrageBahngeschwindigkeit 'AnfrageGeschwindigkeitVariante 'AnfrageMärklin
    ABahngeschwindigkeitLego
        :: AnfrageBahngeschwindigkeit 'AnfrageGeschwindigkeitVariante 'AnfrageLego
    -- GeschwindigkeitsAnschlüsse
    ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
        :: { abgAnfrageGeschwindigkeitsAnschlüsse
                 :: AnfrageGeschwindigkeitsAnschlüsse (FixeGeschwindigkeitVariante g)
           } -> AnfrageBahngeschwindigkeit g 'AnfrageMärklin
    ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
        :: { abgAnfrageGeschwindigkeitsAnschlüsse
                 :: AnfrageGeschwindigkeitsAnschlüsse (FixeGeschwindigkeitVariante g)
           } -> AnfrageBahngeschwindigkeit g 'AnfrageLego
    -- FahrtrichtungsAnschluss
    ABahngeschwindigkeitFahrtrichtungsAnschluss
        :: { abgGeschwindigkeitsAnschlüsse
                 :: GeschwindigkeitsAnschlüsse (FixeGeschwindigkeitVariante g)
           , abgAnfrageFahrtrichtungsAnschluss
                 :: AnfrageFahrtrichtungsAnschluss (FixeGeschwindigkeitVariante g) (FixerZugtyp z)
           } -> AnfrageBahngeschwindigkeit g z

deriving instance Eq (AnfrageBahngeschwindigkeit g z)

deriving instance Show (AnfrageBahngeschwindigkeit g z)

instance Anzeige (AnfrageBahngeschwindigkeit g z) where
    anzeige :: AnfrageBahngeschwindigkeit g z -> Sprache -> Text
    anzeige AnfrageBahngeschwindigkeit = Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitMärklin = Language.märklin <-> Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitLego = Language.lego <-> Language.bahngeschwindigkeit
    anzeige
        ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
        {abgAnfrageGeschwindigkeitsAnschlüsse} = _undefined --TODO
    anzeige
        ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse {abgAnfrageGeschwindigkeitsAnschlüsse} =
        _undefined --TODO
    anzeige
        ABahngeschwindigkeitFahrtrichtungsAnschluss
        {abgGeschwindigkeitsAnschlüsse, abgAnfrageFahrtrichtungsAnschluss} = _undefined --TODO

instance Anfrage (AnfrageBahngeschwindigkeit g z) where
    zeigeAnfrage :: AnfrageBahngeschwindigkeit g z -> Sprache -> Text
    zeigeAnfrage AnfrageBahngeschwindigkeit = Language.zugtyp
    zeigeAnfrage AMärklinBahngeschwindigkeit = Language.geschwindigkeitVariante
    zeigeAnfrage ABahngeschwindigkeitPwmMärklin = Language.name
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
    anfrageMärklin :: AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit 'AnfrageMärklin
    anfrageMärklin = AnfrageGeschwindigkeitNothing AMärklinBahngeschwindigkeit

    anfrageLego :: AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit 'AnfrageLego
    anfrageLego = AnfrageGeschwindigkeitPwm ALegoBahngeschwindigkeit

instance MitAnfrage (Bahngeschwindigkeit 'Pwm 'Märklin) where
    type AnfrageTyp (Bahngeschwindigkeit 'Pwm 'Märklin) =
        AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageMärklin

    -- | Eingabe einer 'Märklin'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageMärklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageMärklin) (Bahngeschwindigkeit 'Pwm 'Märklin)
    anfrageAktualisieren ABahngeschwindigkeitPwmMärklin EingabeToken {eingabe} =
        AFZwischenwert $ AMärklinBahngeschwindigkeitNamePwm eingabe
    anfrageAktualisieren AMärklinBahngeschwindigkeitNamePwm {abgmpName} token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, AMärklinBahngeschwindigkeitNameFließendPwm abgmpName HIGH)
            , (Lexer.LOW, AMärklinBahngeschwindigkeitNameFließendPwm abgmpName LOW)]
    anfrageAktualisieren
        (AMärklinBahngeschwindigkeitNameFließendPwm bgName bgFließend)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis
            $ Bahngeschwindigkeit
            { bgName
            , bgFließend
            , bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin $ Gpio $ fromIntegral pin
            , bgFahrtrichtungsAnschluss = KeinExpliziterAnschluss
            }
        Nothing -> AFFehler eingabe

instance MitAnfrage (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin) where
    type AnfrageTyp (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin) =
        AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin

    -- | Eingabe einer 'Märklin'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin) (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
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
                -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin
            anfrageAnschlussVerwenden
                abgmkFahrstromAnfrageAnschluss = anfrage { abgmkFahrstromAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin) (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
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
                     bgName
                     bgFließend
                     fahrstromAnschlüsse
                     umdrehenAnfrageAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren umdrehenAnfrageAnschluss token
        where
            anfrageAnschlussVerwenden
                :: AnfrageAnschluss 'InterruptPinEgal
                -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin
            anfrageAnschlussVerwenden
                abgmkUmdrehenAnfrageAnschluss = anfrage { abgmkUmdrehenAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin) (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
            anschlussVerwenden umdrehenAnschluss =
                AFErgebnis
                    Bahngeschwindigkeit
                    { bgName
                    , bgFließend
                    , bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse { fahrstromAnschlüsse }
                    , bgFahrtrichtungsAnschluss = UmdrehenAnschluss { umdrehenAnschluss }
                    }

instance MitAnfrage (Bahngeschwindigkeit 'Pwm 'Lego) where
    type AnfrageTyp (Bahngeschwindigkeit 'Pwm 'Lego) =
        AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageLego

    -- | Eingabe einer 'Lego'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageLego
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageLego) (Bahngeschwindigkeit 'Pwm 'Lego)
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
                     bgName
                     bgFließend
                     geschwindigkeitsPin
                     fahrtrichtungsAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren fahrtrichtungsAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss 'InterruptPinEgal
                                      -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageLego
            anfrageAnschlussVerwenden
                abglFahrtrichtungsAnfrageAnschluss = anfrage { abglFahrtrichtungsAnfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageLego) (Bahngeschwindigkeit 'Pwm 'Lego)
            anschlussVerwenden fahrtrichtungsAnschluss =
                AFErgebnis
                    Bahngeschwindigkeit
                    { bgName
                    , bgFließend
                    , bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin { geschwindigkeitsPin }
                    , bgFahrtrichtungsAnschluss =
                          FahrtrichtungsAnschluss { fahrtrichtungsAnschluss }
                    }

instance MitAnfrage (Bahngeschwindigkeit 'KonstanteSpannung 'Lego) where
    type AnfrageTyp (Bahngeschwindigkeit 'KonstanteSpannung 'Lego) =
        AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageLego

    -- | Eingabe einer 'Lego'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageLego
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageLego) (Bahngeschwindigkeit 'KonstanteSpannung 'Lego)
    anfrageAktualisieren _unmöglich _token = _undefined --TODO