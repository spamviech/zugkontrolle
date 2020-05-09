{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.StreckenObjekt.Bahngeschwindigkeit
  ( AnfrageBahngeschwindigkeit(..)
  , GewählteGeschwindigkeitVariante()
  , GewählterZugtyp()
  ) where

import Data.Foldable (Foldable(toList))
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
      , MitAnfrageZugtyp(..), FixerZugtyp, AngefragterZugtyp, AnfrageGeschwindigkeitVariante(..)
      , AnfrageGeschwindigkeitEither(..), FixeGeschwindigkeitVariante
      , AngefragteGeschwindigkeitVariante, AnfrageFortsetzung(..), wähleZwischenwert, ($<<))
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
    -- Name
    ABahngeschwindigkeitMärklinPwm :: AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageMärklin
    ABahngeschwindigkeitMärklinKonstanteSpannung
        :: AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin
    ABahngeschwindigkeitLegoPwm :: AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageLego
    ABahngeschwindigkeitLegoKonstanteSpannung
        :: AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageLego
    -- Fließend
    ABahngeschwindigkeitMärklinPwmFließend :: { abgmpName :: Text }
        -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageMärklin
    ABahngeschwindigkeitMärklinKonstanteSpannungFließend :: { abgmkName :: Text }
        -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin
    ABahngeschwindigkeitLegoPwmFließend :: { abglpName :: Text }
        -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageLego
    ABahngeschwindigkeitLegoKonstanteSpannungFließend :: { abglkName :: Text }
        -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageLego
    -- GeschwindigkeitsAnschlüsse
    ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
        :: { abgmName :: Text
           , abgmFließend :: Value
           , abgmAnfrageGeschwindigkeitsAnschlüsse
                 :: AnfrageGeschwindigkeitsAnschlüsse (FixeGeschwindigkeitVariante g)
           } -> AnfrageBahngeschwindigkeit g 'AnfrageMärklin
    ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
        :: { abglName :: Text
           , abglFließend :: Value
           , abglAnfrageGeschwindigkeitsAnschlüsse
                 :: AnfrageGeschwindigkeitsAnschlüsse (FixeGeschwindigkeitVariante g)
           } -> AnfrageBahngeschwindigkeit g 'AnfrageLego
    -- FahrtrichtungsAnschluss
    ABahngeschwindigkeitFahrtrichtungsAnschluss
        :: { abgName :: Text
           , abgFließend :: Value
           , abgGeschwindigkeitsAnschlüsse
                 :: GeschwindigkeitsAnschlüsse (FixeGeschwindigkeitVariante g)
           , abgAnfrageFahrtrichtungsAnschluss
                 :: AnfrageFahrtrichtungsAnschluss (FixeGeschwindigkeitVariante g) (FixerZugtyp z)
           } -> AnfrageBahngeschwindigkeit g z

deriving instance Eq (AnfrageBahngeschwindigkeit g z)

deriving instance Show (AnfrageBahngeschwindigkeit g z)

class GewählteGeschwindigkeitVariante (g :: AnfrageGeschwindigkeitVariante) where
    gewählteGeschwindigkeitVariante
        :: forall a (z :: AnfrageZugtyp). a g z -> GeschwindigkeitVariante

instance GewählteGeschwindigkeitVariante 'AnfrageGeschwindigkeitVariante where
    gewählteGeschwindigkeitVariante :: forall a (z :: AnfrageZugtyp).
                                     a 'AnfrageGeschwindigkeitVariante z
                                     -> GeschwindigkeitVariante
    gewählteGeschwindigkeitVariante =
        error "gewählteGeschwindigkeitVariante für AnfrageGeschwindigkeitVariante aufgerufen!"

instance GewählteGeschwindigkeitVariante 'AnfragePwm where
    gewählteGeschwindigkeitVariante
        :: forall a (z :: AnfrageZugtyp). a 'AnfragePwm z -> GeschwindigkeitVariante
    gewählteGeschwindigkeitVariante = const Pwm

instance GewählteGeschwindigkeitVariante 'AnfrageKonstanteSpannung where
    gewählteGeschwindigkeitVariante
        :: forall a (z :: AnfrageZugtyp). a 'AnfrageKonstanteSpannung z -> GeschwindigkeitVariante
    gewählteGeschwindigkeitVariante = const KonstanteSpannung

class GewählterZugtyp (z :: AnfrageZugtyp) where
    gewählterZugtyp :: a z -> Zugtyp

instance GewählterZugtyp 'AnfrageZugtyp where
    gewählterZugtyp :: a 'AnfrageZugtyp -> Zugtyp
    gewählterZugtyp = error "gewählterZugtyp für AnfrageZugtyp aufgerufen"

instance GewählterZugtyp 'AnfrageMärklin where
    gewählterZugtyp :: a 'AnfrageMärklin -> Zugtyp
    gewählterZugtyp = const Märklin

instance GewählterZugtyp 'AnfrageLego where
    gewählterZugtyp :: a 'AnfrageLego -> Zugtyp
    gewählterZugtyp = const Lego

instance (GewählteGeschwindigkeitVariante g, GewählterZugtyp z)
    => Anzeige (AnfrageBahngeschwindigkeit g z) where
    anzeige :: AnfrageBahngeschwindigkeit g z -> Sprache -> Text
    anzeige AnfrageBahngeschwindigkeit = Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitMärklin = Language.märklin <-> Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitLego = Language.lego <-> Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitMärklinPwm =
        Language.märklin <-> Language.geschwindigkeitPwm <-> Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitMärklinKonstanteSpannung =
        Language.märklin
        <-> Language.geschwindigkeitKonstanteSpannung
        <-> Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitLegoPwm =
        Language.lego <-> Language.geschwindigkeitPwm <-> Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitLegoKonstanteSpannung =
        Language.lego
        <-> Language.geschwindigkeitKonstanteSpannung
        <-> Language.bahngeschwindigkeit
    anzeige ABahngeschwindigkeitMärklinPwmFließend {abgmpName} =
        Language.märklin
        <-> Language.geschwindigkeitPwm
        <-> Language.bahngeschwindigkeit <^> Language.name <=> abgmpName
    anzeige ABahngeschwindigkeitMärklinKonstanteSpannungFließend {abgmkName} =
        Language.märklin
        <-> Language.geschwindigkeitKonstanteSpannung
        <-> Language.bahngeschwindigkeit <^> Language.name <=> abgmkName
    anzeige ABahngeschwindigkeitLegoPwmFließend {abglpName} =
        Language.lego
        <-> Language.geschwindigkeitPwm
        <-> Language.bahngeschwindigkeit <^> Language.name <=> abglpName
    anzeige ABahngeschwindigkeitLegoKonstanteSpannungFließend {abglkName} =
        Language.lego
        <-> Language.geschwindigkeitKonstanteSpannung
        <-> Language.bahngeschwindigkeit <^> Language.name <=> abglkName
    anzeige
        anfrage@ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
        {abgmName, abgmFließend, abgmAnfrageGeschwindigkeitsAnschlüsse} =
        Language.märklin
        <-> gewählteGeschwindigkeitVariante anfrage
        <-> Language.bahngeschwindigkeit
        <^> Language.name
        <=> abgmName
        <^> Language.fließend <=> abgmFließend <^> abgmAnfrageGeschwindigkeitsAnschlüsse
    anzeige
        anfrage@ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
        {abglName, abglFließend, abglAnfrageGeschwindigkeitsAnschlüsse} =
        Language.lego
        <-> gewählteGeschwindigkeitVariante anfrage
        <-> Language.bahngeschwindigkeit
        <^> Language.name
        <=> abglName
        <^> Language.fließend <=> abglFließend <^> abglAnfrageGeschwindigkeitsAnschlüsse
    anzeige
        anfrage@ABahngeschwindigkeitFahrtrichtungsAnschluss
        {abgName, abgFließend, abgGeschwindigkeitsAnschlüsse, abgAnfrageFahrtrichtungsAnschluss} =
        gewählterZugtyp anfrage
        <-> gewählteGeschwindigkeitVariante anfrage
        <-> Language.bahngeschwindigkeit
        <^> Language.name
        <=> abgName
        <^> Language.fließend
        <=> abgFließend <^> abgGeschwindigkeitsAnschlüsse <^> abgAnfrageFahrtrichtungsAnschluss

instance Anfrage (AnfrageBahngeschwindigkeit g z) where
    zeigeAnfrage :: AnfrageBahngeschwindigkeit g z -> Sprache -> Text
    zeigeAnfrage AnfrageBahngeschwindigkeit = Language.zugtyp
    zeigeAnfrage ABahngeschwindigkeitMärklin = Language.geschwindigkeitVariante
    zeigeAnfrage ABahngeschwindigkeitLego = Language.geschwindigkeitVariante
    zeigeAnfrage ABahngeschwindigkeitMärklinPwm = Language.name
    zeigeAnfrage ABahngeschwindigkeitMärklinKonstanteSpannung = Language.name
    zeigeAnfrage ABahngeschwindigkeitLegoPwm = Language.name
    zeigeAnfrage ABahngeschwindigkeitLegoKonstanteSpannung = Language.name
    zeigeAnfrage ABahngeschwindigkeitMärklinPwmFließend {} = Language.fließendValue
    zeigeAnfrage
        ABahngeschwindigkeitMärklinKonstanteSpannungFließend {} = Language.fließendValue
    zeigeAnfrage ABahngeschwindigkeitLegoPwmFließend {} = Language.fließendValue
    zeigeAnfrage ABahngeschwindigkeitLegoKonstanteSpannungFließend {} = Language.fließendValue
    zeigeAnfrage
        ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
        {abgmAnfrageGeschwindigkeitsAnschlüsse} =
        zeigeAnfrage abgmAnfrageGeschwindigkeitsAnschlüsse
    zeigeAnfrage
        ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
        {abglAnfrageGeschwindigkeitsAnschlüsse} =
        zeigeAnfrage abglAnfrageGeschwindigkeitsAnschlüsse
    zeigeAnfrage ABahngeschwindigkeitFahrtrichtungsAnschluss {abgAnfrageFahrtrichtungsAnschluss} =
        zeigeAnfrage abgAnfrageFahrtrichtungsAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageBahngeschwindigkeit g z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@ABahngeschwindigkeitMärklinPwmFließend {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@ABahngeschwindigkeitMärklinKonstanteSpannungFließend {}
        eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen anfrage@ABahngeschwindigkeitLegoPwmFließend {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@ABahngeschwindigkeitLegoKonstanteSpannungFließend {}
        eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
        {abgmAnfrageGeschwindigkeitsAnschlüsse}
        eingabe = zeigeAnfrageFehlgeschlagen abgmAnfrageGeschwindigkeitsAnschlüsse eingabe
    zeigeAnfrageFehlgeschlagen
        ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
        {abglAnfrageGeschwindigkeitsAnschlüsse}
        eingabe = zeigeAnfrageFehlgeschlagen abglAnfrageGeschwindigkeitsAnschlüsse eingabe
    zeigeAnfrageFehlgeschlagen
        ABahngeschwindigkeitFahrtrichtungsAnschluss {abgAnfrageFahrtrichtungsAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen abgAnfrageFahrtrichtungsAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageBahngeschwindigkeit g z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageBahngeschwindigkeit = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList unterstützteZugtypen
    zeigeAnfrageOptionen ABahngeschwindigkeitMärklin = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ [Pwm, KonstanteSpannung]
    zeigeAnfrageOptionen ABahngeschwindigkeitLego = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ [Pwm, KonstanteSpannung]
    zeigeAnfrageOptionen ABahngeschwindigkeitMärklinPwmFließend {} =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen ABahngeschwindigkeitMärklinKonstanteSpannungFließend {} =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen ABahngeschwindigkeitLegoPwmFließend {} =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen ABahngeschwindigkeitLegoKonstanteSpannungFließend {} =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen
        ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
        {abgmAnfrageGeschwindigkeitsAnschlüsse} =
        zeigeAnfrageOptionen abgmAnfrageGeschwindigkeitsAnschlüsse
    zeigeAnfrageOptionen
        ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
        {abglAnfrageGeschwindigkeitsAnschlüsse} =
        zeigeAnfrageOptionen abglAnfrageGeschwindigkeitsAnschlüsse
    zeigeAnfrageOptionen
        ABahngeschwindigkeitFahrtrichtungsAnschluss {abgAnfrageFahrtrichtungsAnschluss} =
        zeigeAnfrageOptionen abgAnfrageFahrtrichtungsAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrageZugtyp (AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit) where
    anfrageMärklin :: AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit 'AnfrageMärklin
    anfrageMärklin = AnfrageGeschwindigkeitNothing ABahngeschwindigkeitMärklin

    anfrageLego :: AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit 'AnfrageLego
    anfrageLego = AnfrageGeschwindigkeitNothing ABahngeschwindigkeitLego

instance forall g z. ( FixeGeschwindigkeitVariante (AngefragteGeschwindigkeitVariante g) ~ g
                     , FixerZugtyp (AngefragterZugtyp z) ~ z
                     ) => MitAnfrage (Bahngeschwindigkeit g z) where
    type AnfrageTyp (Bahngeschwindigkeit g z) =
        AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)

    -- | Eingabe einer 'Märklin'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)) (Bahngeschwindigkeit g z)
    anfrageAktualisieren AnfrageBahngeschwindigkeit EingabeToken {eingabe} = AFFehler eingabe
    anfrageAktualisieren ABahngeschwindigkeitMärklin EingabeToken {eingabe} = AFFehler eingabe
    anfrageAktualisieren ABahngeschwindigkeitLego EingabeToken {eingabe} = AFFehler eingabe
    anfrageAktualisieren ABahngeschwindigkeitMärklinPwm EingabeToken {eingabe} =
        AFZwischenwert $ ABahngeschwindigkeitMärklinPwmFließend eingabe
    anfrageAktualisieren ABahngeschwindigkeitMärklinKonstanteSpannung EingabeToken {eingabe} =
        AFZwischenwert $ ABahngeschwindigkeitMärklinKonstanteSpannungFließend eingabe
    anfrageAktualisieren ABahngeschwindigkeitLegoPwm EingabeToken {eingabe} =
        AFZwischenwert $ ABahngeschwindigkeitLegoPwmFließend eingabe
    anfrageAktualisieren ABahngeschwindigkeitLegoKonstanteSpannung EingabeToken {eingabe} =
        AFZwischenwert $ ABahngeschwindigkeitLegoKonstanteSpannungFließend eingabe
    anfrageAktualisieren ABahngeschwindigkeitMärklinPwmFließend {abgmpName} token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, nächsteAnfrage HIGH), (Lexer.LOW, nächsteAnfrage LOW)]
        where
            nächsteAnfrage :: Value -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageMärklin
            nächsteAnfrage abgmFließend =
                ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
                { abgmName = abgmpName
                , abgmFließend
                , abgmAnfrageGeschwindigkeitsAnschlüsse = AGeschwindigkeitsPin
                }
    anfrageAktualisieren ABahngeschwindigkeitMärklinKonstanteSpannungFließend {abgmkName} token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, nächsteAnfrage HIGH), (Lexer.LOW, nächsteAnfrage LOW)]
        where
            nächsteAnfrage
                :: Value -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageMärklin
            nächsteAnfrage abgmFließend =
                ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
                { abgmName = abgmkName
                , abgmFließend
                , abgmAnfrageGeschwindigkeitsAnschlüsse = AFahrstromAnschlüsseAnzahl
                }
    anfrageAktualisieren ABahngeschwindigkeitLegoPwmFließend {abglpName} token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, nächsteAnfrage HIGH), (Lexer.LOW, nächsteAnfrage LOW)]
        where
            nächsteAnfrage :: Value -> AnfrageBahngeschwindigkeit 'AnfragePwm 'AnfrageLego
            nächsteAnfrage abglFließend =
                ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
                { abglName = abglpName
                , abglFließend
                , abglAnfrageGeschwindigkeitsAnschlüsse = AGeschwindigkeitsPin
                }
    anfrageAktualisieren ABahngeschwindigkeitLegoKonstanteSpannungFließend {abglkName} token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, nächsteAnfrage HIGH), (Lexer.LOW, nächsteAnfrage LOW)]
        where
            nächsteAnfrage
                :: Value -> AnfrageBahngeschwindigkeit 'AnfrageKonstanteSpannung 'AnfrageLego
            nächsteAnfrage abglFließend =
                ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
                { abglName = abglkName
                , abglFließend
                , abglAnfrageGeschwindigkeitsAnschlüsse = AFahrstromAnschlüsseAnzahl
                }
    anfrageAktualisieren
        anfrage@ABahngeschwindigkeitMärklinGeschwindigkeitsAnschlüsse
        {abgmName, abgmFließend, abgmAnfrageGeschwindigkeitsAnschlüsse}
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren abgmAnfrageGeschwindigkeitsAnschlüsse token
        where
            anfrageAnschlussVerwenden
                :: AnfrageGeschwindigkeitsAnschlüsse g
                -> AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)
            anfrageAnschlussVerwenden anfrageGeschwindigkeitsAnschlüsse =
                anfrage
                { abglAnfrageGeschwindigkeitsAnschlüsse = anfrageGeschwindigkeitsAnschlüsse
                }

            anschlussVerwenden
                :: GeschwindigkeitsAnschlüsse g
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)) (Bahngeschwindigkeit g z)
            anschlussVerwenden bgGeschwindigkeitsAnschlüsse@GeschwindigkeitsPin {} =
                AFErgebnis
                    Bahngeschwindigkeit
                    { bgName = abgmName
                    , bgFließend = abgmFließend
                    , bgGeschwindigkeitsAnschlüsse
                    , bgFahrtrichtungsAnschluss = KeinExpliziterAnschluss
                    }
            anschlussVerwenden abgGeschwindigkeitsAnschlüsse@FahrstromAnschlüsse {} =
                AFZwischenwert
                    ABahngeschwindigkeitFahrtrichtungsAnschluss
                    { abgName = abgmName
                    , abgFließend = abgmFließend
                    , abgGeschwindigkeitsAnschlüsse
                    , abgAnfrageFahrtrichtungsAnschluss =
                          AUmdrehenAnschluss { umdrehenAnfrageAnschluss = AnfrageAnschluss }
                    }
    anfrageAktualisieren
        anfrage@ABahngeschwindigkeitLegoGeschwindigkeitsAnschlüsse
        {abglName, abglFließend, abglAnfrageGeschwindigkeitsAnschlüsse}
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren abglAnfrageGeschwindigkeitsAnschlüsse token
        where
            anfrageAnschlussVerwenden
                :: AnfrageGeschwindigkeitsAnschlüsse g
                -> AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)
            anfrageAnschlussVerwenden anfrageGeschwindigkeitsAnschlüsse =
                anfrage
                { abglAnfrageGeschwindigkeitsAnschlüsse = anfrageGeschwindigkeitsAnschlüsse
                }

            anschlussVerwenden
                :: GeschwindigkeitsAnschlüsse g
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)) (Bahngeschwindigkeit g z)
            anschlussVerwenden abgGeschwindigkeitsAnschlüsse =
                AFZwischenwert
                    ABahngeschwindigkeitFahrtrichtungsAnschluss
                    { abgName = abglName
                    , abgFließend = abglFließend
                    , abgGeschwindigkeitsAnschlüsse
                    , abgAnfrageFahrtrichtungsAnschluss = AFahrtrichtungsAnschluss
                          { fahrtrichtungsAnfrageAnschluss = AnfrageAnschluss
                          }
                    }
    anfrageAktualisieren
        anfrage@ABahngeschwindigkeitFahrtrichtungsAnschluss
        {abgName, abgFließend, abgGeschwindigkeitsAnschlüsse, abgAnfrageFahrtrichtungsAnschluss}
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren abgAnfrageFahrtrichtungsAnschluss token
        where
            anfrageAnschlussVerwenden
                :: AnfrageFahrtrichtungsAnschluss g z
                -> AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)
            anfrageAnschlussVerwenden anfrageFahrtrichtungsAnschluss =
                anfrage { abgAnfrageFahrtrichtungsAnschluss = anfrageFahrtrichtungsAnschluss }

            anschlussVerwenden
                :: FahrtrichtungsAnschluss g z
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)) (Bahngeschwindigkeit g z)
            anschlussVerwenden bgFahrtrichtungsAnschluss =
                AFErgebnis
                    Bahngeschwindigkeit
                    { bgName = abgName
                    , bgFließend = abgFließend
                    , bgGeschwindigkeitsAnschlüsse = abgGeschwindigkeitsAnschlüsse
                    , bgFahrtrichtungsAnschluss
                    }
