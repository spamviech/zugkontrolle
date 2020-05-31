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
  , GeschwindigkeitVarianteDummy(..)
  , ZugtypDummy(..)
  ) where

import Data.Foldable (Foldable(toList))
import qualified Data.List.NonEmpty as NonEmpty
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
                anfrageAnschluss = anfrage { fahrstromAnfrageAnschluss = anfrageAnschluss }

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
                anfrageAnschluss = anfrage { umdrehenAnfrageAnschluss = anfrageAnschluss }

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
                anfrageAnschluss = anfrage { fahrtrichtungsAnfrageAnschluss = anfrageAnschluss }

            anschlussVerwenden
                :: AnschlussEither
                -> AnfrageFortsetzung (AnfrageFahrtrichtungsAnschluss g 'Lego) (FahrtrichtungsAnschluss g 'Lego)
            anschlussVerwenden fahrtrichtungsAnschluss =
                AFErgebnis FahrtrichtungsAnschluss { fahrtrichtungsAnschluss }

-- | Dummy-Wert um einen GADT-Konstruktor auf einen Zugtyp festzulegen
data ZugtypDummy (z :: AnfrageZugtyp) where
    ZugtypDummyMärklin :: ZugtypDummy 'AnfrageMärklin
    ZugtypDummyLego :: ZugtypDummy 'AnfrageLego

deriving instance Eq (ZugtypDummy z)

deriving instance Ord (ZugtypDummy z)

deriving instance Show (ZugtypDummy z)

-- | Dummy-Wert um einen GADT-Konstruktor auf eine GeschwindigkeitVariante festzulegen.
data GeschwindigkeitVarianteDummy (g :: AnfrageGeschwindigkeitVariante) where
    GeschwindigkeitVarianteDummyPwm :: GeschwindigkeitVarianteDummy 'AnfragePwm
    GeschwindigkeitVarianteDummyKonstanteSpannung
        :: GeschwindigkeitVarianteDummy 'AnfrageKonstanteSpannung

deriving instance Eq (GeschwindigkeitVarianteDummy g)

deriving instance Ord (GeschwindigkeitVarianteDummy g)

deriving instance Show (GeschwindigkeitVarianteDummy g)

-- | Zusammenfassen von 'ZugtypDummy' und 'GeschwindigkeitVarianteDummy'.
type GeschwindigkeitZugtypDummy g z = (GeschwindigkeitVarianteDummy g, ZugtypDummy z)

-- | Unvollständige 'Bahngeschwindigkeit'.
data AnfrageBahngeschwindigkeit (g :: AnfrageGeschwindigkeitVariante) (z :: AnfrageZugtyp) where
    ABahngeschwindigkeitZugtyp
        :: AnfrageBahngeschwindigkeit 'AnfrageGeschwindigkeitVariante 'AnfrageZugtyp
    -- GeschwindigkeitVariante
    ABahngeschwindigkeitGeschwindigkeitVariante :: { abgZugtypDummy :: ZugtypDummy z }
        -> AnfrageBahngeschwindigkeit 'AnfrageGeschwindigkeitVariante z
    -- Name
    ABahngeschwindigkeitName :: { abgGeschwindigkeitZugtypDummy :: GeschwindigkeitZugtypDummy g z }
        -> AnfrageBahngeschwindigkeit g z
    -- Fließend
    ABahngeschwindigkeitFließend
        :: { abgGeschwindigkeitZugtypDummy :: GeschwindigkeitZugtypDummy g z, abgName :: Text }
        -> AnfrageBahngeschwindigkeit g z
    -- GeschwindigkeitsAnschlüsse
    ABahngeschwindigkeitGeschwindigkeitsAnschlüsse
        :: { abgGeschwindigkeitZugtypDummy :: GeschwindigkeitZugtypDummy g z
           , abgName :: Text
           , abgFließend :: Value
           , abgAnfrageGeschwindigkeitsAnschlüsse
                 :: AnfrageGeschwindigkeitsAnschlüsse (FixeGeschwindigkeitVariante g)
           } -> AnfrageBahngeschwindigkeit g z
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
    anzeige ABahngeschwindigkeitZugtyp = Language.bahngeschwindigkeit
    anzeige anfrage@ABahngeschwindigkeitGeschwindigkeitVariante {} =
        gewählterZugtyp anfrage <-> Language.bahngeschwindigkeit
    anzeige anfrage@ABahngeschwindigkeitName {} =
        gewählterZugtyp anfrage
        <-> gewählteGeschwindigkeitVariante anfrage
        <-> Language.bahngeschwindigkeit
    anzeige anfrage@ABahngeschwindigkeitFließend {abgName} =
        gewählterZugtyp anfrage
        <-> gewählteGeschwindigkeitVariante anfrage
        <-> Language.bahngeschwindigkeit <^> Language.name <=> abgName
    anzeige
        anfrage@ABahngeschwindigkeitGeschwindigkeitsAnschlüsse
        {abgName, abgFließend, abgAnfrageGeschwindigkeitsAnschlüsse} =
        gewählterZugtyp anfrage
        <-> gewählteGeschwindigkeitVariante anfrage
        <-> Language.bahngeschwindigkeit
        <^> Language.name
        <=> abgName
        <^> Language.fließend <=> abgFließend <^> abgAnfrageGeschwindigkeitsAnschlüsse
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
    zeigeAnfrage ABahngeschwindigkeitZugtyp = Language.zugtyp
    zeigeAnfrage ABahngeschwindigkeitGeschwindigkeitVariante {} = Language.geschwindigkeitVariante
    zeigeAnfrage ABahngeschwindigkeitName {} = Language.name
    zeigeAnfrage ABahngeschwindigkeitFließend {} = Language.fließendValue
    zeigeAnfrage
        ABahngeschwindigkeitGeschwindigkeitsAnschlüsse {abgAnfrageGeschwindigkeitsAnschlüsse} =
        zeigeAnfrage abgAnfrageGeschwindigkeitsAnschlüsse
    zeigeAnfrage ABahngeschwindigkeitFahrtrichtungsAnschluss {abgAnfrageFahrtrichtungsAnschluss} =
        zeigeAnfrage abgAnfrageFahrtrichtungsAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageBahngeschwindigkeit g z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@ABahngeschwindigkeitFließend {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        ABahngeschwindigkeitGeschwindigkeitsAnschlüsse {abgAnfrageGeschwindigkeitsAnschlüsse}
        eingabe = zeigeAnfrageFehlgeschlagen abgAnfrageGeschwindigkeitsAnschlüsse eingabe
    zeigeAnfrageFehlgeschlagen
        ABahngeschwindigkeitFahrtrichtungsAnschluss {abgAnfrageFahrtrichtungsAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen abgAnfrageFahrtrichtungsAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageBahngeschwindigkeit g z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen ABahngeschwindigkeitZugtyp = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList unterstützteZugtypen
    zeigeAnfrageOptionen ABahngeschwindigkeitGeschwindigkeitVariante {} =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) $ [Pwm, KonstanteSpannung]
    zeigeAnfrageOptionen ABahngeschwindigkeitFließend {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen
        ABahngeschwindigkeitGeschwindigkeitsAnschlüsse {abgAnfrageGeschwindigkeitsAnschlüsse} =
        zeigeAnfrageOptionen abgAnfrageGeschwindigkeitsAnschlüsse
    zeigeAnfrageOptionen
        ABahngeschwindigkeitFahrtrichtungsAnschluss {abgAnfrageFahrtrichtungsAnschluss} =
        zeigeAnfrageOptionen abgAnfrageFahrtrichtungsAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrageZugtyp (AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit) where
    anfrageMärklin :: AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit 'AnfrageMärklin
    anfrageMärklin =
        AnfrageGeschwindigkeitNothing
            ABahngeschwindigkeitGeschwindigkeitVariante { abgZugtypDummy = ZugtypDummyMärklin }

    anfrageLego :: AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit 'AnfrageLego
    anfrageLego =
        AnfrageGeschwindigkeitNothing
            ABahngeschwindigkeitGeschwindigkeitVariante { abgZugtypDummy = ZugtypDummyLego }

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
    anfrageAktualisieren ABahngeschwindigkeitZugtyp EingabeToken {eingabe} = AFFehler eingabe
    anfrageAktualisieren ABahngeschwindigkeitGeschwindigkeitVariante {} EingabeToken {eingabe} =
        AFFehler eingabe
    -- obige Konstruktoren werden wegen notwendigen Typ-Wechseln in Objekt-Instanz behandelt.
    -- siehe Zug/UI/Cmd/Parser/StreckenObjekt.hs
    anfrageAktualisieren
        ABahngeschwindigkeitName {abgGeschwindigkeitZugtypDummy}
        EingabeToken {eingabe} =
        AFZwischenwert
        $ ABahngeschwindigkeitFließend { abgGeschwindigkeitZugtypDummy, abgName = eingabe }
    anfrageAktualisieren
        ABahngeschwindigkeitFließend
        { abgGeschwindigkeitZugtypDummy =
              abgGeschwindigkeitZugtypDummy@(geschwindigkeitVarianteDummy, _zugtypDummy)
        , abgName}
        token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, nächsteAnfrage geschwindigkeitVarianteDummy HIGH)
            , (Lexer.LOW, nächsteAnfrage geschwindigkeitVarianteDummy LOW)]
        where
            nächsteAnfrage
                :: GeschwindigkeitVarianteDummy (AngefragteGeschwindigkeitVariante g)
                -> Value
                -> AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)
            nächsteAnfrage GeschwindigkeitVarianteDummyPwm abgFließend =
                ABahngeschwindigkeitGeschwindigkeitsAnschlüsse
                { abgGeschwindigkeitZugtypDummy
                , abgName
                , abgFließend
                , abgAnfrageGeschwindigkeitsAnschlüsse = AGeschwindigkeitsPin
                }
            nächsteAnfrage GeschwindigkeitVarianteDummyKonstanteSpannung abgFließend =
                ABahngeschwindigkeitGeschwindigkeitsAnschlüsse
                { abgGeschwindigkeitZugtypDummy
                , abgName
                , abgFließend
                , abgAnfrageGeschwindigkeitsAnschlüsse = AFahrstromAnschlüsseAnzahl
                }
    anfrageAktualisieren
        anfrage@ABahngeschwindigkeitGeschwindigkeitsAnschlüsse
        { abgGeschwindigkeitZugtypDummy
        , abgName
        , abgFließend
        , abgAnfrageGeschwindigkeitsAnschlüsse}
        token =
        (anschlussVerwenden abgGeschwindigkeitZugtypDummy, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren abgAnfrageGeschwindigkeitsAnschlüsse token
        where
            anfrageAnschlussVerwenden
                :: AnfrageGeschwindigkeitsAnschlüsse g
                -> AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)
            anfrageAnschlussVerwenden anfrageGeschwindigkeitsAnschlüsse =
                anfrage
                { abgAnfrageGeschwindigkeitsAnschlüsse = anfrageGeschwindigkeitsAnschlüsse
                }

            anschlussVerwenden
                :: GeschwindigkeitZugtypDummy (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)
                -> GeschwindigkeitsAnschlüsse g
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit (AngefragteGeschwindigkeitVariante g) (AngefragterZugtyp z)) (Bahngeschwindigkeit g z)
            anschlussVerwenden
                (GeschwindigkeitVarianteDummyPwm, ZugtypDummyMärklin)
                bgGeschwindigkeitsAnschlüsse@GeschwindigkeitsPin {} =
                AFErgebnis
                    Bahngeschwindigkeit
                    { bgName = abgName
                    , bgFließend = abgFließend
                    , bgGeschwindigkeitsAnschlüsse
                    , bgFahrtrichtungsAnschluss = KeinExpliziterAnschluss
                    }
            anschlussVerwenden
                (GeschwindigkeitVarianteDummyKonstanteSpannung, ZugtypDummyMärklin)
                abgGeschwindigkeitsAnschlüsse =
                AFZwischenwert
                    ABahngeschwindigkeitFahrtrichtungsAnschluss
                    { abgName
                    , abgFließend
                    , abgGeschwindigkeitsAnschlüsse
                    , abgAnfrageFahrtrichtungsAnschluss =
                          AUmdrehenAnschluss { umdrehenAnfrageAnschluss = AnfrageAnschluss }
                    }
            anschlussVerwenden
                (_GeschwindigkeitVarianteDummy, ZugtypDummyLego)
                abgGeschwindigkeitsAnschlüsse =
                AFZwischenwert
                    ABahngeschwindigkeitFahrtrichtungsAnschluss
                    { abgName
                    , abgFließend
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
