{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description: Parsen von 'StreckenObjekt'en
-}
module Zug.UI.Cmd.Parser.StreckenObjekt (
    -- * Hilfstypen
    AnfrageZugtyp(..), AnfrageZugtypEither(..),
    AnfrageAnschluss(..),
    -- * StreckenObjekte
    -- ** Bahngeschwindigkeit
    AnfrageBahngeschwindigkeit(..),
    -- ** Weiche
    AnfrageWeiche(..),
    -- ** Streckenabschnitt
    AnfrageStreckenabschnitt(..),
    -- ** Kupplung
    AnfrageKupplung(..),
    -- ** Wegstrecke
    AnfrageWegstrecke(..),
    -- ** Objekt
    AnfrageObjekt(..)) where

import Data.Semigroup (Semigroup(..))
import Data.String (IsString())
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (Bahngeschwindigkeit(..), Streckenabschnitt(..), Weiche(..), WeicheKlasse(..),
                    Kupplung(..), Wegstrecke(..), Anschluss(..), Value(..), alleValues,
                    PCF8574Port(..), PCF8574(..), PCF8574Variant(..), vonPinGpio)
import Zug.Klassen (Zugtyp(..), ZugtypEither(..), unterstützteZugtypen,
                    Richtung(..), unterstützteRichtungen)
import Zug.Language ((<^>), (<=>), (<->), showText, toBefehlsString)
import qualified Zug.Language as Language
import Zug.Objekt (Objekt, ObjektAllgemein(..))
import Zug.UI.Cmd.Lexer (EingabeToken(..), leeresToken)
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, unbekanntShowText, MitAnfrage(..),
                                AnfrageZugtyp(..), AnfrageZugtypEither(..), MitAnfrageZugtyp(..), anfrageAktualisierenZugtyp,
                                StatusAnfrageObjekt(..), wähleBefehl, wähleRichtung, wähleValue,
                                StatusAnfrageObjektZugtyp(..), ObjektZugtyp(..))
import Zug.UI.Cmd.Parser.Plan (AnfragePlan(..))

-- | Unvollständiger 'Anschluss'
data AnfrageAnschluss
    = AnfrageAnschluss
    | AnfrageAnschlussUnbekannt
        AnfrageAnschluss        -- ^ Anfrage
        Text                    -- ^ Eingabe
    | APin
    | APCF8574Port
    | APCF8574PortVariant
        PCF8574Variant          -- ^ Variante
    | APCF8574PortVariantA0
        PCF8574Variant          -- ^ Variante
        Value                   -- ^ a0
    | APCF8574PortVariantA0A1
        PCF8574Variant          -- ^ Variante
        Value                   -- ^ a0
        Value                   -- ^ a1
    | APCF8574PortVariantA0A1A2
        PCF8574Variant          -- ^ Variante
        Value                   -- ^ a0
        Value                   -- ^ a1
        Value                   -- ^ a2

instance Show AnfrageAnschluss where
    show :: AnfrageAnschluss -> String
    show
        AnfrageAnschluss
            = Language.anschluss
    show 
        (AnfrageAnschlussUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show 
        APin
            = unpack $ Language.anschluss <-> Language.pin
    show 
        APCF8574Port
            = unpack $ Language.anschluss <-> Language.pcf8574Port
    show
        (APCF8574PortVariant variante)
            = unpack $ Language.anschluss <-> Language.pcf8574Port
                <^> Language.variante <=> if (variante == VariantNormal)
                    then Language.normal
                    else Language.a
    show
        (APCF8574PortVariantA0 variante a0)
            = unpack $ Language.anschluss <-> Language.pcf8574Port
                <^> Language.variante <=> (if (variante == VariantNormal)
                    then Language.normal
                    else Language.a)
                <^> Language.a0 <=> showText a0
    show
        (APCF8574PortVariantA0A1 variante a0 a1)
            = unpack $ Language.anschluss <-> Language.pcf8574Port
                <^> Language.variante <=> (if (variante == VariantNormal)
                    then Language.normal
                    else Language.a)
                <^> Language.a0 <=> showText a0
                <^> Language.a1 <=> showText a1
    show
        (APCF8574PortVariantA0A1A2 variante a0 a1 a2)
            = unpack $ Language.anschluss <-> Language.pcf8574Port
                <^> Language.variante <=> (if (variante == VariantNormal)
                    then Language.normal
                    else Language.a)
                <^> Language.a0 <=> showText a0
                <^> Language.a1 <=> showText a1
                <^> Language.a2 <=> showText a2

instance Anfrage AnfrageAnschluss where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAnschluss -> s
    zeigeAnfrage
        AnfrageAnschluss
            = Language.anschluss
    zeigeAnfrage 
        (AnfrageAnschlussUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        APin
            = Language.pin
    zeigeAnfrage
        APCF8574Port
            = Language.pcf8574 <-> Language.variante
    zeigeAnfrage
        (APCF8574PortVariant _variante)
            = Language.a0
    zeigeAnfrage
        (APCF8574PortVariantA0 _variante _a0)
            = Language.a1
    zeigeAnfrage
        (APCF8574PortVariantA0A1 _variante _a0 _a1)
            = Language.a2
    zeigeAnfrage
        (APCF8574PortVariantA0A1A2 _variante _a0 _a1 _a2)
            = Language.port
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageAnschluss -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@APin
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(APCF8574PortVariant _variante)
        eingabe
            = zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(APCF8574PortVariantA0 _variante _a0)
        eingabe
            = zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(APCF8574PortVariantA0A1 _variante _a0 _a2)
        eingabe
            = zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(APCF8574PortVariantA0A1A2 _variante _a0 _a1 _a2)
        eingabe
            = zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAnschluss -> Maybe s
    zeigeAnfrageOptionen
        AnfrageAnschluss
            = Just $ toBefehlsString [Language.pin, Language.pcf8574Port]
    zeigeAnfrageOptionen
        (AnfrageAnschlussUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        APCF8574Port
            = Just $ toBefehlsString [Language.normal, Language.a]
    zeigeAnfrageOptionen
        (APCF8574PortVariant _variante)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (APCF8574PortVariantA0 _variante _a0)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (APCF8574PortVariantA0A1 _variante _a0 _a1)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

instance MitAnfrage Anschluss where
    type AnfrageTyp Anschluss = AnfrageAnschluss
    -- | Eingabe eines 'Anschluss'
    anfrageAktualisieren :: AnfrageAnschluss -> EingabeToken -> Either AnfrageAnschluss Anschluss
    anfrageAktualisieren
        AnfrageAnschluss
        token@EingabeToken {eingabe}
            = Left $ wähleBefehl token [
                (Lexer.Pin          , APin),
                (Lexer.PCF8574Port  , APCF8574Port)]
                $ AnfrageAnschlussUnbekannt AnfrageAnschluss eingabe
    anfrageAktualisieren 
        (AnfrageAnschlussUnbekannt anfrage _eingabe)
        _token
            = Left anfrage
    anfrageAktualisieren 
        APin
        EingabeToken {eingabe, ganzzahl}
            = case ganzzahl of
                (Just pin)
                    -> Right $ vonPinGpio pin
                Nothing
                    -> Left $ AnfrageAnschlussUnbekannt APin eingabe
    anfrageAktualisieren
        APCF8574Port
        token@EingabeToken {eingabe}
            = Left $ wähleBefehl token [
                (Lexer.A        , APCF8574PortVariant VariantA),
                (Lexer.Normal   , APCF8574PortVariant VariantNormal)]
                $ AnfrageAnschlussUnbekannt APCF8574Port eingabe
    anfrageAktualisieren
        anfrage@(APCF8574PortVariant variante)
        token@EingabeToken {eingabe}
            = Left $ case wähleValue token of
                (Just a0)
                    -> APCF8574PortVariantA0 variante a0
                Nothing
                    -> AnfrageAnschlussUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(APCF8574PortVariantA0 variante a0)
        token@EingabeToken {eingabe}
            = Left $ case wähleValue token of
                (Just a1)
                    -> APCF8574PortVariantA0A1 variante a0 a1
                Nothing
                    -> AnfrageAnschlussUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(APCF8574PortVariantA0A1 variante a0 a1)
        token@EingabeToken {eingabe}
            = Left $ case wähleValue token of
                (Just a2)
                    -> APCF8574PortVariantA0A1A2 variante a0 a1 a2
                Nothing
                    -> AnfrageAnschlussUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(APCF8574PortVariantA0A1A2 variant a0 a1 a2)
        EingabeToken {eingabe, ganzzahl}
            = case ganzzahl of
                (Just port)
                    -> Right $ AnschlussPCF8574Port $ PCF8574Port {
                        pcf8574 = PCF8574 {variant, a0, a1, a2},
                        port = fromIntegral port}
                Nothing
                    -> Left $ AnfrageAnschlussUnbekannt anfrage eingabe

-- | Unvollständige 'Bahngeschwindigkeit'
data AnfrageBahngeschwindigkeit (z :: AnfrageZugtyp) where
    AnfrageBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageZugtyp
    ABGUnbekannt :: {
        abgAnfrage :: (AnfrageBahngeschwindigkeit z),
        abgEingabe :: Text}
            -> AnfrageBahngeschwindigkeit z
    AMärklinBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitName :: {
        abgmName :: Text}
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNameFließend :: {
        abgmName :: Text,
        abgmFließend :: Value,
        abgmGeschwindigkeitsAnfrageAnschluss :: AnfrageAnschluss}
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    ALegoBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitName :: {
        abglName :: Text}
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitNameFließend :: {
        abglName :: Text,
        abglFließend :: Value,
        abglGeschwindigkeitsAnfrageAnschluss :: AnfrageAnschluss}
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitNameFließendGeschwindigkeit :: {
        abglName :: Text,
        abglFließend :: Value,
        abglGeschwindigkeitsAnschluss :: Anschluss,
        abglFahrtrichtungsAnfrageAnschluss :: AnfrageAnschluss}
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego

instance Show (AnfrageBahngeschwindigkeit z) where
    show :: AnfrageBahngeschwindigkeit z -> String
    show
        AnfrageBahngeschwindigkeit
            = Language.bahngeschwindigkeit
    show
        (ABGUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show
        AMärklinBahngeschwindigkeit
            = Language.märklin <-> Language.bahngeschwindigkeit
    show
        (AMärklinBahngeschwindigkeitName name)
            = unpack $ Language.märklin <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    show
        (AMärklinBahngeschwindigkeitNameFließend name fließend geschwindigkeitsAnschluss)
            = unpack $ Language.märklin <-> Language.bahngeschwindigkeit
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
                <^> Language.geschwindigkeit <-> Language.anschluss <=> showText geschwindigkeitsAnschluss
    show
        ALegoBahngeschwindigkeit
            = Language.lego <-> Language.bahngeschwindigkeit
    show
        (ALegoBahngeschwindigkeitName name)
            = unpack $ Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    show
        (ALegoBahngeschwindigkeitNameFließend name fließend geschwindigkeitsAnschluss)
            = unpack $ Language.lego <-> Language.bahngeschwindigkeit
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
                <^> Language.geschwindigkeit <-> Language.anschluss <=> showText geschwindigkeitsAnschluss
    show
        (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit name fließend geschwindigkeitsAnschluss fahrtrichtungsAnschluss)
            = unpack $ Language.lego <-> Language.bahngeschwindigkeit
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
                <^> Language.geschwindigkeit <-> Language.anschluss <=> showText geschwindigkeitsAnschluss
                <^> Language.fahrtrichtung <-> Language.anschluss <=> showText fahrtrichtungsAnschluss

instance Anfrage (AnfrageBahngeschwindigkeit z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit z -> s
    zeigeAnfrage
        AnfrageBahngeschwindigkeit
            = Language.zugtyp
    zeigeAnfrage
        ABGUnbekannt {abgAnfrage}
            = zeigeAnfrage abgAnfrage
    zeigeAnfrage
        AMärklinBahngeschwindigkeit
            = Language.name
    zeigeAnfrage
        AMärklinBahngeschwindigkeitName {}
            = Language.fließendValue
    zeigeAnfrage
        AMärklinBahngeschwindigkeitNameFließend {abgmGeschwindigkeitsAnfrageAnschluss}
            = zeigeAnfrage abgmGeschwindigkeitsAnfrageAnschluss
    zeigeAnfrage
        ALegoBahngeschwindigkeit
            = Language.name
    zeigeAnfrage
        ALegoBahngeschwindigkeitName {}
            = Language.fließendValue
    zeigeAnfrage
        ALegoBahngeschwindigkeitNameFließend {abglGeschwindigkeitsAnfrageAnschluss}
            = zeigeAnfrage abglGeschwindigkeitsAnfrageAnschluss
    zeigeAnfrage
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss}
            = zeigeAnfrage abglFahrtrichtungsAnfrageAnschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit z -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@AMärklinBahngeschwindigkeitName {}
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        AMärklinBahngeschwindigkeitNameFließend {abgmGeschwindigkeitsAnfrageAnschluss}
        eingabe
            = zeigeAnfrageFehlgeschlagen abgmGeschwindigkeitsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        anfrage@ALegoBahngeschwindigkeitName {}
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        ALegoBahngeschwindigkeitNameFließend {abglGeschwindigkeitsAnfrageAnschluss}
        eingabe
            = zeigeAnfrageFehlgeschlagen abglGeschwindigkeitsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss}
        eingabe
            = zeigeAnfrageFehlgeschlagen abglFahrtrichtungsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit z -> Maybe s
    zeigeAnfrageOptionen
        AnfrageBahngeschwindigkeit
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen
        AMärklinBahngeschwindigkeitName {}
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        AMärklinBahngeschwindigkeitNameFließend {abgmGeschwindigkeitsAnfrageAnschluss}
            = zeigeAnfrageOptionen abgmGeschwindigkeitsAnfrageAnschluss
    zeigeAnfrageOptionen
        ALegoBahngeschwindigkeitName {}
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        ALegoBahngeschwindigkeitNameFließend {abglGeschwindigkeitsAnfrageAnschluss}
            = zeigeAnfrageOptionen abglGeschwindigkeitsAnfrageAnschluss
    zeigeAnfrageOptionen
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss}
            = zeigeAnfrageOptionen abglFahrtrichtungsAnfrageAnschluss
    zeigeAnfrageOptionen
        ABGUnbekannt {abgAnfrage}
            = zeigeAnfrageOptionen abgAnfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

instance MitAnfrageZugtyp AnfrageBahngeschwindigkeit where
    anfrageUnbekannt :: AnfrageBahngeschwindigkeit z -> Text -> AnfrageBahngeschwindigkeit z
    anfrageUnbekannt = ABGUnbekannt
    anfrageMärklin :: AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    anfrageMärklin = AMärklinBahngeschwindigkeit
    anfrageLego :: AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    anfrageLego = ALegoBahngeschwindigkeit

instance MitAnfrage (Bahngeschwindigkeit 'Märklin) where
    type AnfrageTyp (Bahngeschwindigkeit 'Märklin) = AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    -- | Eingabe einer 'Märklin'-'Bahngeschwindigkeit'
    anfrageAktualisieren ::
        AnfrageTyp (Bahngeschwindigkeit 'Märklin) ->
        EingabeToken ->
            Either (AnfrageTyp (Bahngeschwindigkeit 'Märklin)) (Bahngeschwindigkeit 'Märklin)
    anfrageAktualisieren
        AMärklinBahngeschwindigkeit
        EingabeToken {eingabe}
            = Left $ AMärklinBahngeschwindigkeitName eingabe
    anfrageAktualisieren
        anfrage@AMärklinBahngeschwindigkeitName {abgmName}
        token@EingabeToken {eingabe}
            = Left $ wähleBefehl token [
                (Lexer.HIGH , AMärklinBahngeschwindigkeitNameFließend abgmName HIGH AnfrageAnschluss),
                (Lexer.LOW  , AMärklinBahngeschwindigkeitNameFließend abgmName LOW AnfrageAnschluss)]
                $ ABGUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(AMärklinBahngeschwindigkeitNameFließend bgmName bgmFließend geschwindigkeitsAnschluss)
        token
            = case anfrageAktualisieren geschwindigkeitsAnschluss token of
                (Left (AnfrageAnschlussUnbekannt abgmGeschwindigkeitsAnfrageAnschluss eingabe))
                    -> Left $ ABGUnbekannt (anfrage {abgmGeschwindigkeitsAnfrageAnschluss}) eingabe
                (Left abgmGeschwindigkeitsAnfrageAnschluss)
                    -> Left anfrage {abgmGeschwindigkeitsAnfrageAnschluss}
                (Right bgmGeschwindigkeitsAnschluss)
                    -> Right $ MärklinBahngeschwindigkeit {
                        bgmName,
                        bgmFließend,
                        bgmGeschwindigkeitsAnschluss}
    anfrageAktualisieren
        anfrage@ABGUnbekannt {}
        _token
            = Left anfrage

instance MitAnfrage (Bahngeschwindigkeit 'Lego) where
    type AnfrageTyp (Bahngeschwindigkeit 'Lego) = AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    -- | Eingabe einer 'Lego'-'Bahngeschwindigkeit'
    anfrageAktualisieren ::
        AnfrageTyp (Bahngeschwindigkeit 'Lego) ->
        EingabeToken ->
            Either (AnfrageTyp (Bahngeschwindigkeit 'Lego)) (Bahngeschwindigkeit 'Lego)
    anfrageAktualisieren
        ALegoBahngeschwindigkeit
        EingabeToken {eingabe}
            = Left $ ALegoBahngeschwindigkeitName eingabe
    anfrageAktualisieren
        anfrage@ALegoBahngeschwindigkeitName {abglName}
        token@EingabeToken {eingabe}
            = Left $ wähleBefehl token [
                (Lexer.HIGH , ALegoBahngeschwindigkeitNameFließend abglName HIGH AnfrageAnschluss),
                (Lexer.LOW  , ALegoBahngeschwindigkeitNameFließend abglName LOW AnfrageAnschluss)]
                $ ABGUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(ALegoBahngeschwindigkeitNameFließend name fließend geschwindigkeitsAnschluss)
        token
            = case anfrageAktualisieren geschwindigkeitsAnschluss token of
                (Left (AnfrageAnschlussUnbekannt abglGeschwindigkeitsAnfrageAnschluss eingabe))
                    -> Left $ ABGUnbekannt (anfrage {abglGeschwindigkeitsAnfrageAnschluss}) eingabe
                (Left abglGeschwindigkeitsAnfrageAnschluss)
                    -> Left $ anfrage {abglGeschwindigkeitsAnfrageAnschluss}
                (Right anschluss)
                    -> Left $ ALegoBahngeschwindigkeitNameFließendGeschwindigkeit name fließend anschluss AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
            bglName
            bglFließend
            bglGeschwindigkeitsAnschluss
            fahrtrichtungsAnschluss)
        token
        = case anfrageAktualisieren fahrtrichtungsAnschluss token of
            (Left (AnfrageAnschlussUnbekannt abglFahrtrichtungsAnfrageAnschluss eingabe1))
                -> Left $ ABGUnbekannt anfrage {abglFahrtrichtungsAnfrageAnschluss} eingabe1
            (Left abglFahrtrichtungsAnfrageAnschluss)
                -> Left $ anfrage {abglFahrtrichtungsAnfrageAnschluss}
            (Right bglFahrtrichtungsAnschluss)
                -> Right $ LegoBahngeschwindigkeit {
                        bglName,
                        bglFließend,
                        bglGeschwindigkeitsAnschluss,
                        bglFahrtrichtungsAnschluss}
    anfrageAktualisieren
        anfrage@(ABGUnbekannt _anfrage _eingabe)
        _token
            = Left anfrage

-- | Unvollständiger 'Streckenabschnitt'
data AnfrageStreckenabschnitt
    = AnfrageStreckenabschnitt
    | ASTUnbekannt {
        astAnfrage :: AnfrageStreckenabschnitt,
        astEingabe :: Text}
    | AStreckenabschnittName {
        astName :: Text}
    | AStreckenabschnittNameFließend {
        astName ::Text,
        astFließend :: Value,
        astStromAnfrageAnschluss :: AnfrageAnschluss}

instance Show AnfrageStreckenabschnitt where
    show :: AnfrageStreckenabschnitt -> String
    show
        AnfrageStreckenabschnitt
            = Language.streckenabschnitt
    show
        (ASTUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show
        (AStreckenabschnittName name)
            = unpack $ Language.streckenabschnitt <^> Language.name <=> name
    show
        (AStreckenabschnittNameFließend name fließend stromAnschluss)
            = unpack $ Language.streckenabschnitt
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
                <^> Language.strom <-> Language.anschluss <=> showText stromAnschluss

instance Anfrage AnfrageStreckenabschnitt where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> s
    zeigeAnfrage
        AnfrageStreckenabschnitt
            = Language.name
    zeigeAnfrage
        ASTUnbekannt {astAnfrage}
            = zeigeAnfrage astAnfrage
    zeigeAnfrage
        AStreckenabschnittName {}
            = Language.fließendValue
    zeigeAnfrage
        AStreckenabschnittNameFließend {astStromAnfrageAnschluss}
            = zeigeAnfrage astStromAnfrageAnschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@AStreckenabschnittName {}
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        AStreckenabschnittNameFließend {astStromAnfrageAnschluss}
        eingabe
            = zeigeAnfrageFehlgeschlagen astStromAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> Maybe s
    zeigeAnfrageOptionen
        AStreckenabschnittName {}
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        AStreckenabschnittNameFließend {astStromAnfrageAnschluss}
            = zeigeAnfrageOptionen astStromAnfrageAnschluss
    zeigeAnfrageOptionen
        ASTUnbekannt {astAnfrage}
            = zeigeAnfrageOptionen astAnfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

instance MitAnfrage Streckenabschnitt where
    type AnfrageTyp Streckenabschnitt = AnfrageStreckenabschnitt
        -- | Eingabe eines Streckenabschnitts
    anfrageAktualisieren ::
        AnfrageStreckenabschnitt ->
            EingabeToken ->
                Either AnfrageStreckenabschnitt Streckenabschnitt
    anfrageAktualisieren
        AnfrageStreckenabschnitt
        EingabeToken {eingabe}
            = Left $ AStreckenabschnittName eingabe
    anfrageAktualisieren
        anfrage@(AStreckenabschnittName name)
        token@EingabeToken {eingabe}
            = Left $ wähleBefehl token [
                (Lexer.HIGH , AStreckenabschnittNameFließend name HIGH AnfrageAnschluss),
                (Lexer.LOW  , AStreckenabschnittNameFließend name LOW AnfrageAnschluss)]
                $ ASTUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(AStreckenabschnittNameFließend stName stFließend stromAnschluss)
        token
            = case anfrageAktualisieren stromAnschluss token of
                (Left (AnfrageAnschlussUnbekannt astStromAnfrageAnschluss eingabe))
                    -> Left $ ASTUnbekannt (anfrage {astStromAnfrageAnschluss}) eingabe
                (Left astStromAnfrageAnschluss)
                    -> Left $ anfrage {astStromAnfrageAnschluss}
                (Right stromAnschluss)
                    -> Right $ Streckenabschnitt {
                        stName,
                        stFließend,
                        stromAnschluss}
    anfrageAktualisieren
        anfrage@(ASTUnbekannt _anfrage _eingabe)
        _token
            = Left anfrage

-- | Unvollständige 'Weiche'
data AnfrageWeiche (z :: AnfrageZugtyp) where
    AnfrageWeiche
        :: AnfrageWeiche 'AnfrageZugtyp
    AWEUnbekannt :: {
        aweAnfrage :: AnfrageWeiche z,
        aweEingabe :: Text}
            -> AnfrageWeiche z
    AMärklinWeiche
        :: AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheName :: {
        awemName :: Text}
            -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließend :: {
        awemName :: Text,
        awemFließend :: Value}
            -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließendAnzahl :: {
        awemName :: Text,
        awemFließend :: Value,
        awemAnzahl :: Natural,
        awemRichtungsAnschlüsse :: [(Richtung, Anschluss)]}
            -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließendAnzahlRichtung  :: {
        awemName :: Text,
        awemFließend :: Value,
        awemAnzahl :: Natural,
        awemRichtungsAnschlüsse :: [(Richtung, Anschluss)],
        awemRichtung :: Richtung,
        awemAnfrageAnschluss :: AnfrageAnschluss}
            -> AnfrageWeiche 'AnfrageZugtypMärklin
    ALegoWeiche
        :: AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheName :: {
        awelName :: Text}
            -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließend :: {
        awelName :: Text,
        awelFließend :: Value}
            -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließendRichtung1 :: {
        awelName :: Text,
        awelFließend :: Value,
        awelRichtung1 :: Richtung}
            -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließendRichtungen :: {
        awelName :: Text,
        awelFließend :: Value,
        awelRichtung1 :: Richtung,
        awelRichtung2 :: Richtung,
        awelRichtungsAnfrageAnschluss :: AnfrageAnschluss}
            -> AnfrageWeiche 'AnfrageZugtypLego

instance Show (AnfrageWeiche z) where
    show :: AnfrageWeiche z -> String
    show
        AnfrageWeiche
            = Language.weiche
    show
        (AWEUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show
        AMärklinWeiche
            = Language.märklin <-> Language.weiche
    show
        (AMärklinWeicheName name)
            = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name
    show
        (AMärklinWeicheNameFließend name fließend)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
    show
        (AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> Language.erwartet Language.richtungen <=> showText anzahl
                <^> showText acc
    show
        (AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung anschluss)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> Language.erwartet Language.richtungen <=> showText anzahl
                <^> showText acc
                <^> Language.richtung <=> showText richtung
                <^> Language.anschluss <=> showText anschluss
    show
        ALegoWeiche
            = Language.lego <-> Language.weiche
    show
        (ALegoWeicheName name)
            = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name
    show
        (ALegoWeicheNameFließend name fließend)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
    show
        (ALegoWeicheNameFließendRichtung1 name fließend richtung1)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> showText richtung1
    show
        (ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2 anschluss)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> showText richtung1
                <^> showText richtung2
                <^> Language.richtung <-> Language.anschluss <=> showText anschluss

instance Anfrage (AnfrageWeiche z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageWeiche z -> s
    zeigeAnfrage
        AnfrageWeiche
            = Language.zugtyp
    zeigeAnfrage
        AWEUnbekannt {aweAnfrage}
            = zeigeAnfrage aweAnfrage
    zeigeAnfrage
        AMärklinWeiche
            = Language.name
    zeigeAnfrage
        (AMärklinWeicheName _name)
            = Language.fließendValue
    zeigeAnfrage
        (AMärklinWeicheNameFließend _name _fließend)
            = Language.anzahl Language.richtungen
    zeigeAnfrage
        (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc)
            = Language.richtung
    zeigeAnfrage
        AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss}
            = zeigeAnfrage awemAnfrageAnschluss
    zeigeAnfrage
        ALegoWeiche
            = Language.name
    zeigeAnfrage
        ALegoWeicheName {}
            = Language.fließendValue
    zeigeAnfrage
        ALegoWeicheNameFließend {}
            = Language.richtung
    zeigeAnfrage
        ALegoWeicheNameFließendRichtung1 {}
            = Language.richtung
    zeigeAnfrage
        ALegoWeicheNameFließendRichtungen {awelRichtungsAnfrageAnschluss}
            = zeigeAnfrage awelRichtungsAnfrageAnschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageWeiche z -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AMärklinWeicheNameFließend _name _fließend)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss}
        eingabe
            = zeigeAnfrageFehlgeschlagen awemAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        ALegoWeicheNameFließendRichtungen {awelRichtungsAnfrageAnschluss}
        eingabe
            = zeigeAnfrageFehlgeschlagen awelRichtungsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageWeiche z -> Maybe s
    zeigeAnfrageOptionen
        AnfrageWeiche
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen
        (AWEUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        (AMärklinWeicheName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss}
            = zeigeAnfrageOptionen awemAnfrageAnschluss
    zeigeAnfrageOptionen
        (ALegoWeicheName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (ALegoWeicheNameFließend _name _fließend)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        ALegoWeicheNameFließendRichtungen {awelRichtungsAnfrageAnschluss}
            = zeigeAnfrageOptionen awelRichtungsAnfrageAnschluss
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

instance MitAnfrage (Weiche 'Märklin) where
    type AnfrageTyp (Weiche 'Märklin) = AnfrageWeiche 'AnfrageZugtypMärklin
    -- | Eingabe einer 'Märklin'-'Weiche'
    anfrageAktualisieren ::
        AnfrageTyp (Weiche 'Märklin) ->
        EingabeToken ->
            Either (AnfrageTyp (Weiche 'Märklin)) (Weiche 'Märklin)
    anfrageAktualisieren
        AMärklinWeiche
        EingabeToken {eingabe}
            = Left $ AMärklinWeicheName eingabe
    anfrageAktualisieren
        anfrage@(AMärklinWeicheName name)
        token@EingabeToken {eingabe}
            = Left $ wähleBefehl token [
                (Lexer.HIGH , AMärklinWeicheNameFließend name HIGH),
                (Lexer.LOW  , AMärklinWeicheNameFließend name LOW)]
                $ AWEUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(AMärklinWeicheNameFließend name fließend)
        EingabeToken {eingabe, ganzzahl}
            = case ganzzahl of
                Nothing
                    -> Left $ AWEUnbekannt anfrage eingabe
                (Just anzahl)
                    -> Left $ AMärklinWeicheNameFließendAnzahl name fließend anzahl []
    anfrageAktualisieren
        anfrage@(AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)
        token@EingabeToken {eingabe}
            = Left $ case wähleRichtung token of
                Nothing
                    -> AWEUnbekannt anfrage eingabe
                (Just richtung)
                    -> AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(AMärklinWeicheNameFließendAnzahlRichtung wemName wemFließend anzahl acc richtung anfrageAnschluss)
        token
            = case anfrageAktualisieren anfrageAnschluss token of
                (Left (AnfrageAnschlussUnbekannt awemAnfrageAnschluss eingabe))
                    -> Left $ AWEUnbekannt (anfrage {awemAnfrageAnschluss}) eingabe
                (Left awemAnfrageAnschluss)
                    -> Left anfrage {awemAnfrageAnschluss}
                (Right anschluss)
                    | anzahl > 1
                        -> Left $ AMärklinWeicheNameFließendAnzahl
                            wemName
                            wemFließend
                            (pred anzahl)
                            ((richtung, anschluss) : acc)
                    | otherwise
                        -> Right MärklinWeiche {
                            wemName,
                            wemFließend,
                            wemRichtungsAnschlüsse = (richtung, anschluss) :| acc}
    anfrageAktualisieren
        anfrage@(AWEUnbekannt _anfrage _eingabe)
        _token
            = Left anfrage

instance MitAnfrage (Weiche 'Lego) where
    type AnfrageTyp (Weiche 'Lego) = AnfrageWeiche 'AnfrageZugtypLego
    -- | Eingabe einer 'Lego'-'Weiche'
    anfrageAktualisieren ::
        AnfrageTyp (Weiche 'Lego) ->
        EingabeToken ->
            Either (AnfrageTyp (Weiche 'Lego)) (Weiche 'Lego)
    anfrageAktualisieren
        ALegoWeiche
        EingabeToken {eingabe}
            = Left $ ALegoWeicheName eingabe
    anfrageAktualisieren
        anfrage@(ALegoWeicheName name)
        token@EingabeToken {eingabe}
            = Left $ wähleBefehl token [
                (Lexer.HIGH , ALegoWeicheNameFließend name HIGH),
                (Lexer.LOW  , ALegoWeicheNameFließend name LOW)]
                $ AWEUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(ALegoWeicheNameFließend name fließend)
        token@EingabeToken {eingabe}
            = Left $ case wähleRichtung token of
                Nothing
                    -> AWEUnbekannt anfrage eingabe
                (Just richtung1)
                    -> ALegoWeicheNameFließendRichtung1 name fließend richtung1
    anfrageAktualisieren
        anfrage@(ALegoWeicheNameFließendRichtung1 name fließend richtung1)
        token@EingabeToken {eingabe}
            = Left $ case wähleRichtung token of
                Nothing
                    -> AWEUnbekannt anfrage eingabe
                (Just richtung2)
                    -> ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2 AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(ALegoWeicheNameFließendRichtungen welName welFließend richtung1 richtung2 anfrageAnschluss)
        token
            = case anfrageAktualisieren anfrageAnschluss token of
                (Left (AnfrageAnschlussUnbekannt awelRichtungsAnfrageAnschluss eingabe))
                    -> Left $ AWEUnbekannt (anfrage {awelRichtungsAnfrageAnschluss}) eingabe
                (Left awelRichtungsAnfrageAnschluss)
                    -> Left anfrage {awelRichtungsAnfrageAnschluss}
                (Right welRichtungsAnschluss)
                    -> Right $ LegoWeiche {
                        welName,
                        welFließend,
                        welRichtungsAnschluss,
                        welRichtungen = (richtung1,richtung2)}
    anfrageAktualisieren
        anfrage@(AWEUnbekannt _anfrage _eingabe)
        _token
            = Left anfrage

instance MitAnfrageZugtyp AnfrageWeiche where
    anfrageUnbekannt :: AnfrageWeiche z -> Text -> AnfrageWeiche z
    anfrageUnbekannt = AWEUnbekannt
    anfrageMärklin :: AnfrageWeiche 'AnfrageZugtypMärklin
    anfrageMärklin = AMärklinWeiche
    anfrageLego :: AnfrageWeiche 'AnfrageZugtypLego
    anfrageLego = ALegoWeiche

-- | Unvollständige 'Kupplung'
data AnfrageKupplung
    = AnfrageKupplung
    | AKUUnbekannt {
        akuAnfrage ::AnfrageKupplung,
        akuEingabe ::Text}
    | AKupplungName {
        akuName :: Text}
    | AKupplungNameFließend {
        akuName ::Text,
        akuFließend ::Value,
        akuKupplungsAnfrageAnschluss :: AnfrageAnschluss}

instance Show AnfrageKupplung where
    show :: AnfrageKupplung -> String
    show 
        AnfrageKupplung
            = Language.kupplung
    show 
        (AKUUnbekannt anfrage eingabe)
            = unpack $ unbekanntShowText anfrage eingabe
    show 
        (AKupplungName name)
            = unpack $ Language.kupplung <^> Language.name <=> name
    show 
        (AKupplungNameFließend name fließend anschluss)
            = unpack $ Language.kupplung <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
                <^> Language.kupplung <-> Language.anschluss <=> showText anschluss

instance Anfrage AnfrageKupplung where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageKupplung -> s
    zeigeAnfrage
        AnfrageKupplung
            = Language.name
    zeigeAnfrage
        AKUUnbekannt {akuAnfrage}
            = zeigeAnfrage akuAnfrage
    zeigeAnfrage
        AKupplungName {}
            = Language.fließendValue
    zeigeAnfrage
        AKupplungNameFließend {akuKupplungsAnfrageAnschluss}
            = zeigeAnfrage akuKupplungsAnfrageAnschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageKupplung -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@AKupplungName {}
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        AKupplungNameFließend {akuKupplungsAnfrageAnschluss}
        eingabe
            = zeigeAnfrageFehlgeschlagen akuKupplungsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageKupplung -> Maybe s
    zeigeAnfrageOptionen
        AKupplungName {}
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        AKupplungNameFließend {akuKupplungsAnfrageAnschluss}
            = zeigeAnfrageOptionen akuKupplungsAnfrageAnschluss
    zeigeAnfrageOptionen
        AKUUnbekannt {akuAnfrage}
            = zeigeAnfrageOptionen akuAnfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

instance MitAnfrage Kupplung where
    type AnfrageTyp Kupplung = AnfrageKupplung
    -- | Eingabe einer Kupplung
    anfrageAktualisieren :: AnfrageKupplung -> EingabeToken -> Either AnfrageKupplung Kupplung
    anfrageAktualisieren
        AnfrageKupplung
        EingabeToken {eingabe}
            = Left $ AKupplungName eingabe
    anfrageAktualisieren
        anfrage@(AKupplungName name)
        token@EingabeToken {eingabe}
            = Left $ wähleBefehl token [
                (Lexer.HIGH , AKupplungNameFließend name HIGH AnfrageAnschluss),
                (Lexer.LOW  , AKupplungNameFließend name LOW AnfrageAnschluss)]
                $ AKUUnbekannt anfrage eingabe
    anfrageAktualisieren
        anfrage@(AKupplungNameFließend kuName kuFließend anfrageAnschluss)
        token
            = case anfrageAktualisieren anfrageAnschluss token of
                (Left (AnfrageAnschlussUnbekannt akuKupplungsAnfrageAnschluss eingabe))
                    -> Left $ AKUUnbekannt (anfrage {akuKupplungsAnfrageAnschluss}) eingabe
                (Left akuKupplungsAnfrageAnschluss)
                    -> Left anfrage {akuKupplungsAnfrageAnschluss}
                (Right kupplungsAnschluss)
                    -> Right $ Kupplung {
                        kuName,
                        kuFließend,
                        kupplungsAnschluss}
    anfrageAktualisieren
        anfrage@AKUUnbekannt {}
        _token
            = Left anfrage

type family FixerZugtyp (z :: AnfrageZugtyp) :: Zugtyp

type instance FixerZugtyp 'AnfrageZugtypMärklin = 'Märklin
type instance FixerZugtyp 'AnfrageZugtypLego = 'Lego

-- | Unvollständige 'Wegstrecke'
data AnfrageWegstrecke (z :: AnfrageZugtyp) where
    AnfrageWegstreckeZugtyp :: AnfrageWegstrecke 'AnfrageZugtyp
    AnfrageWegstrecke :: AnfrageWegstrecke z
    AWSUnbekannt :: {
        awsAnfrage :: AnfrageWegstrecke z,
        awsEingabe :: Text}
            -> AnfrageWegstrecke z
    AWegstreckeName :: {
        awsName :: Text}
            -> AnfrageWegstrecke z
    AWegstreckeNameAnzahl :: {
        awsAkkumulator :: Wegstrecke (FixerZugtyp z),
        awsAnzahl :: Natural}
            -> AnfrageWegstrecke z
    AWegstreckeNameAnzahlWeicheRichtung :: {
        awsAkkumulator :: Wegstrecke (FixerZugtyp z),
        awsAnzahl :: Natural,
        awsWeiche :: Weiche (FixerZugtyp z)}
            -> AnfrageWegstrecke z
    AWSStatusAnfrage :: {
        awsStatusAnfrageKonstruktor :: EingabeToken -> StatusAnfrageObjektZugtyp (FixerZugtyp z),
        awsEitherKonstruktor ::
            Either
                (ObjektZugtyp (FixerZugtyp z) -> (AnfrageWegstrecke z))
                (ObjektZugtyp (FixerZugtyp z) -> (Wegstrecke (FixerZugtyp z)))}
            -> AnfrageWegstrecke z
    AWegstreckeMStatus :: {
        awsStatusAnfrage :: StatusAnfrageObjektZugtyp (FixerZugtyp z),
        awsEitherKonstruktor ::
            Either
                (ObjektZugtyp (FixerZugtyp z) -> (AnfrageWegstrecke z))
                (ObjektZugtyp (FixerZugtyp z) -> (Wegstrecke (FixerZugtyp z)))}
            -> AnfrageWegstrecke z

instance Show (AnfrageWegstrecke z) where
    show :: AnfrageWegstrecke z -> String
    show
        (AWSUnbekannt aWegstrecke eingabe)
            = unpack $ unbekanntShowText aWegstrecke eingabe
    show
        AnfrageWegstreckeZugtyp
            = unpack $ Language.wegstrecke
    show
        AnfrageWegstrecke
            = unpack $ Language.wegstrecke
    show
        (AWegstreckeName name)
            = unpack $ Language.wegstrecke <^> Language.name <=> name
    show
        (AWegstreckeNameAnzahl acc anzahl)
            = unpack $ Language.wegstrecke
                <^> showText acc
                <^> Language.anzahl Language.wegstreckenElemente <=> showText anzahl
    show
        (AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche)
            = unpack $ Language.wegstrecke
                <^> showText acc
                <^> Language.anzahl Language.wegstreckenElemente <=> showText anzahl
                <^> showText weiche
    show
        AWSStatusAnfrage {awsStatusAnfrageKonstruktor}
            = Language.wegstreckenElement <^> showText (awsStatusAnfrageKonstruktor leeresToken)
    show
        AWegstreckeMStatus {awsStatusAnfrage}
            = Language.wegstrecke <^> showText awsStatusAnfrage

instance Anfrage (AnfrageWegstrecke z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageWegstrecke z -> s
    zeigeAnfrage
        AWSUnbekannt {awsAnfrage}
            = zeigeAnfrage awsAnfrage
    zeigeAnfrage
        AnfrageWegstreckeZugtyp
            = Language.zugtyp
    zeigeAnfrage
        AnfrageWegstrecke
            = Language.name
    zeigeAnfrage
        AWegstreckeName {}
            = Language.anzahl Language.wegstreckenElemente
    zeigeAnfrage
        AWegstreckeNameAnzahl {}
            = Language.wegstreckenElement
    zeigeAnfrage
        AWegstreckeNameAnzahlWeicheRichtung {}
            = Language.richtung
    zeigeAnfrage
        AWSStatusAnfrage {awsStatusAnfrageKonstruktor}
            = zeigeAnfrage $ awsStatusAnfrageKonstruktor leeresToken
    zeigeAnfrage
        AWegstreckeMStatus {awsStatusAnfrage}
            = zeigeAnfrage awsStatusAnfrage
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageWegstrecke z -> Maybe s
    zeigeAnfrageOptionen
        AWSUnbekannt {awsAnfrage}
            = zeigeAnfrageOptionen awsAnfrage
    zeigeAnfrageOptionen
        AnfrageWegstreckeZugtyp
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen
        AWegstreckeNameAnzahl {}
            = Just $ toBefehlsString Language.befehlWegstreckenElemente
    zeigeAnfrageOptionen
        AWegstreckeNameAnzahlWeicheRichtung {}
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        AWSStatusAnfrage {awsStatusAnfrageKonstruktor}
            = zeigeAnfrageOptionen $ awsStatusAnfrageKonstruktor leeresToken
    zeigeAnfrageOptionen
        AWegstreckeMStatus {awsStatusAnfrage}
            = zeigeAnfrageOptionen awsStatusAnfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Bekannte Teil-Typen einer 'Wegstrecke'
data AnfrageWegstreckenElement
    = AWSEUnbekannt
        Text
    | AWSEWeiche
    | AWSEBahngeschwindigkeit
    | AWSEStreckenabschnitt
    | AWSEKupplung

instance MitAnfrage (Wegstrecke 'Märklin) where
    type AnfrageTyp (Wegstrecke 'Märklin) = AnfrageWegstrecke 'AnfrageZugtypMärklin
    anfrageAktualisieren ::
        AnfrageWegstrecke 'AnfrageZugtypMärklin ->
        EingabeToken
            -> Either (AnfrageWegstrecke 'AnfrageZugtypMärklin) (Wegstrecke 'Märklin)
    anfrageAktualisieren = anfrageWegstreckeAktualisieren

instance MitAnfrage (Wegstrecke 'Lego) where
    type AnfrageTyp (Wegstrecke 'Lego) = AnfrageWegstrecke 'AnfrageZugtypLego
    anfrageAktualisieren ::
        AnfrageWegstrecke 'AnfrageZugtypLego ->
        EingabeToken
            -> Either (AnfrageWegstrecke 'AnfrageZugtypLego) (Wegstrecke 'Lego)
    anfrageAktualisieren = anfrageWegstreckeAktualisieren

-- | Eingabe einer Wegstrecke
anfrageWegstreckeAktualisieren :: AnfrageWegstrecke z -> EingabeToken -> Either (AnfrageWegstrecke z) (Wegstrecke (FixerZugtyp z))
anfrageWegstreckeAktualisieren
    anfrage@AnfrageWegstreckeZugtyp
    _token
        = Left anfrage
anfrageWegstreckeAktualisieren
    anfrage@AWegstreckeMStatus {}
    _token
        = Left anfrage
anfrageWegstreckeAktualisieren
    AnfrageWegstrecke
    EingabeToken {eingabe}
        = Left $ AWegstreckeName eingabe
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeName wsName)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AWSUnbekannt anfrage eingabe
            (Just anzahl)
                -> Left $ AWegstreckeNameAnzahl
                    Wegstrecke {
                        wsName,
                        wsBahngeschwindigkeiten = [],
                        wsStreckenabschnitte = [],
                        wsWeichenRichtungen = [],
                        wsKupplungen = []}
                    anzahl
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeNameAnzahl acc anzahl)
    token
        = Left $ case anfrageWegstreckenElement token of
            AWSEWeiche
                -> AWSStatusAnfrage SAOZWeiche $ Left $ anfrageWeicheAnhängen anfrage
            AWSEBahngeschwindigkeit
                -> AWSStatusAnfrage SAOZBahngeschwindigkeit $ eitherObjektAnhängen acc
            AWSEStreckenabschnitt
                -> AWSStatusAnfrage SAOZStreckenabschnitt $ eitherObjektAnhängen acc
            AWSEKupplung
                -> AWSStatusAnfrage SAOZKupplung $ eitherObjektAnhängen acc
            (AWSEUnbekannt eingabe)
                -> AWSUnbekannt anfrage eingabe
    where
        anfrageWegstreckenElement :: EingabeToken -> AnfrageWegstreckenElement
        anfrageWegstreckenElement token@EingabeToken {eingabe} = wähleBefehl token [
            (Lexer.Weiche                , AWSEWeiche),
            (Lexer.Bahngeschwindigkeit   , AWSEBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , AWSEStreckenabschnitt),
            (Lexer.Kupplung              , AWSEKupplung)]
            $ AWSEUnbekannt eingabe
        eitherObjektAnhängen ::
            Wegstrecke (FixerZugtyp z) ->
                Either
                    (ObjektZugtyp (FixerZugtyp z) -> (AnfrageWegstrecke z))
                    (ObjektZugtyp (FixerZugtyp z) -> (Wegstrecke (FixerZugtyp z)))
        eitherObjektAnhängen
            wegstrecke
                | anzahl > 1
                    = Left $ anfrageObjektAnhängen wegstrecke
                | otherwise
                    = Right $ objektAnhängen wegstrecke
        objektAnhängen :: Wegstrecke z -> ObjektZugtyp z -> Wegstrecke z
        objektAnhängen
            wegstrecke@Wegstrecke {wsBahngeschwindigkeiten}
            (OZBahngeschwindigkeit bahngeschwindigkeit)
                = wegstrecke {wsBahngeschwindigkeiten = bahngeschwindigkeit : wsBahngeschwindigkeiten}
        objektAnhängen
            wegstrecke@Wegstrecke {wsStreckenabschnitte}
            (OZStreckenabschnitt streckenabschnitt)
                = wegstrecke {wsStreckenabschnitte = streckenabschnitt : wsStreckenabschnitte}
        objektAnhängen
            wegstrecke@Wegstrecke {wsKupplungen}
            (OZKupplung kupplung)
                = wegstrecke {wsKupplungen = kupplung : wsKupplungen}
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        objektAnhängen
            wegstrecke
            _objekt
                = wegstrecke
        anfrageObjektAnhängen :: Wegstrecke (FixerZugtyp z) -> ObjektZugtyp (FixerZugtyp z) -> AnfrageWegstrecke z
        anfrageObjektAnhängen
            wegstrecke
            objekt
                = AWegstreckeNameAnzahl (objektAnhängen wegstrecke objekt) $ pred anzahl
        anfrageWeicheAnhängen :: AnfrageWegstrecke z -> ObjektZugtyp (FixerZugtyp z) -> AnfrageWegstrecke z
        anfrageWeicheAnhängen
            (AWegstreckeNameAnzahl wegstrecke anzahl)
            (OZWeiche weiche)
                = AWegstreckeNameAnzahlWeicheRichtung wegstrecke anzahl weiche
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        anfrageWeicheAnhängen
            anfrageWegstrecke
            _objekt
                = anfrageWegstrecke
anfrageWegstreckeAktualisieren
    (AWSStatusAnfrage anfrageKonstruktor eitherF)
    token
        = Left $ AWegstreckeMStatus (anfrageKonstruktor token) eitherF
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeNameAnzahlWeicheRichtung
        Wegstrecke {}
        anzahl
        weiche)
    token@EingabeToken {eingabe}
        = case wähleRichtung token of
            (Just richtung)
                | hatRichtung weiche richtung
                    -> eitherWeicheRichtungAnhängen anfrage richtung
            _otherwise
                -> Left $ AWSUnbekannt anfrage eingabe
    where
        eitherWeicheRichtungAnhängen :: AnfrageWegstrecke z -> Richtung -> Either (AnfrageWegstrecke z) (Wegstrecke (FixerZugtyp z))
        eitherWeicheRichtungAnhängen
            anfrageWegstrecke
            richtung
                | anzahl > 1
                    = Left $ qWeicheRichtungAnhängen anfrageWegstrecke richtung
                | otherwise
                    = Right $ weicheRichtungAnhängen anfrageWegstrecke richtung
        qWeicheRichtungAnhängen :: AnfrageWegstrecke z -> Richtung -> AnfrageWegstrecke z
        qWeicheRichtungAnhängen
            anfrageWegstrecke
            richtung
                = AWegstreckeNameAnzahl (weicheRichtungAnhängen anfrageWegstrecke richtung) $ pred anzahl
        weicheRichtungAnhängen :: AnfrageWegstrecke z -> Richtung -> Wegstrecke (FixerZugtyp z)
        weicheRichtungAnhängen
            AWegstreckeNameAnzahlWeicheRichtung {awsAkkumulator = wegstrecke@Wegstrecke {wsWeichenRichtungen}, awsWeiche}
            richtung
                = wegstrecke {wsWeichenRichtungen = (awsWeiche, richtung) : wsWeichenRichtungen}
        weicheRichtungAnhängen
            anfrageWegstrecke
            _richtung
                = error $ "weicheRichtungAnhängen mit unbekannter anfrageWegstrecke aufgerufen: " ++ show anfrageWegstrecke
anfrageWegstreckeAktualisieren
    anfrage@AWSUnbekannt {}
    _token
        = Left anfrage

instance MitAnfrageZugtyp AnfrageWegstrecke where
    anfrageUnbekannt :: AnfrageWegstrecke z -> Text -> AnfrageWegstrecke z
    anfrageUnbekannt = AWSUnbekannt
    anfrageMärklin :: AnfrageWegstrecke 'AnfrageZugtypMärklin
    anfrageMärklin = AnfrageWegstrecke
    anfrageLego :: AnfrageWegstrecke 'AnfrageZugtypLego
    anfrageLego = AnfrageWegstrecke

-- | Unvollständige Objekte
data AnfrageObjekt
    = AnfrageObjekt
    | AOUnbekannt
        AnfrageObjekt   -- ^ Anfrage
        Text            -- ^ Eingabe
    | AOBahngeschwindigkeit
        (AnfrageZugtypEither AnfrageBahngeschwindigkeit)
    | AOStreckenabschnitt
        AnfrageStreckenabschnitt
    | AOWeiche
        (AnfrageZugtypEither AnfrageWeiche)
    | AOKupplung
        AnfrageKupplung
    | AOWegstrecke
        (AnfrageZugtypEither AnfrageWegstrecke)
    | AOPlan
        AnfragePlan
    | AOStatusAnfrage
        StatusAnfrageObjekt
        (Either (Objekt -> AnfrageObjekt) (Objekt -> Objekt))
    | AOStatusAnfrageMärklin
        (StatusAnfrageObjektZugtyp 'Märklin)
        (Either (ObjektZugtyp 'Märklin -> AnfrageObjekt) (ObjektZugtyp 'Märklin -> ObjektZugtyp 'Märklin))
    | AOStatusAnfrageLego
        (StatusAnfrageObjektZugtyp 'Lego)
        (Either (ObjektZugtyp 'Lego -> AnfrageObjekt) (ObjektZugtyp 'Lego -> ObjektZugtyp 'Lego))

instance Show AnfrageObjekt where
    show :: AnfrageObjekt -> String
    show
        (AOUnbekannt anfrageObjekt eingabe)
            = unpack $ unbekanntShowText anfrageObjekt eingabe
    show
        AnfrageObjekt
            = Language.objekt
    show
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = showText aBahngeschwindigkeit
    show
        (AOStreckenabschnitt aStreckenabschnitt)
            = showText aStreckenabschnitt
    show
        (AOWeiche aWeiche)
            = showText aWeiche
    show
        (AOKupplung aKupplung)
            = showText aKupplung
    show
        (AOWegstrecke qWegstrecke)
            = showText qWegstrecke
    show
        (AOPlan aPlan)
            = showText aPlan
    show
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = showText objektStatusAnfrage
    show
        (AOStatusAnfrageMärklin objektStatusAnfrage _eitherKonstruktor)
            = showText objektStatusAnfrage
    show
        (AOStatusAnfrageLego objektStatusAnfrage _eitherKonstruktor)
            = showText objektStatusAnfrage

instance Anfrage AnfrageObjekt where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageObjekt -> s
    zeigeAnfrage
        (AOUnbekannt anfrageObjekt _eingabe)
            = zeigeAnfrage anfrageObjekt
    zeigeAnfrage
        AnfrageObjekt
            = Language.objekt
    zeigeAnfrage
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = zeigeAnfrage aBahngeschwindigkeit
    zeigeAnfrage
        (AOStreckenabschnitt aStreckenabschnitt)
            = zeigeAnfrage aStreckenabschnitt
    zeigeAnfrage
        (AOWeiche aWeiche)
            = zeigeAnfrage aWeiche
    zeigeAnfrage
        (AOKupplung aKupplung)
            = zeigeAnfrage aKupplung
    zeigeAnfrage
        (AOWegstrecke qWegstrecke)
            = zeigeAnfrage qWegstrecke
    zeigeAnfrage
        (AOPlan aPlan)
            = zeigeAnfrage aPlan
    zeigeAnfrage
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage
        (AOStatusAnfrageMärklin objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage
        (AOStatusAnfrageLego objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageObjekt -> Maybe s
    zeigeAnfrageOptionen
        (AOUnbekannt anfrageObjekt _eingabe)
            = zeigeAnfrageOptionen anfrageObjekt
    zeigeAnfrageOptionen
        AnfrageObjekt
            = Just $ toBefehlsString Language.befehlTypen
    zeigeAnfrageOptionen
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = zeigeAnfrageOptionen aBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AOStreckenabschnitt aStreckenabschnitt)
            = zeigeAnfrageOptionen aStreckenabschnitt
    zeigeAnfrageOptionen
        (AOWeiche aWeiche)
            = zeigeAnfrageOptionen aWeiche
    zeigeAnfrageOptionen
        (AOKupplung aKupplung)
            = zeigeAnfrageOptionen aKupplung
    zeigeAnfrageOptionen
        (AOWegstrecke aWegstrecke)
            = zeigeAnfrageOptionen aWegstrecke
    zeigeAnfrageOptionen
        (AOPlan aPlan)
            = zeigeAnfrageOptionen aPlan
    zeigeAnfrageOptionen
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen
        (AOStatusAnfrageMärklin objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen
        (AOStatusAnfrageLego objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage

instance MitAnfrage Objekt where
    type AnfrageTyp Objekt = AnfrageObjekt
    -- | Eingabe eines Objekts
    anfrageAktualisieren :: AnfrageObjekt -> EingabeToken -> Either AnfrageObjekt Objekt
    anfrageAktualisieren
        aFehler@(AOUnbekannt _anfrage _eingabe)
        _token
            = Left aFehler
    anfrageAktualisieren
        anfrageObjekt@(AOStatusAnfrage _objektStatusAnfrage _eitherKonstruktor)
        _token
            = Left anfrageObjekt
    anfrageAktualisieren
        anfrageObjekt@(AOStatusAnfrageMärklin _objektStatusAnfrage _eitherKonstruktor)
        _token
            = Left anfrageObjekt
    anfrageAktualisieren
        anfrageObjekt@(AOStatusAnfrageLego _objektStatusAnfrage _eitherKonstruktor)
        _token
            = Left anfrageObjekt
    anfrageAktualisieren
        AnfrageObjekt
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Bahngeschwindigkeit  , Left $ AOBahngeschwindigkeit $ AnfrageNothing AnfrageBahngeschwindigkeit),
                (Lexer.Streckenabschnitt    , Left $ AOStreckenabschnitt AnfrageStreckenabschnitt),
                (Lexer.Weiche               , Left $ AOWeiche $ AnfrageNothing AnfrageWeiche),
                (Lexer.Wegstrecke           , Left $ AOWegstrecke $ AnfrageNothing AnfrageWegstreckeZugtyp),
                (Lexer.Kupplung             , Left $ AOKupplung AnfrageKupplung),
                (Lexer.Plan                 , Left $ AOPlan AnfragePlan)]
                $ Left $ AOUnbekannt AnfrageObjekt eingabe
    anfrageAktualisieren
        (AOBahngeschwindigkeit (AnfrageNothing aBahngeschwindigkeit))
        token
            = Left $ AOBahngeschwindigkeit $ anfrageAktualisierenZugtyp aBahngeschwindigkeit token
    anfrageAktualisieren
        (AOBahngeschwindigkeit (AnfrageMärklin aBahngeschwindigkeit))
        token
            = case anfrageAktualisieren aBahngeschwindigkeit token of
                (Left (ABGUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOBahngeschwindigkeit $ AnfrageMärklin anfrage) eingabe1
                (Left aBahngeschwindigkeit1)
                    -> Left $ AOBahngeschwindigkeit $ AnfrageMärklin aBahngeschwindigkeit1
                (Right bahngeschwindigkeit)
                    -> Right $ OBahngeschwindigkeit $ ZugtypMärklin bahngeschwindigkeit
    anfrageAktualisieren
        (AOBahngeschwindigkeit (AnfrageLego aBahngeschwindigkeit))
        token
            = case anfrageAktualisieren aBahngeschwindigkeit token of
                (Left (ABGUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOBahngeschwindigkeit $ AnfrageLego anfrage) eingabe1
                (Left aBahngeschwindigkeit1)
                    -> Left $ AOBahngeschwindigkeit $ AnfrageLego aBahngeschwindigkeit1
                (Right bahngeschwindigkeit)
                    -> Right $ OBahngeschwindigkeit $ ZugtypLego bahngeschwindigkeit
    anfrageAktualisieren
        (AOStreckenabschnitt aStreckenabschnitt)
        token
            = case anfrageAktualisieren aStreckenabschnitt token of
                (Left (ASTUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOStreckenabschnitt anfrage) eingabe1
                (Left aStreckenabschnitt1)
                    -> Left $ AOStreckenabschnitt aStreckenabschnitt1
                (Right streckenabschnitt)
                    -> Right $ OStreckenabschnitt streckenabschnitt
    anfrageAktualisieren
        (AOWeiche (AnfrageNothing aWeiche))
        token
            = Left $ AOWeiche $ anfrageAktualisierenZugtyp aWeiche token
    anfrageAktualisieren
        (AOWeiche (AnfrageMärklin aWeiche))
        token
            = case anfrageAktualisieren aWeiche token of
                (Left (AWEUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOWeiche $ AnfrageMärklin anfrage) eingabe1
                (Left aWeiche1)
                    -> Left $ AOWeiche $ AnfrageMärklin aWeiche1
                (Right weiche)
                    -> Right $ OWeiche $ ZugtypMärklin weiche
    anfrageAktualisieren
        (AOWeiche (AnfrageLego aWeiche))
        token
            = case anfrageAktualisieren aWeiche token of
                (Left (AWEUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOWeiche $ AnfrageLego anfrage) eingabe1
                (Left aWeiche1)
                    -> Left $ AOWeiche $ AnfrageLego aWeiche1
                (Right weiche)
                    -> Right $ OWeiche $ ZugtypLego weiche
    anfrageAktualisieren
        (AOKupplung aKupplung)
        token
            = case anfrageAktualisieren aKupplung token of
                (Left (AKUUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOKupplung anfrage) eingabe1
                (Left aKupplung1)
                    -> Left $ AOKupplung aKupplung1
                (Right kupplung)
                    -> Right $ OKupplung kupplung
    anfrageAktualisieren
        (AOWegstrecke (AnfrageNothing aWegstrecke))
        token
            = Left $ AOWegstrecke $ anfrageAktualisierenZugtyp aWegstrecke token
    anfrageAktualisieren
        (AOWegstrecke (AnfrageMärklin aWegstrecke))
        token
            = case anfrageAktualisieren aWegstrecke token of
                (Left (AWSUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOWegstrecke $ AnfrageMärklin anfrage) eingabe1
                (Left (AWegstreckeMStatus objektStatusAnfrage (Right konstruktor)))
                    -> Left $ AOStatusAnfrageMärklin objektStatusAnfrage $ Right $
                        \objekt -> OZWegstrecke $ konstruktor objekt
                (Left (AWegstreckeMStatus objektStatusAnfrage (Left anfrageKonstruktor)))
                    -> Left $ AOStatusAnfrageMärklin objektStatusAnfrage $ Left $
                        \objekt -> AOWegstrecke $ AnfrageMärklin $ anfrageKonstruktor objekt
                (Left aWegstrecke1)
                    -> Left $ AOWegstrecke $ AnfrageMärklin aWegstrecke1
                (Right wegstrecke)
                    -> Right $ OWegstrecke $ ZugtypMärklin wegstrecke
    anfrageAktualisieren
        (AOWegstrecke (AnfrageLego aWegstrecke))
        token
            = case anfrageAktualisieren aWegstrecke token of
                (Left (AWSUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOWegstrecke $ AnfrageLego anfrage) eingabe1
                (Left (AWegstreckeMStatus objektStatusAnfrage (Right konstruktor)))
                    -> Left $ AOStatusAnfrageLego objektStatusAnfrage $ Right $
                        \objekt -> OZWegstrecke $ konstruktor objekt
                (Left (AWegstreckeMStatus objektStatusAnfrage (Left anfrageKonstruktor)))
                    -> Left $ AOStatusAnfrageLego objektStatusAnfrage $ Left $
                        \objekt -> AOWegstrecke $ AnfrageLego $ anfrageKonstruktor objekt
                (Left aWegstrecke1)
                    -> Left $ AOWegstrecke $ AnfrageLego aWegstrecke1
                (Right wegstrecke)
                    -> Right $ OWegstrecke $ ZugtypLego wegstrecke
    anfrageAktualisieren
        (AOPlan aPlan)
        token
            = case anfrageAktualisieren aPlan token of
                (Left (APUnbekannt anfrage eingabe1))
                    -> Left $ AOUnbekannt (AOPlan anfrage) eingabe1
                (Left (APlanIOStatus objektStatusAnfrage (Right konstruktor)))
                    -> Left $ AOStatusAnfrage objektStatusAnfrage $ Right $
                        \objekt -> OPlan $ konstruktor objekt
                (Left (APlanIOStatus objektStatusAnfrage (Left anfrageKonstruktor)))
                    -> Left $ AOStatusAnfrage objektStatusAnfrage $ Left $
                        \objekt -> AOPlan $ anfrageKonstruktor objekt
                (Left aPlan1)
                    -> Left $ AOPlan aPlan1
                (Right plan)
                    -> Right $ OPlan plan