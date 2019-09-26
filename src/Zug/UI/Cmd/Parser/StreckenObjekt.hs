{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description: Parsen von 'StreckenObjekt'en
-}
module Zug.UI.Cmd.Parser.StreckenObjekt (
    -- * Hilfstypen
    AnfrageZugtyp(..), AnfrageZugtypEither(..),
    AnfrageAnschluss(..), anfrageAnschlussAktualisieren,
    -- * StreckenObjekte
    -- ** Bahngeschwindigkeit
    AnfrageBahngeschwindigkeit(..), anfrageBahngeschwindigkeitAktualisieren,
    -- ** Weiche
    AnfrageWeiche(..), anfrageWeicheAktualisieren,
    -- ** Streckenabschnitt
    AnfrageStreckenabschnitt(..), anfrageStreckenabschnittAktualisieren,
    -- ** Kupplung
    AnfrageKupplung(..), anfrageKupplungAktualisieren,
    -- ** Wegstrecke
    AnfrageWegstrecke(..), anfrageWegstreckeAktualisieren,
    -- ** Objekt
    AnfrageObjekt(..), anfrageObjektAktualisieren) where

import Data.Kind (Type)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString())
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (Bahngeschwindigkeit(..), Streckenabschnitt(..), Weiche(..), Kupplung(..), Wegstrecke(..),
                    Anschluss(..), Value(..), alleValues, PCF8574Port(..), PCF8574(..), PCF8574Variant(..),
                    vonPinGpio, vonPCF8574Port)
import Zug.Klassen (Zugtyp(..), ZugtypEither(..), unterstützteZugtypen, Richtung(..), unterstützteRichtungen)
import Zug.Language ((<^>), (<=>), (<->), showText, toBefehlsString)
import qualified Zug.Language as Language
import Zug.Plan (Objekt, ObjektAllgemein(..))
import Zug.UI.Cmd.Lexer (EingabeToken(..), leeresToken)
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, unbekanntShowText, AnfrageFamilie,
                                StatusAnfrageObjekt(..), wähleBefehl, wähleRichtung, wähleValue)
import Zug.UI.Cmd.Parser.Plan (AnfragePlan(..), anfragePlanAktualisieren)

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

type instance AnfrageFamilie Anschluss = AnfrageAnschluss

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
        (APCF8574PortVariant variante)
            = Language.a0
    zeigeAnfrage
        (APCF8574PortVariantA0 variante a0)
            = Language.a1
    zeigeAnfrage
        (APCF8574PortVariantA0A1 variante a0 a1)
            = Language.a2
    zeigeAnfrage
        (APCF8574PortVariantA0A1A2 variante a0 a1 a2)
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
        anfrage@(APCF8574PortVariantA0 _variante a0)
        eingabe
            = zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(APCF8574PortVariantA0A1 _variante a0 a2)
        eingabe
            = zeigeAnfrageFehlgeschlagen anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(APCF8574PortVariantA0A1A2 _variante a0 a1 a2)
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

-- | Eingabe eines 'Anschluss'
anfrageAnschlussAktualisieren :: AnfrageAnschluss -> EingabeToken -> Either AnfrageAnschluss Anschluss
anfrageAnschlussAktualisieren
    AnfrageAnschluss
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.Pin          , APin),
            (Lexer.PCF8574Port  , APCF8574Port)]
            $ AnfrageAnschlussUnbekannt AnfrageAnschluss eingabe
anfrageAnschlussAktualisieren 
    (AnfrageAnschlussUnbekannt anfrage _eingabe)
    token
        = Left anfrage
anfrageAnschlussAktualisieren 
    APin
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            (Just pin)
                -> Right $ vonPinGpio pin
            Nothing
                -> Left $ AnfrageAnschlussUnbekannt APin eingabe
anfrageAnschlussAktualisieren
    APCF8574Port
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.A        , APCF8574PortVariant VariantA),
            (Lexer.Normal   , APCF8574PortVariant VariantNormal)]
            $ AnfrageAnschlussUnbekannt APCF8574Port eingabe
anfrageAnschlussAktualisieren
    anfrage@(APCF8574PortVariant variante)
    token@EingabeToken {eingabe}
        = Left $ case wähleValue token of
            (Just a0)
                -> APCF8574PortVariantA0 variante a0
            Nothing
                -> AnfrageAnschlussUnbekannt anfrage eingabe
anfrageAnschlussAktualisieren
    anfrage@(APCF8574PortVariantA0 variante a0)
    token@EingabeToken {eingabe}
        = Left $ case wähleValue token of
            (Just a1)
                -> APCF8574PortVariantA0A1 variante a0 a1
            Nothing
                -> AnfrageAnschlussUnbekannt anfrage eingabe
anfrageAnschlussAktualisieren
    anfrage@(APCF8574PortVariantA0A1 variante a0 a1)
    token@EingabeToken {eingabe}
        = Left $ case wähleValue token of
            (Just a2)
                -> APCF8574PortVariantA0A1A2 variante a0 a1 a2
            Nothing
                -> AnfrageAnschlussUnbekannt anfrage eingabe
anfrageAnschlussAktualisieren
    anfrage@(APCF8574PortVariantA0A1A2 variant a0 a1 a2)
    token@EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            (Just port)
                -> Right $ AnschlussPCF8574Port $ PCF8574Port {
                    pcf8574 = PCF8574 {variant, a0, a1, a2},
                    port = fromIntegral port}
            Nothing
                -> Left $ AnfrageAnschlussUnbekannt anfrage eingabe

-- | Enumeration-Typ für eventuell noch unbestimmten 'Zugtyp'
data AnfrageZugtyp
    = AnfrageZugtyp
    | AnfrageZugtypMärklin
    | AnfrageZugtypLego

-- | Analog zu 'ZugtypEither' für 'AnfrageZugtyp'
data AnfrageZugtypEither (a :: AnfrageZugtyp -> Type)
    = AnfrageNothing
        (a 'AnfrageZugtyp)
    | AnfrageMärklin
        (a 'AnfrageZugtypMärklin)
    | AnfrageLego
        (a 'AnfrageZugtypLego)

-- | Unvollständige 'Bahngeschwindigkeit'
data AnfrageBahngeschwindigkeit (z :: AnfrageZugtyp) where
    AnfrageBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageZugtyp
    ABGUnbekannt
        :: (AnfrageBahngeschwindigkeit z)               -- ^ Anfrage
        -> Text                                         -- ^ Eingabe
            -> AnfrageBahngeschwindigkeit z
    AMärklinBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitName
        :: Text                                         -- ^ Name
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNameFließend
        :: Text                                         -- ^ Name
        -> Value                                        -- ^ Fließend
        -> AnfrageAnschluss                             -- ^ Anschluss
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    ALegoBahngeschwindigkeit
        :: AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitName
        :: Text                                         -- ^ Name
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitNameFließend
        :: Text                                         -- ^ Name
        -> Value                                        -- ^ Fließend
        -> AnfrageAnschluss                             -- ^ Anschluss
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
        :: Text                                         -- ^ Name
        -> Value                                        -- ^ Fließend
        -> Anschluss                                    -- ^ Geschwindigkeit-Anschluss
        -> AnfrageAnschluss                             -- ^ AnfrageAnschluss
            -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego

type instance AnfrageFamilie (Bahngeschwindigkeit 'Märklin) = AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
type instance AnfrageFamilie (Bahngeschwindigkeit 'Lego) = AnfrageBahngeschwindigkeit 'AnfrageZugtypLego

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
        (ABGUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        AMärklinBahngeschwindigkeit
            = Language.name
    zeigeAnfrage
        (AMärklinBahngeschwindigkeitName _name)
            = Language.fließendValue
    zeigeAnfrage
        (AMärklinBahngeschwindigkeitNameFließend _name _fließend geschwindigkeitsAnschluss)
            = zeigeAnfrage geschwindigkeitsAnschluss
    zeigeAnfrage
        ALegoBahngeschwindigkeit
            = Language.name
    zeigeAnfrage
        (ALegoBahngeschwindigkeitName _name)
            = Language.fließendValue
    zeigeAnfrage
        (ALegoBahngeschwindigkeitNameFließend _name _fließend geschwindigkeitsAnschluss)
            = zeigeAnfrage geschwindigkeitsAnschluss
    zeigeAnfrage
        (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit _name _fließend _geschwindigkeitsAnschluss fahrtrichtungsAnschluss)
            = zeigeAnfrage fahrtrichtungsAnschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit z -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AMärklinBahngeschwindigkeitName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        (AMärklinBahngeschwindigkeitNameFließend _name _fließend geschwindigkeitsAnschluss)
        eingabe
            = zeigeAnfrageFehlgeschlagen geschwindigkeitsAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        anfrage@(ALegoBahngeschwindigkeitName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        (AMärklinBahngeschwindigkeitNameFließend _name _fließend geschwindigkeitsAnschluss)
        eingabe
            = zeigeAnfrageFehlgeschlagen geschwindigkeitsAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        anfrage@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit _name _fließend _geschwindigkeitsAnschluss fahrtrichtungsAnschluss)
        eingabe
            = zeigeAnfrageFehlgeschlagen fahrtrichtungsAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageBahngeschwindigkeit z -> Maybe s
    zeigeAnfrageOptionen
        AnfrageBahngeschwindigkeit
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen
        (AMärklinBahngeschwindigkeitName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (AMärklinBahngeschwindigkeitNameFließend _name _fließend geschwindigkeitsAnschluss)
            = zeigeAnfrageOptionen geschwindigkeitsAnschluss
    zeigeAnfrageOptionen
        (ALegoBahngeschwindigkeitName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (ALegoBahngeschwindigkeitNameFließend _name _fließend geschwindigkeitsAnschluss)
            = zeigeAnfrageOptionen geschwindigkeitsAnschluss
    zeigeAnfrageOptionen
        (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit _name _fließend _geschwindigkeitsAnschluss fahrtrichtungsAnschluss)
            = zeigeAnfrageOptionen fahrtrichtungsAnschluss
    zeigeAnfrageOptionen
        (ABGUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Eingabe des 'Bahngeschwindigkeit'-'Zugtyp's
anfrageBahngeschwindigkeitZugtyp ::
    AnfrageBahngeschwindigkeit 'AnfrageZugtyp ->
    EingabeToken ->
        AnfrageZugtypEither AnfrageBahngeschwindigkeit
anfrageBahngeschwindigkeitZugtyp
    AnfrageBahngeschwindigkeit
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Märklin  , AnfrageMärklin $ AMärklinBahngeschwindigkeit),
            (Lexer.Lego     , AnfrageLego $ ALegoBahngeschwindigkeit)]
            $ AnfrageNothing $ ABGUnbekannt AnfrageBahngeschwindigkeit eingabe

-- | Eingabe einer 'Märklin'-'Bahngeschwindigkeit'
anfrageBahngeschwindigkeitMärklin ::
    AnfrageFamilie (Bahngeschwindigkeit 'Märklin) ->
    EingabeToken ->
        Either (AnfrageFamilie (Bahngeschwindigkeit 'Märklin)) (Bahngeschwindigkeit 'Märklin)
anfrageBahngeschwindigkeitMärklin
    AMärklinBahngeschwindigkeit
    EingabeToken {eingabe}
        = Left $ AMärklinBahngeschwindigkeitName eingabe
anfrageBahngeschwindigkeitMärklin
    anfrage@(AMärklinBahngeschwindigkeitName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , AMärklinBahngeschwindigkeitNameFließend name HIGH),
            (Lexer.LOW  , AMärklinBahngeschwindigkeitNameFließend name LOW)]
            $ ABGUnbekannt anfrage eingabe
anfrageBahngeschwindigkeitMärklin
    anfrage@(AMärklinBahngeschwindigkeitNameFließend bgmName bgmFließend)
    token
        = case anfrageAnschlussAktualisieren AnfrageAnschluss token of
            Nothing
                -> Left $ ABGUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ MärklinBahngeschwindigkeit {
                    bgmName,
                    bgmFließend,
                    bgmGeschwindigkeitsAnschluss = zuPin pin}
anfrageBahngeschwindigkeitMärklin
    anfrage@(ABGUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- | Eingabe einer 'Lego'-'Bahngeschwindigkeit'
anfrageBahngeschwindigkeitLego ::
    AnfrageFamilie (Bahngeschwindigkeit 'Lego) ->
    EingabeToken ->
        Either (AnfrageFamilie (Bahngeschwindigkeit 'Lego)) (Bahngeschwindigkeit 'Lego)
anfrageBahngeschwindigkeitLego
    ALegoBahngeschwindigkeit
    EingabeToken {eingabe}
        = Left $ ALegoBahngeschwindigkeitName eingabe
anfrageBahngeschwindigkeitLego
    anfrage@(ALegoBahngeschwindigkeitName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , ALegoBahngeschwindigkeitNameFließend name HIGH),
            (Lexer.LOW  , ALegoBahngeschwindigkeitNameFließend name LOW)]
            $ ABGUnbekannt anfrage eingabe
anfrageBahngeschwindigkeitLego
    anfrage@(ALegoBahngeschwindigkeitNameFließend name fließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ ABGUnbekannt anfrage eingabe
            (Just pin)
                -> Left $ ALegoBahngeschwindigkeitNameFließendGeschwindigkeit name fließend $ zuPin pin
anfrageBahngeschwindigkeitLego
    anfrage@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit bglName bglFließend bglGeschwindigkeitsAnschluss)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ ABGUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ LegoBahngeschwindigkeit {
                    bglName,
                    bglFließend,
                    bglGeschwindigkeitsAnschluss,
                    bglFahrtrichtungsAnschluss = zuPin pin}
anfrageBahngeschwindigkeitLego
    anfrage@(ABGUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- | Unvollständiger 'Streckenabschnitt'
data AnfrageStreckenabschnitt
    = AnfrageStreckenabschnitt
    | ASTUnbekannt
        AnfrageStreckenabschnitt        -- ^ Anfrage
        Text                            -- ^ Eingabe
    | AStreckenabschnittName
        Text                            -- ^ Name
    | AStreckenabschnittNameFließend
        Text                            -- ^ Name
        Value                           -- ^ Fließend

type instance AnfrageFamilie Streckenabschnitt = AnfrageStreckenabschnitt

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
        (AStreckenabschnittNameFließend name fließend)
            = unpack $ Language.streckenabschnitt
                <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
instance Anfrage AnfrageStreckenabschnitt where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> s
    zeigeAnfrage
        AnfrageStreckenabschnitt
            = Language.name
    zeigeAnfrage
        (ASTUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        (AStreckenabschnittName _name)
            = Language.fließendValue
    zeigeAnfrage
        (AStreckenabschnittNameFließend _name _fließend)
            = Language.anschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AStreckenabschnittName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageStreckenabschnitt -> Maybe s
    zeigeAnfrageOptionen
        (AStreckenabschnittName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (ASTUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

    -- | Eingabe eines Streckenabschnitts
anfrageStreckenabschnittAktualisieren :: AnfrageStreckenabschnitt -> EingabeToken -> Either AnfrageStreckenabschnitt Streckenabschnitt
anfrageStreckenabschnittAktualisieren
    AnfrageStreckenabschnitt
    EingabeToken {eingabe}
        = Left $ AStreckenabschnittName eingabe
anfrageStreckenabschnittAktualisieren
    anfrage@(AStreckenabschnittName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , AStreckenabschnittNameFließend name HIGH),
            (Lexer.LOW  , AStreckenabschnittNameFließend name LOW)]
            $ ASTUnbekannt anfrage eingabe
anfrageStreckenabschnittAktualisieren
    anfrage@(AStreckenabschnittNameFließend stName stFließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ ASTUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ Streckenabschnitt {
                    stName,
                    stFließend,
                    stromAnschluss = zuPin pin}
anfrageStreckenabschnittAktualisieren
    anfrage@(ASTUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- | Unvollständige 'Weiche'
data AnfrageWeiche (z :: AnfrageZugtyp) where
    AnfrageWeiche
        :: AnfrageWeiche 'AnfrageZugtyp
    AWEUnbekannt
        :: AnfrageWeiche z              -- ^ Anfrage
        -> Text                         -- ^ Eingabe
            -> AnfrageWeiche z
    AMärklinWeiche
        :: AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheName
        :: Text                         -- ^ Name
            -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließend
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
            -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließendAnzahl
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
        -> Natural                      -- ^ Verbleibende Richtungen
        -> [(Richtung, Anschluss)]      -- ^ RichtungsAnschlüsse
            -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließendAnzahlRichtung
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
        -> Natural                      -- ^ Verbleibende Richtungen
        -> [(Richtung, Anschluss)]      -- ^ RichtungsAnschlüsse
        -> Richtung                     -- ^ nächste Richtung
            -> AnfrageWeiche 'AnfrageZugtypMärklin
    ALegoWeiche
        :: AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheName
        :: Text                         -- ^ Name
            -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließend
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
            -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließendRichtung1
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
        -> Richtung                     -- ^ Richtung1
            -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließendRichtungen
        :: Text                         -- ^ Name
        -> Value                        -- ^ Fließend
        -> Richtung                     -- ^ Richtung1
        -> Richtung                     -- ^ Richtung2
            -> AnfrageWeiche 'AnfrageZugtypLego

type instance AnfrageFamilie (Weiche 'Märklin) = AnfrageWeiche 'AnfrageZugtypMärklin
type instance AnfrageFamilie (Weiche 'Lego) = AnfrageWeiche 'AnfrageZugtypLego

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
        (AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> Language.erwartet Language.richtungen <=> showText anzahl
                <^> showText acc <> Language.richtung <=> showText richtung
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
        (ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2)
            = unpack $ Language.lego <-> Language.weiche
                <^> Language.name <=> name
                <^> Language.fließend <=> showText fließend
                <^> showText richtung1
                <^> showText richtung2
instance Anfrage (AnfrageWeiche z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageWeiche z -> s
    zeigeAnfrage
        AnfrageWeiche
            = Language.zugtyp
    zeigeAnfrage
        (AWEUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
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
        (AMärklinWeicheNameFließendAnzahlRichtung _name _fließend _anzahl _acc _richtung)
            = Language.anschluss
    zeigeAnfrage
        ALegoWeiche
            = Language.name
    zeigeAnfrage
        (ALegoWeicheName _name)
            = Language.fließendValue
    zeigeAnfrage
        (ALegoWeicheNameFließend _name _fließend)
            = Language.richtung
    zeigeAnfrage
        (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1)
            = Language.richtung
    zeigeAnfrage
        (ALegoWeicheNameFließendRichtungen _name _fließen _richtung1 _richtung2)
            = Language.anschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageWeiche z -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AMärklinWeicheNameFließend _name _fließend)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(AMärklinWeicheNameFließendAnzahlRichtung _name _fließend _anzahl _acc _richtung)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage@(ALegoWeicheNameFließendRichtungen _name _fließend _richtung1 _richtung2)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
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
        (ALegoWeicheName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (ALegoWeicheNameFließend _name _fließend)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Eingabe des 'Weiche'-'Zugtyp's
anfrageWeicheZugtyp ::
    AnfrageWeiche 'AnfrageZugtyp ->
    EingabeToken ->
        AnfrageZugtypEither AnfrageWeiche
anfrageWeicheZugtyp
    AnfrageWeiche
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Märklin  , AnfrageMärklin AMärklinWeiche),
            (Lexer.Lego     , AnfrageLego ALegoWeiche)]
            $ AnfrageNothing $ AWEUnbekannt AnfrageWeiche eingabe

-- | Eingabe einer Weiche
anfrageWeicheAktualisieren ::
    AnfrageFamilie (Weiche z) ->
    EingabeToken ->
        Either (AnfrageFamilie (Weiche z)) (Weiche z)
anfrageWeicheAktualisieren
    AMärklinWeiche
    EingabeToken {eingabe}
        = Left $ AMärklinWeicheName eingabe
anfrageWeicheAktualisieren
    anfrage@(AMärklinWeicheName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , AMärklinWeicheNameFließend name HIGH),
            (Lexer.LOW  , AMärklinWeicheNameFließend name LOW)]
            $ AWEUnbekannt anfrage eingabe
anfrageWeicheAktualisieren
    anfrage@(AMärklinWeicheNameFließend name fließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AWEUnbekannt anfrage eingabe
            (Just anzahl)
                -> Left $ AMärklinWeicheNameFließendAnzahl name fließend anzahl []
anfrageWeicheAktualisieren
    anfrage@(AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)
    token@EingabeToken {eingabe}
        = Left $ case wähleRichtung token of
            Nothing
                -> AWEUnbekannt anfrage eingabe
            (Just richtung)
                -> AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung
anfrageWeicheAktualisieren
    anfrage@(AMärklinWeicheNameFließendAnzahlRichtung wemName wemFließend anzahl acc richtung)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AWEUnbekannt anfrage eingabe
            (Just pin)
                | anzahl > 1
                    -> Left $ AMärklinWeicheNameFließendAnzahl
                        wemName
                        wemFließend
                        (pred anzahl)
                        ((richtung, zuPin pin) : acc)
                | otherwise
                    -> Right MärklinWeiche {
                        wemName,
                        wemFließend,
                        wemRichtungsAnschlüsse = (richtung, zuPin pin) :| acc}
anfrageWeicheAktualisieren
    ALegoWeiche
    EingabeToken {eingabe}
        = Left $ ALegoWeicheName eingabe
anfrageWeicheAktualisieren
    anfrage@(ALegoWeicheName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , ALegoWeicheNameFließend name HIGH),
            (Lexer.LOW  , ALegoWeicheNameFließend name LOW)]
            $ AWEUnbekannt anfrage eingabe
anfrageWeicheAktualisieren
    anfrage@(ALegoWeicheNameFließend name fließend)
    token@EingabeToken {eingabe}
        = Left $ case wähleRichtung token of
            Nothing
                -> AWEUnbekannt anfrage eingabe
            (Just richtung1)
                -> ALegoWeicheNameFließendRichtung1 name fließend richtung1
anfrageWeicheAktualisieren
    anfrage@(ALegoWeicheNameFließendRichtung1 name fließend richtung1)
    token@EingabeToken {eingabe}
        = Left $ case wähleRichtung token of
            Nothing
                -> AWEUnbekannt anfrage eingabe
            (Just richtung2)
                -> ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2
anfrageWeicheAktualisieren
    anfrage@(ALegoWeicheNameFließendRichtungen welName welFließend richtung1 richtung2)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AWEUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ LegoWeiche {
                    welName,
                    welFließend,
                    welRichtungsAnschluss = zuPin pin,
                    welRichtungen = (richtung1,richtung2)}
anfrageWeicheAktualisieren
    anfrage@(AWEUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- | Unvollständige 'Kupplung'
data AnfrageKupplung
    = AnfrageKupplung
    | AKUUnbekannt
        AnfrageKupplung     -- ^ Anfrage
        Text                -- ^ Eingabe
    | AKupplungName
        Text                -- ^ Name
    | AKupplungNameFließend
        Text                -- ^ Name
        Value               -- ^ Fließend

type instance AnfrageFamilie Kupplung = AnfrageKupplung

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
        (AKupplungNameFließend name fließend)
            = unpack $ Language.kupplung <^> Language.name <=> name
                <^> Language.fließendValue <=> showText fließend
instance Anfrage AnfrageKupplung where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageKupplung -> s
    zeigeAnfrage
        AnfrageKupplung
            = Language.name
    zeigeAnfrage
        (AKUUnbekannt anfrage _eingabe)
            = zeigeAnfrage anfrage
    zeigeAnfrage
        (AKupplungName _name)
            = Language.fließendValue
    zeigeAnfrage
        (AKupplungNameFließend _name _fließend)
            = Language.anschluss
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageKupplung -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AKupplungName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageKupplung -> Maybe s
    zeigeAnfrageOptionen
        (AKupplungName _name)
            = Just $ toBefehlsString $ map showText $ NE.toList alleValues
    zeigeAnfrageOptionen
        (AKUUnbekannt anfrage _eingabe)
            = zeigeAnfrageOptionen anfrage
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Eingabe einer Kupplung
anfrageKupplungAktualisieren :: AnfrageKupplung -> EingabeToken -> Either AnfrageKupplung Kupplung
anfrageKupplungAktualisieren
    AnfrageKupplung
    EingabeToken {eingabe}
        = Left $ AKupplungName eingabe
anfrageKupplungAktualisieren
    anfrage@(AKupplungName name)
    token@EingabeToken {eingabe}
        = Left $ wähleBefehl token [
            (Lexer.HIGH , AKupplungNameFließend name HIGH),
            (Lexer.LOW  , AKupplungNameFließend name LOW)]
            $ AKUUnbekannt anfrage eingabe
anfrageKupplungAktualisieren
    anfrage@(AKupplungNameFließend kuName kuFließend)
    EingabeToken {eingabe, ganzzahl}
        = case ganzzahl of
            Nothing
                -> Left $ AKUUnbekannt anfrage eingabe
            (Just pin)
                -> Right $ Kupplung {
                    kuName,
                    kuFließend,
                    kupplungsAnschluss = zuPin pin}
anfrageKupplungAktualisieren
    anfrage@(AKUUnbekannt _anfrage _eingabe)
    _token
        = Left anfrage

-- | Unvollständige 'Wegstrecke'
data AnfrageWegstrecke (z :: Zugtyp)
    = AnfrageWegstrecke
    | AWSUnbekannt
        (AnfrageWegstrecke z)   -- ^ Anfrage
        Text                    -- ^ Eingabe
    | AWegstreckeName
        Text                    -- ^ Name
    | AWegstreckeNameAnzahl
        (Wegstrecke z)          -- ^ Akkumulator
        Natural                 -- ^ Anzahl zusätzliche Elemente
    | AWegstreckeNameAnzahlWeicheRichtung
        (Wegstrecke z)          -- ^ Akkumulator
        Natural                 -- ^ Anzahl zusätzliche Elemente
        (Weiche z)              -- ^ Weiche für Richtung-Angabe
    | AWegstreckeMStatus
        StatusAnfrageObjekt
        (Either (Objekt -> (AnfrageWegstrecke z)) (Objekt -> (Wegstrecke z)))
    | AWSStatusAnfrage
        (EingabeToken -> StatusAnfrageObjekt)
        (Either (Objekt -> (AnfrageWegstrecke z)) (Objekt -> (Wegstrecke z)))

type instance AnfrageFamilie (Wegstrecke z) = AnfrageWegstrecke z

instance Show (AnfrageWegstrecke z) where
    show :: AnfrageWegstrecke z -> String
    show
        (AWSUnbekannt qWegstrecke eingabe)
            = unpack $ unbekanntShowText qWegstrecke eingabe
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
        (AWegstreckeMStatus objektStatusAnfrage _eitherKonstruktor)
            = Language.wegstrecke <^> showText objektStatusAnfrage
    show
        (AWSStatusAnfrage anfrageKonstruktor _eitherF)
            = Language.wegstreckenElement <^> showText (anfrageKonstruktor leeresToken)
instance Anfrage (AnfrageWegstrecke z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageWegstrecke z -> s
    zeigeAnfrage
        (AWSUnbekannt qWegstrecke _eingabe)
            = zeigeAnfrage qWegstrecke
    zeigeAnfrage
        AnfrageWegstrecke
            = Language.name
    zeigeAnfrage
        (AWegstreckeName _name)
            = Language.anzahl Language.wegstreckenElemente
    zeigeAnfrage
        (AWegstreckeNameAnzahl _acc _anzahl)
            = Language.wegstreckenElement
    zeigeAnfrage
        (AWegstreckeNameAnzahlWeicheRichtung _acc _anzahl _weiche)
            = Language.richtung
    zeigeAnfrage
        (AWegstreckeMStatus objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrage
        (AWSStatusAnfrage anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageWegstrecke z -> Maybe s
    zeigeAnfrageOptionen
        (AWSUnbekannt qWegstrecke _eingabe)
            = zeigeAnfrageOptionen qWegstrecke
    zeigeAnfrageOptionen
        (AWegstreckeNameAnzahl _acc _anzahl)
            = Just $ toBefehlsString Language.befehlWegstreckenElemente
    zeigeAnfrageOptionen
        (AWegstreckeNameAnzahlWeicheRichtung _acc _anzahl _weiche)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen
        (AWegstreckeMStatus objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage
    zeigeAnfrageOptionen
        (AWSStatusAnfrage anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken
    zeigeAnfrageOptionen
        _anfrage
            = Nothing

-- | Eingabe einer Wegstrecke
anfrageWegstreckeAktualisieren :: (AnfrageWegstrecke z) -> EingabeToken -> Either (AnfrageWegstrecke z) (Wegstrecke z)
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
    anfrage@(AWegstreckeNameAnzahl
        acc@Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsKupplungen}
        anzahl)
    token
        = Left $ case anfrageWegstreckenElement token of
            AWSEWeiche
                -> AWSStatusAnfrage SAOWeiche $ Left $ anfrageWeicheAnhängen
            AWSEBahngeschwindigkeit
                -> AWSStatusAnfrage SAOBahngeschwindigkeit eitherObjektAnhängen
            AWSEStreckenabschnitt
                -> AWSStatusAnfrage SAOStreckenabschnitt eitherObjektAnhängen
            AWSEKupplung
                -> AWSStatusAnfrage SAOKupplung eitherObjektAnhängen
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
        eitherObjektAnhängen :: Either (Objekt -> (AnfrageWegstrecke z)) (Objekt -> (Wegstrecke z))
        eitherObjektAnhängen
            | anzahl > 1
                = Left anfrageObjektAnhängen
            | otherwise
                = Right objektAnhängen
        objektAnhängen :: Objekt -> (Wegstrecke z)
        objektAnhängen
            (OBahngeschwindigkeit bahngeschwindigkeit)
                = acc {wsBahngeschwindigkeiten = bahngeschwindigkeit : wsBahngeschwindigkeiten}
        objektAnhängen
            (OStreckenabschnitt streckenabschnitt)
                = acc {wsStreckenabschnitte=streckenabschnitt:wsStreckenabschnitte}
        objektAnhängen
            (OKupplung kupplung)
                = acc {wsKupplungen=kupplung:wsKupplungen}
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        objektAnhängen
            _objekt
                = acc
        anfrageObjektAnhängen :: Objekt -> (AnfrageWegstrecke z)
        anfrageObjektAnhängen objekt = AWegstreckeNameAnzahl (objektAnhängen objekt) $ pred anzahl
        anfrageWeicheAnhängen :: Objekt -> (AnfrageWegstrecke z)
        anfrageWeicheAnhängen (OWeiche weiche)  = AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        anfrageWeicheAnhängen _objekt           = anfrage
anfrageWegstreckeAktualisieren
    (AWSStatusAnfrage anfrageKonstruktor eitherF)
    token
        = Left $ AWegstreckeMStatus (anfrageKonstruktor token) eitherF
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeNameAnzahlWeicheRichtung
        acc@Wegstrecke {wsWeichenRichtungen}
        anzahl
        weiche)
    token@EingabeToken {eingabe}
        = case wähleRichtung token of
            Nothing
                -> Left $ AWSUnbekannt anfrage eingabe
            (Just richtung)
                -> eitherWeicheRichtungAnhängen richtung
    where
        eitherWeicheRichtungAnhängen :: Richtung -> Either (AnfrageWegstrecke z) (Wegstrecke z)
        eitherWeicheRichtungAnhängen richtung
            | anzahl > 1
                = Left $ qWeicheRichtungAnhängen richtung
            | otherwise
                = Right $ weicheRichtungAnhängen richtung
        qWeicheRichtungAnhängen :: Richtung -> (AnfrageWegstrecke z)
        qWeicheRichtungAnhängen richtung = AWegstreckeNameAnzahl (weicheRichtungAnhängen richtung) $ pred anzahl
        weicheRichtungAnhängen :: Richtung -> (Wegstrecke z)
        weicheRichtungAnhängen richtung = acc {wsWeichenRichtungen = (weiche, richtung) : wsWeichenRichtungen}
anfrageWegstreckeAktualisieren
    anfrage
    _token
        = Left anfrage

-- | Unvollständige Objekte
data AnfrageObjekt
    = AnfrageObjekt
    | AOUnbekannt
        AnfrageObjekt   -- ^ Anfrage
        Text            -- ^ Eingabe
    | AOPlan
        AnfragePlan
    | AOWegstrecke
        (ZugtypEither AnfrageWegstrecke)
    | AOWeiche
        (AnfrageZugtypEither AnfrageWeiche)
    | AOBahngeschwindigkeit
        (AnfrageZugtypEither AnfrageBahngeschwindigkeit)
    | AOStreckenabschnitt
        AnfrageStreckenabschnitt
    | AOKupplung
        AnfrageKupplung
    | AOStatusAnfrage
        StatusAnfrageObjekt
        (Either (Objekt -> AnfrageObjekt) (Objekt -> Objekt))

type instance AnfrageFamilie Objekt = AnfrageObjekt

instance (Show (AnfrageFamilie (ZugtypEither Weiche))) => Show AnfrageObjekt where
    show :: AnfrageObjekt -> String
    show
        (AOUnbekannt anfrageObjekt eingabe)
            = unpack $ unbekanntShowText anfrageObjekt eingabe
    show
        AnfrageObjekt
            = Language.objekt
    show
        (AOPlan aPlan)
            = showText aPlan
    show
        (AOWegstrecke qWegstrecke)
            = showText qWegstrecke
    show
        (AOWeiche aWeiche)
            = showText aWeiche
    show
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = showText aBahngeschwindigkeit
    show
        (AOStreckenabschnitt aStreckenabschnitt)
            = showText aStreckenabschnitt
    show
        (AOKupplung aKupplung)
            = showText aKupplung
    show
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
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
        (AOPlan aPlan)
            = zeigeAnfrage aPlan
    zeigeAnfrage
        (AOWegstrecke qWegstrecke)
            = zeigeAnfrage qWegstrecke
    zeigeAnfrage
        (AOWeiche aWeiche)
            = zeigeAnfrage aWeiche
    zeigeAnfrage
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = zeigeAnfrage aBahngeschwindigkeit
    zeigeAnfrage
        (AOStreckenabschnitt aStreckenabschnitt)
            = zeigeAnfrage aStreckenabschnitt
    zeigeAnfrage
        (AOKupplung aKupplung)
            = zeigeAnfrage aKupplung
    zeigeAnfrage
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrage objektStatusAnfrage
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageObjekt -> Maybe s
    zeigeAnfrageOptionen
        (AOUnbekannt anfrageObjekt _eingabe)
            = zeigeAnfrageOptionen anfrageObjekt
    zeigeAnfrageOptionen
        AnfrageObjekt
            = Just $ toBefehlsString Language.befehlTypen
    zeigeAnfrageOptionen
        (AOPlan aPlan)
            = zeigeAnfrageOptionen aPlan
    zeigeAnfrageOptionen
        (AOWegstrecke qWegstrecke)
            = zeigeAnfrageOptionen qWegstrecke
    zeigeAnfrageOptionen
        (AOWeiche aWeiche)
            = zeigeAnfrageOptionen aWeiche
    zeigeAnfrageOptionen
        (AOBahngeschwindigkeit aBahngeschwindigkeit)
            = zeigeAnfrageOptionen aBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AOStreckenabschnitt aStreckenabschnitt)
            = zeigeAnfrageOptionen aStreckenabschnitt
    zeigeAnfrageOptionen
        (AOKupplung aKupplung)
            = zeigeAnfrageOptionen aKupplung
    zeigeAnfrageOptionen
        (AOStatusAnfrage objektStatusAnfrage _eitherKonstruktor)
            = zeigeAnfrageOptionen objektStatusAnfrage

-- | Bekannte Teil-Typen einer 'Wegstrecke'
data AnfrageWegstreckenElement
    = AWSEUnbekannt
        Text
    | AWSEWeiche
    | AWSEBahngeschwindigkeit
    | AWSEStreckenabschnitt
    | AWSEKupplung

-- | Eingabe eines Objekts
anfrageObjektAktualisieren :: AnfrageObjekt -> EingabeToken -> Either AnfrageObjekt Objekt
anfrageObjektAktualisieren
    qFehler@(AOUnbekannt _anfrage _eingabe)
    _token
        = Left qFehler
anfrageObjektAktualisieren
    anfrageObjekt@(AOStatusAnfrage _ _)
    _token
        = Left anfrageObjekt
anfrageObjektAktualisieren
    AnfrageObjekt
    token@EingabeToken {eingabe}
        = wähleBefehl token [
            (Lexer.Plan                  , Left $ AOPlan AnfragePlan),
            (Lexer.Wegstrecke            , Left $ AOWegstrecke AnfrageWegstrecke),
            (Lexer.Weiche                , Left $ AOWeiche AnfrageWeiche),
            (Lexer.Bahngeschwindigkeit   , Left $ AOBahngeschwindigkeit AnfrageBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , Left $ AOStreckenabschnitt AnfrageStreckenabschnitt),
            (Lexer.Kupplung              , Left $ AOKupplung AnfrageKupplung)]
            $ Left $ AOUnbekannt AnfrageObjekt eingabe
anfrageObjektAktualisieren
    (AOPlan aPlan)
    token
        = case anfragePlanAktualisieren aPlan token of
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
anfrageObjektAktualisieren
    (AOWegstrecke aWegstrecke0)
    token
        = case anfrageWegstreckeAktualisieren aWegstrecke0 token of
            (Left (AWSUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOWegstrecke anfrage) eingabe1
            (Left (AWegstreckeMStatus objektStatusAnfrage (Right konstruktor)))
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Right $
                    \objekt -> OWegstrecke $ konstruktor objekt
            (Left (AWegstreckeMStatus objektStatusAnfrage (Left anfrageKonstruktor)))
                -> Left $ AOStatusAnfrage objektStatusAnfrage $ Left $
                    \objekt -> AOWegstrecke $ anfrageKonstruktor objekt
            (Left aWegstrecke1)
                -> Left $ AOWegstrecke aWegstrecke1
            (Right wegstrecke)
                -> Right $ OWegstrecke wegstrecke
anfrageObjektAktualisieren
    (AOWeiche aWeiche)
    token
        = case anfrageWeicheAktualisieren aWeiche token of
            (Left (AWEUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOWeiche anfrage) eingabe1
            (Left aWeiche1)
                -> Left $ AOWeiche aWeiche1
            (Right weiche)
                -> Right $ OWeiche weiche
anfrageObjektAktualisieren
    (AOBahngeschwindigkeit aBahngeschwindigkeit)
    token
        = case anfrageBahngeschwindigkeitAktualisieren aBahngeschwindigkeit token of
            (Left (ABGUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOBahngeschwindigkeit anfrage) eingabe1
            (Left aBahngeschwindigkeit1)
                -> Left $ AOBahngeschwindigkeit aBahngeschwindigkeit1
            (Right bahngeschwindigkeit)
                -> Right $ OBahngeschwindigkeit bahngeschwindigkeit
anfrageObjektAktualisieren
    (AOStreckenabschnitt aStreckenabschnitt)
    token
        = case anfrageStreckenabschnittAktualisieren aStreckenabschnitt token of
            (Left (ASTUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOStreckenabschnitt anfrage) eingabe1
            (Left aStreckenabschnitt1)
                -> Left $ AOStreckenabschnitt aStreckenabschnitt1
            (Right streckenabschnitt)
                -> Right $ OStreckenabschnitt streckenabschnitt
anfrageObjektAktualisieren
    (AOKupplung aKupplung)
    token
        = case anfrageKupplungAktualisieren aKupplung token of
            (Left (AKUUnbekannt anfrage eingabe1))
                -> Left $ AOUnbekannt (AOKupplung anfrage) eingabe1
            (Left aKupplung1)
                -> Left $ AOKupplung aKupplung1
            (Right kupplung)
                -> Right $ OKupplung kupplung