{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description: Parsen von 'StreckenObjekt'en
-}
module Zug.UI.Cmd.Parser.StreckenObjekt
  ( -- * Hilfstypen
    AnfrageZugtyp(..)
  , AnfrageZugtypEither(..)
  , AnfrageAnschluss(..)
    -- * StreckenObjekte
    -- ** Bahngeschwindigkeit
  , AnfrageBahngeschwindigkeit(..)
    -- ** Weiche
  , AnfrageWeiche(..)
    -- ** Streckenabschnitt
  , AnfrageStreckenabschnitt(..)
    -- ** Kupplung
  , AnfrageKupplung(..)
    -- ** Wegstrecke
  , AnfrageWegstrecke(..)
    -- ** Objekt
  , AnfrageObjekt(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)

-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (Bahngeschwindigkeit(..), Streckenabschnitt(..), Weiche(..), WeicheKlasse(..)
                    , Kupplung(..), Wegstrecke(..), Anschluss(..), Value(..), alleValues
                    , PCF8574Port(..), PCF8574(..), PCF8574Variant(..), vonPinGpio)
import Zug.Enums
       (Zugtyp(..), ZugtypEither(..), unterstützteZugtypen, Richtung(..), unterstützteRichtungen)
import Zug.Language (Anzeige(..), Sprache(..), ($#), (<^>), (<=>), (<->), toBefehlsString)
import qualified Zug.Language as Language
import Zug.Objekt (Objekt, ObjektAllgemein(..))
import Zug.UI.Cmd.Lexer (EingabeToken(..), leeresToken)
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, MitAnfrage(..), AnfrageZugtyp(..)
      , AnfrageZugtypEither(..), MitAnfrageZugtyp(..), anfrageAktualisierenZugtyp
      , AnfrageFortsetzung(..), ($<<), wähleBefehl, wähleRichtung, wähleValue, wähleZwischenwert
      , StatusAnfrageObjektZugtyp(..), ObjektZugtyp(..))
import Zug.UI.Cmd.Parser.Plan (AnfragePlan(..))

-- | Unvollständiger 'Anschluss'
data AnfrageAnschluss
    = AnfrageAnschluss
    | APin
    | APCF8574Port
    | APCF8574PortVariant PCF8574Variant          -- ^ Variante
    | APCF8574PortVariantA0 PCF8574Variant          -- ^ Variante
                            Value                   -- ^ a0
    | APCF8574PortVariantA0A1 PCF8574Variant          -- ^ Variante
                              Value                   -- ^ a0
                              Value                   -- ^ a1
    | APCF8574PortVariantA0A1A2 PCF8574Variant          -- ^ Variante
                                Value                   -- ^ a0
                                Value                   -- ^ a1
                                Value                   -- ^ a2
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
        -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen (APCF8574PortVariantA0 _variante _a0) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen (APCF8574PortVariantA0A1 _variante _a0 _a1) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage Anschluss where
    type AnfrageTyp Anschluss = AnfrageAnschluss

    -- | Eingabe eines 'Anschluss'
    anfrageAktualisieren
        :: AnfrageAnschluss -> EingabeToken -> AnfrageFortsetzung AnfrageAnschluss Anschluss
    anfrageAktualisieren AnfrageAnschluss token =
        wähleZwischenwert token [(Lexer.Pin, APin), (Lexer.PCF8574Port, APCF8574Port)]
    anfrageAktualisieren APin EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        (Just pin) -> AFErgebnis $ vonPinGpio pin
        Nothing -> AFFehler eingabe
    anfrageAktualisieren APCF8574Port token =
        wähleZwischenwert
            token
            [(Lexer.A, APCF8574PortVariant VariantA),
             (Lexer.Normal, APCF8574PortVariant VariantNormal)]
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
            { pcf8574 = PCF8574
                  { variant,
                    a0,
                    a1,
                    a2
                  },
              port = fromIntegral port
            }
        Nothing -> AFFehler eingabe

   -- | Unvollständige 'Bahngeschwindigkeit'
data AnfrageBahngeschwindigkeit (z :: AnfrageZugtyp) where
    AnfrageBahngeschwindigkeit :: AnfrageBahngeschwindigkeit 'AnfrageZugtyp
    AMärklinBahngeschwindigkeit :: AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitName :: { abgmName :: Text
                                        } -> AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    AMärklinBahngeschwindigkeitNameFließend
        :: { abgmName :: Text,
             abgmFließend :: Value,
             abgmGeschwindigkeitsAnfrageAnschluss :: AnfrageAnschluss
           } -> AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    ALegoBahngeschwindigkeit :: AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitName :: { abglName :: Text
                                    } -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitNameFließend
        :: { abglName :: Text,
             abglFließend :: Value,
             abglGeschwindigkeitsAnfrageAnschluss :: AnfrageAnschluss
           } -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
        :: { abglName :: Text,
             abglFließend :: Value,
             abglGeschwindigkeitsAnschluss :: Anschluss,
             abglFahrtrichtungsAnfrageAnschluss :: AnfrageAnschluss
           } -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego

deriving instance Eq (AnfrageBahngeschwindigkeit z)

deriving instance Show (AnfrageBahngeschwindigkeit z)

instance Anzeige (AnfrageBahngeschwindigkeit z) where
    anzeige :: AnfrageBahngeschwindigkeit z -> Sprache -> Text
    anzeige AnfrageBahngeschwindigkeit = Language.bahngeschwindigkeit
    anzeige AMärklinBahngeschwindigkeit = Language.märklin <-> Language.bahngeschwindigkeit
    anzeige (AMärklinBahngeschwindigkeitName name) =
        Language.märklin <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    anzeige (AMärklinBahngeschwindigkeitNameFließend name fließend geschwindigkeitsAnschluss) =
        Language.märklin
        <-> Language.bahngeschwindigkeit
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend
        <^> Language.geschwindigkeit <-> Language.anschluss <=> geschwindigkeitsAnschluss
    anzeige ALegoBahngeschwindigkeit = Language.lego <-> Language.bahngeschwindigkeit
    anzeige (ALegoBahngeschwindigkeitName name) =
        Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    anzeige (ALegoBahngeschwindigkeitNameFließend name fließend geschwindigkeitsAnschluss) =
        Language.lego
        <-> Language.bahngeschwindigkeit
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend
        <^> Language.geschwindigkeit <-> Language.anschluss <=> geschwindigkeitsAnschluss
    anzeige
        (ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
             name
             fließend
             geschwindigkeitsAnschluss
             fahrtrichtungsAnschluss) =
        Language.lego
        <-> Language.bahngeschwindigkeit
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend
        <^> Language.geschwindigkeit
        <-> Language.anschluss
        <=> geschwindigkeitsAnschluss
        <^> Language.fahrtrichtung <-> Language.anschluss <=> fahrtrichtungsAnschluss

instance Anfrage (AnfrageBahngeschwindigkeit z) where
    zeigeAnfrage :: AnfrageBahngeschwindigkeit z -> Sprache -> Text
    zeigeAnfrage AnfrageBahngeschwindigkeit = Language.zugtyp
    zeigeAnfrage AMärklinBahngeschwindigkeit = Language.name
    zeigeAnfrage AMärklinBahngeschwindigkeitName {} = Language.fließendValue
    zeigeAnfrage AMärklinBahngeschwindigkeitNameFließend {abgmGeschwindigkeitsAnfrageAnschluss} =
        zeigeAnfrage abgmGeschwindigkeitsAnfrageAnschluss
    zeigeAnfrage ALegoBahngeschwindigkeit = Language.name
    zeigeAnfrage ALegoBahngeschwindigkeitName {} = Language.fließendValue
    zeigeAnfrage ALegoBahngeschwindigkeitNameFließend {abglGeschwindigkeitsAnfrageAnschluss} =
        zeigeAnfrage abglGeschwindigkeitsAnfrageAnschluss
    zeigeAnfrage
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss} =
        zeigeAnfrage abglFahrtrichtungsAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageBahngeschwindigkeit z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AMärklinBahngeschwindigkeitName {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        AMärklinBahngeschwindigkeitNameFließend {abgmGeschwindigkeitsAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen abgmGeschwindigkeitsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage@ALegoBahngeschwindigkeitName {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        ALegoBahngeschwindigkeitNameFließend {abglGeschwindigkeitsAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen abglGeschwindigkeitsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen abglFahrtrichtungsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageBahngeschwindigkeit z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageBahngeschwindigkeit = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen AMärklinBahngeschwindigkeitName {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen
        AMärklinBahngeschwindigkeitNameFließend {abgmGeschwindigkeitsAnfrageAnschluss} =
        zeigeAnfrageOptionen abgmGeschwindigkeitsAnfrageAnschluss
    zeigeAnfrageOptionen ALegoBahngeschwindigkeitName {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen
        ALegoBahngeschwindigkeitNameFließend {abglGeschwindigkeitsAnfrageAnschluss} =
        zeigeAnfrageOptionen abglGeschwindigkeitsAnfrageAnschluss
    zeigeAnfrageOptionen
        ALegoBahngeschwindigkeitNameFließendGeschwindigkeit {abglFahrtrichtungsAnfrageAnschluss} =
        zeigeAnfrageOptionen abglFahrtrichtungsAnfrageAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrageZugtyp AnfrageBahngeschwindigkeit where
    anfrageMärklin :: AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
    anfrageMärklin = AMärklinBahngeschwindigkeit

    anfrageLego :: AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
    anfrageLego = ALegoBahngeschwindigkeit

instance MitAnfrage (Bahngeschwindigkeit 'Märklin) where
    type AnfrageTyp (Bahngeschwindigkeit 'Märklin) =
        AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin

    -- | Eingabe einer 'Märklin'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageTyp (Bahngeschwindigkeit 'Märklin)
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin) (Bahngeschwindigkeit 'Märklin)
    anfrageAktualisieren AMärklinBahngeschwindigkeit EingabeToken {eingabe} =
        AFZwischenwert $ AMärklinBahngeschwindigkeitName eingabe
    anfrageAktualisieren AMärklinBahngeschwindigkeitName {abgmName} token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, AMärklinBahngeschwindigkeitNameFließend abgmName HIGH AnfrageAnschluss),
             (Lexer.LOW, AMärklinBahngeschwindigkeitNameFließend abgmName LOW AnfrageAnschluss)]
    anfrageAktualisieren
        anfrage@(AMärklinBahngeschwindigkeitNameFließend
                     bgmName
                     bgmFließend
                     geschwindigkeitsAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren geschwindigkeitsAnschluss token
        where
            anfrageAnschlussVerwenden
                :: AnfrageAnschluss -> AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin
            anfrageAnschlussVerwenden abgmGeschwindigkeitsAnfrageAnschluss =
                anfrage
                { abgmGeschwindigkeitsAnfrageAnschluss
                }

            anschlussVerwenden
                :: Anschluss
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageZugtypMärklin) (Bahngeschwindigkeit 'Märklin)
            anschlussVerwenden bgmGeschwindigkeitsAnschluss =
                AFErgebnis
                    MärklinBahngeschwindigkeit
                        { bgmName,
                          bgmFließend,
                          bgmGeschwindigkeitsAnschluss
                        }

instance MitAnfrage (Bahngeschwindigkeit 'Lego) where
    type AnfrageTyp (Bahngeschwindigkeit 'Lego) = AnfrageBahngeschwindigkeit 'AnfrageZugtypLego

    -- | Eingabe einer 'Lego'-'Bahngeschwindigkeit'
    anfrageAktualisieren
        :: AnfrageTyp (Bahngeschwindigkeit 'Lego)
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageZugtypLego) (Bahngeschwindigkeit 'Lego)
    anfrageAktualisieren ALegoBahngeschwindigkeit EingabeToken {eingabe} =
        AFZwischenwert $ ALegoBahngeschwindigkeitName eingabe
    anfrageAktualisieren ALegoBahngeschwindigkeitName {abglName} token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, ALegoBahngeschwindigkeitNameFließend abglName HIGH AnfrageAnschluss),
             (Lexer.LOW, ALegoBahngeschwindigkeitNameFließend abglName LOW AnfrageAnschluss)]
    anfrageAktualisieren
        anfrage@(ALegoBahngeschwindigkeitNameFließend name fließend geschwindigkeitsAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren geschwindigkeitsAnschluss token
        where
            anfrageAnschlussVerwenden
                :: AnfrageAnschluss -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
            anfrageAnschlussVerwenden abglGeschwindigkeitsAnfrageAnschluss =
                anfrage
                { abglGeschwindigkeitsAnfrageAnschluss
                }

            anschlussVerwenden
                :: Anschluss
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageZugtypLego) (Bahngeschwindigkeit 'Lego)
            anschlussVerwenden anschluss =
                AFZwischenwert
                $ ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
                    name
                    fließend
                    anschluss
                    AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(ALegoBahngeschwindigkeitNameFließendGeschwindigkeit
                     bglName
                     bglFließend
                     bglGeschwindigkeitsAnschluss
                     fahrtrichtungsAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren fahrtrichtungsAnschluss token
        where
            anfrageAnschlussVerwenden
                :: AnfrageAnschluss -> AnfrageBahngeschwindigkeit 'AnfrageZugtypLego
            anfrageAnschlussVerwenden abglFahrtrichtungsAnfrageAnschluss =
                anfrage
                { abglFahrtrichtungsAnfrageAnschluss
                }

            anschlussVerwenden
                :: Anschluss
                -> AnfrageFortsetzung (AnfrageBahngeschwindigkeit 'AnfrageZugtypLego) (Bahngeschwindigkeit 'Lego)
            anschlussVerwenden bglFahrtrichtungsAnschluss =
                AFErgebnis
                    LegoBahngeschwindigkeit
                        { bglName,
                          bglFließend,
                          bglGeschwindigkeitsAnschluss,
                          bglFahrtrichtungsAnschluss
                        }

-- | Unvollständiger 'Streckenabschnitt'
data AnfrageStreckenabschnitt
    = AnfrageStreckenabschnitt
    | AStreckenabschnittName
          { astName :: Text
          }
    | AStreckenabschnittNameFließend
          { astName :: Text,
            astFließend :: Value,
            astStromAnfrageAnschluss :: AnfrageAnschluss
          }
    deriving (Eq, Show)

instance Anzeige AnfrageStreckenabschnitt where
    anzeige :: AnfrageStreckenabschnitt -> Sprache -> Text
    anzeige AnfrageStreckenabschnitt = Language.streckenabschnitt
    anzeige (AStreckenabschnittName name) = Language.streckenabschnitt <^> Language.name <=> name
    anzeige (AStreckenabschnittNameFließend name fließend stromAnschluss) =
        Language.streckenabschnitt
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend <^> Language.strom <-> Language.anschluss <=> stromAnschluss

instance Anfrage AnfrageStreckenabschnitt where
    zeigeAnfrage :: AnfrageStreckenabschnitt -> Sprache -> Text
    zeigeAnfrage AnfrageStreckenabschnitt = Language.name
    zeigeAnfrage AStreckenabschnittName {} = Language.fließendValue
    zeigeAnfrage AStreckenabschnittNameFließend {astStromAnfrageAnschluss} =
        zeigeAnfrage astStromAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageStreckenabschnitt -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AStreckenabschnittName {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen AStreckenabschnittNameFließend {astStromAnfrageAnschluss} eingabe =
        zeigeAnfrageFehlgeschlagen astStromAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageStreckenabschnitt -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AStreckenabschnittName {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen AStreckenabschnittNameFließend {astStromAnfrageAnschluss} =
        zeigeAnfrageOptionen astStromAnfrageAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage Streckenabschnitt where
    type AnfrageTyp Streckenabschnitt = AnfrageStreckenabschnitt

    -- | Eingabe eines Streckenabschnitts
    anfrageAktualisieren :: AnfrageStreckenabschnitt
                         -> EingabeToken
                         -> AnfrageFortsetzung AnfrageStreckenabschnitt Streckenabschnitt
    anfrageAktualisieren AnfrageStreckenabschnitt EingabeToken {eingabe} =
        AFZwischenwert $ AStreckenabschnittName eingabe
    anfrageAktualisieren (AStreckenabschnittName name) token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, AStreckenabschnittNameFließend name HIGH AnfrageAnschluss),
             (Lexer.LOW, AStreckenabschnittNameFließend name LOW AnfrageAnschluss)]
    anfrageAktualisieren
        anfrage@(AStreckenabschnittNameFließend stName stFließend stromAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren stromAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss -> AnfrageStreckenabschnitt
            anfrageAnschlussVerwenden astStromAnfrageAnschluss =
                anfrage
                { astStromAnfrageAnschluss
                }

            anschlussVerwenden
                :: Anschluss -> AnfrageFortsetzung AnfrageStreckenabschnitt Streckenabschnitt
            anschlussVerwenden stromAnschluss =
                AFErgebnis
                    Streckenabschnitt
                        { stName,
                          stFließend,
                          stromAnschluss
                        }

   -- | Unvollständige 'Weiche'
data AnfrageWeiche (z :: AnfrageZugtyp) where
    AnfrageWeiche :: AnfrageWeiche 'AnfrageZugtyp
    AMärklinWeiche :: AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheName :: { awemName :: Text
                           } -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließend :: { awemName :: Text,
                                      awemFließend :: Value
                                    } -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließendAnzahl
        :: { awemName :: Text,
             awemFließend :: Value,
             awemAnzahl :: Natural,
             awemRichtungsAnschlüsse :: [(Richtung, Anschluss)]
           } -> AnfrageWeiche 'AnfrageZugtypMärklin
    AMärklinWeicheNameFließendAnzahlRichtung
        :: { awemName :: Text,
             awemFließend :: Value,
             awemAnzahl :: Natural,
             awemRichtungsAnschlüsse :: [(Richtung, Anschluss)],
             awemRichtung :: Richtung,
             awemAnfrageAnschluss :: AnfrageAnschluss
           } -> AnfrageWeiche 'AnfrageZugtypMärklin
    ALegoWeiche :: AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheName :: { awelName :: Text
                       } -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließend :: { awelName :: Text,
                                  awelFließend :: Value
                                } -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließendRichtung1
        :: { awelName :: Text,
             awelFließend :: Value,
             awelRichtung1 :: Richtung
           } -> AnfrageWeiche 'AnfrageZugtypLego
    ALegoWeicheNameFließendRichtungen
        :: { awelName :: Text,
             awelFließend :: Value,
             awelRichtung1 :: Richtung,
             awelRichtung2 :: Richtung,
             awelRichtungsAnfrageAnschluss :: AnfrageAnschluss
           } -> AnfrageWeiche 'AnfrageZugtypLego

deriving instance Eq (AnfrageWeiche z)

deriving instance Show (AnfrageWeiche z)

instance Anzeige (AnfrageWeiche z) where
    anzeige :: AnfrageWeiche z -> Sprache -> Text
    anzeige AnfrageWeiche = Language.weiche
    anzeige AMärklinWeiche = Language.märklin <-> Language.weiche
    anzeige (AMärklinWeicheName name) =
        Language.lego <-> Language.weiche <^> Language.name <=> name
    anzeige (AMärklinWeicheNameFließend name fließend) =
        Language.lego
        <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> fließend
    anzeige (AMärklinWeicheNameFließendAnzahl name fließend anzahl acc) =
        Language.lego
        <-> Language.weiche
        <^> Language.name
        <=> name
        <^> Language.fließend
        <=> fließend <^> (Language.erwartet $# Language.richtungen) <=> anzahl <^> acc
    anzeige
        (AMärklinWeicheNameFließendAnzahlRichtung name fließend anzahl acc richtung anschluss) =
        Language.lego
        <-> Language.weiche
        <^> Language.name
        <=> name
        <^> Language.fließend
        <=> fließend
        <^> (Language.erwartet $# Language.richtungen)
        <=> anzahl <^> acc <^> Language.richtung <=> richtung <^> Language.anschluss <=> anschluss
    anzeige ALegoWeiche = Language.lego <-> Language.weiche
    anzeige (ALegoWeicheName name) = Language.lego <-> Language.weiche <^> Language.name <=> name
    anzeige (ALegoWeicheNameFließend name fließend) =
        Language.lego
        <-> Language.weiche <^> Language.name <=> name <^> Language.fließend <=> fließend
    anzeige (ALegoWeicheNameFließendRichtung1 name fließend richtung1) =
        Language.lego
        <-> Language.weiche
        <^> Language.name <=> name <^> Language.fließend <=> fließend <^> richtung1
    anzeige (ALegoWeicheNameFließendRichtungen name fließend richtung1 richtung2 anschluss) =
        Language.lego
        <-> Language.weiche
        <^> Language.name
        <=> name
        <^> Language.fließend
        <=> fließend
        <^> richtung1
        <^> richtung2
        <^> Language.richtung <-> Language.anschluss <=> anschluss

instance Anfrage (AnfrageWeiche z) where
    zeigeAnfrage :: AnfrageWeiche z -> Sprache -> Text
    zeigeAnfrage AnfrageWeiche = Language.zugtyp
    zeigeAnfrage AMärklinWeiche = Language.name
    zeigeAnfrage (AMärklinWeicheName _name) = Language.fließendValue
    zeigeAnfrage (AMärklinWeicheNameFließend _name _fließend) =
        Language.anzahl $# Language.richtungen
    zeigeAnfrage (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc) =
        Language.richtung
    zeigeAnfrage AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss} =
        zeigeAnfrage awemAnfrageAnschluss
    zeigeAnfrage ALegoWeiche = Language.name
    zeigeAnfrage ALegoWeicheName {} = Language.fließendValue
    zeigeAnfrage ALegoWeicheNameFließend {} = Language.richtung
    zeigeAnfrage ALegoWeicheNameFließendRichtung1 {} = Language.richtung
    zeigeAnfrage ALegoWeicheNameFließendRichtungen {awelRichtungsAnfrageAnschluss} =
        zeigeAnfrage awelRichtungsAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageWeiche z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@(AMärklinWeicheNameFließend _name _fließend) eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen awemAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen
        ALegoWeicheNameFließendRichtungen {awelRichtungsAnfrageAnschluss}
        eingabe = zeigeAnfrageFehlgeschlagen awelRichtungsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageWeiche z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageWeiche = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen (AMärklinWeicheName _name) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen (AMärklinWeicheNameFließendAnzahl _name _fließend _anzahl _acc) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen AMärklinWeicheNameFließendAnzahlRichtung {awemAnfrageAnschluss} =
        zeigeAnfrageOptionen awemAnfrageAnschluss
    zeigeAnfrageOptionen (ALegoWeicheName _name) = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen (ALegoWeicheNameFließend _name _fließend) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen (ALegoWeicheNameFließendRichtung1 _name _fließend _richtung1) =
        Just $ toBefehlsString . \sprache -> map (`anzeige` sprache)
        $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen ALegoWeicheNameFließendRichtungen {awelRichtungsAnfrageAnschluss} =
        zeigeAnfrageOptionen awelRichtungsAnfrageAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage (Weiche 'Märklin) where
    type AnfrageTyp (Weiche 'Märklin) = AnfrageWeiche 'AnfrageZugtypMärklin

    -- | Eingabe einer 'Märklin'-'Weiche'
    anfrageAktualisieren
        :: AnfrageTyp (Weiche 'Märklin)
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageZugtypMärklin) (Weiche 'Märklin)
    anfrageAktualisieren AMärklinWeiche EingabeToken {eingabe} =
        AFZwischenwert $ AMärklinWeicheName eingabe
    anfrageAktualisieren (AMärklinWeicheName name) token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, AMärklinWeicheNameFließend name HIGH),
             (Lexer.LOW, AMärklinWeicheNameFließend name LOW)]
    anfrageAktualisieren
        (AMärklinWeicheNameFließend name fließend)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just anzahl)
            -> AFZwischenwert $ AMärklinWeicheNameFließendAnzahl name fließend anzahl []
    anfrageAktualisieren
        (AMärklinWeicheNameFließendAnzahl name fließend anzahl acc)
        token@EingabeToken {eingabe} = case wähleRichtung token of
        Nothing -> AFFehler eingabe
        (Just richtung) -> AFZwischenwert
            $ AMärklinWeicheNameFließendAnzahlRichtung
                name
                fließend
                anzahl
                acc
                richtung
                AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(AMärklinWeicheNameFließendAnzahlRichtung
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
            anfrageAnschlussVerwenden :: AnfrageAnschluss -> AnfrageWeiche 'AnfrageZugtypMärklin
            anfrageAnschlussVerwenden awemAnfrageAnschluss =
                anfrage
                { awemAnfrageAnschluss
                }

            anschlussVerwenden
                :: Anschluss
                -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageZugtypMärklin) (Weiche 'Märklin)
            anschlussVerwenden anschluss
                | anzahl > 1 =
                    AFZwischenwert
                    $ AMärklinWeicheNameFließendAnzahl
                        wemName
                        wemFließend
                        (pred anzahl)
                        ((richtung, anschluss) : acc)
                | otherwise =
                    AFErgebnis
                        MärklinWeiche
                            { wemName,
                              wemFließend,
                              wemRichtungsAnschlüsse = (richtung, anschluss) :| acc
                            }

instance MitAnfrage (Weiche 'Lego) where
    type AnfrageTyp (Weiche 'Lego) = AnfrageWeiche 'AnfrageZugtypLego

    -- | Eingabe einer 'Lego'-'Weiche'
    anfrageAktualisieren :: AnfrageTyp (Weiche 'Lego)
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageZugtypLego) (Weiche 'Lego)
    anfrageAktualisieren ALegoWeiche EingabeToken {eingabe} =
        AFZwischenwert $ ALegoWeicheName eingabe
    anfrageAktualisieren (ALegoWeicheName name) token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, ALegoWeicheNameFließend name HIGH),
             (Lexer.LOW, ALegoWeicheNameFließend name LOW)]
    anfrageAktualisieren (ALegoWeicheNameFließend name fließend) token@EingabeToken {eingabe} =
        case wähleRichtung token of
            Nothing -> AFFehler eingabe
            (Just richtung1)
                -> AFZwischenwert $ ALegoWeicheNameFließendRichtung1 name fließend richtung1
    anfrageAktualisieren
        (ALegoWeicheNameFließendRichtung1 name fließend richtung1)
        token@EingabeToken {eingabe} = case wähleRichtung token of
        Nothing -> AFFehler eingabe
        (Just richtung2) -> AFZwischenwert
            $ ALegoWeicheNameFließendRichtungen
                name
                fließend
                richtung1
                richtung2
                AnfrageAnschluss
    anfrageAktualisieren
        anfrage@(ALegoWeicheNameFließendRichtungen
                     welName
                     welFließend
                     richtung1
                     richtung2
                     anfrageAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren anfrageAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss -> AnfrageWeiche 'AnfrageZugtypLego
            anfrageAnschlussVerwenden awelRichtungsAnfrageAnschluss =
                anfrage
                { awelRichtungsAnfrageAnschluss
                }

            anschlussVerwenden
                :: Anschluss
                -> AnfrageFortsetzung (AnfrageWeiche 'AnfrageZugtypLego) (Weiche 'Lego)
            anschlussVerwenden welRichtungsAnschluss =
                AFErgebnis
                    LegoWeiche
                        { welName,
                          welFließend,
                          welRichtungsAnschluss,
                          welRichtungen = (richtung1, richtung2)
                        }

instance MitAnfrageZugtyp AnfrageWeiche where
    anfrageMärklin :: AnfrageWeiche 'AnfrageZugtypMärklin
    anfrageMärklin = AMärklinWeiche

    anfrageLego :: AnfrageWeiche 'AnfrageZugtypLego
    anfrageLego = ALegoWeiche

-- | Unvollständige 'Kupplung'
data AnfrageKupplung
    = AnfrageKupplung
    | AKupplungName
          { akuName :: Text
          }
    | AKupplungNameFließend
          { akuName :: Text,
            akuFließend :: Value,
            akuKupplungsAnfrageAnschluss :: AnfrageAnschluss
          }
    deriving (Eq, Show)

instance Anzeige AnfrageKupplung where
    anzeige :: AnfrageKupplung -> Sprache -> Text
    anzeige AnfrageKupplung = Language.kupplung
    anzeige (AKupplungName name) = Language.kupplung <^> Language.name <=> name
    anzeige (AKupplungNameFließend name fließend anschluss) =
        Language.kupplung
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend <^> Language.kupplung <-> Language.anschluss <=> anschluss

instance Anfrage AnfrageKupplung where
    zeigeAnfrage :: AnfrageKupplung -> Sprache -> Text
    zeigeAnfrage AnfrageKupplung = Language.name
    zeigeAnfrage AKupplungName {} = Language.fließendValue
    zeigeAnfrage AKupplungNameFließend {akuKupplungsAnfrageAnschluss} =
        zeigeAnfrage akuKupplungsAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageKupplung -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AKupplungName {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen AKupplungNameFließend {akuKupplungsAnfrageAnschluss} eingabe =
        zeigeAnfrageFehlgeschlagen akuKupplungsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageKupplung -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AKupplungName {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList alleValues
    zeigeAnfrageOptionen AKupplungNameFließend {akuKupplungsAnfrageAnschluss} =
        zeigeAnfrageOptionen akuKupplungsAnfrageAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage Kupplung where
    type AnfrageTyp Kupplung = AnfrageKupplung

    -- | Eingabe einer Kupplung
    anfrageAktualisieren
        :: AnfrageKupplung -> EingabeToken -> AnfrageFortsetzung AnfrageKupplung Kupplung
    anfrageAktualisieren AnfrageKupplung EingabeToken {eingabe} =
        AFZwischenwert $ AKupplungName eingabe
    anfrageAktualisieren (AKupplungName name) token =
        wähleZwischenwert
            token
            [(Lexer.HIGH, AKupplungNameFließend name HIGH AnfrageAnschluss),
             (Lexer.LOW, AKupplungNameFließend name LOW AnfrageAnschluss)]
    anfrageAktualisieren anfrage@(AKupplungNameFließend kuName kuFließend anfrageAnschluss) token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren anfrageAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss -> AnfrageKupplung
            anfrageAnschlussVerwenden akuKupplungsAnfrageAnschluss =
                anfrage
                { akuKupplungsAnfrageAnschluss
                }

            anschlussVerwenden :: Anschluss -> AnfrageFortsetzung AnfrageKupplung Kupplung
            anschlussVerwenden kupplungsAnschluss =
                AFErgebnis
                    Kupplung
                        { kuName,
                          kuFließend,
                          kupplungsAnschluss
                        }

class AnfrageZugtypKlasse (z :: AnfrageZugtyp) where
    type FixerZugtyp z :: Zugtyp

    afStatusAnfrage
        :: StatusAnfrageObjektZugtyp (FixerZugtyp z)
        -> (ObjektZugtyp (FixerZugtyp z) -> AnfrageFortsetzung (a z) (e (FixerZugtyp z)))
        -> AnfrageFortsetzung (a z) (e (FixerZugtyp z))

instance AnfrageZugtypKlasse 'AnfrageZugtypMärklin where
    type FixerZugtyp 'AnfrageZugtypMärklin = 'Märklin

    afStatusAnfrage
        :: StatusAnfrageObjektZugtyp 'Märklin
        -> (ObjektZugtyp 'Märklin -> AnfrageFortsetzung (a 'AnfrageZugtypMärklin) (e 'Märklin))
        -> AnfrageFortsetzung (a 'AnfrageZugtypMärklin) (e 'Märklin)
    afStatusAnfrage = AFStatusAnfrageMärklin

instance AnfrageZugtypKlasse 'AnfrageZugtypLego where
    type FixerZugtyp 'AnfrageZugtypLego = 'Lego

    afStatusAnfrage :: StatusAnfrageObjektZugtyp 'Lego
                    -> (ObjektZugtyp 'Lego -> AnfrageFortsetzung (a 'AnfrageZugtypLego) (e 'Lego))
                    -> AnfrageFortsetzung (a 'AnfrageZugtypLego) (e 'Lego)
    afStatusAnfrage = AFStatusAnfrageLego

   -- | Unvollständige 'Wegstrecke'
data AnfrageWegstrecke (z :: AnfrageZugtyp) where
    AnfrageWegstreckeZugtyp :: AnfrageWegstrecke 'AnfrageZugtyp
    AnfrageWegstrecke :: AnfrageWegstrecke z
    AWegstreckeName :: { awsName :: Text
                       } -> AnfrageWegstrecke z
    AWegstreckeNameAnzahl :: { awsAkkumulator :: Wegstrecke (FixerZugtyp z),
                               awsAnzahl :: Natural
                             } -> AnfrageWegstrecke z
    AWegstreckeNameAnzahlWeicheRichtung
        :: { awsAkkumulator :: Wegstrecke (FixerZugtyp z),
             awsAnzahl :: Natural,
             awsWeiche :: Weiche (FixerZugtyp z)
           } -> AnfrageWegstrecke z
    AWSStatusAnfrage
        :: { awsStatusAnfrageKonstruktor
                 :: EingabeToken -> StatusAnfrageObjektZugtyp (FixerZugtyp z),
             awsEitherKonstruktor :: Either (ObjektZugtyp (FixerZugtyp z)
                                             -> AnfrageWegstrecke z) (ObjektZugtyp (FixerZugtyp z)
                                                                      -> Wegstrecke (FixerZugtyp z))
           } -> AnfrageWegstrecke z

instance Show (AnfrageWegstrecke z) where
    show :: AnfrageWegstrecke z -> String
    show = Text.unpack . flip anzeige Deutsch

instance Anzeige (AnfrageWegstrecke z) where
    anzeige :: AnfrageWegstrecke z -> Sprache -> Text
    anzeige AnfrageWegstreckeZugtyp = Language.wegstrecke
    anzeige AnfrageWegstrecke = Language.wegstrecke
    anzeige (AWegstreckeName name) = Language.wegstrecke <^> Language.name <=> name
    anzeige (AWegstreckeNameAnzahl acc anzahl) =
        Language.wegstrecke
        <^> acc
        <^> (Language.anzahl $# Language.wegstreckenElemente) <=> anzahl
    anzeige (AWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche) =
        Language.wegstrecke
        <^> acc
        <^> (Language.anzahl $# Language.wegstreckenElemente) <=> anzahl <^> weiche
    anzeige AWSStatusAnfrage {awsStatusAnfrageKonstruktor} =
        Language.wegstreckenElement <^> awsStatusAnfrageKonstruktor leeresToken

instance Anfrage (AnfrageWegstrecke z) where
    zeigeAnfrage :: AnfrageWegstrecke z -> Sprache -> Text
    zeigeAnfrage AnfrageWegstreckeZugtyp = Language.zugtyp
    zeigeAnfrage AnfrageWegstrecke = Language.name
    zeigeAnfrage AWegstreckeName {} = Language.anzahl $# Language.wegstreckenElemente
    zeigeAnfrage AWegstreckeNameAnzahl {} = Language.wegstreckenElement
    zeigeAnfrage AWegstreckeNameAnzahlWeicheRichtung {} = Language.richtung
    zeigeAnfrage AWSStatusAnfrage {awsStatusAnfrageKonstruktor} =
        zeigeAnfrage $ awsStatusAnfrageKonstruktor leeresToken

    zeigeAnfrageOptionen :: AnfrageWegstrecke z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageWegstreckeZugtyp = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList unterstützteZugtypen
    zeigeAnfrageOptionen
        AWegstreckeNameAnzahl {} = Just $ toBefehlsString . Language.befehlWegstreckenElemente
    zeigeAnfrageOptionen AWegstreckeNameAnzahlWeicheRichtung {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NE.toList unterstützteRichtungen
    zeigeAnfrageOptionen AWSStatusAnfrage {awsStatusAnfrageKonstruktor} =
        zeigeAnfrageOptionen $ awsStatusAnfrageKonstruktor leeresToken
    zeigeAnfrageOptionen _anfrage = Nothing

-- | Bekannte Teil-Typen einer 'Wegstrecke'
data AnfrageWegstreckenElement
    = AWSEUnbekannt Text
    | AWSEWeiche
    | AWSEBahngeschwindigkeit
    | AWSEStreckenabschnitt
    | AWSEKupplung

instance MitAnfrage (Wegstrecke 'Märklin) where
    type AnfrageTyp (Wegstrecke 'Märklin) = AnfrageWegstrecke 'AnfrageZugtypMärklin

    anfrageAktualisieren
        :: AnfrageWegstrecke 'AnfrageZugtypMärklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageWegstrecke 'AnfrageZugtypMärklin) (Wegstrecke 'Märklin)
    anfrageAktualisieren = anfrageWegstreckeAktualisieren

instance MitAnfrage (Wegstrecke 'Lego) where
    type AnfrageTyp (Wegstrecke 'Lego) = AnfrageWegstrecke 'AnfrageZugtypLego

    anfrageAktualisieren
        :: AnfrageWegstrecke 'AnfrageZugtypLego
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageWegstrecke 'AnfrageZugtypLego) (Wegstrecke 'Lego)
    anfrageAktualisieren = anfrageWegstreckeAktualisieren

-- | Eingabe einer Wegstrecke
anfrageWegstreckeAktualisieren
    :: (AnfrageZugtypKlasse z)
    => AnfrageWegstrecke z
    -> EingabeToken
    -> AnfrageFortsetzung (AnfrageWegstrecke z) (Wegstrecke (FixerZugtyp z))
anfrageWegstreckeAktualisieren anfrage@AnfrageWegstreckeZugtyp _token = AFZwischenwert anfrage
anfrageWegstreckeAktualisieren AnfrageWegstrecke EingabeToken {eingabe} =
    AFZwischenwert $ AWegstreckeName eingabe
anfrageWegstreckeAktualisieren (AWegstreckeName wsName) EingabeToken {eingabe, ganzzahl} =
    case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just anzahl) -> AFZwischenwert
            $ AWegstreckeNameAnzahl
                Wegstrecke
                    { wsName,
                      wsBahngeschwindigkeiten = [],
                      wsStreckenabschnitte = [],
                      wsWeichenRichtungen = [],
                      wsKupplungen = []
                    }
                anzahl
anfrageWegstreckeAktualisieren anfrage@(AWegstreckeNameAnzahl acc anzahl) token =
    case anfrageWegstreckenElement token of
        (AWSEUnbekannt eingabe) -> AFFehler eingabe
        AWSEWeiche
         -> AFZwischenwert $ AWSStatusAnfrage SAOZWeiche $ Left $ anfrageWeicheAnhängen anfrage
        AWSEBahngeschwindigkeit
         -> AFZwischenwert $ AWSStatusAnfrage SAOZBahngeschwindigkeit $ eitherObjektAnhängen acc
        AWSEStreckenabschnitt
         -> AFZwischenwert $ AWSStatusAnfrage SAOZStreckenabschnitt $ eitherObjektAnhängen acc
        AWSEKupplung -> AFZwischenwert $ AWSStatusAnfrage SAOZKupplung $ eitherObjektAnhängen acc
    where
        anfrageWegstreckenElement :: EingabeToken -> AnfrageWegstreckenElement
        anfrageWegstreckenElement token@EingabeToken {eingabe} =
            wähleBefehl
                token
                [(Lexer.Weiche, AWSEWeiche),
                 (Lexer.Bahngeschwindigkeit, AWSEBahngeschwindigkeit),
                 (Lexer.Streckenabschnitt, AWSEStreckenabschnitt),
                 (Lexer.Kupplung, AWSEKupplung)]
            $ AWSEUnbekannt eingabe

        eitherObjektAnhängen :: Wegstrecke (FixerZugtyp z)
                              -> Either (ObjektZugtyp (FixerZugtyp z)
                                         -> AnfrageWegstrecke z) (ObjektZugtyp (FixerZugtyp z)
                                                                  -> Wegstrecke (FixerZugtyp z))
        eitherObjektAnhängen wegstrecke
            | anzahl > 1 = Left $ anfrageObjektAnhängen wegstrecke
            | otherwise = Right $ objektAnhängen wegstrecke

        objektAnhängen :: Wegstrecke z -> ObjektZugtyp z -> Wegstrecke z
        objektAnhängen
            wegstrecke@Wegstrecke {wsBahngeschwindigkeiten}
            (OZBahngeschwindigkeit bahngeschwindigkeit) =
            wegstrecke
            { wsBahngeschwindigkeiten = bahngeschwindigkeit : wsBahngeschwindigkeiten
            }
        objektAnhängen
            wegstrecke@Wegstrecke {wsStreckenabschnitte}
            (OZStreckenabschnitt streckenabschnitt) =
            wegstrecke
            { wsStreckenabschnitte = streckenabschnitt : wsStreckenabschnitte
            }
        objektAnhängen wegstrecke@Wegstrecke {wsKupplungen} (OZKupplung kupplung) =
            wegstrecke
            { wsKupplungen = kupplung : wsKupplungen
            }
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        objektAnhängen wegstrecke objekt =
            error
            $ "Unbekanntes Objekt zum anhängen an Wegstrecke ("
            ++ show wegstrecke
            ++ ") erhalten: "
            ++ show objekt

        anfrageObjektAnhängen
            :: Wegstrecke (FixerZugtyp z) -> ObjektZugtyp (FixerZugtyp z) -> AnfrageWegstrecke z
        anfrageObjektAnhängen wegstrecke objekt =
            AWegstreckeNameAnzahl (objektAnhängen wegstrecke objekt) $ pred anzahl

        anfrageWeicheAnhängen
            :: AnfrageWegstrecke z -> ObjektZugtyp (FixerZugtyp z) -> AnfrageWegstrecke z
        anfrageWeicheAnhängen (AWegstreckeNameAnzahl wegstrecke anzahl) (OZWeiche weiche) =
            AWegstreckeNameAnzahlWeicheRichtung wegstrecke anzahl weiche
        anfrageWeicheAnhängen anfrageWegstrecke objekt =
            error
            $ "Unbekanntes Objekt zum anhängen einer Weiche an AnfrageWegstrecke ("
            ++ show anfrageWegstrecke
            ++ ") erhalten: "
            ++ show objekt
anfrageWegstreckeAktualisieren
    (AWSStatusAnfrage anfrageKonstruktor (Left zwischenwertKonstruktor))
    token = afStatusAnfrage (anfrageKonstruktor token) $ AFZwischenwert . zwischenwertKonstruktor
anfrageWegstreckeAktualisieren (AWSStatusAnfrage anfrageKonstruktor (Right konstruktor)) token =
    afStatusAnfrage (anfrageKonstruktor token) $ AFErgebnis . konstruktor
anfrageWegstreckeAktualisieren
    anfrage@(AWegstreckeNameAnzahlWeicheRichtung Wegstrecke {} anzahl weiche)
    token@EingabeToken {eingabe} = case wähleRichtung token of
    (Just richtung)
        | hatRichtung weiche richtung -> eitherWeicheRichtungAnhängen anfrage richtung
    _otherwise -> AFFehler eingabe
    where
        eitherWeicheRichtungAnhängen
            :: AnfrageWegstrecke z
            -> Richtung
            -> AnfrageFortsetzung (AnfrageWegstrecke z) (Wegstrecke (FixerZugtyp z))
        eitherWeicheRichtungAnhängen anfrageWegstrecke richtung
            | anzahl > 1 = AFZwischenwert $ qWeicheRichtungAnhängen anfrageWegstrecke richtung
            | otherwise = AFErgebnis $ weicheRichtungAnhängen anfrageWegstrecke richtung

        qWeicheRichtungAnhängen :: AnfrageWegstrecke z -> Richtung -> AnfrageWegstrecke z
        qWeicheRichtungAnhängen anfrageWegstrecke richtung =
            AWegstreckeNameAnzahl (weicheRichtungAnhängen anfrageWegstrecke richtung)
            $ pred anzahl

        weicheRichtungAnhängen :: AnfrageWegstrecke z -> Richtung -> Wegstrecke (FixerZugtyp z)
        weicheRichtungAnhängen
            AWegstreckeNameAnzahlWeicheRichtung
                {awsAkkumulator = wegstrecke@Wegstrecke {wsWeichenRichtungen}, awsWeiche}
            richtung =
            wegstrecke
            { wsWeichenRichtungen = (awsWeiche, richtung) : wsWeichenRichtungen
            }
        weicheRichtungAnhängen anfrageWegstrecke _richtung =
            error
            $ "weicheRichtungAnhängen mit unerwarteter anfrageWegstrecke aufgerufen: "
            ++ show anfrageWegstrecke

instance MitAnfrageZugtyp AnfrageWegstrecke where
    anfrageMärklin :: AnfrageWegstrecke 'AnfrageZugtypMärklin
    anfrageMärklin = AnfrageWegstrecke

    anfrageLego :: AnfrageWegstrecke 'AnfrageZugtypLego
    anfrageLego = AnfrageWegstrecke

-- | Unvollständige Objekte
data AnfrageObjekt
    = AnfrageObjekt
    | AOBahngeschwindigkeit (AnfrageZugtypEither AnfrageBahngeschwindigkeit)
    | AOStreckenabschnitt AnfrageStreckenabschnitt
    | AOWeiche (AnfrageZugtypEither AnfrageWeiche)
    | AOKupplung AnfrageKupplung
    | AOWegstrecke (AnfrageZugtypEither AnfrageWegstrecke)
    | AOPlan AnfragePlan
    deriving (Show)

instance Anzeige AnfrageObjekt where
    anzeige :: AnfrageObjekt -> Sprache -> Text
    anzeige AnfrageObjekt = Language.objekt
    anzeige (AOBahngeschwindigkeit aBahngeschwindigkeit) = anzeige aBahngeschwindigkeit
    anzeige (AOStreckenabschnitt aStreckenabschnitt) = anzeige aStreckenabschnitt
    anzeige (AOWeiche aWeiche) = anzeige aWeiche
    anzeige (AOKupplung aKupplung) = anzeige aKupplung
    anzeige (AOWegstrecke qWegstrecke) = anzeige qWegstrecke
    anzeige (AOPlan aPlan) = anzeige aPlan

instance Anfrage AnfrageObjekt where
    zeigeAnfrage :: AnfrageObjekt -> Sprache -> Text
    zeigeAnfrage AnfrageObjekt = Language.objekt
    zeigeAnfrage (AOBahngeschwindigkeit aBahngeschwindigkeit) = zeigeAnfrage aBahngeschwindigkeit
    zeigeAnfrage (AOStreckenabschnitt aStreckenabschnitt) = zeigeAnfrage aStreckenabschnitt
    zeigeAnfrage (AOWeiche aWeiche) = zeigeAnfrage aWeiche
    zeigeAnfrage (AOKupplung aKupplung) = zeigeAnfrage aKupplung
    zeigeAnfrage (AOWegstrecke qWegstrecke) = zeigeAnfrage qWegstrecke
    zeigeAnfrage (AOPlan aPlan) = zeigeAnfrage aPlan

    zeigeAnfrageOptionen :: AnfrageObjekt -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageObjekt = Just $ toBefehlsString . Language.befehlTypen
    zeigeAnfrageOptionen (AOBahngeschwindigkeit aBahngeschwindigkeit) =
        zeigeAnfrageOptionen aBahngeschwindigkeit
    zeigeAnfrageOptionen (AOStreckenabschnitt aStreckenabschnitt) =
        zeigeAnfrageOptionen aStreckenabschnitt
    zeigeAnfrageOptionen (AOWeiche aWeiche) = zeigeAnfrageOptionen aWeiche
    zeigeAnfrageOptionen (AOKupplung aKupplung) = zeigeAnfrageOptionen aKupplung
    zeigeAnfrageOptionen (AOWegstrecke aWegstrecke) = zeigeAnfrageOptionen aWegstrecke
    zeigeAnfrageOptionen (AOPlan aPlan) = zeigeAnfrageOptionen aPlan

instance MitAnfrage Objekt where
    type AnfrageTyp Objekt = AnfrageObjekt

    -- | Eingabe eines Objekts
    anfrageAktualisieren
        :: AnfrageObjekt -> EingabeToken -> AnfrageFortsetzung AnfrageObjekt Objekt
    anfrageAktualisieren AnfrageObjekt token =
        wähleZwischenwert
            token
            [(Lexer.Bahngeschwindigkeit,
              AOBahngeschwindigkeit $ AnfrageNothing AnfrageBahngeschwindigkeit
             ),
             (Lexer.Streckenabschnitt, AOStreckenabschnitt AnfrageStreckenabschnitt),
             (Lexer.Weiche, AOWeiche $ AnfrageNothing AnfrageWeiche),
             (Lexer.Wegstrecke, AOWegstrecke $ AnfrageNothing AnfrageWegstreckeZugtyp),
             (Lexer.Kupplung, AOKupplung AnfrageKupplung),
             (Lexer.Plan, AOPlan AnfragePlan)]
    anfrageAktualisieren (AOBahngeschwindigkeit (AnfrageNothing AnfrageBahngeschwindigkeit)) token =
        (id, AOBahngeschwindigkeit) $<< anfrageAktualisierenZugtyp token
    anfrageAktualisieren (AOBahngeschwindigkeit (AnfrageMärklin aBahngeschwindigkeit)) token =
        (AFErgebnis . OBahngeschwindigkeit . ZugtypMärklin,
         AOBahngeschwindigkeit . AnfrageMärklin
        )
        $<< anfrageAktualisieren aBahngeschwindigkeit token
    anfrageAktualisieren (AOBahngeschwindigkeit (AnfrageLego aBahngeschwindigkeit)) token =
        (AFErgebnis . OBahngeschwindigkeit . ZugtypLego, AOBahngeschwindigkeit . AnfrageLego)
        $<< anfrageAktualisieren aBahngeschwindigkeit token
    anfrageAktualisieren (AOStreckenabschnitt aStreckenabschnitt) token =
        (AFErgebnis . OStreckenabschnitt, AOStreckenabschnitt)
        $<< anfrageAktualisieren aStreckenabschnitt token
    anfrageAktualisieren (AOWeiche (AnfrageNothing AnfrageWeiche)) token =
        (id, AOWeiche) $<< anfrageAktualisierenZugtyp token
    anfrageAktualisieren (AOWeiche (AnfrageMärklin aWeiche)) token =
        (AFErgebnis . OWeiche . ZugtypMärklin, AOWeiche . AnfrageMärklin)
        $<< anfrageAktualisieren aWeiche token
    anfrageAktualisieren (AOWeiche (AnfrageLego aWeiche)) token =
        (AFErgebnis . OWeiche . ZugtypLego, AOWeiche . AnfrageLego)
        $<< anfrageAktualisieren aWeiche token
    anfrageAktualisieren (AOKupplung aKupplung) token =
        (AFErgebnis . OKupplung, AOKupplung) $<< anfrageAktualisieren aKupplung token
    anfrageAktualisieren (AOWegstrecke (AnfrageNothing _aWegstrecke)) token =
        (id, AOWegstrecke) $<< anfrageAktualisierenZugtyp token
    anfrageAktualisieren (AOWegstrecke (AnfrageMärklin aWegstrecke)) token =
        (AFErgebnis . OWegstrecke . ZugtypMärklin, AOWegstrecke . AnfrageMärklin)
        $<< anfrageAktualisieren aWegstrecke token
    anfrageAktualisieren (AOWegstrecke (AnfrageLego aWegstrecke)) token =
        (AFErgebnis . OWegstrecke . ZugtypLego, AOWegstrecke . AnfrageLego)
        $<< anfrageAktualisieren aWegstrecke token
    anfrageAktualisieren (AOPlan aPlan) token =
        (AFErgebnis . OPlan, AOPlan) $<< anfrageAktualisieren aPlan token