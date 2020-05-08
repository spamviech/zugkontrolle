{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description : Speichern und laden
-}
module Zug.UI.Save
  ( -- * Speichern & Laden
    speichern
  , laden
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), Value(..), Parser, Object, object, (.:), (.:?), (.=))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust, fromJust)
import Data.Semigroup (Semigroup((<>)))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word8)
import Data.Yaml (encodeFile, decodeFileEither)
import System.Directory (doesFileExist)

import Zug.Anbindung
       (Wartezeit(..), Anschluss(..), AnschlussEither(..), MitInterruptPin(..), AnschlussKlasse(..)
      , Pin(..), PCF8574Port(..), PCF8574(..), PCF8574Variant(..), Bahngeschwindigkeit(..)
      , Streckenabschnitt(..), Weiche(..), Kupplung(..), Kontakt(..), Wegstrecke(..))
import qualified Zug.Anbindung as Anbindung
import Zug.Enums
       (Richtung(..), Zugtyp(..), ZugtypEither(..), GeschwindigkeitVariante(..)
      , GeschwindigkeitEither(..), GeschwindigkeitPhantom(..), Fahrtrichtung(..), Strom(..))
import Zug.Language (Sprache())
import Zug.Objekt (ObjektAllgemein(..), ObjektKlasse(..), ObjektElement(zuObjektTyp, ObjektTyp))
import Zug.Plan (Aktion, AktionAllgemein(..), AktionWegstrecke(..), AktionBahngeschwindigkeit(..)
               , AktionStreckenabschnitt(..), AktionWeiche(..), AktionKupplung(..)
               , AktionKontakt(..), Plan, PlanAllgemein(..))
import Zug.UI.Base (StatusAllgemein(..), Status)

-- | Speichere aktuellen Zustand in Datei.
speichern :: (ObjektKlasse o, ToJSON o) => StatusAllgemein o -> FilePath -> IO ()
speichern = flip encodeFile

-- | Lade Datei.
--
-- Dateifehler und nicht-existente Dateien geben 'Nothing' zurück.
-- Ansonsten wird ein aus einem 'Status' konstruierter Typ zurückgegeben.
laden :: FilePath -> (Status -> IO s) -> Sprache -> IO (Maybe s)
laden path fromStatus sprache = do
    fileExists <- doesFileExist path
    if fileExists
        then decodeFileEither path >>= \case
            (Left _error) -> pure Nothing
            (Right f) -> fmap Just $ fromStatus $ f sprache
        else pure Nothing

-- Feld-Namen/Bezeichner in der erzeugten/erwarteten json-Datei.
-- Definition hier und nicht in Language.hs, damit einmal erzeugte json-Dateien auch nach einer Sprachänderung gültig bleiben.
bahngeschwindigkeitenJS :: Text
bahngeschwindigkeitenJS = "Bahngeschwindigkeiten"

streckenabschnitteJS :: Text
streckenabschnitteJS = "Streckenabschnitte"

weichenJS :: Text
weichenJS = "Weichen"

kupplungenJS :: Text
kupplungenJS = "Kupplungen"

kontakteJS :: Text
kontakteJS = "Kontakte"

wegstreckenJS :: Text
wegstreckenJS = "Wegstrecken"

pläneJS :: Text
pläneJS = "Pläne"

instance FromJSON (Sprache -> Status) where
    parseJSON :: Value -> Parser (Sprache -> Status)
    parseJSON (Object v) =
        Status <$> ((v .: bahngeschwindigkeitenJS) <|> pure [])
        <*> ((v .: streckenabschnitteJS) <|> pure [])
        <*> ((v .: weichenJS) <|> pure [])
        <*> ((v .: kupplungenJS) <|> pure [])
        <*> ((v .: kontakteJS) <|> pure [])
        <*> ((v .: wegstreckenJS) <|> pure [])
        <*> ((v .: pläneJS) <|> pure [])
    parseJSON _value = mzero

instance forall o. (ObjektKlasse o, ToJSON o) => ToJSON (StatusAllgemein o) where
    toJSON :: StatusAllgemein o -> Value
    toJSON status =
        object
            [ bahngeschwindigkeitenJS
                  .= (map (ausObjekt . OBahngeschwindigkeit) $ _bahngeschwindigkeiten status :: [o])
            , streckenabschnitteJS
                  .= (map (ausObjekt . OStreckenabschnitt) $ _streckenabschnitte status :: [o])
            , weichenJS .= (map (ausObjekt . OWeiche) $ _weichen status :: [o])
            , kupplungenJS .= (map (ausObjekt . OKupplung) $ _kupplungen status :: [o])
            , kontakteJS .= (map (ausObjekt . OKontakt) $ _kontakte status :: [o])
            , wegstreckenJS .= (map (ausObjekt . OWegstrecke) $ _wegstrecken status :: [o])
            , pläneJS .= (map (ausObjekt . OPlan) $ _pläne status :: [o])]

instance ( FromJSON (GeschwindigkeitEither bg 'Märklin)
         , FromJSON (GeschwindigkeitEither bg 'Lego)
         , FromJSON st
         , FromJSON (we 'Märklin)
         , FromJSON (we 'Lego)
         , FromJSON ku
         , FromJSON ko
         , FromJSON (ws 'Lego)
         , FromJSON (ws 'Märklin)
         , FromJSON pl
         ) => FromJSON (ObjektAllgemein bg st we ku ko ws pl) where
    parseJSON :: Value -> Parser (ObjektAllgemein bg st we ku ko ws pl)
    parseJSON value =
        OBahngeschwindigkeit
        <$> parseJSON value <|> OStreckenabschnitt
        <$> parseJSON value <|> OWeiche
        <$> parseJSON value <|> OKupplung
        <$> parseJSON value <|> OWegstrecke
        <$> parseJSON value <|> OPlan
        <$> parseJSON value

instance ( ToJSON (GeschwindigkeitEither bg 'Märklin)
         , ToJSON (GeschwindigkeitEither bg 'Lego)
         , ToJSON st
         , ToJSON (we 'Märklin)
         , ToJSON (we 'Lego)
         , ToJSON ku
         , ToJSON ko
         , ToJSON (ws 'Märklin)
         , ToJSON (ws 'Lego)
         , ToJSON pl
         ) => ToJSON (ObjektAllgemein bg st we ku ko ws pl) where
    toJSON :: ObjektAllgemein bg st we ku ko ws pl -> Value
    toJSON (OBahngeschwindigkeit bg) = toJSON bg
    toJSON (OStreckenabschnitt st) = toJSON st
    toJSON (OWeiche we) = toJSON we
    toJSON (OKupplung ku) = toJSON ku
    toJSON (OKontakt ko) = toJSON ko
    toJSON (OWegstrecke ws) = toJSON ws
    toJSON (OPlan pl) = toJSON pl

instance (FromJSON (a 'Märklin), FromJSON (a 'Lego)) => FromJSON (ZugtypEither a) where
    parseJSON :: Value -> Parser (ZugtypEither a)
    parseJSON value = (ZugtypMärklin <$> parseJSON value) <|> (ZugtypLego <$> parseJSON value)

instance (ToJSON (a 'Märklin), ToJSON (a 'Lego)) => ToJSON (ZugtypEither a) where
    toJSON :: ZugtypEither a -> Value
    toJSON (ZugtypMärklin a) = toJSON a
    toJSON (ZugtypLego a) = toJSON a

-- Instanz-Deklarationen für Anschluss
-- Dabei wird eine Rückwärtskompatibilität zu Versionen < 1.0.1.0 berücksichtigt.
-- Bei diesen war nur ein Pin-Anschluss erlaubt, wodurch ein Anschluss nur durch eine Zahl gespeichert wurde.
-- neue Feld-Namen/Bezeichner in json-Datei
pinJS :: Text
pinJS = "Pin"

portJS :: Text
portJS = "PCF8574Port"

instance FromJSON AnschlussEither where
    parseJSON :: Value -> Parser AnschlussEither
    parseJSON v = (AnschlussMit <$> parseJSON v) <|> (AnschlussOhne <$> parseJSON v)

instance ToJSON AnschlussEither where
    toJSON :: AnschlussEither -> Value
    toJSON (AnschlussMit a) = toJSON a
    toJSON (AnschlussOhne a) = toJSON a

-- | Parse einen 'AnschlussEither'.
--
-- Dabei wird eine Rückwärtskompatibilität zu Versionen < 1.0.1.0 berücksichtigt.
-- Bei diesen war nur ein 'Pin'-Anschluss erlaubt, weshalb die JSON-Felder anders hießen.
parseAnschlussEither :: Object -> Text -> Text -> Parser AnschlussEither
parseAnschlussEither v anschlussJS pinJS =
    (v .: anschlussJS) <|> (AnschlussMit . AnschlussPin . Gpio <$> v .: pinJS)

instance FromJSON (Anschluss 'MitInterruptPin) where
    parseJSON :: Value -> Parser (Anschluss 'MitInterruptPin)
    parseJSON (Object v) =
        (AnschlussPin . Gpio <$> v .: pinJS) <|> (AnschlussPCF8574Port <$> v .: portJS)
    parseJSON (Number pin) = pure $ AnschlussPin $ Gpio $ floor pin
    parseJSON _value = mzero

instance FromJSON (Anschluss 'OhneInterruptPin) where
    parseJSON :: Value -> Parser (Anschluss 'OhneInterruptPin)
    parseJSON (Object v) = AnschlussPCF8574Port <$> v .: portJS
    parseJSON _value = mzero

instance ToJSON (Anschluss i) where
    toJSON :: Anschluss i -> Value
    toJSON anschluss@AnschlussPin {} = object [pinJS .= (fromJust $ zuPinGpio anschluss :: Int)]
    toJSON AnschlussPCF8574Port {pcf8574Port} = object [portJS .= pcf8574Port]

-- Instanz-Deklarationen für PCF8574Port
-- neue Feld-Namen/Bezeichner in json-Datei
variantJS :: Text
variantJS = "Variante"

a0JS :: Text
a0JS = "a0"

a1JS :: Text
a1JS = "a1"

a2JS :: Text
a2JS = "a2"

interruptPinJS :: Text
interruptPinJS = "InterruptPin"

instance FromJSON (PCF8574Port 'OhneInterruptPin) where
    parseJSON :: Value -> Parser (PCF8574Port 'OhneInterruptPin)
    parseJSON (Object v) =
        PCF8574Port <$> (PCF8574 <$> v .: variantJS <*> v .: a0JS <*> v .: a1JS <*> v .: a2JS)
        <*> (v .: portJS)
    parseJSON _anschluss = mzero

instance FromJSON (PCF8574Port 'MitInterruptPin) where
    parseJSON :: Value -> Parser (PCF8574Port 'MitInterruptPin)
    parseJSON (Object v) =
        PCF8574Port
        <$> (PCF8574InterruptPin <$> v .: variantJS
             <*> v .: a0JS
             <*> v .: a1JS
             <*> v .: a2JS
             <*> (Gpio <$> v .: interruptPinJS))
        <*> (v .: portJS)
    parseJSON _anschluss = mzero

instance ToJSON (PCF8574Port i) where
    toJSON :: PCF8574Port i -> Value
    toJSON PCF8574Port {pcf8574 = PCF8574 {variant, a0, a1, a2}, port} =
        object [variantJS .= variant, a0JS .= a0, a1JS .= a1, a2JS .= a2, portJS .= port]
    toJSON
        PCF8574Port {pcf8574 = PCF8574InterruptPin {iVariant, iA0, iA1, iA2, interruptPin}, port} =
        object
            [ variantJS .= iVariant
            , a0JS .= iA0
            , a1JS .= iA1
            , a2JS .= iA2
            , portJS .= port
            , interruptPinJS .= (fromJust $ zuPinGpio interruptPin :: Word)]

-- Instanz-Deklarationen für PCF8574Variant
-- neue Feld-Namen/Bezeichner in json-Datei
variantNormalJS :: Text
variantNormalJS = "Normal"

variantAJS :: Text
variantAJS = "A"

instance FromJSON PCF8574Variant where
    parseJSON :: Value -> Parser PCF8574Variant
    parseJSON = findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON PCF8574Variant where
    toJSON :: PCF8574Variant -> Value
    toJSON VariantNormal = String variantNormalJS
    toJSON VariantA = String variantAJS

-- Instanz-Deklarationen für Wartezeit
-- Dabei wird eine Rückwärtskompatibilität zu Versionen < 1.0.1.0 berücksichtigt.
-- Bei diesen wurde implizit immer Mikrosekunden angenommen, wodurch nur eine Zahl gespeichert wurde.
-- neue Feld-Namen/Bezeichner in json-Datei
nsJS :: Text
nsJS = "ns"

µsJS :: Text
µsJS = "µs"

msJS :: Text
msJS = "ms"

sJS :: Text
sJS = "s"

minJS :: Text
minJS = "min"

hJS :: Text
hJS = "h"

dJS :: Text
dJS = "d"

instance FromJSON Wartezeit where
    parseJSON :: Value -> Parser Wartezeit
    parseJSON (Object v) =
        (NanoSekunden <$> v .: nsJS)
        <|> (MikroSekunden <$> v .: µsJS)
        <|> (MilliSekunden <$> v .: msJS)
        <|> (Sekunden <$> v .: sJS)
        <|> (Minuten <$> v .: minJS)
        <|> (Stunden <$> v .: hJS)
        <|> (Tage <$> v .: dJS)
    parseJSON (Number µs) = pure $ MikroSekunden $ floor µs
    parseJSON _value = mzero

instance ToJSON Wartezeit where
    toJSON :: Wartezeit -> Value
    toJSON (NanoSekunden ns) = object [nsJS .= ns]
    toJSON (MikroSekunden µs) = object [µsJS .= µs]
    toJSON (MilliSekunden ms) = object [msJS .= ms]
    toJSON (Sekunden s) = object [sJS .= s]
    toJSON (Minuten min) = object [minJS .= min]
    toJSON (Stunden h) = object [hJS .= h]
    toJSON (Tage d) = object [dJS .= d]

-- Instanz-Deklaration für Richtung
-- neue Feld-Namen/Bezeichner in json-Datei
geradeJS :: Text
geradeJS = "Gerade"

kurveJS :: Text
kurveJS = "Kurve"

linksJS :: Text
linksJS = "Links"

rechtsJS :: Text
rechtsJS = "Rechts"

instance FromJSON Richtung where
    parseJSON :: Value -> Parser Richtung
    parseJSON = findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON Richtung where
    toJSON :: Richtung -> Value
    toJSON Links = String linksJS
    toJSON Rechts = String rechtsJS
    toJSON Gerade = String geradeJS
    toJSON Kurve = String kurveJS

-- Instanz-Deklaration für Zugtyp
-- neue Feld-Namen/Bezeichner in json-Datei
märklinJS :: Text
märklinJS = "Märklin"

legoJS :: Text
legoJS = "Lego"

instance FromJSON Zugtyp where
    parseJSON :: Value -> Parser Zugtyp
    parseJSON = findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON Zugtyp where
    toJSON :: Zugtyp -> Value
    toJSON Märklin = String märklinJS
    toJSON Lego = String legoJS

-- Instanz-Deklaration für Fahrtrichtung
-- neue Feld-Namen/Bezeichner in json-Datei
vorwärtsJS :: Text
vorwärtsJS = "Vorwärts"

rückwärtsJS :: Text
rückwärtsJS = "Rückwärts"

instance FromJSON Fahrtrichtung where
    parseJSON :: Value -> Parser Fahrtrichtung
    parseJSON = findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON Fahrtrichtung where
    toJSON :: Fahrtrichtung -> Value
    toJSON Vorwärts = String vorwärtsJS
    toJSON Rückwärts = String rückwärtsJS

-- Instanz-Deklaration für Strom
-- neue Feld-Namen/Bezeichner in json-Datei
fließendJS :: Text
fließendJS = "Fließend"

gesperrtJS :: Text
gesperrtJS = "Gesperrt"

instance FromJSON Strom where
    parseJSON :: Value -> Parser Strom
    parseJSON = findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON Strom where
    toJSON :: Strom -> Value
    toJSON Fließend = String fließendJS
    toJSON Gesperrt = String gesperrtJS

-- Instanz-Deklaration für Value
-- neue Feld-Namen/Bezeichner in json-Datei
highJS :: Text
highJS = "HIGH"

lowJS :: Text
lowJS = "LOW"

instance FromJSON Anbindung.Value where
    parseJSON :: Value -> Parser Anbindung.Value
    parseJSON = findeÜbereinstimmendenWert [minBound .. maxBound]

instance ToJSON Anbindung.Value where
    toJSON :: Anbindung.Value -> Value
    toJSON Anbindung.HIGH = String highJS
    toJSON Anbindung.LOW = String lowJS

-- Instanz-Deklarationen für Bahngeschwindigkeit
-- neue Feld-Namen/Bezeichner in json-Datei
nameJS :: Text
nameJS = "Name"

zugtypJS :: Text
zugtypJS = "Zugtyp"

geschwindigkeitsPinJS :: Text
geschwindigkeitsPinJS = "GeschwindigkeitsPin"

fahrtrichtungsPinJS :: Text
fahrtrichtungsPinJS = "FahrtrichtungsPin"

fahrtrichtungsAnschlussJS :: Text
fahrtrichtungsAnschlussJS = "FahrtrichtungsAnschluss"

fahrstromAnschlüsseJS :: Text
fahrstromAnschlüsseJS = "FahrstromAnschlüsse"

umdrehenAnschlussJS :: Text
umdrehenAnschlussJS = "UmdrehenAnschluss"

-- | Parse das Fließend-Feld.
-- Dabei wird eine Rückwärtskompatibilität zu Versionen <1.0.0.14 berücksichtigt.
-- Bei diesen wurde intern immer 'HIGH' angenommen.
parseFließend :: Object -> Parser Anbindung.Value
parseFließend v = (v .: fließendJS) <|> pure Anbindung.HIGH

instance (FromJSON (bg 'Pwm z), FromJSON (bg 'KonstanteSpannung z))
    => FromJSON (GeschwindigkeitEither bg z) where
    parseJSON :: Value -> Parser (GeschwindigkeitEither bg z)
    parseJSON value =
        (GeschwindigkeitPwm <$> parseJSON value)
        <|> (GeschwindigkeitKonstanteSpannung <$> parseJSON value)

instance (ToJSON (bg 'Pwm z), ToJSON (bg 'KonstanteSpannung z))
    => ToJSON (GeschwindigkeitEither bg z) where
    toJSON :: GeschwindigkeitEither bg z -> Value
    toJSON (GeschwindigkeitPwm bg) = toJSON bg
    toJSON (GeschwindigkeitKonstanteSpannung bg) = toJSON bg

instance FromJSON (Bahngeschwindigkeit 'Pwm 'Märklin) where
    parseJSON :: Value -> Parser (Bahngeschwindigkeit 'Pwm 'Märklin)
    parseJSON (Object v) = do
        Märklin <- v .: zugtypJS
        bgmpName <- v .: nameJS
        bgmpFließend <- parseFließend v
        bgmpGeschwindigkeitsPin <- Gpio <$> v .: geschwindigkeitsPinJS
        pure BahngeschwindigkeitPwmMärklin { bgmpName, bgmpFließend, bgmpGeschwindigkeitsPin }
    parseJSON _value = mzero

instance FromJSON (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin) where
    parseJSON :: Value -> Parser (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
    parseJSON (Object v) = do
        Märklin <- v .: zugtypJS
        bgmkName <- v .: nameJS
        bgmkFließend <- parseFließend v
        bgmkFahrstromAnschlüsse <- v .: fahrstromAnschlüsseJS
        bgmkUmdrehenAnschluss <- v .: umdrehenAnschlussJS
        pure
            BahngeschwindigkeitKonstanteSpannungMärklin
            { bgmkName
            , bgmkFließend
            , bgmkFahrstromAnschlüsse
            , bgmkUmdrehenAnschluss
            }
    parseJSON _value = mzero

instance FromJSON (Bahngeschwindigkeit 'Pwm 'Lego) where
    parseJSON :: Value -> Parser (Bahngeschwindigkeit 'Pwm 'Lego)
    parseJSON (Object v) = do
        Lego <- v .: zugtypJS
        bglName <- v .: nameJS
        bglGeschwindigkeitsPin <- Gpio <$> v .: geschwindigkeitsPinJS
        bglFahrtrichtungsAnschluss
            <- parseAnschlussEither v fahrtrichtungsAnschlussJS fahrtrichtungsPinJS
        bglFließend <- parseFließend v
        pure
            BahngeschwindigkeitPwmLego
            { bglName
            , bglFließend
            , bglGeschwindigkeitsPin
            , bglFahrtrichtungsAnschluss
            }
    parseJSON _value = mzero

instance FromJSON (Bahngeschwindigkeit 'KonstanteSpannung 'Lego) where
    parseJSON :: Value -> Parser (Bahngeschwindigkeit 'KonstanteSpannung 'Lego)
    parseJSON _value = mzero

instance ToJSON (Bahngeschwindigkeit b z) where
    toJSON :: Bahngeschwindigkeit b z -> Value
    toJSON
        BahngeschwindigkeitPwmLego
        {bglName, bglFließend, bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss} =
        object
            [ nameJS .= bglName
            , fließendJS .= bglFließend
            , geschwindigkeitsPinJS .= (fromJust $ zuPinGpio bglGeschwindigkeitsPin :: Word8)
            , zugtypJS .= Lego
            , fahrtrichtungsAnschlussJS .= bglFahrtrichtungsAnschluss]
    toJSON BahngeschwindigkeitPwmMärklin {bgmpName, bgmpFließend, bgmpGeschwindigkeitsPin} =
        object
            [ nameJS .= bgmpName
            , fließendJS .= bgmpFließend
            , geschwindigkeitsPinJS .= (fromJust $ zuPinGpio bgmpGeschwindigkeitsPin :: Word8)
            , zugtypJS .= Märklin]
    toJSON
        BahngeschwindigkeitKonstanteSpannungMärklin
        {bgmkName, bgmkFließend, bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss} =
        object
            [ nameJS .= bgmkName
            , fließendJS .= bgmkFließend
            , fahrstromAnschlüsseJS .= bgmkFahrstromAnschlüsse
            , umdrehenAnschlussJS .= bgmkUmdrehenAnschluss
            , zugtypJS .= Märklin]

-- Instanz-Deklarationen für Streckenabschnitt
-- neue Feld-Namen/Bezeichner in json-Datei
stromPinJS :: Text
stromPinJS = "StromPin"

stromAnschlussJS :: Text
stromAnschlussJS = "StromAnschluss"

instance FromJSON Streckenabschnitt where
    parseJSON :: Value -> Parser Streckenabschnitt
    parseJSON (Object v) =
        Streckenabschnitt <$> v .: nameJS
        <*> parseFließend v
        <*> parseAnschlussEither v stromAnschlussJS stromPinJS
    parseJSON _value = mzero

instance ToJSON Streckenabschnitt where
    toJSON :: Streckenabschnitt -> Value
    toJSON Streckenabschnitt {stName, stFließend, stromAnschluss} =
        object [nameJS .= stName, fließendJS .= stFließend, stromAnschlussJS .= stromAnschluss]

-- Instanz-Deklarationen für Weiche
-- neue Feld-Namen/Bezeichner in json-Datei
richtungsPinJS :: Text
richtungsPinJS = "RichtungsPin"

richtungenJS :: Text
richtungenJS = "Richtungen"

richtungsPinsJS :: Text
richtungsPinsJS = "RichtungsPins"

richtungsAnschlüsseJS :: Text
richtungsAnschlüsseJS = "RichtungsAnschlüsse"

instance FromJSON (Weiche 'Märklin) where
    parseJSON :: Value -> Parser (Weiche 'Märklin)
    parseJSON (Object v) = do
        Märklin <- v .: zugtypJS
        wemName <- v .: nameJS
        wemRichtungsAnschlüsse <- v .: richtungsPinsJS <|> v .: richtungsAnschlüsseJS
        wemFließend <- parseFließend v
        pure MärklinWeiche { wemName, wemFließend, wemRichtungsAnschlüsse }
    parseJSON _value = mzero

instance FromJSON (Weiche 'Lego) where
    parseJSON :: Value -> Parser (Weiche 'Lego)
    parseJSON (Object v) = do
        Lego <- v .: zugtypJS
        welName <- v .: nameJS
        welRichtungsPin <- Gpio <$> v .: richtungsPinJS
        welRichtungen <- v .: richtungenJS
        welFließend <- parseFließend v
        pure LegoWeiche { welName, welFließend, welRichtungsPin, welRichtungen }
    parseJSON _value = mzero

instance ToJSON (Weiche z) where
    toJSON :: Weiche z -> Value
    toJSON LegoWeiche {welName, welFließend, welRichtungsPin, welRichtungen} =
        object
            [ nameJS .= welName
            , fließendJS .= welFließend
            , richtungsPinJS .= (fromJust $ zuPinGpio welRichtungsPin :: Word8)
            , richtungenJS .= welRichtungen
            , zugtypJS .= Lego]
    toJSON MärklinWeiche {wemName, wemFließend, wemRichtungsAnschlüsse} =
        object
            [ nameJS .= wemName
            , fließendJS .= wemFließend
            , richtungsAnschlüsseJS .= NonEmpty.toList wemRichtungsAnschlüsse
            , zugtypJS .= Märklin]

-- Instanz-Deklarationen für Kupplung
-- neue Feld-Namen/Bezeichner in json-Datei
kupplungsPinJS :: Text
kupplungsPinJS = "KupplungsPin"

kupplungsAnschlussJS :: Text
kupplungsAnschlussJS = "KupplungsAnschluss"

instance FromJSON Kupplung where
    parseJSON :: Value -> Parser Kupplung
    parseJSON (Object v) =
        Kupplung <$> v .: nameJS
        <*> parseFließend v
        <*> parseAnschlussEither v kupplungsAnschlussJS kupplungsPinJS
    parseJSON _value = mzero

instance ToJSON Kupplung where
    toJSON :: Kupplung -> Value
    toJSON Kupplung {kuName, kuFließend, kupplungsAnschluss} =
        object
            [ nameJS .= kuName
            , fließendJS .= kuFließend
            , kupplungsAnschlussJS .= kupplungsAnschluss]

-- Instanz-Deklarationen für Kontakt
-- neue Feld-Namen/Bezeichner in json-Datei
kontaktAnschlussJS :: Text
kontaktAnschlussJS = "KontaktAnschluss"

instance FromJSON Kontakt where
    parseJSON :: Value -> Parser Kontakt
    parseJSON (Object v) = Kontakt <$> v .: nameJS <*> v .: fließendJS <*> v .: kontaktAnschlussJS
    parseJSON _value = mzero

instance ToJSON Kontakt where
    toJSON :: Kontakt -> Value
    toJSON Kontakt {koName, koFließend, kontaktAnschluss} =
        object
            [nameJS .= koName, fließendJS .= koFließend, kontaktAnschlussJS .= kontaktAnschluss]

-- Instanz-Deklaration für Wegstrecke
-- neue Feld-Namen/Bezeichner in json-Datei
weichenRichtungenJS :: Text
weichenRichtungenJS = "Weichen-Richtungen"

-- Kontakte optional, um Kompatibilität mit älteren yaml-Dateien zu garantieren
instance (FromJSON (GeschwindigkeitEither Bahngeschwindigkeit z), FromJSON (Weiche z))
    => FromJSON (Wegstrecke z) where
    parseJSON :: Value -> Parser (Wegstrecke z)
    parseJSON (Object v) =
        Wegstrecke <$> (v .: nameJS)
        <*> v .: bahngeschwindigkeitenJS
        <*> v .: streckenabschnitteJS
        <*> v .: weichenRichtungenJS
        <*> v .: kupplungenJS
        <*> (v .: kontakteJS <|> pure Set.empty)
    parseJSON _value = mzero

instance ToJSON (Wegstrecke z) where
    toJSON :: Wegstrecke z -> Value
    toJSON
        Wegstrecke { wsName
                   , wsBahngeschwindigkeiten
                   , wsStreckenabschnitte
                   , wsWeichenRichtungen
                   , wsKupplungen
                   , wsKontakte} =
        object
            [ nameJS .= wsName
            , bahngeschwindigkeitenJS .= wsBahngeschwindigkeiten
            , streckenabschnitteJS .= wsStreckenabschnitte
            , weichenRichtungenJS .= wsWeichenRichtungen
            , kupplungenJS .= wsKupplungen
            , kontakteJS .= wsKontakte]

-- Instanz-Deklaration für Plan
-- neue Feld-Namen/Bezeichner in json-Datei
wartenJS :: Text
wartenJS = "Warten"

kontaktJS :: Text
kontaktJS = "Kontakt"

einstellenJS :: Text
einstellenJS = "Einstellen"

stellenJS :: Text
stellenJS = "Stellen"

geschwindigkeitJS :: Text
geschwindigkeitJS = "Geschwindigkeit"

fahrstromJS :: Text
fahrstromJS = "Fahrstrom"

umdrehenJS :: Text
umdrehenJS = "Umdrehen"

fahrtrichtungEinstellenJS :: Text
fahrtrichtungEinstellenJS = "FahrtrichtungEinstellen"

stromJS :: Text
stromJS = "Strom"

kuppelnJS :: Text
kuppelnJS = "Kuppeln"

wegstreckeJS :: Text
wegstreckeJS = "Wegstrecke"

aktionJS :: Text
aktionJS = "Aktion"

weicheJS :: Text
weicheJS = "Weiche"

richtungJS :: Text
richtungJS = "Richtung"

bahngeschwindigkeitJS :: Text
bahngeschwindigkeitJS = "Bahngeschwindigkeit"

wertJS :: Text
wertJS = "Wert"

fahrtrichtungJS :: Text
fahrtrichtungJS = "Fahrtrichtung"

streckenabschnittJS :: Text
streckenabschnittJS = "Streckenabschnitt"

anJS :: Text
anJS = "Fließend"

kupplungJS :: Text
kupplungJS = "Kupplung"

aktionenJS :: Text
aktionenJS = "Aktionen"

ausführenJS :: Text
ausführenJS = "Ausführen"

planJS :: Text
planJS = "Plan"

dauerschleifeJS :: Text
dauerschleifeJS = "Dauerschleife"

instance FromJSON Aktion where
    parseJSON :: Value -> Parser Aktion
    parseJSON (Object v) = (v .: aktionJS) >>= \s -> if
        | s == wartenJS -> (Warten <$> v .: wertJS)
            <|> parseMaybeWegstrecke
                v
                (\w _v -> pure $ AWSKontakt $ WartenAuf w)
                (\w _v -> pure $ AWSKontakt $ WartenAuf w)
                (\v -> AKontakt . WartenAuf <$> v .: kontaktJS)
        | s == einstellenJS -> (AWegstreckeMärklin . Einstellen <$> v .: wegstreckeJS)
            <|> (AWegstreckeLego . Einstellen <$> v .: wegstreckeJS)
        | s == stellenJS -> AWeiche <$> (Stellen <$> v .: weicheJS <*> v .: richtungJS)
        | s == geschwindigkeitJS -> parseMaybeWegstrecke
            v
            (\w v -> AWSBahngeschwindigkeit
             . GeschwindigkeitPwm
             . Geschwindigkeit (GeschwindigkeitPhantom w)
             <$> v .: wertJS)
            (\w v -> AWSBahngeschwindigkeit
             . GeschwindigkeitPwm
             . Geschwindigkeit (GeschwindigkeitPhantom w)
             <$> v .: wertJS)
            (\v -> (ABahngeschwindigkeitMärklinPwm <$> geschwindigkeitParserMärklin v)
             <|> (ABahngeschwindigkeitLegoPwm <$> geschwindigkeitParserLego v))
        | s == fahrstromJS -> parseMaybeWegstrecke
            v
            (\w v -> AWSBahngeschwindigkeit
             . GeschwindigkeitKonstanteSpannung
             . Fahrstrom (GeschwindigkeitPhantom w)
             <$> v .: stromJS)
            (\w v -> AWSBahngeschwindigkeit
             . GeschwindigkeitKonstanteSpannung
             . Fahrstrom (GeschwindigkeitPhantom w)
             <$> v .: stromJS)
            (\v -> fmap ABahngeschwindigkeitMärklinKonstanteSpannung
             $ Fahrstrom <$> v .: bahngeschwindigkeitJS <*> v .: stromJS)
        | s == umdrehenJS -> parseMaybeWegstrecke
            v
            (\w _v -> pure
             $ AWSBahngeschwindigkeit
             $ GeschwindigkeitPwm
             $ Umdrehen
             $ GeschwindigkeitPhantom w)
            parseFail
            (\v -> (ABahngeschwindigkeitMärklinPwm . Umdrehen <$> v .: bahngeschwindigkeitJS)
             <|> (ABahngeschwindigkeitMärklinKonstanteSpannung . Umdrehen
                  <$> v .: bahngeschwindigkeitJS))
        | s == fahrtrichtungEinstellenJS -> parseMaybeWegstrecke
            v
            parseFail
            (\w v -> AWSBahngeschwindigkeit
             . GeschwindigkeitPwm
             . FahrtrichtungEinstellen (GeschwindigkeitPhantom w)
             <$> v .: fahrtrichtungJS)
            (\v -> (ABahngeschwindigkeitLegoPwm
                    <$> (FahrtrichtungEinstellen <$> v .: bahngeschwindigkeitJS
                         <*> v .: fahrtrichtungJS))
             <|> (ABahngeschwindigkeitLegoKonstanteSpannung
                  <$> (FahrtrichtungEinstellen <$> v .: bahngeschwindigkeitJS
                       <*> v .: fahrtrichtungJS)))
        | s == stromJS -> parseMaybeWegstrecke
            v
            (\w v -> AWSStreckenabschnitt . Strom w <$> v .: anJS)
            (\w v -> AWSStreckenabschnitt . Strom w <$> v .: anJS)
            (\v -> AStreckenabschnitt <$> (Strom <$> v .: streckenabschnittJS <*> v .: anJS))
        | s == kuppelnJS -> parseMaybeWegstrecke
            v
            (\w _v -> pure $ AWSKupplung $ Kuppeln w)
            (\w _v -> pure $ AWSKupplung $ Kuppeln w)
            (\v -> AKupplung . Kuppeln <$> v .: kupplungJS)
        | s == ausführenJS -> AktionAusführen <$> v .: planJS
        | otherwise -> mzero
        where
            parseMaybeWegstrecke
                :: Object
                -> (Wegstrecke 'Märklin
                    -> Object
                    -> Parser (AktionWegstrecke Wegstrecke 'Märklin))
                -> (Wegstrecke 'Lego -> Object -> Parser (AktionWegstrecke Wegstrecke 'Lego))
                -> (Object -> Parser Aktion)
                -> Parser Aktion
            parseMaybeWegstrecke v wsParserMärklin wsParserLego altParser = do
                maybeWegstreckeMärklin <- v .:? wegstreckeJS
                    :: Parser (Maybe (Wegstrecke 'Märklin))
                maybeWegstreckeLego <- v .:? wegstreckeJS :: Parser (Maybe (Wegstrecke 'Lego))
                if
                    | isJust maybeWegstreckeMärklin -> AWegstreckeMärklin
                        <$> wsParserMärklin (fromJust maybeWegstreckeMärklin) v
                    | isJust maybeWegstreckeLego -> AWegstreckeLego
                        <$> wsParserLego (fromJust maybeWegstreckeLego) v
                    | otherwise -> altParser v

            parseFail :: Wegstrecke z -> Object -> Parser (AktionWegstrecke Wegstrecke z)
            parseFail _w _v = mzero

            geschwindigkeitParserMärklin
                :: Object -> Parser (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Pwm 'Märklin)
            geschwindigkeitParserMärklin
                v = Geschwindigkeit <$> v .: bahngeschwindigkeitJS <*> v .: wertJS

            geschwindigkeitParserLego
                :: Object -> Parser (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Pwm 'Lego)
            geschwindigkeitParserLego
                v = Geschwindigkeit <$> v .: bahngeschwindigkeitJS <*> v .: wertJS
    parseJSON _value = mzero

aktionToJSON :: Text -> Aktion -> Value
aktionToJSON _name (Warten wert) = object [aktionJS .= wartenJS, wertJS .= wert]
aktionToJSON _name (AWegstreckeMärklin (Einstellen w)) =
    object [wegstreckeJS .= w, aktionJS .= einstellenJS]
aktionToJSON _name (AWegstreckeLego (Einstellen w)) =
    object [wegstreckeJS .= w, aktionJS .= einstellenJS]
aktionToJSON
    _name
    (AWegstreckeMärklin
         (AWSBahngeschwindigkeit
              (GeschwindigkeitPwm (Geschwindigkeit (GeschwindigkeitPhantom w) wert)))) =
    object [wegstreckeJS .= w, aktionJS .= geschwindigkeitJS, wertJS .= wert]
aktionToJSON
    _name
    (AWegstreckeMärklin
         (AWSBahngeschwindigkeit
              (GeschwindigkeitKonstanteSpannung (Fahrstrom (GeschwindigkeitPhantom w) strom)))) =
    object [wegstreckeJS .= w, aktionJS .= geschwindigkeitJS, stromJS .= strom]
aktionToJSON
    _name
    (AWegstreckeLego
         (AWSBahngeschwindigkeit
              (GeschwindigkeitPwm (Geschwindigkeit (GeschwindigkeitPhantom w) wert)))) =
    object [wegstreckeJS .= w, aktionJS .= geschwindigkeitJS, wertJS .= wert]
aktionToJSON
    _name
    (AWegstreckeLego
         (AWSBahngeschwindigkeit
              (GeschwindigkeitKonstanteSpannung (Fahrstrom (GeschwindigkeitPhantom w) strom)))) =
    object [wegstreckeJS .= w, aktionJS .= geschwindigkeitJS, stromJS .= strom]
aktionToJSON
    _name
    (AWegstreckeMärklin
         (AWSBahngeschwindigkeit (GeschwindigkeitPwm (Umdrehen (GeschwindigkeitPhantom w))))) =
    object [wegstreckeJS .= w, aktionJS .= umdrehenJS]
aktionToJSON
    _name
    (AWegstreckeMärklin
         (AWSBahngeschwindigkeit
              (GeschwindigkeitKonstanteSpannung (Umdrehen (GeschwindigkeitPhantom w))))) =
    object [wegstreckeJS .= w, aktionJS .= umdrehenJS]
aktionToJSON
    _name
    (AWegstreckeLego
         (AWSBahngeschwindigkeit
              (GeschwindigkeitPwm
                   (FahrtrichtungEinstellen (GeschwindigkeitPhantom w) fahrtrichtung)))) =
    object [wegstreckeJS .= w, aktionJS .= umdrehenJS, fahrtrichtungJS .= fahrtrichtung]
aktionToJSON
    _name
    (AWegstreckeLego
         (AWSBahngeschwindigkeit
              (GeschwindigkeitKonstanteSpannung
                   (FahrtrichtungEinstellen (GeschwindigkeitPhantom w) fahrtrichtung)))) =
    object [wegstreckeJS .= w, aktionJS .= umdrehenJS, fahrtrichtungJS .= fahrtrichtung]
aktionToJSON _name (AWegstreckeMärklin (AWSStreckenabschnitt (Strom w an))) =
    object [wegstreckeJS .= w, aktionJS .= stromJS, anJS .= an]
aktionToJSON _name (AWegstreckeLego (AWSStreckenabschnitt (Strom w an))) =
    object [wegstreckeJS .= w, aktionJS .= stromJS, anJS .= an]
aktionToJSON _name (AWegstreckeMärklin (AWSKupplung (Kuppeln w))) =
    object [wegstreckeJS .= w, aktionJS .= kuppelnJS]
aktionToJSON _name (AWegstreckeLego (AWSKupplung (Kuppeln w))) =
    object [wegstreckeJS .= w, aktionJS .= kuppelnJS]
aktionToJSON _name (AWegstreckeMärklin (AWSKontakt (WartenAuf w))) =
    object [wegstreckeJS .= w, aktionJS .= wartenJS]
aktionToJSON _name (AWegstreckeLego (AWSKontakt (WartenAuf w))) =
    object [wegstreckeJS .= w, aktionJS .= wartenJS]
aktionToJSON _name (AWeiche (Stellen w richtung)) =
    object [weicheJS .= w, aktionJS .= stellenJS, richtungJS .= richtung]
aktionToJSON _name (ABahngeschwindigkeitMärklinPwm (Geschwindigkeit b wert)) =
    object [bahngeschwindigkeitJS .= b, aktionJS .= geschwindigkeitJS, wertJS .= wert]
aktionToJSON _name (ABahngeschwindigkeitLegoPwm (Geschwindigkeit b wert)) =
    object [bahngeschwindigkeitJS .= b, aktionJS .= geschwindigkeitJS, wertJS .= wert]
aktionToJSON _name (ABahngeschwindigkeitMärklinKonstanteSpannung (Fahrstrom b strom)) =
    object [bahngeschwindigkeitJS .= b, aktionJS .= fahrstromJS, stromJS .= strom]
aktionToJSON _name (ABahngeschwindigkeitLegoKonstanteSpannung (Fahrstrom b strom)) =
    object [bahngeschwindigkeitJS .= b, aktionJS .= geschwindigkeitJS, stromJS .= strom]
aktionToJSON _name (ABahngeschwindigkeitMärklinPwm (Umdrehen b)) =
    object [bahngeschwindigkeitJS .= b, aktionJS .= umdrehenJS]
aktionToJSON _name (ABahngeschwindigkeitMärklinKonstanteSpannung (Umdrehen b)) =
    object [bahngeschwindigkeitJS .= b, aktionJS .= umdrehenJS]
aktionToJSON _name (ABahngeschwindigkeitLegoPwm (FahrtrichtungEinstellen b fahrtrichtung)) =
    object [bahngeschwindigkeitJS .= b, aktionJS .= umdrehenJS, fahrtrichtungJS .= fahrtrichtung]
aktionToJSON
    _name
    (ABahngeschwindigkeitLegoKonstanteSpannung (FahrtrichtungEinstellen b fahrtrichtung)) =
    object [bahngeschwindigkeitJS .= b, aktionJS .= umdrehenJS, fahrtrichtungJS .= fahrtrichtung]
aktionToJSON _name (AStreckenabschnitt (Strom s an)) =
    object [streckenabschnittJS .= s, aktionJS .= stromJS, anJS .= an]
aktionToJSON _name (AKupplung (Kuppeln k)) = object [kupplungJS .= k, aktionJS .= kuppelnJS]
aktionToJSON _name (AKontakt (WartenAuf k)) = object [kontaktJS .= k, aktionJS .= wartenJS]
aktionToJSON name (AktionAusführen plan@Plan {plName})
    | name == plName =
        -- Verwende Name als Marker für Namensgleichheit (== angenommene Dauerschleife)
        -- Explizit gehandhabt, da sonst die Berechnung des Value nicht terminiert.
        String name
    | otherwise = object [aktionJS .= ausführenJS, planJS .= plan]

instance FromJSON Plan where
    parseJSON :: Value -> Parser Plan
    parseJSON (Object v) = do
        plName <- v .: nameJS
        aktionen <- v .: aktionenJS
        let erzeugeDauerschleife :: Maybe Bool -> Plan
            erzeugeDauerschleife (Just True) =
                let plan = Plan { plName, plAktionen = aktionen <> (AktionAusführen plan :| []) }
                in plan
            erzeugeDauerschleife _NothingOderFalse = Plan { plName, plAktionen = aktionen }
        erzeugeDauerschleife <$> (v .:? dauerschleifeJS)
    parseJSON _value = mzero

instance ( ObjektElement (bg 'Pwm 'Märklin)
         , ObjektTyp (bg 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
         , ObjektElement (bg 'KonstanteSpannung 'Märklin)
         , ObjektTyp (bg 'KonstanteSpannung 'Märklin)
               ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
         , ObjektElement (bg 'Pwm 'Lego)
         , ObjektTyp (bg 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
         , ObjektElement (bg 'KonstanteSpannung 'Lego)
         , ObjektTyp (bg 'KonstanteSpannung 'Lego) ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
         , ObjektElement st
         , ObjektTyp st ~ Streckenabschnitt
         , ObjektElement (we 'Märklin)
         , ObjektTyp (we 'Märklin) ~ Weiche 'Märklin
         , ObjektElement (we 'Lego)
         , ObjektTyp (we 'Lego) ~ Weiche 'Lego
         , ObjektElement ku
         , ObjektTyp ku ~ Kupplung
         , ObjektElement ko
         , ObjektTyp ko ~ Kontakt
         , ObjektElement (ws 'Märklin)
         , ObjektTyp (ws 'Märklin) ~ Wegstrecke 'Märklin
         , ObjektElement (ws 'Lego)
         , ObjektTyp (ws 'Lego) ~ Wegstrecke 'Lego
         ) => ToJSON (PlanAllgemein bg st we ku ko ws) where
    toJSON :: PlanAllgemein bg st we ku ko ws -> Value
    toJSON (zuObjektTyp -> Plan {plName, plAktionen}) =
        let aktionen =
                NonEmpty.takeWhile ((/=) $ String plName) $ fmap (aktionToJSON plName) plAktionen
        in object
               [ nameJS .= plName
               , aktionenJS .= aktionen
               , dauerschleifeJS .= (length aktionen /= length plAktionen)]

-- Hilfsfunktion, um einfache FromJSON-Instanzen zu erstellen
findeÜbereinstimmendenWert :: (ToJSON a) => [a] -> Value -> Parser a
findeÜbereinstimmendenWert [] _v = mzero
findeÜbereinstimmendenWert (h:t) v
    | v == toJSON h = pure h
    | otherwise = findeÜbereinstimmendenWert t v