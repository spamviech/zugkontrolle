{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description : Speichern und laden
-}
module Zug.UI.Save (
                -- * Speichern & Laden
                speichern, laden) where

-- Bibliotheken
import Control.Applicative (Alternative(..))
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad (MonadPlus(..))
import Data.Aeson (encode, decode)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Value(..), Parser, Object, object, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import Numeric.Natural (Natural)
import System.Directory (doesFileExist)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (PwmMap, I2CMap, Wartezeit(..),
            Anschluss(..), PCF8574Port(..), PCF8574(..), PCF8574Variant(..), vonPinGpio, zuPinGpio,
            Bahngeschwindigkeit(..), Streckenabschnitt(..), Weiche(..), Kupplung(..), Wegstrecke(..))
import qualified Zug.Anbindung as Anbindung
import Zug.Klassen (Richtung(..), Zugtyp(..), ZugtypEither(..), Fahrtrichtung(..), Strom(..))
import Zug.Menge (Menge, leer)
import Zug.Plan (ObjektKlasse(), ausBG, ausST, ausWE, ausKU, ausWS, ausPL, Ausführend(), ObjektAllgemein(..),
                Aktion(..), AktionWegstrecke(..), AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..),
                AktionWeiche(..), AktionKupplung(..), Plan(..))
import Zug.UI.Base (StatusAllgemein(..), Status, phantom)

-- | Speichere aktuellen Zustand in Datei
speichern :: (ObjektKlasse o, ToJSON o) => StatusAllgemein o -> FilePath -> IO ()
speichern contents path = ByteString.writeFile path $ encode contents

-- | Lade Datei.
-- 
-- Dateifehler und nicht-existente Dateien geben Nothing zurück.
-- Ansonsten wird ein Konstruktor für einen aus einem 'Status' konstruiertem Typ zurückgegeben.
laden :: FilePath -> (Status -> IO (s o)) -> IO (Maybe (TVar PwmMap -> TVar I2CMap -> IO (s o)))
laden path fromStatus = do
    fileExists <- doesFileExist path
    if fileExists
        then do
            byteString <- ByteString.readFile path
            tvarAusführend <- newTVarIO leer
            pure $ getStatusFunction <$> decode byteString >>= \f -> Just $ \tvarPwmMap tvarI2CMap -> fromStatus (f tvarAusführend tvarPwmMap tvarI2CMap)
        else pure Nothing

newtype AlmostStatus = AlmostStatus {
    getStatusFunction :: (TVar (Menge Ausführend) -> TVar PwmMap -> TVar I2CMap -> Status)}

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
wegstreckenJS :: Text
wegstreckenJS = "Wegstrecken"
pläneJS :: Text
pläneJS = "Pläne"

instance FromJSON AlmostStatus where
    parseJSON :: Value -> Parser AlmostStatus
    parseJSON   (Object v)
        = AlmostStatus <$> (Status
            <$> ((v .: bahngeschwindigkeitenJS) <|> pure [])
            <*> ((v .: streckenabschnitteJS)    <|> pure [])
            <*> ((v .: weichenJS)               <|> pure [])
            <*> ((v .: kupplungenJS)            <|> pure [])
            <*> ((v .: wegstreckenJS)           <|> pure [])
            <*> ((v .: pläneJS)                 <|> pure []))
    parseJSON _value
        = mzero

instance (ObjektKlasse o, ToJSON o) => ToJSON (StatusAllgemein o) where
    toJSON :: StatusAllgemein o -> Value
    toJSON status = object [
        bahngeschwindigkeitenJS .= (ausBG (phantom status) <$> (_bahngeschwindigkeiten status)),
        streckenabschnitteJS    .= (ausST (phantom status) <$> (_streckenabschnitte status)),
        weichenJS               .= (ausWE (phantom status) <$> (_weichen status)),
        kupplungenJS            .= (ausKU (phantom status) <$> (_kupplungen status)),
        wegstreckenJS           .= (ausWS (phantom status) <$> (_wegstrecken status)),
        pläneJS                 .= (ausPL (phantom status) <$> (_pläne status))]

instance (FromJSON (bg 'Märklin), FromJSON (bg 'Lego), FromJSON st, FromJSON (we 'Märklin), FromJSON (we 'Lego),
        FromJSON ku, FromJSON (ws 'Lego), FromJSON (ws 'Märklin), FromJSON pl)
            => FromJSON (ObjektAllgemein bg st we ku ws pl) where
    parseJSON :: Value -> Parser (ObjektAllgemein bg st we ku ws pl)
    parseJSON value = OBahngeschwindigkeit <$> parseJSON value
        <|> OStreckenabschnitt <$> parseJSON value
        <|> OWeiche <$> parseJSON value
        <|> OKupplung <$> parseJSON value
        <|> OWegstrecke <$> parseJSON value
        <|> OPlan <$> parseJSON value

instance (ToJSON (bg 'Märklin), ToJSON (bg 'Lego), ToJSON st, ToJSON (we 'Märklin), ToJSON (we 'Lego),
        ToJSON ku, ToJSON (ws 'Märklin), ToJSON (ws 'Lego), ToJSON pl)
            => ToJSON (ObjektAllgemein bg st we ku ws pl) where
    toJSON :: ObjektAllgemein bg st we ku ws pl -> Value
    toJSON  (OBahngeschwindigkeit bg)   = toJSON bg
    toJSON  (OStreckenabschnitt st)     = toJSON st
    toJSON  (OWeiche we)                = toJSON we
    toJSON  (OKupplung ku)              = toJSON ku
    toJSON  (OWegstrecke ws)            = toJSON ws
    toJSON  (OPlan pl)                  = toJSON pl

instance (FromJSON (a 'Märklin), FromJSON (a 'Lego)) => FromJSON (ZugtypEither a) where
    parseJSON :: Value -> Parser (ZugtypEither a)
    parseJSON value = (ZugtypMärklin <$> parseJSON value) <|> (ZugtypLego <$> parseJSON value)

instance (ToJSON (a 'Märklin), ToJSON (a 'Lego)) => ToJSON (ZugtypEither a) where
    toJSON :: ZugtypEither a -> Value
    toJSON  (ZugtypMärklin a)   = toJSON a
    toJSON  (ZugtypLego a)      = toJSON a

-- neue Feld-Namen/Bezeichner in json-Datei
pinJS :: Text
pinJS = "Pin"
portJS :: Text
portJS = "PCF8574Port"

-- Instanz-Deklarationen für Anschluss
instance FromJSON Anschluss where
    parseJSON :: Value -> Parser Anschluss
    parseJSON   (Object v)  = (vonPinGpio <$> (v .: pinJS :: Parser Int))
                            <|> (AnschlussPCF8574Port <$> v .: portJS)
    parseJSON   _value      = mzero

instance ToJSON Anschluss where
    toJSON :: Anschluss -> Value
    toJSON  anschluss@(AnschlussPin _pin)   = object [pinJS .= (fromJust $ zuPinGpio anschluss :: Int)]
    toJSON  (AnschlussPCF8574Port port)     = object [portJS .= port]

instance FromJSON PCF8574Port where
    parseJSON :: Value -> Parser PCF8574Port
    parseJSON = _parseJSON_PCF8574Port

instance  ToJSON PCF8574Port where
    toJSON :: PCF8574Port -> Value
    toJSON = _toJSON_PCF8574Port

-- Instanz-Deklarationen für Wartezeit
instance FromJSON Wartezeit where
    parseJSON :: Value -> Parser Wartezeit
    parseJSON = _parseJSON_Wartezeit

instance ToJSON Wartezeit where
    toJSON :: Wartezeit -> Value
    toJSON = _toJSON_Wartezeit

-- neue Feld-Namen/Bezeichner in json-Datei
geradeJS :: Text
geradeJS = "Gerade"
kurveJS :: Text
kurveJS = "Kurve"
linksJS :: Text
linksJS = "Links"
rechtsJS :: Text
rechtsJS = "Rechts"

-- Instanz-Deklaration für Richtung
instance FromJSON Richtung where
    parseJSON :: Value -> Parser Richtung
    parseJSON = findeÜbereinstimmendenWert [minBound..maxBound]

instance ToJSON Richtung where
    toJSON :: Richtung -> Value
    toJSON  Links   = String linksJS
    toJSON  Rechts  = String rechtsJS
    toJSON  Gerade  = String geradeJS
    toJSON  Kurve   = String kurveJS

-- neue Feld-Namen/Bezeichner in json-Datei
märklinJS :: Text
märklinJS = "Märklin"
legoJS :: Text
legoJS = "Lego"

-- Instanz-Deklaration für Zugtyp
instance FromJSON Zugtyp where
    parseJSON :: Value -> Parser Zugtyp
    parseJSON = findeÜbereinstimmendenWert [minBound..maxBound]

instance ToJSON Zugtyp where
    toJSON :: Zugtyp -> Value
    toJSON  Märklin     = String märklinJS
    toJSON  Lego        = String legoJS

-- neue Feld-Namen/Bezeichner in json-Datei
vorwärtsJS :: Text
vorwärtsJS = "Vorwärts"
rückwärtsJS :: Text
rückwärtsJS = "Rückwärts"

-- Instanz-Deklaration für Fahrtrichtung
instance FromJSON Fahrtrichtung where
    parseJSON :: Value -> Parser Fahrtrichtung
    parseJSON = findeÜbereinstimmendenWert [minBound..maxBound]

instance ToJSON Fahrtrichtung where
    toJSON :: Fahrtrichtung -> Value
    toJSON  Vorwärts    = String vorwärtsJS
    toJSON  Rückwärts   = String rückwärtsJS

-- neue Feld-Namen/Bezeichner in json-Datei
fließendJS :: Text
fließendJS = "Fließend"
gesperrtJS :: Text
gesperrtJS = "Gesperrt"

-- Instanz-Deklaration für Strom
instance FromJSON Strom where
    parseJSON :: Value -> Parser Strom
    parseJSON = findeÜbereinstimmendenWert [minBound..maxBound]

instance ToJSON Strom where
    toJSON :: Strom -> Value
    toJSON  Fließend    = String fließendJS
    toJSON  Gesperrt    = String gesperrtJS

-- Instanz-Deklaration für Value
instance FromJSON Anbindung.Value where
    parseJSON :: Value -> Parser Anbindung.Value
    parseJSON = findeÜbereinstimmendenWert [minBound..maxBound]

highJS :: Text
highJS = "HIGH"
lowJS :: Text
lowJS = "LOW"

instance ToJSON Anbindung.Value where
    toJSON :: Anbindung.Value -> Value
    toJSON  Anbindung.HIGH  = String highJS
    toJSON  Anbindung.LOW   = String lowJS

-- neue Feld-Namen/Bezeichner in json-Datei
nameJS :: Text
nameJS = "Name"
zugtypJS :: Text
zugtypJS = "Zugtyp"
geschwindigkeitsPinJS :: Text
geschwindigkeitsPinJS = "GeschwindigkeitsPin"
fahrtrichtungsPinJS :: Text
fahrtrichtungsPinJS = "FahrtrichtungsPin"
geschwindigkeitsAnschlussJS :: Text
geschwindigkeitsAnschlussJS = "GeschwindigkeitsPin"
fahrtrichtungsAnschlussJS :: Text
fahrtrichtungsAnschlussJS = "FahrtrichtungsPin"

-- | Parse einen Anschluss.
-- Dabei wird eine Rückwärtskompatibilität zu Versionen < 1.0.1 berücksichtigt.
-- Bei diesen war nur ein Pin-Anschluss erlaubt, weshalb die JSON-Felder anders hießen.
parseAnschluss :: Object -> Text -> Text -> Parser Anschluss
parseAnschluss v anschlussJS pinJS = (v .: anschlussJS) <|> (vonPinGpio <$> (v .: pinJS :: Parser Natural))

-- | Parse das Fließend-Feld.
-- Dabei wird eine Rückwärtskompatibilität zu Versionen <1.0.0.14 berücksichtigt.
-- Bei diesen wurde intern immer 'HIGH' angenommen.
parseFließend :: Object -> Parser Anbindung.Value
parseFließend v = (v .: fließendJS) <|> pure Anbindung.HIGH

-- Instanz-Deklarationen für Bahngeschwindigkeit
instance FromJSON (Bahngeschwindigkeit 'Märklin) where
    parseJSON :: Value -> Parser (Bahngeschwindigkeit 'Märklin)
    parseJSON   (Object v)  = do
        Märklin <- v .: zugtypJS
        bgmName <- v .: nameJS
        bgmGeschwindigkeitsAnschluss <- parseAnschluss v geschwindigkeitsAnschlussJS geschwindigkeitsPinJS
        bgmFließend <- parseFließend v
        pure MärklinBahngeschwindigkeit {bgmName, bgmFließend, bgmGeschwindigkeitsAnschluss}
    parseJSON   _value      = mzero
instance FromJSON (Bahngeschwindigkeit 'Lego) where
    parseJSON :: Value -> Parser (Bahngeschwindigkeit 'Lego)
    parseJSON   (Object v)  = do
        Lego <- v .: zugtypJS
        bglName <- v .: nameJS
        bglGeschwindigkeitsAnschluss <- parseAnschluss v geschwindigkeitsAnschlussJS geschwindigkeitsPinJS
        bglFahrtrichtungsAnschluss <- parseAnschluss v fahrtrichtungsAnschlussJS fahrtrichtungsPinJS
        bglFließend <- parseFließend v
        pure LegoBahngeschwindigkeit {bglName, bglFließend, bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss}
    parseJSON   _value      = mzero

instance ToJSON (Bahngeschwindigkeit z) where
    toJSON :: Bahngeschwindigkeit z -> Value
    toJSON  (LegoBahngeschwindigkeit {bglName, bglFließend, bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss})
        = object [
            nameJS .= bglName,
            fließendJS .= bglFließend,
            geschwindigkeitsPinJS .= bglGeschwindigkeitsAnschluss,
            zugtypJS .= Lego,
            fahrtrichtungsPinJS .= bglFahrtrichtungsAnschluss]
    toJSON  (MärklinBahngeschwindigkeit {bgmName, bgmFließend, bgmGeschwindigkeitsAnschluss})
        = object [
            nameJS .= bgmName,
            fließendJS .= bgmFließend,
            geschwindigkeitsPinJS .= bgmGeschwindigkeitsAnschluss,
            zugtypJS .= Märklin]

-- neue Feld-Namen/Bezeichner in json-Datei
stromPinJS :: Text
stromPinJS = "StromPin"
stromAnschlussJS :: Text
stromAnschlussJS = "StromPin"

-- Instanz-Deklarationen für Streckenabschnitt
instance FromJSON Streckenabschnitt where
    parseJSON :: Value -> Parser Streckenabschnitt
    parseJSON   (Object v)
        = Streckenabschnitt
            <$> v .: nameJS
            <*> parseFließend v
            <*> parseAnschluss v stromAnschlussJS stromPinJS
    parseJSON   _value
        = mzero

instance ToJSON Streckenabschnitt where
    toJSON :: Streckenabschnitt -> Value
    toJSON (Streckenabschnitt {stName, stFließend, stromAnschluss})
        = object [
            nameJS .= stName,
            fließendJS .= stFließend,
            stromPinJS .= stromAnschluss]

-- neue Feld-Namen/Bezeichner in json-Datei
richtungsPinJS :: Text
richtungsPinJS = "RichtungsPin"
richtungenJS :: Text
richtungenJS = "Richtungen"
richtungsPinsJS :: Text
richtungsPinsJS = "RichtungsPins"

-- Instanz-Deklarationen für Weiche
instance FromJSON (Weiche 'Märklin) where
    parseJSON :: Value -> Parser (Weiche 'Märklin)
    parseJSON   (Object v)  = do
        Märklin <- v .: zugtypJS
        wemName <- v .: nameJS
        wemRichtungsAnschlüsse <- v .: richtungsPinsJS
        wemFließend <- parseFließend v
        pure MärklinWeiche {wemName, wemFließend, wemRichtungsAnschlüsse}
    parseJSON   _value      = mzero
instance FromJSON (Weiche 'Lego) where
    parseJSON :: Value -> Parser (Weiche 'Lego)
    parseJSON   (Object v)  = do
        Lego <- v .: zugtypJS
        welName <- v .: nameJS
        welRichtungsAnschluss <- v .: richtungsPinJS
        welRichtungen <- v .: richtungenJS
        welFließend <- parseFließend v
        pure LegoWeiche {welName, welFließend, welRichtungsAnschluss, welRichtungen}
    parseJSON   _value      = mzero

instance ToJSON (Weiche z) where
    toJSON :: Weiche z -> Value
    toJSON  (LegoWeiche {welName, welFließend, welRichtungsAnschluss, welRichtungen})
        = object [
            nameJS .= welName,
            fließendJS .= welFließend,
            richtungsPinJS .= welRichtungsAnschluss,
            richtungenJS .= welRichtungen,
            zugtypJS .= Lego]
    toJSON  (MärklinWeiche {wemName, wemFließend, wemRichtungsAnschlüsse})
        = object [
            nameJS .= wemName,
            fließendJS .= wemFließend,
            richtungsPinsJS .= NE.toList wemRichtungsAnschlüsse,
            zugtypJS .= Märklin]

-- neue Feld-Namen/Bezeichner in json-Datei
kupplungsPinJS :: Text
kupplungsPinJS = "KupplungsPin"
kupplungsAnschlussJS :: Text
kupplungsAnschlussJS = "KupplungsAnschluss"

-- Instanz-Deklarationen für Kupplung
instance FromJSON Kupplung where
    parseJSON :: Value -> Parser Kupplung
    parseJSON   (Object v)
        = Kupplung
            <$> v .: nameJS
            <*> parseFließend v
            <*> parseAnschluss v kupplungsAnschlussJS kupplungsPinJS
    parseJSON   _value
        = mzero

instance ToJSON Kupplung where
    toJSON :: Kupplung -> Value
    toJSON  (Kupplung {kuName, kuFließend, kupplungsAnschluss})
        = object [
            nameJS .= kuName,
            fließendJS .= kuFließend,
            kupplungsAnschlussJS .= kupplungsAnschluss]

-- neue Feld-Namen/Bezeichner in json-Datei
weichenRichtungenJS :: Text
weichenRichtungenJS = "Weichen-Richtungen"

-- Instanz-Deklaration für Wegstrecke
instance FromJSON (Wegstrecke 'Märklin) where
    parseJSON :: Value -> Parser (Wegstrecke 'Märklin)
    parseJSON   (Object v)
        = Wegstrecke
            <$> (v .: nameJS)
            <*> (v .: bahngeschwindigkeitenJS)
            <*> (v .: streckenabschnitteJS)
            <*> (v .: weichenRichtungenJS)
            <*> (v .: kupplungenJS)
    parseJSON   _value
        = mzero
instance FromJSON (Wegstrecke 'Lego) where
    parseJSON :: Value -> Parser (Wegstrecke 'Lego)
    parseJSON   (Object v)
        = Wegstrecke
            <$> (v .: nameJS)
            <*> (v .: bahngeschwindigkeitenJS)
            <*> (v .: streckenabschnitteJS)
            <*> (v .: weichenRichtungenJS)
            <*> (v .: kupplungenJS)
    parseJSON   _value
        = mzero

instance ToJSON (Wegstrecke z) where
    toJSON :: Wegstrecke z -> Value
    toJSON (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen})
        = object [
            nameJS .= wsName,
            bahngeschwindigkeitenJS .= wsBahngeschwindigkeiten,
            streckenabschnitteJS .= wsStreckenabschnitte,
            weichenRichtungenJS .= wsWeichenRichtungen,
            kupplungenJS .= wsKupplungen]

-- neue Feld-Namen/Bezeichner in json-Datei
wartenJS :: Text
wartenJS = "Warten"
einstellenJS :: Text
einstellenJS = "Einstellen"
stellenJS :: Text
stellenJS = "Stellen"
geschwindigkeitJS :: Text
geschwindigkeitJS = "Geschwindigkeit"
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

-- Instanz-Deklaration für Plan
instance FromJSON Aktion where
    parseJSON :: Value -> Parser Aktion
    parseJSON   (Object v)  = (v .: "Aktion") >>= \s -> if
        | s == wartenJS
            -> Warten <$> v .: "Wert"
        | s == einstellenJS
            -> (AWegstreckeMärklin <$> Einstellen <$> v .: wegstreckeJS)
                <|> (AWegstreckeLego <$> Einstellen <$> v .: wegstreckeJS)
        | s == stellenJS
            -> AWeiche <$> (Stellen <$> v .: weicheJS <*> v .: richtungJS)
        | s == geschwindigkeitJS
            -> parseMaybeWegstrecke v
                (\w v   -> AWSBahngeschwindigkeit <$> Geschwindigkeit w <$> v .: wertJS)
                (\w v   -> AWSBahngeschwindigkeit <$> Geschwindigkeit w <$> v .: wertJS)
                (\v     -> (ABahngeschwindigkeitMärklin <$> geschwindigkeitParserMärklin v)
                            <|> (ABahngeschwindigkeitLego <$> geschwindigkeitParserLego v))
        | s == umdrehenJS
            -> parseMaybeWegstrecke v
                (\w _v  -> pure $ AWSBahngeschwindigkeit $ Umdrehen w)
                parseFail
                (\v     -> ABahngeschwindigkeitMärklin <$> Umdrehen <$> v .: bahngeschwindigkeitJS)
        | s == fahrtrichtungEinstellenJS
            -> parseMaybeWegstrecke v
                parseFail
                (\w v   -> AWSBahngeschwindigkeit <$> FahrtrichtungEinstellen w <$> v .: fahrtrichtungJS)
                (\v     -> ABahngeschwindigkeitLego <$>
                            (FahrtrichtungEinstellen <$> v .: bahngeschwindigkeitJS <*> v .: fahrtrichtungJS))
        | s == stromJS
            -> parseMaybeWegstrecke v
                (\w v   -> AWSStreckenabschnitt <$> Strom w <$> v .: anJS)
                (\w v   -> AWSStreckenabschnitt <$> Strom w <$> v .: anJS)
                (\v     -> AStreckenabschnitt <$> (Strom <$> v .: streckenabschnittJS <*> v .: anJS))
        | s == kuppelnJS
            -> parseMaybeWegstrecke v
                (\w _v  -> pure $ AWSKupplung $ Kuppeln w)
                (\w _v  -> pure $ AWSKupplung $ Kuppeln w)
                (\v     -> AKupplung <$> Kuppeln <$> v .: kupplungJS)
        | otherwise
            -> mzero
        where
            parseMaybeWegstrecke ::
                Object ->
                (Wegstrecke 'Märklin -> Object -> Parser (AktionWegstrecke Wegstrecke 'Märklin)) ->
                (Wegstrecke 'Lego -> Object -> Parser (AktionWegstrecke Wegstrecke 'Lego)) ->
                (Object -> Parser Aktion) ->
                    Parser Aktion
            parseMaybeWegstrecke v wsParserMärklin wsParserLego altParser = do
                maybeWegstreckeMärklin <- v .:? wegstreckeJS :: Parser (Maybe (Wegstrecke 'Märklin))
                maybeWegstreckeLego <- v .:? wegstreckeJS :: Parser (Maybe (Wegstrecke 'Lego))
                if
                    | isJust maybeWegstreckeMärklin
                        -> AWegstreckeMärklin <$> wsParserMärklin (fromJust maybeWegstreckeMärklin) v
                    | isJust maybeWegstreckeLego
                        -> AWegstreckeLego <$> wsParserLego (fromJust maybeWegstreckeLego) v
                    | otherwise
                        -> altParser v
            parseFail :: Wegstrecke z -> Object -> Parser (AktionWegstrecke Wegstrecke z)
            parseFail _w _v = mzero
            geschwindigkeitParserMärklin :: Object -> Parser (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Märklin)
            geschwindigkeitParserMärklin v = Geschwindigkeit <$> v .: bahngeschwindigkeitJS <*> v .: wertJS
            geschwindigkeitParserLego :: Object -> Parser (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Lego)
            geschwindigkeitParserLego v = Geschwindigkeit <$> v .: bahngeschwindigkeitJS <*> v .: wertJS
    parseJSON   _value      = mzero

instance ToJSON Aktion where
    toJSON :: Aktion -> Value
    toJSON  (Warten wert)
        = object [aktionJS .= wartenJS, wertJS .= wert]
    toJSON  (AWegstreckeMärklin (Einstellen w))
        = object [wegstreckeJS .= w, aktionJS .= einstellenJS]
    toJSON  (AWegstreckeLego (Einstellen w))
        = object [wegstreckeJS .= w, aktionJS .= einstellenJS]
    toJSON  (AWegstreckeMärklin (AWSBahngeschwindigkeit (Geschwindigkeit w wert)))
        = object [wegstreckeJS .= w, aktionJS .= geschwindigkeitJS, wertJS .= wert]
    toJSON  (AWegstreckeLego (AWSBahngeschwindigkeit (Geschwindigkeit w wert)))
        = object [wegstreckeJS .= w, aktionJS .= geschwindigkeitJS, wertJS .= wert]
    toJSON  (AWegstreckeMärklin (AWSBahngeschwindigkeit (Umdrehen w)))
        = object [wegstreckeJS .= w, aktionJS .= umdrehenJS]
    toJSON  (AWegstreckeLego (AWSBahngeschwindigkeit (FahrtrichtungEinstellen w fahrtrichtung)))
        = object [wegstreckeJS .= w, aktionJS .= umdrehenJS, fahrtrichtungJS .= fahrtrichtung]
    toJSON  (AWegstreckeMärklin (AWSStreckenabschnitt (Strom w an)))
        = object [wegstreckeJS .= w, aktionJS .= stromJS, anJS .= an]
    toJSON  (AWegstreckeLego (AWSStreckenabschnitt (Strom w an)))
        = object [wegstreckeJS .= w, aktionJS .= stromJS, anJS .= an]
    toJSON  (AWegstreckeMärklin (AWSKupplung (Kuppeln w)))
        = object [wegstreckeJS .= w, aktionJS .= kuppelnJS]
    toJSON  (AWegstreckeLego (AWSKupplung (Kuppeln w)))
        = object [wegstreckeJS .= w, aktionJS .= kuppelnJS]
    toJSON  (AWeiche (Stellen w richtung))
        = object [weicheJS .= w, aktionJS .= stellenJS, richtungJS .= richtung]
    toJSON  (ABahngeschwindigkeitMärklin (Geschwindigkeit b wert))
        = object [bahngeschwindigkeitJS .= b, aktionJS .= geschwindigkeitJS, wertJS .= wert]
    toJSON  (ABahngeschwindigkeitLego (Geschwindigkeit b wert))
        = object [bahngeschwindigkeitJS .= b, aktionJS .= geschwindigkeitJS, wertJS .= wert]
    toJSON  (ABahngeschwindigkeitMärklin (Umdrehen b))
        = object [bahngeschwindigkeitJS .= b, aktionJS .= umdrehenJS]
    toJSON  (ABahngeschwindigkeitLego (FahrtrichtungEinstellen b fahrtrichtung))
        = object [bahngeschwindigkeitJS .= b, aktionJS .= umdrehenJS, fahrtrichtungJS .= fahrtrichtung]
    toJSON  (AStreckenabschnitt (Strom s an))
        = object [streckenabschnittJS .= s, aktionJS .= stromJS, anJS .= an]
    toJSON  (AKupplung (Kuppeln k))
        = object [kupplungJS .= k, aktionJS .= kuppelnJS]

instance FromJSON Plan where
    parseJSON :: Value -> Parser Plan
    parseJSON   (Object v)  = Plan <$> (v .: nameJS) <*> (v .: aktionenJS)
    parseJSON   _value      = mzero

instance ToJSON Plan where
    toJSON :: Plan -> Value
    toJSON  (Plan {plName, plAktionen}) = object [nameJS .= plName, aktionenJS .= plAktionen]

-- Hilfsfunktion, um einfache FromJSON-Instanzen zu erstellen
findeÜbereinstimmendenWert :: (ToJSON a) => [a] -> Value -> Parser a
findeÜbereinstimmendenWert  ([])    _v  = mzero
findeÜbereinstimmendenWert  (h:t)   v
    | v == toJSON h             = pure h
    | otherwise                 = findeÜbereinstimmendenWert t v