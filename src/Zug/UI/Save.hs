{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Concurrent.MVar (MVar)
import Control.Monad (MonadPlus(..))
import Data.Aeson (encode, decode)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Value(..), Parser, Object, object, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Numeric.Natural (Natural)
import System.Directory (doesFileExist)
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen
import Zug.Anbindung (PinMap, Bahngeschwindigkeit(..), Streckenabschnitt(..), Weiche(..), Kupplung(..), Wegstrecke(..), zuPin, vonPin)
import qualified Zug.Anbindung as Anbindung
import Zug.Plan
import Zug.UI.Base

-- | Speichere aktuellen Zustand in Datei
speichern :: (ToJSON (BG o), ToJSON (ST o), ToJSON (WE o), ToJSON (KU o), ToJSON (WS o), ToJSON (PL o)) => StatusAllgemein o -> FilePath -> IO ()
speichern contents path = ByteString.writeFile path $ encode contents

-- | Lade Datei.
-- 
-- Dateifehler und nicht-existente Dateien geben Nothing zurück.
-- Ansonsten wird ein Konstruktor für einen aus einem 'Status' konstruiertem Typ zurückgegeben. 
laden :: FilePath -> (Status -> IO s) -> IO (Maybe (MVar PinMap -> IO s))
laden path fromStatus = do
    fileExists <- doesFileExist path
    if fileExists
        then ByteString.readFile path >>= \byteString -> pure $ getStatusFunction <$> decode byteString >>= \f -> Just $ \mvarPinMap -> fromStatus (f mvarPinMap)
        else pure Nothing

newtype AlmostStatus o = AlmostStatus {getStatusFunction :: (MVar PinMap -> StatusAllgemein o)}

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

instance (FromJSON (BG o), FromJSON (ST o), FromJSON (WE o), FromJSON (KU o), FromJSON (WS o), FromJSON (PL o)) => FromJSON (AlmostStatus o) where
    parseJSON :: Value -> Parser (AlmostStatus o)
    parseJSON   (Object v)  = AlmostStatus
                            <$> (Status
                                <$> ((v .: bahngeschwindigkeitenJS) <|> pure [])
                                <*> ((v .: streckenabschnitteJS)    <|> pure [])
                                <*> ((v .: weichenJS)               <|> pure [])
                                <*> ((v .: kupplungenJS)            <|> pure [])
                                <*> ((v .: wegstreckenJS)           <|> pure [])
                                <*> ((v .: pläneJS)                 <|> pure []))
    parseJSON   _           = mzero

instance (ToJSON (BG o), ToJSON (ST o), ToJSON (WE o), ToJSON (KU o), ToJSON (WS o), ToJSON (PL o)) => ToJSON (StatusAllgemein o) where
    toJSON :: StatusAllgemein o -> Value
    toJSON   status  = object [
                            bahngeschwindigkeitenJS .= _bahngeschwindigkeiten status,
                            streckenabschnitteJS    .= _streckenabschnitte status,
                            weichenJS               .= _weichen status,
                            kupplungenJS            .= _kupplungen status,
                            wegstreckenJS           .= _wegstrecken status,
                            pläneJS                 .= _pläne status]

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
    toJSON  (Links)     = String linksJS
    toJSON  (Rechts)    = String rechtsJS
    toJSON  (Gerade)    = String geradeJS
    toJSON  (Kurve)     = String kurveJS

-- neue Feld-Namen/Bezeichner in json-Datei
undefiniertJS :: Text
undefiniertJS = "Undefiniert"
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
    toJSON  (Undefiniert)   = String undefiniertJS
    toJSON  (Märklin)       = String märklinJS
    toJSON  (Lego)          = String legoJS

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
    toJSON  (Vorwärts)  = String vorwärtsJS
    toJSON  (Rückwärts) = String rückwärtsJS

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
    toJSON  (Fließend)  = String fließendJS
    toJSON  (Gesperrt)  = String gesperrtJS

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
    toJSON  (Anbindung.HIGH) = String highJS
    toJSON  (Anbindung.LOW)  = String lowJS

-- neue Feld-Namen/Bezeichner in json-Datei
nameJS :: Text
nameJS = "Name"
zugtypJS :: Text
zugtypJS = "Zugtyp"
geschwindigkeitsPinJS :: Text
geschwindigkeitsPinJS = "GeschwindigkeitsPin"
fahrtrichtungsPinJS :: Text
fahrtrichtungsPinJS = "FahrtrichtungsPin"

-- Instanz-Deklarationen für Bahngeschwindigkeit
instance FromJSON Bahngeschwindigkeit where
    parseJSON :: Value -> Parser Bahngeschwindigkeit
    parseJSON   (Object v)  = do
        name <- (v .: nameJS)
        zugtyp <- (v .: zugtypJS)
        geschwindigkeitsPin <- (v.: geschwindigkeitsPinJS)
        maybeFahrtrichtungsPin <- (v .:? fahrtrichtungsPinJS)
        bgFließend <- (v .: fließendJS) <|> pure Anbindung.HIGH
        createBahngeschwindigkeit zugtyp name bgFließend geschwindigkeitsPin maybeFahrtrichtungsPin
            where
                createBahngeschwindigkeit :: Zugtyp -> Text -> Anbindung.Value -> Natural -> Maybe Natural -> Parser Bahngeschwindigkeit
                createBahngeschwindigkeit Lego      bgName  bgFließend  geschwindigkeitsPin     (Nothing)                   = pure LegoBahngeschwindigkeit {bgName, bgFließend, geschwindigkeitsPin = zuPin geschwindigkeitsPin, fahrtrichtungsPin = zuPin (0 :: Int)}
                createBahngeschwindigkeit Lego      bgName  bgFließend  geschwindigkeitsPin     (Just fahrtrichtungsPin)    = pure LegoBahngeschwindigkeit {bgName, bgFließend, geschwindigkeitsPin = zuPin geschwindigkeitsPin, fahrtrichtungsPin = zuPin fahrtrichtungsPin}
                createBahngeschwindigkeit Märklin   bgName  bgFließend  geschwindigkeitsPin     _maybeFahrtrichtungsPin     = pure MärklinBahngeschwindigkeit {bgName, bgFließend, geschwindigkeitsPin = zuPin geschwindigkeitsPin}
                createBahngeschwindigkeit _zutyp    _name   _bgFließend _geschwindigkeitsPin    _maybeFahrtrichtungsPin     = mzero
    parseJSON   _           = mzero

instance ToJSON Bahngeschwindigkeit where
    toJSON :: Bahngeschwindigkeit -> Value
    toJSON  (LegoBahngeschwindigkeit {bgName, bgFließend, geschwindigkeitsPin, fahrtrichtungsPin})  = object [nameJS .= bgName, fließendJS .= bgFließend, geschwindigkeitsPinJS .= vonPin geschwindigkeitsPin, zugtypJS .= Lego, fahrtrichtungsPinJS .= vonPin fahrtrichtungsPin]
    toJSON  (MärklinBahngeschwindigkeit {bgName, bgFließend, geschwindigkeitsPin})                  = object [nameJS .= bgName, fließendJS .= bgFließend, geschwindigkeitsPinJS .= vonPin geschwindigkeitsPin, zugtypJS .= Märklin]

-- neue Feld-Namen/Bezeichner in json-Datei
stromPinJS :: Text
stromPinJS = "StromPin"

-- Instanz-Deklarationen für Streckenabschnitt
instance FromJSON Streckenabschnitt where
    parseJSON :: Value -> Parser Streckenabschnitt
    parseJSON   (Object v)  = do
        stName <- (v .: nameJS)
        stromPin <- (v .: stromPinJS) :: Parser Natural
        stFließend <- (v .: fließendJS) <|> pure Anbindung.HIGH
        pure Streckenabschnitt {stName, stFließend, stromPin=zuPin stromPin}
    parseJSON   _           = mzero

instance ToJSON Streckenabschnitt where
    toJSON :: Streckenabschnitt -> Value
    toJSON  (Streckenabschnitt {stName, stFließend, stromPin})  = object [nameJS .= stName, fließendJS .= stFließend, stromPinJS .= vonPin stromPin]

-- neue Feld-Namen/Bezeichner in json-Datei
richtungsPinJS :: Text
richtungsPinJS = "RichtungsPin"
richtungenJS :: Text
richtungenJS = "Richtungen"
richtungsPinsJS :: Text
richtungsPinsJS = "RichtungsPins"

-- Instanz-Deklarationen für Weiche
instance FromJSON Weiche where
    parseJSON :: Value -> Parser Weiche
    parseJSON   (Object v)  = do
        name <- (v .: nameJS)
        zugtyp <- (v .: zugtypJS)
        maybeRichtungsPin <- (v .:? richtungsPinJS)
        maybeRichtungen <- (v .:? richtungenJS)
        maybeRichtungsPins <- (v .:? richtungsPinsJS)
        fließend <- (v .: fließendJS) <|> pure Anbindung.HIGH
        erstelleWeiche zugtyp name maybeRichtungsPin maybeRichtungen maybeRichtungsPins fließend
            where
                erstelleWeiche :: Zugtyp -> Text -> Maybe Natural -> Maybe (Richtung, Richtung) -> Maybe [(Richtung, Natural)] -> Anbindung.Value -> Parser Weiche
                erstelleWeiche Lego       weName   (Just richtungsPin) (Just richtungen)   _maybeRichtungsPins                    weFließend   = pure LegoWeiche {weName, weFließend, richtungsPin=zuPin richtungsPin, richtungen}
                erstelleWeiche Märklin    weName   _maybeRichtungsPin  _maybeRichtungen    (Just ((richtung, pin):richtungsPins)) weFließend   = pure MärklinWeiche {weName, weFließend, richtungsPins=(richtung, zuPin pin):|map (\(richtung, pin) -> (richtung, zuPin pin)) richtungsPins}
                erstelleWeiche _zugtyp    _name   _maybeRichtungsPin  _maybeRichtungen    _maybeRichtungsPins                     _weFließend  = mzero
    parseJSON   _           = mzero

instance ToJSON Weiche where
    toJSON :: Weiche -> Value
    toJSON  (LegoWeiche {weName, weFließend, richtungsPin, richtungen}) = object [nameJS .= weName, fließendJS .= weFließend, richtungsPinJS .= vonPin richtungsPin, richtungenJS .= richtungen, zugtypJS .= Lego]
    toJSON  (MärklinWeiche {weName, weFließend, richtungsPins})         = object [nameJS .= weName, fließendJS .= weFließend, richtungsPinsJS .= map (\(richtung, pin) -> (richtung, vonPin pin)) (NE.toList richtungsPins), zugtypJS .= Märklin]

-- neue Feld-Namen/Bezeichner in json-Datei
kupplungsPinJS :: Text
kupplungsPinJS = "KupplungsPin"

-- Instanz-Deklarationen für Kupplung
instance FromJSON Kupplung where
    parseJSON :: Value -> Parser Kupplung
    parseJSON   (Object v)  = do
        kuName <- (v .: nameJS)
        kupplungsPin <- (v .: kupplungsPinJS) :: Parser Natural
        kuFließend <- (v .: fließendJS) <|> pure Anbindung.HIGH
        pure Kupplung {kuName, kuFließend, kupplungsPin=zuPin kupplungsPin}
    parseJSON   _           = mzero

instance ToJSON Kupplung where
    toJSON :: Kupplung -> Value
    toJSON  (Kupplung {kuName, kuFließend, kupplungsPin}) = object [nameJS .= kuName, fließendJS .= kuFließend, kupplungsPinJS .= vonPin kupplungsPin]

-- neue Feld-Namen/Bezeichner in json-Datei
weichenRichtungenJS :: Text
weichenRichtungenJS = "Weichen-Richtungen"

-- Instanz-Deklaration für Wegstrecke
instance FromJSON Wegstrecke where
    parseJSON :: Value -> Parser Wegstrecke
    parseJSON   (Object v)  = Wegstrecke <$> (v .: nameJS) <*> (v .: bahngeschwindigkeitenJS) <*> (v .: streckenabschnitteJS) <*> (v .: weichenRichtungenJS) <*> (v .: kupplungenJS)
    parseJSON   _           = mzero

instance ToJSON Wegstrecke where
    toJSON :: Wegstrecke -> Value
    toJSON  (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen})   = object [nameJS .= wsName, bahngeschwindigkeitenJS .= wsBahngeschwindigkeiten, streckenabschnitteJS .= wsStreckenabschnitte, weichenRichtungenJS .= wsWeichenRichtungen, kupplungenJS .= wsKupplungen]

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
instance (FromJSON bg, FromJSON st, FromJSON we, FromJSON ku, FromJSON ws) => FromJSON (AktionAllgemein bg st we ku ws) where
    parseJSON :: Value -> Parser (AktionAllgemein bg st we ku ws)
    parseJSON   (Object v)  = ((v .: "Aktion") :: Parser Text) >>= \case
        s   | s == wartenJS             -> Warten <$> v .: "Wert"
            | s == einstellenJS         -> AWegstrecke <$> Einstellen <$> v .: wegstreckeJS
            | s == stellenJS            -> AWeiche <$> (Stellen <$> v .: weicheJS <*> v .: richtungJS)
            | s == geschwindigkeitJS    -> parseMaybeWegstrecke v
                (\w v   -> AWSBahngeschwindigkeit <$> Geschwindigkeit w <$> v .: wertJS)
                (\v     -> ABahngeschwindigkeit <$> (Geschwindigkeit <$> v .: bahngeschwindigkeitJS <*> v .: wertJS))
            | s == umdrehenJS           -> parseMaybeWegstrecke v
                (\w v   -> AWSBahngeschwindigkeit <$> Umdrehen w <$> v .:? fahrtrichtungJS)
                (\v     -> ABahngeschwindigkeit <$> (Umdrehen <$> v .: bahngeschwindigkeitJS <*> v .:? fahrtrichtungJS))
            | s == stromJS              -> parseMaybeWegstrecke v
                (\w v   -> AWSStreckenabschnitt <$> Strom w <$> v .: anJS)
                (\v     -> AStreckenabschnitt <$> (Strom <$> v .: streckenabschnittJS <*> v .: anJS))
            | s == kuppelnJS            -> parseMaybeWegstrecke v
                (\w _v  -> pure $ AWSKupplung $ Kuppeln w)
                (\v     -> AKupplung <$> Kuppeln <$> v .: kupplungJS)
            | otherwise                 -> mzero
        where
            parseMaybeWegstrecke :: (FromJSON ws) => Object -> (ws -> Object -> Parser (AktionWegstrecke ws)) -> (Object -> Parser (AktionAllgemein bg st we ku ws)) -> Parser (AktionAllgemein bg st we ku ws)
            parseMaybeWegstrecke    v   wsParser    altParser   = v .:? wegstreckeJS >>= \case
                (Just w)    -> AWegstrecke <$> wsParser w v
                (Nothing)   -> altParser v
    parseJSON   _           = mzero

instance (ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws) => ToJSON (AktionAllgemein bg st we ku ws) where
    toJSON :: AktionAllgemein bg st we ku ws -> Value
    toJSON  (Warten wert)                                                               = object [aktionJS .= wartenJS, wertJS .= wert]
    toJSON  (AWegstrecke (Einstellen w))                                                = object [wegstreckeJS .= w, aktionJS .= einstellenJS]
    toJSON  (AWegstrecke (AWSBahngeschwindigkeit (Geschwindigkeit w wert)))             = object [wegstreckeJS .= w, aktionJS .= geschwindigkeitJS, wertJS .= wert]
    toJSON  (AWegstrecke (AWSBahngeschwindigkeit (Umdrehen w Nothing)))                 = object [wegstreckeJS .= w, aktionJS .= umdrehenJS]
    toJSON  (AWegstrecke (AWSBahngeschwindigkeit (Umdrehen w (Just fahrtrichtung))))    = object [wegstreckeJS .= w, aktionJS .= umdrehenJS, fahrtrichtungJS .= fahrtrichtung]
    toJSON  (AWegstrecke (AWSStreckenabschnitt (Strom w an)))                           = object [wegstreckeJS .= w, aktionJS .= stromJS, anJS .= an]
    toJSON  (AWegstrecke (AWSKupplung (Kuppeln w)))                                     = object [wegstreckeJS .= w, aktionJS .= kuppelnJS]
    toJSON  (AWeiche (Stellen w richtung))                                              = object [weicheJS .= w, aktionJS .= stellenJS, richtungJS .= richtung]
    toJSON  (ABahngeschwindigkeit (Geschwindigkeit b wert))                             = object [bahngeschwindigkeitJS .= b, aktionJS .= geschwindigkeitJS, wertJS .= wert]
    toJSON  (ABahngeschwindigkeit (Umdrehen b maybeFahrtrichtung))                      = object [bahngeschwindigkeitJS .= b, aktionJS .= umdrehenJS, fahrtrichtungJS .= maybeFahrtrichtung]
    toJSON  (AStreckenabschnitt (Strom s an))                                           = object [streckenabschnittJS .= s, aktionJS .= stromJS, anJS .= an]
    toJSON  (AKupplung (Kuppeln k))                                                     = object [kupplungJS .= k, aktionJS .= kuppelnJS]

instance (FromJSON bg, FromJSON st, FromJSON we, FromJSON ku, FromJSON ws) => FromJSON (PlanAllgemein bg st we ku ws) where
    parseJSON :: Value -> Parser (PlanAllgemein bg st we ku ws)
    parseJSON   (Object v)  = Plan <$> (v .: nameJS) <*> (v .: aktionenJS)
    parseJSON   _           = mzero

instance (ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws) => ToJSON (PlanAllgemein bg st we ku ws) where
    toJSON :: PlanAllgemein bg st we ku ws -> Value
    toJSON  (Plan {plName, plAktionen}) = object [nameJS .= plName, aktionenJS .= plAktionen]

-- Hilfsfunktion, um einfache FromJSON-Instanzen zu erstellen
findeÜbereinstimmendenWert :: (ToJSON a) => [a] -> Value -> Parser a
findeÜbereinstimmendenWert  ([])    _v  = mzero
findeÜbereinstimmendenWert  (h:t)   v
    | v == toJSON h             = pure h
    | otherwise                 = findeÜbereinstimmendenWert t v