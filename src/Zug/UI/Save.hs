{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Zug.Anbindung
import Zug.Plan
import Zug.UI.Base

-- | Speichere aktuellen Zustand in Datei
speichern :: (ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl) => StatusAllgemein bg st we ku ws pl -> FilePath -> IO ()
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

newtype AlmostStatus bg st we ku ws pl = AlmostStatus {getStatusFunction :: (MVar PinMap -> StatusAllgemein bg st we ku ws pl)}

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

instance (FromJSON bg, FromJSON st, FromJSON we, FromJSON ku, FromJSON ws, FromJSON pl) => FromJSON (AlmostStatus bg st we ku ws pl) where
    parseJSON :: Value -> Parser (AlmostStatus bg st we ku ws pl)
    parseJSON   (Object v)  = AlmostStatus
                            <$> (Status
                                <$> ((v .: bahngeschwindigkeitenJS) <|> pure [])
                                <*> ((v .: streckenabschnitteJS)    <|> pure [])
                                <*> ((v .: weichenJS)               <|> pure [])
                                <*> ((v .: kupplungenJS)            <|> pure [])
                                <*> ((v .: wegstreckenJS)           <|> pure [])
                                <*> ((v .: pläneJS)                 <|> pure []))
    parseJSON   _           = mzero

instance (ToJSON bahngeschwindigkeit, ToJSON streckenabschnitt, ToJSON weiche, ToJSON kupplung, ToJSON wegstrecke, ToJSON plan) => ToJSON (StatusAllgemein bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan) where
    toJSON :: StatusAllgemein bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan -> Value
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
        createBahngeschwindigkeit zugtyp name geschwindigkeitsPin maybeFahrtrichtungsPin
            where
                createBahngeschwindigkeit :: Zugtyp -> Text -> Natural -> Maybe Natural -> Parser Bahngeschwindigkeit
                createBahngeschwindigkeit Lego      bgName  geschwindigkeitsPin     (Nothing)                   = pure LegoBahngeschwindigkeit {bgName, geschwindigkeitsPin = toPin geschwindigkeitsPin, fahrtrichtungsPin = toPin (0 :: Int)}
                createBahngeschwindigkeit Lego      bgName  geschwindigkeitsPin     (Just fahrtrichtungsPin)    = pure LegoBahngeschwindigkeit {bgName, geschwindigkeitsPin = toPin geschwindigkeitsPin, fahrtrichtungsPin = toPin fahrtrichtungsPin}
                createBahngeschwindigkeit Märklin   bgName  geschwindigkeitsPin     _maybeFahrtrichtungsPin     = pure MärklinBahngeschwindigkeit {bgName, geschwindigkeitsPin = toPin geschwindigkeitsPin}
                createBahngeschwindigkeit _zutyp    _name   _geschwindigkeitsPin    _maybeFahrtrichtungsPin     = mzero
    parseJSON   _           = mzero

instance ToJSON Bahngeschwindigkeit where
    toJSON :: Bahngeschwindigkeit -> Value
    toJSON  (LegoBahngeschwindigkeit {bgName, geschwindigkeitsPin, fahrtrichtungsPin})  = object [nameJS .= bgName, geschwindigkeitsPinJS .= fromPin geschwindigkeitsPin, zugtypJS .= Lego, fahrtrichtungsPinJS .= fromPin fahrtrichtungsPin]
    toJSON  (MärklinBahngeschwindigkeit {bgName, geschwindigkeitsPin})                  = object [nameJS .= bgName, geschwindigkeitsPinJS .= fromPin geschwindigkeitsPin, zugtypJS .= Märklin]

-- neue Feld-Namen/Bezeichner in json-Datei
stromPinJS :: Text
stromPinJS = "StromPin"

-- Instanz-Deklarationen für Streckenabschnitt
instance FromJSON Streckenabschnitt where
    parseJSON :: Value -> Parser Streckenabschnitt
    parseJSON   (Object v)  = do
        stName <- (v .: nameJS)
        stromPin <- (v .: stromPinJS) :: Parser Natural
        pure $ Streckenabschnitt {stName, stromPin=toPin stromPin}
    parseJSON   _           = mzero

instance ToJSON Streckenabschnitt where
    toJSON :: Streckenabschnitt -> Value
    toJSON  (Streckenabschnitt {stName, stromPin})  = object [nameJS .= stName, stromPinJS .= fromPin stromPin]

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
        createWeiche zugtyp name maybeRichtungsPin maybeRichtungen maybeRichtungsPins
            where
                createWeiche :: Zugtyp -> Text -> Maybe Natural -> Maybe (Richtung, Richtung) -> Maybe [(Richtung, Natural)] -> Parser Weiche
                createWeiche Lego       weName   (Just richtungsPin) (Just richtungen)   _maybeRichtungsPins                     = pure LegoWeiche {weName, richtungsPin=toPin richtungsPin, richtungen}
                createWeiche Märklin    weName   _maybeRichtungsPin  _maybeRichtungen    (Just ((richtung, pin):richtungsPins))  = pure MärklinWeiche {weName, richtungsPins=(richtung, toPin pin):|map (\(richtung, pin) -> (richtung, toPin pin)) richtungsPins}
                createWeiche _zugtyp    _name   _maybeRichtungsPin  _maybeRichtungen    _maybeRichtungsPins                     = mzero
    parseJSON   _           = mzero

instance ToJSON Weiche where
    toJSON :: Weiche -> Value
    toJSON  (LegoWeiche {weName, richtungsPin, richtungen})   = object [nameJS .= weName, richtungsPinJS .= fromPin richtungsPin, richtungenJS .= richtungen, zugtypJS .= Lego]
    toJSON  (MärklinWeiche {weName, richtungsPins})           = object [nameJS .= weName, richtungsPinsJS .= map (\(richtung, pin) -> (richtung, fromPin pin)) (NE.toList richtungsPins), zugtypJS .= Märklin]

-- neue Feld-Namen/Bezeichner in json-Datei
kupplungsPinJS :: Text
kupplungsPinJS = "KupplungsPin"

-- Instanz-Deklarationen für Kupplung
instance FromJSON Kupplung where
    parseJSON :: Value -> Parser Kupplung
    parseJSON   (Object v)  = do
        kuName <- (v .: nameJS)
        kupplungsPin <- (v .: kupplungsPinJS) :: Parser Natural
        pure $ Kupplung {kuName, kupplungsPin=toPin kupplungsPin}
    parseJSON   _           = mzero

instance ToJSON Kupplung where
    toJSON :: Kupplung -> Value
    toJSON  (Kupplung {kuName, kupplungsPin}) = object [nameJS .= kuName, kupplungsPinJS .= fromPin kupplungsPin]

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