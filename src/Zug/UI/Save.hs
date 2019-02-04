{-# LANGUAGE NamedFieldPuns, OverloadedStrings, InstanceSigs, LambdaCase, MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description : Speichern und laden
-}
module Zug.UI.Save (
                -- * Speichern & Laden
                save, load) where

-- Bibliotheken
import Control.Concurrent.MVar
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Numeric.Natural
import System.Directory
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen
import Zug.Anbindung
import Zug.Plan
import Zug.UI.Base

-- | Speichere aktuellen Zustand in Datei
save :: (ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws, ToJSON pl) => StatusGeneral bg st we ku ws pl -> FilePath -> IO ()
save contents path = ByteString.writeFile path $ encode contents

-- | Lade Datei
-- Dateifehler und nicht-existente Dateien geben Nothing zurück.
-- Ansonsten wird ein Konstruktor für einen aus einem 'Status' konstruiertem Typ zurückgegeben. 
load :: FilePath -> (Status -> IO s) -> IO (Maybe (MVar PinMap -> IO s))
load path fromStatus = do
    fileExists <- doesFileExist path
    if fileExists
        then ByteString.readFile path >>= \byteString -> pure $ getStatusFunction <$> decode byteString >>= \f -> Just $ \mvarPinMap -> fromStatus (f mvarPinMap)
        else pure Nothing

newtype AlmostStatus bg st we ku ws pl = AlmostStatus {getStatusFunction :: (MVar PinMap -> StatusGeneral bg st we ku ws pl)}

instance (FromJSON bg, FromJSON st, FromJSON we, FromJSON ku, FromJSON ws, FromJSON pl) => FromJSON (AlmostStatus bg st we ku ws pl) where
    parseJSON :: Value -> Parser (AlmostStatus bg st we ku ws pl)
    parseJSON   (Object v)  = AlmostStatus <$> (Status <$> (v .: "Bahngeschwindigkeiten") <*> (v .: "Streckenabschnitte") <*> (v .: "Weichen") <*> (v .: "Kupplungen") <*> (v .: "Wegstrecken") <*> (v .: "Pläne"))
    parseJSON   _           = mzero

instance (ToJSON bahngeschwindigkeit, ToJSON streckenabschnitt, ToJSON weiche, ToJSON kupplung, ToJSON wegstrecke, ToJSON plan) => ToJSON (StatusGeneral bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan) where
    toJSON :: StatusGeneral bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan -> Value
    toJSON   status  = object [
                            "Bahngeschwindigkeiten" .= _bahngeschwindigkeiten status,
                            "Streckenabschnitte"    .= _streckenabschnitte status,
                            "Weichen"               .= _weichen status,
                            "Kupplungen"            .= _kupplungen status,
                            "Wegstrecken"           .= _wegstrecken status,
                            "Pläne"                 .= _pläne status]

-- Instanz-Deklaration für Richtung
instance FromJSON Richtung where
    parseJSON :: Value -> Parser Richtung
    parseJSON   (String s)  = returnMatchingShow (NE.toList unterstützteRichtungen) s
    parseJSON   _           = mzero

instance ToJSON Richtung where
    toJSON :: Richtung -> Value
    toJSON  richtung    = String $ pack $ show richtung

-- Instanz-Deklaration für Zugtyp
instance FromJSON Zugtyp where
    parseJSON :: Value -> Parser Zugtyp
    parseJSON   (String s)  = returnMatchingShow (Undefiniert:(NE.toList unterstützteZugtypen)) s
    parseJSON   _           = mzero

instance ToJSON Zugtyp where
    toJSON :: Zugtyp -> Value
    toJSON  zugtyp  = String $ pack $ show zugtyp

-- Instanz-Deklaration für Fahrtrichtung
instance FromJSON Fahrtrichtung where
    parseJSON :: Value -> Parser Fahrtrichtung
    parseJSON   (String s)  = returnMatchingShow (NE.toList unterstützteFahrtrichtungen) s
    parseJSON   _           = mzero

instance ToJSON Fahrtrichtung where
    toJSON :: Fahrtrichtung -> Value
    toJSON  fahrtrichtung   = String $ pack $ show fahrtrichtung

-- Instanz-Deklarationen für Bahngeschwindigkeit
instance FromJSON Bahngeschwindigkeit where
    parseJSON :: Value -> Parser Bahngeschwindigkeit
    parseJSON   (Object v)  = do
        name <- (v .: "Name")
        zugtyp <- (v .: "Zugtyp") 
        geschwindigkeitsPin <- (v.: "GeschwindigkeitsPin")
        maybeFahrtrichtungsPin <- (v .:? "FahrtrichtungsPin")
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
    toJSON  (LegoBahngeschwindigkeit {bgName, geschwindigkeitsPin, fahrtrichtungsPin})  = object ["Name" .= bgName, "GeschwindigkeitsPin" .= (fromPin geschwindigkeitsPin), "Zugtyp" .= Lego, "FahrtrichtungsPin" .= (fromPin fahrtrichtungsPin)]
    toJSON  (MärklinBahngeschwindigkeit {bgName, geschwindigkeitsPin})                  = object ["Name" .= bgName, "GeschwindigkeitsPin" .= fromPin geschwindigkeitsPin, "Zugtyp" .= Märklin]

-- Instanz-Deklarationen für Streckenabschnitt
instance FromJSON Streckenabschnitt where
    parseJSON :: Value -> Parser Streckenabschnitt
    parseJSON   (Object v)  = do
        stName <- (v .: "Name")
        stromPin <- (v .: "StromPin") :: Parser Natural
        pure $ Streckenabschnitt {stName, stromPin=toPin stromPin}
    parseJSON   _           = mzero

instance ToJSON Streckenabschnitt where
    toJSON :: Streckenabschnitt -> Value
    toJSON  (Streckenabschnitt {stName, stromPin})  = object ["Name" .= stName, "StromPin" .= fromPin stromPin]

-- Instanz-Deklarationen für Weiche
instance FromJSON Weiche where
    parseJSON :: Value -> Parser Weiche
    parseJSON   (Object v)  = do
        name <- (v .: "Name")
        zugtyp <- (v .: "Zugtyp")
        maybeRichtungsPin <- (v .:? "RichtungsPin")
        maybeRichtungen <- (v .:? "Richtungen")
        maybeRichtungsPins <- (v .:? "RichtungsPins")
        createWeiche zugtyp name maybeRichtungsPin maybeRichtungen maybeRichtungsPins
            where
                createWeiche :: Zugtyp -> Text -> Maybe Natural -> Maybe (Richtung, Richtung) -> Maybe [(Richtung, Natural)] -> Parser Weiche
                createWeiche Lego       weName   (Just richtungsPin) (Just richtungen)   _maybeRichtungsPins                     = pure LegoWeiche {weName, richtungsPin=toPin richtungsPin, richtungen}
                createWeiche Märklin    weName   _maybeRichtungsPin  _maybeRichtungen    (Just ((richtung, pin):richtungsPins))  = pure MärklinWeiche {weName, richtungsPins=(richtung, toPin pin):|map (\(richtung, pin) -> (richtung, toPin pin)) richtungsPins}
                createWeiche _zugtyp    _name   _maybeRichtungsPin  _maybeRichtungen    _maybeRichtungsPins                     = mzero
    parseJSON   _           = mzero

instance ToJSON Weiche where
    toJSON :: Weiche -> Value
    toJSON  (LegoWeiche {weName, richtungsPin, richtungen})   = object ["Name" .= weName, "RichtungsPin" .= fromPin richtungsPin, "Richtungen" .= richtungen, "Zugtyp" .= Lego]
    toJSON  (MärklinWeiche {weName, richtungsPins})           = object ["Name" .= weName, "RichtungsPins" .= map (\(richtung, pin) -> (richtung, fromPin pin)) (NE.toList richtungsPins), "Zugtyp" .= Märklin]

-- Instanz-Deklarationen für Kupplung
instance FromJSON Kupplung where
    parseJSON :: Value -> Parser Kupplung
    parseJSON   (Object v)  = do
        kuName <- (v .: "Name")
        kupplungsPin <- (v .: "KupplungsPin") :: Parser Natural
        pure $ Kupplung {kuName, kupplungsPin=toPin kupplungsPin}
    parseJSON   _           = mzero

instance ToJSON Kupplung where
    toJSON :: Kupplung -> Value
    toJSON  (Kupplung {kuName, kupplungsPin}) = object ["Name" .= kuName, "KupplungsPin" .= fromPin kupplungsPin]

-- Instanz-Deklaration für Wegstrecke
instance FromJSON Wegstrecke where
    parseJSON :: Value -> Parser Wegstrecke
    parseJSON   (Object v)  = Wegstrecke <$> (v .: "Name") <*> (v .: "Bahngeschwindigkeiten") <*> (v .: "Streckenabschnitte") <*> (v .: "Weichen-Richtungen") <*> (v .: "Kupplungen")
    parseJSON   _           = mzero

instance ToJSON Wegstrecke where
    toJSON :: Wegstrecke -> Value
    toJSON  (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen})   = object ["Name" .= wsName, "Bahngeschwindigkeiten" .= wsBahngeschwindigkeiten, "Streckenabschnitte" .= wsStreckenabschnitte, "Weichen-Richtungen" .= wsWeichenRichtungen, "Kupplungen" .= wsKupplungen]

-- Instanz-Deklaration für Plan
instance (FromJSON bg, FromJSON st, FromJSON we, FromJSON ku, FromJSON ws) => FromJSON (AktionGeneral bg st we ku ws) where
    parseJSON :: Value -> Parser (AktionGeneral bg st we ku ws)
    parseJSON   (Object v)  = do
        ((v .: "Aktion") :: Parser Text) >>= \case
            "Warten"            -> Warten <$> v .: "Wert"
            "Einstellen"        -> AWegstrecke <$> Einstellen <$> v .: "Wegstrecke"
            "Stellen"           -> AWeiche <$> (Stellen <$> v .: "Weiche" <*> v .: "Richtung")
            "Geschwindigkeit"   -> parseMaybeWegstrecke v
                (\w v   -> AWSBahngeschwindigkeit <$> Geschwindigkeit w <$> v .: "Wert")
                (\v     -> ABahngeschwindigkeit <$> (Geschwindigkeit <$> v .: "Bahngeschwindigkeit" <*> v .: "Wert"))
            "Umdrehen"          -> parseMaybeWegstrecke v
                (\w v   -> AWSBahngeschwindigkeit <$> Umdrehen w <$> v .:? "Fahrtrichtung")
                (\v     -> ABahngeschwindigkeit <$> (Umdrehen <$> v .: "Bahngeschwindigkeit" <*> v .:? "Fahrtrichtung"))
            "Strom"             -> parseMaybeWegstrecke v
                (\w v   -> AWSStreckenabschnitt <$> Strom w <$> v .: "An")
                (\v     -> AStreckenabschnitt <$> (Strom <$> v .: "Streckenabschnitt" <*> v .: "An"))
            "Kuppeln"           -> parseMaybeWegstrecke v
                (\w _v  -> pure $ AWSKupplung $ Kuppeln w)
                (\v     -> AKupplung <$> Kuppeln <$> v .: "Kupplung")
            _otherwise          -> mzero
        where
            parseMaybeWegstrecke :: (FromJSON ws) => Object -> (ws -> Object -> Parser (AktionWegstrecke ws)) -> (Object -> Parser (AktionGeneral bg st we ku ws)) -> Parser (AktionGeneral bg st we ku ws)
            parseMaybeWegstrecke    v   wsParser    altParser   = v .:? "Wegstrecke" >>= \case
                (Just w)    -> AWegstrecke <$> wsParser w v
                (Nothing)   -> altParser v
    parseJSON   _           = mzero

instance (ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws) => ToJSON (AktionGeneral bg st we ku ws) where
    toJSON :: AktionGeneral bg st we ku ws -> Value
    toJSON  (Warten wert)                                                               = object ["Aktion" .= pack "Warten", "Wert" .= wert]
    toJSON  (AWegstrecke (Einstellen w))                                                = object ["Wegstrecke" .= w, "Aktion" .= pack "Einstellen"]
    toJSON  (AWegstrecke (AWSBahngeschwindigkeit (Geschwindigkeit w wert)))             = object ["Wegstrecke" .= w, "Aktion" .= pack "Geschwindigkeit", "Wert" .= wert]
    toJSON  (AWegstrecke (AWSBahngeschwindigkeit (Umdrehen w Nothing)))                 = object ["Wegstrecke" .= w, "Aktion" .= pack "Umdrehen"]
    toJSON  (AWegstrecke (AWSBahngeschwindigkeit (Umdrehen w (Just fahrtrichtung))))    = object ["Wegstrecke" .= w, "Aktion" .= pack "Umdrehen", "Fahrtrichtung" .= fahrtrichtung]
    toJSON  (AWegstrecke (AWSStreckenabschnitt (Strom w an)))                           = object ["Wegstrecke" .= w, "Aktion" .= pack "Strom", "An" .= an]
    toJSON  (AWegstrecke (AWSKupplung (Kuppeln w)))                                     = object ["Wegstrecke" .= w, "Aktion" .= pack "Kuppeln"]
    toJSON  (AWeiche (Stellen w richtung))                                              = object ["Weiche" .= w, "Aktion" .= pack "Stellen", "Richtung" .= richtung]
    toJSON  (ABahngeschwindigkeit (Geschwindigkeit b wert))                             = object ["Bahngeschwindigkeit" .= b, "Aktion" .= pack "Geschwindigkeit", "Wert" .= wert]
    toJSON  (ABahngeschwindigkeit (Umdrehen b maybeFahrtrichtung))                      = object ["Bahngeschwindigkeit" .= b, "Aktion" .= pack "Umdrehen", "Fahrtrichtung" .= maybeFahrtrichtung]
    toJSON  (AStreckenabschnitt (Strom s an))                                           = object ["Streckenabschnitt" .= s, "Aktion" .= pack "Strom", "An" .= an]
    toJSON  (AKupplung (Kuppeln k))                                                     = object ["Kupplung" .= k, "Aktion" .= pack "Kuppeln"]

instance (FromJSON bg, FromJSON st, FromJSON we, FromJSON ku, FromJSON ws) => FromJSON (PlanGeneral bg st we ku ws) where
    parseJSON :: Value -> Parser (PlanGeneral bg st we ku ws)
    parseJSON   (Object v)  = Plan <$> (v .: "Name") <*> (v .: "Aktionen")
    parseJSON   _           = mzero

instance (ToJSON bg, ToJSON st, ToJSON we, ToJSON ku, ToJSON ws) => ToJSON (PlanGeneral bg st we ku ws) where
    toJSON :: PlanGeneral bg st we ku ws -> Value
    toJSON  (Plan {plName, plAktionen}) = object ["Name" .= plName, "Aktionen" .= plAktionen]

-- | Hilfsfunktion um JSON-Instanzen zu erstellen
returnMatchingShow :: (Show a, FromJSON a, IsString s, Eq s) => [a] -> s -> Parser a
returnMatchingShow (h:t)    s   | s == (fromString $ show h)    = pure h
                                | otherwise                     = returnMatchingShow t s
returnMatchingShow ([])     _s                                  = mzero