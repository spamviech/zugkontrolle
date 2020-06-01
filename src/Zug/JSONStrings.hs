{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description: Bezeichner für JSON-Felder & String-Werte.

Sie werden hier definiert, damit sie auch nach einem Sprachwechsel ihre Gültigkeit behalten.
Dieses Modul sollte qualifiziert importiert werden.
-}
module Zug.JSONStrings where

import Control.Applicative (Alternative(..))
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import System.Hardware.WiringPi (Value(..))

instance Aeson.FromJSON Value where
    parseJSON :: Aeson.Value -> Aeson.Parser Value
    parseJSON = findeÜbereinstimmendenWert [minBound .. maxBound]

instance Aeson.ToJSON Value where
    toJSON :: Value -> Aeson.Value
    toJSON HIGH = Aeson.String high
    toJSON LOW = Aeson.String low

-- | Hilfsfunktion, um einfache FromJSON-Instanzen zu erstellen.
findeÜbereinstimmendenWert :: (Aeson.ToJSON a) => [a] -> Aeson.Value -> Aeson.Parser a
findeÜbereinstimmendenWert [] _v = empty
findeÜbereinstimmendenWert (a:as) v
    | v == Aeson.toJSON h = pure a
    | otherwise = findeÜbereinstimmendenWert as v

-- | Gerade
gerade :: Text
gerade = "Gerade"

-- | Kurve
kurve :: Text
kurve = "Kurve"

-- | Links
links :: Text
links = "Links"

-- | Rechts
rechts :: Text
rechts = "Rechts"

-- | Märklin
märklin :: Text
märklin = "Märklin"

-- | Lego
lego :: Text
lego = "Lego"

-- | Vorwärts
vorwärts :: Text
vorwärts = "Vorwärts"

-- | Rückwärts
rückwärts :: Text
rückwärts = "Rückwärts"

-- | Fließend
fließend :: Text
fließend = "Fließend"

-- | Gesperrt
gesperrt :: Text
gesperrt = "Gesperrt"

-- | Variante
variant :: Text
variant = "Variante"

-- | a0
a0 :: Text
a0 = "a0"

-- | a1
a1 :: Text
a1 = "a1"

-- | a2
a2 :: Text
a2 = "a2"

-- | InterruptPin
interruptPin :: Text
interruptPin = "InterruptPin"

-- | PCF8574Port
port :: Text
port = "PCF8574Port"

-- | Normal
variantNormal :: Text
variantNormal = "Normal"

-- | A
variantA :: Text
variantA = "A"

-- | HIGH
high :: Text
high = "HIGH"

-- | LOW
low :: Text
low = "LOW"

-- | Pin
pin :: Text
pin = "Pin"

-- | Name
name :: Text
name = "Name"

-- | Zugtyp
zugtyp :: Text
zugtyp = "Zugtyp"

-- | GeschwindigkeitsPin
geschwindigkeitsPin :: Text
geschwindigkeitsPin = "GeschwindigkeitsPin"

-- | FahrtrichtungsPin
fahrtrichtungsPin :: Text
fahrtrichtungsPin = "FahrtrichtungsPin"

-- | FahrtrichtungsAnschluss
fahrtrichtungsAnschluss :: Text
fahrtrichtungsAnschluss = "FahrtrichtungsAnschluss"

-- | FahrtrichtungsAnschlüsse
fahrstromAnschlüsse :: Text
fahrstromAnschlüsse = "FahrstromAnschlüsse"

-- | UmdrehenAnschluss
umdrehenAnschluss :: Text
umdrehenAnschluss = "UmdrehenAnschluss"

-- | StromPin
stromPin :: Text
stromPin = "StromPin"

-- | StromAnschluss
stromAnschluss :: Text
stromAnschluss = "StromAnschluss"

-- | ns
ns :: Text
ns = "ns"

-- | µs
µs :: Text
µs = "µs"

-- | ms
ms :: Text
ms = "ms"

-- | s
s :: Text
s = "s"

-- | min
min :: Text
min = "min"

-- | h
h :: Text
h = "h"

-- | d
d :: Text
d = "d"

-- | RichtungsPin
richtungsPin :: Text
richtungsPin = "RichtungsPin"

-- | Richtungen
richtungen :: Text
richtungen = "Richtungen"

-- | RichtungsPins
richtungsPins :: Text
richtungsPins = "RichtungsPins"

-- | RichtungsAnschlüsse
richtungsAnschlüsse :: Text
richtungsAnschlüsse = "RichtungsAnschlüsse"

-- | Bahngeschwindigkeiten
bahngeschwindigkeiten :: Text
bahngeschwindigkeiten = "Bahngeschwindigkeiten"

-- | Streckenabschnitte
streckenabschnitte :: Text
streckenabschnitte = "Streckenabschnitte"

-- | Weichen
weichen :: Text
weichen = "Weichen"

-- | Kupplungen
kupplungen :: Text
kupplungen = "Kupplungen"

-- | Kontakte
kontakte :: Text
kontakte = "Kontakte"

-- | Wegstrecken
wegstrecken :: Text
wegstrecken = "Wegstrecken"

-- | Pläne
pläne :: Text
pläne = "Pläne"

-- | KupplungsPin
kupplungsPin :: Text
kupplungsPin = "KupplungsPin"

-- | KupplungsAnschluss
kupplungsAnschluss :: Text
kupplungsAnschluss = "KupplungsAnschluss"

-- | KontaktAnschluss
kontaktAnschluss :: Text
kontaktAnschluss = "KontaktAnschluss"

-- | Weichen-Richtungen
weichenRichtungen :: Text
weichenRichtungen = "Weichen-Richtungen"

-- | Warten
warten :: Text
warten = "Warten"

-- | Kontakt
kontakt :: Text
kontakt = "Kontakt"

-- | Einstellen
einstellen :: Text
einstellen = "Einstellen"

-- | Stellen
stellen :: Text
stellen = "Stellen"

-- | Geschwindigkeit
geschwindigkeit :: Text
geschwindigkeit = "Geschwindigkeit"

-- | Fahrstrom
fahrstrom :: Text
fahrstrom = "Fahrstrom"

-- | Umdrehen
umdrehen :: Text
umdrehen = "Umdrehen"

-- | FahrtrichtungEinstellen
fahrtrichtungEinstellen :: Text
fahrtrichtungEinstellen = "FahrtrichtungEinstellen"

-- | Strom
strom :: Text
strom = "Strom"

-- | Kuppeln
kuppeln :: Text
kuppeln = "Kuppeln"

-- | Wegstrecke
wegstrecke :: Text
wegstrecke = "Wegstrecke"

-- | Aktion
aktion :: Text
aktion = "Aktion"

-- | Weiche
weiche :: Text
weiche = "Weiche"

-- | Richtung
richtung :: Text
richtung = "Richtung"

-- | Bahngeschwindigkeit
bahngeschwindigkeit :: Text
bahngeschwindigkeit = "Bahngeschwindigkeit"

-- | Wert
wert :: Text
wert = "Wert"

-- | Fahrtrichtung
fahrtrichtung :: Text
fahrtrichtung = "Fahrtrichtung"

-- | Streckenabschnitt
streckenabschnitt :: Text
streckenabschnitt = "Streckenabschnitt"

-- | Fließend
an :: Text
an = "Fließend"

-- | Kupplung
kupplung :: Text
kupplung = "Kupplung"

-- | Aktion
aktionen :: Text
aktionen = "Aktionen"

-- | Ausführen
ausführen :: Text
ausführen = "Ausführen"

-- | Plan
plan :: Text
plan = "Plan"

-- | Dauerschleife
dauerschleife :: Text
dauerschleife = "Dauerschleife"
