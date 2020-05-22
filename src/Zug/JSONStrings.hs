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
findeÜbereinstimmendenWert (h:t) v
    | v == Aeson.toJSON h = pure h
    | otherwise = findeÜbereinstimmendenWert t v

gerade :: Text
gerade = "Gerade"

kurve :: Text
kurve = "Kurve"

links :: Text
links = "Links"

rechts :: Text
rechts = "Rechts"

märklin :: Text
märklin = "Märklin"

lego :: Text
lego = "Lego"

vorwärts :: Text
vorwärts = "Vorwärts"

rückwärts :: Text
rückwärts = "Rückwärts"

fließend :: Text
fließend = "Fließend"

gesperrt :: Text
gesperrt = "Gesperrt"

variant :: Text
variant = "Variante"

a0 :: Text
a0 = "a0"

a1 :: Text
a1 = "a1"

a2 :: Text
a2 = "a2"

interruptPin :: Text
interruptPin = "InterruptPin"

port :: Text
port = "PCF8574Port"

variantNormal :: Text
variantNormal = "Normal"

variantA :: Text
variantA = "A"

high :: Text
high = "HIGH"

low :: Text
low = "LOW"

pin :: Text
pin = "Pin"

name :: Text
name = "Name"

zugtyp :: Text
zugtyp = "Zugtyp"

geschwindigkeitsPin :: Text
geschwindigkeitsPin = "GeschwindigkeitsPin"

fahrtrichtungsPin :: Text
fahrtrichtungsPin = "FahrtrichtungsPin"

fahrtrichtungsAnschluss :: Text
fahrtrichtungsAnschluss = "FahrtrichtungsAnschluss"

fahrstromAnschlüsse :: Text
fahrstromAnschlüsse = "FahrstromAnschlüsse"

umdrehenAnschluss :: Text
umdrehenAnschluss = "UmdrehenAnschluss"

stromPin :: Text
stromPin = "StromPin"

stromAnschluss :: Text
stromAnschluss = "StromAnschluss"

ns :: Text
ns = "ns"

µs :: Text
µs = "µs"

ms :: Text
ms = "ms"

s :: Text
s = "s"

min :: Text
min = "min"

h :: Text
h = "h"

d :: Text
d = "d"

richtungsPin :: Text
richtungsPin = "RichtungsPin"

richtungen :: Text
richtungen = "Richtungen"

richtungsPins :: Text
richtungsPins = "RichtungsPins"

richtungsAnschlüsse :: Text
richtungsAnschlüsse = "RichtungsAnschlüsse"

bahngeschwindigkeiten :: Text
bahngeschwindigkeiten = "Bahngeschwindigkeiten"

streckenabschnitte :: Text
streckenabschnitte = "Streckenabschnitte"

weichen :: Text
weichen = "Weichen"

kupplungen :: Text
kupplungen = "Kupplungen"

kontakte :: Text
kontakte = "Kontakte"

wegstrecken :: Text
wegstrecken = "Wegstrecken"

pläne :: Text
pläne = "Pläne"

kupplungsPin :: Text
kupplungsPin = "KupplungsPin"

kupplungsAnschluss :: Text
kupplungsAnschluss = "KupplungsAnschluss"

kontaktAnschluss :: Text
kontaktAnschluss = "KontaktAnschluss"

weichenRichtungen :: Text
weichenRichtungen = "Weichen-Richtungen"

warten :: Text
warten = "Warten"

kontakt :: Text
kontakt = "Kontakt"

einstellen :: Text
einstellen = "Einstellen"

stellen :: Text
stellen = "Stellen"

geschwindigkeit :: Text
geschwindigkeit = "Geschwindigkeit"

fahrstrom :: Text
fahrstrom = "Fahrstrom"

umdrehen :: Text
umdrehen = "Umdrehen"

fahrtrichtungEinstellen :: Text
fahrtrichtungEinstellen = "FahrtrichtungEinstellen"

strom :: Text
strom = "Strom"

kuppeln :: Text
kuppeln = "Kuppeln"

wegstrecke :: Text
wegstrecke = "Wegstrecke"

aktion :: Text
aktion = "Aktion"

weiche :: Text
weiche = "Weiche"

richtung :: Text
richtung = "Richtung"

bahngeschwindigkeit :: Text
bahngeschwindigkeit = "Bahngeschwindigkeit"

wert :: Text
wert = "Wert"

fahrtrichtung :: Text
fahrtrichtung = "Fahrtrichtung"

streckenabschnitt :: Text
streckenabschnitt = "Streckenabschnitt"

an :: Text
an = "Fließend"

kupplung :: Text
kupplung = "Kupplung"

aktionen :: Text
aktionen = "Aktionen"

ausführen :: Text
ausführen = "Ausführen"

plan :: Text
plan = "Plan"

dauerschleife :: Text
dauerschleife = "Dauerschleife"
