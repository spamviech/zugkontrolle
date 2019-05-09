{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Sammlung aller verwendeten Strings (Deutsche Version).

Sammlung aller verwendeten Strings (Deutsche Version).
-}
module Zug.Language.DE where

import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import System.Hardware.WiringPi (Value(..))
-- Abhängigkeit von anderen Modulen
import Zug.Language.Operatoren

-- * Titel / Title
-- | Train Control
zugkontrolle :: (Semigroup s, IsString s) => s
zugkontrolle                    = "Zugkontrolle"

-- * Haupt-Befehle / Main Orders
-- | Quit
beenden :: (Semigroup s, IsString s) => s
beenden                         = "Beenden"
-- | Cancel
abbrechen :: (Semigroup s, IsString s) => s
abbrechen                       = "Abbrechen"
-- | Undo
rückgängig :: (Semigroup s, IsString s) => s
rückgängig                      = "Rückgängig"
-- | Continue
weiter :: (Semigroup s, IsString s) => s
weiter                          = "Weiter"
-- | Back
zurück :: (Semigroup s, IsString s) => s
zurück                          = "Zurück"
-- | Add
hinzufügen :: (Semigroup s, IsString s) => s
hinzufügen                      = "Hinzufügen"
-- | Remove
entfernen :: (Semigroup s, IsString s) => s
entfernen                       = "Entfernen"
-- | Save
speichern :: (Semigroup s, IsString s) => s
speichern                       = "Speichern"
-- |Load
laden :: (Semigroup s, IsString s) => s
laden                           = "Laden"

-- * Spezielle Befehle / Special orders
-- | Speed
geschwindigkeit :: (Semigroup s, IsString s) => s
geschwindigkeit                 = "Geschwindigkeit"
-- | Turn around
umdrehen :: (Semigroup s, IsString s) => s
umdrehen                        = "Umdrehen"
-- | Change
stellen :: (Semigroup s, IsString s) => s
stellen                         = "Stellen"
-- | Current
strom :: (Semigroup s, IsString s) => s
strom                           = "Strom"
-- | On
an :: (Semigroup s, IsString s) => s
an                              = "An"
-- | Off
aus :: (Semigroup s, IsString s) => s
aus                             = "Aus"
-- | Flowing
fließend :: (Semigroup s, IsString s) => s
fließend                        = "Fließend"
-- | Blocked
gesperrt :: (Semigroup s, IsString s) => s
gesperrt                        = "Gesperrt"
-- | Uncouple
kuppeln :: (Semigroup s, IsString s) => s
kuppeln                         = "Kuppeln"
-- | Adjust the Switches
einstellen :: (Semigroup s, IsString s) => s
einstellen                      = "Einstellen"
-- | Execute
ausführen :: (Semigroup s, IsString s) => s
ausführen                       = "Ausführen"
-- | /s/ <~> "in execution"
wirdAusgeführt :: (Semigroup s, IsString s) => s -> s
wirdAusgeführt s = s <~> "wird ausgeführt"
-- | Abort!  
-- Exclamation point is important to distinguish from 'abbrechen'; required for Cmd-UI
ausführenAbbrechen :: (Semigroup s, IsString s) => s
ausführenAbbrechen              = "Abbrechen!"
-- | aktion <~> "blocked"
aktionGesperrt :: (Semigroup s, IsString s) => s
aktionGesperrt = aktion <~> "gesperrt"
-- | ausführen <~> "blocked" <!> "Pins" <~> /s/ <~> "are already in use."
ausführenGesperrt :: (Semigroup s, IsString s) => s -> s
ausführenGesperrt s = ausführen <~> "gesperrt" <!> "Die Pins" <~> s <~> "werden bereits verwendet."
-- | Wait
warten :: (Semigroup s, IsString s) => s
warten                          = "Warten"
-- | µs
-- 
-- Unit of time used to specify waiting time
wartenEinheit :: (Semigroup s, IsString s) => s
wartenEinheit                   = "µs"
-- | Time
zeit :: (Semigroup s, IsString s) => s
zeit                            = "Zeit"
-- | fließend <-> "Value"
fließendValue :: (Semigroup s, IsString s) => s
fließendValue = fließend <-> "Value"
-- | HIGH
high :: (Semigroup s, IsString s) => s
high = showText HIGH
-- | LOW
low :: (Semigroup s, IsString s) => s
low = showText LOW

-- * Typ-Namen / Type names
-- | Object
objekt :: (Semigroup s, IsString s) => s
objekt                          = "Objekt"
-- | Order
befehl :: (Semigroup s, IsString s) => s
befehl                          = "Befehl"
-- | Train speed
bahngeschwindigkeit :: (Semigroup s, IsString s) => s
bahngeschwindigkeit             = "Bahngeschwindigkeit"
-- | Train speeds
bahngeschwindigkeiten :: (Semigroup s, IsString s) => s
bahngeschwindigkeiten           = "Bahngeschwindigkeiten"
-- | Rail section
streckenabschnitt :: (Semigroup s, IsString s) => s
streckenabschnitt               = "Streckenabschnitt"
-- | Rail sections
streckenabschnitte :: (Semigroup s, IsString s) => s
streckenabschnitte              = "Streckenabschnitte"
-- | Switch
weiche :: (Semigroup s, IsString s) => s
weiche                          = "Weiche"
-- | Switches
weichen :: (Semigroup s, IsString s) => s
weichen                         = "Weichen"
-- | Coupler
kupplung :: (Semigroup s, IsString s) => s
kupplung                        = "Kupplung"
-- | Couplers
kupplungen :: (Semigroup s, IsString s) => s
kupplungen                      = "Kupplungen"
-- | Rail collection
wegstrecke :: (Semigroup s, IsString s) => s
wegstrecke                      = "Wegstrecke"
-- | Rail collections
wegstrecken :: (Semigroup s, IsString s) => s
wegstrecken                     = "Wegstrecken"
-- | Plan
plan :: (Semigroup s, IsString s) => s
plan                            = "Plan"
-- | Plans
pläne :: (Semigroup s, IsString s) => s
pläne                           = "Pläne"

-- * Eigenschafts/Feld-Namen / Attributes/Field names
-- | File path
dateiname :: (Semigroup s, IsString s) => s
dateiname                       = "Dateiname"
-- | Pin
pin :: (Semigroup s, IsString s) => s
pin                             = "Pin"
-- | Name
name :: (Semigroup s, IsString s) => s
name                            = "Name"
-- | Direction
richtung :: (Semigroup s, IsString s) => s
richtung                        = "Richtung"
-- | Directions
richtungen :: (Semigroup s, IsString s) => s
richtungen                      = "Richtungen"
-- | Direction of travel
fahrtrichtung :: (Semigroup s, IsString s) => s
fahrtrichtung                   = "Fahrtrichtung"

-- * Query-Abfragen / Queries
-- | Rail collection element
wegstreckenElement :: (Semigroup s, IsString s) => s
wegstreckenElement              = "Wegstrecken-Element"
-- | Rail collection elements
wegstreckenElemente :: (Semigroup s, IsString s) => s
wegstreckenElemente             = "Wegstrecken-Elemente"
-- | Action
aktion :: (Semigroup s, IsString s) => s
aktion                          = "Aktion"
-- | Actions
aktionen :: (Semigroup s, IsString s) => s
aktionen                        = "Aktionen"
-- | Train model
zugtyp :: (Semigroup s, IsString s) => s
zugtyp                          = "Zugtyp"

-- | Ask to specify the object (type indicated by /s/) either by its index or name
indexOderName :: (Semigroup s, IsString s) => s -> s
indexOderName   s   = s <~> "Index/Name"

-- | Ask to specify the count of the object (indicated by /s/)
anzahl :: (Semigroup s, IsString s) => s -> s
anzahl  s   = "Anzahl" <~> s

-- * Fehlermeldungen / Error Messages
-- | zugkontrolle <:> "Execution requires root priviledges."
nichtRoot :: (Semigroup s, IsString s) => s
nichtRoot                       = zugkontrolle <:> "Ausführung benötigt Root-Rechte!"
-- | Not implemented: ToDo!!!
toDo :: (Semigroup s, IsString s) => s
toDo                            = "Nicht implementiert: ToDo!!!"
-- | Invalid input
ungültigeEingabe :: (Semigroup s, IsString s) => s
ungültigeEingabe                = "Ungültige Eingabe"
-- | Action not supported
nichtUnterstützteAktion :: (Semigroup s, IsString s) => s
nichtUnterstützteAktion         = "Aktion nicht unterstützt"
-- | File not found/Unknown format
nichtGefundeneDatei :: (Semigroup s, IsString s) => s
nichtGefundeneDatei             = "Datei nicht gefunden/Format nicht erkannt"
-- | Selected UI not supported. Cmd-UI is used instead.
uiNichtUnterstützt :: (Semigroup s, IsString s) => s
uiNichtUnterstützt              = "Gewählte UI-Option nicht unterstützt! Nutze stattdessen Cmd-UI."
-- | erwartet "Integer"
integerErwartet :: (Semigroup s, IsString s) => s
integerErwartet                 = erwartet "Integer"
-- | erwartet richtung
richtungErwartet :: (Semigroup s, IsString s) => s
richtungErwartet                = erwartet richtung
-- | mindestens $ "one" <~> richtung
richtungZuWenig :: (Semigroup s, IsString s) => s
richtungZuWenig                 = mindestens $ "eine" <~> richtung
-- | mindestens $ "one" <~> wegstreckenElement
wegstreckeLeer :: (Semigroup s, IsString s) => s
wegstreckeLeer                  = mindestens $ "ein" <~> wegstreckenElement

-- | /s/ <~> "not recognized"
unbekannt :: (Semigroup s, IsString s) => s -> s
unbekannt s = s <~> "nicht erkannt"

-- | /s/ <~> "expected"
erwartet :: (Semigroup s, IsString s) => s -> s
erwartet s = s <~> "erwartet"

-- | "At least" <~> /s/ <~> "required"
mindestens :: (Semigroup s, IsString s) => s -> s
mindestens s = "Mindestens" <~> s <~> "benötigt"

-- * Typ-namen / Type names
-- | Undefined
undefiniert :: (Semigroup s, IsString s) => s
undefiniert                     = "Undefiniert"
-- | Märklin
märklin :: (Semigroup s, IsString s) => s
märklin                         = "Märklin"
-- | Lego
lego :: (Semigroup s, IsString s) => s
lego                            = "Lego"
-- | Straight
gerade :: (Semigroup s, IsString s) => s
gerade                          = "Gerade"
-- | Turn
kurve :: (Semigroup s, IsString s) => s
kurve                           = "Kurve"
-- | Left
links :: (Semigroup s, IsString s) => s
links                           = "Links"
-- | Right
rechts :: (Semigroup s, IsString s) => s
rechts                          = "Rechts"
-- | Forward
vorwärts :: (Semigroup s, IsString s) => s
vorwärts                        = "Vorwärts"
-- | Reverse
rückwärts :: (Semigroup s, IsString s) => s
rückwärts                       = "Rückwärts"