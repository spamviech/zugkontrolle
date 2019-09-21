{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Collection of all Strings used in the program (Englisch Version).

Collection of all Strings used in the program (Englisch Version).
-}
module Zug.Language.EN where

import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import System.Hardware.WiringPi (Value(..))
-- Abhängigkeit von anderen Modulen
import Zug.Language.Operatoren

-- * Titel / Title
-- | Train Control
zugkontrolle :: (Semigroup s, IsString s) => s
zugkontrolle                    = "Train Control"

-- * Haupt-Befehle / Main Orders
-- | Quit
beenden :: (Semigroup s, IsString s) => s
beenden                         = "Quit"
-- | Cancel
abbrechen :: (Semigroup s, IsString s) => s
abbrechen                       = "Cancel"
-- | Undo
rückgängig :: (Semigroup s, IsString s) => s
rückgängig                      = "Undo"
-- | Continue
weiter :: (Semigroup s, IsString s) => s
weiter                          = "Continue"
-- | Back
zurück :: (Semigroup s, IsString s) => s
zurück                          = "Back"
-- | Add
hinzufügen :: (Semigroup s, IsString s) => s
hinzufügen                      = "Add"
-- | Remove
entfernen :: (Semigroup s, IsString s) => s
entfernen                       = "Remove"
-- | Save
speichern :: (Semigroup s, IsString s) => s
speichern                       = "Save"
-- |Load
laden :: (Semigroup s, IsString s) => s
laden                           = "Load"

-- * Spezielle Befehle / Special orders
-- | Speed
geschwindigkeit :: (Semigroup s, IsString s) => s
geschwindigkeit                 = "Speed"
-- | Turn around
umdrehen :: (Semigroup s, IsString s) => s
umdrehen                        = "Turn around"
-- | Direction of Travel
fahrtrichtungEinstellen :: (Semigroup s, IsString s) => s
fahrtrichtungEinstellen         = "Direction of Travel"
-- | Change
stellen :: (Semigroup s, IsString s) => s
stellen                         = "Change"
-- | Current
strom :: (Semigroup s, IsString s) => s
strom                           = "Current"
-- | On
an :: (Semigroup s, IsString s) => s
an                              = "On"
-- | Off
aus :: (Semigroup s, IsString s) => s
aus                             = "Off"
-- | Flowing
fließend :: (Semigroup s, IsString s) => s
fließend                        = "Flowing"
-- | Blocked
gesperrt :: (Semigroup s, IsString s) => s
gesperrt                        = "Blocked"
-- | Uncouple
kuppeln :: (Semigroup s, IsString s) => s
kuppeln                         = "Uncouple"
-- | Adjust the Switches
einstellen :: (Semigroup s, IsString s) => s
einstellen                      = "Adjust the switches"
-- | Execute
ausführen :: (Semigroup s, IsString s) => s
ausführen                       = "Execute"
-- | Abort  
-- Exclamation point is important to distinguish from 'abbrechen'; required for Cmd-UI
ausführenAbbrechen :: (Semigroup s, IsString s) => s
ausführenAbbrechen              = "Abort!"
-- | /s/ <~> "in execution"
wirdAusgeführt :: (Semigroup s, IsString s) => s -> s
wirdAusgeführt s = s <~> "in execution"
-- | aktion <~> "blocked"
aktionGesperrt :: (Semigroup s, IsString s) => s
aktionGesperrt = aktion <~> "blocked"
-- | ausführen <~> "blocked" <!> "Pins" <~> /s/ <~> "are already in use."
ausführenGesperrt :: (Semigroup s, IsString s) => s -> s
ausführenGesperrt s = ausführen <~> "blocked" <!> "Pins" <~> s <~> "are already in use."
-- | Wait
warten :: (Semigroup s, IsString s) => s
warten                          = "Wait"
-- | µs  
-- Unit of time used to specify waiting time
wartenEinheit :: (Semigroup s, IsString s) => s
wartenEinheit                   = "µs"
-- | Time
zeit :: (Semigroup s, IsString s) => s
zeit                            = "Time"
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
objekt                          = "Object"
-- | Order
befehl :: (Semigroup s, IsString s) => s
befehl                          = "Order"
-- | Train speed
bahngeschwindigkeit :: (Semigroup s, IsString s) => s
bahngeschwindigkeit             = "Train speed"
-- | Train speeds
bahngeschwindigkeiten :: (Semigroup s, IsString s) => s
bahngeschwindigkeiten           = "Train speeds"
-- | Rail section
streckenabschnitt :: (Semigroup s, IsString s) => s
streckenabschnitt               = "Rail section"
-- | Rail sections
streckenabschnitte :: (Semigroup s, IsString s) => s
streckenabschnitte              = "Rail sections"
-- | Switch
weiche :: (Semigroup s, IsString s) => s
weiche                          = "Switch"
-- | Switches
weichen :: (Semigroup s, IsString s) => s
weichen                         = "Switches"
-- | Coupler
kupplung :: (Semigroup s, IsString s) => s
kupplung                        = "Coupler"
-- | Couplers
kupplungen :: (Semigroup s, IsString s) => s
kupplungen                      = "Couplers"
-- | Rail collection
wegstrecke :: (Semigroup s, IsString s) => s
wegstrecke                      = "Rail collection"
-- | Rail collections
wegstrecken :: (Semigroup s, IsString s) => s
wegstrecken                     = "Rail collections"
-- | Plan
plan :: (Semigroup s, IsString s) => s
plan                            = "Plan"
-- | Plans
pläne :: (Semigroup s, IsString s) => s
pläne                           = "Plans"

-- * Eigenschafts/Feld-Namen / Attributes/Field names
-- | File path
dateiname :: (Semigroup s, IsString s) => s
dateiname                       = "File path"
-- | Connection
anschluss :: (Semigroup s, IsString s) => s
anschluss                       = "Connection"
-- | Pin
pin :: (Semigroup s, IsString s) => s
pin                             = "Pin"
-- | PCF8574Port
pcf8574Port :: (Semigroup s, IsString s) => s
pcf8574Port                     = "PCF8574Port"
-- | Name
name :: (Semigroup s, IsString s) => s
name                            = "Name"
-- | Direction
richtung :: (Semigroup s, IsString s) => s
richtung                        = "Direction"
-- | Directions
richtungen :: (Semigroup s, IsString s) => s
richtungen                      = "Directions"
-- | Direction of travel
fahrtrichtung :: (Semigroup s, IsString s) => s
fahrtrichtung                   = "Direction of travel"

-- * Query-Abfragen / Queries
-- | Rail collection element
wegstreckenElement :: (Semigroup s, IsString s) => s
wegstreckenElement              = "Rail collection element"
-- | Rail collection elements
wegstreckenElemente :: (Semigroup s, IsString s) => s
wegstreckenElemente             = "Rail collection elements"
-- | Action
aktion :: (Semigroup s, IsString s) => s
aktion                          = "Action"
-- | Actions
aktionen :: (Semigroup s, IsString s) => s
aktionen                        = "Actions"
-- | Train model
zugtyp :: (Semigroup s, IsString s) => s
zugtyp                          = "Train model"

-- | Ask to specify the object (type indicated by /s/) either by its index or name
indexOderName :: (Semigroup s, IsString s) => s -> s
indexOderName   s   = s <~> "Index/Name"

-- | Ask to specify the count of the object (indicated by /s/)
anzahl :: (Semigroup s, IsString s) => s -> s
anzahl  s   = "Count" <~> s

-- * Fehlermeldungen / Error Messages
-- | zugkontrolle <:> "Execution requires root priviledges."
nichtRoot :: (Semigroup s, IsString s) => s
nichtRoot                       = zugkontrolle <:> "Execution requires root priviledges!"
-- | Not implemented: ToDo!!!
toDo :: (Semigroup s, IsString s) => s
toDo                            = "Not implemented: ToDo!!!"
-- | Invalid input
ungültigeEingabe :: (Semigroup s, IsString s) => s
ungültigeEingabe                = "Invalid input"
-- | Action not supported
nichtUnterstützteAktion :: (Semigroup s, IsString s) => s
nichtUnterstützteAktion         = "Action not supported"
-- | File not found/Unknown format
nichtGefundeneDatei :: (Semigroup s, IsString s) => s
nichtGefundeneDatei             = "File not found/Unknown format"
-- | Selected UI not supported. Cmd-UI is used instead.
uiNichtUnterstützt :: (Semigroup s, IsString s) => s
uiNichtUnterstützt              = "Selected UI not supported. Cmd-UI is used instead."
-- | erwartet "Integer"
integerErwartet :: (Semigroup s, IsString s) => s
integerErwartet                 = erwartet "Integer"
-- | erwartet richtung
richtungErwartet :: (Semigroup s, IsString s) => s
richtungErwartet                = erwartet richtung
-- | mindestens $ "one" <~> richtung
richtungZuWenig :: (Semigroup s, IsString s) => s
richtungZuWenig                 = mindestens $ "one" <~> richtung
-- | mindestens $ "one" <~> wegstreckenElement
wegstreckeLeer :: (Semigroup s, IsString s) => s
wegstreckeLeer                  = mindestens $ "one" <~> wegstreckenElement

-- | /s/ <~> "not recognized"
unbekannt :: (Semigroup s, IsString s) => s -> s
unbekannt s = s <~> "not recognized"

-- | /s/ <~> "expected"
erwartet :: (Semigroup s, IsString s) => s -> s
erwartet s = s <~> "expected"

-- | "At least" <~> /s/ <~> "required"
mindestens :: (Semigroup s, IsString s) => s -> s
mindestens s = "At least" <~> s <~> "required"

-- * Typ-namen / Type names
-- | Märklin
märklin :: (Semigroup s, IsString s) => s
märklin                         = "Märklin"
-- | Lego
lego :: (Semigroup s, IsString s) => s
lego                            = "Lego"
-- | Straight
gerade :: (Semigroup s, IsString s) => s
gerade                          = "Straight"
-- | Turn
kurve :: (Semigroup s, IsString s) => s
kurve                           = "Turn"
-- | Left
links :: (Semigroup s, IsString s) => s
links                           = "Left"
-- | Right
rechts :: (Semigroup s, IsString s) => s
rechts                          = "Right"
-- | Forward
vorwärts :: (Semigroup s, IsString s) => s
vorwärts                        = "Forward"
-- | Reverse
rückwärts :: (Semigroup s, IsString s) => s
rückwärts                       = "Reverse"