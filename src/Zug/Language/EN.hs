{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Collection of all Strings used in the program (Englisch Version).

Collection of all Strings used in the program (Englisch Version).
-}
module Zug.Language.EN where

import Data.Text (Text)
import System.Hardware.WiringPi (Value(..))
-- Abhängigkeit von anderen Modulen
import Zug.Language.Operatoren

-- * Titel / Title
-- | Train Control
zugkontrolle :: Text
zugkontrolle                    = "Train Control"

-- * Haupt-Befehle / Main Orders
-- | Quit
beenden :: Text
beenden                         = "Quit"
-- | Cancel
abbrechen :: Text
abbrechen                       = "Cancel"
-- | Undo
rückgängig :: Text
rückgängig                      = "Undo"
-- | Continue
weiter :: Text
weiter                          = "Continue"
-- | Back
zurück :: Text
zurück                          = "Back"
-- | Add
hinzufügen :: Text
hinzufügen                      = "Add"
-- | Remove
entfernen :: Text
entfernen                       = "Remove"
-- | Save
speichern :: Text
speichern                       = "Save"
-- |Load
laden :: Text
laden                           = "Load"

-- * Spezielle Befehle / Special orders
-- | Speed
geschwindigkeit :: Text
geschwindigkeit                 = "Speed"
-- | Turn around
umdrehen :: Text
umdrehen                        = "Turn around"
-- | Direction of Travel
fahrtrichtungEinstellen :: Text
fahrtrichtungEinstellen         = "Direction of Travel"
-- | Change
stellen :: Text
stellen                         = "Change"
-- | Current
strom :: Text
strom                           = "Current"
-- | On
an :: Text
an                              = "On"
-- | Off
aus :: Text
aus                             = "Off"
-- | Flowing
fließend :: Text
fließend                        = "Flowing"
-- | Blocked
gesperrt :: Text
gesperrt                        = "Blocked"
-- | Uncouple
kuppeln :: Text
kuppeln                         = "Uncouple"
-- | Adjust the Switches
einstellen :: Text
einstellen                      = "Adjust the switches"
-- | Execute
ausführen :: Text
ausführen                       = "Execute"
-- | Abort  
-- Exclamation point is important to distinguish from 'abbrechen'; required for Cmd-UI
ausführenAbbrechen :: Text
ausführenAbbrechen              = "Abort!"
-- | /s/ <~> "in execution"
wirdAusgeführt :: Text -> Text
wirdAusgeführt s = s <~> ("in execution" :: Text) $ Englisch
-- | aktion <~> "blocked"
aktionGesperrt :: Text
aktionGesperrt = aktion <~> ("blocked" :: Text) $ Englisch
-- | ausführen <~> "blocked" <!> "Pins" <~> /s/ <~> "are already in use."
ausführenGesperrt :: Text -> Text
ausführenGesperrt s
    = ausführen <~> ("blocked" :: Text) <!> ("Pins" :: Text) <~> s <~> ("are already in use." :: Text) $ Englisch
-- | Wait
warten :: Text
warten                          = "Wait"
-- | µs  
-- Unit of time used to specify waiting time
wartenEinheit :: Text
wartenEinheit                   = "µs"
-- | Time
zeit :: Text
zeit                            = "Time"
-- | fließend <-> "Value"
fließendValue :: Text
fließendValue = fließend <-> ("Value" :: Text) $ Englisch
-- | HIGH
high :: Text
high = showText HIGH
-- | LOW
low :: Text
low = showText LOW
-- | Execute
aktionAusführen :: Text
aktionAusführen = "Execute"
-- | Execute-Once
einfachAusführung :: Text
einfachAusführung = "Execute-Once"
-- | Loop
dauerschleife :: Text
dauerschleife = "Loop"

-- * Typ-Namen / Type names
-- | Object
objekt :: Text
objekt                          = "Object"
-- | Order
befehl :: Text
befehl                          = "Order"
-- | Train speed
bahngeschwindigkeit :: Text
bahngeschwindigkeit             = "Train speed"
-- | Train speeds
bahngeschwindigkeiten :: Text
bahngeschwindigkeiten           = "Train speeds"
-- | Rail section
streckenabschnitt :: Text
streckenabschnitt               = "Rail section"
-- | Rail sections
streckenabschnitte :: Text
streckenabschnitte              = "Rail sections"
-- | Switch
weiche :: Text
weiche                          = "Switch"
-- | Switches
weichen :: Text
weichen                         = "Switches"
-- | Coupler
kupplung :: Text
kupplung                        = "Coupler"
-- | Couplers
kupplungen :: Text
kupplungen                      = "Couplers"
-- | Rail collection
wegstrecke :: Text
wegstrecke                      = "Rail collection"
-- | Rail collections
wegstrecken :: Text
wegstrecken                     = "Rail collections"
-- | Plan
plan :: Text
plan                            = "Plan"
-- | Plans
pläne :: Text
pläne                           = "Plans"

-- * Eigenschafts/Feld-Namen / Attributes/Field names
-- | File path
dateiname :: Text
dateiname                       = "File path"
-- | Connection
anschluss :: Text
anschluss                       = "Connection"
-- | Pin
pin :: Text
pin                             = "Pin"
-- | PCF8574Port
pcf8574Port :: Text
pcf8574Port                     = pcf8574 <-> port $ Englisch
-- | PCF8574
pcf8574 :: Text
pcf8574                         = "PCF8574"
-- | Variant
variante :: Text
variante                        = "Variant"
-- | normal
normal :: Text
normal                          = "normal"
-- | A
a :: Text
a                               = "A"
-- | a0
a0 :: Text
a0                              = "a0"
-- | a1
a1 :: Text
a1                              = "a1"
-- | a2
a2 :: Text
a2                              = "a2"
-- | Port
port :: Text
port                            = "Port"
-- | Name
name :: Text
name                            = "Name"
-- | Direction
richtung :: Text
richtung                        = "Direction"
-- | Directions
richtungen :: Text
richtungen                      = "Directions"
-- | Direction of travel
fahrtrichtung :: Text
fahrtrichtung                   = "Direction of travel"

-- * Query-Abfragen / Queries
-- | Rail collection element
wegstreckenElement :: Text
wegstreckenElement              = "Rail collection element"
-- | Rail collection elements
wegstreckenElemente :: Text
wegstreckenElemente             = "Rail collection elements"
-- | Action
aktion :: Text
aktion                          = "Action"
-- | Actions
aktionen :: Text
aktionen                        = "Actions"
-- | Train model
zugtyp :: Text
zugtyp                          = "Train model"
-- | Execution mode
ausführModus :: Text
ausführModus                    = "Execution mode"
-- | Which Object should be added?
welchesObjektHinzufügen :: Text
welchesObjektHinzufügen         = "Which Object should be added?"

-- | Ask to specify the object (type indicated by /s/) either by its index or name
indexOderName :: Text -> Text
indexOderName   s   = s <~> ("Index/Name" :: Text) $ Englisch

-- | Ask to specify the count of the object (indicated by /s/)
anzahl :: Text -> Text
anzahl  s   = ("Count" :: Text) <~> s $ Englisch

-- * Fehlermeldungen / Error Messages
-- | zugkontrolle <:> "Execution requires root priviledges."
nichtRoot :: Text
nichtRoot                       = zugkontrolle <:> ("Execution requires root priviledges!" :: Text) $ Englisch
-- | Not implemented: ToDo!!!
toDo :: Text
toDo                            = "Not implemented: ToDo!!!"
-- | Invalid input
ungültigeEingabe :: Text
ungültigeEingabe                = "Invalid input"
-- | Action not supported
nichtUnterstützteAktion :: Text
nichtUnterstützteAktion         = "Action not supported"
-- | File not found/Unknown format
nichtGefundeneDatei :: Text
nichtGefundeneDatei             = "File not found/Unknown format"
-- | Selected UI not supported. Cmd-UI is used instead.
uiNichtUnterstützt :: Text
uiNichtUnterstützt              = "Selected UI not supported. Cmd-UI is used instead."
-- | erwartet "Integer"
integerErwartet :: Text
integerErwartet                 = erwartet "Integer"
-- | erwartet "Value"
valueErwartet :: Text
valueErwartet                   = erwartet "Value"
-- | erwartet richtung
richtungErwartet :: Text
richtungErwartet                = erwartet richtung
-- | mindestens $ "one" <~> richtung
richtungZuWenig :: Text
richtungZuWenig                 = mindestens $ ("one" :: Text) <~> richtung $ Englisch 
-- | mindestens $ "one" <~> wegstreckenElement
wegstreckeLeer :: Text
wegstreckeLeer                  = mindestens $ ("one" :: Text) <~> wegstreckenElement $ Englisch 

-- | /s/ <~> "not recognized"
unbekannt :: Text -> Text
unbekannt s = s <~> ("not recognized" :: Text) $ Englisch

-- | /s/ <~> "expected"
erwartet :: Text -> Text
erwartet s = s <~> ("expected" :: Text) $ Englisch

-- | "At least" <~> /s/ <~> "required"
mindestens :: Text -> Text
mindestens s = ("At least" :: Text) <~> s <~> ("required" :: Text) $ Englisch

-- * Typ-namen / Type names
-- | Märklin
märklin :: Text
märklin                         = "Märklin"
-- | Lego
lego :: Text
lego                            = "Lego"
-- | Straight
gerade :: Text
gerade                          = "Straight"
-- | Turn
kurve :: Text
kurve                           = "Turn"
-- | Left
links :: Text
links                           = "Left"
-- | Right
rechts :: Text
rechts                          = "Right"
-- | Forward
vorwärts :: Text
vorwärts                        = "Forward"
-- | Reverse
rückwärts :: Text
rückwärts                       = "Reverse"