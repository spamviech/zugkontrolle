{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Sammlung aller verwendeten Strings (Deutsche Version).

Sammlung aller verwendeten Strings (Deutsche Version).
-}
module Zug.Language.DE where

import Data.Text (Text)
import System.Hardware.WiringPi (Value(..))
-- Abhängigkeit von anderen Modulen
import Zug.Language.Operatoren

-- * Titel / Title
-- | Train Control
zugkontrolle :: Text
zugkontrolle                    = "Zugkontrolle"

-- * Haupt-Befehle / Main Orders
-- | Quit
beenden :: Text
beenden                         = "Beenden"
-- | Cancel
abbrechen :: Text
abbrechen                       = "Abbrechen"
-- | Undo
rückgängig :: Text
rückgängig                      = "Rückgängig"
-- | Continue
weiter :: Text
weiter                          = "Weiter"
-- | Back
zurück :: Text
zurück                          = "Zurück"
-- | Add
hinzufügen :: Text
hinzufügen                      = "Hinzufügen"
-- | Remove
entfernen :: Text
entfernen                       = "Entfernen"
-- | Save
speichern :: Text
speichern                       = "Speichern"
-- |Load
laden :: Text
laden                           = "Laden"

-- * Spezielle Befehle / Special orders
-- | Speed
geschwindigkeit :: Text
geschwindigkeit                 = "Geschwindigkeit"
-- | Turn around
umdrehen :: Text
umdrehen                        = "Umdrehen"
-- | Direction of Travel
fahrtrichtungEinstellen :: Text
fahrtrichtungEinstellen         = "Fahrtrichtung"
-- | Change
stellen :: Text
stellen                         = "Stellen"
-- | Current
strom :: Text
strom                           = "Strom"
-- | On
an :: Text
an                              = "An"
-- | Off
aus :: Text
aus                             = "Aus"
-- | Flowing
fließend :: Text
fließend                        = "Fließend"
-- | Blocked
gesperrt :: Text
gesperrt                        = "Gesperrt"
-- | Uncouple
kuppeln :: Text
kuppeln                         = "Kuppeln"
-- | Adjust the Switches
einstellen :: Text
einstellen                      = "Einstellen"
-- | Execute
ausführen :: Text
ausführen                       = "Ausführen"
-- | /s/ <~> "in execution"
wirdAusgeführt :: Text -> Text
wirdAusgeführt s = s <~> ("wird ausgeführt" :: Text) $ Deutsch
-- | Abort!  
-- Exclamation point is important to distinguish from 'abbrechen'; required for Cmd-UI
ausführenAbbrechen :: Text
ausführenAbbrechen              = "Abbrechen!"
-- | aktion <~> "blocked"
aktionGesperrt :: Text
aktionGesperrt = aktion <~> ("gesperrt" :: Text) $ Deutsch
-- | ausführen <~> "blocked" <!> "Pins" <~> /s/ <~> "are already in use."
ausführenGesperrt :: Text -> Text
ausführenGesperrt s
    = ausführen <~> ("gesperrt" :: Text) <!>
        ("Die Pins" :: Text) <~> s <~> ("werden bereits verwendet." :: Text) $ Deutsch
-- | Wait
warten :: Text
warten                          = "Warten"
-- | µs
-- 
-- Unit of time used to specify waiting time
wartenEinheit :: Text
wartenEinheit                   = "µs"
-- | Time
zeit :: Text
zeit                            = "Zeit"
-- | fließend <-> "Value"
fließendValue :: Text
fließendValue                   = fließend <-> ("Value" :: Text) $ Deutsch
-- | HIGH
high :: Text
high                            = showText HIGH
-- | LOW
low :: Text
low                             = showText LOW
-- | Execute
aktionAusführen :: Text
aktionAusführen                 = "Ausführen"
-- | Execute-Once
einfachAusführung :: Text
einfachAusführung               = "Einfach-Ausführung"
-- | Loop
dauerschleife :: Text
dauerschleife                   = "Dauerschleife"

-- * Typ-Namen / Type names
-- | Object
objekt :: Text
objekt                          = "Objekt"
-- | Order
befehl :: Text
befehl                          = "Befehl"
-- | Train speed
bahngeschwindigkeit :: Text
bahngeschwindigkeit             = "Bahngeschwindigkeit"
-- | Train speeds
bahngeschwindigkeiten :: Text
bahngeschwindigkeiten           = "Bahngeschwindigkeiten"
-- | Rail section
streckenabschnitt :: Text
streckenabschnitt               = "Streckenabschnitt"
-- | Rail sections
streckenabschnitte :: Text
streckenabschnitte              = "Streckenabschnitte"
-- | Switch
weiche :: Text
weiche                          = "Weiche"
-- | Switches
weichen :: Text
weichen                         = "Weichen"
-- | Coupler
kupplung :: Text
kupplung                        = "Kupplung"
-- | Couplers
kupplungen :: Text
kupplungen                      = "Kupplungen"
-- | Rail collection
wegstrecke :: Text
wegstrecke                      = "Wegstrecke"
-- | Rail collections
wegstrecken :: Text
wegstrecken                     = "Wegstrecken"
-- | Plan
plan :: Text
plan                            = "Plan"
-- | Plans
pläne :: Text
pläne                           = "Pläne"

-- * Eigenschafts/Feld-Namen / Attributes/Field names
-- | File path
dateiname :: Text
dateiname                       = "Dateiname"
-- | Connection
anschluss :: Text
anschluss                       = "Anschluss"
-- | Pin
pin :: Text
pin                             = "Pin"
-- | PCF8574Port
pcf8574Port :: Text
pcf8574Port                     = pcf8574 <-> port $ Deutsch
-- | PCF8574
pcf8574 :: Text
pcf8574                         = "PCF8574"
-- | Variant
variante :: Text
variante                        = "Variante"
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
richtung                        = "Richtung"
-- | Directions
richtungen :: Text
richtungen                      = "Richtungen"
-- | Direction of travel
fahrtrichtung :: Text
fahrtrichtung                   = "Fahrtrichtung"

-- * Query-Abfragen / Queries
-- | Rail collection element
wegstreckenElement :: Text
wegstreckenElement              = "Wegstrecken-Element"
-- | Rail collection elements
wegstreckenElemente :: Text
wegstreckenElemente             = "Wegstrecken-Elemente"
-- | Action
aktion :: Text
aktion                          = "Aktion"
-- | Actions
aktionen :: Text
aktionen                        = "Aktionen"
-- | Train model
zugtyp :: Text
zugtyp                          = "Zugtyp"
-- | Execution mode
ausführModus :: Text
ausführModus                    = "Ausführ-Modus"
-- | Which Object should be added?
welchesObjektHinzufügen :: Text
welchesObjektHinzufügen         = "Welches Objekt soll hinzugefügt werden?"

-- | Ask to specify the object (type indicated by /s/) either by its index or name
indexOderName :: Text -> Text
indexOderName   s   = s <~> ("Index/Name" :: Text) $ Deutsch

-- | Ask to specify the count of the object (indicated by /s/)
anzahl :: Text -> Text
anzahl  s   = ("Anzahl" :: Text) <~> s $ Deutsch

-- * Fehlermeldungen / Error Messages
-- | zugkontrolle <:> "Execution requires root priviledges."
nichtRoot :: Text
nichtRoot                       = zugkontrolle <:> ("Ausführung benötigt Root-Rechte!" :: Text) $ Deutsch
-- | Not implemented: ToDo!!!
toDo :: Text
toDo                            = "Nicht implementiert: ToDo!!!"
-- | Invalid input
ungültigeEingabe :: Text
ungültigeEingabe                = "Ungültige Eingabe"
-- | Action not supported
nichtUnterstützteAktion :: Text
nichtUnterstützteAktion         = "Aktion nicht unterstützt"
-- | File not found/Unknown format
nichtGefundeneDatei :: Text
nichtGefundeneDatei             = "Datei nicht gefunden/Format nicht erkannt"
-- | Selected UI not supported. Cmd-UI is used instead.
uiNichtUnterstützt :: Text
uiNichtUnterstützt              = "Gewählte UI-Option nicht unterstützt! Nutze stattdessen Cmd-UI."
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
richtungZuWenig                 = mindestens $ ("eine" :: Text) <~> richtung $ Deutsch
-- | mindestens $ "one" <~> wegstreckenElement
wegstreckeLeer :: Text
wegstreckeLeer                  = mindestens $ ("ein" :: Text) <~> wegstreckenElement $ Deutsch

-- | /s/ <~> "not recognized"
unbekannt :: Text -> Text
unbekannt s = s <~> ("nicht erkannt" :: Text) $ Deutsch

-- | /s/ <~> "expected"
erwartet :: Text -> Text
erwartet s = s <~> ("erwartet" :: Text) $ Deutsch

-- | "At least" <~> /s/ <~> "required"
mindestens :: Text -> Text
mindestens s = ("Mindestens" :: Text) <~> s <~> ("benötigt" :: Text) $ Deutsch

-- * Typ-namen / Type names
-- | Märklin
märklin :: Text
märklin                         = "Märklin"
-- | Lego
lego :: Text
lego                            = "Lego"
-- | Straight
gerade :: Text
gerade                          = "Gerade"
-- | Turn
kurve :: Text
kurve                           = "Kurve"
-- | Left
links :: Text
links                           = "Links"
-- | Right
rechts :: Text
rechts                          = "Rechts"
-- | Forward
vorwärts :: Text
vorwärts                        = "Vorwärts"
-- | Reverse
rückwärts :: Text
rückwärts                       = "Rückwärts"