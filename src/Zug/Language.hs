{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Sammlung aller verwendeten Strings, zur einfacheren Unterstützung einer neuen Sprache.

Every used string is stored here. Names are in German and represent the string pretty much on a 1-1 basis.
Comments are what I think the English translation should be.
-}
module Zug.Language where

import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.IO as T

-- * Titel / Title
-- | Title of the program
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
-- | Uncouple
kuppeln :: (Semigroup s, IsString s) => s
kuppeln                         = "Kuppeln"
-- | Adjust the Switches
einstellen :: (Semigroup s, IsString s) => s
einstellen                      = "Einstellen"
-- | Execute
ausführen :: (Semigroup s, IsString s) => s
ausführen                       = "Ausführen"
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
-- | action
aktion :: (Semigroup s, IsString s) => s
aktion                          = "Aktion"
-- | actions
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

-- Befehlsgruppen / Order classifications
-- | All supported Orders in the main menu
befehlAlle :: (Semigroup s, IsString s) => [s]
befehlAlle = [beenden, hinzufügen, entfernen, speichern, laden] <> befehlTypen
-- | All supported Orders, classified by a type
befehlTypen :: (Semigroup s, IsString s) => [s]
befehlTypen = [plan] <> befehlObjekte
-- | All supported Orders, classified by a (physical) object
befehlObjekte :: (Semigroup s, IsString s) => [s]
befehlObjekte = [wegstrecke] <> befehlWegstreckenElemente
-- | All supported Orders, classified by a train collection element
befehlWegstreckenElemente :: (Semigroup s, IsString s) => [s]
befehlWegstreckenElemente = [weiche, bahngeschwindigkeit, streckenabschnitt, kupplung]
-- | All supported actions
aktionGruppen :: (Semigroup s, IsString s) => [s]
aktionGruppen = [warten] <> befehlObjekte
-- | All supported actions for a 'Plan'
aktionPlan :: (Semigroup s, IsString s) => [s]
aktionPlan = [ausführen]
-- | All supported actions for a train collection ('Wegstrecke')
aktionWegstrecke :: (Semigroup s, IsString s) => [s]
aktionWegstrecke = [einstellen] <> aktionBahngeschwindigkeit <> aktionStreckenabschnitt <> aktionKupplung
-- | All supported actions for a switch ('Weiche')
aktionWeiche :: (Semigroup s, IsString s) => [s]
aktionWeiche = [stellen]
-- | All supported actions for a train speed ('Bahngeschwindigkeit')
aktionBahngeschwindigkeit :: (Semigroup s, IsString s) => [s]
aktionBahngeschwindigkeit = [geschwindigkeit, umdrehen]
-- | All supported actions for a rail section ('Streckenabschnitt')
aktionStreckenabschnitt :: (Semigroup s, IsString s) => [s]
aktionStreckenabschnitt = [strom]
-- | All supported actions for a coupler ('Kupplung')
aktionKupplung :: (Semigroup s, IsString s) => [s]
aktionKupplung = [kuppeln]

-- | Concatenate a list of strings to an eye-pleasing format
toBefehlsString :: (Semigroup s, IsString s) => [s] -> s
toBefehlsString ([])    = ""
toBefehlsString ([s])   = s
toBefehlsString (h:t)   = h <^> toBefehlsString t

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
-- | File not found
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

-- | s <~> "not recognized"
unbekannt :: (Semigroup s, IsString s) => s -> s
unbekannt   s   = s <~> "nicht erkannt"

-- | s <~> "expected"
erwartet :: (Semigroup s, IsString s) => s -> s
erwartet    s   = s <~> "erwartet"

-- | "At least" <~> s <~> "required"
mindestens :: (Semigroup s, IsString s) => s -> s
mindestens  s   = "Mindestens" <~> s <~> "benötigt"

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

-- * Strings mit Leerzeichen/Trennzeichen verknüpfen / Concatenate strings with a space/seperator
infixr 6 <~>
-- | Verkette zwei Strings mit einem Leerzeichen.
-- 
-- Concatenate two strings with a space.
(<~>) :: (Semigroup s, IsString s) => s -> s -> s
a <~> b = a <> " " <> b

infixr 6 <^>
-- | Verkette zwei Strings mit einem Komma.
-- 
-- Concatenate two strings with a comma.
(<^>) :: (Semigroup s, IsString s) => s -> s -> s
a <^> b = a <> ", " <> b

infixr 6 <=>
-- | Verkette zwei Strings mit einem Gleichheitszeichen.
-- 
-- Concatenate two strings with a equal sign.
(<=>) :: (Semigroup s, IsString s) => s -> s -> s
a <=> b = a <> "=" <> b

infixr 6 <->
-- | Verkette zwei Strings mit einem Bindestrinch.
-- 
-- Concatenate two strings with a hypthen.
(<->) :: (Semigroup s, IsString s) => s -> s -> s
a <-> b = a <> "-" <> b

infixr 6 <|>
-- | Verkette zwei Strings mit einem '|'.
-- 
-- Concatenate two strings with a '|'.
(<|>) :: (Semigroup s, IsString s) => s -> s -> s
a <|> b = a <> "|" <> b

infixr 6 <:>
-- | Verkette zwei Strings mit einem Doppelpunkt.
-- 
-- Concatenate two strings with a colon.
(<:>) :: (Semigroup s, IsString s) => s -> s -> s
a <:> b = a <> ": " <> b

infixr 6 <!>
-- | Verkette zwei Strings mit einem Ausrufezeichen und einem Zeilenumbruch.
-- 
-- Concatenate two strings with a exclamation mark an a new line.
(<!>) :: (Semigroup s, IsString s) => s -> s -> s
a <!> b = a <> "!\n" <> b

infixr 6 <°>
-- | Verkette zwei Strings mit einem Pfeil.
-- 
-- Concatenate two strings with an arrow.
(<°>) :: (Semigroup s, IsString s) => s -> s -> s
a <°> b = a <> "->" <> b

infixr 6 <\>
-- | Verkette zwei Strings mit einem Zeilenumbruch.
-- 
-- Concatenate two strings with a new line.
(<\>) :: (Semigroup s, IsString s) => s -> s -> s
a <\> b = a <> "\n" <> b

-- * Text-Hilfsfunktionen
-- | Show for 'IsString'
showText :: (Show a, IsString s) => a -> s
showText = fromString . show

-- ** Unbekannte Eingabe melden
-- | Report an error due to _begründung_
fehlerText :: (Semigroup s, IsString s) => s -> s
fehlerText begründung = ungültigeEingabe <^> begründung <!> ""

-- | Report an error due to _begründung_ and print it to the console.
fehlerhafteEingabe :: Text -> IO ()
fehlerhafteEingabe begründung = T.putStrLn $ fehlerText begründung

-- ** GUI
-- | Mnemonic-Markierung hinzufügen
addMnemonic :: (Semigroup s, IsString s) => s -> s
addMnemonic s   = "_" <> s