{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description : Sammlung aller verwendeten Strings.

In diesem Modul werden alle verwendeten Strings sprachabhängig definiert.
Zusätzlich wird die 'Anzeige'-Klasse und einige Hilfsfunktionen und Operatoren definiert.

Die Dokumentation entspricht im allgemeinen nur der englischen Version.
Die deutsche Version ist im durch den Namen gegeben.
-}
module Zug.Language
  ( -- * Titel / Title
    zugkontrolle
    -- ** Version
  , versionValue
  , version
    -- * Haupt-Befehle / Main Orders
  , beenden
  , abbrechen
  , rückgängig
  , zurücksetzen
  , weiter
  , zurück
  , hinzufügen
  , entfernen
  , speichern
  , laden
  , sprache
  , deutsch
  , englisch
    -- * Spezielle Befehle / Special Orders
  , geschwindigkeit
  , umdrehen
  , fahrtrichtungEinstellen
  , stellen
  , strom
  , an
  , aus
  , fließend
  , gesperrt
  , kuppeln
  , einstellen
  , ausführen
  , ausführenAbbrechen
  , aktionGesperrt
  , warten
  , wartenEinheit
  , zeit
  , fließendValue
  , high
  , low
  , aktionAusführen
  , einfachAusführung
  , dauerschleife
  , wirdAusgeführt
  , ausführenGesperrt
    -- * Typ-Namen / Type names
  , objekt
  , befehl
  , bahngeschwindigkeit
  , bahngeschwindigkeiten
  , streckenabschnitt
  , streckenabschnitte
  , weiche
  , weichen
  , kupplung
  , kupplungen
  , wegstrecke
  , wegstrecken
  , plan
  , pläne
  , märklin
  , lego
  , gerade
  , kurve
  , links
  , rechts
  , vorwärts
  , rückwärts
    -- * Eingenschafts-/Feld-Namen / Attribute/Field Names
  , dateiname
  , name
  , richtung
  , richtungen
  , fahrtrichtung
  , anschluss
  , pin
  , pcf8574Port
  , pcf8574
  , variante
  , normal
  , a
  , a0
  , a1
  , a2
  , port
    -- * Query-Abfragen / Queries
  , wegstreckenElement
  , wegstreckenElemente
  , aktion
  , aktionen
  , zugtyp
  , welchesObjektHinzufügen
  , ausführModus
  , einzelseiten
  , indexOderName
  , anzahl
    -- * Fehlermeldungen / Error Messages
  , nichtRoot
  , toDo
  , ungültigeEingabe
  , nichtUnterstützteAktion
  , nichtGefundeneDatei
  , uiNichtUnterstützt
  , integerErwartet
  , richtungErwartet
  , richtungZuWenig
  , wegstreckeLeer
  , valueErwartet
  , unbekannt
  , erwartet
  , mindestens
    -- * Befehlsgruppen / Order classifications
  , befehlAlle
  , befehlTypen
  , befehlObjekte
  , befehlWegstreckenElemente
  , aktionGruppen
  , aktionPlan
  , aktionPlanAusführend
  , aktionPlanGesperrt
  , aktionWeiche
  , aktionBahngeschwindigkeit
  , aktionStreckenabschnitt
  , aktionKupplung
  , aktionWegstrecke
  , toBefehlsString
    -- * Unbekannte Eingabe melden
  , fehlerText
  , fehlerhafteEingabe
    -- * Datentyp / Data Type
  , Sprache(..)
  , MitSprache(..)
  , alleSprachen
    -- ** Typ-Klasse / Type Class
  , Anzeige(..)
  , ($#)
  , (.#)
    -- ** Hilfsfunktionen / Helper Functions
  , showText
  , addMnemonic
  , (<~>)
  , (<^>)
  , (<=>)
  , (<->)
  , (<|>)
  , (<:>)
  , (<!>)
  , (<°>)
  , (<\>)
  , (<#>)
  ) where

import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Version (Version, showVersion)
import Numeric.Natural (Natural)
-- Auto-generiertes Cabal-Modul
import qualified Paths_Zugkontrolle as Paths
import System.Hardware.WiringPi (Value(..))

-- * Titel / Title
-- | Train Control
zugkontrolle :: Sprache -> Text
zugkontrolle Deutsch = "Zugkontrolle"
zugkontrolle Englisch = "Train Control"

-- ** Version
-- | Aktuelle Version
versionValue :: Version
versionValue = Paths.version

-- | 'Text'-Ausgabe von 'versionValue'
version :: Text
version = Text.pack $ showVersion versionValue

-- * Haupt-Befehle / Main Orders
-- | Quit
beenden :: Sprache -> Text
beenden Deutsch = "Beenden"
beenden Englisch = "Quit"

-- | Cancel
abbrechen :: Sprache -> Text
abbrechen Deutsch = "Abbrechen"
abbrechen Englisch = "Cancel"

-- | Undo
rückgängig :: Sprache -> Text
rückgängig Deutsch = "Rückgängig"
rückgängig Englisch = "Undo"

-- | Reset
zurücksetzen :: Sprache -> Text
zurücksetzen Deutsch = "Zurücksetzen"
zurücksetzen Englisch = "Reset"

-- | Language
sprache :: Sprache -> Text
sprache Deutsch = "Sprache"
sprache Englisch = "Language"

-- | German
deutsch :: Sprache -> Text
deutsch Deutsch = "Deutsch"
deutsch Englisch = "German"

-- | English
englisch :: Sprache -> Text
englisch Deutsch = "Englisch"
englisch Englisch = "English"

-- | Continue
weiter :: Sprache -> Text
weiter Deutsch = "Weiter"
weiter Englisch = "Continue"

-- | Back
zurück :: Sprache -> Text
zurück Deutsch = "Zurück"
zurück Englisch = "Back"

-- | Add
hinzufügen :: Sprache -> Text
hinzufügen Deutsch = "Hinzufügen"
hinzufügen Englisch = "Add"

-- | Remove
entfernen :: Sprache -> Text
entfernen Deutsch = "Entfernen"
entfernen Englisch = "Remove"

-- | Save
speichern :: Sprache -> Text
speichern Deutsch = "Speichern"
speichern Englisch = "Save"

-- | Load
laden :: Sprache -> Text
laden Deutsch = "Laden"
laden Englisch = "Load"

-- * Spezielle Befehle / Special order
-- | Speed
geschwindigkeit :: Sprache -> Text
geschwindigkeit Deutsch = "Geschwindigkeit"
geschwindigkeit Englisch = "Speed"

-- | Turn around
umdrehen :: Sprache -> Text
umdrehen Deutsch = "Umdrehen"
umdrehen Englisch = "Turn around"

-- | Direction of Travel
fahrtrichtungEinstellen :: Sprache -> Text
fahrtrichtungEinstellen Deutsch = "Fahrtrichtung"
fahrtrichtungEinstellen Englisch = "Direction of Travel"

-- | Change
stellen :: Sprache -> Text
stellen Deutsch = "Stellen"
stellen Englisch = "Change"

-- | Current
strom :: Sprache -> Text
strom Deutsch = "Strom"
strom Englisch = "Current"

-- | On
an :: Sprache -> Text
an Deutsch = "An"
an Englisch = "Off"

-- | Off
aus :: Sprache -> Text
aus Deutsch = "Aus"
aus Englisch = "Off"

-- | Flowing
fließend :: Sprache -> Text
fließend Deutsch = "Fließend"
fließend Englisch = "Flowing"

-- | Blocked
gesperrt :: Sprache -> Text
gesperrt Deutsch = "Gesperrt"
gesperrt Englisch = "Blocked"

-- | Uncouple
kuppeln :: Sprache -> Text
kuppeln Deutsch = "Kuppeln"
kuppeln Englisch = "Uncouple"

-- | Adjust the Switches
einstellen :: Sprache -> Text
einstellen Deutsch = "Einstellen"
einstellen Englisch = "Adjust the Switches"

-- | Execute
ausführen :: Sprache -> Text
ausführen Deutsch = "Ausführen"
ausführen Englisch = "Execute"

-- | /s/ <~> "in execution"
wirdAusgeführt :: Sprache -> Text -> Text
wirdAusgeführt Deutsch s = s <~> ("wird ausgeführt" :: Text) $ Deutsch
wirdAusgeführt Englisch s = s <~> ("in execution" :: Text) $ Englisch

-- | Abort!  
-- Exclamation point is important to distinguish from 'abbrechen'; required for Cmd-UI
ausführenAbbrechen :: Sprache -> Text
ausführenAbbrechen Deutsch = "Abbrechen!"
ausführenAbbrechen Englisch = "Abort!"

-- | aktion <~> "blocked"
aktionGesperrt :: Sprache -> Text
aktionGesperrt Deutsch = aktion <~> ("gesperrt" :: Text) $ Deutsch
aktionGesperrt Englisch = aktion <~> ("blocked" :: Text) $ Englisch

-- | ausführen <~> "blocked" <!> "Pins" <~> /s/ <~> "are already in use."
ausführenGesperrt :: Sprache -> Text -> Text
ausführenGesperrt Deutsch s =
    ausführen
    <~> ("gesperrt" :: Text)
    <!> ("Die Pins" :: Text) <~> s <~> ("werden bereits verwendet." :: Text)
    $ Deutsch
ausführenGesperrt Englisch s =
    ausführen
    <~> ("blocked" :: Text) <!> ("Pins" :: Text) <~> s <~> ("are already in use." :: Text)
    $ Englisch

-- | Wait
warten :: Sprache -> Text
warten Deutsch = "Warten"
warten Englisch = "Wait"

-- | µs
--
-- Unit of time used to specify waiting time
wartenEinheit :: Sprache -> Text
wartenEinheit Deutsch = "µs"
wartenEinheit Englisch = "µs"

-- | Time
zeit :: Sprache -> Text
zeit Deutsch = "Zeit"
zeit Englisch = "Time"

-- | fließend <-> "Value"
fließendValue :: Sprache -> Text
fließendValue Deutsch = fließend <-> ("Value" :: Text) $ Deutsch
fließendValue Englisch = fließend <-> ("Value" :: Text) $ Englisch

-- | HIGH
high :: Sprache -> Text
high Deutsch = showText HIGH
high Englisch = showText HIGH

-- | LOW
low :: Sprache -> Text
low Deutsch = showText LOW
low Englisch = showText LOW

-- | Execute
aktionAusführen :: Sprache -> Text
aktionAusführen Deutsch = "Ausführen"
aktionAusführen Englisch = "Execute"

-- | Execute-Once
einfachAusführung :: Sprache -> Text
einfachAusführung Deutsch = "Einfach-Ausführung"
einfachAusführung Englisch = "Execute-Once"

-- | Loop
dauerschleife :: Sprache -> Text
dauerschleife Deutsch = "Dauerschleife"
dauerschleife Englisch = "Loop"

-- * Typ-Namen / Type names
-- | Object
objekt :: Sprache -> Text
objekt Deutsch = "Objekt"
objekt Englisch = "Object"

-- | Order
befehl :: Sprache -> Text
befehl Deutsch = "Befehl"
befehl Englisch = "Order"

-- | Train speed
bahngeschwindigkeit :: Sprache -> Text
bahngeschwindigkeit Deutsch = "Bahngeschwindigkeit"
bahngeschwindigkeit Englisch = "Train speed"

-- | Train speeds
bahngeschwindigkeiten :: Sprache -> Text
bahngeschwindigkeiten Deutsch = "Bahngeschwindigkeiten"
bahngeschwindigkeiten Englisch = "Train speeds"

-- | Rail section
streckenabschnitt :: Sprache -> Text
streckenabschnitt Deutsch = "Streckenabschnitt"
streckenabschnitt Englisch = "Rail section"

-- | Rail sections
streckenabschnitte :: Sprache -> Text
streckenabschnitte Deutsch = "Streckenabschnitte"
streckenabschnitte Englisch = "Rail sections"

-- | Switch
weiche :: Sprache -> Text
weiche Deutsch = "Weiche"
weiche Englisch = "Switch"

-- | Switches
weichen :: Sprache -> Text
weichen Deutsch = "Weichen"
weichen Englisch = "Switches"

-- | Coupler
kupplung :: Sprache -> Text
kupplung Deutsch = "Kupplung"
kupplung Englisch = "Coupler"

-- | Couplers
kupplungen :: Sprache -> Text
kupplungen Deutsch = "Kupplungen"
kupplungen Englisch = "Couplers"

-- | Rail collection
wegstrecke :: Sprache -> Text
wegstrecke Deutsch = "Wegstrecke"
wegstrecke Englisch = "Rail collection"

-- | Rail collections
wegstrecken :: Sprache -> Text
wegstrecken Deutsch = "Wegstrecken"
wegstrecken Englisch = "Rail collections"

-- | Plan
plan :: Sprache -> Text
plan Deutsch = "Plan"
plan Englisch = "Plan"

-- | Plans
pläne :: Sprache -> Text
pläne Deutsch = "Pläne"
pläne Englisch = "Plans"

-- | Märklin
märklin :: Sprache -> Text
märklin Deutsch = "Märklin"
märklin Englisch = "Märklin"

-- | Lego
lego :: Sprache -> Text
lego Deutsch = "Lego"
lego Englisch = "Lego"

-- | Straight
gerade :: Sprache -> Text
gerade Deutsch = "Gerade"
gerade Englisch = "Straight"

-- | Turn
kurve :: Sprache -> Text
kurve Deutsch = "Kurve"
kurve Englisch = "Turn"

-- | Left
links :: Sprache -> Text
links Deutsch = "Links"
links Englisch = "Left"

-- | Right
rechts :: Sprache -> Text
rechts Deutsch = "Rechts"
rechts Englisch = "Right"

-- | Forward
vorwärts :: Sprache -> Text
vorwärts Deutsch = "Vorwärts"
vorwärts Englisch = "Forward"

-- | Reverse
rückwärts :: Sprache -> Text
rückwärts Deutsch = "Rückwärts"
rückwärts Englisch = "Reverse"

-- * Eigenschafts/Feld-Namen / Attributes/Field names
-- | File path
dateiname :: Sprache -> Text
dateiname Deutsch = "Dateiname"
dateiname Englisch = "File path"

-- | Connection
anschluss :: Sprache -> Text
anschluss Deutsch = "Anschluss"
anschluss Englisch = "Connection"

-- | Pin
pin :: Sprache -> Text
pin Deutsch = "Pin"
pin Englisch = "Pin"

-- | PCF8574-Port
pcf8574Port :: Sprache -> Text
pcf8574Port = pcf8574 <-> port

-- | PCF8574
pcf8574 :: Sprache -> Text
pcf8574 Deutsch = "PCF8574"
pcf8574 Englisch = "PCF8574"

-- | Variant
variante :: Sprache -> Text
variante Deutsch = "Variante"
variante Englisch = "Variant"

-- | normal
normal :: Sprache -> Text
normal Deutsch = "normal"
normal Englisch = "normal"

-- | A
a :: Sprache -> Text
a Deutsch = "A"
a Englisch = "A"

-- | a0
a0 :: Sprache -> Text
a0 Deutsch = "a0"
a0 Englisch = "a0"

-- | a1
a1 :: Sprache -> Text
a1 Deutsch = "a1"
a1 Englisch = "a1"

-- | a2
a2 :: Sprache -> Text
a2 Deutsch = "a2"
a2 Englisch = "a2"

-- | Port
port :: Sprache -> Text
port Deutsch = "Port"
port Englisch = "Port"

-- | Name
name :: Sprache -> Text
name Deutsch = "Name"
name Englisch = "Name"

-- | Direction
richtung :: Sprache -> Text
richtung Deutsch = "Richtung"
richtung Englisch = "Direction"

-- | Directions
richtungen :: Sprache -> Text
richtungen Deutsch = "Richtungen"
richtungen Englisch = "Directions"

-- | Direction of travel
fahrtrichtung :: Sprache -> Text
fahrtrichtung Deutsch = "Fahrtrichtung"
fahrtrichtung Englisch = "Direction of travel"

-- * Query-Abfragen / Queries
-- | Rail collection element
wegstreckenElement :: Sprache -> Text
wegstreckenElement Deutsch = "Wegstrecken-Element"
wegstreckenElement Englisch = "Rail collection element"

-- | Rail collection elements
wegstreckenElemente :: Sprache -> Text
wegstreckenElemente Deutsch = "Wegstrecken-Elemente"
wegstreckenElemente Englisch = "Rail collection elements"

-- | Action
aktion :: Sprache -> Text
aktion Deutsch = "Aktion"
aktion Englisch = "Action"

-- | Actions
aktionen :: Sprache -> Text
aktionen Deutsch = "Aktionen"
aktionen Englisch = "Actions"

-- | Train model
zugtyp :: Sprache -> Text
zugtyp Deutsch = "Zugtyp"
zugtyp Englisch = "Train model"

-- | Execution mode
ausführModus :: Sprache -> Text
ausführModus Deutsch = "Ausführ-Modus"
ausführModus Englisch = "Execution mode"

-- | Which object should be added?
welchesObjektHinzufügen :: Sprache -> Text
welchesObjektHinzufügen Deutsch = "Welches Objekt soll hinzugefügt werden?"
welchesObjektHinzufügen Englisch = "Which object should be added?"

-- | Single Pages
einzelseiten :: Sprache -> Text
einzelseiten Deutsch = "Einzelseiten"
einzelseiten Englisch = "Single Pages"

-- | Ask to specify the object (type indicated by /s/) either by its index or name
indexOderName :: Sprache -> Text -> Text
indexOderName Deutsch s = s <~> ("Index/Name" :: Text) $ Deutsch
indexOderName Englisch s = s <~> ("Index/Name" :: Text) $ Englisch

-- | Ask to specify the count of the object (indicated by /s/)
anzahl :: Sprache -> Text -> Text
anzahl Deutsch s = ("Anzahl" :: Text) <~> s $ Deutsch
anzahl Englisch s = ("Count" :: Text) <~> s $ Englisch

-- * Fehlermeldungen / Error Messages
-- | zugkontrolle <:> "Execution requires root privileges."
nichtRoot :: Sprache -> Text
nichtRoot Deutsch = zugkontrolle <:> ("Ausführung benötigt Root-Rechte!" :: Text) $ Deutsch
nichtRoot Englisch = zugkontrolle <:> ("Execution requires root privileges." :: Text) $ Englisch

-- | Not implemented: ToDo!!!
toDo :: Sprache -> Text
toDo Deutsch = "Nicht implementiert: ToDo!!!"
toDo Englisch = "Not implemented: ToDo!!!"

-- | Invalid input
ungültigeEingabe :: Sprache -> Text
ungültigeEingabe Deutsch = "Ungültige Eingabe"
ungültigeEingabe Englisch = "Invalid input"

-- | Action not supported
nichtUnterstützteAktion :: Sprache -> Text
nichtUnterstützteAktion Deutsch = "Aktion nicht unterstützt"
nichtUnterstützteAktion Englisch = "Action not supported"

-- | File not found/Unknown format
nichtGefundeneDatei :: Sprache -> Text
nichtGefundeneDatei Deutsch = "Datei nicht gefunden/Format nicht erkannt"
nichtGefundeneDatei Englisch = "File not found/Unknown format"

-- | Selected UI not supported. Cmd-UI is used instead.
uiNichtUnterstützt :: Sprache -> Text
uiNichtUnterstützt Deutsch = "Gewählte UI-Option nicht unterstützt! Nutze stattdessen Cmd-UI."
uiNichtUnterstützt Englisch = "Selected UI not supported. Cmd-UI is used instead."

-- | erwartet "Integer"
integerErwartet :: Sprache -> Text
integerErwartet = erwartet $# ("Integer" :: Text)

-- | erwartet "Value"
valueErwartet :: Sprache -> Text
valueErwartet = erwartet $# ("Value" :: Text)

-- | erwartet richtung
richtungErwartet :: Sprache -> Text
richtungErwartet = erwartet $# richtung

-- | mindestens $ "one" <~> richtung
richtungZuWenig :: Sprache -> Text
richtungZuWenig Deutsch = mindestens Deutsch $ ("eine" :: Text) <~> richtung $ Deutsch
richtungZuWenig Englisch = mindestens Englisch $ ("one" :: Text) <~> richtung $ Englisch

-- | mindestens $ "one" <~> wegstreckenElement
wegstreckeLeer :: Sprache -> Text
wegstreckeLeer Deutsch = mindestens Deutsch $ ("ein" :: Text) <~> wegstreckenElement $ Deutsch
wegstreckeLeer Englisch = mindestens Englisch $ ("one" :: Text) <~> wegstreckenElement $ Englisch

-- | /s/ <~> "not recognized"
unbekannt :: Sprache -> Text -> Text
unbekannt Deutsch s = s <~> ("nicht erkannt" :: Text) $ Deutsch
unbekannt Englisch s = s <~> ("not recognized" :: Text) $ Englisch

-- | /s/ <~> "expected"
erwartet :: Sprache -> Text -> Text
erwartet Deutsch s = s <~> ("erwartet" :: Text) $ Deutsch
erwartet Englisch s = s <~> ("expected" :: Text) $ Englisch

-- | "At least" <~> /s/ <~> "required"
mindestens :: Sprache -> Text -> Text
mindestens Deutsch s = ("Mindestens" :: Text) <~> s <~> ("benötigt" :: Text) $ Deutsch
mindestens Englisch s = ("At least" :: Text) <~> s <~> ("required" :: Text) $ Englisch

-- * Befehlsgruppen / Order classifications
-- | All supported Orders in the main menu
befehlAlle :: Sprache -> [Text]
befehlAlle gewählteSprache =
    map ($ gewählteSprache) [beenden, sprache, hinzufügen, entfernen, speichern, laden]
    <> befehlTypen gewählteSprache

-- | All supported Orders, classified by a type
befehlTypen :: Sprache -> [Text]
befehlTypen sprache = [plan sprache] <> befehlObjekte sprache

-- | All supported Orders, classified by a (physical) object
befehlObjekte :: Sprache -> [Text]
befehlObjekte sprache = [wegstrecke sprache] <> befehlWegstreckenElemente sprache

-- | All supported Orders, classified by a train collection element
befehlWegstreckenElemente :: Sprache -> [Text]
befehlWegstreckenElemente sprache =
    map ($ sprache) [weiche, bahngeschwindigkeit, streckenabschnitt, kupplung]

-- | All supported actions
aktionGruppen :: Sprache -> [Text]
aktionGruppen sprache = map ($ sprache) [warten, aktionAusführen] <> befehlObjekte sprache

-- | All supported actions for a 'Plan'
aktionPlan :: Sprache -> [Text]
aktionPlan sprache = [ausführen sprache]

-- | All supported actions for a currently executed 'Plan'
aktionPlanAusführend :: Sprache -> [Text]
aktionPlanAusführend sprache = [ausführenAbbrechen sprache]

-- | All supported actions for a blocked 'Plan'
aktionPlanGesperrt :: Sprache -> [Text]
aktionPlanGesperrt _sprache = []

-- | All supported actions for a train collection ('Wegstrecke')
aktionWegstrecke :: Sprache -> [Text]
aktionWegstrecke sprache =
    [einstellen sprache]
    <> aktionBahngeschwindigkeit sprache
    <> aktionStreckenabschnitt sprache
    <> aktionKupplung sprache

-- | All supported actions for a switch ('Weiche')
aktionWeiche :: Sprache -> [Text]
aktionWeiche sprache = [stellen sprache]

-- | All supported actions for a train speed ('Bahngeschwindigkeit')
aktionBahngeschwindigkeit :: Sprache -> [Text]
aktionBahngeschwindigkeit sprache = map ($ sprache) [geschwindigkeit, umdrehen]

-- | All supported actions for a rail section ('Streckenabschnitt')
aktionStreckenabschnitt :: Sprache -> [Text]
aktionStreckenabschnitt sprache = [strom sprache]

-- | All supported actions for a coupler ('Kupplung')
aktionKupplung :: Sprache -> [Text]
aktionKupplung sprache = [kuppeln sprache]

-- | Concatenate a list of strings to an eye-pleasing format
toBefehlsString :: [Text] -> Text
toBefehlsString [] = "[]"
toBefehlsString [s] = s
-- Sprach-Angabe hier irrelevant, da Texteingabe unbeeinflusst von der gewählten Sprache bleibt
toBefehlsString (h:t) = h <^> toBefehlsString t $ Deutsch

-- * Unbekannte Eingabe melden
-- | Report an error due to _begründung_
fehlerText :: Sprache -> Text -> Text
fehlerText sprache begründung = ungültigeEingabe <^> begründung <!> Text.empty $ sprache

-- | Report an error due to _begründung_ and print it to the console.
fehlerhafteEingabe :: Sprache -> Text -> IO ()
fehlerhafteEingabe sprache begründung = Text.putStrLn $ fehlerText sprache begründung

-- * Datentypen
-- | Bekannte Sprachen
data Sprache
    = Deutsch
    | Englisch
    deriving (Show, Read, Bounded, Enum, Eq)

-- | Klasse für Typen, die als 'Sprache' verwendet werden können.
class MitSprache s where
    leseSprache :: (Sprache -> a) -> s -> a

instance MitSprache Sprache where
    leseSprache :: (Sprache -> a) -> Sprache -> a
    leseSprache = id

-- | Alle unterstützten Sprachen
alleSprachen :: [Sprache]
alleSprachen = [minBound .. maxBound]

-- ** Typ-Klasse
-- | Zeige ein Objekt sprachabhängig an.
class Anzeige a where
    anzeige :: a -> Sprache -> Text
    default anzeige :: (Show a) => a -> Sprache -> Text
    anzeige = const . showText

instance Anzeige Text where
    anzeige :: Text -> Sprache -> Text
    anzeige = const

instance Anzeige (Sprache -> Text) where
    anzeige :: (Sprache -> Text) -> Sprache -> Text
    anzeige = id

instance Anzeige Char where
    anzeige :: Char -> Sprache -> Text
    anzeige a = Text.pack . const [a]

instance Anzeige Natural

instance Anzeige Int

instance Anzeige Double

instance Anzeige Value

instance Anzeige Sprache where
    anzeige :: Sprache -> Sprache -> Text
    anzeige Deutsch = deutsch
    anzeige Englisch = englisch

instance (Anzeige a) => Anzeige [a] where
    anzeige :: [a] -> Sprache -> Text
    anzeige liste = ("[" :: Text) <#> anzeigeAux liste <#> ("]" :: Text)
        where
            anzeigeAux :: (Anzeige b) => [b] -> Sprache -> Text
            anzeigeAux [] = const ""
            anzeigeAux [b] = anzeige b
            anzeigeAux (h:t) = h <^> anzeigeAux t

instance (Anzeige a, Anzeige b) => Anzeige (a, b) where
    anzeige :: (a, b) -> Sprache -> Text
    anzeige (a, b) = ("(" :: Text) <#> a <^> b <#> (")" :: Text)

infixr 0 $#

-- | Werte eine 'Sprache'-abhänge Funktion mit einem 'Anzeige'-Typen aus.
-- Die Fix-Stärke ist dabei identisch zu '$' gewählt.
($#) :: (Anzeige a) => (Sprache -> Text -> b) -> a -> Sprache -> b
($#) f a sprache = f sprache $ anzeige a sprache

infixr 9 .#

-- | Verkette eine 'Sprache'-abhängige Funktion mit einer Funktion, die einen 'Anzeige'-Typ liefert.
(.#) :: (Anzeige b) => (Sprache -> Text -> c) -> (a -> b) -> a -> Sprache -> c
(.#) f g a = f $# g a

-- ** Hilfsfunktionen
verketten :: (Anzeige a, Anzeige b) => Text -> a -> b -> Sprache -> Text
verketten trennzeichen a b sprache = anzeige a sprache <> trennzeichen <> anzeige b sprache

infixr 6 <~>

-- | Verkette zwei Strings mit einem Leerzeichen.
--
-- Concatenate two strings with a space.
(<~>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<~>) = verketten " "

infixr 6 <^>

-- | Verkette zwei Strings mit einem Komma.
--
-- Concatenate two strings with a comma.
(<^>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<^>) = verketten ", "

infixr 6 <=>

-- | Verkette zwei Strings mit einem Gleichheitszeichen.
--
-- Concatenate two strings with a equal sign.
(<=>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<=>) = verketten "="

infixr 6 <->

-- | Verkette zwei Strings mit einem Bindestrich.
--
-- Concatenate two strings with a hyphen.
(<->) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<->) = verketten "-"

infixr 6 <|>

-- | Verkette zwei Strings mit einem '|'.
--
-- Concatenate two strings with a '|'.
(<|>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<|>) = verketten "|"

infixr 6 <:>

-- | Verkette zwei Strings mit einem Doppelpunkt.
--
-- Concatenate two strings with a colon.
(<:>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<:>) = verketten ": "

infixr 6 <!>

-- | Verkette zwei Strings mit einem Ausrufezeichen und einem Zeilenumbruch.
--
-- Concatenate two strings with a exclamation mark an a new line.
(<!>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<!>) = verketten "!\n"

infixr 6 <°>

-- | Verkette zwei Strings mit einem Pfeil.
--
-- Concatenate two strings with an arrow.
(<°>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<°>) = verketten "->"

infixr 6 <\>

-- | Verkette zwei Strings mit einem Zeilenumbruch.
--
-- Concatenate two strings with a new line.
(<\>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<\>) = verketten "\n"

infixr 6 <#>

-- | Verkette zwi Strings.
--
-- Concatenate two strings
(<#>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<#>) = verketten Text.empty

-- | Show for 'Text'
showText :: (Show a) => a -> Text
showText = Text.pack . show

-- | Mnemonic-Markierung hinzufügen
addMnemonic :: Text -> Text
addMnemonic = Text.cons '_'