{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Template-Haskell Deklarationen der Strings abhängig von der gewählten Sprache.

Strings werden via Template-Haskell abhängig von der Sprache importiert.  
Wenn eine String andere Sprache gewünscht wird kann dieser mit der gleichnamigen Funktionen mit angehängtem __S__ erhalten werden.
-}
module Zug.Language (module Zug.Language, module Zug.Language.Operatoren) where

import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.IO as T
-- Abhängigkeit von anderen Modulen
import Zug.Language.Operatoren
import Zug.Options
-- TH-Auswahl der Sprache
import Zug.Language.TemplateHaskell
import qualified Zug.Language.DE
import qualified Zug.Language.EN
-- Unsafe-IO
import System.IO.Unsafe (unsafePerformIO)

-- | Gewählte Sprache
gewählteSprache :: Sprache
gewählteSprache = unsafePerformIO $ getOptions >>= pure . sprache

-- * Titel / Title
-- | Title of the program / Zugkontrolle
erzeugeDeklaration "zugkontrolle"

-- * Haupt-Befehle / Main Orders
-- | Quit / Beenden
erzeugeDeklaration "beenden"
-- | Cancel / Abbrechen
erzeugeDeklaration "abbrechen"
-- | Undo / Rückgängig
erzeugeDeklaration "rückgängig"
-- | Continue / Weiter
erzeugeDeklaration "weiter"
-- | Back / Zurück
erzeugeDeklaration "zurück"
-- | Add / Hinzufügen
erzeugeDeklaration "hinzufügen"
-- | Remove / Entfernen
erzeugeDeklaration "entfernen"
-- | Save / Speichern
erzeugeDeklaration "speichern"
-- | Load / Laden
erzeugeDeklaration "laden"

-- * Spezielle Befehle / Special orders
-- | Speed / Geschwindigkeit
erzeugeDeklaration "geschwindigkeit"
-- | Turn around / Umdrehen
erzeugeDeklaration "umdrehen"
-- | Change / Stellen
erzeugeDeklaration "stellen"
-- | Current / Strom
erzeugeDeklaration "strom"
-- | On / An
erzeugeDeklaration "an"
-- | Off / Aus
erzeugeDeklaration "aus"
-- | Flowing / Fließend
erzeugeDeklaration "fließend"
-- | blocked / Gesperrt
erzeugeDeklaration "gesperrt"
-- | Uncouple / Kuppeln
erzeugeDeklaration "kuppeln"
-- | Adjust the Switches / Einstellen
erzeugeDeklaration "einstellen"
-- | Execute / Ausführen
erzeugeDeklaration "ausführen"
-- | Abort / Abbrechen
erzeugeDeklaration "ausführenAbbrechen"
-- | Wait / Warten
erzeugeDeklaration "warten"
-- | µs
-- 
-- Unit of time used to specify waiting time / µs
erzeugeDeklaration "wartenEinheit"
-- | Time / Zeit
erzeugeDeklaration "zeit"
-- | fließend <-> "Value"
erzeugeDeklaration "fließendValue"
-- | HIGH
erzeugeDeklaration "high"
-- | LOW
erzeugeDeklaration "low"

-- * Typ-Namen / Type names
-- | Object / Objekt
erzeugeDeklaration "objekt"
-- | Order / Befehl
erzeugeDeklaration "befehl"
-- | Train speed / Bahngeschwindigkeit
erzeugeDeklaration "bahngeschwindigkeit"
-- | Train speeds / Bahngeschwindigkeiten
erzeugeDeklaration "bahngeschwindigkeiten"
-- | Rail section / Streckenabschnitt
erzeugeDeklaration "streckenabschnitt"
-- | Rail sections / Streckenabschnitte
erzeugeDeklaration "streckenabschnitte"
-- | Switch / Weiche
erzeugeDeklaration "weiche"
-- | Switches / Weichen
erzeugeDeklaration "weichen"
-- | Coupler / Kupplung
erzeugeDeklaration "kupplung"
-- | Couplers / Kupplungen
erzeugeDeklaration "kupplungen"
-- | Rail collection / Wegstrecke
erzeugeDeklaration "wegstrecke"
-- | Rail collections / Wegstrecken
erzeugeDeklaration "wegstrecken"
-- | Plan / Plan
erzeugeDeklaration "plan"
-- | Plans / Pläne
erzeugeDeklaration "pläne"

-- * Eigenschafts/Feld-Namen / Attributes/Field names
-- | File path / Dateiname
erzeugeDeklaration "dateiname"
-- | Pin / Pin
erzeugeDeklaration "pin"
-- | Name / Name
erzeugeDeklaration "name"
-- | Direction / Richtung
erzeugeDeklaration "richtung"
-- | Directions / Richtungen
erzeugeDeklaration "richtungen"
-- | Direction of travel / Fahrtrichtung
erzeugeDeklaration "fahrtrichtung"

-- * Query-Abfragen / Queries
-- | Rail collection element / Wegstrecken-Element
erzeugeDeklaration "wegstreckenElement"
-- | Rail collection elements / Wegstrecken-Elemente
erzeugeDeklaration "wegstreckenElemente"
-- | action / Aktion
erzeugeDeklaration "aktion"
-- | actions / Aktionen
erzeugeDeklaration "aktionen"
-- | Train model / Zugtyp
erzeugeDeklaration "zugtyp"

-- | Ask to specify the object (type indicated by /s/) either by its index or name
erzeugeFunktionDeklaration "indexOderName"

-- | Ask to specify the count of the object (indicated by /s/)
erzeugeFunktionDeklaration "anzahl"

-- * Befehlsgruppen / Order classifications
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
-- | All supported actions for a currently executed 'Plan'
aktionPlanAusführend :: (Semigroup s, IsString s) => [s]
aktionPlanAusführend = [ausführenAbbrechen]
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
-- | zugkontrolle <:> "Execution requires root priviledges." / "Ausführung benötigt Root-Rechte!"
erzeugeDeklaration "nichtRoot"
-- | Not implemented: ToDo!!! / Nicht implementiert: ToDo!!!
erzeugeDeklaration "toDo"
-- | Invalid input / Ungültige Eingabe
erzeugeDeklaration "ungültigeEingabe"
-- | Action not supported / Aktion nicht unterstützt
erzeugeDeklaration "nichtUnterstützteAktion"
-- | File not found / Datei nicht gefunden/Format nicht erkannt
erzeugeDeklaration "nichtGefundeneDatei"
-- | Selected UI not supported. Cmd-UI is used instead. / Gewählte UI-Option nicht unterstützt! Nutze stattdessen Cmd-UI.
erzeugeDeklaration "uiNichtUnterstützt"
-- | erwartet "Integer"
erzeugeDeklaration "integerErwartet"
-- | erwartet richtung
erzeugeDeklaration "richtungErwartet"
-- | mindestens $ "one" / "eine" <~> richtung
erzeugeDeklaration "richtungZuWenig"
-- | mindestens $ "one" / "ein" <~> wegstreckenElement
erzeugeDeklaration "wegstreckeLeer"

-- | s <~> "not recognized" / "nicht erkannt"
erzeugeFunktionDeklaration "unbekannt"

-- | s <~> "expected" / "erwartet"
erzeugeFunktionDeklaration "erwartet"

-- | "At least" / "Mindestens" <~> s <~> "required" / "benötigt"
erzeugeFunktionDeklaration "mindestens"

-- * Typ-namen / Type names
-- | Undefined / Undefiniert
erzeugeDeklaration "undefiniert"
-- | Märklin / Märklin
erzeugeDeklaration "märklin"
-- | Lego / Lego
erzeugeDeklaration "lego"
-- | Straight / Gerade
erzeugeDeklaration "gerade"
-- | Turn / Kurve
erzeugeDeklaration "kurve"
-- | Left / Links
erzeugeDeklaration "links"
-- | Right / Rechts
erzeugeDeklaration "rechts"
-- | Forward / Vorwärts
erzeugeDeklaration "vorwärts"
-- | Reverse / Rückwärts
erzeugeDeklaration "rückwärts"

-- * Unbekannte Eingabe melden
-- | Report an error due to _begründung_
fehlerText :: (Semigroup s, IsString s) => s -> s
fehlerText begründung = ungültigeEingabe <^> begründung <!> ""

-- | Report an error due to _begründung_ and print it to the console.
fehlerhafteEingabe :: Text -> IO ()
fehlerhafteEingabe begründung = T.putStrLn $ fehlerText begründung