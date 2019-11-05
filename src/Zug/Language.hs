{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Template-Haskell Deklarationen der Strings abhängig von der gewählten Sprache.

Strings werden via Template-Haskell abhängig von der Sprache importiert.  
Wenn eine String andere Sprache gewünscht wird kann dieser mit der gleichnamigen Funktionen mit angehängtem __S__ erhalten werden.
-}
module Zug.Language (module Zug.Language, module Zug.Language.Operatoren) where

import MonadUtils (concatMapM)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.IO as T
-- Abhängigkeit von anderen Modulen
import Zug.Language.Operatoren ((<~>), (<^>), (<=>), (<->), (<|>), (<:>), (<!>), (<°>), (<\>), showText, addMnemonic)
import Zug.Options (getOptions, Options(..), Sprache())
-- TH-Auswahl der Sprache
import Zug.Language.TemplateHaskell (erzeugeDeklaration, erzeugeFunktionDeklaration)
import qualified Zug.Language.DE
import qualified Zug.Language.EN
-- Unsafe-IO
import System.IO.Unsafe (unsafePerformIO)

-- | Gewählte Sprache
gewählteSprache :: Sprache
{-# NOINLINE gewählteSprache #-}
gewählteSprache = unsafePerformIO $ getOptions >>= pure . sprache

-- * Titel / Title
erzeugeDeklaration "zugkontrolle"
-- * Haupt-Befehle / Main Orders
concatMapM erzeugeDeklaration
    ["beenden", "abbrechen", "rückgängig", "weiter", "zurück", "hinzufügen", "entfernen", "speichern", "laden"]
-- * Spezielle Befehle / Special order
concatMapM erzeugeDeklaration [
    "geschwindigkeit", "umdrehen", "fahrtrichtungEinstellen", "stellen", "strom", "an", "aus", "fließend", "gesperrt",
    "kuppeln", "einstellen", "ausführen", "ausführenAbbrechen", "aktionGesperrt", "warten", "wartenEinheit", "zeit",
    "fließendValue", "high", "low"]
concatMapM erzeugeDeklaration ["aktionAusführen", "einfachAusführung", "dauerschleife"]
-- * Typ-Namen / Type names
concatMapM erzeugeDeklaration [
    "objekt", "befehl", "bahngeschwindigkeit", "bahngeschwindigkeiten", "streckenabschnitt", "streckenabschnitte",
    "weiche", "weichen", "kupplung", "kupplungen", "wegstrecke", "wegstrecken", "plan", "pläne"]
-- * Eigenschafts/Feld-Namen / Attributes/Field names
concatMapM erzeugeDeklaration [
    "dateiname", "name", "richtung", "richtungen", "fahrtrichtung", "anschluss",
    "pin", "pcf8574Port", "pcf8574", "variante", "normal", "a", "a0", "a1", "a2", "port"]
-- * Query-Abfragen / Queries
concatMapM erzeugeDeklaration ["wegstreckenElement", "wegstreckenElemente", "aktion", "aktionen", "zugtyp"]
concatMapM erzeugeDeklaration ["welchesObjektHinzufügen", "ausführModus"]
-- * Fehlermeldungen / Error Messages
concatMapM erzeugeDeklaration [
    "nichtRoot", "toDo", "ungültigeEingabe", "nichtUnterstützteAktion", "nichtGefundeneDatei", "uiNichtUnterstützt",
    "integerErwartet", "richtungErwartet", "richtungZuWenig", "wegstreckeLeer", "valueErwartet"]
-- * Typ-namen / Type names
concatMapM erzeugeDeklaration ["märklin", "lego", "gerade", "kurve", "links", "rechts", "vorwärts", "rückwärts"]

concatMapM erzeugeFunktionDeklaration [
    -- * Spezielle Befehle / Special orders
    "wirdAusgeführt", "ausführenGesperrt",
    -- * Query-Abfragen / Queries
    "indexOderName", "anzahl",
    -- * Fehlermeldungen / Error Messages
    "unbekannt", "erwartet", "mindestens"]

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
aktionGruppen = [warten, aktionAusführen] <> befehlObjekte
-- | All supported actions for a 'Plan'
aktionPlan :: (Semigroup s, IsString s) => [s]
aktionPlan = [ausführen]
-- | All supported actions for a currently executed 'Plan'
aktionPlanAusführend :: (Semigroup s, IsString s) => [s]
aktionPlanAusführend = [ausführenAbbrechen]
-- | All supported actions for a blocked 'Plan'
aktionPlanGesperrt :: (Semigroup s, IsString s) => [s]
aktionPlanGesperrt = []
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
toBefehlsString []      = "[]"
toBefehlsString [s]     = s
toBefehlsString (h:t)   = h <^> toBefehlsString t

-- * Unbekannte Eingabe melden
-- | Report an error due to _begründung_
fehlerText :: (Semigroup s, IsString s) => s -> s
fehlerText begründung = ungültigeEingabe <^> begründung <!> ""

-- | Report an error due to _begründung_ and print it to the console.
fehlerhafteEingabe :: Text -> IO ()
fehlerhafteEingabe begründung = T.putStrLn $ fehlerText begründung