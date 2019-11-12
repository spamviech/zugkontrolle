{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Version (showVersion)
-- Abhängigkeit von anderen Modulen
import Zug.Language.Operatoren ((<~>), (<^>), (<=>), (<->), (<|>), (<:>), (<!>), (<°>), (<\>), showText, addMnemonic)
import Zug.Options (Sprache())
import qualified Zug.Options as Options
-- TH-Auswahl der Sprache
import Zug.Language.TemplateHaskell (erzeugeFunktion)
import qualified Zug.Language.DE
import qualified Zug.Language.EN

-- | Zeige ein Objekt sprachabhängig an.
class Anzeige a where
    anzeige :: a -> Sprache -> Text

instance {-# Overlappable #-} (Show a) => Anzeige a where
    anzeige :: a -> Sprache -> Text
    anzeige a = const $ showText a

instance Anzeige (Sprache -> Text) where
    anzeige :: (Sprache -> Text) -> Sprache -> Text
    anzeige = id

-- * Titel / Title
erzeugeFunktion "zugkontrolle"
-- ** Version
version :: Text
version = fromString $ showVersion Options.version
-- * Haupt-Befehle / Main Orders
concatMapM erzeugeFunktion
    ["beenden", "abbrechen", "rückgängig", "weiter", "zurück", "hinzufügen", "entfernen", "speichern", "laden"]
-- * Spezielle Befehle / Special order
concatMapM erzeugeFunktion [
    "geschwindigkeit", "umdrehen", "fahrtrichtungEinstellen", "stellen", "strom", "an", "aus", "fließend", "gesperrt",
    "kuppeln", "einstellen", "ausführen", "ausführenAbbrechen", "aktionGesperrt", "warten", "wartenEinheit", "zeit",
    "fließendValue", "high", "low"]
concatMapM erzeugeFunktion ["aktionAusführen", "einfachAusführung", "dauerschleife"]
-- * Typ-Namen / Type names
concatMapM erzeugeFunktion [
    "objekt", "befehl", "bahngeschwindigkeit", "bahngeschwindigkeiten", "streckenabschnitt", "streckenabschnitte",
    "weiche", "weichen", "kupplung", "kupplungen", "wegstrecke", "wegstrecken", "plan", "pläne"]
-- * Eigenschafts/Feld-Namen / Attributes/Field names
concatMapM erzeugeFunktion [
    "dateiname", "name", "richtung", "richtungen", "fahrtrichtung", "anschluss",
    "pin", "pcf8574Port", "pcf8574", "variante", "normal", "a", "a0", "a1", "a2", "port"]
-- * Query-Abfragen / Queries
concatMapM erzeugeFunktion ["wegstreckenElement", "wegstreckenElemente", "aktion", "aktionen", "zugtyp"]
concatMapM erzeugeFunktion ["welchesObjektHinzufügen", "ausführModus"]
-- * Fehlermeldungen / Error Messages
concatMapM erzeugeFunktion [
    "nichtRoot", "toDo", "ungültigeEingabe", "nichtUnterstützteAktion", "nichtGefundeneDatei", "uiNichtUnterstützt",
    "integerErwartet", "richtungErwartet", "richtungZuWenig", "wegstreckeLeer", "valueErwartet"]
-- * Typ-namen / Type names
concatMapM erzeugeFunktion ["märklin", "lego", "gerade", "kurve", "links", "rechts", "vorwärts", "rückwärts"]

concatMapM erzeugeFunktion [
    -- * Spezielle Befehle / Special orders
    "wirdAusgeführt", "ausführenGesperrt",
    -- * Query-Abfragen / Queries
    "indexOderName", "anzahl",
    -- * Fehlermeldungen / Error Messages
    "unbekannt", "erwartet", "mindestens"]

-- * Befehlsgruppen / Order classifications
-- | All supported Orders in the main menu
befehlAlle :: Sprache -> [Text]
befehlAlle sprache = map ($ sprache) [beenden, hinzufügen, entfernen, speichern, laden] <> befehlTypen sprache
-- | All supported Orders, classified by a type
befehlTypen :: Sprache -> [Text]
befehlTypen sprache = [plan sprache] <> befehlObjekte sprache
-- | All supported Orders, classified by a (physical) object
befehlObjekte :: Sprache -> [Text]
befehlObjekte sprache = [wegstrecke sprache] <> befehlWegstreckenElemente sprache
-- | All supported Orders, classified by a train collection element
befehlWegstreckenElemente :: Sprache -> [Text]
befehlWegstreckenElemente sprache = map ($ sprache) [weiche, bahngeschwindigkeit, streckenabschnitt, kupplung]
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
aktionWegstrecke sprache
    = [einstellen sprache]
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
toBefehlsString []      = "[]"
toBefehlsString [s]     = s
toBefehlsString (h:t)   = h <^> toBefehlsString t

-- * Unbekannte Eingabe melden
-- | Report an error due to _begründung_
fehlerText :: Text -> Sprache -> Text
fehlerText begründung sprache = ungültigeEingabe sprache <^> begründung <!> ""

-- | Report an error due to _begründung_ and print it to the console.
fehlerhafteEingabe :: Text -> Sprache -> IO ()
fehlerhafteEingabe begründung sprache = T.putStrLn $ fehlerText begründung sprache