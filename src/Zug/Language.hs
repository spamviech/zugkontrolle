{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Sammlung aller verwendeten Strings, zur einfacheren Unterstützung einer neuen Sprache.
-}
module Zug.Language where

import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.IO as T

-- * Titel
zugkontrolle :: (Semigroup s, IsString s) => s
zugkontrolle                    = "Zugkontrolle"

-- * Haupt-Befehle
beenden :: (Semigroup s, IsString s) => s
beenden                         = "Beenden"
abbrechen :: (Semigroup s, IsString s) => s
abbrechen                       = "Abbrechen"
rückgängig :: (Semigroup s, IsString s) => s
rückgängig                      = "Rückgängig"
weiter :: (Semigroup s, IsString s) => s
weiter                          = "Weiter"
zurück :: (Semigroup s, IsString s) => s
zurück                          = "Zurück"
hinzufügen :: (Semigroup s, IsString s) => s
hinzufügen                      = "Hinzufügen"
entfernen :: (Semigroup s, IsString s) => s
entfernen                       = "Entfernen"
speichern :: (Semigroup s, IsString s) => s
speichern                       = "Speichern"
laden :: (Semigroup s, IsString s) => s
laden                           = "Laden"

-- * Spezielle Befehle
geschwindigkeit :: (Semigroup s, IsString s) => s
geschwindigkeit                 = "Geschwindigkeit"
umdrehen :: (Semigroup s, IsString s) => s
umdrehen                        = "Umdrehen"
stellen :: (Semigroup s, IsString s) => s
stellen                         = "Stellen"
strom :: (Semigroup s, IsString s) => s
strom                           = "Strom"
an :: (Semigroup s, IsString s) => s
an                              = "An"
aus :: (Semigroup s, IsString s) => s
aus                             = "Aus"
kuppeln :: (Semigroup s, IsString s) => s
kuppeln                         = "Kuppeln"
einstellen :: (Semigroup s, IsString s) => s
einstellen                      = "Einstellen"
ausführen :: (Semigroup s, IsString s) => s
ausführen                       = "Ausführen"
warten :: (Semigroup s, IsString s) => s
warten                          = "Warten"
wartenEinheit :: (Semigroup s, IsString s) => s
wartenEinheit                   = "µs"
zeit :: (Semigroup s, IsString s) => s
zeit                            = "Zeit"

-- * Typ-Namen
objekt :: (Semigroup s, IsString s) => s
objekt                          = "Objekt"
befehl :: (Semigroup s, IsString s) => s
befehl                          = "Befehl"
bahngeschwindigkeit :: (Semigroup s, IsString s) => s
bahngeschwindigkeit             = "Bahngeschwindigkeit"
bahngeschwindigkeiten :: (Semigroup s, IsString s) => s
bahngeschwindigkeiten           = "Bahngeschwindigkeiten"
streckenabschnitt :: (Semigroup s, IsString s) => s
streckenabschnitt               = "Streckenabschnitt"
streckenabschnitte :: (Semigroup s, IsString s) => s
streckenabschnitte              = "Streckenabschnitte"
weiche :: (Semigroup s, IsString s) => s
weiche                          = "Weiche"
weichen :: (Semigroup s, IsString s) => s
weichen                         = "Weichen"
kupplungen :: (Semigroup s, IsString s) => s
kupplungen                      = "Kupplungen"
kupplung :: (Semigroup s, IsString s) => s
kupplung                        = "Kupplung"
wegstrecke :: (Semigroup s, IsString s) => s
wegstrecke                      = "Wegstrecke"
wegstrecken :: (Semigroup s, IsString s) => s
wegstrecken                     = "Wegstrecken"
plan :: (Semigroup s, IsString s) => s
plan                            = "Plan"
pläne :: (Semigroup s, IsString s) => s
pläne                           = "Pläne"

-- * Eigenschafts/Feld-Namen
dateiname :: (Semigroup s, IsString s) => s
dateiname                       = "Dateiname"
pin :: (Semigroup s, IsString s) => s
pin                             = "Pin"
name :: (Semigroup s, IsString s) => s
name                            = "Name"
richtung :: (Semigroup s, IsString s) => s
richtung                        = "Richtung"
richtungen :: (Semigroup s, IsString s) => s
richtungen                      = "Richtungen"
fahrtrichtung :: (Semigroup s, IsString s) => s
fahrtrichtung                   = "Fahrtrichtung"

-- * Query-Abfragen
wegstreckenElemente :: (Semigroup s, IsString s) => s
wegstreckenElemente             = "Wegstrecken-Elemente"
wegstreckenElement :: (Semigroup s, IsString s) => s
wegstreckenElement              = "Wegstrecken-Element"
aktionen :: (Semigroup s, IsString s) => s
aktionen                        = "Aktionen"
aktion :: (Semigroup s, IsString s) => s
aktion                          = "Aktion"
zugtyp :: (Semigroup s, IsString s) => s
zugtyp                          = "Zugtyp"

indexOderName :: (Semigroup s, IsString s) => s -> s
indexOderName   s   = s <~> "Index/Name"

anzahl :: (Semigroup s, IsString s) => s -> s
anzahl  s   = "Anzahl" <~> s

-- Befehlsgruppen
befehlAlle :: (Semigroup s, IsString s) => [s]
befehlAlle = [beenden, hinzufügen, entfernen, speichern, laden] <> befehlTypen
befehlTypen :: (Semigroup s, IsString s) => [s]
befehlTypen = [plan] <> befehlObjekte
befehlObjekte :: (Semigroup s, IsString s) => [s]
befehlObjekte = [wegstrecke] <> befehlWegstreckenElemente
befehlWegstreckenElemente :: (Semigroup s, IsString s) => [s]
befehlWegstreckenElemente = [weiche, bahngeschwindigkeit, streckenabschnitt, kupplung]
aktionGruppen :: (Semigroup s, IsString s) => [s]
aktionGruppen = [warten] <> befehlObjekte
aktionPlan :: (Semigroup s, IsString s) => [s]
aktionPlan = [ausführen]
aktionWegstrecke :: (Semigroup s, IsString s) => [s]
aktionWegstrecke = [einstellen] <> aktionBahngeschwindigkeit <> aktionStreckenabschnitt <> aktionKupplung
aktionWeiche :: (Semigroup s, IsString s) => [s]
aktionWeiche = [stellen]
aktionBahngeschwindigkeit :: (Semigroup s, IsString s) => [s]
aktionBahngeschwindigkeit = [geschwindigkeit, umdrehen]
aktionStreckenabschnitt :: (Semigroup s, IsString s) => [s]
aktionStreckenabschnitt = [strom]
aktionKupplung :: (Semigroup s, IsString s) => [s]
aktionKupplung = [kuppeln]

toBefehlsString :: (Semigroup s, IsString s) => [s] -> s
toBefehlsString ([])    = ""
toBefehlsString ([s])   = s
toBefehlsString (h:t)   = h <^> toBefehlsString t

-- * Fehlermeldungen
nichtRoot :: (Semigroup s, IsString s) => s
nichtRoot                       = zugkontrolle <:> "Ausführung benötigt Root-Rechte!"
toDo :: (Semigroup s, IsString s) => s
toDo                            = "Nicht implementiert: ToDo!!!"
ungültigeEingabe :: (Semigroup s, IsString s) => s
ungültigeEingabe                = "Ungültige Eingabe"
nichtUnterstützteAktion :: (Semigroup s, IsString s) => s
nichtUnterstützteAktion         = "Aktion nicht unterstützt"
nichtGefundeneDatei :: (Semigroup s, IsString s) => s
nichtGefundeneDatei             = "Datei nicht gefunden/Format nicht erkannt"
uiNichtUnterstützt :: (Semigroup s, IsString s) => s
uiNichtUnterstützt              = "Gewählte UI-Option nicht unterstützt! Nutze stattdessen Cmd-UI."
integerErwartet :: (Semigroup s, IsString s) => s
integerErwartet                 = erwartet "Integer"
richtungErwartet :: (Semigroup s, IsString s) => s
richtungErwartet                = erwartet richtung
richtungZuWenig :: (Semigroup s, IsString s) => s
richtungZuWenig                 = mindestens $ "eine " <> richtung
wegstreckeLeer :: (Semigroup s, IsString s) => s
wegstreckeLeer                  = mindestens $ "ein " <> wegstreckenElement

unbekannt :: (Semigroup s, IsString s) => s -> s
unbekannt   s   = s <~> "nicht erkannt"

erwartet :: (Semigroup s, IsString s) => s -> s
erwartet    s   = s <~> "erwartet"

mindestens :: (Semigroup s, IsString s) => s -> s
mindestens  s   = "Mindestens" <~> s <~> "benötigt"

-- * Klassennamen
undefiniert :: (Semigroup s, IsString s) => s
undefiniert                     = "Undefiniert"
märklin :: (Semigroup s, IsString s) => s
märklin                         = "Märklin"
lego :: (Semigroup s, IsString s) => s
lego                            = "Lego"
gerade :: (Semigroup s, IsString s) => s
gerade                          = "Gerade"
kurve :: (Semigroup s, IsString s) => s
kurve                           = "Kurve"
links :: (Semigroup s, IsString s) => s
links                           = "Links"
rechts :: (Semigroup s, IsString s) => s
rechts                          = "Rechts"
vorwärts :: (Semigroup s, IsString s) => s
vorwärts                        = "Vorwärts"
rückwärts :: (Semigroup s, IsString s) => s
rückwärts                       = "Rückwärts"

-- * Strings mit Leerzeichen/Trennzeichen verknüpfen
infixr 6 <~>
(<~>) :: (Semigroup s, IsString s) => s -> s -> s
a <~> b = a <> " " <> b

infixr 6 <^>
(<^>) :: (Semigroup s, IsString s) => s -> s -> s
a <^> b = a <> ", " <> b

infixr 6 <=>
(<=>) :: (Semigroup s, IsString s) => s -> s -> s
a <=> b = a <> "=" <> b

infixr 6 <->
(<->) :: (Semigroup s, IsString s) => s -> s -> s
a <-> b = a <> "-" <> b

infixr 6 <|>
(<|>) :: (Semigroup s, IsString s) => s -> s -> s
a <|> b = a <> "|" <> b

infixr 6 <:>
(<:>) :: (Semigroup s, IsString s) => s -> s -> s
a <:> b = a <> ": " <> b

infixr 6 <!>
(<!>) :: (Semigroup s, IsString s) => s -> s -> s
a <!> b = a <> "!\n" <> b

infixr 6 <°>
(<°>) :: (Semigroup s, IsString s) => s -> s -> s
a <°> b = a <> "->" <> b

infixr 6 <\>
(<\>) :: (Semigroup s, IsString s) => s -> s -> s
a <\> b = a <> "\n" <> b

-- * Text-Hilfsfunktionen
showText :: (Show a, IsString s) => a -> s
showText = fromString . show

-- ** Unbekannte Eingabe melden
fehlerText :: (Semigroup s, IsString s) => s -> s
fehlerText begründung = ungültigeEingabe <^> begründung <!> ""

fehlerhafteEingabe :: Text -> IO ()
fehlerhafteEingabe begründung = T.putStrLn $ fehlerText begründung

-- ** GUI
-- | Mnemonic-Markierung hinzufügen
addMnemonic :: (Semigroup s, IsString s) => s -> s
addMnemonic s   = "_" <> s