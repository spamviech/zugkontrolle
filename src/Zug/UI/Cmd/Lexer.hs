{-# LANGUAGE NamedFieldPuns #-}

{-|
Description : Umwandeln einer Text-Eingabe in Token.
-}
module Zug.UI.Cmd.Lexer (lexer,Token(..),leeresToken,EingabeTokenAllgemein(..),EingabeToken(..)) where

import qualified Data.Text as Text
import Data.Text (Text,unpack)

import Numeric.Natural (Natural)

-- Bibliotheken
import Text.Read (readMaybe)

-- Abhängigkeiten von anderen Modulen
import Zug.Language (Sprache(),alleSprachen)
import qualified Zug.Language as Language

-- | Text-Eingabe in Token übersetzen
lexer :: [Text] -> [EingabeTokenAllgemein]
lexer = map lexEinen

lexEinen :: Text -> EingabeTokenAllgemein
lexEinen eingabe
  | istGleich eingabe Language.beenden = TkBeenden
  | istGleich eingabe Language.abbrechen = TkAbbrechen
lexEinen eingabe =
    Tk
    $ EingabeToken
    { eingabe
    , möglichkeiten = [token | (befehl,token) <- befehlToken, istBefehl eingabe befehl]
    , ganzzahl = readMaybe $ unpack eingabe
    }
  where
    befehlToken :: [(Sprache -> Text, Token)]
    befehlToken =
        [ (Language.beenden, Beenden)
        , (Language.abbrechen, Abbrechen)
        , (Language.rückgängig, Rückgängig)
        , (Language.hinzufügen, Hinzufügen)
        , (Language.entfernen, Entfernen)
        , (Language.speichern, Speichern)
        , (Language.laden, Laden)
        , (Language.geschwindigkeit, Geschwindigkeit)
        , (Language.umdrehen, Umdrehen)
        , (Language.fahrtrichtungEinstellen, FahrtrichtungEinstellen)
        , (Language.stellen, Stellen)
        , (Language.strom, Strom)
        , (Language.fließend, Fließend)
        , (Language.gesperrt, Gesperrt)
        , (Language.an, An)
        , (Language.aus, Aus)
        , (Language.kuppeln, Kuppeln)
        , (Language.einstellen, Einstellen)
        , (Language.ausführen, Ausführen)
        , (Language.ausführenAbbrechen, AusführenAbbrechen)
        , (Language.warten, Warten)
        , (Language.plan, Plan)
        , (Language.wegstrecke, Wegstrecke)
        , (Language.weiche, Weiche)
        , (Language.bahngeschwindigkeit, Bahngeschwindigkeit)
        , (Language.streckenabschnitt, Streckenabschnitt)
        , (Language.kupplung, Kupplung)
        , (Language.märklin, Märklin)
        , (Language.lego, Lego)
        , (Language.pin, Pin)
        , (Language.pcf8574Port, PCF8574Port)
        , (Language.normal, Normal)
        , (Language.a, A)
        , (Language.gerade, Gerade)
        , (Language.kurve, Kurve)
        , (Language.links, Links)
        , (Language.rechts, Rechts)
        , (Language.vorwärts, Vorwärts)
        , (Language.rückwärts, Rückwärts)
        , (Language.high, HIGH)
        , (Language.low, LOW)]

-- | Summen-Typ aus UI-Befehlen oder 'EingabeToken'
data EingabeTokenAllgemein
    = Tk EingabeToken
    | TkBeenden
    | TkAbbrechen
    deriving (Eq,Show)

-- | Eingabe im Klartext, alle möglichen Interpretation der Eingabe und mögliche Umwandlung in ein 'Natural'
data EingabeToken =
    EingabeToken
    { eingabe :: Text
    , möglichkeiten :: [Token]
    , ganzzahl :: Maybe Natural
    }
    deriving (Eq,Show)

-- | Ein Token ohne Eingabe. Wird für Anzeige einiger Anfrage-Typen benötigt.
leeresToken :: EingabeToken
leeresToken =
    EingabeToken
    { eingabe = Text.pack ""
    , möglichkeiten = []
    , ganzzahl = Nothing
    }

-- | Bekannte Befehle
data Token
    = Beenden
    | Abbrechen
    | Rückgängig
    | Sprache
    | Deutsch
    | Englisch
    | Hinzufügen
    | Entfernen
    | Speichern
    | Laden
    | Geschwindigkeit
    | Umdrehen
    | FahrtrichtungEinstellen
    | Stellen
    | Strom
    | Fließend
    | Gesperrt
    | An
    | Aus
    | Kuppeln
    | Einstellen
    | AktionAusführen   -- ^ Ausführen plan :: Aktion
    | Dauerschleife
    | EinfachAusführen
    | Ausführen
    | AusführenAbbrechen
    | Warten
    | Plan
    | Wegstrecke
    | Weiche
    | Bahngeschwindigkeit
    | Streckenabschnitt
    | Kupplung
    | Märklin
    | Lego
    | Pin
    | PCF8574Port
    | Normal
    | A
    | Gerade
    | Kurve
    | Links
    | Rechts
    | Vorwärts
    | Rückwärts
    | HIGH
    | LOW
    deriving (Eq,Show)

-- * Hilfs-Befehle
-- | Eingabe überprüfen (istBefehl <eingabe> <befehl>)
istBefehl :: Text -> (Sprache -> Text) -> Bool
istBefehl = istPrefix

-- | Teste auf Prefix-Eigenschaft ohne Berücksichtigung von Groß-/Klein-Schreibung
istPrefix :: Text -> (Sprache -> Text) -> Bool
istPrefix eingabe befehl = any (Text.isPrefixOf (Text.toCaseFold eingabe) . Text.toCaseFold . befehl) alleSprachen

-- | Teste auf Gleichheit ohne Berücksichtigung von Groß-/Klein-Schreibung
istGleich :: Text -> (Sprache -> Text) -> Bool
istGleich eingabe befehl = any (\sprache -> Text.toCaseFold eingabe == Text.toCaseFold (befehl sprache)) alleSprachen