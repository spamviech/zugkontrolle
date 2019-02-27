{-# LANGUAGE NamedFieldPuns #-}

{-|
Description : Umwandeln einer Text-Eingabe in Token.
-}
module Zug.UI.Cmd.Lexer (lexer, Token(..), EingabeTokenAllgemein(..), EingabeToken(..)) where

-- Bibliotheken
import Text.Read
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Numeric.Natural
-- Abhängigkeiten von anderen Modulen
import Zug.Language

-- | Text-Eingabe in Token übersetzen
lexer :: [Text] -> [EingabeTokenAllgemein]
lexer   ([])    = []
lexer   (h:t)   = (lexOne h):(lexer t)

lexOne :: Text -> EingabeTokenAllgemein
lexOne  eingabe
    | istGleich eingabe beenden     = TkBeenden
    | istGleich eingabe abbrechen   = TkAbbrechen
lexOne  eingabe = Tk $ EingabeToken {eingabe, möglichkeiten=[token | (befehl, token) <- befehlToken, istBefehl eingabe befehl], ganzzahl=readMaybe $ unpack eingabe}
    where
        befehlToken :: [(Text, Token)]
        befehlToken = [
            (beenden            , Beenden),
            (abbrechen          , Abbrechen),
            (rückgängig         , Rückgängig),
            (hinzufügen         , Hinzufügen),
            (entfernen          , Entfernen),
            (speichern          , Speichern),
            (laden              , Laden),
            (geschwindigkeit    , Geschwindigkeit),
            (umdrehen           , Umdrehen),
            (stellen            , Stellen),
            (strom              , Strom),
            (fließend           , Fließend),
            (gesperrt           , Gesperrt),
            (an                 , An),
            (aus                , Aus),
            (kuppeln            , Kuppeln),
            (einstellen         , Einstellen),
            (ausführen          , Ausführen),
            (warten             , Warten),
            (plan               , Plan),
            (wegstrecke         , Wegstrecke),
            (weiche             , Weiche),
            (bahngeschwindigkeit, Bahngeschwindigkeit),
            (streckenabschnitt  , Streckenabschnitt),
            (kupplung           , Kupplung),
            (märklin            , Märklin),
            (lego               , Lego),
            (gerade             , Gerade),
            (kurve              , Kurve),
            (links              , Links),
            (rechts             , Rechts),
            (vorwärts           , Vorwärts),
            (rückwärts          , Rückwärts)]

-- | Summen-Typ aus UI-Befehlen oder 'EingabeToken'
data EingabeTokenAllgemein    = Tk            EingabeToken
                                | TkBeenden
                                | TkAbbrechen
                                    deriving (Show)

-- | Eingabe im Klartext, alle möglichen Interpretation der Eingabe und mögliche Umwandlung in ein 'Natural'
data EingabeToken = EingabeToken {eingabe::Text, möglichkeiten::[Token], ganzzahl::Maybe Natural}
                        deriving (Show)

-- | Bekannte Befehle
data Token  = Beenden
            | Abbrechen
            | Rückgängig
            | Hinzufügen
            | Entfernen
            | Speichern
            | Laden
            | Geschwindigkeit
            | Umdrehen
            | Stellen
            | Strom
            | Fließend
            | Gesperrt
            | An
            | Aus
            | Kuppeln
            | Einstellen
            | Ausführen
            | Warten
            | Plan
            | Wegstrecke
            | Weiche
            | Bahngeschwindigkeit
            | Streckenabschnitt
            | Kupplung
            | Märklin
            | Lego
            | Gerade
            | Kurve
            | Links
            | Rechts
            | Vorwärts
            | Rückwärts
                deriving (Eq, Show)

-- * Hilfs-Befehle
-- | Eingabe überprüfen (istBefehl <eingabe> <befehl>)
istBefehl :: Text -> Text -> Bool
istBefehl = istPrefix

-- | Teste auf Prefix-Eigenschaft ohne Berücksichtigung von Groß-/Klein-Schreibung
istPrefix :: Text -> Text -> Bool
istPrefix eingabe command = T.isPrefixOf (T.toCaseFold $ eingabe) (T.toCaseFold $ command)

-- | Teste auf Gleichheit ohne Berücksichtigung von Groß-/Klein-Schreibung
istGleich :: Text -> Text -> Bool
istGleich eingabe command = T.toCaseFold eingabe == T.toCaseFold command