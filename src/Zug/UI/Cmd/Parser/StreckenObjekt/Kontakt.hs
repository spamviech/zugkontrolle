{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.StreckenObjekt.Kontakt (AnfrageKontakt(..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import Zug.Anbindung
       (Value(..), alleValues, Anschluss(), Kontakt(..), MitInterruptPin(MitInterruptPin)
      , InterruptPinBenötigt(InterruptPinBenötigt))
import Zug.Language (Anzeige(..), Sprache(), (<^>), (<=>), (<->), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), MitAnfrage(..), zeigeAnfrageFehlgeschlagenStandard
                                , AnfrageFortsetzung(..), wähleZwischenwert, ($<<))
import Zug.UI.Cmd.Parser.Anschluss (AnfrageAnschluss(AnfrageAnschluss))

-- | Unvollständiger 'Kontakt'.
data AnfrageKontakt
    = AnfrageKontakt
    | AKontaktName { akoName :: Text }
    | AKontaktNameFließend
          { akoName :: Text
          , akoFließend :: Value
          , akoKontaktAnfrageAnschluss :: AnfrageAnschluss 'InterruptPinBenötigt
          }
    deriving (Eq, Show)

instance Anzeige AnfrageKontakt where
    anzeige :: AnfrageKontakt -> Sprache -> Text
    anzeige AnfrageKontakt = Language.kontakt
    anzeige (AKontaktName name) = Language.kontakt <^> Language.name <=> name
    anzeige (AKontaktNameFließend name fließend anschluss) =
        Language.kontakt
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend <^> Language.kontakt <-> Language.anschluss <=> anschluss

instance Anfrage AnfrageKontakt where
    zeigeAnfrage :: AnfrageKontakt -> Sprache -> Text
    zeigeAnfrage AnfrageKontakt = Language.name
    zeigeAnfrage AKontaktName {} = Language.fließendValue
    zeigeAnfrage AKontaktNameFließend {akoKontaktAnfrageAnschluss} =
        zeigeAnfrage akoKontaktAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageKontakt -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AKontaktName {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen AKontaktNameFließend {akoKontaktAnfrageAnschluss} eingabe =
        zeigeAnfrageFehlgeschlagen akoKontaktAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageKontakt -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AKontaktName {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen AKontaktNameFließend {akoKontaktAnfrageAnschluss} =
        zeigeAnfrageOptionen akoKontaktAnfrageAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage Kontakt where
    type AnfrageTyp Kontakt = AnfrageKontakt

    -- | Eingabe einer Kupplung
    anfrageAktualisieren
        :: AnfrageKontakt -> EingabeToken -> AnfrageFortsetzung AnfrageKontakt Kontakt
    anfrageAktualisieren AnfrageKontakt EingabeToken {eingabe} =
        AFZwischenwert $ AKontaktName eingabe
    anfrageAktualisieren (AKontaktName name) token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, AKontaktNameFließend name HIGH AnfrageAnschluss)
            , (Lexer.LOW, AKontaktNameFließend name LOW AnfrageAnschluss)]
    anfrageAktualisieren anfrage@(AKontaktNameFließend koName koFließend anfrageAnschluss) token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren anfrageAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss 'InterruptPinBenötigt -> AnfrageKontakt
            anfrageAnschlussVerwenden
                akoKontaktAnfrageAnschluss = anfrage { akoKontaktAnfrageAnschluss }

            anschlussVerwenden
                :: Anschluss 'MitInterruptPin -> AnfrageFortsetzung AnfrageKontakt Kontakt
            anschlussVerwenden
                kontaktAnschluss = AFErgebnis Kontakt { koName, koFließend, kontaktAnschluss }
