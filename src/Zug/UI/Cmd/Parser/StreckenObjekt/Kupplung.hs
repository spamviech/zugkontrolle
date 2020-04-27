{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.StreckenObjekt.Kupplung (AnfrageKupplung(..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import Zug.Anbindung (Value(..), alleValues, Anschluss(), Kupplung(..))
import Zug.Language (Anzeige(..), Sprache(), (<^>), (<=>), (<->), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), MitAnfrage(..), zeigeAnfrageFehlgeschlagenStandard
                                , AnfrageFortsetzung(..), wähleZwischenwert, ($<<))
import Zug.UI.Cmd.Parser.Anschluss
       (AnfrageAnschluss(AnfrageAnschluss), MitInterruptPin(InterruptPinEgal))

-- | Unvollständige 'Kupplung'.
data AnfrageKupplung
    = AnfrageKupplung
    | AKupplungName { akuName :: Text }
    | AKupplungNameFließend
          { akuName :: Text
          , akuFließend :: Value
          , akuKupplungsAnfrageAnschluss :: AnfrageAnschluss
          }
    deriving (Eq, Show)

instance Anzeige AnfrageKupplung where
    anzeige :: AnfrageKupplung -> Sprache -> Text
    anzeige AnfrageKupplung = Language.kupplung
    anzeige (AKupplungName name) = Language.kupplung <^> Language.name <=> name
    anzeige (AKupplungNameFließend name fließend anschluss) =
        Language.kupplung
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend <^> Language.kupplung <-> Language.anschluss <=> anschluss

instance Anfrage AnfrageKupplung where
    zeigeAnfrage :: AnfrageKupplung -> Sprache -> Text
    zeigeAnfrage AnfrageKupplung = Language.name
    zeigeAnfrage AKupplungName {} = Language.fließendValue
    zeigeAnfrage AKupplungNameFließend {akuKupplungsAnfrageAnschluss} =
        zeigeAnfrage akuKupplungsAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageKupplung -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AKupplungName {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen AKupplungNameFließend {akuKupplungsAnfrageAnschluss} eingabe =
        zeigeAnfrageFehlgeschlagen akuKupplungsAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageKupplung -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AKupplungName {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen AKupplungNameFließend {akuKupplungsAnfrageAnschluss} =
        zeigeAnfrageOptionen akuKupplungsAnfrageAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage Kupplung where
    type AnfrageTyp Kupplung = AnfrageKupplung

    -- | Eingabe einer Kupplung
    anfrageAktualisieren
        :: AnfrageKupplung -> EingabeToken -> AnfrageFortsetzung AnfrageKupplung Kupplung
    anfrageAktualisieren AnfrageKupplung EingabeToken {eingabe} =
        AFZwischenwert $ AKupplungName eingabe
    anfrageAktualisieren (AKupplungName name) token =
        wähleZwischenwert
            token
            [ (Lexer.HIGH, AKupplungNameFließend name HIGH $ AnfrageAnschluss InterruptPinEgal)
            , (Lexer.LOW, AKupplungNameFließend name LOW $ AnfrageAnschluss InterruptPinEgal)]
    anfrageAktualisieren anfrage@(AKupplungNameFließend kuName kuFließend anfrageAnschluss) token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren anfrageAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss -> AnfrageKupplung
            anfrageAnschlussVerwenden
                akuKupplungsAnfrageAnschluss = anfrage { akuKupplungsAnfrageAnschluss }

            anschlussVerwenden :: Anschluss -> AnfrageFortsetzung AnfrageKupplung Kupplung
            anschlussVerwenden kupplungsAnschluss =
                AFErgebnis Kupplung { kuName, kuFließend, kupplungsAnschluss }
