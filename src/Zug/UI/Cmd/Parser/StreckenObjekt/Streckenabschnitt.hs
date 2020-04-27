{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Zug.UI.Cmd.Parser.StreckenObjekt.Streckenabschnitt (AnfrageStreckenabschnitt(..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import Zug.Anbindung (Value(..), alleValues, Anschluss(), Streckenabschnitt(..))
import Zug.Language (Anzeige(..), Sprache(), (<^>), (<=>), (<->), toBefehlsString)
import qualified Zug.Language as Language
import Zug.UI.Cmd.Lexer (EingabeToken(..))
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), MitAnfrage(..), zeigeAnfrageFehlgeschlagenStandard
                                , AnfrageFortsetzung(..), wähleZwischenwert, ($<<))
import Zug.UI.Cmd.Parser.Anschluss
       (AnfrageAnschluss(AnfrageAnschluss), MitInterruptPin(InterruptPinEgal))

-- | Unvollständiger 'Streckenabschnitt'
data AnfrageStreckenabschnitt
    = AnfrageStreckenabschnitt
    | AStreckenabschnittName { astName :: Text }
    | AStreckenabschnittNameFließend
          { astName :: Text
          , astFließend :: Value
          , astStromAnfrageAnschluss :: AnfrageAnschluss
          }
    deriving (Eq, Show)

instance Anzeige AnfrageStreckenabschnitt where
    anzeige :: AnfrageStreckenabschnitt -> Sprache -> Text
    anzeige AnfrageStreckenabschnitt = Language.streckenabschnitt
    anzeige (AStreckenabschnittName name) = Language.streckenabschnitt <^> Language.name <=> name
    anzeige (AStreckenabschnittNameFließend name fließend stromAnschluss) =
        Language.streckenabschnitt
        <^> Language.name
        <=> name
        <^> Language.fließendValue
        <=> fließend <^> Language.strom <-> Language.anschluss <=> stromAnschluss

instance Anfrage AnfrageStreckenabschnitt where
    zeigeAnfrage :: AnfrageStreckenabschnitt -> Sprache -> Text
    zeigeAnfrage AnfrageStreckenabschnitt = Language.name
    zeigeAnfrage AStreckenabschnittName {} = Language.fließendValue
    zeigeAnfrage AStreckenabschnittNameFließend {astStromAnfrageAnschluss} =
        zeigeAnfrage astStromAnfrageAnschluss

    zeigeAnfrageFehlgeschlagen :: AnfrageStreckenabschnitt -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AStreckenabschnittName {} eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.valueErwartet
    zeigeAnfrageFehlgeschlagen AStreckenabschnittNameFließend {astStromAnfrageAnschluss} eingabe =
        zeigeAnfrageFehlgeschlagen astStromAnfrageAnschluss eingabe
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageStreckenabschnitt -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AStreckenabschnittName {} = Just $ toBefehlsString . \sprache
        -> map (`anzeige` sprache) $ NonEmpty.toList alleValues
    zeigeAnfrageOptionen AStreckenabschnittNameFließend {astStromAnfrageAnschluss} =
        zeigeAnfrageOptionen astStromAnfrageAnschluss
    zeigeAnfrageOptionen _anfrage = Nothing

instance MitAnfrage Streckenabschnitt where
    type AnfrageTyp Streckenabschnitt = AnfrageStreckenabschnitt

    -- | Eingabe eines Streckenabschnitts
    anfrageAktualisieren :: AnfrageStreckenabschnitt
                         -> EingabeToken
                         -> AnfrageFortsetzung AnfrageStreckenabschnitt Streckenabschnitt
    anfrageAktualisieren AnfrageStreckenabschnitt EingabeToken {eingabe} =
        AFZwischenwert $ AStreckenabschnittName eingabe
    anfrageAktualisieren (AStreckenabschnittName name) token =
        wähleZwischenwert
            token
            [ ( Lexer.HIGH
                  , AStreckenabschnittNameFließend name HIGH $ AnfrageAnschluss InterruptPinEgal
                  )
            , ( Lexer.LOW
                  , AStreckenabschnittNameFließend name LOW $ AnfrageAnschluss InterruptPinEgal
                  )]
    anfrageAktualisieren
        anfrage@(AStreckenabschnittNameFließend stName stFließend stromAnschluss)
        token =
        (anschlussVerwenden, anfrageAnschlussVerwenden)
        $<< anfrageAktualisieren stromAnschluss token
        where
            anfrageAnschlussVerwenden :: AnfrageAnschluss -> AnfrageStreckenabschnitt
            anfrageAnschlussVerwenden
                astStromAnfrageAnschluss = anfrage { astStromAnfrageAnschluss }

            anschlussVerwenden
                :: Anschluss -> AnfrageFortsetzung AnfrageStreckenabschnitt Streckenabschnitt
            anschlussVerwenden stromAnschluss =
                AFErgebnis Streckenabschnitt { stName, stFließend, stromAnschluss }
