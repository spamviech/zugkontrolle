{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Description : Klasse und Typfamilie für unvollständige Objekte.
-}
module Zug.UI.Cmd.Parser.Anfrage (
    -- * Unvollständige Befehle/Objekte
    Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, AnfrageFamilie,
    showMitAnfrage, showMitAnfrageFehlgeschlagen,
    -- * Suche ein existierendes Objekt im Status
    StatusAnfrageObjekt(..), statusAnfrageObjekt,
    -- * Hilfsfunktionen
    wähleBefehl, wähleRichtung, wähleValue, unbekanntShowText) where

import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Semigroup (Semigroup())
import Data.String (IsString())
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (StreckenObjekt(..), Value(..))
import Zug.Klassen (Zugtyp(..), ZugtypEither(..), Richtung(..))
import Zug.Language ((<=>), (<^>), showText, fehlerText)
import qualified Zug.Language as Language
import Zug.Plan (ObjektAllgemein(..), Objekt)
import Zug.UI.Base (MStatus, getPläne, getWegstrecken, getWeichen, getBahngeschwindigkeiten,
                    getStreckenabschnitte, getKupplungen)
import Zug.UI.Cmd.Lexer (EingabeToken(..), Token())
import qualified Zug.UI.Cmd.Lexer as Lexer

-- | Unvollständige Befehle/Objekte stellen Funktionen bereit dem Nutzer angzuzeigen, was als nächstes zum vervollständigen benötigt wird.
class Anfrage a where
    zeigeAnfrage :: (IsString s, Semigroup s) => a -> s
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => a -> s -> s
    zeigeAnfrageFehlgeschlagen = zeigeAnfrageFehlgeschlagenStandard
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => a -> Maybe s
    zeigeAnfrageOptionen _anfrage = Nothing
    {-# MINIMAL zeigeAnfrage #-}

-- | Standard-Implementierung zum Anzeigen einer fehlgeschlagenen 'Anfrage'
zeigeAnfrageFehlgeschlagenStandard :: (Anfrage a, IsString s, Semigroup s) => a -> s -> s
zeigeAnfrageFehlgeschlagenStandard a eingabe = Language.unbekannt (zeigeAnfrage a) <=> eingabe

instance (Anfrage (a 'Märklin), Anfrage (a 'Lego)) => Anfrage (ZugtypEither a) where
    zeigeAnfrage :: (IsString s, Semigroup s) => ZugtypEither a -> s
    zeigeAnfrage    (ZugtypMärklin a)   = zeigeAnfrage a
    zeigeAnfrage    (ZugtypLego a)      = zeigeAnfrage a
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => ZugtypEither a -> s -> s
    zeigeAnfrageFehlgeschlagen  (ZugtypMärklin a)   = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageFehlgeschlagen  (ZugtypLego a)      = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => ZugtypEither a -> Maybe s
    zeigeAnfrageOptionen    (ZugtypMärklin a)   = zeigeAnfrageOptionen a
    zeigeAnfrageOptionen    (ZugtypLego a)      = zeigeAnfrageOptionen a

-- | Typfamilie für den assoziierten 'Anfrage'typ
type family AnfrageFamilie a :: Type

-- | Zeige ein unvollständiges Objekt, gefolgt von der nächsten Nachfrage an
showMitAnfrage :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s
showMitAnfrage a = showText a <^> zeigeAnfrage a

-- | Zeige Meldung für eine invalide Eingabe auf die Nachfrage einer 'Anfrage' an
showMitAnfrageFehlgeschlagen :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s -> s
showMitAnfrageFehlgeschlagen a eingabe = showText a <^> zeigeAnfrageFehlgeschlagen a eingabe

-- | Ein Objekt aus dem aktuellen Status wird benötigt
data StatusAnfrageObjekt
    = SAOUnbekannt
        Text
    | SAOPlan
        EingabeToken
    | SAOWegstrecke
        EingabeToken
    | SAOWeiche
        EingabeToken
    | SAOBahngeschwindigkeit
        EingabeToken
    | SAOStreckenabschnitt
        EingabeToken
    | SAOKupplung
        EingabeToken

instance Show StatusAnfrageObjekt where
    show :: StatusAnfrageObjekt -> String
    show    anfrage@(SAOUnbekannt eingabe)    = unpack $ zeigeAnfrageFehlgeschlagen anfrage eingabe
    show    (SAOPlan _token)                  = Language.plan
    show    (SAOWegstrecke _token)            = Language.wegstrecke
    show    (SAOWeiche _token)                = Language.weiche
    show    (SAOBahngeschwindigkeit _token)   = Language.bahngeschwindigkeit
    show    (SAOStreckenabschnitt _token)     = Language.streckenabschnitt
    show    (SAOKupplung _token)              = Language.kupplung
instance Anfrage StatusAnfrageObjekt where
    zeigeAnfrage :: (IsString s, Semigroup s) => StatusAnfrageObjekt -> s
    zeigeAnfrage    (SAOUnbekannt _eingabe)           = Language.objekt
    zeigeAnfrage    (SAOPlan _token)                  = Language.indexOderName Language.plan
    zeigeAnfrage    (SAOWegstrecke _token)            = Language.indexOderName Language.wegstrecke
    zeigeAnfrage    (SAOWeiche _token)                = Language.indexOderName Language.weiche
    zeigeAnfrage    (SAOBahngeschwindigkeit _token)   = Language.indexOderName Language.bahngeschwindigkeit
    zeigeAnfrage    (SAOStreckenabschnitt _token)     = Language.indexOderName Language.streckenabschnitt
    zeigeAnfrage    (SAOKupplung _token)              = Language.indexOderName Language.kupplung

-- | Erhalte ein im Status existierendes Objekt
statusAnfrageObjekt :: StatusAnfrageObjekt -> MStatus (Either StatusAnfrageObjekt Objekt)
statusAnfrageObjekt
    anfrage@(SAOUnbekannt _eingabe0)
        = pure $ Left anfrage
statusAnfrageObjekt
    anfrage@(SAOPlan eingabe)
        = statusAnfrageObjektAux anfrage eingabe getPläne OPlan
statusAnfrageObjekt
    anfrage@(SAOWegstrecke eingabe)
        = statusAnfrageObjektAux anfrage eingabe getWegstrecken OWegstrecke
statusAnfrageObjekt
    anfrage@(SAOWeiche eingabe)
        = statusAnfrageObjektAux anfrage eingabe getWeichen OWeiche
statusAnfrageObjekt
    anfrage@(SAOBahngeschwindigkeit eingabe)
        = statusAnfrageObjektAux anfrage eingabe getBahngeschwindigkeiten OBahngeschwindigkeit
statusAnfrageObjekt
    anfrage@(SAOStreckenabschnitt eingabe)
        = statusAnfrageObjektAux anfrage eingabe getStreckenabschnitte OStreckenabschnitt
statusAnfrageObjekt
    anfrage@(SAOKupplung eingabe)
        = statusAnfrageObjektAux anfrage eingabe getKupplungen OKupplung

-- | Ein Objekt mit bestimmten Zugtyp
data ObjektZugtyp (z :: Zugtyp)
    = OZWegstrecke
        (Wegstrecke z)
    | OZWeiche
        (Weiche z)
    | OZBahngeschwindigkeit
        (Bahngeschwindigkeit z)

-- | Ein Objekt mit bestimmten Zugtyp aus dem aktullen Status wird benötigt
data StatusAnfrageObjektZugtyp (z :: Zugtyp)
    = SAOZUnbekannt
        Text
    | SAOZWegstrecke
        EingabeToken
    | SAOZWeiche
        EingabeToken
    | SAOZBahngeschwindigkeit
        EingabeToken

-- Hilfsfunktion
-- | Finde ein Objekt anhand seines Namens/Indizes
statusAnfrageObjektAux :: (StreckenObjekt a)
    => StatusAnfrageObjekt
    -> EingabeToken
    -> MStatus [a]
    -> (a -> Objekt)
        -> MStatus (Either StatusAnfrageObjekt Objekt)
statusAnfrageObjektAux anfrage eingabe getFromStatus konstruktor = do
    objekte <- getFromStatus
    pure $ case findByNameOrIndex objekte eingabe of
        Nothing         -> Left anfrage
        (Just objekt)   -> Right $ konstruktor objekt

-- | Element einer Liste anhand des Index oder Namens finden
findByNameOrIndex :: (StreckenObjekt a) => [a] -> EingabeToken -> Maybe a
findByNameOrIndex liste EingabeToken {eingabe, ganzzahl} = case ganzzahl of
    (Just index)
        | index >= 0, längerAls liste index
            -> Just $ liste !! fromIntegral index
    _maybeIndex
        -> listToMaybe $ filter ((== eingabe) . erhalteName) liste

-- | Prüft, ob eine Liste mindestens von der Länge i ist, ohne die komplette Länge zu berechnen
längerAls :: [a] -> Natural -> Bool
längerAls   []      i   = i < 0
längerAls   _liste  0   = True
längerAls   (_h:t)  i   = längerAls t $ pred i

-- | Wähle aus möglichen Interpretationen der Eingabe die erste passende aus und gebe den daraus resultierenden Befehl zurück.
-- Falls keine Möglichkeit passend ist, gebe wird das Ersatz-Ergebnis zurückgegeben.
wähleBefehl :: EingabeToken -> [(Token, a)] -> a -> a
wähleBefehl
    _eingabe
    []
    ersatz
        = ersatz
wähleBefehl
    eingabe@EingabeToken {möglichkeiten}
    ((befehl, ergebnis) : t)
    ersatz
        | elem befehl möglichkeiten
            = ergebnis
        | otherwise
            = wähleBefehl eingabe t ersatz

-- | Gebe (falls möglich) die zur Eingabe passende 'Richtung' zurück.
wähleRichtung :: EingabeToken -> Maybe Richtung
wähleRichtung token = wähleBefehl token [
    (Lexer.Gerade   , Just Gerade),
    (Lexer.Kurve    , Just Kurve),
    (Lexer.Links    , Just Links),
    (Lexer.Rechts   , Just Rechts)]
    Nothing

-- | Gebe (falls möglich) den zur Eingabe passenden 'Value' zurück.
wähleValue :: EingabeToken -> Maybe Value
wähleValue token = wähleBefehl token [
    (Lexer.HIGH , Just HIGH),
    (Lexer.LOW  , Just LOW)]
    Nothing

-- | Fehlerhafte Eingabe anzeigen
unbekanntShowText :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s -> s
unbekanntShowText a eingabe = fehlerText $ showMitAnfrageFehlgeschlagen a eingabe