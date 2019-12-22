{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Description : Klasse und Typfamilie für unvollständige Objekte.
-}
module Zug.UI.Cmd.Parser.Anfrage (
    -- * Unvollständige Befehle/Objekte
    Anfrage(..), zeigeAnfrageFehlgeschlagenStandard,
    anzeigeMitAnfrage, anzeigeMitAnfrageFehlgeschlagen,
    AnfrageFortsetzung(..), verwendeAnfrageFortsetzung, ($<<), (.<<),
    MitAnfrage(..), AnfrageZugtyp(..), AnfrageZugtypEither(..),
    MitAnfrageZugtyp(..), anfrageAktualisierenZugtyp,
    -- * Suche ein existierendes Objekt im Status
    StatusAnfrageObjekt(..), statusAnfrageObjekt,
    ObjektZugtyp(..), StatusAnfrageObjektZugtyp(..), statusAnfrageObjektZugtyp, zuObjekt,
    -- * Hilfsfunktionen
    wähleBefehl, wähleRichtung, wähleValue, unbekanntShowText,
    wähleZwischenwert, wähleErgebnis) where

import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (
    StreckenObjekt(..), Value(..), Bahngeschwindigkeit(), Streckenabschnitt(),
    Weiche(), Kupplung(), Wegstrecke())
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), Richtung(..))
import Zug.Language (Anzeige(..), ($#), Sprache(), (<=>), (<^>), fehlerText)
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (Plan())
import Zug.UI.Base (
    MStatusT, getPläne, getWegstrecken, getWeichen, getBahngeschwindigkeiten,
    getStreckenabschnitte, getKupplungen)
import Zug.UI.Cmd.Lexer (EingabeToken(..), Token())
import qualified Zug.UI.Cmd.Lexer as Lexer

-- | Unvollständige Befehle/Objekte stellen Funktionen bereit dem Nutzer angzuzeigen, was als nächstes zum vervollständigen benötigt wird.
class Anfrage a where
    zeigeAnfrage :: a -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen :: a -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen = zeigeAnfrageFehlgeschlagenStandard
    zeigeAnfrageOptionen :: a -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen _anfrage = Nothing
    {-# MINIMAL zeigeAnfrage #-}

-- | Standard-Implementierung zum Anzeigen einer fehlgeschlagenen 'Anfrage'
zeigeAnfrageFehlgeschlagenStandard :: (Anfrage a) => a -> Text -> Sprache -> Text
zeigeAnfrageFehlgeschlagenStandard a eingabe
    = Language.unbekannt $# zeigeAnfrage a <=> eingabe

instance (Anfrage (a 'Märklin), Anfrage (a 'Lego)) => Anfrage (ZugtypEither a) where
    zeigeAnfrage :: ZugtypEither a -> Sprache -> Text
    zeigeAnfrage    (ZugtypMärklin a)   = zeigeAnfrage a
    zeigeAnfrage    (ZugtypLego a)      = zeigeAnfrage a
    zeigeAnfrageFehlgeschlagen :: ZugtypEither a -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen  (ZugtypMärklin a)   = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageFehlgeschlagen  (ZugtypLego a)      = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageOptionen :: ZugtypEither a -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen    (ZugtypMärklin a)   = zeigeAnfrageOptionen a
    zeigeAnfrageOptionen    (ZugtypLego a)      = zeigeAnfrageOptionen a

-- | Zeige ein unvollständiges Objekt, gefolgt von der nächsten Nachfrage an
anzeigeMitAnfrage :: (Anzeige a, Anfrage a) => a -> Sprache -> Text
anzeigeMitAnfrage a = a <^> zeigeAnfrage a

-- | Zeige Meldung für eine invalide Eingabe auf die Nachfrage einer 'Anfrage' an
anzeigeMitAnfrageFehlgeschlagen :: (Anzeige a, Anfrage a) => a -> Text -> Sprache -> Text
anzeigeMitAnfrageFehlgeschlagen a eingabe = a <^> zeigeAnfrageFehlgeschlagen a eingabe

-- | Klasse für Typen mit assiziiertem 'Anfrage'-Type
class MitAnfrage a where
    -- | Typfamilie für den assoziierten 'Anfrage'typ
    type family AnfrageTyp a :: Type
    -- | Eingabe eines Typs mit 'AnfrageTyp'
    anfrageAktualisieren :: AnfrageTyp a -> EingabeToken -> AnfrageFortsetzung (AnfrageTyp a) a

-- | Enumeration-Typ für eventuell noch unbestimmten 'Zugtyp'
data AnfrageZugtyp
    = AnfrageZugtyp
    | AnfrageZugtypMärklin
    | AnfrageZugtypLego

-- | Analog zu 'ZugtypEither' für 'AnfrageZugtyp'
data AnfrageZugtypEither (a :: AnfrageZugtyp -> Type)
    = AnfrageNothing
        (a 'AnfrageZugtyp)
    | AnfrageMärklin
        (a 'AnfrageZugtypMärklin)
    | AnfrageLego
        (a 'AnfrageZugtypLego)
deriving instance (Eq (a 'AnfrageZugtyp), Eq (a 'AnfrageZugtypMärklin), Eq (a 'AnfrageZugtypLego))
    => Eq (AnfrageZugtypEither a)
instance (Show (a 'AnfrageZugtyp), Show (a 'AnfrageZugtypMärklin), Show (a 'AnfrageZugtypLego))
    => Show (AnfrageZugtypEither a) where
    show :: AnfrageZugtypEither a -> String
    show    (AnfrageNothing a)  = show a
    show    (AnfrageMärklin a)  = show a
    show    (AnfrageLego a)     = show a
instance (Anzeige (a 'AnfrageZugtyp), Anzeige (a 'AnfrageZugtypMärklin), Anzeige (a 'AnfrageZugtypLego))
    => Anzeige (AnfrageZugtypEither a) where
    anzeige :: AnfrageZugtypEither a -> Sprache -> Text
    anzeige (AnfrageNothing a)  = anzeige a
    anzeige (AnfrageMärklin a)  = anzeige a
    anzeige (AnfrageLego a)     = anzeige a
instance (Anfrage (a 'AnfrageZugtyp), Anfrage (a 'AnfrageZugtypMärklin), Anfrage (a 'AnfrageZugtypLego))
    => Anfrage (AnfrageZugtypEither a) where
    zeigeAnfrage :: AnfrageZugtypEither a -> Sprache -> Text
    zeigeAnfrage    (AnfrageNothing a)  = zeigeAnfrage a
    zeigeAnfrage    (AnfrageMärklin a)  = zeigeAnfrage a
    zeigeAnfrage    (AnfrageLego a)     = zeigeAnfrage a
    zeigeAnfrageFehlgeschlagen :: AnfrageZugtypEither a -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen  (AnfrageNothing a)  = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageFehlgeschlagen  (AnfrageMärklin a)  = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageFehlgeschlagen  (AnfrageLego a)     = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageOptionen :: AnfrageZugtypEither a -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen    (AnfrageNothing a)  = zeigeAnfrageOptionen a
    zeigeAnfrageOptionen    (AnfrageMärklin a)  = zeigeAnfrageOptionen a
    zeigeAnfrageOptionen    (AnfrageLego a)     = zeigeAnfrageOptionen a

-- | Klasse für 'AnfrageTyp'en mit 'AnfrageZugtyp'
class MitAnfrageZugtyp (a :: AnfrageZugtyp -> Type) where
    anfrageMärklin :: a 'AnfrageZugtypMärklin
    anfrageLego :: a 'AnfrageZugtypLego

anfrageAktualisierenZugtyp :: (MitAnfrageZugtyp a) =>
    EingabeToken ->
        AnfrageFortsetzung (AnfrageZugtypEither a) b
anfrageAktualisierenZugtyp
    token
        = wähleZwischenwert token [
            (Lexer.Märklin  , AnfrageMärklin anfrageMärklin),
            (Lexer.Lego     , AnfrageLego anfrageLego)]

-- | Ein Objekt aus dem aktuellen Status wird benötigt
data StatusAnfrageObjekt
    = SAOBahngeschwindigkeit
        EingabeToken
    | SAOStreckenabschnitt
        EingabeToken
    | SAOWeiche
        EingabeToken
    | SAOKupplung
        EingabeToken
    | SAOWegstrecke
        EingabeToken
    | SAOPlan
        EingabeToken
    deriving (Eq, Show)

instance Anzeige StatusAnfrageObjekt where
    anzeige :: StatusAnfrageObjekt -> Sprache -> Text
    anzeige (SAOBahngeschwindigkeit _token)   = Language.bahngeschwindigkeit
    anzeige (SAOStreckenabschnitt _token)     = Language.streckenabschnitt
    anzeige (SAOWeiche _token)                = Language.weiche
    anzeige (SAOKupplung _token)              = Language.kupplung
    anzeige (SAOWegstrecke _token)            = Language.wegstrecke
    anzeige (SAOPlan _token)                  = Language.plan
instance Anfrage StatusAnfrageObjekt where
    zeigeAnfrage :: StatusAnfrageObjekt -> Sprache -> Text
    zeigeAnfrage    (SAOBahngeschwindigkeit _token)   = Language.indexOderName $# Language.bahngeschwindigkeit
    zeigeAnfrage    (SAOStreckenabschnitt _token)     = Language.indexOderName $# Language.streckenabschnitt
    zeigeAnfrage    (SAOWeiche _token)                = Language.indexOderName $# Language.weiche
    zeigeAnfrage    (SAOKupplung _token)              = Language.indexOderName $# Language.kupplung
    zeigeAnfrage    (SAOWegstrecke _token)            = Language.indexOderName $# Language.wegstrecke
    zeigeAnfrage    (SAOPlan _token)                  = Language.indexOderName $# Language.plan

-- | Erhalte ein im Status existierendes Objekt
statusAnfrageObjekt :: (Monad m) => StatusAnfrageObjekt -> MStatusT m (Either Text Objekt)
statusAnfrageObjekt
    (SAOBahngeschwindigkeit eingabe)
        = statusAnfrageObjektAux eingabe getBahngeschwindigkeiten $ Just . OBahngeschwindigkeit
statusAnfrageObjekt
    (SAOStreckenabschnitt eingabe)
        = statusAnfrageObjektAux eingabe getStreckenabschnitte $ Just . OStreckenabschnitt
statusAnfrageObjekt
    (SAOWeiche eingabe)
        = statusAnfrageObjektAux eingabe getWeichen $ Just . OWeiche
statusAnfrageObjekt
    (SAOKupplung eingabe)
        = statusAnfrageObjektAux eingabe getKupplungen $ Just . OKupplung
statusAnfrageObjekt
    (SAOWegstrecke eingabe)
        = statusAnfrageObjektAux eingabe getWegstrecken $ Just . OWegstrecke
statusAnfrageObjekt
    (SAOPlan eingabe)
        = statusAnfrageObjektAux eingabe getPläne $ Just . OPlan

-- | Ein Objekt mit bestimmten Zugtyp
data ObjektZugtyp (z :: Zugtyp)
    = OZBahngeschwindigkeit
        (Bahngeschwindigkeit z)
    | OZStreckenabschnitt
        Streckenabschnitt
    | OZWeiche
        (Weiche z)
    | OZKupplung
        Kupplung
    | OZWegstrecke
        (Wegstrecke z)
    | OZPlan
        Plan
    deriving (Eq)

instance Show (ObjektZugtyp z) where
    show :: ObjektZugtyp z -> String
    show
        (OZBahngeschwindigkeit bahngeschwindigkeit)
            = show bahngeschwindigkeit
    show
        (OZStreckenabschnitt streckenabschnitt)
            = show streckenabschnitt
    show
        (OZWeiche weiche)
            = show weiche
    show
        (OZKupplung kupplung)
            = show kupplung
    show
        (OZWegstrecke wegstrecke)
            = show wegstrecke
    show
        (OZPlan plan)
            = show plan

zuObjekt :: (ZugtypKlasse z) => ObjektZugtyp z -> Objekt
zuObjekt    (OZBahngeschwindigkeit bg)  = OBahngeschwindigkeit $ zuZugtypEither bg
zuObjekt    (OZStreckenabschnitt st)    = OStreckenabschnitt st
zuObjekt    (OZWeiche we)               = OWeiche $ zuZugtypEither we
zuObjekt    (OZKupplung ku)             = OKupplung ku
zuObjekt    (OZWegstrecke ws)           = OWegstrecke $ zuZugtypEither ws
zuObjekt    (OZPlan pl)                 = OPlan pl

-- | Ein Objekt mit bestimmten Zugtyp aus dem aktullen Status wird benötigt
data StatusAnfrageObjektZugtyp (z :: Zugtyp)
    = SAOZBahngeschwindigkeit
        EingabeToken
    | SAOZStreckenabschnitt
        EingabeToken
    | SAOZWeiche
        EingabeToken
    | SAOZKupplung
        EingabeToken
    | SAOZWegstrecke
        EingabeToken
    | SAOZPlan
        EingabeToken
    deriving (Eq, Show)

instance Anzeige (StatusAnfrageObjektZugtyp z) where
    anzeige :: StatusAnfrageObjektZugtyp z -> Sprache -> Text
    anzeige (SAOZBahngeschwindigkeit _token)    = Language.bahngeschwindigkeit
    anzeige (SAOZStreckenabschnitt _token)      = Language.streckenabschnitt
    anzeige (SAOZWeiche _token)                 = Language.weiche
    anzeige (SAOZKupplung _token)               = Language.kupplung
    anzeige (SAOZWegstrecke _token)             = Language.wegstrecke
    anzeige (SAOZPlan _token)                   = Language.plan
instance Anfrage (StatusAnfrageObjektZugtyp z) where
    zeigeAnfrage :: StatusAnfrageObjektZugtyp z -> Sprache -> Text
    zeigeAnfrage    (SAOZBahngeschwindigkeit _token)    = Language.indexOderName $# Language.bahngeschwindigkeit
    zeigeAnfrage    (SAOZStreckenabschnitt _token)      = Language.indexOderName $# Language.streckenabschnitt 
    zeigeAnfrage    (SAOZWeiche _token)                 = Language.indexOderName $# Language.weiche
    zeigeAnfrage    (SAOZKupplung _token)               = Language.indexOderName $# Language.kupplung
    zeigeAnfrage    (SAOZWegstrecke _token)             = Language.indexOderName $# Language.wegstrecke
    zeigeAnfrage    (SAOZPlan _token)                   = Language.indexOderName $# Language.plan

-- | Erhalte ein im Status existierendes Objekt mit bestimmten Zugtyp
statusAnfrageObjektZugtyp :: (Monad m, ZugtypKlasse z) =>
    StatusAnfrageObjektZugtyp z ->
        MStatusT m (Either Text (ObjektZugtyp z))
statusAnfrageObjektZugtyp
    (SAOZBahngeschwindigkeit eingabe)
        = statusAnfrageObjektAux eingabe (fmap vonZugtypEither <$> getBahngeschwindigkeiten) $
            fmap OZBahngeschwindigkeit
statusAnfrageObjektZugtyp
    (SAOZStreckenabschnitt eingabe)
        = statusAnfrageObjektAux eingabe getStreckenabschnitte $
            Just . OZStreckenabschnitt
statusAnfrageObjektZugtyp
    (SAOZWeiche eingabe)
        = statusAnfrageObjektAux eingabe (fmap vonZugtypEither <$> getWeichen) $
            fmap OZWeiche
statusAnfrageObjektZugtyp
    (SAOZKupplung eingabe)
        = statusAnfrageObjektAux eingabe getKupplungen $
            Just . OZKupplung
statusAnfrageObjektZugtyp
    (SAOZWegstrecke eingabe)
        = statusAnfrageObjektAux eingabe (fmap vonZugtypEither <$> getWegstrecken) $
            fmap OZWegstrecke
statusAnfrageObjektZugtyp
    (SAOZPlan eingabe)
        = statusAnfrageObjektAux eingabe getPläne $
            Just . OZPlan

-- Hilfsfunktion
-- | Finde ein Objekt anhand seines Namens/Indizes
statusAnfrageObjektAux :: (Monad m, StreckenObjekt a) =>
    EingabeToken ->
    MStatusT m [a] ->
    (a -> Maybe o) ->
        MStatusT m (Either Text o)
statusAnfrageObjektAux
    token@EingabeToken {eingabe}
    getFromStatus
    konstruktor
        = do
            objekte <- getFromStatus
            pure $ case findByNameOrIndex objekte token >>= konstruktor of
                Nothing         -> Left eingabe
                (Just objekt)   -> Right objekt

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

-- | Wähle aus möglichen Interpretationen der Eingabe die erste passende und gebe den zugehörigen Befehl zurück.
-- Falls keine Möglichkeit passend ist, wird das Ersatz-Ergebnis zurückgegeben.
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
unbekanntShowText :: (Anzeige a, Anfrage a) => a -> Text -> Sprache -> Text
unbekanntShowText a eingabe = fehlerText $# anzeigeMitAnfrageFehlgeschlagen a eingabe

data AnfrageFortsetzung a e
    = AFErgebnis {
        ergebnis :: e}
    | AFZwischenwert {
        anfrage :: a}
    | AFFehler {
        unbekannteEingabe :: Text}
    | AFStatusAnfrage {
        anfrageObjekt :: StatusAnfrageObjekt,
        konstruktor :: (Objekt -> AnfrageFortsetzung a e)}
    | AFStatusAnfrageMärklin {
        anfrageObjektMärklin :: (StatusAnfrageObjektZugtyp 'Märklin),
        konstruktorMärklin :: (ObjektZugtyp 'Märklin -> AnfrageFortsetzung a e)}
    | AFStatusAnfrageLego {
        anfrageObjektLego :: (StatusAnfrageObjektZugtyp 'Lego),
        konstruktorLego :: (ObjektZugtyp 'Lego -> AnfrageFortsetzung a e)}

-- | Spezialisierung von 'wähleBefehl' auf 'AFZwischenwert'
wähleZwischenwert :: EingabeToken -> [(Token, a)]-> AnfrageFortsetzung a e
wähleZwischenwert
    token@EingabeToken {eingabe}
    liste
        = wähleBefehl token (map (\(t, a) -> (t, AFZwischenwert a)) liste) $ AFFehler eingabe

-- | Spezialisierung von 'wähleBefehl' auf 'AFErgebnis'
wähleErgebnis :: EingabeToken -> [(Token, e)]-> AnfrageFortsetzung a e
wähleErgebnis
    token@EingabeToken {eingabe}
    liste
        = wähleBefehl token (map (\(t, e) -> (t, AFErgebnis e)) liste) $ AFFehler eingabe

-- | Komposition zweier Funktionen, die ein 'AnfrageFortsetzung' zurückgeben.
verwendeAnfrageFortsetzung ::
    (e -> AnfrageFortsetzung b f) ->
    (a -> b) ->
    AnfrageFortsetzung a e ->
        AnfrageFortsetzung b f
verwendeAnfrageFortsetzung
    wertFunktion
    _anfrageFunktion
    AFErgebnis {ergebnis}
        = wertFunktion ergebnis
verwendeAnfrageFortsetzung
    _wertFunktion
    anfrageFunktion
    AFZwischenwert {anfrage}
        = AFZwischenwert $ anfrageFunktion anfrage
verwendeAnfrageFortsetzung
    wertFunktion
    anfrageFunktion
    AFStatusAnfrage {anfrageObjekt, konstruktor}
        = AFStatusAnfrage {
            anfrageObjekt,
            konstruktor = (wertFunktion, anfrageFunktion) .<< konstruktor}
verwendeAnfrageFortsetzung
    wertFunktion
    anfrageFunktion
    AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin}
        = AFStatusAnfrageMärklin {
            anfrageObjektMärklin,
            konstruktorMärklin = (wertFunktion, anfrageFunktion) .<< konstruktorMärklin}
verwendeAnfrageFortsetzung
    wertFunktion
    anfrageFunktion
    AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego}
        = AFStatusAnfrageLego {
            anfrageObjektLego,
            konstruktorLego = (wertFunktion, anfrageFunktion) .<< konstruktorLego}
verwendeAnfrageFortsetzung
    _wertFunktion
    _anfrageFunktion
    AFFehler {unbekannteEingabe}
        = AFFehler {unbekannteEingabe}

infixr 0 $<<
($<<) :: (e -> AnfrageFortsetzung b f, a -> b) -> AnfrageFortsetzung a e -> AnfrageFortsetzung b f
(wertFunktion, anfrageFunktion) $<< anfrageErgebnis
    = verwendeAnfrageFortsetzung wertFunktion anfrageFunktion anfrageErgebnis

infixr 9 .<<
(.<<) :: (e -> AnfrageFortsetzung b f, a -> b) -> (o -> AnfrageFortsetzung a e) -> (o -> AnfrageFortsetzung b f)
funktionen .<< konstruktor
    = \o -> funktionen $<< konstruktor o