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
    showMitAnfrage, showMitAnfrageFehlgeschlagen,
    AnfrageFortsetzung(..), verwendeAnfrageFortsetzung, ($<<),
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
import Data.Semigroup (Semigroup())
import Data.String (IsString())
import Data.Text (Text)
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (StreckenObjekt(..), Value(..), Bahngeschwindigkeit(), Streckenabschnitt(),
                    Weiche(), Kupplung(), Wegstrecke())
import Zug.Klassen (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), Richtung(..))
import Zug.Language ((<=>), (<^>), showText, fehlerText)
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (Plan())
import Zug.UI.Base (MStatusT, getPläne, getWegstrecken, getWeichen, getBahngeschwindigkeiten,
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

-- | Zeige ein unvollständiges Objekt, gefolgt von der nächsten Nachfrage an
showMitAnfrage :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s
showMitAnfrage a = showText a <^> zeigeAnfrage a

-- | Zeige Meldung für eine invalide Eingabe auf die Nachfrage einer 'Anfrage' an
showMitAnfrageFehlgeschlagen :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s -> s
showMitAnfrageFehlgeschlagen a eingabe = showText a <^> zeigeAnfrageFehlgeschlagen a eingabe

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
instance (Anfrage (a 'AnfrageZugtyp), Anfrage (a 'AnfrageZugtypMärklin), Anfrage (a 'AnfrageZugtypLego))
    => Anfrage (AnfrageZugtypEither a) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageZugtypEither a -> s
    zeigeAnfrage    (AnfrageNothing a)  = zeigeAnfrage a
    zeigeAnfrage    (AnfrageMärklin a)  = zeigeAnfrage a
    zeigeAnfrage    (AnfrageLego a)     = zeigeAnfrage a
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageZugtypEither a -> s -> s
    zeigeAnfrageFehlgeschlagen  (AnfrageNothing a)  = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageFehlgeschlagen  (AnfrageMärklin a)  = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageFehlgeschlagen  (AnfrageLego a)     = zeigeAnfrageFehlgeschlagen a
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageZugtypEither a -> Maybe s
    zeigeAnfrageOptionen    (AnfrageNothing a)  = zeigeAnfrageOptionen a
    zeigeAnfrageOptionen    (AnfrageMärklin a)  = zeigeAnfrageOptionen a
    zeigeAnfrageOptionen    (AnfrageLego a)     = zeigeAnfrageOptionen a

-- | Klasse für 'AnfrageTyp'en mit 'AnfrageZugtyp'
class MitAnfrageZugtyp (a :: AnfrageZugtyp -> Type) where
    anfrageMärklin :: a 'AnfrageZugtypMärklin
    anfrageLego :: a 'AnfrageZugtypLego

anfrageAktualisierenZugtyp :: (MitAnfrageZugtyp a) =>
    a 'AnfrageZugtyp ->
    EingabeToken ->
        AnfrageFortsetzung (AnfrageZugtypEither a) b
anfrageAktualisierenZugtyp
    anfrage
    token
        = wähleZwischenwert (AnfrageNothing anfrage) token [
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

instance Show StatusAnfrageObjekt where
    show :: StatusAnfrageObjekt -> String
    -- show    anfrage@(SAOUnbekannt eingabe)    = unpack $ zeigeAnfrageFehlgeschlagen anfrage eingabe
    show    (SAOBahngeschwindigkeit _token)   = Language.bahngeschwindigkeit
    show    (SAOStreckenabschnitt _token)     = Language.streckenabschnitt
    show    (SAOWeiche _token)                = Language.weiche
    show    (SAOKupplung _token)              = Language.kupplung
    show    (SAOWegstrecke _token)            = Language.wegstrecke
    show    (SAOPlan _token)                  = Language.plan
instance Anfrage StatusAnfrageObjekt where
    zeigeAnfrage :: (IsString s, Semigroup s) => StatusAnfrageObjekt -> s
    -- zeigeAnfrage    (SAOUnbekannt _eingabe)           = Language.objekt
    zeigeAnfrage    (SAOBahngeschwindigkeit _token)   = Language.indexOderName Language.bahngeschwindigkeit
    zeigeAnfrage    (SAOStreckenabschnitt _token)     = Language.indexOderName Language.streckenabschnitt
    zeigeAnfrage    (SAOWeiche _token)                = Language.indexOderName Language.weiche
    zeigeAnfrage    (SAOKupplung _token)              = Language.indexOderName Language.kupplung
    zeigeAnfrage    (SAOWegstrecke _token)            = Language.indexOderName Language.wegstrecke
    zeigeAnfrage    (SAOPlan _token)                  = Language.indexOderName Language.plan

-- | Erhalte ein im Status existierendes Objekt
statusAnfrageObjekt :: (Monad m) => StatusAnfrageObjekt -> MStatusT m (AnfrageFortsetzung StatusAnfrageObjekt Objekt)
-- statusAnfrageObjekt
--     anfrage@(SAOUnbekannt _eingabe)
--         = pure $ Left anfrage
statusAnfrageObjekt
    anfrage@(SAOBahngeschwindigkeit eingabe)
        = statusAnfrageObjektAux anfrage eingabe getBahngeschwindigkeiten $ Just . OBahngeschwindigkeit
statusAnfrageObjekt
    anfrage@(SAOStreckenabschnitt eingabe)
        = statusAnfrageObjektAux anfrage eingabe getStreckenabschnitte $ Just . OStreckenabschnitt
statusAnfrageObjekt
    anfrage@(SAOWeiche eingabe)
        = statusAnfrageObjektAux anfrage eingabe getWeichen $ Just . OWeiche
statusAnfrageObjekt
    anfrage@(SAOKupplung eingabe)
        = statusAnfrageObjektAux anfrage eingabe getKupplungen $ Just . OKupplung
statusAnfrageObjekt
    anfrage@(SAOWegstrecke eingabe)
        = statusAnfrageObjektAux anfrage eingabe getWegstrecken $ Just . OWegstrecke
statusAnfrageObjekt
    anfrage@(SAOPlan eingabe)
        = statusAnfrageObjektAux anfrage eingabe getPläne $ Just . OPlan

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

instance Show (StatusAnfrageObjektZugtyp z) where
    show :: StatusAnfrageObjektZugtyp z -> String
    -- show    anfrage@(SAOZUnbekannt eingabe)     = unpack $ zeigeAnfrageFehlgeschlagen anfrage eingabe
    show    (SAOZBahngeschwindigkeit _token)    = Language.bahngeschwindigkeit
    show    (SAOZStreckenabschnitt _token)      = Language.streckenabschnitt
    show    (SAOZWeiche _token)                 = Language.weiche
    show    (SAOZKupplung _token)               = Language.kupplung
    show    (SAOZWegstrecke _token)             = Language.wegstrecke
    show    (SAOZPlan _token)                   = Language.plan
instance Anfrage (StatusAnfrageObjektZugtyp z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => StatusAnfrageObjektZugtyp z -> s
    -- zeigeAnfrage    (SAOZUnbekannt _eingabe)            = Language.objekt
    zeigeAnfrage    (SAOZBahngeschwindigkeit _token)    = Language.indexOderName Language.bahngeschwindigkeit
    zeigeAnfrage    (SAOZStreckenabschnitt _token)      = Language.indexOderName Language.streckenabschnitt 
    zeigeAnfrage    (SAOZWeiche _token)                 = Language.indexOderName Language.weiche
    zeigeAnfrage    (SAOZKupplung _token)               = Language.indexOderName Language.kupplung
    zeigeAnfrage    (SAOZWegstrecke _token)             = Language.indexOderName Language.wegstrecke
    zeigeAnfrage    (SAOZPlan _token)                   = Language.indexOderName Language.plan

-- | Erhalte ein im Status existierendes Objekt mit bestimmten Zugtyp
statusAnfrageObjektZugtyp :: (Monad m, ZugtypKlasse z) =>
    StatusAnfrageObjektZugtyp z ->
        MStatusT m (AnfrageFortsetzung (StatusAnfrageObjektZugtyp z) (ObjektZugtyp z))
-- statusAnfrageObjektZugtyp
--         anfrage@(SAOZUnbekannt _eingabe)
--             = pure $ Left anfrage
statusAnfrageObjektZugtyp
    anfrage@(SAOZBahngeschwindigkeit eingabe)
        = statusAnfrageObjektAux anfrage eingabe (fmap vonZugtypEither <$> getBahngeschwindigkeiten) $
            fmap OZBahngeschwindigkeit
statusAnfrageObjektZugtyp
    anfrage@(SAOZStreckenabschnitt eingabe)
        = statusAnfrageObjektAux anfrage eingabe getStreckenabschnitte $
            Just . OZStreckenabschnitt
statusAnfrageObjektZugtyp
    anfrage@(SAOZWeiche eingabe)
        = statusAnfrageObjektAux anfrage eingabe (fmap vonZugtypEither <$> getWeichen) $
            fmap OZWeiche
statusAnfrageObjektZugtyp
    anfrage@(SAOZKupplung eingabe)
        = statusAnfrageObjektAux anfrage eingabe getKupplungen $
            Just . OZKupplung
statusAnfrageObjektZugtyp
    anfrage@(SAOZWegstrecke eingabe)
        = statusAnfrageObjektAux anfrage eingabe (fmap vonZugtypEither <$> getWegstrecken) $
            fmap OZWegstrecke
statusAnfrageObjektZugtyp
    anfrage@(SAOZPlan eingabe)
        = statusAnfrageObjektAux anfrage eingabe getPläne $
            Just . OZPlan

-- Hilfsfunktion
-- | Finde ein Objekt anhand seines Namens/Indizes
statusAnfrageObjektAux :: (Monad m, StreckenObjekt a)
    => statusAnfrageObjekt
    -> EingabeToken
    -> MStatusT m [a]
    -> (a -> Maybe o)
        -> MStatusT m (AnfrageFortsetzung statusAnfrageObjekt o)
statusAnfrageObjektAux
    anfrage
    token@EingabeToken {eingabe}
    getFromStatus
    konstruktor
        = do
            objekte <- getFromStatus
            pure $ case findByNameOrIndex objekte token >>= konstruktor of
                Nothing         -> AFFehler anfrage eingabe
                (Just objekt)   -> AFErgebnis objekt

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
unbekanntShowText :: (Show a, Anfrage a, IsString s, Semigroup s) => a -> s -> s
unbekanntShowText a eingabe = fehlerText $ showMitAnfrageFehlgeschlagen a eingabe

data AnfrageFortsetzung a e
    = AFErgebnis {
        ergebnis :: e}
    | AFZwischenwert {
        anfrage :: a}
    | AFFehler {
        anfrage :: a,
        fehlerhafteEingabe :: Text}
    deriving (Show, Eq)

-- | Spezialisierung von 'wähleBefehl' auf 'AFZwischenwert'
wähleZwischenwert :: a -> EingabeToken -> [(Token, a)]-> AnfrageFortsetzung a e
wähleZwischenwert
    anfrage
    token@EingabeToken {eingabe}
    liste
        = wähleBefehl token (map (\(t, a) -> (t, AFZwischenwert a)) liste) $ AFFehler anfrage eingabe

-- | Spezialisierung von 'wähleBefehl' auf 'AFErgebnis'
wähleErgebnis :: a -> EingabeToken -> [(Token, e)]-> AnfrageFortsetzung a e
wähleErgebnis
    anfrage
    token@EingabeToken {eingabe}
    liste
        = wähleBefehl token (map (\(t, e) -> (t, AFErgebnis e)) liste) $ AFFehler anfrage eingabe

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
    _wertFunktion
    anfrageFunktion
    AFFehler {anfrage, fehlerhafteEingabe}
        = AFFehler {anfrage = anfrageFunktion anfrage, fehlerhafteEingabe}

infixr 0 $<<
($<<) :: (e -> AnfrageFortsetzung b f, a -> b) -> AnfrageFortsetzung a e -> AnfrageFortsetzung b f
(wertFunktion, anfrageFunktion) $<< anfrageErgebnis
    = verwendeAnfrageFortsetzung wertFunktion anfrageFunktion anfrageErgebnis