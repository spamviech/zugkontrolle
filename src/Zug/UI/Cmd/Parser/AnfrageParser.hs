{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zug.UI.Cmd.Parser.AnfrageParser where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import Zug.Anbindung (Bahngeschwindigkeit, Kontakt, Kupplung, Streckenabschnitt, Wegstrecke, Weiche)
import Zug.Enums (GeschwindigkeitEither, Zugtyp (..))
import Zug.Language ((<~>), Sprache (Deutsch))
import Zug.Objekt (Objekt)
import Zug.Plan (Plan)
import Zug.UI.Cmd.Lexer (EingabeToken)

-- | Ein Objekt aus dem aktuellen Status wird benötigt
data StatusAnfrageObjekt
  = SAOBahngeschwindigkeit EingabeToken
  | SAOStreckenabschnitt EingabeToken
  | SAOWeiche EingabeToken
  | SAOKupplung EingabeToken
  | SAOKontakt EingabeToken
  | SAOWegstrecke EingabeToken
  | SAOPlan EingabeToken
  deriving (Eq, Show)

-- | Ein Objekt mit bestimmten Zugtyp
data ObjektZugtyp (z :: Zugtyp)
  = OZBahngeschwindigkeit (GeschwindigkeitEither Bahngeschwindigkeit z)
  | OZStreckenabschnitt Streckenabschnitt
  | OZWeiche (Weiche z)
  | OZKupplung Kupplung
  | OZKontakt Kontakt
  | OZWegstrecke (Wegstrecke z)
  | OZPlan Plan
  deriving (Eq)

-- | Ein Objekt mit bestimmten Zugtyp aus dem aktuellen Status wird benötigt
data StatusAnfrageObjektZugtyp (z :: Zugtyp)
  = SAOZBahngeschwindigkeit EingabeToken
  | SAOZStreckenabschnitt EingabeToken
  | SAOZWeiche EingabeToken
  | SAOZKupplung EingabeToken
  | SAOZKontakt EingabeToken
  | SAOZWegstrecke EingabeToken
  | SAOZPlan EingabeToken
  deriving (Eq, Show)

-- | Ergebnis-Typ eines 'AnfrageParser'.
data AnfrageFortsetzung a e
  = AFErgebnis {ergebnis :: e, fortsetzung :: Maybe a, eingabeRest :: [Text]}
  | AFZwischenwert {zwischenwert :: a}
  | AFFehler {alterZwischenwert :: Maybe a, unbekannteEingabe :: Text}
  | AFStatusAnfrage
      { anfrageObjekt :: StatusAnfrageObjekt,
        konstruktor :: Objekt -> AnfrageFortsetzung a e
      }
  | AFStatusAnfrageMärklin
      { anfrageObjektMärklin :: StatusAnfrageObjektZugtyp 'Märklin,
        konstruktorMärklin :: ObjektZugtyp 'Märklin -> AnfrageFortsetzung a e
      }
  | AFStatusAnfrageLego
      { anfrageObjektLego :: StatusAnfrageObjektZugtyp 'Lego,
        konstruktorLego :: ObjektZugtyp 'Lego -> AnfrageFortsetzung a e
      }

instance Functor (AnfrageFortsetzung a) where
  fmap :: (e -> f) -> AnfrageFortsetzung a e -> AnfrageFortsetzung a f
  fmap funktion AFErgebnis {ergebnis, fortsetzung, eingabeRest} =
    AFErgebnis {ergebnis = funktion ergebnis, fortsetzung, eingabeRest}
  fmap _funktion AFZwischenwert {zwischenwert} = AFZwischenwert {zwischenwert}
  fmap _funktion AFFehler {alterZwischenwert, unbekannteEingabe} =
    AFFehler {alterZwischenwert, unbekannteEingabe}
  fmap funktion AFStatusAnfrage {anfrageObjekt, konstruktor} =
    AFStatusAnfrage {anfrageObjekt, konstruktor = fmap funktion . konstruktor}
  fmap funktion AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin} =
    AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin = fmap funktion . konstruktorMärklin}
  fmap funktion AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego} =
    AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego = fmap funktion . konstruktorLego}

-- vgl. FFP Ub6b, ApplikativerParser.
newtype AnfrageParser a e = AnfrageParser {runParser :: Maybe a -> [Text] -> AnfrageFortsetzung a e}

instance Functor (AnfrageParser a) where
  fmap :: (e -> f) -> AnfrageParser a e -> AnfrageParser a f
  fmap f (AnfrageParser p) = AnfrageParser $ \ma s -> fmap f $ p ma s

instance Applicative (AnfrageParser a) where
  pure :: e -> AnfrageParser a e
  -- konsumiert keine Eingabe und liefer immer ein Ergebnis
  pure ergebnis = AnfrageParser $ \fortsetzung eingabe ->
    AFErgebnis {ergebnis, fortsetzung, eingabeRest = eingabe}

  (<*>) :: forall e f. AnfrageParser a (e -> f) -> AnfrageParser a e -> AnfrageParser a f
  -- parsed eine Funktion und aus dem Rest der Eingabe ein Argument für diese Funktion und liefert das Ergebnis
  {-
  * Identity:
    pure id <*> v = v
  * Composition:
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  * Homomorphism:
    pure f <*> pure x = pure (f x)
  * Interchange:
    u <*> pure y = pure ($ y) <*> u
  -}
  (AnfrageParser p1) <*> (AnfrageParser p2) = AnfrageParser $ \ma eingabe0 ->
    let funktionAnwenden :: AnfrageFortsetzung a (e -> f) -> AnfrageFortsetzung a f
        -- Zwischenwert (as argument to function) doesn't work with identity law from Applicative class!!!
        -- was passiert mit Zwischenwert bei pure???
        funktionAnwenden AFErgebnis {ergebnis = funktion, fortsetzung, eingabeRest} =
          funktion <$> p2 fortsetzung eingabeRest
        funktionAnwenden AFZwischenwert {zwischenwert} = _
        funktionAnwenden AFFehler {unbekannteEingabe, alterZwischenwert} = AFFehler {unbekannteEingabe}
        funktionAnwenden AFStatusAnfrage {anfrageObjekt, konstruktor} = _
        --AFStatusAnfrage {anfrageObjekt, konstruktor = fmap funktion . konstruktor}
        funktionAnwenden AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin} = _
        --AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin = fmap funktion . konstruktorMärklin}
        funktionAnwenden AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego} = _
     in --AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego = fmap funktion . konstruktorLego}
        funktionAnwenden $ p1 ma eingabe0
--[(r1 r2, rem2) | (r1, rem1) <- p1 eingabe0, (r2, rem2) <- p2 rem1]


-- Ebenfalls in Modul Control.Applicative definiert:
-- Typklasse Alternative ist eine Unterklasse für Applikative Funktoren
-- mit Monoid-Struktur, d.h.: es gibt eine binäre Verknüpfung mit neutralem Element!
instance Alternative (AnfrageParser a) where
  empty :: (AnfrageParser a) e
  -- neutrales Element, ein Parser der immer fehlschlägt
  empty = AnfrageParser $ \alterZwischenwert eingabe ->
    AFFehler {alterZwischenwert, unbekannteEingabe = foldl' (<~>) (const Text.empty) eingabe $ Deutsch}

  (<|>) :: forall a e. AnfrageParser a e -> AnfrageParser a e -> AnfrageParser a e
  -- verknüpft zwei Parser zu einem Parser, linker Parser wird bevorzugt
  -- bei unvollständiger Eingabe (AFZwischenwert) wird bereits eine Entscheidung getroffen,
  -- die Parser sollten sich also so schnell wie möglich unterscheiden!
  (AnfrageParser p1) <|> (AnfrageParser p2) = AnfrageParser $ \ma eingabe -> case p1 ma eingabe of
    afErgebnis@AFErgebnis {} -> afErgebnis
    AFZwischenwert {zwischenwert} -> AFZwischenwert {zwischenwert}
    AFFehler {unbekannteEingabe} -> AFFehler {unbekannteEingabe}
    AFStatusAnfrage {anfrageObjekt, konstruktor} -> _
    AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin} -> _
    AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego} -> _

-- where
--   pbranches s
--     | null r1 = r2
--     | null r2 = r1
--     | otherwise = r1 ++ r2
--     where
--       r1 = p1 s
--       r2 = p2 s
