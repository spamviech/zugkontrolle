{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-}

module Zug.UI.Cmd.Parser.AnfrageParser where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), ap)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import Zug.Anbindung (Bahngeschwindigkeit, Kontakt, Kupplung, Streckenabschnitt, Wegstrecke, Weiche)
import Zug.Enums (GeschwindigkeitEither, Zugtyp (..))
import Zug.Language ((<~>), Sprache (Deutsch))
import Zug.Objekt (Objekt)
import Zug.Plan (Plan)
import Zug.UI.Cmd.Lexer (EingabeToken)
import Zug.Warteschlange (Warteschlange ())
import qualified Zug.Warteschlange as Warteschlange

{-
import Data.Kind (Type)

-- | Ability to convert from one type 'into' another (injective).
--
-- The default implementation uses 'id' for the case that a ~ b.
class Into a b where
  into :: a -> b
  default into :: (a ~ b) => a -> b
  into = id

instance (Into a b) => Into (Maybe a) (Maybe b) where
  into :: Maybe a -> Maybe b
  into Nothing = Nothing
  into (Just a) = Just $ into a

-- | Typfamilie für den assoziierten 'Anfrage'typ
type family AnfrageTyp e :: Type

-}
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
  | AFZwischenwert
      { zwischenwert :: a,
        verarbeiteteEingabe :: Warteschlange Text,
        alternativeParser :: Warteschlange (AnfrageParser a e)
      }
  | AFFehler
      { alterZwischenwert :: Maybe a,
        unbekannteEingabe :: Text,
        verarbeiteteEingabe :: Warteschlange Text,
        alternativeParser :: Warteschlange (AnfrageParser a e)
      }
  | AFStatusAnfrage
      { anfrageObjekt :: StatusAnfrageObjekt,
        konstruktor :: Objekt -> AnfrageParser a e
      }
  | AFStatusAnfrageMärklin
      { anfrageObjektMärklin :: StatusAnfrageObjektZugtyp 'Märklin,
        konstruktorMärklin :: ObjektZugtyp 'Märklin -> AnfrageParser a e
      }
  | AFStatusAnfrageLego
      { anfrageObjektLego :: StatusAnfrageObjektZugtyp 'Lego,
        konstruktorLego :: ObjektZugtyp 'Lego -> AnfrageParser a e
      }

instance Functor (AnfrageFortsetzung a) where
  fmap :: (e -> f) -> AnfrageFortsetzung a e -> AnfrageFortsetzung a f
  fmap funktion AFErgebnis {ergebnis, fortsetzung, eingabeRest} =
    AFErgebnis {ergebnis = funktion ergebnis, fortsetzung, eingabeRest}
  fmap funktion AFZwischenwert {zwischenwert, verarbeiteteEingabe, alternativeParser} =
    AFZwischenwert
      { zwischenwert,
        verarbeiteteEingabe,
        alternativeParser = fmap (fmap funktion) alternativeParser
      }
  fmap
    funktion
    AFFehler {alterZwischenwert, unbekannteEingabe, verarbeiteteEingabe, alternativeParser} =
      AFFehler
        { alterZwischenwert,
          unbekannteEingabe,
          verarbeiteteEingabe,
          alternativeParser = fmap (fmap funktion) alternativeParser
        }
  fmap funktion AFStatusAnfrage {anfrageObjekt, konstruktor} =
    AFStatusAnfrage {anfrageObjekt, konstruktor = fmap funktion . konstruktor}
  fmap funktion AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin} =
    AFStatusAnfrageMärklin
      { anfrageObjektMärklin,
        konstruktorMärklin = fmap funktion . konstruktorMärklin
      }
  fmap funktion AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego} =
    AFStatusAnfrageLego
      { anfrageObjektLego,
        konstruktorLego = fmap funktion . konstruktorLego
      }

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

  (<*>) :: AnfrageParser a (e -> f) -> AnfrageParser a e -> AnfrageParser a f
  (<*>) = ap

-- Ebenfalls in Modul Control.Applicative definiert:
-- Typklasse Alternative ist eine Unterklasse für Applikative Funktoren
-- mit Monoid-Struktur, d.h.: es gibt eine binäre Verknüpfung mit neutralem Element!
instance Alternative (AnfrageParser a) where
  empty :: AnfrageParser a e
  -- neutrales Element, ein Parser der immer fehlschlägt
  empty = AnfrageParser $ \alterZwischenwert eingabe ->
    AFFehler
      { alterZwischenwert,
        unbekannteEingabe = foldl' (<~>) (const Text.empty) eingabe $ Deutsch,
        verarbeiteteEingabe = Warteschlange.leer,
        alternativeParser = Warteschlange.leer
      }

  (<|>) :: forall e. AnfrageParser a e -> AnfrageParser a e -> AnfrageParser a e
  -- verknüpft zwei Parser zu einem Parser, linker Parser wird bevorzugt
  -- bei unvollständiger Eingabe (AFZwischenwert) wird bereits eine Entscheidung getroffen,
  -- die Parser sollten sich also so schnell wie möglich unterscheiden!
  (AnfrageParser p1) <|> alternativerParser = AnfrageParser $ \ma eingabe ->
    alternativenAuswerten eingabe (Warteschlange.einzelElement alternativerParser) $
      p1 ma eingabe

alternativenAuswerten ::
  forall a e.
  [Text] ->
  Warteschlange (AnfrageParser a e) ->
  AnfrageFortsetzung a e ->
  AnfrageFortsetzung a e
alternativenAuswerten eingabe alternativen anfrageFortsetzung =
  alternativenAuswertenAux anfrageFortsetzung
  where
    alternativenAuswertenAux :: AnfrageFortsetzung a e -> AnfrageFortsetzung a e
    alternativenAuswertenAux afErgebnis@AFErgebnis {} = afErgebnis
    alternativenAuswertenAux AFZwischenwert {zwischenwert, verarbeiteteEingabe, alternativeParser} =
      AFZwischenwert
        { zwischenwert,
          verarbeiteteEingabe = verarbeiteteEingabe <> Warteschlange.vonListe eingabe,
          alternativeParser = alternativeParser <> alternativen
        }
    alternativenAuswertenAux AFFehler {unbekannteEingabe, alterZwischenwert, verarbeiteteEingabe, alternativeParser} =
      case Warteschlange.zeigeErstes $ alternativeParser <> alternativen of
        (Warteschlange.Gefüllt pHead rest) ->
          alternativenAuswerten eingabe rest $ runParser pHead Nothing volleEingabe
          where
            volleEingabe :: [Text]
            volleEingabe =
              Warteschlange.zuListe $
                verarbeiteteEingabe <> Warteschlange.vonListe eingabe
        Warteschlange.Leer ->
          AFFehler
            { unbekannteEingabe,
              alterZwischenwert,
              verarbeiteteEingabe,
              alternativeParser = Warteschlange.leer
            }
    alternativenAuswertenAux AFStatusAnfrage {anfrageObjekt, konstruktor} =
      AFStatusAnfrage {anfrageObjekt, konstruktor}
    alternativenAuswertenAux AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin} =
      AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin}
    alternativenAuswertenAux AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego} =
      AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego}

instance Monad (AnfrageParser a) where
  (>>=) :: AnfrageParser a e -> (e -> AnfrageParser a f) -> AnfrageParser a f
  (AnfrageParser p1) >>= f = AnfrageParser $ \ma eingabe ->
    case alternativenAuswerten eingabe Warteschlange.leer $ p1 ma eingabe of
      AFErgebnis {ergebnis, fortsetzung, eingabeRest} ->
        let AnfrageParser p2 = f ergebnis
         in p2 fortsetzung eingabeRest
      AFZwischenwert {zwischenwert, verarbeiteteEingabe, alternativeParser} ->
        AFZwischenwert
          { zwischenwert,
            verarbeiteteEingabe,
            alternativeParser = fmap (>>= f) alternativeParser
          }
      AFFehler {unbekannteEingabe, alterZwischenwert, verarbeiteteEingabe} ->
        AFFehler
          { unbekannteEingabe,
            alterZwischenwert,
            verarbeiteteEingabe,
            alternativeParser = Warteschlange.leer
          }
      AFStatusAnfrage {anfrageObjekt, konstruktor} ->
        AFStatusAnfrage
          { anfrageObjekt,
            konstruktor = \objekt ->
              konstruktor objekt >>= f
          }
      AFStatusAnfrageMärklin {anfrageObjektMärklin, konstruktorMärklin} ->
        AFStatusAnfrageMärklin
          { anfrageObjektMärklin,
            konstruktorMärklin = \objekt ->
              konstruktorMärklin objekt >>= f
          }
      AFStatusAnfrageLego {anfrageObjektLego, konstruktorLego} ->
        AFStatusAnfrageLego
          { anfrageObjektLego,
            konstruktorLego = \objekt ->
              konstruktorLego objekt >>= f
          }

instance MonadPlus (AnfrageParser a)