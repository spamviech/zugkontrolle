{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Description :
Single-Ended Queue mit O(1) 'anhängen' und amortisierter O(1) 'zeigeErstes'-Funktion.
-}
module Zug.Warteschlange
  ( Warteschlange()
  , leer
  , einzelElement
  , vonListe
  , anhängen
  , zeigeErstes
  , zeigeLetztes
  , Anzeige(..)
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Prelude hiding (seq)

import qualified Zug.Language as Language

-- | Single-Ended Queue. Ein auslesen ist von beiden Seiten der Warteschlange möglich.
newtype Warteschlange a = Warteschlange { seq :: Seq a }

instance Foldable Warteschlange where
    foldMap :: Monoid m => (a -> m) -> Warteschlange a -> m
    foldMap f = foldMap f . seq

instance Functor Warteschlange where
    fmap :: (a -> b) -> Warteschlange a -> Warteschlange b
    fmap f = Warteschlange . fmap f . seq

instance (Show a) => Show (Warteschlange a) where
    show :: Warteschlange a -> String
    show = show . seq

instance (Language.Anzeige a) => Language.Anzeige (Warteschlange a) where
    anzeige :: Warteschlange a -> Language.Sprache -> Text
    anzeige = Language.anzeige . toList

instance (Eq a) => Eq (Warteschlange a) where
    (==) :: Warteschlange a -> Warteschlange a -> Bool
    a == b = seq a == seq b

-- | Ergebnis-Typ von 'zeigeErstes' und 'zeigeLetztes'.
data Anzeige a
    = Leer
    | Gefüllt a (Warteschlange a)

-- | Leere 'Warteschlange'
leer :: Warteschlange a
leer = Warteschlange { seq = Seq.Empty }

-- | 'Warteschlange' mit genau einem Element
einzelElement :: a -> Warteschlange a
einzelElement = Warteschlange . Seq.singleton

-- | Erstelle 'Warteschlange' aus einer Liste.
-- Der Kopf der Liste ist das erste Element in der Warteschlange.
vonListe :: [a] -> Warteschlange a
vonListe = Warteschlange . Seq.fromList

-- | Hänge ein Element an eine 'Warteschlange' an. Effizienz ist O(1).
anhängen :: a -> Warteschlange a -> Warteschlange a
anhängen a Warteschlange {seq} = Warteschlange { seq = seq :|> a }

-- | Erhalte das erste Element und die Verbleibende 'Warteschlange'. Amortisierte Effizienz ist O(1).
zeigeErstes :: Warteschlange a -> Anzeige a
zeigeErstes Warteschlange {seq = a :<| seq} = Gefüllt a Warteschlange { seq }
zeigeErstes Warteschlange {seq = Seq.Empty} = Leer

-- | Erhalte das zuletzt hinzugefügte Element und die 'Warteschlange' vor hinzufügen des selben.
-- Effizienz ist im schlimmsten Fall O(n).
zeigeLetztes :: Warteschlange a -> Anzeige a
zeigeLetztes Warteschlange {seq = seq :|> a} = Gefüllt a Warteschlange { seq }
zeigeLetztes Warteschlange {seq = Seq.Empty} = Leer