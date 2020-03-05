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

-- Bibliotheken
import Data.Foldable (toList)
import Data.Text (Text)

-- Abhängigkeit von anderen Modulen
import qualified Zug.Language as Language

-- | Single-Ended Queue
data Warteschlange a = Warteschlange { eingabe :: [a], ausgabe :: [a] }

-- | Ergebnis-Typ von 'zeigeErstes' und 'zeigeLetztes'
data Anzeige a
    = Leer
    | Gefüllt a (Warteschlange a)

-- | Leere 'Warteschlange'
leer :: Warteschlange a
leer = Warteschlange { eingabe = [], ausgabe = [] }

-- | 'Warteschlange' mit genau einem Element
einzelElement :: a -> Warteschlange a
einzelElement a = Warteschlange { eingabe = [], ausgabe = [a] }

-- | Erstelle 'Warteschlange' aus einer Liste. Der Kopf der Liste ist das erste Element in der Warteschlange.
vonListe :: [a] -> Warteschlange a
vonListe ausgabe = Warteschlange { eingabe = [], ausgabe }

-- | Hänge ein Element an eine 'Warteschlange' an. Effizienz ist O(1).
anhängen :: a -> Warteschlange a -> Warteschlange a
anhängen a Warteschlange {eingabe = e1, ausgabe} = Warteschlange { eingabe = a : e1, ausgabe }

-- | Erhalte das erste Element und die Verbleibende 'Warteschlange'. Amortisierte Effizienz ist O(1).
zeigeErstes :: Warteschlange a -> Anzeige a
zeigeErstes Warteschlange {eingabe = [], ausgabe = []} = Leer
zeigeErstes Warteschlange {eingabe, ausgabe = []} =
    let (a:ausgabe) = reverse eingabe
    in Gefüllt a Warteschlange { eingabe = [], ausgabe }
zeigeErstes Warteschlange {eingabe, ausgabe = (h:t)} =
    Gefüllt h Warteschlange { eingabe, ausgabe = t }

-- | Erhalte das zuletzt hinzugefügte Element und die 'Warteschlange' vor hinzufügen des selben.
-- Effizienz ist im schlimmsten Fall O(n).
zeigeLetztes :: Warteschlange a -> Anzeige a
zeigeLetztes Warteschlange {eingabe = [], ausgabe = []} = Leer
zeigeLetztes Warteschlange {eingabe = [], ausgabe = a1} =
    Gefüllt (last a1) Warteschlange { eingabe = [], ausgabe = init a1 }
zeigeLetztes Warteschlange {eingabe = (h:t), ausgabe} =
    Gefüllt h Warteschlange { eingabe = t, ausgabe }

instance Foldable Warteschlange where
    foldMap :: Monoid m => (a -> m) -> Warteschlange a -> m
    foldMap f seQueue = case zeigeErstes seQueue of
        Leer -> mempty
        (Gefüllt h t) -> mappend (f h) $ foldMap f t

instance Functor Warteschlange where
    fmap :: (a -> b) -> Warteschlange a -> Warteschlange b
    fmap f Warteschlange {eingabe = e1, ausgabe = a1} =
        Warteschlange { eingabe = f <$> e1, ausgabe = f <$> a1 }

instance (Show a) => Show (Warteschlange a) where
    show :: Warteschlange a -> String
    show = show . toList

instance (Language.Anzeige a) => Language.Anzeige (Warteschlange a) where
    anzeige :: Warteschlange a -> Language.Sprache -> Text
    anzeige = Language.anzeige . toList