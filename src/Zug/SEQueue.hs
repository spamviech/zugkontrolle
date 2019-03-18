{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Description :
Single-Ended Queue mit O(1) 'anhängen' und amortisierter O(1) 'zeigeErstes'-Funktion.
-}
module Zug.SEQueue (SEQueue(), leer, einzelelement, vonListe, anhängen, zeigeErstes, zeigeLetztes, Anzeige(..)) where

-- Bibliotheken
import Data.Foldable (toList)

-- | Single-Ended Queue
data SEQueue a = SEQueue {eingabe::[a], ausgabe::[a]}

-- | Ergebnis-Typ von 'zeigeErstes' und 'zeigeLetztes'
data Anzeige a = Leer | Gefüllt a (SEQueue a)

-- | Leere 'SEQueue'
leer :: SEQueue a
leer = SEQueue {eingabe=[], ausgabe=[]}

-- | 'SEQueue' mit genau einem Element
einzelelement :: a -> SEQueue a
einzelelement a = SEQueue {eingabe=[], ausgabe=[a]}

-- | Erstelle 'SEQueue' aus einer Liste. Der Kopf der Liste ist das erste Element in der SEQueue.
vonListe :: [a] -> SEQueue a
vonListe ausgabe = SEQueue {eingabe=[], ausgabe}

-- | Hänge ein Element an eine 'SEQueue' an. Effizenz ist O(1).
anhängen :: a -> SEQueue a -> SEQueue a
anhängen a (SEQueue {eingabe=e1, ausgabe}) = SEQueue {eingabe=a:e1, ausgabe}

-- | Erhalte das erste Element und die Verbleibende 'SEQueue'. Amortisierte Effizenz ist O(1).
zeigeErstes :: SEQueue a -> Anzeige a
zeigeErstes (SEQueue {eingabe=[], ausgabe=[]}) = Leer
zeigeErstes (SEQueue {eingabe, ausgabe=[]})    = let (a:ausgabe) = reverse eingabe in Gefüllt a (SEQueue {eingabe=[], ausgabe})
zeigeErstes (SEQueue {eingabe, ausgabe=(h:t)}) = Gefüllt h (SEQueue {eingabe, ausgabe=t})

-- | Erhalte das zuletzt hinzugefügte Element und die 'SEQueue' vor hinzufügen des selben. Effizenz ist im schlimmsten Fall O(n).
zeigeLetztes :: SEQueue a -> Anzeige a
zeigeLetztes (SEQueue {eingabe=[], ausgabe=[]}) = Leer
zeigeLetztes (SEQueue {eingabe=[], ausgabe=a1}) = Gefüllt (last a1) (SEQueue {eingabe=[], ausgabe=init a1})
zeigeLetztes (SEQueue {eingabe=(h:t), ausgabe}) = Gefüllt h (SEQueue {eingabe=t, ausgabe})

instance Foldable SEQueue where
    foldMap :: Monoid m => (a -> m) -> SEQueue a -> m 
    foldMap f seQueue = case zeigeErstes seQueue of
        (Leer)         -> mempty
        (Gefüllt h t)    -> mappend (f h) $ foldMap f t

instance Functor SEQueue where
    fmap :: (a -> b) -> SEQueue a -> SEQueue b
    fmap f (SEQueue {eingabe=e1, ausgabe=a1}) = SEQueue {eingabe=f <$> e1, ausgabe=f <$> a1}

instance (Show a) => Show (SEQueue a) where
    show :: SEQueue a -> String
    show = show . toList