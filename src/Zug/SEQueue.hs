{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Description :
Single-Ended Queue mit O(1) 'append' und amortisierter O(1) 'view'-Funktion.
-}
module Zug.SEQueue (SEQueue(), empty, singleton, fromList, append, view, viewLast, View(..)) where

-- Bibliotheken
import Data.Foldable (toList)

-- | Single-Ended Queue
data SEQueue a = SEQueue {eingabe::[a], ausgabe::[a]}

-- | Ergebnis-Typ von 'view' und 'viewLast'
data View a = Empty | Filled a (SEQueue a)

-- | Leere 'SEQueue'
empty :: SEQueue a
empty = SEQueue {eingabe=[], ausgabe=[]}

-- | 'SEQueue' mit genau einem Element
singleton :: a -> SEQueue a
singleton a = SEQueue {eingabe=[], ausgabe=[a]}

-- | Erstelle 'SEQueue' aus einer Liste. Der Kopf der Liste ist das erste Element in der SEQueue.
fromList :: [a] -> SEQueue a
fromList ausgabe = SEQueue {eingabe=[], ausgabe}

-- | Hänge ein Element an eine 'SEQueue' an. Effizenz ist O(1).
append :: a -> SEQueue a -> SEQueue a
append a (SEQueue {eingabe=e1, ausgabe}) = SEQueue {eingabe=a:e1, ausgabe}

-- | Erhalte das erste Element und die Verbleibende 'SEQueue'. Amortisierte Effizenz ist O(1).
view :: SEQueue a -> View a
view (SEQueue {eingabe=[], ausgabe=[]}) = Empty
view (SEQueue {eingabe, ausgabe=[]})    = let (a:ausgabe) = reverse eingabe in Filled a (SEQueue {eingabe=[], ausgabe})
view (SEQueue {eingabe, ausgabe=(h:t)}) = Filled h (SEQueue {eingabe, ausgabe=t})

-- | Erhalte das zuletzt hinzugefügte Element und die 'SEQueue' vor hinzufügen des selben. Effizenz ist im schlimmsten Fall O(n).
viewLast :: SEQueue a -> View a
viewLast (SEQueue {eingabe=[], ausgabe=[]}) = Empty
viewLast (SEQueue {eingabe=[], ausgabe=a1}) = Filled (last a1) (SEQueue {eingabe=[], ausgabe=init a1})
viewLast (SEQueue {eingabe=(h:t), ausgabe}) = Filled h (SEQueue {eingabe=t, ausgabe})

instance Foldable SEQueue where
    foldMap :: Monoid m => (a -> m) -> SEQueue a -> m 
    foldMap f seQueue = case view seQueue of
        (Empty)         -> mempty
        (Filled h t)    -> mappend (f h) $ foldMap f t

instance Functor SEQueue where
    fmap :: (a -> b) -> SEQueue a -> SEQueue b
    fmap f (SEQueue {eingabe=e1, ausgabe=a1}) = SEQueue {eingabe=f <$> e1, ausgabe=f <$> a1}

instance (Show a) => Show (SEQueue a) where
    show :: SEQueue a -> String
    show = show . toList