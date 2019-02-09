{-# LANGUAGE InstanceSigs #-}

module Zug.SEQueue (SEQueue(), seEmpty, append, view, viewLast, ViewL(..), ViewR(..)) where

-- Bibliotheken
import Data.Foldable (toList)
import Data.Sequence.Queue

-- | Foldable-Instanz von Queue erzeugt Liste von toList in umgekehrter Reihenfolge
newtype SEQueue a = SEQueue (Queue a)

seEmpty :: SEQueue a
seEmpty = SEQueue empty

append :: a -> SEQueue a -> SEQueue a
append a (SEQueue queue) = SEQueue $ queue |> a

view :: SEQueue a -> ViewL SEQueue a
view (SEQueue queue) = case viewl queue of
    (EmptyL)    -> EmptyL
    (h :< t)    -> h :< SEQueue t

viewLast :: SEQueue a -> ViewR SEQueue a
viewLast (SEQueue queue) = case viewr queue of
    (EmptyR)    -> EmptyR
    (t :> h)    -> SEQueue t :> h

instance Foldable SEQueue where
    foldMap :: Monoid m => (a -> m) -> SEQueue a -> m 
    foldMap f seQueue = case view seQueue of
        (EmptyL)    -> mempty
        (h :< t)    -> mappend (f h) $ foldMap f t

instance Functor SEQueue where
    fmap :: (a -> b) -> SEQueue a -> SEQueue b
    fmap f (SEQueue queue) = SEQueue $ fmap f queue

instance (Show a) => Show (SEQueue a) where
    show :: SEQueue a -> String
    show = show . toList