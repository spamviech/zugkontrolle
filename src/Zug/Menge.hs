{-# LANGUAGE InstanceSigs #-}

{-|
Description : Ungeordnete Mengen.

Mengen enthalten jedes Element höchstens einmal. Die Implementierung ist nicht effizient, dafür werden keine weiteren Anforderungen an die Elemente gestellt.
-}
module Zug.Menge (Menge(), leer, hinzufügen, entfernen, mitglied) where

import Data.List (delete)

-- | Eine 'Menge' ist eine Sammlung an Elementen, bei denen jedes Element höchstens einmal vorkommt.
newtype Menge a = Menge [a]

instance Foldable Menge where
    foldMap :: Monoid m => (a -> m) -> Menge a -> m 
    foldMap f (Menge liste) = foldMap f liste

instance Functor Menge where
    fmap :: (a -> b) -> Menge a -> Menge b
    fmap f (Menge liste) = Menge $ f <$> liste

-- | Die leere Menge.
leer :: Menge a
leer = Menge []

-- | Füge ein Element zu einer 'Menge' hinzu.
hinzufügen :: (Eq a) => a -> Menge a -> Menge a
hinzufügen a menge@(Menge liste) = if mitglied a menge then menge else Menge $ a : liste

-- | Entferne ein Element aus einer 'Menge'. Hat keinen Effekt, falls das Element vorher nicht in der 'Menge' enthalten war.
entfernen :: (Eq a) => a -> Menge a -> Menge a
entfernen a (Menge liste) = Menge $ delete a liste

-- | Ist das Argument in der 'Menge' enthalten?
mitglied :: (Eq a) => a -> Menge a -> Bool
mitglied a (Menge liste) = elem a liste