{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

{-|
Description : Ungeordnete Mengen.

Mengen enthalten jedes Element höchstens einmal.
Die Implementierung ist nicht effizient, dafür werden keine weiteren Anforderungen an die Elemente gestellt.
-}
module Zug.Menge (Menge(),leer,ausFoldable,hinzufügen,entfernen,mitglied,vereinigung,schnittmenge) where

-- Bibliotheken
import Data.Foldable (Foldable(..))
import Data.List (delete,union,intersect,nub)
import Data.Text (Text)
import qualified Data.Text as Text

-- Abhängigkeit von anderen Modulen
import Zug.Language (Anzeige(..),Sprache(),(<#>))

-- | Eine 'Menge' ist eine Sammlung an Elementen, bei denen jedes Element höchstens einmal vorkommt.
newtype Menge a = Menge [a]

instance (Show a) => Show (Menge a) where
    show :: Menge a -> String
    show (Menge liste) = '{' : init (tail $ show liste) ++ "}"

instance (Anzeige a) => Anzeige (Menge a) where
    anzeige :: Menge a -> Sprache -> Text
    anzeige (Menge liste) = ("{" :: Text) <#> Text.init . Text.tail . anzeige liste <#> ("}" :: Text)

instance Foldable Menge where
    foldMap :: Monoid m => (a -> m) -> Menge a -> m
    foldMap f (Menge liste) = foldMap f liste

instance Functor Menge where
    fmap :: (a -> b) -> Menge a -> Menge b
    fmap f (Menge liste) = Menge $ f <$> liste

-- | Die leere Menge.
leer :: Menge a
leer = Menge []

-- | Erstelle eine 'Menge' aus einem 'Foldable'
ausFoldable :: (Eq a, Foldable f) => f a -> Menge a
ausFoldable foldable = Menge $ nub $ toList foldable

-- | Füge ein Element zu einer 'Menge' hinzu.
hinzufügen :: (Eq a) => a -> Menge a -> Menge a
hinzufügen a menge @ (Menge liste) =
    if mitglied a menge
        then menge
        else Menge $ a : liste

-- | Entferne ein Element aus einer 'Menge'. Hat keinen Effekt, falls das Element vorher nicht in der 'Menge' enthalten war.
entfernen :: (Eq a) => a -> Menge a -> Menge a
entfernen a (Menge liste) = Menge $ delete a liste

-- | Ist das Argument in der 'Menge' enthalten?
mitglied :: (Eq a) => a -> Menge a -> Bool
mitglied a (Menge liste) = a `elem` liste

-- | Erhalte die Vereinigung zweier 'Menge'n
vereinigung :: (Eq a) => Menge a -> Menge a -> Menge a
vereinigung (Menge liste0) (Menge liste1) = Menge $ union liste0 liste1

-- | Erhalte die Schnittmenge zweier 'Menge'n
schnittmenge :: (Eq a) => Menge a -> Menge a -> Menge a
schnittmenge (Menge liste0) (Menge liste1) = Menge $ intersect liste0 liste1