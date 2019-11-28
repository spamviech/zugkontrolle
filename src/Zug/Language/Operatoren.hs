{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description : Operatoren zur Verkettung von Strings.

Dieses Modul stellt Operatoren zur Verknüpfung von zwei 'IsString' mit einem Leerzeichen/Trennzeichen bereit.
-}
module Zug.Language.Operatoren (
    Anzeige(..), ($#), (.#), Sprache(..), MitSprache(..), alleSprachen,
    (<~>), (<^>), (<=>), (<->), (<|>), (<:>), (<!>), (<°>), (<\>), (<#>),
    showText, addMnemonic) where

-- Bibliotheken
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.Hardware.WiringPi (Value())

-- | Bekannte Sprachen
data Sprache = Deutsch | Englisch
    deriving (Show, Read, Bounded, Enum, Eq)

-- | Klasse für Typen, die als 'Sprache' verwendet werden können.
class MitSprache s where
    leseSprache :: (Sprache -> a) -> s -> a
instance MitSprache Sprache where
    leseSprache :: (Sprache -> a) -> Sprache -> a
    leseSprache = id

-- | Alle unterstützten Sprachen
alleSprachen :: [Sprache]
alleSprachen = [minBound..maxBound]

-- | Zeige ein Objekt sprachabhängig an.
class Anzeige a where
    anzeige :: a -> Sprache -> Text
    default anzeige :: (Show a) => a -> Sprache -> Text
    anzeige = const . showText

instance Anzeige Text where
    anzeige :: Text -> Sprache -> Text
    anzeige = const

instance Anzeige (Sprache -> Text) where
    anzeige :: (Sprache -> Text) -> Sprache -> Text
    anzeige = id

instance Anzeige Char where
    anzeige :: Char -> Sprache -> Text
    anzeige a = pack . const [a]

instance Anzeige Natural

instance Anzeige Int

instance Anzeige Double

instance Anzeige Value

instance (Anzeige a) => Anzeige [a] where
    anzeige :: [a] -> Sprache -> Text
    anzeige liste = ("[" :: Text) <#> anzeigeAux liste <#> ("]" :: Text)
        where
            anzeigeAux :: (Anzeige b) => [b] -> Sprache -> Text
            anzeigeAux  []      = const ""
            anzeigeAux  [b]     = anzeige b
            anzeigeAux  (h : t) = h <^> anzeigeAux t

instance (Anzeige a, Anzeige b) => Anzeige (a, b) where
    anzeige :: (a, b) -> Sprache -> Text
    anzeige (a, b) = ("(" :: Text) <#> a <^> b <#> (")" :: Text)

infixr 0 $#
($#) :: (Anzeige a) => (Sprache -> Text -> b) -> a -> Sprache -> b
($#) f a sprache = f sprache $ anzeige a sprache

infixr 9 .#
(.#) :: (Anzeige b) => (Sprache -> Text -> c) -> (a -> b) -> a -> Sprache -> c
(.#) f g a = f $# g a

-- * Operatoren
verketten :: (Anzeige a, Anzeige b) => Text -> a -> b -> Sprache -> Text
verketten trennzeichen a b sprache = anzeige a sprache <> trennzeichen <> anzeige b sprache

infixr 6 <~>
-- | Verkette zwei Strings mit einem Leerzeichen.
-- 
-- Concatenate two strings with a space.
(<~>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<~>) = verketten " "

infixr 6 <^>
-- | Verkette zwei Strings mit einem Komma.
-- 
-- Concatenate two strings with a comma.
(<^>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<^>) = verketten ", "

infixr 6 <=>
-- | Verkette zwei Strings mit einem Gleichheitszeichen.
-- 
-- Concatenate two strings with a equal sign.
(<=>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<=>) = verketten "="

infixr 6 <->
-- | Verkette zwei Strings mit einem Bindestrinch.
-- 
-- Concatenate two strings with a hypthen.
(<->) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<->) = verketten "-"

infixr 6 <|>
-- | Verkette zwei Strings mit einem '|'.
-- 
-- Concatenate two strings with a '|'.
(<|>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<|>) = verketten "|"

infixr 6 <:>
-- | Verkette zwei Strings mit einem Doppelpunkt.
-- 
-- Concatenate two strings with a colon.
(<:>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<:>) = verketten ": "

infixr 6 <!>
-- | Verkette zwei Strings mit einem Ausrufezeichen und einem Zeilenumbruch.
-- 
-- Concatenate two strings with a exclamation mark an a new line.
(<!>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<!>) = verketten "!\n"

infixr 6 <°>
-- | Verkette zwei Strings mit einem Pfeil.
-- 
-- Concatenate two strings with an arrow.
(<°>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<°>) = verketten "->"

infixr 6 <\>
-- | Verkette zwei Strings mit einem Zeilenumbruch.
-- 
-- Concatenate two strings with a new line.
(<\>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<\>) = verketten "\n"

infixr 6 <#>
-- | Verkette zwi Strings.
-- 
-- Concatentate two strings
(<#>) :: (Anzeige a, Anzeige b) => a -> b -> Sprache -> Text
(<#>) = verketten ""

-- * Text-Hilfsfunktionen
-- | Show for 'Text'
showText :: (Show a) => a -> Text
showText = pack . show

-- | Mnemonic-Markierung hinzufügen
addMnemonic :: Text -> Text
addMnemonic s   = "_" <> s