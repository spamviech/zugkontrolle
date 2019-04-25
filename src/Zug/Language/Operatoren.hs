{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Operatoren zur Verkettung von Strings.

Dieses Modul stellt Operatoren zur Verknüpfung von zwei 'IsString' mit einem Leerzeichen/Trennzeichen bereit.
-}
module Zug.Language.Operatoren where

import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))

-- * Operatoren
infixr 6 <~>
-- | Verkette zwei Strings mit einem Leerzeichen.
-- 
-- Concatenate two strings with a space.
(<~>) :: (Semigroup s, IsString s) => s -> s -> s
a <~> b = a <> " " <> b

infixr 6 <^>
-- | Verkette zwei Strings mit einem Komma.
-- 
-- Concatenate two strings with a comma.
(<^>) :: (Semigroup s, IsString s) => s -> s -> s
a <^> b = a <> ", " <> b

infixr 6 <=>
-- | Verkette zwei Strings mit einem Gleichheitszeichen.
-- 
-- Concatenate two strings with a equal sign.
(<=>) :: (Semigroup s, IsString s) => s -> s -> s
a <=> b = a <> "=" <> b

infixr 6 <->
-- | Verkette zwei Strings mit einem Bindestrinch.
-- 
-- Concatenate two strings with a hypthen.
(<->) :: (Semigroup s, IsString s) => s -> s -> s
a <-> b = a <> "-" <> b

infixr 6 <|>
-- | Verkette zwei Strings mit einem '|'.
-- 
-- Concatenate two strings with a '|'.
(<|>) :: (Semigroup s, IsString s) => s -> s -> s
a <|> b = a <> "|" <> b

infixr 6 <:>
-- | Verkette zwei Strings mit einem Doppelpunkt.
-- 
-- Concatenate two strings with a colon.
(<:>) :: (Semigroup s, IsString s) => s -> s -> s
a <:> b = a <> ": " <> b

infixr 6 <!>
-- | Verkette zwei Strings mit einem Ausrufezeichen und einem Zeilenumbruch.
-- 
-- Concatenate two strings with a exclamation mark an a new line.
(<!>) :: (Semigroup s, IsString s) => s -> s -> s
a <!> b = a <> "!\n" <> b

infixr 6 <°>
-- | Verkette zwei Strings mit einem Pfeil.
-- 
-- Concatenate two strings with an arrow.
(<°>) :: (Semigroup s, IsString s) => s -> s -> s
a <°> b = a <> "->" <> b

infixr 6 <\>
-- | Verkette zwei Strings mit einem Zeilenumbruch.
-- 
-- Concatenate two strings with a new line.
(<\>) :: (Semigroup s, IsString s) => s -> s -> s
a <\> b = a <> "\n" <> b

-- * Text-Hilfsfunktionen
-- | Show for 'IsString'
showText :: (Show a, IsString s) => a -> s
showText = fromString . show

-- | Mnemonic-Markierung hinzufügen
addMnemonic :: (Semigroup s, IsString s) => s -> s
addMnemonic s   = "_" <> s