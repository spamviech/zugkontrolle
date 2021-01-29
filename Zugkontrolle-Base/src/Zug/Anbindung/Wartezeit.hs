{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Description: Stellt eine Funktion zur Verfügung einen Thread für eine bestimmte Zeit zu pausieren.
Im Gegensatz zu 'threadDelay' kommt es bei 0-Argumenten zu keinem divide-by-zero error.
Außerdem wird ein 'Wartezeit'-Typ mit automatischer Einheiten-Konvertierung berietgestellt.
-}
module Zug.Anbindung.Wartezeit
  ( warte
  , Wartezeit(.., MikroSekunden, MilliSekunden, Sekunden, Minuten,
          Stunden, Tage)
  , addition
  , differenz
  , multiplizieren
  , dividieren
  ) where

import Control.Applicative (Alternative(..))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Aeson.Types ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import Data.Semigroup(Semigroup((<>)))
import Data.Text (Text)
import Numeric.Natural (Natural)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (Read(..), ReadPrec, lexP, parens)
import Text.Read.Lex (numberToInteger, Lexeme(Number))

import qualified Zug.JSONStrings as JS
import Zug.Language (Anzeige(..), Sprache(), (<#>))

-- | Warte mindestens das Argument in µs.
--
-- Die Wartezeit kann länger sein (bedingt durch 'threadDelay'), allerdings kommt es nicht zu einem divide-by-zero error für 0-Argumente.
warte :: (MonadIO m) => Wartezeit -> m ()
warte (MikroSekunden µs) = liftIO $ when (µs > 0) $ threadDelay $ fromIntegral µs

-- | Argument-Typ für 'warte'. Die 'Semigroup'-Instanz ist eine Addition der Wartezeiten.
--
-- Zur einfacheren Benutzung werden Pattern Synonyme für unterschieliche Zeiteinheiten bereitgestellt.
newtype Wartezeit = NanoSekunden Natural
    deriving (Show, Eq, Ord)

instance Read Wartezeit where
    readPrec :: ReadPrec Wartezeit
    readPrec =
        (NanoSekunden <$> parseNano)
        <|> (MikroSekunden <$> parseMikro)
        <|> (MilliSekunden <$> parseMilli)
        <|> (Sekunden <$> parseSekunden)
        <|> (Minuten <$> parseMinuten)
        <|> (Stunden <$> parseStunden)
        <|> (Tage <$> parseTage)
        where
            withConstructor :: String -> ReadPrec Natural
            withConstructor constructor = parens $ do
                ReadPrec.lift $ do
                    _string <- ReadP.string constructor
                    _space <- ReadP.char ' '
                    ReadP.skipSpaces
                (Number number) <- lexP
                case numberToInteger number >>= Just . fromInteger of
                    (Just n) -> pure n
                    _fail -> ReadPrec.pfail

            withPostfixUnit :: String -> ReadPrec Natural
            withPostfixUnit unit = parens $ do
                (Number number) <- lexP
                _unit <- ReadPrec.lift $ do
                    _maybeSpace <- ReadP.optional $ ReadP.char ' '
                    ReadP.string unit
                case numberToInteger number >>= Just . fromInteger of
                    (Just n) -> pure n
                    _fail -> ReadPrec.pfail

            parseNano :: ReadPrec Natural
            parseNano = withConstructor "NanoSekunden" <|> withPostfixUnit "ns"

            parseMikro :: ReadPrec Natural
            parseMikro = withConstructor "MikroSekunden" <|> withPostfixUnit "µs"

            parseMilli :: ReadPrec Natural
            parseMilli = withConstructor "MilliSekunden" <|> withPostfixUnit "ms"

            parseSekunden :: ReadPrec Natural
            parseSekunden = withConstructor "Sekunden" <|> withPostfixUnit "s"

            parseMinuten :: ReadPrec Natural
            parseMinuten = withConstructor "Minuten" <|> withPostfixUnit "min"

            parseStunden :: ReadPrec Natural
            parseStunden = withConstructor "Stunden" <|> withPostfixUnit "h"

            parseTage :: ReadPrec Natural
            parseTage = withConstructor "Tage" <|> withPostfixUnit "d"

instance Anzeige Wartezeit where
    anzeige :: Wartezeit -> Sprache -> Text
    anzeige (NanoSekunden n) = n <#> ("ns" :: Text)

instance Semigroup Wartezeit where
    (<>) :: Wartezeit -> Wartezeit -> Wartezeit
    (<>) (NanoSekunden ns1) (NanoSekunden ns2) = NanoSekunden $ (+) ns1 ns2

-- | Addiere zwei Wartezeiten.
addition :: Wartezeit -> Wartezeit -> Wartezeit
addition = (<>)

-- | Berechne den Betrag der differenz zwischen zwei 'Wartezeit'en.
differenz :: Wartezeit -> Wartezeit -> Wartezeit
differenz (NanoSekunden ns1) (NanoSekunden ns2)
    | ns1 > ns2 = NanoSekunden $ ns1 - ns2
    | otherwise = NanoSekunden $ ns2 - ns1

-- | Multipliziere eine 'Wartezeit' mit einem 'Natural'
multiplizieren :: Wartezeit -> Natural -> Wartezeit
multiplizieren (NanoSekunden ns) = NanoSekunden . (ns *)

-- | Dividiere eine 'Wartezeit' durch einem 'Natural'
dividieren :: Wartezeit -> Natural -> Wartezeit
dividieren (NanoSekunden ns) = NanoSekunden . div ns

-- Pattern Synonyme
pattern MikroSekunden :: Natural -> Wartezeit
pattern MikroSekunden µs <- NanoSekunden ((`div` nsInµs) -> µs)
    where
        MikroSekunden µs = NanoSekunden $ nsInµs * µs

{-# COMPLETE MikroSekunden #-}

pattern MilliSekunden :: Natural -> Wartezeit
pattern MilliSekunden ms <- NanoSekunden ((`div` nsInms) -> ms)
    where
        MilliSekunden ms = NanoSekunden $ nsInms * ms

{-# COMPLETE MilliSekunden #-}

pattern Sekunden :: Natural -> Wartezeit
pattern Sekunden s <- NanoSekunden ((`div` nsInS) -> s)
    where
        Sekunden s = NanoSekunden $ nsInS * s

{-# COMPLETE Sekunden #-}

pattern Minuten :: Natural -> Wartezeit
pattern Minuten s <- NanoSekunden ((`div` nsInMin) -> s)
    where
        Minuten s = NanoSekunden $ nsInMin * s

{-# COMPLETE Minuten #-}

pattern Stunden :: Natural -> Wartezeit
pattern Stunden s <- NanoSekunden ((`div` nsInH) -> s)
    where
        Stunden s = NanoSekunden $ nsInH * s

{-# COMPLETE Stunden #-}

pattern Tage :: Natural -> Wartezeit
pattern Tage s <- NanoSekunden ((`div` nsInD) -> s)
    where
        Tage s = NanoSekunden $ nsInD * s

{-# COMPLETE Tage #-}

-- Nanosekunden
-- | ns in a µs
nsInµs :: Natural
nsInµs = 1000

-- | ns in a millisecond
nsInms :: Natural
nsInms = nsInµs * µsInms

-- | ns in a second
nsInS :: Natural
nsInS = nsInµs * µsInS

-- | ns in a minute
nsInMin :: Natural
nsInMin = nsInµs * µsInMin

-- | ns in an hour
nsInH :: Natural
nsInH = nsInµs * µsInH

-- | ns in a day
nsInD :: Natural
nsInD = nsInµs * µsInD

-- Mikrosekunden
-- | µs in a millisecond
µsInms :: Natural
µsInms = 1000

-- | µs in a second
µsInS :: Natural
µsInS = µsInms * msInS

-- | µs in a minute
µsInMin :: Natural
µsInMin = µsInms * msInMin

-- | µs in an hour
µsInH :: Natural
µsInH = µsInms * msInH

-- | µs in an day
µsInD :: Natural
µsInD = µsInms * msInD

-- Millisekunden
-- | ms in a second
msInS :: Natural
msInS = 1000

-- | ms in a minute
msInMin :: Natural
msInMin = msInS * sInMin

-- | ms in an hour
msInH :: Natural
msInH = msInS * sInH

-- | ms in an hour
msInD :: Natural
msInD = msInS * sInH

-- Sekunden
-- | s in a minute
sInMin :: Natural
sInMin = 60

-- | s in an hour
sInH :: Natural
sInH = sInMin * minInH

-- -- | s in a day
-- sInD :: Natural
-- sInD = sInMin * minInD
-- Minuten
-- | min in an hour
minInH :: Natural
minInH = 60

-- -- | s in a Year
-- minInD :: Natural
-- minInD = minInH * hInD
-- -- Stunden
-- -- | hour in a day
-- hInD :: Natural
-- hInD = 24
-- JSON-Instanz-Deklarationen für Wartezeit
-- Dabei wird eine Rückwärtskompatibilität zu Versionen < 1.0.1.0 berücksichtigt.
-- Bei diesen wurde implizit immer Mikrosekunden angenommen, wodurch nur eine Zahl gespeichert wurde.
instance Aeson.FromJSON Wartezeit where
    parseJSON :: Aeson.Value -> Aeson.Parser Wartezeit
    parseJSON (Aeson.Object v) =
        (NanoSekunden <$> v .: JS.ns)
        <|> (MikroSekunden <$> v .: JS.µs)
        <|> (MilliSekunden <$> v .: JS.ms)
        <|> (Sekunden <$> v .: JS.s)
        <|> (Minuten <$> v .: JS.min)
        <|> (Stunden <$> v .: JS.h)
        <|> (Tage <$> v .: JS.d)
    parseJSON (Aeson.Number µs) = pure $ MikroSekunden $ floor µs
    parseJSON _value = empty

instance Aeson.ToJSON Wartezeit where
    toJSON :: Wartezeit -> Aeson.Value
    toJSON (NanoSekunden ns) = Aeson.object [JS.ns .= ns]
