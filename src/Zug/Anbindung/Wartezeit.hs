{-# LANGUAGE InstanceSigs #-}

{-|
Description: Stellt eine Funktion zur Verfügung einen Thread für eine bestimmte Zeit zu pausieren.
Im Gegensatz zu 'threadDelay' kommt es bei 0-Argumenten zu keinem divide-by-zero error.
Außerdem wird ein 'Wartezeit'-Typ mit automatischer Einheiten-Konvertierung berietgestellt.
-}
module Zug.Anbindung.Wartezeit (warte, Wartezeit(..), addition, differenz, multiplizieren, dividieren) where

-- Bibliotheken
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..))
import Data.Semigroup (Semigroup(..))
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Language (Anzeige(..))

-- | Warte mindestens das Argument in µs.
-- 
-- Die Wartezeit kann länger sein (bedingt durch 'threadDelay'), allerdings kommt es nicht zu einem divide-by-zero error für 0-Argumente.
warte :: (MonadIO m) => Wartezeit -> m ()
warte (NanoSekunden ns)   = let µs = div ns nsInµs in liftIO $ when (µs > 0) $ threadDelay $ fromIntegral µs
warte (MikroSekunden µs)  = liftIO $ when (µs > 0) $ threadDelay $ fromIntegral µs
warte (MilliSekunden ms)  = liftIO $ when (ms > 0) $ threadDelay $ fromIntegral $ µsInms * ms
warte (Sekunden s)        = liftIO $ when (s > 0) $ threadDelay $ fromIntegral $ µsInS * s
warte (Minuten min)       = liftIO $ when (min > 0) $ threadDelay $ fromIntegral $ µsInMin * min
warte (Stunden h)         = liftIO $ when (h > 0) $ threadDelay $ fromIntegral $ µsInH * h
warte (Tage d)            = liftIO $ when (d > 0) $ threadDelay $ fromIntegral $ µsInD * d

-- | Wartezeit mit unterschiedlichen Zeiteinheiten markiert. Argument-Typ für 'warte'. Die 'Semigroup'-Instanz ist eine Addition der Wartezeiten.
data Wartezeit
    = NanoSekunden Natural
    | MikroSekunden Natural
    | MilliSekunden Natural
    | Sekunden Natural
    | Minuten Natural
    | Stunden Natural
    | Tage Natural
    deriving (Read, Show)

instance Anzeige Wartezeit

instance Ord Wartezeit where
    compare :: Wartezeit -> Wartezeit -> Ordering
    compare
        (NanoSekunden ns1)
        (NanoSekunden ns2)
            = compare ns1 ns2
    compare
        (NanoSekunden ns)
        (MikroSekunden µs)
            = compare ns $ nsInµs * µs
    compare
        (NanoSekunden ns)
        (MilliSekunden ms)
            = compare ns $ nsInms * ms
    compare
        (NanoSekunden ns)
        (Sekunden s)
            = compare ns $ nsInS * s
    compare
        (NanoSekunden ns)
        (Minuten min)
            = compare ns $ nsInMin * min
    compare
        (NanoSekunden ns)
        (Stunden h)
            = compare ns $ nsInH * h
    compare
        (NanoSekunden ns)
        (Tage d)
            = compare ns $ nsInD * d
    compare
        (MikroSekunden µs)
        (NanoSekunden ns)
            = compare (nsInµs * µs) ns
    compare
        (MikroSekunden µs1)
        (MikroSekunden µs2)
            = compare µs1 µs2
    compare
        (MikroSekunden µs)
        (MilliSekunden ms)
            = compare µs $ µsInms * ms
    compare
        (MikroSekunden µs)
        (Sekunden s)
            = compare µs $ µsInS * s
    compare
        (MikroSekunden µs)
        (Minuten min)
            = compare µs $ µsInMin * min
    compare
        (MikroSekunden µs)
        (Stunden h)
            = compare µs $ µsInH * h
    compare
        (MikroSekunden µs)
        (Tage d)
            = compare µs $ µsInD * d
    compare
        (MilliSekunden ms)
        (NanoSekunden ns)
            = compare (nsInms * ms) ns
    compare
        (MilliSekunden ms)
        (MikroSekunden µs)
            = compare (µsInms * ms) µs
    compare
        (MilliSekunden ms1)
        (MilliSekunden ms2)
            = compare ms1 ms2
    compare
        (MilliSekunden ms)
        (Sekunden s)
            = compare ms $ msInS * s
    compare
        (MilliSekunden ms)
        (Minuten min)
            = compare ms $ msInMin * min
    compare
        (MilliSekunden ms)
        (Stunden h)
            = compare ms $ msInH * h
    compare
        (MilliSekunden ms)
        (Tage d)
            = compare ms $ msInD * d
    compare
        (Sekunden s)
        (NanoSekunden ns)
            = compare (nsInS * s) ns
    compare
        (Sekunden s)
        (MikroSekunden µs)
            = compare (µsInS * s) µs
    compare
        (Sekunden s)
        (MilliSekunden ms)
            = compare (msInS * s) ms
    compare
        (Sekunden s1)
        (Sekunden s2)
            = compare s1 s2
    compare
        (Sekunden s)
        (Minuten min)
            = compare s $ sInMin * min
    compare
        (Sekunden s)
        (Stunden h)
            = compare s $ sInH * h
    compare
        (Sekunden s)
        (Tage d)
            = compare s $ sInD * d
    compare
        (Minuten min)
        (NanoSekunden ns)
            = compare (nsInMin * min) ns
    compare
        (Minuten min)
        (MikroSekunden µs)
            = compare (µsInMin * min) µs
    compare
        (Minuten min)
        (MilliSekunden ms)
            = compare (msInMin * min) ms
    compare
        (Minuten min)
        (Sekunden s)
            = compare (sInMin * min) s
    compare
        (Minuten min1)
        (Minuten min2)
            = compare min1 min2
    compare
        (Minuten min)
        (Stunden h)
            = compare min $ minInH * h
    compare
        (Minuten min)
        (Tage d)
            = compare min $ minInD * d
    compare
        (Stunden h)
        (NanoSekunden ns)
            = compare (nsInH * h) ns
    compare
        (Stunden h)
        (MikroSekunden µs)
            = compare (µsInH * h) µs
    compare
        (Stunden h)
        (MilliSekunden ms)
            = compare (msInH * h) ms
    compare
        (Stunden h)
        (Sekunden s)
            = compare (sInH * h) s
    compare
        (Stunden h)
        (Minuten min)
            = compare (minInH * h) min
    compare
        (Stunden h1)
        (Stunden h2)
            = compare h1 h2
    compare
        (Stunden h)
        (Tage d)
            = compare h $ hInD * d
    compare
        (Tage d)
        (NanoSekunden ns)
            = compare (nsInD * d) ns
    compare
        (Tage d)
        (MikroSekunden µs)
            = compare (µsInD * d) µs
    compare
        (Tage d)
        (MilliSekunden ms)
            = compare (msInD * d) ms
    compare
        (Tage d)
        (Sekunden s)
            = compare (sInD * d) s
    compare
        (Tage d)
        (Minuten min)
            = compare (minInD * d) min
    compare
        (Tage d)
        (Stunden h)
            = compare (hInD * d) h
    compare
        (Tage d1)
        (Tage d2)
            = compare d1 d2

instance Eq Wartezeit where
    (==) :: Wartezeit -> Wartezeit -> Bool
    (==) = curry $ (== EQ) . uncurry compare


instance Semigroup Wartezeit where
    (<>) :: Wartezeit -> Wartezeit -> Wartezeit
    (<>)
        (NanoSekunden ns1)
        (NanoSekunden ns2)
            = NanoSekunden $ (+) ns1 ns2
    (<>)
        (NanoSekunden ns)
        (MikroSekunden µs)
            = NanoSekunden $ (+) ns $ nsInµs * µs
    (<>)
        (NanoSekunden ns)
        (MilliSekunden ms)
            = NanoSekunden $ (+) ns $ nsInms * ms
    (<>)
        (NanoSekunden ns)
        (Sekunden s)
            = NanoSekunden $ (+) ns $ nsInS * s
    (<>)
        (NanoSekunden ns)
        (Minuten min)
            = NanoSekunden $ (+) ns $ nsInMin * min
    (<>)
        (NanoSekunden ns)
        (Stunden h)
            = NanoSekunden $ (+) ns $ nsInH * h
    (<>)
        (NanoSekunden ns)
        (Tage d)
            = NanoSekunden $ (+) ns $ nsInD * d
    (<>)
        (MikroSekunden µs)
        (NanoSekunden ns)
            = MikroSekunden $ (+) (nsInµs * µs) ns
    (<>)
        (MikroSekunden µs1)
        (MikroSekunden µs2)
            = MikroSekunden $ (+) µs1 µs2
    (<>)
        (MikroSekunden µs)
        (MilliSekunden ms)
            = MikroSekunden $ (+) µs $ µsInms * ms
    (<>)
        (MikroSekunden µs)
        (Sekunden s)
            = MikroSekunden $ (+) µs $ µsInS * s
    (<>)
        (MikroSekunden µs)
        (Minuten min)
            = MikroSekunden $ (+) µs $ µsInMin * min
    (<>)
        (MikroSekunden µs)
        (Stunden h)
            = MikroSekunden $ (+) µs $ µsInH * h
    (<>)
        (MikroSekunden µs)
        (Tage d)
            = MikroSekunden $ (+) µs $ µsInD * d
    (<>)
        (MilliSekunden ms)
        (NanoSekunden ns)
            = NanoSekunden $ (+) (nsInms * ms) ns
    (<>)
        (MilliSekunden ms)
        (MikroSekunden µs)
            = MikroSekunden $ (+) (µsInms * ms) µs
    (<>)
        (MilliSekunden ms1)
        (MilliSekunden ms2)
            = MilliSekunden $ (+) ms1 ms2
    (<>)
        (MilliSekunden ms)
        (Sekunden s)
            = MilliSekunden $ (+) ms $ msInS * s
    (<>)
        (MilliSekunden ms)
        (Minuten min)
            = MilliSekunden $ (+) ms $ msInMin * min
    (<>)
        (MilliSekunden ms)
        (Stunden h)
            = MilliSekunden $ (+) ms $ msInH * h
    (<>)
        (MilliSekunden ms)
        (Tage d)
            = MilliSekunden $ (+) ms $ msInD * d
    (<>)
        (Sekunden s)
        (NanoSekunden ns)
            = NanoSekunden $ (+) (nsInS * s) ns
    (<>)
        (Sekunden s)
        (MikroSekunden µs)
            = MikroSekunden $ (+) (µsInS * s) µs
    (<>)
        (Sekunden s)
        (MilliSekunden ms)
            = MilliSekunden $ (+) (msInS * s) ms
    (<>)
        (Sekunden s1)
        (Sekunden s2)
            = Sekunden $ (+) s1 s2
    (<>)
        (Sekunden s)
        (Minuten min)
            = Sekunden $ (+) s $ sInMin * min
    (<>)
        (Sekunden s)
        (Stunden h)
            = Sekunden $ (+) s $ sInH * h
    (<>)
        (Sekunden s)
        (Tage d)
            = Sekunden $ (+) s $ sInD * d
    (<>)
        (Minuten min)
        (NanoSekunden ns)
            = NanoSekunden $ (+) (nsInMin * min) ns
    (<>)
        (Minuten min)
        (MikroSekunden µs)
            = MikroSekunden $ (+) (µsInMin * min) µs
    (<>)
        (Minuten min)
        (MilliSekunden ms)
            = MilliSekunden $ (+) (msInMin * min) ms
    (<>)
        (Minuten min)
        (Sekunden s)
            = Sekunden $ (+) (sInMin * min) s
    (<>)
        (Minuten min1)
        (Minuten min2)
            = Minuten $ (+) min1 min2
    (<>)
        (Minuten min)
        (Stunden h)
            = Minuten $ (+) min $ minInH * h
    (<>)
        (Minuten min)
        (Tage d)
            = Minuten $ (+) min $ minInD * d
    (<>)
        (Stunden h)
        (NanoSekunden ns)
            = NanoSekunden $ (+) (nsInH * h) ns
    (<>)
        (Stunden h)
        (MikroSekunden µs)
            = MikroSekunden $ (+) (µsInH * h) µs
    (<>)
        (Stunden h)
        (MilliSekunden ms)
            = MilliSekunden $ (+) (msInH * h) ms
    (<>)
        (Stunden h)
        (Sekunden s)
            = Sekunden $ (+) (sInH * h) s
    (<>)
        (Stunden h)
        (Minuten min)
            = Minuten $ (+) (minInH * h) min
    (<>)
        (Stunden h1)
        (Stunden h2)
            = Stunden $ (+) h1 h2
    (<>)
        (Stunden h)
        (Tage d)
            = Stunden $ (+) h $ hInD * d
    (<>)
        (Tage d)
        (NanoSekunden ns)
            = NanoSekunden $ (+) (nsInD * d) ns
    (<>)
        (Tage d)
        (MikroSekunden µs)
            = MikroSekunden $ (+) (µsInD * d) µs
    (<>)
        (Tage d)
        (MilliSekunden ms)
            = MilliSekunden $ (+) (msInD * d) ms
    (<>)
        (Tage d)
        (Sekunden s)
            = Sekunden $ (+) (sInD * d) s
    (<>)
        (Tage d)
        (Minuten min)
            = Minuten $ (+) (minInD * d) min
    (<>)
        (Tage d)
        (Stunden h)
            = Stunden $ (+) (hInD * d) h
    (<>)
        (Tage d1)
        (Tage d2)
            = Tage $ (+) d1 d2

-- | Addiere zwei Wartezeiten.
addition :: Wartezeit -> Wartezeit -> Wartezeit
addition = (<>)

-- | Berechne den Betrag der differenz zwischen zwei 'Wartezeit'en.
differenz :: Wartezeit -> Wartezeit -> Wartezeit
differenz t1 t2
    | t1 > t2   = differenzAux t1 t2
    | otherwise = differenzAux t2 t1
        where
            differenzAux
                (NanoSekunden ns1)
                (NanoSekunden ns2)
                    = NanoSekunden $ (-) ns1 ns2
            differenzAux
                (NanoSekunden ns)
                (MikroSekunden µs)
                    = NanoSekunden $ (-) ns $ nsInµs * µs
            differenzAux
                (NanoSekunden ns)
                (MilliSekunden ms)
                    = NanoSekunden $ (-) ns $ nsInms * ms
            differenzAux
                (NanoSekunden ns)
                (Sekunden s)
                    = NanoSekunden $ (-) ns $ nsInS * s
            differenzAux
                (NanoSekunden ns)
                (Minuten min)
                    = NanoSekunden $ (-) ns $ nsInMin * min
            differenzAux
                (NanoSekunden ns)
                (Stunden h)
                    = NanoSekunden $ (-) ns $ nsInH * h
            differenzAux
                (NanoSekunden ns)
                (Tage d)
                    = NanoSekunden $ (-) ns $ nsInD * d
            differenzAux
                (MikroSekunden µs)
                (NanoSekunden ns)
                    = MikroSekunden $ (-) (nsInµs * µs) ns
            differenzAux
                (MikroSekunden µs1)
                (MikroSekunden µs2)
                    = MikroSekunden $ (-) µs1 µs2
            differenzAux
                (MikroSekunden µs)
                (MilliSekunden ms)
                    = MikroSekunden $ (-) µs $ µsInms * ms
            differenzAux
                (MikroSekunden µs)
                (Sekunden s)
                    = MikroSekunden $ (-) µs $ µsInS * s
            differenzAux
                (MikroSekunden µs)
                (Minuten min)
                    = MikroSekunden $ (-) µs $ µsInMin * min
            differenzAux
                (MikroSekunden µs)
                (Stunden h)
                    = MikroSekunden $ (-) µs $ µsInH * h
            differenzAux
                (MikroSekunden µs)
                (Tage d)
                    = MikroSekunden $ (-) µs $ µsInD * d
            differenzAux
                (MilliSekunden ms)
                (NanoSekunden ns)
                    = NanoSekunden $ (-) (nsInms * ms) ns
            differenzAux
                (MilliSekunden ms)
                (MikroSekunden µs)
                    = MikroSekunden $ (-) (µsInms * ms) µs
            differenzAux
                (MilliSekunden ms1)
                (MilliSekunden ms2)
                    = MilliSekunden $ (-) ms1 ms2
            differenzAux
                (MilliSekunden ms)
                (Sekunden s)
                    = MilliSekunden $ (-) ms $ msInS * s
            differenzAux
                (MilliSekunden ms)
                (Minuten min)
                    = MilliSekunden $ (-) ms $ msInMin * min
            differenzAux
                (MilliSekunden ms)
                (Stunden h)
                    = MilliSekunden $ (-) ms $ msInH * h
            differenzAux
                (MilliSekunden ms)
                (Tage d)
                    = MilliSekunden $ (-) ms $ msInD * d
            differenzAux
                (Sekunden s)
                (NanoSekunden ns)
                    = NanoSekunden $ (-) (nsInS * s) ns
            differenzAux
                (Sekunden s)
                (MikroSekunden µs)
                    = MikroSekunden $ (-) (µsInS * s) µs
            differenzAux
                (Sekunden s)
                (MilliSekunden ms)
                    = MilliSekunden $ (-) (msInS * s) ms
            differenzAux
                (Sekunden s1)
                (Sekunden s2)
                    = Sekunden $ (-) s1 s2
            differenzAux
                (Sekunden s)
                (Minuten min)
                    = Sekunden $ (-) s $ sInMin * min
            differenzAux
                (Sekunden s)
                (Stunden h)
                    = Sekunden $ (-) s $ sInH * h
            differenzAux
                (Sekunden s)
                (Tage d)
                    = Sekunden $ (-) s $ sInD * d
            differenzAux
                (Minuten min)
                (NanoSekunden ns)
                    = NanoSekunden $ (-) (nsInMin * min) ns
            differenzAux
                (Minuten min)
                (MikroSekunden µs)
                    = MikroSekunden $ (-) (µsInMin * min) µs
            differenzAux
                (Minuten min)
                (MilliSekunden ms)
                    = MilliSekunden $ (-) (msInMin * min) ms
            differenzAux
                (Minuten min)
                (Sekunden s)
                    = Sekunden $ (-) (sInMin * min) s
            differenzAux
                (Minuten min1)
                (Minuten min2)
                    = Minuten $ (-) min1 min2
            differenzAux
                (Minuten min)
                (Stunden h)
                    = Minuten $ (-) min $ minInH * h
            differenzAux
                (Minuten min)
                (Tage d)
                    = Minuten $ (-) min $ minInD * d
            differenzAux
                (Stunden h)
                (NanoSekunden ns)
                    = NanoSekunden $ (-) (nsInH * h) ns
            differenzAux
                (Stunden h)
                (MikroSekunden µs)
                    = MikroSekunden $ (-) (µsInH * h) µs
            differenzAux
                (Stunden h)
                (MilliSekunden ms)
                    = MilliSekunden $ (-) (msInH * h) ms
            differenzAux
                (Stunden h)
                (Sekunden s)
                    = Sekunden $ (-) (sInH * h) s
            differenzAux
                (Stunden h)
                (Minuten min)
                    = Minuten $ (-) (minInH * h) min
            differenzAux
                (Stunden h1)
                (Stunden h2)
                    = Stunden $ (-) h1 h2
            differenzAux
                (Stunden h)
                (Tage d)
                    = Stunden $ (-) h $ hInD * d
            differenzAux
                (Tage d)
                (NanoSekunden ns)
                    = NanoSekunden $ (-) (nsInD * d) ns
            differenzAux
                (Tage d)
                (MikroSekunden µs)
                    = MikroSekunden $ (-) (µsInD * d) µs
            differenzAux
                (Tage d)
                (MilliSekunden ms)
                    = MilliSekunden $ (-) (msInD * d) ms
            differenzAux
                (Tage d)
                (Sekunden s)
                    = Sekunden $ (-) (sInD * d) s
            differenzAux
                (Tage d)
                (Minuten min)
                    = Minuten $ (-) (minInD * d) min
            differenzAux
                (Tage d)
                (Stunden h)
                    = Stunden $ (-) (hInD * d) h
            differenzAux
                (Tage d1)
                (Tage d2)
                    = Tage $ (-) d1 d2

-- | Multipliziere eine 'Wartezeit' mit einem 'Natural'
multiplizieren :: Wartezeit -> Natural -> Wartezeit
multiplizieren  (NanoSekunden ns)  = NanoSekunden . (ns *)
multiplizieren  (MikroSekunden µs) = MikroSekunden . (µs *)
multiplizieren  (MilliSekunden ms) = MilliSekunden . (ms *)
multiplizieren  (Sekunden s)       = Sekunden . (s *)
multiplizieren  (Minuten min)      = Minuten . (min *)
multiplizieren  (Stunden h)        = Stunden . (h *)
multiplizieren  (Tage d)           = Tage . (d *)

-- | Dividiere eine 'Wartezeit' durch einem 'Natural'
dividieren :: Wartezeit -> Natural -> Wartezeit
dividieren  (NanoSekunden ns)  = NanoSekunden . div ns
dividieren  (MikroSekunden µs) = MikroSekunden . div µs
dividieren  (MilliSekunden ms) = MilliSekunden . div ms
dividieren  (Sekunden s)       = Sekunden . div s
dividieren  (Minuten min)      = Minuten . div min
dividieren  (Stunden h)        = Stunden . div h
dividieren  (Tage d)           = Tage . div d

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
-- | s in a day
sInD :: Natural
sInD = sInMin * minInD
-- Minuten
-- | min in an hour
minInH :: Natural
minInH = 60
-- | s in a Year
minInD :: Natural
minInD = minInH * hInD
-- Stunden
-- | hour in a day
hInD :: Natural
hInD = 24