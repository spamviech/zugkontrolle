{-# LANGUAGE NamedFieldPuns, InstanceSigs #-}

{-|
Description : Low-Level-Definition der unterstützen Aktionen auf Pin-Ebene.
-}
module Zug.Anbindung (
                    -- * Pin-Repräsentation
                    Pin(), toPin, fromPin, hasPWM, hasClock, PinMap, pinMapEmpty, PinMapIO, delayµs,
                    -- * Strecken-Objekte
                    StreckenObjekt(..),
                    -- ** Bahngeschwindigkeiten
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..),
                    -- ** Streckenabschnitte
                    Streckenabschnitt(..), StreckenabschnittKlasse(..),
                    -- ** Weichen
                    Weiche(..), WeicheKlasse(..),
                    -- ** Kupplungen
                    Kupplung(..), KupplungKlasse(..),
                    -- ** Wegstrecken
                    Wegstrecke(..), WegstreckeKlasse(..)) where

-- Bibliotheken
import Control.Concurrent
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Numeric.Natural
import System.Hardware.WiringPi
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen
import Zug.Options
import qualified Zug.Language as Language
import Zug.Language ((<^>), (<=>), (<->), (<|>), (<:>), (<°>))
import Zug.Anbindung.SoftwarePWM

-- * Repräsentation von Pins
-- | Funktion um ein Pin-Objekt erstellen zu können, ohne WiringPi importieren zu müssen
toPin :: (Enum n) => n -> Pin
toPin = Gpio . fromEnum

-- | Funktion um Gpio-Zahl eines Pins zu erhalten, ohne WiringPi importieren zu müssen; Rückgabewert ist 0 falls der Pin nicht existiert
fromPin :: Pin -> Natural
fromPin pin = case (pinToBcmGpio pin) of
    (Just gpio) -> fromIntegral gpio
    (Nothing)   -> 0

-- * Test-Funktionen, ob Pins bestimmte Funktionen unterstützen
hasPWM :: Pin -> Bool
hasPWM = (`elem` [Wpi 1, Wpi 23, Wpi 24, Wpi 26])

hasClock :: Pin -> Bool
hasClock = (`elem` [Wpi 7, Wpi 21, Wpi 22, Wpi 29])

-- * PWM-Funktion
-- | pwmWrite mit alternativer Software-basierter PWM-Funktion
pwmWriteSoftHardware :: Pin -> PwmValue -> PinMapIO ()
pwmWriteSoftHardware pin pwmValue mvarPinMap = if hasPWM pin
    then do
        pinMode pin PWM_OUTPUT
        pwmSetRange pwmRange
        pwmWrite pin pwmValue
    else pwmWriteSoftware pin pwmFrequencyHzNormal pwmValue mvarPinMap

-- | Erzeuge PWM-Funktion für einen Servo-Motor
--   Nutze SoftwarePWM für eine konstante Frequenz (sonst abhängig pwmRange und pwmValue)
pwmServo :: Pin -> Natural -> PinMapIO ()
pwmServo pin value = pwmWriteSoftware pin pwmFrequencyHzServo $ getPwmValueFull value

-- ** Frequenzen
-- | 50 Hz Frequenz; Standard-Wert von Servo-Motoren
pwmFrequencyHzServo :: Natural
pwmFrequencyHzServo  = 50

-- | Normale PWM-Frequenz
pwmFrequencyHzNormal :: Natural
pwmFrequencyHzNormal = pwmFrequencyHzServo

-- | Erhalte PWMValue ausgehend von einem Wert zwischen 0 und einem Maximalwert
getPwmValue :: (Integral i) => i -> i -> PwmValue
getPwmValue maxValue value = fromIntegral pwmValue
    where
        pwmValue :: Natural
        pwmValue = div a 100
        a :: Natural
        a = (fromIntegral pwmRange) * (min (fromIntegral maxValue) $ fromIntegral value)

-- | Nutze komplette pwmRange
getPwmValueFull :: (Integral i) => i -> PwmValue
getPwmValueFull = getPwmValue 100

-- | Nutze 3/4 der pwmRange (maximal 16V von 24V Maximalspannung)
getPwmValueReduced :: (Integral i) => i -> PwmValue
getPwmValueReduced = getPwmValue 75

-- | Zeit, die Strom bei kurzen Schalt-Aktionen anliegt
generalDelayµs :: Natural
generalDelayµs = 1000000

-- * Repräsentation von StreckenObjekten
-- | Klassen-Definitionen
class StreckenObjekt s where
    zugtyp :: s -> Zugtyp
    zugtyp  _s  = Undefiniert
    pins :: s -> [Pin]
    getName :: s -> String
    {-# MINIMAL pins, getName #-}

data Bahngeschwindigkeit    = LegoBahngeschwindigkeit {bgName :: String, geschwindigkeitsPin::Pin, fahrtrichtungsPin::Pin}
                            | MärklinBahngeschwindigkeit {bgName :: String, geschwindigkeitsPin::Pin}
                                        deriving (Eq)

instance Show Bahngeschwindigkeit where
    show :: Bahngeschwindigkeit -> String
    show    (LegoBahngeschwindigkeit {bgName, geschwindigkeitsPin, fahrtrichtungsPin})    = Language.lego <-> Language.bahngeschwindigkeit <:> Language.name <=> bgName <^> Language.geschwindigkeit <-> Language.pin <=> show geschwindigkeitsPin <^> Language.fahrtrichtung <-> Language.pin <=> show fahrtrichtungsPin
    show    (MärklinBahngeschwindigkeit {bgName, geschwindigkeitsPin})                    = Language.märklin <-> Language.bahngeschwindigkeit <:> Language.name <=> bgName <^> Language.geschwindigkeit <-> Language.pin <=> show geschwindigkeitsPin

instance StreckenObjekt Bahngeschwindigkeit where
    zugtyp :: Bahngeschwindigkeit -> Zugtyp
    zugtyp  (LegoBahngeschwindigkeit {})    = Lego
    zugtyp  (MärklinBahngeschwindigkeit {}) = Märklin
    pins :: Bahngeschwindigkeit -> [Pin]
    pins    (LegoBahngeschwindigkeit {geschwindigkeitsPin, fahrtrichtungsPin})  = [geschwindigkeitsPin, fahrtrichtungsPin]
    pins    (MärklinBahngeschwindigkeit {geschwindigkeitsPin})                  = [geschwindigkeitsPin]
    getName :: Bahngeschwindigkeit -> String
    getName (LegoBahngeschwindigkeit {bgName})    = bgName
    getName (MärklinBahngeschwindigkeit {bgName}) = bgName

class (StreckenObjekt b) => BahngeschwindigkeitKlasse b where
    -- | Geschwindigkeit einstellen (akzeptiere Werte von 0 bis 100)
    geschwindigkeit :: b -> Natural -> PinMapIO ()
    -- | Gebe allen Zügen den Befehl zum Umdrehen/in einer bestimmen Richtung zu fahren
    umdrehen :: b -> Maybe Fahrtrichtung -> PinMapIO ()
    {-# MINIMAL geschwindigkeit, umdrehen #-}

-- | Zeit, die Strom beim Umdrehen einer Märklin-Bahngeschwindigkeit anliegt
umdrehenDelayµs :: Natural
umdrehenDelayµs = generalDelayµs

instance BahngeschwindigkeitKlasse Bahngeschwindigkeit where
    geschwindigkeit :: Bahngeschwindigkeit -> Natural -> PinMapIO ()
    geschwindigkeit (LegoBahngeschwindigkeit {geschwindigkeitsPin})     geschwindigkeit mvarPinMap  = befehlAusführen
        (pwmWriteSoftHardware geschwindigkeitsPin (getPwmValueFull geschwindigkeit) mvarPinMap)
        ("Geschwindigkeit (" ++ (show geschwindigkeitsPin) ++ ")->" ++ (show geschwindigkeit))
    geschwindigkeit (MärklinBahngeschwindigkeit {geschwindigkeitsPin})  geschwindigkeit mvarPinMap  = befehlAusführen
        (pwmWriteSoftHardware geschwindigkeitsPin (getPwmValueReduced geschwindigkeit) mvarPinMap)
        ("Geschwindigkeit (" ++ (show geschwindigkeitsPin) ++ ")->" ++ (show geschwindigkeit))
    umdrehen :: Bahngeschwindigkeit -> Maybe Fahrtrichtung -> PinMapIO ()
    umdrehen (LegoBahngeschwindigkeit {geschwindigkeitsPin, fahrtrichtungsPin}) (Just fahrtrichtung)    mvarPinMap = befehlAusführen
        (pwmWriteSoftHardware geschwindigkeitsPin 0 mvarPinMap >> delayµs umdrehenDelayµs >> pwmServo fahrtrichtungsPin (if (fahrtrichtung == Vorwärts) then 100 else 0) mvarPinMap)
        ("Umdrehen (" ++ (show geschwindigkeitsPin) ++ ", " ++ (show fahrtrichtungsPin) ++ ")->" ++ (show fahrtrichtung))
    umdrehen bahngeschwindigkeit@(LegoBahngeschwindigkeit {})                   (Nothing)               mvarPinMap = umdrehen bahngeschwindigkeit (Just Vorwärts) mvarPinMap
    umdrehen (MärklinBahngeschwindigkeit {geschwindigkeitsPin})                 _maybeRichtung  mvarPinMap = befehlAusführen
        (pwmWriteSoftHardware geschwindigkeitsPin 0 mvarPinMap >> delayµs umdrehenDelayµs >> pwmWriteSoftHardware geschwindigkeitsPin pwmRange mvarPinMap >> delayµs umdrehenDelayµs >> pwmWriteSoftHardware geschwindigkeitsPin 0 mvarPinMap)
        ("Umdrehen (" ++ (show geschwindigkeitsPin) ++ ")")

data Streckenabschnitt = Streckenabschnitt {stName :: String, stromPin::Pin}
                            deriving (Eq)

instance Show Streckenabschnitt where
    show :: Streckenabschnitt -> String
    show    (Streckenabschnitt {stName, stromPin})    = Language.streckenabschnitt <:> Language.name <=> stName <^> Language.strom <-> Language.pin <=> show stromPin

instance StreckenObjekt Streckenabschnitt where
    pins :: Streckenabschnitt -> [Pin]
    pins (Streckenabschnitt {stromPin})    = [stromPin]
    getName :: Streckenabschnitt -> String
    getName (Streckenabschnitt {stName})  = stName

class (StreckenObjekt s) => StreckenabschnittKlasse s where
    -- | Strom ein-/ausschalten
    strom :: s -> Bool -> MVar PinMap -> IO ()
    {-# MINIMAL strom #-}

instance StreckenabschnittKlasse Streckenabschnitt where
    strom :: Streckenabschnitt -> Bool -> PinMapIO ()
    strom   (Streckenabschnitt {stromPin})  an  _mvarPinMap = befehlAusführen
        (pinMode stromPin OUTPUT >> digitalWrite stromPin (if an then HIGH else LOW))
        ("Strom (" <> (show stromPin) <> ")->" <> (show an))

data Weiche = LegoWeiche {weName :: String, richtungsPin :: Pin, richtungen::(Richtung,Richtung)}
            | MärklinWeiche {weName :: String, richtungsPins :: NonEmpty (Richtung, Pin)}
                    deriving (Eq)

instance Show Weiche where
    show :: Weiche -> String
    show    (LegoWeiche {weName, richtungsPin, richtungen=(richtung1, richtung2)})    = Language.lego <-> Language.weiche <:> Language.name <=> weName <^> Language.richtung <-> Language.pin <=> show richtungsPin <^> Language.richtungen <=> show richtung1 <|> show richtung2
    show    (MärklinWeiche {weName, richtungsPins})                                   = Language.märklin <-> Language.weiche <:> Language.name <=> weName <^> foldl (\acc (pin, richtung) -> acc <^> show richtung <=> show pin) "" richtungsPins

instance StreckenObjekt Weiche where
    zugtyp :: Weiche -> Zugtyp
    zugtyp (LegoWeiche {})      = Lego
    zugtyp (MärklinWeiche {})   = Märklin
    pins :: Weiche -> [Pin]
    pins (LegoWeiche {richtungsPin})        = [richtungsPin]
    pins (MärklinWeiche {richtungsPins})    = map snd $ NE.toList richtungsPins
    getName :: Weiche -> String
    getName (LegoWeiche {weName})     = weName
    getName (MärklinWeiche {weName})  = weName

class (StreckenObjekt w) => WeicheKlasse w where
    -- | Weiche einstellen
    stellen :: w -> Richtung -> PinMapIO ()
    -- | Überprüfe, ob Weiche eine Richtung unterstützt
    hatRichtung :: w -> Richtung -> Bool
    hatRichtung weiche richtung = elem richtung $ getRichtungen weiche
    -- | Erhalte alle Richtungen einer Weiche
    getRichtungen :: w -> NonEmpty Richtung
    {-# MINIMAL stellen, getRichtungen #-}

-- | Zeit, die Strom beim Stellen einer Märklin-Weiche anliegt
weicheDelayµs :: Natural
weicheDelayµs = generalDelayµs

instance WeicheKlasse Weiche where
    stellen :: Weiche -> Richtung -> PinMapIO ()
    stellen (LegoWeiche {richtungsPin, richtungen}) richtung    mvarPinMap  | richtung == fst richtungen    = befehlAusführen
        (pwmServo richtungsPin 25 mvarPinMap >> delayµs weicheDelayµs >> pwmServo richtungsPin 0 mvarPinMap)
        ("Stellen (" ++ (show richtungsPin) ++ ") -> " ++ (show richtung))
                                                                            | richtung == snd richtungen    = befehlAusführen
        (pwmServo richtungsPin 75 mvarPinMap >> delayµs weicheDelayµs >> pwmServo richtungsPin 0 mvarPinMap)
        ("stellen (" ++ (show richtungsPin) ++ ") -> " ++ (show richtung))
                                                                            | otherwise                     = pure ()
    stellen (MärklinWeiche {richtungsPins})         richtung    _mvarPinMap = befehlAusführen
        richtungStellen
        ("Stellen (" ++ (show $ getRichtungsPin richtung $ NE.toList richtungsPins) ++ ") -> " ++ (show richtung))
            where
                richtungStellen :: IO ()
                richtungStellen = case getRichtungsPin richtung $ NE.toList richtungsPins of
                    (Nothing)           -> pure ()
                    (Just richtungsPin) -> pinMode richtungsPin OUTPUT >> digitalWrite richtungsPin HIGH >> delayµs weicheDelayµs >> digitalWrite richtungsPin LOW
                getRichtungsPin :: Richtung -> [(Richtung, Pin)] -> Maybe Pin
                getRichtungsPin _richtung   []  = Nothing
                getRichtungsPin richtung    ((ersteRichtung, ersterPin):andereRichtungen)
                    | richtung == ersteRichtung = Just ersterPin
                    | otherwise                 = getRichtungsPin richtung andereRichtungen
    hatRichtung :: Weiche -> Richtung -> Bool
    hatRichtung (LegoWeiche {richtungen=(erste, zweite)})   richtung    = (erste == richtung) || (zweite == richtung)
    hatRichtung (MärklinWeiche {richtungsPins})             richtung    = foldr (\(richtung0, _pin0) acc -> acc || (richtung0 == richtung)) False richtungsPins
    getRichtungen :: Weiche -> NonEmpty Richtung
    getRichtungen   (LegoWeiche {richtungen=(richtung1, richtung2)})    = richtung1 :| [richtung2]
    getRichtungen   (MärklinWeiche {richtungsPins})                     = fst <$> richtungsPins

data Kupplung = Kupplung {kuName :: String, kupplungsPin::Pin}
                    deriving (Eq)

instance Show Kupplung where
    show :: Kupplung -> String
    show    (Kupplung {kuName, kupplungsPin}) = Language.kupplung <:> Language.name <=> kuName <^> Language.kupplung <-> Language.pin <=> show kupplungsPin

instance StreckenObjekt Kupplung where
    pins :: Kupplung -> [Pin]
    pins (Kupplung {kupplungsPin})  = [kupplungsPin]
    getName :: Kupplung -> String
    getName (Kupplung {kuName})   = kuName

class (StreckenObjekt k) => KupplungKlasse k where
    -- | Kupplung betätigen
    kuppeln :: k -> PinMapIO ()
    {-# MINIMAL kuppeln #-}

-- | Zeit, die Strom beim Kuppeln anliegt
kuppelnDelayµs :: Natural
kuppelnDelayµs = generalDelayµs

instance KupplungKlasse Kupplung where
    kuppeln :: Kupplung -> PinMapIO ()
    kuppeln (Kupplung {kupplungsPin})   _mvarPinMap = befehlAusführen
        (pinMode kupplungsPin OUTPUT >> digitalWrite kupplungsPin HIGH >> delayµs kuppelnDelayµs >> digitalWrite kupplungsPin LOW)
        ("Kuppeln (" ++ (show kupplungsPin) ++ ")")

-- | Zusammenfassung von Einzel-Elementen. Weichen haben eine vorgegebene Richtung.
data Wegstrecke = Wegstrecke {
    wsName :: String,
    wsBahngeschwindigkeiten :: [Bahngeschwindigkeit],
    wsStreckenabschnitte :: [Streckenabschnitt],
    wsWeichenRichtungen :: [(Weiche, Richtung)],
    wsKupplungen :: [Kupplung]}
                    deriving (Eq)

instance Show Wegstrecke where
    show :: Wegstrecke -> String
    show    (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen}) = Language.wegstrecke <:> Language.name <=> wsName
                                                                                                                    <^> Language.bahngeschwindigkeiten <=> show wsBahngeschwindigkeiten
                                                                                                                    <^> Language.streckenabschnitte <=> show wsStreckenabschnitte
                                                                                                                    <^> Language.weichen <=> "[" <> foldl (\acc (weiche, richtung) -> (if null acc then id else (acc <^>)) $ show weiche <°> show richtung) "" wsWeichenRichtungen <> "]"
                                                                                                                    <^> Language.kupplungen <=> show wsKupplungen

instance StreckenObjekt Wegstrecke where
    zugtyp :: Wegstrecke -> Zugtyp
    zugtyp wegstrecke@(Wegstrecke {wsBahngeschwindigkeiten=(h:t)})    = case zugtyp h of
        Undefiniert -> zugtyp $ wegstrecke {wsBahngeschwindigkeiten=t}
        zugtypWert  -> zugtypWert
    zugtyp wegstrecke@(Wegstrecke {wsStreckenabschnitte=(h:t)})       = case zugtyp h of
        Undefiniert -> zugtyp $ wegstrecke {wsStreckenabschnitte=t}
        zugtypWert  -> zugtypWert
    zugtyp wegstrecke@(Wegstrecke {wsWeichenRichtungen=(h:t)})        = case zugtyp $ fst h of
        Undefiniert -> zugtyp $ wegstrecke {wsWeichenRichtungen=t}
        zugtypWert  -> zugtypWert
    zugtyp wegstrecke@(Wegstrecke {wsKupplungen=(h:t)})               = case zugtyp h of
        Undefiniert -> zugtyp $ wegstrecke {wsKupplungen=t}
        zugtypWert  -> zugtypWert
    zugtyp _wegstrecke                                              = Undefiniert
    pins :: Wegstrecke -> [Pin]
    pins (Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen})    = join $ (map pins wsBahngeschwindigkeiten) <> (map pins wsStreckenabschnitte) <> (map pins (map fst wsWeichenRichtungen)) <> (map pins wsKupplungen)
    getName :: Wegstrecke -> String
    getName (Wegstrecke {wsName}) = wsName

instance BahngeschwindigkeitKlasse Wegstrecke where
    geschwindigkeit :: Wegstrecke -> Natural -> PinMapIO ()
    geschwindigkeit (Wegstrecke {wsBahngeschwindigkeiten}) wert mvarPinMap = mapM_ (\bahngeschwindigkeit -> geschwindigkeit bahngeschwindigkeit wert mvarPinMap) wsBahngeschwindigkeiten
    umdrehen :: Wegstrecke -> Maybe Fahrtrichtung -> PinMapIO ()
    umdrehen    (Wegstrecke {wsBahngeschwindigkeiten})    maybeFahrtrichtung  mvarPinMap  = mapM_ (\bahngeschwindigkeit -> umdrehen bahngeschwindigkeit maybeFahrtrichtung mvarPinMap) wsBahngeschwindigkeiten

instance StreckenabschnittKlasse Wegstrecke where
    strom :: Wegstrecke -> Bool -> PinMapIO ()
    strom   (Wegstrecke {wsStreckenabschnitte})   an  mvarPinMap  = mapM_ (flip (flip strom an) mvarPinMap) wsStreckenabschnitte

instance KupplungKlasse Wegstrecke where
    kuppeln :: Wegstrecke -> PinMapIO ()
    kuppeln (Wegstrecke {wsKupplungen})  mvarPinMap  = mapM_ (flip kuppeln mvarPinMap) wsKupplungen

class (StreckenObjekt w, BahngeschwindigkeitKlasse w, StreckenabschnittKlasse w, KupplungKlasse w) => WegstreckeKlasse w where
    einstellen :: w -> PinMapIO ()
    {-# MINIMAL einstellen #-}

instance WegstreckeKlasse Wegstrecke where
    einstellen :: Wegstrecke -> PinMapIO ()
    einstellen  (Wegstrecke {wsWeichenRichtungen})    mvarPinMap  = mapM_ (\(weiche, richtung) -> stellen weiche richtung mvarPinMap) wsWeichenRichtungen

-- | Ausführen einer IO-Aktion, bzw. Ausgabe eines Strings, abhängig vom Kommandozeilen-Argument
befehlAusführen :: IO () -> String -> IO ()
befehlAusführen ioAction ersatzNachricht = do
    (Options {printCmd}) <- getOptions
    if printCmd
        then putStrLn ersatzNachricht
        else ioAction