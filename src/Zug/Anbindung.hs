{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

{-|
Description : Low-Level-Definition der unterstützen Aktionen auf Pin-Ebene.
-}
module Zug.Anbindung (
                    -- * Pin-Repräsentation
                    Pin(), zuPin, vonPin, pwmMöglich, clockMöglich, PinMap, pinMapEmpty, PinMapIO, warteµs,
                    -- * Strecken-Objekte
                    StreckenObjekt(..),
                    -- ** Bahngeschwindigkeiten
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..), pwmEingabeMaximal, erhaltePwmWertVoll, erhaltePWMWertReduziert,
                    -- ** Streckenabschnitte
                    Streckenabschnitt(..), StreckenabschnittKlasse(..),
                    -- ** Weichen
                    Weiche(..), WeicheKlasse(..),
                    -- ** Kupplungen
                    Kupplung(..), KupplungKlasse(..),
                    -- ** Wegstrecken
                    Wegstrecke(..), WegstreckeKlasse(..)) where

-- Bibliotheken
import Control.Concurrent (forkIO, MVar)
import Control.Monad (join)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import Numeric.Natural (Natural)
import System.Hardware.WiringPi
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen
import Zug.Options
import qualified Zug.Language as Language
import Zug.Language (showText, (<^>), (<=>), (<->), (<|>), (<:>), (<°>))
import Zug.Anbindung.SoftwarePWM

-- * Repräsentation von Pins
-- | Funktion um ein Pin-Objekt erstellen zu können, ohne WiringPi importieren zu müssen
zuPin :: (Enum n) => n -> Pin
zuPin = Gpio . fromEnum

-- | Funktion um Gpio-Zahl eines Pins zu erhalten, ohne WiringPi importieren zu müssen; Rückgabewert ist 0 falls der Pin nicht existiert
vonPin :: Pin -> Natural
vonPin pin = case (pinToBcmGpio pin) of
    (Just gpio) -> fromIntegral gpio
    (Nothing)   -> 0

-- * Test-Funktionen, ob Pins bestimmte Funktionen unterstützen
-- | Unterstützt der 'Pin'
-- >'pinMode' pin PWM_OUTPUT
pwmMöglich :: Pin -> Bool
pwmMöglich = (`elem` [Wpi 1, Wpi 23, Wpi 24, Wpi 26])

-- | Unterstützt der 'Pin'
-- >'pinMode' pin GPIO_CLOCK
clockMöglich :: Pin -> Bool
clockMöglich = (`elem` [Wpi 7, Wpi 21, Wpi 22, Wpi 29])

-- * PWM-Funktion
-- | pwmWrite mit alternativer Software-basierter PWM-Funktion
pwmSetzeWert :: Pin -> PwmValue -> PinMapIO ()
pwmSetzeWert pin pwmValue mvarPinMap = getOptions >>= \(Options {pwm}) -> case pwm of
    (HardwarePWM) | pwmMöglich pin  -> do
        pinMode pin PWM_OUTPUT
        pwmSetRange pwmGrenze
        pwmWrite pin pwmValue
    _otherwise                  -> pwmSoftwareSetzteWert pin pwmFrequenzHzNormal pwmValue mvarPinMap

-- | Erzeuge PWM-Funktion für einen Servo-Motor
--   Nutze SoftwarePWM für eine konstante Frequenz (sonst abhängig pwmGrenze und pwmValue)
pwmServo :: Pin -> Natural -> PinMapIO ()
pwmServo pin wert = pwmSoftwareSetzteWert pin pwmFrequenzHzServo $ erhaltePwmWertVoll wert

-- ** Frequenzen
-- | 50 Hz Frequenz; Standard-Wert von Servo-Motoren
pwmFrequenzHzServo :: Natural
pwmFrequenzHzServo  = 50

-- | Normale PWM-Frequenz
pwmFrequenzHzNormal :: Natural
pwmFrequenzHzNormal = 250

-- | Erhalte PWMValue ausgehend von einem Wert zwischen 0 und 'pwmEingabeMaximal'.
erhaltePwmWert :: (Integral i) => Natural -> i -> PwmValue
erhaltePwmWert pwmGrenzeMax wert = fromIntegral ergebnis
    where
        {-
            Verwende Natural um Fehler wegen zu kleinem Wertebereich zu vermeiden.
            Multipliziere zuerst alle Werte, bevor sie normaliert werden um Rundungsfehler zu verhindern.
            Möglich, nachdem die Funktion nicht in Perfomance-kritischen Bereichen (und selten) aufgerufen wird.
            Effektivspannung skaliert wie die Wurzel des PwmValue.
            Der Eingabewert wird daher quadriert um linear mit der Effektivspannung zu skalieren.
        -}
        ergebnis :: Natural
        ergebnis = div wertSkaliert (pwmEingabeMaximal * pwmEingabeMaximal)
        wertSkaliert :: Natural
        wertSkaliert = pwmGrenzeMax * wertBegrenzt * wertBegrenzt
        wertBegrenzt :: Natural
        wertBegrenzt = min pwmEingabeMaximal $ fromIntegral wert

-- | Maximaler Eingabewert für 'geschwindigkeit'.
pwmEingabeMaximal :: Natural
pwmEingabeMaximal = 100

-- | Vollständige pwmGrenze als Natural.
pwmGrenzeVoll :: Natural
pwmGrenzeVoll = fromIntegral pwmGrenze

-- | Maximal erlaubter pwmGrenze um eine Effektivspannung von 16V zu erhalten.
pwmGrenzeReduziert :: Natural
pwmGrenzeReduziert = div (pwmGrenzeVoll * spannungFahrt * spannungFahrt) (spannungQuelle * spannungQuelle)
    -- Effektivspannung skaliert wie die Wurzel des PwmValues.
    where
        spannungFahrt :: Natural
        spannungFahrt = 16
        spannungQuelle :: Natural
        spannungQuelle = 25

-- | Nutze komplette pwmGrenze
erhaltePwmWertVoll :: (Integral i) => i -> PwmValue
erhaltePwmWertVoll = erhaltePwmWert pwmGrenzeVoll

-- | Nutze einen reduzierten Bereich der pwmGrenze (maximal 16V von 24V Maximalspannung)
erhaltePWMWertReduziert :: (Integral i) => i -> PwmValue
erhaltePWMWertReduziert = erhaltePwmWert pwmGrenzeReduziert

-- | µs in a second
µsInS :: Natural
µsInS = 1000 * µsInms

-- | µs in a millisecond
µsInms :: Natural
µsInms = 1000

-- | Stelle Strom fließend/gesperrt
stromStellen :: Pin -> Strom -> IO ()
stromStellen pin (Fließend)  = getOptions >>= digitalWrite pin . fließend
stromStellen pin (Gesperrt)  = getOptions >>= digitalWrite pin . (\case {HIGH -> LOW; LOW -> HIGH}) . fließend

-- * Repräsentation von StreckenObjekten
-- | Klassen-Definitionen
class StreckenObjekt s where
    zugtyp :: s -> Zugtyp
    zugtyp  _s  = Undefiniert
    pins :: s -> [Pin]
    erhalteName :: s -> Text
    {-# MINIMAL pins, erhalteName #-}

-- | Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung
data Bahngeschwindigkeit    = LegoBahngeschwindigkeit {bgName :: Text, geschwindigkeitsPin::Pin, fahrtrichtungsPin::Pin}
                            | MärklinBahngeschwindigkeit {bgName :: Text, geschwindigkeitsPin::Pin}
                                        deriving (Eq)

instance Show Bahngeschwindigkeit where
    show :: Bahngeschwindigkeit -> String
    show    (LegoBahngeschwindigkeit {bgName, geschwindigkeitsPin, fahrtrichtungsPin})    = Language.lego <-> Language.bahngeschwindigkeit <:> Language.name <=> unpack bgName <^> Language.geschwindigkeit <-> Language.pin <=> show geschwindigkeitsPin <^> Language.fahrtrichtung <-> Language.pin <=> show fahrtrichtungsPin
    show    (MärklinBahngeschwindigkeit {bgName, geschwindigkeitsPin})                    = Language.märklin <-> Language.bahngeschwindigkeit <:> Language.name <=> unpack bgName <^> Language.geschwindigkeit <-> Language.pin <=> show geschwindigkeitsPin

instance StreckenObjekt Bahngeschwindigkeit where
    zugtyp :: Bahngeschwindigkeit -> Zugtyp
    zugtyp  (LegoBahngeschwindigkeit {})    = Lego
    zugtyp  (MärklinBahngeschwindigkeit {}) = Märklin
    pins :: Bahngeschwindigkeit -> [Pin]
    pins    (LegoBahngeschwindigkeit {geschwindigkeitsPin, fahrtrichtungsPin})  = [geschwindigkeitsPin, fahrtrichtungsPin]
    pins    (MärklinBahngeschwindigkeit {geschwindigkeitsPin})                  = [geschwindigkeitsPin]
    erhalteName :: Bahngeschwindigkeit -> Text
    erhalteName (LegoBahngeschwindigkeit {bgName})    = bgName
    erhalteName (MärklinBahngeschwindigkeit {bgName}) = bgName

-- | Sammel-Klasse für 'Bahngeschwindigkeit'-artige Typen
class (StreckenObjekt b) => BahngeschwindigkeitKlasse b where
    -- | Geschwindigkeit einstellen (akzeptiere Werte von 0 bis 100)
    geschwindigkeit :: b -> Natural -> PinMapIO ()
    -- | Gebe allen Zügen den Befehl zum Umdrehen/in einer bestimmen Richtung zu fahren
    umdrehen :: b -> Maybe Fahrtrichtung -> PinMapIO ()
    {-# MINIMAL geschwindigkeit, umdrehen #-}

-- | Zeit, die Strom beim Umdrehen einer Märklin-Bahngeschwindigkeit anliegt
umdrehenZeitµs :: Natural
umdrehenZeitµs = 250 * µsInms

instance BahngeschwindigkeitKlasse Bahngeschwindigkeit where
    geschwindigkeit :: Bahngeschwindigkeit -> Natural -> PinMapIO ()
    geschwindigkeit (LegoBahngeschwindigkeit {geschwindigkeitsPin})     geschwindigkeit mvarPinMap  = befehlAusführen
        (pwmSetzeWert geschwindigkeitsPin (erhaltePwmWertVoll geschwindigkeit) mvarPinMap)
        ("Geschwindigkeit (" <> showText geschwindigkeitsPin <> ")->" <> showText geschwindigkeit)
    geschwindigkeit (MärklinBahngeschwindigkeit {geschwindigkeitsPin})  geschwindigkeit mvarPinMap  = befehlAusführen
        (pwmSetzeWert geschwindigkeitsPin (erhaltePWMWertReduziert geschwindigkeit) mvarPinMap)
        ("Geschwindigkeit (" <> showText geschwindigkeitsPin <> ")->" <> showText geschwindigkeit)
    umdrehen :: Bahngeschwindigkeit -> Maybe Fahrtrichtung -> PinMapIO ()
    umdrehen (LegoBahngeschwindigkeit {geschwindigkeitsPin, fahrtrichtungsPin}) (Just fahrtrichtung)    mvarPinMap = befehlAusführen
        (umdrehenAux geschwindigkeitsPin mvarPinMap [pwmServo fahrtrichtungsPin (if (fahrtrichtung == Vorwärts) then 100 else 0)])
        ("Umdrehen (" <> showText geschwindigkeitsPin <^> showText fahrtrichtungsPin <> ")->" <> showText fahrtrichtung)
    umdrehen bahngeschwindigkeit@(LegoBahngeschwindigkeit {})                   (Nothing)               mvarPinMap = umdrehen bahngeschwindigkeit (Just Vorwärts) mvarPinMap
    umdrehen (MärklinBahngeschwindigkeit {geschwindigkeitsPin})                 _maybeRichtung  mvarPinMap = befehlAusführen
        (umdrehenAux geschwindigkeitsPin mvarPinMap [pwmSetzeWert geschwindigkeitsPin pwmGrenze, const $ warteµs umdrehenZeitµs, pwmSetzeWert geschwindigkeitsPin 0])
        ("Umdrehen (" <> showText geschwindigkeitsPin <> ")")

umdrehenAux :: Pin -> MVar PinMap -> [PinMapIO ()] -> IO ()
umdrehenAux geschwindigkeitsPin mvarPinMap umdrehenAktionen = do
    pwmSetzeWert geschwindigkeitsPin 0 mvarPinMap
    warteµs umdrehenZeitµs
    mapM_ ($ mvarPinMap) umdrehenAktionen
    warteµs umdrehenZeitµs

-- | Steuere die Stromzufuhr einer Schiene
data Streckenabschnitt = Streckenabschnitt {stName :: Text, stromPin::Pin}
                            deriving (Eq)

instance Show Streckenabschnitt where
    show :: Streckenabschnitt -> String
    show    (Streckenabschnitt {stName, stromPin})    = Language.streckenabschnitt <:> Language.name <=> unpack stName <^> Language.strom <-> Language.pin <=> show stromPin

instance StreckenObjekt Streckenabschnitt where
    pins :: Streckenabschnitt -> [Pin]
    pins (Streckenabschnitt {stromPin})    = [stromPin]
    erhalteName :: Streckenabschnitt -> Text
    erhalteName (Streckenabschnitt {stName})  = stName

-- | Sammel-Klasse für 'Streckenabschnitt'-artige Typen
class (StreckenObjekt s) => StreckenabschnittKlasse s where
    -- | Strom ein-/ausschalten
    strom :: s -> Strom -> MVar PinMap -> IO ()
    {-# MINIMAL strom #-}

instance StreckenabschnittKlasse Streckenabschnitt where
    strom :: Streckenabschnitt -> Strom -> PinMapIO ()
    strom   (Streckenabschnitt {stromPin})  an  _mvarPinMap = befehlAusführen
        (pinMode stromPin OUTPUT >> stromStellen stromPin an)
        ("Strom (" <> showText stromPin <> ")->" <> showText an)

-- | Stellen einer 'Weiche'
data Weiche = LegoWeiche {weName :: Text, richtungsPin :: Pin, richtungen::(Richtung,Richtung)}
            | MärklinWeiche {weName :: Text, richtungsPins :: NonEmpty (Richtung, Pin)}
                    deriving (Eq)

instance Show Weiche where
    show :: Weiche -> String
    show    (LegoWeiche {weName, richtungsPin, richtungen=(richtung1, richtung2)})    = Language.lego <-> Language.weiche <:> Language.name <=> unpack weName <^> Language.richtung <-> Language.pin <=> show richtungsPin <^> Language.richtungen <=> show richtung1 <|> show richtung2
    show    (MärklinWeiche {weName, richtungsPins})                                   = Language.märklin <-> Language.weiche <:> Language.name <=> unpack weName <^> foldl (\acc (pin, richtung) -> acc <^> show richtung <=> show pin) "" richtungsPins

instance StreckenObjekt Weiche where
    zugtyp :: Weiche -> Zugtyp
    zugtyp (LegoWeiche {})      = Lego
    zugtyp (MärklinWeiche {})   = Märklin
    pins :: Weiche -> [Pin]
    pins (LegoWeiche {richtungsPin})        = [richtungsPin]
    pins (MärklinWeiche {richtungsPins})    = map snd $ NE.toList richtungsPins
    erhalteName :: Weiche -> Text
    erhalteName (LegoWeiche {weName})     = weName
    erhalteName (MärklinWeiche {weName})  = weName

-- | Sammel-Klasse für 'Weiche'n-artige Typen
class (StreckenObjekt w) => WeicheKlasse w where
    -- | Weiche einstellen
    stellen :: w -> Richtung -> PinMapIO ()
    -- | Überprüfe, ob Weiche eine Richtung unterstützt
    hatRichtung :: w -> Richtung -> Bool
    hatRichtung weiche richtung = elem richtung $ erhalteRichtungen weiche
    -- | Erhalte alle Richtungen einer Weiche
    erhalteRichtungen :: w -> NonEmpty Richtung
    {-# MINIMAL stellen, erhalteRichtungen #-}

-- | Zeit, die Strom beim Stellen einer Märklin-Weiche anliegt
weicheZeitµs :: Natural
weicheZeitµs = 500 * µsInms

instance WeicheKlasse Weiche where
    stellen :: Weiche -> Richtung -> PinMapIO ()
    stellen (LegoWeiche {richtungsPin, richtungen}) richtung    mvarPinMap  | richtung == fst richtungen    = befehlAusführen
        (pwmServo richtungsPin 25 mvarPinMap >> warteµs weicheZeitµs >> pwmServo richtungsPin 0 mvarPinMap)
        ("Stellen (" <> showText richtungsPin <> ") -> " <> showText richtung)
                                                                            | richtung == snd richtungen    = befehlAusführen
        (pwmServo richtungsPin 75 mvarPinMap >> warteµs weicheZeitµs >> pwmServo richtungsPin 0 mvarPinMap)
        ("stellen (" <> showText richtungsPin <> ") -> " <> showText richtung)
                                                                            | otherwise                     = pure ()
    stellen (MärklinWeiche {richtungsPins})         richtung    _mvarPinMap = befehlAusführen
        richtungStellen
        ("Stellen (" <> showText (getRichtungsPin richtung $ NE.toList richtungsPins) <> ") -> " <> showText richtung)
            where
                richtungStellen :: IO ()
                richtungStellen = case getRichtungsPin richtung $ NE.toList richtungsPins of
                    (Nothing)           -> pure ()
                    (Just richtungsPin) -> pinMode richtungsPin OUTPUT >> stromStellen richtungsPin Fließend >> warteµs weicheZeitµs >> stromStellen richtungsPin Gesperrt
                getRichtungsPin :: Richtung -> [(Richtung, Pin)] -> Maybe Pin
                getRichtungsPin _richtung   []  = Nothing
                getRichtungsPin richtung    ((ersteRichtung, ersterPin):andereRichtungen)
                    | richtung == ersteRichtung = Just ersterPin
                    | otherwise                 = getRichtungsPin richtung andereRichtungen
    hatRichtung :: Weiche -> Richtung -> Bool
    hatRichtung (LegoWeiche {richtungen=(erste, zweite)})   richtung    = (erste == richtung) || (zweite == richtung)
    hatRichtung (MärklinWeiche {richtungsPins})             richtung    = foldr (\(richtung0, _pin0) acc -> acc || (richtung0 == richtung)) False richtungsPins
    erhalteRichtungen :: Weiche -> NonEmpty Richtung
    erhalteRichtungen   (LegoWeiche {richtungen=(richtung1, richtung2)})    = richtung1 :| [richtung2]
    erhalteRichtungen   (MärklinWeiche {richtungsPins})                     = fst <$> richtungsPins

-- | Kontrolliere, wann Wagons über eine Kupplungs-Schiene abgekuppelt werden
data Kupplung = Kupplung {kuName :: Text, kupplungsPin::Pin}
                    deriving (Eq)

instance Show Kupplung where
    show :: Kupplung -> String
    show    (Kupplung {kuName, kupplungsPin}) = Language.kupplung <:> Language.name <=> unpack kuName <^> Language.kupplung <-> Language.pin <=> show kupplungsPin

instance StreckenObjekt Kupplung where
    pins :: Kupplung -> [Pin]
    pins (Kupplung {kupplungsPin})  = [kupplungsPin]
    erhalteName :: Kupplung -> Text
    erhalteName (Kupplung {kuName})   = kuName

-- | Sammel-Klasse für 'Kupplung'-artige Typen
class (StreckenObjekt k) => KupplungKlasse k where
    -- | Kupplung betätigen
    kuppeln :: k -> PinMapIO ()
    {-# MINIMAL kuppeln #-}

-- | Zeit, die Strom beim Kuppeln anliegt
kuppelnZeitµs :: Natural
kuppelnZeitµs = µsInS

instance KupplungKlasse Kupplung where
    kuppeln :: Kupplung -> PinMapIO ()
    kuppeln (Kupplung {kupplungsPin})   _mvarPinMap = befehlAusführen
        (pinMode kupplungsPin OUTPUT >> stromStellen kupplungsPin Fließend >> warteµs kuppelnZeitµs >> stromStellen kupplungsPin Gesperrt)
        ("Kuppeln (" <> showText kupplungsPin <> ")")

-- | Zusammenfassung von Einzel-Elementen. Weichen haben eine vorgegebene Richtung.
data Wegstrecke = Wegstrecke {
    wsName :: Text,
    wsBahngeschwindigkeiten :: [Bahngeschwindigkeit],
    wsStreckenabschnitte :: [Streckenabschnitt],
    wsWeichenRichtungen :: [(Weiche, Richtung)],
    wsKupplungen :: [Kupplung]}
                    deriving (Eq)

instance Show Wegstrecke where
    show :: Wegstrecke -> String
    show    (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen}) = Language.wegstrecke <:> Language.name <=> unpack wsName
                                                                                                                    <^> Language.bahngeschwindigkeiten <=> show wsBahngeschwindigkeiten
                                                                                                                    <^> Language.streckenabschnitte <=> show wsStreckenabschnitte
                                                                                                                    <^> Language.weichen <=> "[" <> foldl (\acc (weiche, richtung) -> (if null acc then id else (acc <^>)) $ show weiche <°> show richtung) "" wsWeichenRichtungen <> "]"
                                                                                                                    <^> Language.kupplungen <=> show wsKupplungen

instance StreckenObjekt Wegstrecke where
    zugtyp :: Wegstrecke -> Zugtyp
    zugtyp wegstrecke@(Wegstrecke {wsBahngeschwindigkeiten=(h:t)})    = case zugtyp h of
        (Undefiniert)   -> zugtyp $ wegstrecke {wsBahngeschwindigkeiten=t}
        zugtypWert      -> zugtypWert
    zugtyp wegstrecke@(Wegstrecke {wsStreckenabschnitte=(h:t)})       = case zugtyp h of
        (Undefiniert)   -> zugtyp $ wegstrecke {wsStreckenabschnitte=t}
        zugtypWert      -> zugtypWert
    zugtyp wegstrecke@(Wegstrecke {wsWeichenRichtungen=(h:t)})        = case zugtyp $ fst h of
        (Undefiniert)   -> zugtyp $ wegstrecke {wsWeichenRichtungen=t}
        zugtypWert      -> zugtypWert
    zugtyp wegstrecke@(Wegstrecke {wsKupplungen=(h:t)})               = case zugtyp h of
        (Undefiniert)   -> zugtyp $ wegstrecke {wsKupplungen=t}
        zugtypWert      -> zugtypWert
    zugtyp _wegstrecke                                              = Undefiniert
    pins :: Wegstrecke -> [Pin]
    pins (Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen})    = join $ (map pins wsBahngeschwindigkeiten) <> (map pins wsStreckenabschnitte) <> (map pins (map fst wsWeichenRichtungen)) <> (map pins wsKupplungen)
    erhalteName :: Wegstrecke -> Text
    erhalteName (Wegstrecke {wsName}) = wsName

instance BahngeschwindigkeitKlasse Wegstrecke where
    geschwindigkeit :: Wegstrecke -> Natural -> PinMapIO ()
    geschwindigkeit (Wegstrecke {wsBahngeschwindigkeiten}) wert mvarPinMap = mapM_ (\bahngeschwindigkeit -> forkIO $ geschwindigkeit bahngeschwindigkeit wert mvarPinMap) wsBahngeschwindigkeiten
    umdrehen :: Wegstrecke -> Maybe Fahrtrichtung -> PinMapIO ()
    umdrehen (Wegstrecke {wsBahngeschwindigkeiten}) maybeFahrtrichtung mvarPinMap = mapM_ (\bahngeschwindigkeit -> forkIO $ umdrehen bahngeschwindigkeit maybeFahrtrichtung mvarPinMap) wsBahngeschwindigkeiten

instance StreckenabschnittKlasse Wegstrecke where
    strom :: Wegstrecke -> Strom -> PinMapIO ()
    strom (Wegstrecke {wsStreckenabschnitte}) an mvarPinMap = mapM_ (\streckenabschnitt -> forkIO $ strom streckenabschnitt an mvarPinMap) wsStreckenabschnitte

instance KupplungKlasse Wegstrecke where
    kuppeln :: Wegstrecke -> PinMapIO ()
    kuppeln (Wegstrecke {wsKupplungen}) mvarPinMap = mapM_ (\kupplung -> forkIO $ kuppeln kupplung mvarPinMap) wsKupplungen

-- | Sammel-Klasse für 'Wegstrecke'n-artige Typen
class (StreckenObjekt w, BahngeschwindigkeitKlasse w, StreckenabschnittKlasse w, KupplungKlasse w) => WegstreckeKlasse w where
    einstellen :: w -> PinMapIO ()
    {-# MINIMAL einstellen #-}

instance WegstreckeKlasse Wegstrecke where
    einstellen :: Wegstrecke -> PinMapIO ()
    einstellen (Wegstrecke {wsWeichenRichtungen}) mvarPinMap = mapM_ (\(weiche, richtung) -> forkIO $ stellen weiche richtung mvarPinMap) wsWeichenRichtungen

-- | Ausführen einer IO-Aktion, bzw. Ausgabe eines Strings, abhängig vom Kommandozeilen-Argument
befehlAusführen :: IO () -> Text -> IO ()
befehlAusführen ioAction ersatzNachricht = do
    (Options {printCmd}) <- getOptions
    if printCmd
        then T.putStrLn ersatzNachricht
        else ioAction