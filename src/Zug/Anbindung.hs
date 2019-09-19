{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description : Low-Level-Definition der unterstützen Aktionen auf Anschluss-Ebene.
-}
module Zug.Anbindung (
                    -- * Anschluss-Repräsentation
                    Anschluss(), vonPinGpio, zuPinGpio, vonPCF8574Port, zuPCF8574Port,
                    PwmMap, pwmMapEmpty, PwmMapT, I2CMap, i2cMapEmpty, I2CMapT,
                    runPwmMapT, liftI2CMapT, runI2CMapT,
                    Value(..), alleValues,
                    Pin(), vonPin, zuPin, pwmMöglich, clockMöglich, PwmValueUnmodifiziert,
                    -- * Strecken-Objekte
                    StreckenObjekt(..), StreckenAtom(..),
                    -- ** Bahngeschwindigkeiten
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..), pwmEingabeMaximal, erhaltePwmWertVoll, erhaltePWMWertReduziert,
                    -- ** Streckenabschnitte
                    Streckenabschnitt(..), StreckenabschnittKlasse(..),
                    -- ** Weichen
                    Weiche(..), WeicheKlasse(..),
                    -- ** Kupplungen
                    Kupplung(..), KupplungKlasse(..),
                    -- ** Wegstrecken
                    Wegstrecke(..), WegstreckeKlasse(..),
                    -- * Allgemeine Funktionen
                    warteµs) where

-- Bibliotheken
import Control.Monad (join)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import Numeric.Natural (Natural)
import System.Hardware.WiringPi (Pin(..), PwmValue(), Mode(..),
                                pinMode, pwmSetRange, pwmWrite)
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen (Zugtyp(..), Strom(..), Fahrtrichtung(..), Richtung(..))
import Zug.Options (Options(..), PWM(..), getOptions)
import qualified Zug.Language as Language
import Zug.Language (showText, (<^>), (<=>), (<->), (<|>), (<:>), (<°>))
import Zug.Anbindung.Anschluss (Anschluss(..), vonPin, zuPin, vonPinGpio, zuPinGpio, vonPCF8574Port, zuPCF8574Port,
                                anschlussWrite, Value(..), I2CMapT, I2CMap, i2cMapEmpty, runI2CMapT, forkI2CMapT, warteµs)
import Zug.Anbindung.SoftwarePWM (PwmMapT, PwmMap, pwmMapEmpty, pwmGrenze, pwmSoftwareSetzteWert, liftI2CMapT, runPwmMapT, forkPwmMapT)

-- | Alle Möglichen Werte von 'Value'
alleValues :: NonEmpty Value
alleValues = NE.fromList [minBound..maxBound]

-- * Test-Funktionen, ob Anschlusss bestimmte Funktionen unterstützen
-- | Unterstützt der 'Pin'
-- >'pinMode' pin 'PWM_OUTPUT'
pwmMöglich :: Pin -> Bool
pwmMöglich = flip elem [Wpi 1, Wpi 23, Wpi 24, Wpi 26]

-- | Unterstützt der 'Pin'
-- >'pinMode' pin 'GPIO_CLOCK'
clockMöglich :: Pin -> Bool
clockMöglich = flip elem [Wpi 7, Wpi 21, Wpi 22, Wpi 29]

-- * PWM-Funktion
-- | 'pwmWrite' mit alternativer Software-basierter PWM-Funktion
pwmSetzeWert :: (StreckenAtom s) => s -> Anschluss -> PwmValueUnmodifiziert -> PwmMapT IO ()
pwmSetzeWert s anschluss pwmValue = do
    Options {pwm} <- getOptions
    case zuPin anschluss of
        (Just pin) | (pwm == HardwarePWM) && pwmMöglich pin
            -> liftIO $ do
                pinMode pin PWM_OUTPUT
                pwmSetRange pwmGrenze
                pwmWrite pin $ pwmValueModifiziert s pwmValue
        _otherwise
            -> pwmSoftwareSetzteWert anschluss pwmFrequenzHzNormal $ pwmValueModifiziert s pwmValue

-- | Erzeuge PWM-Funktion für einen Servo-Motor
--   Nutze SoftwarePWM für eine konstante Frequenz (sonst abhängig pwmGrenze und pwmValue)
pwmServo :: (StreckenAtom s) => s -> Anschluss -> Natural -> PwmMapT IO ()
pwmServo s anschluss = pwmSoftwareSetzteWert anschluss pwmFrequenzHzServo . pwmValueModifiziert s . erhaltePwmWertVoll

-- | newtype auf 'PwmValue' um ein noch bevorstehendes modifizieren bzgl. fließend-Value zu signalisieren
newtype PwmValueUnmodifiziert = PwmValueUnmodifiziert PwmValue

-- | Berechne den PWM-Wert abhängig davon, bei welchen 'Anschluss'-Output ('HIGH'/'LOW') der Strom fließt.
pwmValueModifiziert :: (StreckenAtom s) => s -> PwmValueUnmodifiziert -> PwmValue
pwmValueModifiziert s (PwmValueUnmodifiziert pwmValue) = case fließend s of
    HIGH    -> pwmValue
    LOW     -> pwmGrenze - pwmValue

-- ** Frequenzen
-- | 50 Hz Frequenz; Standard-Wert von Servo-Motoren
pwmFrequenzHzServo :: Natural
pwmFrequenzHzServo  = 50

-- | Normale PWM-Frequenz
pwmFrequenzHzNormal :: Natural
pwmFrequenzHzNormal = 500

-- | Erhalte PWMValue ausgehend von einem Wert zwischen 0 und 'pwmEingabeMaximal'.
erhaltePwmWert :: (Integral i) => Natural -> i -> PwmValueUnmodifiziert
erhaltePwmWert pwmGrenzeMax wert = PwmValueUnmodifiziert $ fromIntegral ergebnis
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
erhaltePwmWertVoll :: (Integral i) => i -> PwmValueUnmodifiziert
erhaltePwmWertVoll = erhaltePwmWert pwmGrenzeVoll

-- | Nutze einen reduzierten Bereich der pwmGrenze (maximal 16V von 24V Maximalspannung)
erhaltePWMWertReduziert :: (Integral i) => i -> PwmValueUnmodifiziert
erhaltePWMWertReduziert = erhaltePwmWert pwmGrenzeReduziert

-- | µs in a second
µsInS :: Natural
µsInS = 1000 * µsInms

-- | µs in a millisecond
µsInms :: Natural
µsInms = 1000

-- * Repräsentation von StreckenObjekten
-- | Klassen-Definitionen
class StreckenObjekt s where
    anschlüsse :: s -> [Anschluss]
    erhalteName :: s -> Text
    {-# MINIMAL anschlüsse, erhalteName #-}

-- | Eine Klasse für alle Typen, die direkt mit wiringPi interagieren.
class StreckenAtom s where
    fließend :: s -> Value
    fließend = erhalteValue Fließend
    gesperrt :: s -> Value
    gesperrt s = case fließend s of
        HIGH    -> LOW
        LOW     -> HIGH
    erhalteValue :: Strom -> s -> Value
    erhalteValue    Fließend    = fließend
    erhalteValue    Gesperrt    = gesperrt
    {-# MINIMAL fließend | erhalteValue #-}

-- | Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung
data Bahngeschwindigkeit (z :: Zugtyp) where
    LegoBahngeschwindigkeit :: {
        bglName :: Text,
        bglFließend :: Value,
        bglGeschwindigkeitsAnschluss :: Anschluss,
        bglFahrtrichtungsAnschluss :: Anschluss}
            -> Bahngeschwindigkeit 'Lego
    MärklinBahngeschwindigkeit :: {
        bgmName :: Text,
        bgmFließend :: Value,
        bgmGeschwindigkeitsAnschluss :: Anschluss}
            -> Bahngeschwindigkeit 'Märklin

deriving instance Eq (Bahngeschwindigkeit z)

instance Show (Bahngeschwindigkeit z) where
    show :: Bahngeschwindigkeit z -> String
    show    (LegoBahngeschwindigkeit {bglName, bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss})
        = Language.lego <-> Language.bahngeschwindigkeit <:>
            Language.name <=> unpack bglName <^>
            Language.geschwindigkeit <-> Language.pin <=> show bglGeschwindigkeitsAnschluss <^>
            Language.fahrtrichtung <-> Language.pin <=> show bglFahrtrichtungsAnschluss
    show    (MärklinBahngeschwindigkeit {bgmName, bgmGeschwindigkeitsAnschluss})
        = Language.märklin <-> Language.bahngeschwindigkeit <:>
            Language.name <=> unpack bgmName <^>
            Language.geschwindigkeit <-> Language.pin <=> show bgmGeschwindigkeitsAnschluss

instance StreckenObjekt (Bahngeschwindigkeit z) where
    anschlüsse :: Bahngeschwindigkeit z -> [Anschluss]
    anschlüsse  (LegoBahngeschwindigkeit {bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss})
        = [bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss]
    anschlüsse  (MärklinBahngeschwindigkeit {bgmGeschwindigkeitsAnschluss})
        = [bgmGeschwindigkeitsAnschluss]
    erhalteName :: Bahngeschwindigkeit z -> Text
    erhalteName (LegoBahngeschwindigkeit {bglName})    = bglName
    erhalteName (MärklinBahngeschwindigkeit {bgmName}) = bgmName

instance StreckenAtom (Bahngeschwindigkeit z) where
    fließend :: Bahngeschwindigkeit z -> Value
    fließend    (LegoBahngeschwindigkeit {bglFließend})     = bglFließend
    fließend    (MärklinBahngeschwindigkeit {bgmFließend})  = bgmFließend

-- | Sammel-Klasse für 'Bahngeschwindigkeit'-artige Typen
class (StreckenObjekt b) => BahngeschwindigkeitKlasse b where
    -- | Geschwindigkeit einstellen (akzeptiere Werte von 0 bis 100)
    geschwindigkeit :: b -> Natural -> PwmMapT IO ()
    -- | Gebe allen Zügen den Befehl zum Umdrehen/in einer bestimmen Richtung zu fahren
    umdrehen :: b -> Maybe Fahrtrichtung -> PwmMapT IO ()
    {-# MINIMAL geschwindigkeit, umdrehen #-}

-- | Zeit, die Strom beim Umdrehen einer Märklin-Bahngeschwindigkeit anliegt
umdrehenZeitµs :: Natural
umdrehenZeitµs = 250 * µsInms

instance BahngeschwindigkeitKlasse (Bahngeschwindigkeit z) where
    geschwindigkeit :: Bahngeschwindigkeit z -> Natural -> PwmMapT IO ()
    geschwindigkeit
        bg@(LegoBahngeschwindigkeit {bglGeschwindigkeitsAnschluss})
        geschwindigkeit
            = befehlAusführen
                (pwmSetzeWert bg bglGeschwindigkeitsAnschluss $ erhaltePwmWertVoll geschwindigkeit)
                ("Geschwindigkeit (" <> showText bglGeschwindigkeitsAnschluss <> ")->" <> showText geschwindigkeit)
    geschwindigkeit
        bg@(MärklinBahngeschwindigkeit {bgmGeschwindigkeitsAnschluss})
        geschwindigkeit
            = befehlAusführen
                (pwmSetzeWert bg bgmGeschwindigkeitsAnschluss $ erhaltePWMWertReduziert geschwindigkeit)
                ("Geschwindigkeit (" <> showText bgmGeschwindigkeitsAnschluss <> ")->" <> showText geschwindigkeit)
    umdrehen :: Bahngeschwindigkeit z -> Maybe Fahrtrichtung -> PwmMapT IO ()
    umdrehen
        bg@(LegoBahngeschwindigkeit {bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss})
        (Just fahrtrichtung)
            = befehlAusführen
                (umdrehenAux bg bglGeschwindigkeitsAnschluss [
                    liftI2CMapT $ anschlussWrite bglFahrtrichtungsAnschluss $ (if (fahrtrichtung == Vorwärts) then fließend else gesperrt) bg])
                ("Umdrehen (" <> showText bglGeschwindigkeitsAnschluss <^> showText bglFahrtrichtungsAnschluss <> ")->" <> showText fahrtrichtung)
    umdrehen
        bahngeschwindigkeit@(LegoBahngeschwindigkeit {})
        (Nothing)
            = umdrehen bahngeschwindigkeit (Just Vorwärts)
    umdrehen
        bg@(MärklinBahngeschwindigkeit {bgmGeschwindigkeitsAnschluss})
        _maybeRichtung
            = befehlAusführen
                (umdrehenAux bg bgmGeschwindigkeitsAnschluss [
                    pwmSetzeWert bg bgmGeschwindigkeitsAnschluss $ PwmValueUnmodifiziert pwmGrenze,
                    warteµs umdrehenZeitµs,
                    pwmSetzeWert bg bgmGeschwindigkeitsAnschluss $ PwmValueUnmodifiziert 0])
                ("Umdrehen (" <> showText bgmGeschwindigkeitsAnschluss <> ")")

umdrehenAux :: (StreckenAtom s) => s -> Anschluss -> [PwmMapT IO ()] -> PwmMapT IO ()
umdrehenAux s geschwindigkeitsAnschluss umdrehenAktionen = do
    pwmSetzeWert s geschwindigkeitsAnschluss $ PwmValueUnmodifiziert 0
    warteµs umdrehenZeitµs
    sequence_ umdrehenAktionen
    warteµs umdrehenZeitµs

-- | Steuere die Stromzufuhr einer Schiene
data Streckenabschnitt = Streckenabschnitt {stName :: Text, stFließend :: Value, stromAnschluss::Anschluss}
                            deriving (Eq)

instance Show Streckenabschnitt where
    show :: Streckenabschnitt -> String
    show (Streckenabschnitt {stName, stromAnschluss})
        = Language.streckenabschnitt <:>
            Language.name <=> unpack stName <^>
            Language.strom <-> Language.pin <=> show stromAnschluss

instance StreckenObjekt Streckenabschnitt where
    anschlüsse :: Streckenabschnitt -> [Anschluss]
    anschlüsse (Streckenabschnitt {stromAnschluss}) = [stromAnschluss]
    erhalteName :: Streckenabschnitt -> Text
    erhalteName (Streckenabschnitt {stName}) = stName

instance StreckenAtom Streckenabschnitt where
    fließend :: Streckenabschnitt -> Value
    fließend = stFließend

-- | Sammel-Klasse für 'Streckenabschnitt'-artige Typen
class (StreckenObjekt s) => StreckenabschnittKlasse s where
    -- | Strom ein-/ausschalten
    strom :: s -> Strom -> I2CMapT IO ()
    {-# MINIMAL strom #-}

instance StreckenabschnittKlasse Streckenabschnitt where
    strom :: Streckenabschnitt -> Strom -> I2CMapT IO ()
    strom st@(Streckenabschnitt {stromAnschluss}) an = befehlAusführen
        (anschlussWrite stromAnschluss $ erhalteValue an st)
        ("Strom (" <> showText stromAnschluss <> ")->" <> showText an)

-- | Stellen einer 'Weiche'
data Weiche (z :: Zugtyp) where
    LegoWeiche :: {
        welName :: Text,
        welFließend :: Value,
        welRichtungsAnschluss :: Anschluss,
        welRichtungen :: (Richtung,Richtung)}
            -> Weiche 'Lego
    MärklinWeiche :: {
        wemName :: Text,
        wemFließend :: Value,
        wemRichtungsAnschluss :: NonEmpty (Richtung, Anschluss)}
            -> Weiche 'Märklin

deriving instance Eq (Weiche z)

instance Show (Weiche z) where
    show :: Weiche z -> String
    show    (LegoWeiche {welName, welRichtungsAnschluss, welRichtungen=(richtung1, richtung2)})
        = Language.lego <-> Language.weiche <:>
            Language.name <=> unpack welName <^>
            Language.richtung <-> Language.pin <=> show welRichtungsAnschluss <^>
            Language.richtungen <=> show richtung1 <|> show richtung2
    show    (MärklinWeiche {wemName, wemRichtungsAnschluss})
        = Language.märklin <-> Language.weiche <:>
            Language.name <=> unpack wemName <^>
            foldl (\acc (pin, richtung) -> acc <^> show richtung <=> show pin) "" wemRichtungsAnschluss

instance StreckenObjekt (Weiche z) where
    anschlüsse :: Weiche z -> [Anschluss]
    anschlüsse  (LegoWeiche {welRichtungsAnschluss})       = [welRichtungsAnschluss]
    anschlüsse  (MärklinWeiche {wemRichtungsAnschluss})   = map snd $ NE.toList wemRichtungsAnschluss
    erhalteName :: Weiche z -> Text
    erhalteName (LegoWeiche {welName})     = welName
    erhalteName (MärklinWeiche {wemName})  = wemName

instance StreckenAtom (Weiche z) where
    fließend :: Weiche z -> Value
    fließend    (LegoWeiche {welFließend})      = welFließend
    fließend    (MärklinWeiche {wemFließend})   = wemFließend

-- | Sammel-Klasse für 'Weiche'n-artige Typen
class (StreckenObjekt w) => WeicheKlasse w where
    -- | Weiche einstellen
    stellen :: w -> Richtung -> PwmMapT IO ()
    -- | Überprüfe, ob Weiche eine Richtung unterstützt
    hatRichtung :: w -> Richtung -> Bool
    hatRichtung weiche richtung = elem richtung $ erhalteRichtungen weiche
    -- | Erhalte alle Richtungen einer Weiche
    erhalteRichtungen :: w -> NonEmpty Richtung
    {-# MINIMAL stellen, erhalteRichtungen #-}

-- | Zeit, die Strom beim Stellen einer Märklin-Weiche anliegt
weicheZeitµs :: Natural
weicheZeitµs = 500 * µsInms

instance WeicheKlasse (Weiche z) where
    stellen :: Weiche z -> Richtung -> PwmMapT IO ()
    stellen we@(LegoWeiche {welRichtungsAnschluss, welRichtungen})  richtung
        | richtung == fst welRichtungen = befehlAusführen
            (pwmServo we welRichtungsAnschluss 25 >> warteµs weicheZeitµs >> pwmServo we welRichtungsAnschluss 0)
            ("Stellen (" <> showText welRichtungsAnschluss <> ") -> " <> showText richtung)
        | richtung == snd welRichtungen = befehlAusführen
            (pwmServo we welRichtungsAnschluss 75 >> warteµs weicheZeitµs >> pwmServo we welRichtungsAnschluss 0)
            ("stellen (" <> showText welRichtungsAnschluss <> ") -> " <> showText richtung)
        | otherwise                     = pure ()
    stellen we@(MärklinWeiche {wemRichtungsAnschluss})              richtung = liftI2CMapT $ befehlAusführen
        richtungStellen
        ("Stellen (" <> showText (getRichtungsAnschluss richtung $ NE.toList wemRichtungsAnschluss) <> ") -> " <> showText richtung)
            where
                richtungStellen :: I2CMapT IO ()
                richtungStellen = case getRichtungsAnschluss richtung $ NE.toList wemRichtungsAnschluss of
                    (Nothing)           -> pure ()
                    (Just richtungsAnschluss) -> do
                        anschlussWrite richtungsAnschluss $ fließend we
                        warteµs weicheZeitµs
                        anschlussWrite richtungsAnschluss $ gesperrt we
                getRichtungsAnschluss :: Richtung -> [(Richtung, Anschluss)] -> Maybe Anschluss
                getRichtungsAnschluss _richtung   []  = Nothing
                getRichtungsAnschluss richtung    ((ersteRichtung, ersterAnschluss):andereRichtungen)
                    | richtung == ersteRichtung = Just ersterAnschluss
                    | otherwise                 = getRichtungsAnschluss richtung andereRichtungen
    hatRichtung :: Weiche z -> Richtung -> Bool
    hatRichtung (LegoWeiche {welRichtungen=(erste, zweite)})    richtung
        = (erste == richtung) || (zweite == richtung)
    hatRichtung (MärklinWeiche {wemRichtungsAnschluss})         richtung
        = any (\(richtung0, _pin0) -> (richtung0 == richtung))  wemRichtungsAnschluss
    erhalteRichtungen :: Weiche z -> NonEmpty Richtung
    erhalteRichtungen   (LegoWeiche {welRichtungen=(richtung1, richtung2)}) = richtung1 :| [richtung2]
    erhalteRichtungen   (MärklinWeiche {wemRichtungsAnschluss})             = fst <$> wemRichtungsAnschluss

-- | Kontrolliere, wann Wagons über eine Kupplungs-Schiene abgekuppelt werden
data Kupplung = Kupplung {kuName :: Text, kuFließend :: Value, kupplungsAnschluss::Anschluss}
                    deriving (Eq)

instance Show Kupplung where
    show :: Kupplung -> String
    show (Kupplung {kuName, kupplungsAnschluss})
        = Language.kupplung <:>
            Language.name <=> unpack kuName <^>
            Language.kupplung <-> Language.pin <=> show kupplungsAnschluss

instance StreckenObjekt Kupplung where
    anschlüsse :: Kupplung -> [Anschluss]
    anschlüsse (Kupplung {kupplungsAnschluss}) = [kupplungsAnschluss]
    erhalteName :: Kupplung -> Text
    erhalteName (Kupplung {kuName}) = kuName

instance StreckenAtom Kupplung where
    fließend :: Kupplung -> Value
    fließend = kuFließend

-- | Sammel-Klasse für 'Kupplung'-artige Typen
class (StreckenObjekt k) => KupplungKlasse k where
    -- | Kupplung betätigen
    kuppeln :: k -> I2CMapT IO ()
    {-# MINIMAL kuppeln #-}

-- | Zeit, die Strom beim Kuppeln anliegt
kuppelnZeitµs :: Natural
kuppelnZeitµs = µsInS

instance KupplungKlasse Kupplung where
    kuppeln :: Kupplung -> I2CMapT IO ()
    kuppeln ku@(Kupplung {kupplungsAnschluss}) = befehlAusführen
        (anschlussWrite kupplungsAnschluss (fließend ku) >> warteµs kuppelnZeitµs >> anschlussWrite kupplungsAnschluss (gesperrt ku))
        ("Kuppeln (" <> showText kupplungsAnschluss <> ")")

-- | Zusammenfassung von Einzel-Elementen. Weichen haben eine vorgegebene Richtung.
data Wegstrecke (z :: Zugtyp) = Wegstrecke {
    wsName :: Text,
    wsBahngeschwindigkeiten :: [Bahngeschwindigkeit z],
    wsStreckenabschnitte :: [Streckenabschnitt],
    wsWeichenRichtungen :: [(Weiche z, Richtung)],
    wsKupplungen :: [Kupplung]}
                    deriving (Eq)

instance Show (Wegstrecke z) where
    show :: Wegstrecke z -> String
    show    (Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen})
        = Language.wegstrecke <:> Language.name <=> unpack wsName
        <^> Language.bahngeschwindigkeiten <=> show wsBahngeschwindigkeiten
        <^> Language.streckenabschnitte <=> show wsStreckenabschnitte
        <^> Language.weichen
            <=> "[" <> foldl (\acc (weiche, richtung) -> (if null acc then id else (acc <^>)) $ show weiche <°> show richtung) "" wsWeichenRichtungen <> "]"
        <^> Language.kupplungen <=> show wsKupplungen

instance StreckenObjekt (Wegstrecke z) where
    anschlüsse :: Wegstrecke z -> [Anschluss]
    anschlüsse (Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen})
        = join $ (map anschlüsse wsBahngeschwindigkeiten)
                <> (map anschlüsse wsStreckenabschnitte)
                <> (map anschlüsse (map fst wsWeichenRichtungen))
                <> (map anschlüsse wsKupplungen)
    erhalteName :: Wegstrecke z -> Text
    erhalteName (Wegstrecke {wsName}) = wsName

instance BahngeschwindigkeitKlasse (Wegstrecke z) where
    geschwindigkeit :: Wegstrecke z -> Natural -> PwmMapT IO ()
    geschwindigkeit (Wegstrecke {wsBahngeschwindigkeiten}) wert = mapM_ (forkPwmMapT . flip geschwindigkeit wert) wsBahngeschwindigkeiten
    umdrehen :: Wegstrecke z -> Maybe Fahrtrichtung -> PwmMapT IO ()
    umdrehen (Wegstrecke {wsBahngeschwindigkeiten}) maybeFahrtrichtung = mapM_ (forkPwmMapT . flip umdrehen maybeFahrtrichtung) wsBahngeschwindigkeiten

instance StreckenabschnittKlasse (Wegstrecke z) where
    strom :: Wegstrecke z -> Strom -> I2CMapT IO ()
    strom (Wegstrecke {wsStreckenabschnitte}) an = mapM_ (forkI2CMapT . flip strom an) wsStreckenabschnitte

instance KupplungKlasse (Wegstrecke z) where
    kuppeln :: Wegstrecke z -> I2CMapT IO ()
    kuppeln (Wegstrecke {wsKupplungen}) = mapM_ (forkI2CMapT . kuppeln) wsKupplungen

-- | Sammel-Klasse für 'Wegstrecke'n-artige Typen
class (StreckenObjekt w, BahngeschwindigkeitKlasse w, StreckenabschnittKlasse w, KupplungKlasse w) => WegstreckeKlasse w where
    einstellen :: w -> PwmMapT IO ()
    {-# MINIMAL einstellen #-}

instance WegstreckeKlasse (Wegstrecke z) where
    einstellen :: Wegstrecke z -> PwmMapT IO ()
    einstellen (Wegstrecke {wsWeichenRichtungen}) = mapM_ (forkPwmMapT . uncurry stellen) wsWeichenRichtungen

-- | Ausführen einer IO-Aktion, bzw. Ausgabe eines Strings, abhängig vom Kommandozeilen-Argument
befehlAusführen :: (MonadIO m) => m () -> Text -> m ()
befehlAusführen ioAction ersatzNachricht = do
    Options {printCmd} <- getOptions
    if printCmd
        then liftIO $ T.putStrLn ersatzNachricht
        else ioAction