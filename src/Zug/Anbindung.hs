{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description : Low-Level-Definition der unterstützen Aktionen auf Anschluss-Ebene.
-}
module Zug.Anbindung (
    -- * Anschluss-Repräsentation
    Anschluss(..), PCF8574Port(..), PCF8574(..), PCF8574Variant(..),
    vonPinGpio, zuPinGpio, vonPCF8574Port, zuPCF8574Port,
    PwmMap, pwmMapEmpty, MitPwmMap(..), PwmReader(..),
    I2CMap, i2cMapEmpty, MitI2CMap(..), I2CReader(..),
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
    -- * Wartezeit
    warte, Wartezeit(..), addition, differenz, multiplizieren, dividieren) where

-- Bibliotheken
import Control.Monad (join)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Numeric.Natural (Natural)
import System.Hardware.WiringPi (Pin(..), PwmValue(), Mode(..),
                                pinMode, pwmSetRange, pwmWrite)
-- Abhängigkeiten von anderen Modulen
import Zug.Enums (Zugtyp(..), ZugtypEither(..), Strom(..), Fahrtrichtung(..), Richtung(..))
import Zug.Options (Options(..), PWM(..), getOptions)
import qualified Zug.Language as Language
import Zug.Language (Anzeige(..), Sprache(), showText, (<^>), (<=>), (<->), (<|>), (<:>), (<°>))
import Zug.Anbindung.Anschluss (Anschluss(..), PCF8574Port(..), PCF8574(..), PCF8574Variant(..),
                                vonPin, zuPin, vonPinGpio, zuPinGpio, vonPCF8574Port, zuPCF8574Port,
                                anschlussWrite, Value(..), I2CMap, i2cMapEmpty, MitI2CMap(..), I2CReader(..))
import Zug.Anbindung.SoftwarePWM (PwmMap, pwmMapEmpty, MitPwmMap(..), PwmReader(..), pwmGrenze, pwmSoftwareSetzteWert)
import Zug.Anbindung.Wartezeit (warte, Wartezeit(..), addition, differenz, multiplizieren, dividieren)

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
pwmSetzeWert :: (StreckenAtom s, PwmReader r m, MonadIO m) => s -> Anschluss -> PwmValueUnmodifiziert -> m ()
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
pwmServo :: (StreckenAtom s, PwmReader r m, MonadIO m) => s -> Anschluss -> Natural -> m ()
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

-- * Repräsentation von StreckenObjekten
-- | Klassen-Definitionen
class StreckenObjekt s where
    anschlüsse :: s -> [Anschluss]
    erhalteName :: s -> Text
    {-# MINIMAL anschlüsse, erhalteName #-}

instance (StreckenObjekt (a 'Märklin), StreckenObjekt (a 'Lego)) => StreckenObjekt (ZugtypEither a) where
    anschlüsse :: ZugtypEither a -> [Anschluss]
    anschlüsse  (ZugtypMärklin a)   = anschlüsse a
    anschlüsse  (ZugtypLego a)      = anschlüsse a
    erhalteName :: ZugtypEither a -> Text
    erhalteName (ZugtypMärklin a)   = erhalteName a
    erhalteName (ZugtypLego a)      = erhalteName a

instance (StreckenObjekt a) => StreckenObjekt (Maybe a) where
    anschlüsse :: Maybe a -> [Anschluss]
    anschlüsse  (Just a)    = anschlüsse a
    anschlüsse  Nothing     = []
    erhalteName (Just a)    = erhalteName a
    erhalteName Nothing     = ""

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

instance (StreckenAtom (a 'Märklin), StreckenAtom (a 'Lego)) => StreckenAtom (ZugtypEither a) where
    fließend :: ZugtypEither a -> Value
    fließend    (ZugtypMärklin a)   = fließend a
    fließend    (ZugtypLego a)      = fließend a

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
deriving instance Show (Bahngeschwindigkeit z)

instance Anzeige (Bahngeschwindigkeit z) where
    anzeige :: Bahngeschwindigkeit z -> Sprache -> Text
    anzeige
        LegoBahngeschwindigkeit {bglName, bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss}
            = Language.lego <-> Language.bahngeschwindigkeit <:>
                Language.name <=> bglName <^>
                Language.geschwindigkeit <-> Language.anschluss <=> bglGeschwindigkeitsAnschluss <^>
                Language.fahrtrichtung <-> Language.anschluss <=> bglFahrtrichtungsAnschluss
    anzeige
        MärklinBahngeschwindigkeit {bgmName, bgmGeschwindigkeitsAnschluss}
            = Language.märklin <-> Language.bahngeschwindigkeit <:>
                Language.name <=> bgmName <^>
                Language.geschwindigkeit <-> Language.anschluss <=> bgmGeschwindigkeitsAnschluss

instance StreckenObjekt (Bahngeschwindigkeit z) where
    anschlüsse :: Bahngeschwindigkeit z -> [Anschluss]
    anschlüsse
        LegoBahngeschwindigkeit {bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss}
            = [bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss]
    anschlüsse
        MärklinBahngeschwindigkeit {bgmGeschwindigkeitsAnschluss}
            = [bgmGeschwindigkeitsAnschluss]
    erhalteName :: Bahngeschwindigkeit z -> Text
    erhalteName
        LegoBahngeschwindigkeit {bglName}
            = bglName
    erhalteName
        MärklinBahngeschwindigkeit {bgmName}
            = bgmName

instance StreckenAtom (Bahngeschwindigkeit z) where
    fließend :: Bahngeschwindigkeit z -> Value
    fließend
        LegoBahngeschwindigkeit {bglFließend}
            = bglFließend
    fließend
        MärklinBahngeschwindigkeit {bgmFließend}
            = bgmFließend

-- | Sammel-Klasse für 'Bahngeschwindigkeit'-artige Typen
class (StreckenObjekt (b 'Märklin), StreckenObjekt (b 'Lego)) => BahngeschwindigkeitKlasse b where
    -- | Geschwindigkeit einstellen (akzeptiere Werte von 0 bis 100)
    geschwindigkeit :: (PwmReader r m, MonadIO m) => b z -> Natural -> m ()
    -- | Gebe allen Zügen den Befehl zum Umdrehen
    umdrehen :: (PwmReader r m, MonadIO m) => b 'Märklin -> m ()
    -- | Gebe allen Zugen den Befehl in einer bestimmen Richtung zu fahren
    fahrtrichtungEinstellen :: (PwmReader r m, MonadIO m) => b 'Lego -> Fahrtrichtung -> m ()
    {-# MINIMAL geschwindigkeit, umdrehen, fahrtrichtungEinstellen #-}

-- | Zeit, die Strom beim Umdrehen einer Märklin-Bahngeschwindigkeit anliegt
umdrehenZeit :: Wartezeit
umdrehenZeit = MilliSekunden 250

instance BahngeschwindigkeitKlasse Bahngeschwindigkeit where
    geschwindigkeit :: (PwmReader r m, MonadIO m) => Bahngeschwindigkeit z -> Natural -> m ()
    geschwindigkeit
        bg@LegoBahngeschwindigkeit {bglGeschwindigkeitsAnschluss}
        geschwindigkeit
            = befehlAusführen
                (pwmSetzeWert bg bglGeschwindigkeitsAnschluss $ erhaltePwmWertVoll geschwindigkeit)
                ("Geschwindigkeit (" <> showText bglGeschwindigkeitsAnschluss <> ")->" <> showText geschwindigkeit)
    geschwindigkeit
        bg@MärklinBahngeschwindigkeit {bgmGeschwindigkeitsAnschluss}
        geschwindigkeit
            = befehlAusführen
                (pwmSetzeWert bg bgmGeschwindigkeitsAnschluss $ erhaltePWMWertReduziert geschwindigkeit)
                ("Geschwindigkeit (" <> showText bgmGeschwindigkeitsAnschluss <> ")->" <> showText geschwindigkeit)
    umdrehen :: (PwmReader r m, MonadIO m) => Bahngeschwindigkeit 'Märklin -> m ()
    umdrehen
        bg@MärklinBahngeschwindigkeit {bgmGeschwindigkeitsAnschluss}
            = befehlAusführen
                (umdrehenAux bg bgmGeschwindigkeitsAnschluss [
                    pwmSetzeWert bg bgmGeschwindigkeitsAnschluss $ PwmValueUnmodifiziert pwmGrenze,
                    warte umdrehenZeit,
                    pwmSetzeWert bg bgmGeschwindigkeitsAnschluss $ PwmValueUnmodifiziert 0])
                ("Umdrehen (" <> showText bgmGeschwindigkeitsAnschluss <> ")")
    fahrtrichtungEinstellen :: (PwmReader r m, MonadIO m) => Bahngeschwindigkeit 'Lego -> Fahrtrichtung -> m ()
    fahrtrichtungEinstellen
        bg@LegoBahngeschwindigkeit {bglGeschwindigkeitsAnschluss, bglFahrtrichtungsAnschluss}
        fahrtrichtung
            = befehlAusführen
                (umdrehenAux bg bglGeschwindigkeitsAnschluss [
                    anschlussWrite bglFahrtrichtungsAnschluss $
                        (if fahrtrichtung == Vorwärts then fließend else gesperrt) bg])
                ("Umdrehen (" <>
                (showText bglGeschwindigkeitsAnschluss <^> showText bglFahrtrichtungsAnschluss $ Language.Deutsch) <>
                ")->" <> showText fahrtrichtung)

umdrehenAux :: (StreckenAtom s, PwmReader r m, MonadIO m) => s -> Anschluss -> [m ()] -> m ()
umdrehenAux s geschwindigkeitsAnschluss umdrehenAktionen = do
    pwmSetzeWert s geschwindigkeitsAnschluss $ PwmValueUnmodifiziert 0
    warte umdrehenZeit
    sequence_ umdrehenAktionen
    warte umdrehenZeit

-- | Steuere die Stromzufuhr einer Schiene
data Streckenabschnitt = Streckenabschnitt {stName :: Text, stFließend :: Value, stromAnschluss::Anschluss}
    deriving (Eq, Show)

instance Anzeige Streckenabschnitt where
    anzeige :: Streckenabschnitt -> Sprache -> Text
    anzeige Streckenabschnitt {stName, stromAnschluss}
        = Language.streckenabschnitt <:>
            Language.name <=> stName <^>
            Language.strom <-> Language.anschluss <=> stromAnschluss

instance StreckenObjekt Streckenabschnitt where
    anschlüsse :: Streckenabschnitt -> [Anschluss]
    anschlüsse Streckenabschnitt {stromAnschluss} = [stromAnschluss]
    erhalteName :: Streckenabschnitt -> Text
    erhalteName Streckenabschnitt {stName} = stName

instance StreckenAtom Streckenabschnitt where
    fließend :: Streckenabschnitt -> Value
    fließend = stFließend

-- | Sammel-Klasse für 'Streckenabschnitt'-artige Typen
class (StreckenObjekt s) => StreckenabschnittKlasse s where
    -- | Strom ein-/ausschalten
    strom :: (I2CReader r m, MonadIO m) => s -> Strom -> m ()
    {-# MINIMAL strom #-}

instance (StreckenabschnittKlasse (s 'Märklin), StreckenabschnittKlasse (s 'Lego)) => StreckenabschnittKlasse (ZugtypEither s) where
    strom :: (I2CReader r m, MonadIO m) => ZugtypEither s -> Strom -> m ()
    strom   (ZugtypMärklin a)   = strom a
    strom   (ZugtypLego a)      = strom a

instance StreckenabschnittKlasse Streckenabschnitt where
    strom :: (I2CReader r m, MonadIO m) => Streckenabschnitt -> Strom -> m ()
    strom st@Streckenabschnitt {stromAnschluss} an = befehlAusführen
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
        wemRichtungsAnschlüsse :: NonEmpty (Richtung, Anschluss)}
            -> Weiche 'Märklin

deriving instance Eq (Weiche z)
deriving instance Show (Weiche z)

instance Anzeige (Weiche z) where
    anzeige :: Weiche z -> Sprache -> Text
    anzeige
        LegoWeiche {welName, welRichtungsAnschluss, welRichtungen=(richtung1, richtung2)}
            = Language.lego <-> Language.weiche <:>
                Language.name <=> welName <^>
                Language.richtung <-> Language.anschluss <=> welRichtungsAnschluss <^>
                Language.richtungen <=> richtung1 <|> richtung2
    anzeige
        MärklinWeiche {wemName, wemRichtungsAnschlüsse}
            = Language.märklin <-> Language.weiche <:>
                Language.name <=> wemName <^>
                foldl (\acc (anschluss, richtung) -> acc <^> richtung <=> anschluss) (const "") wemRichtungsAnschlüsse

instance StreckenObjekt (Weiche z) where
    anschlüsse :: Weiche z -> [Anschluss]
    anschlüsse
        LegoWeiche {welRichtungsAnschluss}
            = [welRichtungsAnschluss]
    anschlüsse
        MärklinWeiche {wemRichtungsAnschlüsse}
            = map snd $ NE.toList wemRichtungsAnschlüsse
    erhalteName :: Weiche z -> Text
    erhalteName
        LegoWeiche {welName}
            = welName
    erhalteName
        MärklinWeiche {wemName}
            = wemName

instance StreckenAtom (Weiche z) where
    fließend :: Weiche z -> Value
    fließend
        LegoWeiche {welFließend}
            = welFließend
    fließend
        MärklinWeiche {wemFließend}
            = wemFließend

-- | Sammel-Klasse für 'Weiche'n-artige Typen
class (StreckenObjekt w) => WeicheKlasse w where
    -- | Weiche einstellen
    stellen :: (PwmReader r m, MonadIO m) => w -> Richtung -> m ()
    -- | Überprüfe, ob Weiche eine Richtung unterstützt
    hatRichtung :: w -> Richtung -> Bool
    hatRichtung weiche richtung = elem richtung $ erhalteRichtungen weiche
    -- | Erhalte alle Richtungen einer Weiche
    erhalteRichtungen :: w -> NonEmpty Richtung
    {-# MINIMAL stellen, erhalteRichtungen #-}

instance (WeicheKlasse (we 'Märklin), WeicheKlasse (we 'Lego)) => WeicheKlasse (ZugtypEither we) where
    stellen :: (PwmReader r m, MonadIO m) => ZugtypEither we -> Richtung -> m ()
    stellen (ZugtypMärklin a)   = stellen a
    stellen (ZugtypLego a)      = stellen a
    erhalteRichtungen :: ZugtypEither we -> NonEmpty Richtung
    erhalteRichtungen   (ZugtypMärklin a)   = erhalteRichtungen a
    erhalteRichtungen   (ZugtypLego a)      = erhalteRichtungen a

-- | Zeit, die Strom beim Stellen einer Märklin-Weiche anliegt
weicheZeit :: Wartezeit
weicheZeit = MilliSekunden 500

instance WeicheKlasse (Weiche z) where
    stellen :: (PwmReader r m, MonadIO m) => Weiche z -> Richtung -> m ()
    stellen
        we@LegoWeiche {welRichtungsAnschluss, welRichtungen}
        richtung
            | richtung == fst welRichtungen = befehlAusführen
                (pwmServo we welRichtungsAnschluss 25 >> warte weicheZeit >> pwmServo we welRichtungsAnschluss 0)
                ("Stellen (" <> showText welRichtungsAnschluss <> ") -> " <> showText richtung)
            | richtung == snd welRichtungen = befehlAusführen
                (pwmServo we welRichtungsAnschluss 75 >> warte weicheZeit >> pwmServo we welRichtungsAnschluss 0)
                ("stellen (" <> showText welRichtungsAnschluss <> ") -> " <> showText richtung)
            | otherwise                     = pure ()
    stellen
        we@MärklinWeiche {wemRichtungsAnschlüsse}
        richtung
            = befehlAusführen
                richtungStellen
                ("Stellen (" <> showText (getRichtungsAnschluss richtung $ NE.toList wemRichtungsAnschlüsse) <> ") -> " <> showText richtung)
            where
                richtungStellen :: (I2CReader r m, MonadIO m) => m ()
                richtungStellen = case getRichtungsAnschluss richtung $ NE.toList wemRichtungsAnschlüsse of
                    Nothing                     -> pure ()
                    (Just richtungsAnschluss)   -> do
                        anschlussWrite richtungsAnschluss $ fließend we
                        warte weicheZeit
                        anschlussWrite richtungsAnschluss $ gesperrt we
                getRichtungsAnschluss :: Richtung -> [(Richtung, Anschluss)] -> Maybe Anschluss
                getRichtungsAnschluss _richtung   []  = Nothing
                getRichtungsAnschluss richtung    ((ersteRichtung, ersterAnschluss) : andereRichtungen)
                    | richtung == ersteRichtung = Just ersterAnschluss
                    | otherwise                 = getRichtungsAnschluss richtung andereRichtungen
    hatRichtung :: Weiche z -> Richtung -> Bool
    hatRichtung
        LegoWeiche {welRichtungen=(erste, zweite)}
        richtung
            = (erste == richtung) || (zweite == richtung)
    hatRichtung
        MärklinWeiche {wemRichtungsAnschlüsse}
        richtung
            = any (\(richtung0, _pin0) -> (richtung0 == richtung)) wemRichtungsAnschlüsse
    erhalteRichtungen :: Weiche z -> NonEmpty Richtung
    erhalteRichtungen
        LegoWeiche {welRichtungen=(richtung1, richtung2)}
            = richtung1 :| [richtung2]
    erhalteRichtungen
        MärklinWeiche {wemRichtungsAnschlüsse}
            = fst <$> wemRichtungsAnschlüsse

-- | Kontrolliere, wann Wagons über eine Kupplungs-Schiene abgekuppelt werden
data Kupplung = Kupplung {kuName :: Text, kuFließend :: Value, kupplungsAnschluss::Anschluss}
    deriving (Eq, Show)

instance Anzeige Kupplung where
    anzeige :: Kupplung -> Sprache -> Text
    anzeige
        Kupplung {kuName, kupplungsAnschluss}
            = Language.kupplung <:>
                Language.name <=> kuName <^>
                Language.kupplung <-> Language.anschluss <=> kupplungsAnschluss

instance StreckenObjekt Kupplung where
    anschlüsse :: Kupplung -> [Anschluss]
    anschlüsse
        Kupplung {kupplungsAnschluss}
            = [kupplungsAnschluss]
    erhalteName :: Kupplung -> Text
    erhalteName
        Kupplung {kuName}
            = kuName

instance StreckenAtom Kupplung where
    fließend :: Kupplung -> Value
    fließend = kuFließend

-- | Sammel-Klasse für 'Kupplung'-artige Typen
class (StreckenObjekt k) => KupplungKlasse k where
    -- | Kupplung betätigen
    kuppeln :: (I2CReader r m, MonadIO m) => k -> m ()
    {-# MINIMAL kuppeln #-}

instance (KupplungKlasse (ku 'Märklin), KupplungKlasse (ku 'Lego)) => KupplungKlasse (ZugtypEither ku) where
    kuppeln :: (I2CReader r m, MonadIO m) => ZugtypEither ku -> m ()
    kuppeln (ZugtypMärklin a)   = kuppeln a
    kuppeln (ZugtypLego a)      = kuppeln a

-- | Zeit, die Strom beim Kuppeln anliegt
kuppelnZeit :: Wartezeit
kuppelnZeit = MilliSekunden 500

instance KupplungKlasse Kupplung where
    kuppeln :: (I2CReader r m, MonadIO m) => Kupplung -> m ()
    kuppeln ku@Kupplung {kupplungsAnschluss} = befehlAusführen
        (anschlussWrite kupplungsAnschluss (fließend ku) >> warte kuppelnZeit >> anschlussWrite kupplungsAnschluss (gesperrt ku))
        ("Kuppeln (" <> showText kupplungsAnschluss <> ")")

-- | Zusammenfassung von Einzel-Elementen. Weichen haben eine vorgegebene Richtung.
data Wegstrecke (z :: Zugtyp)
    = Wegstrecke {
        wsName :: Text,
        wsBahngeschwindigkeiten :: [Bahngeschwindigkeit z],
        wsStreckenabschnitte :: [Streckenabschnitt],
        wsWeichenRichtungen :: [(Weiche z, Richtung)],
        wsKupplungen :: [Kupplung]}
    deriving (Eq, Show)

instance Anzeige (Wegstrecke z) where
    anzeige :: Wegstrecke z -> Sprache -> Text
    anzeige
        Wegstrecke {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen} 
            = Language.wegstrecke <:> Language.name <=> wsName
            <^> Language.bahngeschwindigkeiten <=> wsBahngeschwindigkeiten
            <^> Language.streckenabschnitte <=> wsStreckenabschnitte
            <^> Language.weichen <=> map (uncurry (<°>)) wsWeichenRichtungen
            <^> Language.kupplungen <=> wsKupplungen

instance StreckenObjekt (Wegstrecke z) where
    anschlüsse :: Wegstrecke z -> [Anschluss]
    anschlüsse Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen}
        = join $ map anschlüsse wsBahngeschwindigkeiten
                ++ map anschlüsse wsStreckenabschnitte
                ++ map (anschlüsse . fst) wsWeichenRichtungen
                ++ map anschlüsse wsKupplungen
    erhalteName :: Wegstrecke z -> Text
    erhalteName Wegstrecke {wsName} = wsName

instance BahngeschwindigkeitKlasse Wegstrecke where
    geschwindigkeit :: (PwmReader r m, MonadIO m) => Wegstrecke z -> Natural -> m ()
    geschwindigkeit Wegstrecke {wsBahngeschwindigkeiten} wert
        = mapM_ (forkI2CReader . flip geschwindigkeit wert) wsBahngeschwindigkeiten
    umdrehen :: (PwmReader r m, MonadIO m) => Wegstrecke 'Märklin -> m ()
    umdrehen Wegstrecke {wsBahngeschwindigkeiten} = mapM_ (forkI2CReader . umdrehen) wsBahngeschwindigkeiten
    fahrtrichtungEinstellen :: (PwmReader r m, MonadIO m) => Wegstrecke 'Lego -> Fahrtrichtung -> m ()
    fahrtrichtungEinstellen Wegstrecke {wsBahngeschwindigkeiten} neueFahrtrichtung
        = mapM_ (forkI2CReader . flip fahrtrichtungEinstellen neueFahrtrichtung) wsBahngeschwindigkeiten

instance StreckenabschnittKlasse (Wegstrecke z) where
    strom :: (I2CReader r m, MonadIO m) => Wegstrecke z -> Strom -> m ()
    strom Wegstrecke {wsStreckenabschnitte} an = mapM_ (forkI2CReader . flip strom an) wsStreckenabschnitte

instance KupplungKlasse (Wegstrecke z) where
    kuppeln :: (I2CReader r m, MonadIO m) => Wegstrecke z -> m ()
    kuppeln Wegstrecke {wsKupplungen} = mapM_ (forkI2CReader . kuppeln) wsKupplungen

-- | Sammel-Klasse für 'Wegstrecke'n-artige Typen
class (StreckenObjekt w, StreckenabschnittKlasse w, KupplungKlasse w) => WegstreckeKlasse w where
    einstellen :: (PwmReader r m, MonadIO m) => w -> m ()
    {-# MINIMAL einstellen #-}

instance (WegstreckeKlasse (w 'Märklin), WegstreckeKlasse (w 'Lego)) => WegstreckeKlasse (ZugtypEither w) where
    einstellen :: (PwmReader r m, MonadIO m) => ZugtypEither w -> m ()
    einstellen  (ZugtypMärklin a)   = einstellen a
    einstellen  (ZugtypLego a)      = einstellen a

instance WegstreckeKlasse (Wegstrecke z) where
    einstellen :: (PwmReader r m, MonadIO m) => Wegstrecke z -> m ()
    einstellen Wegstrecke {wsWeichenRichtungen} = mapM_ (forkI2CReader . uncurry stellen) wsWeichenRichtungen

-- | Ausführen einer IO-Aktion, bzw. Ausgabe eines Strings, abhängig vom Kommandozeilen-Argument
befehlAusführen :: (MonadIO m) => m () -> Text -> m ()
befehlAusführen ioAction ersatzNachricht = do
    Options {printCmd} <- getOptions
    if printCmd
        then liftIO $ T.putStrLn ersatzNachricht
        else ioAction