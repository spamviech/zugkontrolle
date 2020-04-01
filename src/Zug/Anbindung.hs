{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

{-|
Description : Low-Level-Definition der unterstützen Aktionen auf Anschluss-Ebene.
-}
module Zug.Anbindung
  ( -- * Anschluss-Repräsentation
    Anschluss(..)
  , PCF8574Port(..)
  , PCF8574(..)
  , PCF8574Variant(..)
  , vonPinGpio
  , zuPinGpio
  , vonPCF8574Port
  , zuPCF8574Port
  , PwmMap
  , pwmMapEmpty
  , MitPwmMap(..)
  , PwmReader(..)
  , I2CMap
  , i2cMapEmpty
  , MitI2CMap(..)
  , I2CReader(..)
  , Value(..)
  , alleValues
  , Pin(..)
  , pinToBcmGpio
  , vonPin
  , zuPin
  , pwmMöglich
  , clockMöglich
  , PwmValueUnmodifiziert
    -- * Strecken-Objekte
  , StreckenObjekt(..)
  , StreckenAtom(..)
    -- ** Bahngeschwindigkeiten
  , Bahngeschwindigkeit(..)
  , BahngeschwindigkeitKlasse(..)
  , verwendetPwm
  , pwmEingabeMaximal
  , erhaltePwmWertVoll
  , erhaltePWMWertReduziert
    -- ** Streckenabschnitte
  , Streckenabschnitt(..)
  , StreckenabschnittKlasse(..)
    -- ** Weichen
  , Weiche(..)
  , WeicheKlasse(..)
    -- ** Kupplungen
  , Kupplung(..)
  , KupplungKlasse(..)
    -- ** Wegstrecken
  , Wegstrecke(..)
  , WegstreckeKlasse(..)
    -- * Wartezeit
  , warte
  , Wartezeit(..)
  , addition
  , differenz
  , multiplizieren
  , dividieren
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (join, forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Word (Word8)
import Numeric.Natural (Natural)
import System.Hardware.WiringPi
       (Pin(..), PwmValue(), Mode(..), pinMode, digitalWrite, pwmSetRange, pwmWrite, pinToBcmGpio)

import Zug.Anbindung.Anschluss
       (Anschluss(..), PCF8574Port(..), PCF8574(..), PCF8574Variant(..), vonPin, zuPin, vonPinGpio
      , zuPinGpio, vonPCF8574Port, zuPCF8574Port, anschlussWrite, Value(..), I2CMap, i2cMapEmpty
      , MitI2CMap(..), I2CReader(..), pcf8574Gruppieren, pcf8574MultiPortWrite)
import Zug.Anbindung.SoftwarePWM
       (PwmMap, pwmMapEmpty, MitPwmMap(..), PwmReader(..), pwmGrenze, pwmSoftwareSetzteWert)
import Zug.Anbindung.Wartezeit
       (warte, Wartezeit(..), addition, differenz, multiplizieren, dividieren)
import Zug.Enums (Zugtyp(..), ZugtypEither(..), GeschwindigkeitVariante(..)
                , GeschwindigkeitEither(..), GeschwindigkeitPhantom(..), catPwm
                , catKonstanteSpannung, Strom(..), Fahrtrichtung(..), Richtung(..))
import qualified Zug.Language as Language
import Zug.Language (Anzeige(..), Sprache(), showText, (<^>), (<=>), (<->), (<|>), (<:>), (<°>))
import Zug.Options (Options(..), PWM(..), getOptions)

-- | Alle Möglichen Werte von 'Value'
alleValues :: NonEmpty Value
alleValues = NE.fromList [minBound .. maxBound]

-- * Test-Funktionen, ob Anschluss bestimmte Funktionen unterstützen
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
pwmSetzeWert
    :: (StreckenAtom s, PwmReader r m, MonadIO m) => s -> Pin -> PwmValueUnmodifiziert -> m ()
pwmSetzeWert s pin pwmValue = do
    Options {pwm} <- getOptions
    if (pwm == HardwarePWM) && pwmMöglich pin
        then liftIO $ do
            pinMode pin PWM_OUTPUT
            pwmSetRange pwmGrenze
            pwmWrite pin $ pwmValueModifiziert s pwmValue
        else pwmSoftwareSetzteWert pin pwmFrequenzHzNormal $ pwmValueModifiziert s pwmValue

-- | Erzeuge PWM-Funktion für einen Servo-Motor
--   Nutze SoftwarePWM für eine konstante Frequenz (sonst abhängig pwmGrenze und pwmValue)
pwmServo :: (StreckenAtom s, PwmReader r m, MonadIO m) => s -> Pin -> Natural -> m ()
pwmServo s pin =
    pwmSoftwareSetzteWert pin pwmFrequenzHzServo . pwmValueModifiziert s . erhaltePwmWertVoll

-- | newtype auf 'PwmValue' um ein noch bevorstehendes modifizieren bzgl. fließend-Value zu signalisieren
newtype PwmValueUnmodifiziert = PwmValueUnmodifiziert PwmValue

-- | Berechne den PWM-Wert abhängig davon, bei welchen 'Anschluss'-Output ('HIGH'/'LOW') der Strom fließt.
pwmValueModifiziert :: (StreckenAtom s) => s -> PwmValueUnmodifiziert -> PwmValue
pwmValueModifiziert s (PwmValueUnmodifiziert pwmValue) = case fließend s of
    HIGH -> pwmValue
    LOW -> pwmGrenze - pwmValue

-- ** Frequenzen
-- | 50 Hz Frequenz; Standard-Wert von Servo-Motoren
pwmFrequenzHzServo :: Natural
pwmFrequenzHzServo = 50

-- | Normale PWM-Frequenz
pwmFrequenzHzNormal :: Natural
pwmFrequenzHzNormal = 500

-- | Erhalte PWMValue ausgehend von einem Wert zwischen 0 und 'pwmEingabeMaximal'.
erhaltePwmWert :: (Integral i) => Natural -> i -> PwmValueUnmodifiziert
erhaltePwmWert pwmGrenzeMax wert = PwmValueUnmodifiziert $ fromIntegral ergebnis
    {-
            Verwende Natural um Fehler wegen zu kleinem Wertebereich zu vermeiden.
            Multipliziere zuerst alle Werte, bevor sie normalisiert werden um Rundungsfehler zu verhindern.
            Möglich, nachdem die Funktion nicht in Performance-kritischen Bereichen (und selten) aufgerufen wird.
            Effektivspannung skaliert wie die Wurzel des PwmValue.
            Der Eingabewert wird daher quadriert um linear mit der Effektivspannung zu skalieren.
        -}
        where
            ergebnis :: Natural
            ergebnis = div wertSkaliert (pwmEingabeMaximal * pwmEingabeMaximal)

            wertSkaliert :: Natural
            wertSkaliert = pwmGrenzeMax * wertBegrenzt * wertBegrenzt

            wertBegrenzt :: Natural
            wertBegrenzt = min pwmEingabeMaximal $ fromIntegral wert

-- | Maximaler Eingabewert für 'geschwindigkeit'.
pwmEingabeMaximal :: Natural
pwmEingabeMaximal = fromIntegral $ (maxBound :: Word8)

-- | Vollständige pwmGrenze als Natural.
pwmGrenzeVoll :: Natural
pwmGrenzeVoll = fromIntegral pwmGrenze

-- | Maximal erlaubter pwmGrenze um eine Effektivspannung von 16V zu erhalten.
pwmGrenzeReduziert :: Natural
pwmGrenzeReduziert =
    div (pwmGrenzeVoll * spannungFahrt * spannungFahrt) (spannungQuelle * spannungQuelle)
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

instance (StreckenObjekt (a 'Märklin), StreckenObjekt (a 'Lego))
    => StreckenObjekt (ZugtypEither a) where
    anschlüsse :: ZugtypEither a -> [Anschluss]
    anschlüsse (ZugtypMärklin a) = anschlüsse a
    anschlüsse (ZugtypLego a) = anschlüsse a

    erhalteName :: ZugtypEither a -> Text
    erhalteName (ZugtypMärklin a) = erhalteName a
    erhalteName (ZugtypLego a) = erhalteName a

instance (StreckenObjekt (bg 'Pwm z), StreckenObjekt (bg 'KonstanteSpannung z))
    => StreckenObjekt (GeschwindigkeitEither bg z) where
    anschlüsse :: GeschwindigkeitEither bg z -> [Anschluss]
    anschlüsse (GeschwindigkeitPwm bg) = anschlüsse bg
    anschlüsse (GeschwindigkeitKonstanteSpannung bg) = anschlüsse bg

    erhalteName :: GeschwindigkeitEither bg z -> Text
    erhalteName (GeschwindigkeitPwm bg) = erhalteName bg
    erhalteName (GeschwindigkeitKonstanteSpannung bg) = erhalteName bg

instance (StreckenObjekt (a z)) => StreckenObjekt (GeschwindigkeitPhantom a g z) where
    anschlüsse :: GeschwindigkeitPhantom a g z -> [Anschluss]
    anschlüsse (GeschwindigkeitPhantom a) = anschlüsse a

    erhalteName :: GeschwindigkeitPhantom a g z -> Text
    erhalteName (GeschwindigkeitPhantom a) = erhalteName a

instance (StreckenObjekt a) => StreckenObjekt (Maybe a) where
    anschlüsse :: Maybe a -> [Anschluss]
    anschlüsse (Just a) = anschlüsse a
    anschlüsse Nothing = []

    erhalteName (Just a) = erhalteName a
    erhalteName Nothing = ""

-- | Eine Klasse für alle Typen, die direkt mit wiringPi interagieren.
class StreckenAtom s where
    fließend :: s -> Value
    fließend = erhalteValue Fließend

    gesperrt :: s -> Value
    gesperrt s = case fließend s of
        HIGH -> LOW
        LOW -> HIGH

    erhalteValue :: Strom -> s -> Value
    erhalteValue Fließend = fließend
    erhalteValue Gesperrt = gesperrt

    {-# MINIMAL fließend | erhalteValue #-}

instance (StreckenAtom (a 'Märklin), StreckenAtom (a 'Lego)) => StreckenAtom (ZugtypEither a) where
    fließend :: ZugtypEither a -> Value
    fließend (ZugtypMärklin a) = fließend a
    fließend (ZugtypLego a) = fließend a

-- | Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung
data Bahngeschwindigkeit (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
    LegoBahngeschwindigkeit :: { bglName :: Text
                               , bglFließend :: Value
                               , bglGeschwindigkeitsPin :: Pin
                               , bglFahrtrichtungsAnschluss :: Anschluss
                               } -> Bahngeschwindigkeit 'Pwm 'Lego
    MärklinBahngeschwindigkeitPwm
        :: { bgmpName :: Text, bgmpFließend :: Value, bgmpGeschwindigkeitsPin :: Pin }
        -> Bahngeschwindigkeit 'Pwm 'Märklin
    MärklinBahngeschwindigkeitKonstanteSpannung
        :: { bgmkName :: Text
           , bgmkFließend :: Value
           , bgmkFahrstromAnschlüsse :: NonEmpty Anschluss
           , bgmkUmdrehenAnschluss :: Anschluss
           } -> Bahngeschwindigkeit 'KonstanteSpannung 'Märklin

deriving instance Eq (Bahngeschwindigkeit g z)

deriving instance Show (Bahngeschwindigkeit g z)

instance Anzeige (Bahngeschwindigkeit g z) where
    anzeige :: Bahngeschwindigkeit g z -> Sprache -> Text
    anzeige LegoBahngeschwindigkeit {bglName, bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss} =
        Language.lego
        <-> Language.bahngeschwindigkeit
        <:> Language.name
        <=> bglName
        <^> Language.geschwindigkeit
        <-> Language.pin
        <=> bglGeschwindigkeitsPin
        <^> Language.fahrtrichtung <-> Language.anschluss <=> bglFahrtrichtungsAnschluss
    anzeige MärklinBahngeschwindigkeitPwm {bgmpName, bgmpGeschwindigkeitsPin} =
        Language.märklin
        <-> Language.bahngeschwindigkeit
        <:> Language.name
        <=> bgmpName <^> Language.geschwindigkeit <-> Language.pin <=> bgmpGeschwindigkeitsPin
    anzeige
        MärklinBahngeschwindigkeitKonstanteSpannung
        {bgmkName, bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss} =
        Language.märklin
        <-> Language.bahngeschwindigkeit
        <:> Language.name
        <=> bgmkName
        <^> Language.fahrstrom
        <-> Language.anschlüsse
        <=> bgmkFahrstromAnschlüsse
        <^> Language.umdrehen <-> Language.anschluss <=> bgmkUmdrehenAnschluss

instance StreckenObjekt (Bahngeschwindigkeit b z) where
    anschlüsse :: Bahngeschwindigkeit b z -> [Anschluss]
    anschlüsse LegoBahngeschwindigkeit {bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss} =
        [AnschlussPin bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss]
    anschlüsse MärklinBahngeschwindigkeitPwm {bgmpGeschwindigkeitsPin} =
        [AnschlussPin bgmpGeschwindigkeitsPin]
    anschlüsse
        MärklinBahngeschwindigkeitKonstanteSpannung
        {bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss} =
        bgmkUmdrehenAnschluss : NonEmpty.toList bgmkFahrstromAnschlüsse

    erhalteName :: Bahngeschwindigkeit b z -> Text
    erhalteName LegoBahngeschwindigkeit {bglName} = bglName
    erhalteName MärklinBahngeschwindigkeitPwm {bgmpName} = bgmpName
    erhalteName MärklinBahngeschwindigkeitKonstanteSpannung {bgmkName} = bgmkName

instance StreckenAtom (Bahngeschwindigkeit b z) where
    fließend :: Bahngeschwindigkeit b z -> Value
    fließend LegoBahngeschwindigkeit {bglFließend} = bglFließend
    fließend MärklinBahngeschwindigkeitPwm {bgmpFließend} = bgmpFließend
    fließend MärklinBahngeschwindigkeitKonstanteSpannung {bgmkFließend} = bgmkFließend

-- | Verwendet die 'Bahngeschwindigkeit' PWM zur Geschwindigkeitskontrolle?
verwendetPwm :: Bahngeschwindigkeit g z -> GeschwindigkeitVariante
verwendetPwm MärklinBahngeschwindigkeitKonstanteSpannung {} = KonstanteSpannung
verwendetPwm MärklinBahngeschwindigkeitPwm {} = Pwm
verwendetPwm LegoBahngeschwindigkeit {} = Pwm

-- | Sammel-Klasse für 'Bahngeschwindigkeit'-artige Typen
class ( StreckenObjekt (b 'Pwm 'Märklin)
      , StreckenObjekt (b 'Pwm 'Lego)
      , StreckenObjekt (b 'KonstanteSpannung 'Märklin)
      , StreckenObjekt (b 'KonstanteSpannung 'Lego)
      ) => BahngeschwindigkeitKlasse b where
    -- | Geschwindigkeit einstellen (akzeptiere Werte von 0 bis 100)
    geschwindigkeit :: (I2CReader r m, PwmReader r m, MonadIO m) => b 'Pwm z -> Word8 -> m ()

    -- | Fahrstrom ein-/ausschalten
    fahrstrom :: (I2CReader r m, MonadIO m) => b 'KonstanteSpannung z -> Word8 -> m ()

    -- | Gebe allen Zügen den Befehl zum Umdrehen
    umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m) => b g 'Märklin -> m ()

    -- | Gebe allen Zügen den Befehl in einer bestimmen Richtung zu fahren
    fahrtrichtungEinstellen
        :: (I2CReader r m, PwmReader r m, MonadIO m) => b g 'Lego -> Fahrtrichtung -> m ()
    {-# MINIMAL geschwindigkeit, fahrstrom, umdrehen, fahrtrichtungEinstellen #-}

-- | Erhalte das Element an Position /i/, angefangen bei /1/.
-- Ist die Position größer als die Länge der Liste wird das letzte Element zurückgegeben.
positionOderLetztes :: Word8 -> NonEmpty a -> Maybe a
positionOderLetztes 0 _nonEmpty = Nothing
positionOderLetztes 1 (a :| _t) = Just a
positionOderLetztes _i (a :| []) = Just a
positionOderLetztes i (_a :| (h:t)) = positionOderLetztes (pred i) $ h :| t

-- | Zeit, die Strom beim Umdrehen einer Märklin-Bahngeschwindigkeit anliegt
umdrehenZeit :: Wartezeit
umdrehenZeit = MilliSekunden 250

instance BahngeschwindigkeitKlasse Bahngeschwindigkeit where
    geschwindigkeit
        :: (I2CReader r m, PwmReader r m, MonadIO m) => Bahngeschwindigkeit 'Pwm z -> Word8 -> m ()
    geschwindigkeit bg@LegoBahngeschwindigkeit {bglGeschwindigkeitsPin} geschwindigkeit =
        befehlAusführen
            (pwmSetzeWert bg bglGeschwindigkeitsPin $ erhaltePwmWertVoll geschwindigkeit)
            ("Geschwindigkeit ("
             <> showText bglGeschwindigkeitsPin
             <> ")->"
             <> showText geschwindigkeit)
    geschwindigkeit bg@MärklinBahngeschwindigkeitPwm {bgmpGeschwindigkeitsPin} geschwindigkeit =
        befehlAusführen
            (pwmSetzeWert bg bgmpGeschwindigkeitsPin $ erhaltePWMWertReduziert geschwindigkeit)
            ("Geschwindigkeit ("
             <> showText bgmpGeschwindigkeitsPin
             <> ")->"
             <> showText geschwindigkeit)

    fahrstrom
        :: (I2CReader r m, MonadIO m) => Bahngeschwindigkeit 'KonstanteSpannung z -> Word8 -> m ()
    fahrstrom
        bg@MärklinBahngeschwindigkeitKonstanteSpannung
        {bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss}
        fahrstromAnschluss =
        befehlAusführen
            (anschlussWrite bgmkUmdrehenAnschluss (gesperrt bg)
             >> liftIO (forM_ fahrstromPins $ forkIO . uncurry digitalWrite)
             >> mapM_
                 (\(pcf8574, ports) -> forkI2CReader $ pcf8574MultiPortWrite pcf8574 ports HIGH)
                 (Map.toList fahrstromPortMapHigh)
             >> mapM_
                 (\(pcf8574, ports) -> forkI2CReader $ pcf8574MultiPortWrite pcf8574 ports LOW)
                 (Map.toList fahrstromPortMapLow))
            ("Fahrstrom ("
             <> showText bgmkFahrstromAnschlüsse
             <> ")->"
             <> showText fahrstromAnschluss)
        where
            (fahrstromPins, fahrstromPcf8574PortsHigh, fahrstromPcf8574PortsLow) =
                foldl splitAnschlüsse ([], [], []) bgmkFahrstromAnschlüsse

            splitAnschlüsse :: ([(Pin, Value)], [PCF8574Port], [PCF8574Port])
                             -> Anschluss
                             -> ([(Pin, Value)], [PCF8574Port], [PCF8574Port])
            splitAnschlüsse (pins, portsHigh, portsLow) anschluss@(AnschlussPin pin) =
                ((pin, anschlussValue anschluss) : pins, portsHigh, portsLow)
            splitAnschlüsse (pins, portsHigh, portsLow) anschluss@(AnschlussPCF8574Port port)
                | anschlussValue anschluss == HIGH = (pins, port : portsHigh, portsLow)
                | otherwise = (pins, portsHigh, port : portsLow)

            fahrstromPortMapHigh = pcf8574Gruppieren fahrstromPcf8574PortsHigh

            fahrstromPortMapLow = pcf8574Gruppieren fahrstromPcf8574PortsLow

            anschlussValue :: Anschluss -> Value
            anschlussValue anschluss
                | positionOderLetztes fahrstromAnschluss bgmkFahrstromAnschlüsse
                    == (Just anschluss) =
                    fließend bg
                | otherwise = gesperrt bg

    umdrehen
        :: (I2CReader r m, PwmReader r m, MonadIO m) => Bahngeschwindigkeit b 'Märklin -> m ()
    umdrehen bg@MärklinBahngeschwindigkeitPwm {bgmpGeschwindigkeitsPin} =
        befehlAusführen
            (pwmSetzeWert bg bgmpGeschwindigkeitsPin (PwmValueUnmodifiziert 0)
             >> warte umdrehenZeit
             >> pwmSetzeWert bg bgmpGeschwindigkeitsPin (PwmValueUnmodifiziert pwmGrenze)
             >> warte umdrehenZeit
             >> pwmSetzeWert bg bgmpGeschwindigkeitsPin (PwmValueUnmodifiziert 0))
            ("Umdrehen (" <> showText bgmpGeschwindigkeitsPin <> ")")
    umdrehen bg@MärklinBahngeschwindigkeitKonstanteSpannung {bgmkUmdrehenAnschluss} =
        befehlAusführen
            (fahrstrom bg 0
             >> warte umdrehenZeit
             >> anschlussWrite bgmkUmdrehenAnschluss (fließend bg)
             >> warte umdrehenZeit
             >> anschlussWrite bgmkUmdrehenAnschluss (gesperrt bg))
            ("Umdrehen (" <> showText bgmkUmdrehenAnschluss <> ")")

    fahrtrichtungEinstellen :: (I2CReader r m, PwmReader r m, MonadIO m)
                            => Bahngeschwindigkeit b 'Lego
                            -> Fahrtrichtung
                            -> m ()
    fahrtrichtungEinstellen
        bg@LegoBahngeschwindigkeit {bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss}
        fahrtrichtung =
        befehlAusführen
            (pwmSetzeWert bg bglGeschwindigkeitsPin (PwmValueUnmodifiziert 0)
             >> warte umdrehenZeit
             >> anschlussWrite
                 bglFahrtrichtungsAnschluss
                 ((if fahrtrichtung == Vorwärts
                       then fließend
                       else gesperrt)
                      bg))
            ("Umdrehen ("
             <> (showText bglGeschwindigkeitsPin <^> showText bglFahrtrichtungsAnschluss
                 $ Language.Deutsch)
             <> ")->"
             <> showText fahrtrichtung)

-- | Steuere die Stromzufuhr einer Schiene
data Streckenabschnitt =
    Streckenabschnitt { stName :: Text, stFließend :: Value, stromAnschluss :: Anschluss }
    deriving (Eq, Show)

instance Anzeige Streckenabschnitt where
    anzeige :: Streckenabschnitt -> Sprache -> Text
    anzeige Streckenabschnitt {stName, stromAnschluss} =
        Language.streckenabschnitt
        <:> Language.name <=> stName <^> Language.strom <-> Language.anschluss <=> stromAnschluss

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

instance (StreckenabschnittKlasse (s 'Märklin), StreckenabschnittKlasse (s 'Lego))
    => StreckenabschnittKlasse (ZugtypEither s) where
    strom :: (I2CReader r m, MonadIO m) => ZugtypEither s -> Strom -> m ()
    strom (ZugtypMärklin a) = strom a
    strom (ZugtypLego a) = strom a

instance StreckenabschnittKlasse Streckenabschnitt where
    strom :: (I2CReader r m, MonadIO m) => Streckenabschnitt -> Strom -> m ()
    strom st@Streckenabschnitt {stromAnschluss} an =
        befehlAusführen
            (anschlussWrite stromAnschluss $ erhalteValue an st)
            ("Strom (" <> showText stromAnschluss <> ")->" <> showText an)

-- | Stellen einer 'Weiche'.
data Weiche (z :: Zugtyp) where
    LegoWeiche :: { welName :: Text
                  , welFließend :: Value
                  , welRichtungsPin :: Pin
                  , welRichtungen :: (Richtung, Richtung)
                  } -> Weiche 'Lego
    MärklinWeiche :: { wemName :: Text
                      , wemFließend :: Value
                      , wemRichtungsAnschlüsse :: NonEmpty (Richtung, Anschluss)
                      } -> Weiche 'Märklin

deriving instance Eq (Weiche z)

deriving instance Show (Weiche z)

instance Anzeige (Weiche z) where
    anzeige :: Weiche z -> Sprache -> Text
    anzeige LegoWeiche {welName, welRichtungsPin, welRichtungen = (richtung1, richtung2)} =
        Language.lego
        <-> Language.weiche
        <:> Language.name
        <=> welName
        <^> Language.richtung
        <-> Language.pin <=> welRichtungsPin <^> Language.richtungen <=> richtung1 <|> richtung2
    anzeige MärklinWeiche {wemName, wemRichtungsAnschlüsse} =
        Language.märklin
        <-> Language.weiche
        <:> Language.name
        <=> wemName
        <^> foldl
            (\acc (anschluss, richtung) -> acc <^> richtung <=> anschluss)
            (const "")
            wemRichtungsAnschlüsse

instance StreckenObjekt (Weiche z) where
    anschlüsse :: Weiche z -> [Anschluss]
    anschlüsse LegoWeiche {welRichtungsPin} = [AnschlussPin welRichtungsPin]
    anschlüsse
        MärklinWeiche {wemRichtungsAnschlüsse} = map snd $ NE.toList wemRichtungsAnschlüsse

    erhalteName :: Weiche z -> Text
    erhalteName LegoWeiche {welName} = welName
    erhalteName MärklinWeiche {wemName} = wemName

instance StreckenAtom (Weiche z) where
    fließend :: Weiche z -> Value
    fließend LegoWeiche {welFließend} = welFließend
    fließend MärklinWeiche {wemFließend} = wemFließend

-- | Sammel-Klasse für 'Weiche'n-artige Typen
class (StreckenObjekt w) => WeicheKlasse w where
    -- | Weiche einstellen
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => w -> Richtung -> m ()

    -- | Überprüfe, ob Weiche eine Richtung unterstützt
    hatRichtung :: w -> Richtung -> Bool
    hatRichtung weiche richtung = elem richtung $ erhalteRichtungen weiche

    -- | Erhalte alle Richtungen einer Weiche
    erhalteRichtungen :: w -> NonEmpty Richtung
    {-# MINIMAL stellen, erhalteRichtungen #-}

instance (WeicheKlasse (we 'Märklin), WeicheKlasse (we 'Lego))
    => WeicheKlasse (ZugtypEither we) where
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => ZugtypEither we -> Richtung -> m ()
    stellen (ZugtypMärklin a) = stellen a
    stellen (ZugtypLego a) = stellen a

    erhalteRichtungen :: ZugtypEither we -> NonEmpty Richtung
    erhalteRichtungen (ZugtypMärklin a) = erhalteRichtungen a
    erhalteRichtungen (ZugtypLego a) = erhalteRichtungen a

-- | Zeit, die Strom beim Stellen einer Märklin-Weiche anliegt
weicheZeit :: Wartezeit
weicheZeit = MilliSekunden 250

instance WeicheKlasse (Weiche z) where
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => Weiche z -> Richtung -> m ()
    stellen we@LegoWeiche {welRichtungsPin, welRichtungen} richtung
        | richtung == fst welRichtungen =
            befehlAusführen
                (pwmServo we welRichtungsPin 25
                 >> warte weicheZeit
                 >> pwmServo we welRichtungsPin 0)
                ("Stellen (" <> showText welRichtungsPin <> ") -> " <> showText richtung)
        | richtung == snd welRichtungen =
            befehlAusführen
                (pwmServo we welRichtungsPin 75
                 >> warte weicheZeit
                 >> pwmServo we welRichtungsPin 0)
                ("stellen (" <> showText welRichtungsPin <> ") -> " <> showText richtung)
        | otherwise = pure ()
    stellen we@MärklinWeiche {wemRichtungsAnschlüsse} richtung =
        befehlAusführen
            richtungStellen
            ("Stellen ("
             <> showText (getRichtungsAnschluss richtung $ NE.toList wemRichtungsAnschlüsse)
             <> ") -> "
             <> showText richtung)
        where
            richtungStellen :: (I2CReader r m, MonadIO m) => m ()
            richtungStellen =
                case getRichtungsAnschluss richtung $ NE.toList wemRichtungsAnschlüsse of
                    Nothing -> pure ()
                    (Just richtungsAnschluss) -> do
                        anschlussWrite richtungsAnschluss $ fließend we
                        warte weicheZeit
                        anschlussWrite richtungsAnschluss $ gesperrt we

            getRichtungsAnschluss :: Richtung -> [(Richtung, Anschluss)] -> Maybe Anschluss
            getRichtungsAnschluss _richtung [] = Nothing
            getRichtungsAnschluss richtung ((ersteRichtung, ersterAnschluss):andereRichtungen)
                | richtung == ersteRichtung = Just ersterAnschluss
                | otherwise = getRichtungsAnschluss richtung andereRichtungen

    hatRichtung :: Weiche z -> Richtung -> Bool
    hatRichtung LegoWeiche {welRichtungen = (erste, zweite)} richtung =
        (erste == richtung) || (zweite == richtung)
    hatRichtung MärklinWeiche {wemRichtungsAnschlüsse} richtung =
        any ((richtung ==) . fst) wemRichtungsAnschlüsse

    erhalteRichtungen :: Weiche z -> NonEmpty Richtung
    erhalteRichtungen
        LegoWeiche {welRichtungen = (richtung1, richtung2)} = richtung1 :| [richtung2]
    erhalteRichtungen MärklinWeiche {wemRichtungsAnschlüsse} = fst <$> wemRichtungsAnschlüsse

-- | Kontrolliere, wann Wagons über eine Kupplungs-Schiene abgekoppelt werden
data Kupplung = Kupplung { kuName :: Text, kuFließend :: Value, kupplungsAnschluss :: Anschluss }
    deriving (Eq, Show)

instance Anzeige Kupplung where
    anzeige :: Kupplung -> Sprache -> Text
    anzeige Kupplung {kuName, kupplungsAnschluss} =
        Language.kupplung
        <:> Language.name
        <=> kuName <^> Language.kupplung <-> Language.anschluss <=> kupplungsAnschluss

instance StreckenObjekt Kupplung where
    anschlüsse :: Kupplung -> [Anschluss]
    anschlüsse Kupplung {kupplungsAnschluss} = [kupplungsAnschluss]

    erhalteName :: Kupplung -> Text
    erhalteName Kupplung {kuName} = kuName

instance StreckenAtom Kupplung where
    fließend :: Kupplung -> Value
    fließend = kuFließend

-- | Sammel-Klasse für 'Kupplung'-artige Typen
class (StreckenObjekt k) => KupplungKlasse k where
    -- | Kupplung betätigen
    kuppeln :: (I2CReader r m, MonadIO m) => k -> m ()
    {-# MINIMAL kuppeln #-}

instance (KupplungKlasse (ku 'Märklin), KupplungKlasse (ku 'Lego))
    => KupplungKlasse (ZugtypEither ku) where
    kuppeln :: (I2CReader r m, MonadIO m) => ZugtypEither ku -> m ()
    kuppeln (ZugtypMärklin a) = kuppeln a
    kuppeln (ZugtypLego a) = kuppeln a

-- | Zeit, die Strom beim Kuppeln anliegt
kuppelnZeit :: Wartezeit
kuppelnZeit = MilliSekunden 300

instance KupplungKlasse Kupplung where
    kuppeln :: (I2CReader r m, MonadIO m) => Kupplung -> m ()
    kuppeln ku@Kupplung {kupplungsAnschluss} =
        befehlAusführen
            (anschlussWrite kupplungsAnschluss (fließend ku)
             >> warte kuppelnZeit
             >> anschlussWrite kupplungsAnschluss (gesperrt ku))
            ("Kuppeln (" <> showText kupplungsAnschluss <> ")")

-- | Zusammenfassung von Einzel-Elementen. Weichen haben eine vorgegebene Richtung.
data Wegstrecke (z :: Zugtyp) =
    Wegstrecke
    { wsName :: Text
    , wsBahngeschwindigkeiten :: [GeschwindigkeitEither Bahngeschwindigkeit z]
    , wsStreckenabschnitte :: [Streckenabschnitt]
    , wsWeichenRichtungen :: [(Weiche z, Richtung)]
    , wsKupplungen :: [Kupplung]
    }
    deriving (Eq, Show)

instance Anzeige (Wegstrecke z) where
    anzeige :: Wegstrecke z -> Sprache -> Text
    anzeige
        Wegstrecke
        {wsName, wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen} =
        Language.wegstrecke
        <:> Language.name
        <=> wsName
        <^> Language.bahngeschwindigkeiten
        <=> wsBahngeschwindigkeiten
        <^> Language.streckenabschnitte
        <=> wsStreckenabschnitte
        <^> Language.weichen
        <=> map (uncurry (<°>)) wsWeichenRichtungen <^> Language.kupplungen <=> wsKupplungen

instance StreckenObjekt (Wegstrecke z) where
    anschlüsse :: Wegstrecke z -> [Anschluss]
    anschlüsse
        Wegstrecke
        {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen} =
        join
        $ map anschlüsse wsBahngeschwindigkeiten
        ++ map anschlüsse wsStreckenabschnitte
        ++ map (anschlüsse . fst) wsWeichenRichtungen
        ++ map anschlüsse wsKupplungen

    erhalteName :: Wegstrecke z -> Text
    erhalteName Wegstrecke {wsName} = wsName

instance BahngeschwindigkeitKlasse (GeschwindigkeitPhantom Wegstrecke) where
    geschwindigkeit :: (I2CReader r m, PwmReader r m, MonadIO m)
                    => GeschwindigkeitPhantom Wegstrecke 'Pwm z
                    -> Word8
                    -> m ()
    geschwindigkeit (GeschwindigkeitPhantom ws@Wegstrecke {wsBahngeschwindigkeiten}) wert =
        befehlAusführen
            (mapM_ (forkI2CReader . flip geschwindigkeit wert) $ catPwm wsBahngeschwindigkeiten)
            ("Geschwindigkeit (" <> showText ws <> ")->" <> showText wert)

    umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m)
             => GeschwindigkeitPhantom Wegstrecke b 'Märklin
             -> m ()
    umdrehen (GeschwindigkeitPhantom ws@Wegstrecke {wsBahngeschwindigkeiten}) =
        flip befehlAusführen ("Umdrehen (" <> showText ws <> ")") $ do
            geschwindigkeit (GeschwindigkeitPhantom ws) 0
            fahrstrom (GeschwindigkeitPhantom ws) 0
            warte umdrehenZeit
            strom ws Fließend
            mapM_ (forkI2CReader . umdrehen) geschwindigkeitenPwm
            liftIO $ forM_ umdrehenPins $ \(pin, valueFunktion) -> forkIO $ do
                pinMode pin OUTPUT
                digitalWrite pin $ valueFunktion Fließend
                warte umdrehenZeit
                digitalWrite pin $ valueFunktion Gesperrt
            forM_ (Map.toList umdrehenPortMapHigh) $ \(pcf8574, ports) -> forkI2CReader $ do
                pcf8574MultiPortWrite pcf8574 ports HIGH
                warte umdrehenZeit
                pcf8574MultiPortWrite pcf8574 ports LOW
            forM_ (Map.toList umdrehenPortMapLow) $ \(pcf8574, ports) -> forkI2CReader $ do
                pcf8574MultiPortWrite pcf8574 ports LOW
                warte umdrehenZeit
                pcf8574MultiPortWrite pcf8574 ports HIGH
        where
            (geschwindigkeitenPwm, geschwindigkeitenKonstanteSpannung) =
                foldl splitGeschwindigkeiten ([], []) wsBahngeschwindigkeiten

            splitGeschwindigkeiten
                :: ( [Bahngeschwindigkeit 'Pwm 'Märklin]
                   , [Bahngeschwindigkeit 'KonstanteSpannung 'Märklin]
                   )
                -> GeschwindigkeitEither Bahngeschwindigkeit 'Märklin
                -> ( [Bahngeschwindigkeit 'Pwm 'Märklin]
                   , [Bahngeschwindigkeit 'KonstanteSpannung 'Märklin]
                   )
            splitGeschwindigkeiten (p, k) (GeschwindigkeitPwm bg) = (bg : p, k)
            splitGeschwindigkeiten (p, k) (GeschwindigkeitKonstanteSpannung bg) = (p, bg : k)

            (umdrehenPins, umdrehenPcf8574PortsHigh, umdrehenPcf8574PortsLow) =
                foldl splitAnschlüsse ([], [], []) geschwindigkeitenKonstanteSpannung

            splitAnschlüsse
                :: ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
                -> Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
                -> ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                bg@MärklinBahngeschwindigkeitKonstanteSpannung
                {bgmkUmdrehenAnschluss = AnschlussPin pin} =
                ((pin, flip erhalteValue bg) : pins, portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                MärklinBahngeschwindigkeitKonstanteSpannung
                {bgmkFließend = HIGH, bgmkUmdrehenAnschluss = AnschlussPCF8574Port port} =
                (pins, port : portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                MärklinBahngeschwindigkeitKonstanteSpannung
                {bgmkFließend = LOW, bgmkUmdrehenAnschluss = AnschlussPCF8574Port port} =
                (pins, portsHigh, port : portsLow)

            umdrehenPortMapHigh = pcf8574Gruppieren umdrehenPcf8574PortsHigh

            umdrehenPortMapLow = pcf8574Gruppieren umdrehenPcf8574PortsLow

    fahrstrom :: (I2CReader r m, MonadIO m)
              => GeschwindigkeitPhantom Wegstrecke 'KonstanteSpannung z
              -> Word8
              -> m ()
    fahrstrom (GeschwindigkeitPhantom ws@Wegstrecke {wsBahngeschwindigkeiten}) fahrstromAnschluss =
        flip
            befehlAusführen
            ("Fahrstrom (" <> showText ws <> ")->" <> showText fahrstromAnschluss)
        $ do
            liftIO $ forM_ fahrstromPins $ uncurry digitalWrite
            forM_ (Map.toList fahrstromPortMapHigh)
                $ \(pcf8574, ports) -> forkI2CReader $ pcf8574MultiPortWrite pcf8574 ports HIGH
            forM_ (Map.toList fahrstromPortMapLow)
                $ \(pcf8574, ports) -> forkI2CReader $ pcf8574MultiPortWrite pcf8574 ports LOW
        where
            (fahrstromPins, fahrstromPcf8574PortsHigh, fahrstromPcf8574PortsLow) =
                foldl splitBahngeschwindigkeiten ([], [], [])
                $ catKonstanteSpannung wsBahngeschwindigkeiten

            splitBahngeschwindigkeiten :: ([(Pin, Value)], [PCF8574Port], [PCF8574Port])
                                       -> Bahngeschwindigkeit 'KonstanteSpannung z
                                       -> ([(Pin, Value)], [PCF8574Port], [PCF8574Port])
            splitBahngeschwindigkeiten
                acc
                bg@MärklinBahngeschwindigkeitKonstanteSpannung {bgmkFahrstromAnschlüsse} =
                foldl (splitAnschlüsse bg) acc bgmkFahrstromAnschlüsse

            splitAnschlüsse :: Bahngeschwindigkeit 'KonstanteSpannung z
                             -> ([(Pin, Value)], [PCF8574Port], [PCF8574Port])
                             -> Anschluss
                             -> ([(Pin, Value)], [PCF8574Port], [PCF8574Port])
            splitAnschlüsse bg (pins, portsHigh, portsLow) anschluss@(AnschlussPin pin) =
                ((pin, anschlussValue bg anschluss) : pins, portsHigh, portsLow)
            splitAnschlüsse bg (pins, portsHigh, portsLow) anschluss@(AnschlussPCF8574Port port)
                | anschlussValue bg anschluss == HIGH = (pins, port : portsHigh, portsLow)
                | otherwise = (pins, portsHigh, port : portsLow)

            fahrstromPortMapHigh = pcf8574Gruppieren fahrstromPcf8574PortsHigh

            fahrstromPortMapLow = pcf8574Gruppieren fahrstromPcf8574PortsLow

            anschlussValue :: Bahngeschwindigkeit 'KonstanteSpannung z -> Anschluss -> Value
            anschlussValue
                bg@MärklinBahngeschwindigkeitKonstanteSpannung {bgmkFahrstromAnschlüsse}
                anschluss
                | positionOderLetztes fahrstromAnschluss bgmkFahrstromAnschlüsse
                    == (Just anschluss) =
                    fließend bg
                | otherwise = gesperrt bg

    fahrtrichtungEinstellen :: (I2CReader r m, PwmReader r m, MonadIO m)
                            => GeschwindigkeitPhantom Wegstrecke b 'Lego
                            -> Fahrtrichtung
                            -> m ()
    fahrtrichtungEinstellen
        (GeschwindigkeitPhantom ws@Wegstrecke {wsBahngeschwindigkeiten})
        neueFahrtrichtung =
        flip
            befehlAusführen
            ("Fahrtrichtung (" <> showText ws <> ")->" <> showText neueFahrtrichtung)
        $ do
            geschwindigkeit (GeschwindigkeitPhantom ws) 0
            fahrstrom (GeschwindigkeitPhantom ws) 0
            warte umdrehenZeit
            strom ws Fließend
            liftIO $ forM_ fahrtrichtungsPins $ \(pin, valueFunktion) -> forkIO $ do
                pinMode pin OUTPUT
                digitalWrite pin $ valueFunktion $ case neueFahrtrichtung of
                    Vorwärts -> Fließend
                    Rückwärts -> Gesperrt
            forM_ (Map.toList fahrtrichtungPortMapHigh) $ \(pcf8574, ports)
                -> pcf8574MultiPortWrite pcf8574 ports $ case neueFahrtrichtung of
                    Vorwärts -> HIGH
                    Rückwärts -> LOW
            forM_ (Map.toList fahrtrichtungPortMapLow) $ \(pcf8574, ports)
                -> pcf8574MultiPortWrite pcf8574 ports $ case neueFahrtrichtung of
                    Vorwärts -> LOW
                    Rückwärts -> HIGH
        where
            (fahrtrichtungsPins, fahrtrichtungPcf8574PortsHigh, fahrtrichtungPcf8574PortsLow) =
                foldl splitAnschlüsse ([], [], []) wsBahngeschwindigkeiten

            splitAnschlüsse
                :: ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
                -> GeschwindigkeitEither Bahngeschwindigkeit 'Lego
                -> ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                (GeschwindigkeitPwm
                     bg@LegoBahngeschwindigkeit {bglFahrtrichtungsAnschluss = AnschlussPin pin}) =
                ((pin, flip erhalteValue bg) : pins, portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                (GeschwindigkeitPwm
                     LegoBahngeschwindigkeit
                     {bglFließend = HIGH, bglFahrtrichtungsAnschluss = AnschlussPCF8574Port port}) =
                (pins, port : portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                (GeschwindigkeitPwm
                     LegoBahngeschwindigkeit
                     {bglFließend = LOW, bglFahrtrichtungsAnschluss = AnschlussPCF8574Port port}) =
                (pins, portsHigh, port : portsLow)
            splitAnschlüsse _acc (GeschwindigkeitKonstanteSpannung _) =
                error "Lego-Bahngeschwindigkeit mit konstanter Spannung!"

            fahrtrichtungPortMapHigh = pcf8574Gruppieren fahrtrichtungPcf8574PortsHigh

            fahrtrichtungPortMapLow = pcf8574Gruppieren fahrtrichtungPcf8574PortsLow

instance StreckenabschnittKlasse (Wegstrecke z) where
    strom :: (I2CReader r m, MonadIO m) => Wegstrecke z -> Strom -> m ()
    strom ws@Wegstrecke {wsStreckenabschnitte} an =
        flip befehlAusführen ("Strom (" <> showText ws <> ")->" <> showText an) $ do
            liftIO $ forM_ stromPins $ \(pin, valueFunktion) -> digitalWrite pin $ valueFunktion an
            forM_ (Map.toList stromPortMapHigh)
                $ \(pcf8574, ports) -> forkI2CReader $ pcf8574MultiPortWrite pcf8574 ports HIGH
            forM_ (Map.toList stromPortMapLow)
                $ \(pcf8574, ports) -> forkI2CReader $ pcf8574MultiPortWrite pcf8574 ports LOW
        where
            (stromPins, stromPcf8574PortsHigh, stromPcf8574PortsLow) =
                foldl splitAnschlüsse ([], [], []) wsStreckenabschnitte

            splitAnschlüsse
                :: ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
                -> Streckenabschnitt
                -> ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                st@Streckenabschnitt {stromAnschluss = AnschlussPin pin} =
                ((pin, flip erhalteValue st) : pins, portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                Streckenabschnitt {stFließend = HIGH, stromAnschluss = AnschlussPCF8574Port port} =
                (pins, port : portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                Streckenabschnitt {stFließend = LOW, stromAnschluss = AnschlussPCF8574Port port} =
                (pins, portsHigh, port : portsLow)

            stromPortMapHigh = pcf8574Gruppieren stromPcf8574PortsHigh

            stromPortMapLow = pcf8574Gruppieren stromPcf8574PortsLow

instance KupplungKlasse (Wegstrecke z) where
    kuppeln :: (I2CReader r m, MonadIO m) => Wegstrecke z -> m ()
    kuppeln ws@Wegstrecke {wsKupplungen} =
        flip befehlAusführen ("Kuppeln (" <> showText ws <> ")") $ do
            liftIO $ forM_ kupplungsPins $ \(pin, valueFunktion) -> forkIO $ do
                pinMode pin OUTPUT
                digitalWrite pin $ valueFunktion Fließend
                warte kuppelnZeit
                digitalWrite pin $ valueFunktion Gesperrt
            forM_ (Map.toList kupplungsPortMapHigh) $ \(pcf8574, ports) -> forkI2CReader $ do
                pcf8574MultiPortWrite pcf8574 ports HIGH
                warte kuppelnZeit
                pcf8574MultiPortWrite pcf8574 ports LOW
            forM_ (Map.toList kupplungsPortMapLow) $ \(pcf8574, ports) -> forkI2CReader $ do
                pcf8574MultiPortWrite pcf8574 ports LOW
                warte kuppelnZeit
                pcf8574MultiPortWrite pcf8574 ports HIGH
        where
            (kupplungsPins, kupplungsPcf8574PortsHigh, kupplungsPcf8574PortsLow) =
                foldl splitAnschlüsse ([], [], []) wsKupplungen

            splitAnschlüsse
                :: ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
                -> Kupplung
                -> ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                ku@Kupplung {kupplungsAnschluss = AnschlussPin pin} =
                ((pin, flip erhalteValue ku) : pins, portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                Kupplung {kuFließend = HIGH, kupplungsAnschluss = AnschlussPCF8574Port port} =
                (pins, port : portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                Kupplung {kuFließend = LOW, kupplungsAnschluss = AnschlussPCF8574Port port} =
                (pins, portsHigh, port : portsLow)

            kupplungsPortMapHigh = pcf8574Gruppieren kupplungsPcf8574PortsHigh

            kupplungsPortMapLow = pcf8574Gruppieren kupplungsPcf8574PortsLow

-- | Sammel-Klasse für 'Wegstrecke'n-artige Typen
class (StreckenObjekt w, StreckenabschnittKlasse w, KupplungKlasse w) => WegstreckeKlasse w where
    einstellen :: (I2CReader r m, PwmReader r m, MonadIO m) => w -> m ()
    {-# MINIMAL einstellen #-}

instance (WegstreckeKlasse (w 'Märklin), WegstreckeKlasse (w 'Lego))
    => WegstreckeKlasse (ZugtypEither w) where
    einstellen :: (I2CReader r m, PwmReader r m, MonadIO m) => ZugtypEither w -> m ()
    einstellen (ZugtypMärklin a) = einstellen a
    einstellen (ZugtypLego a) = einstellen a

instance WegstreckeKlasse (Wegstrecke 'Märklin) where
    einstellen :: (I2CReader r m, PwmReader r m, MonadIO m) => Wegstrecke 'Märklin -> m ()
    einstellen ws@Wegstrecke {wsWeichenRichtungen} =
        flip befehlAusführen ("Einstellen (" <> showText ws <> ")") $ do
            liftIO $ forM_ richtungsPins $ \(pin, valueFunktion) -> forkIO $ do
                pinMode pin OUTPUT
                digitalWrite pin $ valueFunktion Fließend
                warte weicheZeit
                digitalWrite pin $ valueFunktion Gesperrt
            forM_ (Map.toList richtungsPortMapHigh) $ \(pcf8574, ports) -> forkI2CReader $ do
                pcf8574MultiPortWrite pcf8574 ports HIGH
                warte weicheZeit
                pcf8574MultiPortWrite pcf8574 ports LOW
            forM_ (Map.toList richtungsPortMapLow) $ \(pcf8574, ports) -> forkI2CReader $ do
                pcf8574MultiPortWrite pcf8574 ports LOW
                warte weicheZeit
                pcf8574MultiPortWrite pcf8574 ports HIGH
        where
            (richtungsPins, richtungsPcf8574PortsHigh, richtungsPcf8574PortsLow) =
                foldl splitAnschlüsse ([], [], []) wsWeichenRichtungen

            splitAnschlüsse
                :: ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
                -> (Weiche 'Märklin, Richtung)
                -> ( [( Pin
                      , Strom
                            -> Value
                      )]
                   , [PCF8574Port]
                   , [PCF8574Port]
                   )
            splitAnschlüsse
                acc@(pins, portsHigh, portsLow)
                (we@MärklinWeiche {wemFließend, wemRichtungsAnschlüsse}, richtung) =
                case getRichtungsAnschluss richtung (NE.toList wemRichtungsAnschlüsse) of
                    (Just (AnschlussPin pin))
                        -> ((pin, flip erhalteValue we) : pins, portsHigh, portsLow)
                    (Just (AnschlussPCF8574Port port)) -> case wemFließend of
                        HIGH -> (pins, port : portsHigh, portsLow)
                        LOW -> (pins, portsHigh, port : portsLow)
                    Nothing -> acc

            getRichtungsAnschluss :: Richtung -> [(Richtung, Anschluss)] -> Maybe Anschluss
            getRichtungsAnschluss _richtung [] = Nothing
            getRichtungsAnschluss richtung ((ersteRichtung, ersterAnschluss):andereRichtungen)
                | richtung == ersteRichtung = Just ersterAnschluss
                | otherwise = getRichtungsAnschluss richtung andereRichtungen

            richtungsPortMapHigh = pcf8574Gruppieren richtungsPcf8574PortsHigh

            richtungsPortMapLow = pcf8574Gruppieren richtungsPcf8574PortsLow

instance WegstreckeKlasse (Wegstrecke 'Lego) where
    einstellen :: (I2CReader r m, PwmReader r m, MonadIO m) => Wegstrecke 'Lego -> m ()
    einstellen Wegstrecke {wsWeichenRichtungen} =
        mapM_ (forkI2CReader . uncurry stellen) wsWeichenRichtungen

-- | Ausführen einer IO-Aktion, bzw. Ausgabe eines Strings, abhängig vom Kommandozeilen-Argument
befehlAusführen :: (MonadIO m) => m () -> Text -> m ()
befehlAusführen ioAction ersatzNachricht = do
    Options {printCmd} <- getOptions
    if printCmd
        then liftIO $ T.putStrLn ersatzNachricht
        else ioAction