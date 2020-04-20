{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung.
-}
module Zug.Anbindung.Bahngeschwindigkeit
  ( -- * Datentyp und Typ-Klasse
    Bahngeschwindigkeit(..)
  , BahngeschwindigkeitKlasse(..)
    -- * Hilfsfunktionen
  , verwendetPwm
  , umdrehenZeit
  , positionOderLetztes
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO())
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Semigroup (Semigroup((<>)))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word8)

import Zug.Anbindung.Anschluss
       (Value(..), Anschluss(..), Pin(), PCF8574Port(), I2CReader(forkI2CReader)
      , AnschlussKlasse(..), pcf8574MultiPortWrite, pcf8574Gruppieren)
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..), befehlAusführen)
import Zug.Anbindung.Pwm
       (PwmReader(), pwmSetzeWert, erhaltePwmWertVoll, erhaltePwmWertReduziert, pwmGrenze)
import Zug.Anbindung.Wartezeit (Wartezeit(..), warte)
import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..), Fahrtrichtung(Vorwärts))
import Zug.Language (Anzeige(..), Sprache(), showText, (<->), (<:>), (<=>), (<^>))
import qualified Zug.Language as Language

-- | Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung.
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

instance Ord (Bahngeschwindigkeit b z) where
    compare :: Bahngeschwindigkeit b z -> Bahngeschwindigkeit b z -> Ordering
    compare
        LegoBahngeschwindigkeit { bglName = n0
                                , bglFließend = f0
                                , bglGeschwindigkeitsPin = gp0
                                , bglFahrtrichtungsAnschluss = fa0}
        LegoBahngeschwindigkeit { bglName = n1
                                , bglFließend = f1
                                , bglGeschwindigkeitsPin = gp1
                                , bglFahrtrichtungsAnschluss = fa1} = case compare n0 n1 of
        EQ -> case compare f0 f1 of
            EQ -> case compare gp0 gp1 of
                EQ -> compare fa0 fa1
                ordering -> ordering
            ordering -> ordering
        ordering -> ordering
    compare
        MärklinBahngeschwindigkeitPwm
        {bgmpName = n0, bgmpFließend = f0, bgmpGeschwindigkeitsPin = gp0}
        MärklinBahngeschwindigkeitPwm
        {bgmpName = n1, bgmpFließend = f1, bgmpGeschwindigkeitsPin = gp1} = case compare n0 n1 of
        EQ -> case compare f0 f1 of
            EQ -> compare gp0 gp1
            ordering -> ordering
        ordering -> ordering
    compare
        MärklinBahngeschwindigkeitKonstanteSpannung
        { bgmkName = n0
        , bgmkFließend = f0
        , bgmkFahrstromAnschlüsse = fa0
        , bgmkUmdrehenAnschluss = ua0}
        MärklinBahngeschwindigkeitKonstanteSpannung
        { bgmkName = n1
        , bgmkFließend = f1
        , bgmkFahrstromAnschlüsse = fa1
        , bgmkUmdrehenAnschluss = ua1} = case compare n0 n1 of
        EQ -> case compare f0 f1 of
            EQ -> case compare fa0 fa1 of
                EQ -> compare ua0 ua1
                ordering -> ordering
            ordering -> ordering
        ordering -> ordering

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
    anschlüsse :: Bahngeschwindigkeit b z -> Set Anschluss
    anschlüsse LegoBahngeschwindigkeit {bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss} =
        [AnschlussPin bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss]
    anschlüsse MärklinBahngeschwindigkeitPwm {bgmpGeschwindigkeitsPin} =
        [AnschlussPin bgmpGeschwindigkeitsPin]
    anschlüsse
        MärklinBahngeschwindigkeitKonstanteSpannung
        {bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss} =
        Set.insert bgmkUmdrehenAnschluss $ Set.fromList $ NonEmpty.toList bgmkFahrstromAnschlüsse

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
            (pwmSetzeWert bg bgmpGeschwindigkeitsPin $ erhaltePwmWertReduziert geschwindigkeit)
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
        flip
            befehlAusführen
            ("Fahrstrom ("
             <> showText bgmkFahrstromAnschlüsse
             <> ")->"
             <> showText fahrstromAnschluss)
        $ do
            anschlussWrite bgmkUmdrehenAnschluss $ gesperrt bg
            forM_ fahrstromPins $ \(pin, value) -> forkI2CReader $ anschlussWrite pin value
            forM_ (Map.toList fahrstromPortMapHigh)
                $ \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports HIGH
            forM_ (Map.toList fahrstromPortMapLow)
                $ \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports LOW
        where
            (fahrstromPins, fahrstromPcf8574PortsHigh, fahrstromPcf8574PortsLow) =
                foldl splitAnschlüsse ([], [], []) bgmkFahrstromAnschlüsse

            splitAnschlüsse :: ([(Pin, Value)], [PCF8574Port], [PCF8574Port])
                             -> Anschluss
                             -> ([(Pin, Value)], [PCF8574Port], [PCF8574Port])
            splitAnschlüsse (pins, portsHigh, portsLow) anschluss@AnschlussPin {pin} =
                ((pin, anschlussValue anschluss) : pins, portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                anschluss@AnschlussPCF8574Port {pcf8574Port}
                | anschlussValue anschluss == HIGH = (pins, pcf8574Port : portsHigh, portsLow)
                | otherwise = (pins, portsHigh, pcf8574Port : portsLow)

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
        flip befehlAusführen ("Umdrehen (" <> showText bgmpGeschwindigkeitsPin <> ")") $ do
            pwmSetzeWert bg bgmpGeschwindigkeitsPin $ erhaltePwmWertVoll (0 :: Word)
            warte umdrehenZeit
            pwmSetzeWert bg bgmpGeschwindigkeitsPin $ erhaltePwmWertVoll pwmGrenze
            warte umdrehenZeit
            pwmSetzeWert bg bgmpGeschwindigkeitsPin $ erhaltePwmWertVoll (0 :: Word)
    umdrehen bg@MärklinBahngeschwindigkeitKonstanteSpannung {bgmkUmdrehenAnschluss} =
        flip befehlAusführen ("Umdrehen (" <> showText bgmkUmdrehenAnschluss <> ")") $ do
            fahrstrom bg 0
            warte umdrehenZeit
            anschlussWrite bgmkUmdrehenAnschluss $ fließend bg
            warte umdrehenZeit
            anschlussWrite bgmkUmdrehenAnschluss $ gesperrt bg

    fahrtrichtungEinstellen :: (I2CReader r m, PwmReader r m, MonadIO m)
                            => Bahngeschwindigkeit b 'Lego
                            -> Fahrtrichtung
                            -> m ()
    fahrtrichtungEinstellen
        bg@LegoBahngeschwindigkeit {bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss}
        fahrtrichtung =
        flip
            befehlAusführen
            ("Umdrehen ("
             <> (showText bglGeschwindigkeitsPin <^> showText bglFahrtrichtungsAnschluss
                 $ Language.Deutsch)
             <> ")->"
             <> showText fahrtrichtung)
        $ do
            pwmSetzeWert bg bglGeschwindigkeitsPin $ erhaltePwmWertVoll (0 :: Word)
            warte umdrehenZeit
            anschlussWrite bglFahrtrichtungsAnschluss
                $ (if fahrtrichtung == Vorwärts
                       then fließend
                       else gesperrt)
                    bg