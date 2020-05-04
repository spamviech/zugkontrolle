{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description: Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung.
-}
module Zug.Anbindung.Bahngeschwindigkeit
  ( -- * Datentyp und Typ-Klasse
    Bahngeschwindigkeit(..)
  , BahngeschwindigkeitKlasse(..)
  , PwmZugtyp()
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
       (Value(..), AnschlussEither(..), Anschluss(..), MitInterruptPin(OhneInterruptPin)
      , PCF8574Klasse(ohneInterruptPin), Pin(), PCF8574Port(), I2CReader(forkI2CReader)
      , AnschlussKlasse(anschlussWrite, zuAnschluss), pcf8574MultiPortWrite, pcf8574Gruppieren)
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..), befehlAusführen)
import Zug.Anbindung.Pwm (PwmReader(), pwmSetzeWert, PwmValueUnmodifiziert, erhaltePwmWertVoll
                        , erhaltePwmWertReduziert, pwmGrenze)
import Zug.Anbindung.Wartezeit (Wartezeit(..), warte)
import Zug.Derive.Ord (deriveOrd)
import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..), Fahrtrichtung(Vorwärts))
import Zug.Language (Anzeige(..), Sprache(), showText, (<->), (<:>), (<=>), (<^>))
import qualified Zug.Language as Language

data GeschwindigkeitsAnschlüsse (g :: GeschwindigkeitVariante) where
    GeschwindigkeitsPin :: { geschwindigkeitsPin :: Pin } -> GeschwindigkeitsAnschlüsse 'Pwm
    FahrstromAnschlüsse :: { fahrstromAnschlüsse :: NonEmpty AnschlussEither }
        -> GeschwindigkeitsAnschlüsse 'KonstanteSpannung

deriving instance Eq (GeschwindigkeitsAnschlüsse g)

deriving instance Ord (GeschwindigkeitsAnschlüsse g)

deriving instance Show (GeschwindigkeitsAnschlüsse g)

instance Anzeige (GeschwindigkeitsAnschlüsse g) where
    anzeige :: GeschwindigkeitsAnschlüsse g -> Sprache -> Text
    anzeige = _undefined --TODO

data FahrtrichtungsAnschluss (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
    KeinExpliziterAnschluss :: FahrtrichtungsAnschluss 'Pwm 'Märklin
    UmdrehenAnschluss :: { umdrehenAnschluss :: AnschlussEither }
        -> FahrtrichtungsAnschluss 'KonstanteSpannung 'Märklin
    FahrtrichtungsAnschluss :: { fahrtrichtungsAnschluss :: AnschlussEither }
        -> FahrtrichtungsAnschluss g 'Lego

deriving instance Eq (FahrtrichtungsAnschluss g z)

deriveOrd $ Left ''FahrtrichtungsAnschluss

deriving instance Show (FahrtrichtungsAnschluss g z)

instance Anzeige (FahrtrichtungsAnschluss g z) where
    anzeige :: FahrtrichtungsAnschluss g z -> Sprache -> Text
    anzeige = _undefined --TODO

-- | Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung.
data Bahngeschwindigkeit (g :: GeschwindigkeitVariante) (z :: Zugtyp) =
    Bahngeschwindigkeit
    { bgName :: Text
    , bgFließend :: Value
    , bgGeschwindigkeitsAnschlüsse :: GeschwindigkeitsAnschlüsse g
    , bgFahrtrichtungsAnschluss :: FahrtrichtungsAnschluss g z
    }
    deriving (Eq, Ord, Show)

-- BahngeschwindigkeitPwmMärklin
--     :: { bgmpName :: Text, bgmpFließend :: Value, bgmpGeschwindigkeitsPin :: Pin }
--     -> Bahngeschwindigkeit 'Pwm 'Märklin
-- BahngeschwindigkeitKonstanteSpannungMärklin
--     :: { bgmkName :: Text
--        , bgmkFließend :: Value
--        , bgmkFahrstromAnschlüsse :: NonEmpty AnschlussEither
--        , bgmkUmdrehenAnschluss :: AnschlussEither
--        } -> Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
-- BahngeschwindigkeitPwmLego
--     :: { bglpName :: Text
--        , bglpFließend :: Value
--        , bglpGeschwindigkeitsPin :: Pin
--        , bglpFahrtrichtungsAnschluss :: AnschlussEither
--        } -> Bahngeschwindigkeit 'Pwm 'Lego
-- BahngeschwindigkeitKonstanteSpannungLego
--     :: { bglkName :: Text
--        , bglkFließend :: Value
--        , bglkFahrstromAnschlüsse :: NonEmpty AnschlussEither
--        , bglkFahrtrichtungsAnschluss :: AnschlussEither
--        } -> Bahngeschwindigkeit 'KonstanteSpannung 'Lego
-- deriveOrd $ Left ''Bahngeschwindigkeit
-- deriving instance Eq (Bahngeschwindigkeit g z)
-- deriving instance Show (Bahngeschwindigkeit g z)
instance Anzeige (Bahngeschwindigkeit g z) where
    anzeige :: Bahngeschwindigkeit g z -> Sprache -> Text

    -- anzeige BahngeschwindigkeitPwmMärklin {bgmpName, bgmpGeschwindigkeitsPin} =
    --     Language.märklin
    --     <-> Language.bahngeschwindigkeit
    --     <:> Language.name
    --     <=> bgmpName <^> Language.geschwindigkeit <-> Language.pin <=> bgmpGeschwindigkeitsPin
    -- anzeige
    --     BahngeschwindigkeitKonstanteSpannungMärklin
    --     {bgmkName, bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss} =
    --     Language.märklin
    --     <-> Language.bahngeschwindigkeit
    --     <:> Language.name
    --     <=> bgmkName
    --     <^> Language.fahrstrom
    --     <-> Language.anschlüsse
    --     <=> bgmkFahrstromAnschlüsse
    --     <^> Language.umdrehen <-> Language.anschluss <=> bgmkUmdrehenAnschluss
    -- anzeige
    --     BahngeschwindigkeitPwmLego {bglpName, bglpGeschwindigkeitsPin, bglpFahrtrichtungsAnschluss} =
    --     Language.lego
    --     <-> Language.bahngeschwindigkeit
    --     <:> Language.name
    --     <=> bglpName
    --     <^> Language.geschwindigkeit
    --     <-> Language.pin
    --     <=> bglpGeschwindigkeitsPin
    --     <^> Language.fahrtrichtung <-> Language.anschluss <=> bglpFahrtrichtungsAnschluss
    anzeige = _undefined --TODO

instance StreckenObjekt (Bahngeschwindigkeit b z) where
    anschlüsse :: Bahngeschwindigkeit b z -> Set AnschlussEither

    -- anschlüsse BahngeschwindigkeitPwmMärklin {bgmpGeschwindigkeitsPin} =
    --     [zuAnschluss bgmpGeschwindigkeitsPin]
    -- anschlüsse
    --     BahngeschwindigkeitKonstanteSpannungMärklin
    --     {bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss} =
    --     Set.insert bgmkUmdrehenAnschluss $ Set.fromList $ NonEmpty.toList bgmkFahrstromAnschlüsse
    -- anschlüsse BahngeschwindigkeitPwmLego {bglpGeschwindigkeitsPin, bglpFahrtrichtungsAnschluss} =
    --     [zuAnschluss bglpGeschwindigkeitsPin, bglpFahrtrichtungsAnschluss]
    anschlüsse = _undefined --TODO

    erhalteName :: Bahngeschwindigkeit b z -> Text

    -- erhalteName BahngeschwindigkeitPwmLego {bglpName} = bglpName
    -- erhalteName BahngeschwindigkeitPwmMärklin {bgmpName} = bgmpName
    -- erhalteName BahngeschwindigkeitKonstanteSpannungMärklin {bgmkName} = bgmkName
    erhalteName = _undefined --TODO

instance StreckenAtom (Bahngeschwindigkeit b z) where
    fließend :: Bahngeschwindigkeit b z -> Value

    -- fließend BahngeschwindigkeitPwmMärklin {bgmpFließend} = bgmpFließend
    -- fließend BahngeschwindigkeitKonstanteSpannungMärklin {bgmkFließend} = bgmkFließend
    -- fließend BahngeschwindigkeitPwmLego {bglpFließend} = bglpFließend
    fließend = _undefined --TODO

-- | Verwendet die 'Bahngeschwindigkeit' PWM zur Geschwindigkeitskontrolle?
verwendetPwm :: Bahngeschwindigkeit g z -> GeschwindigkeitVariante
verwendetPwm Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {}} = Pwm
verwendetPwm Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {}} =
    KonstanteSpannung

-- | Sammel-Klasse für 'Bahngeschwindigkeit'-artige Typen.
class ( StreckenObjekt (b 'Pwm 'Märklin)
      , StreckenObjekt (b 'Pwm 'Lego)
      , StreckenObjekt (b 'KonstanteSpannung 'Märklin)
      , StreckenObjekt (b 'KonstanteSpannung 'Lego)
      ) => BahngeschwindigkeitKlasse b where
    -- | Geschwindigkeit einstellen (akzeptiere Werte von 0 bis 100)
    geschwindigkeit
        :: (I2CReader r m, PwmReader r m, PwmZugtyp z, MonadIO m) => b 'Pwm z -> Word8 -> m ()

    -- | Fahrstrom ein-/ausschalten
    fahrstrom :: (I2CReader r m, MonadIO m) => b 'KonstanteSpannung z -> Word8 -> m ()

    -- | Gebe allen Zügen den Befehl zum Umdrehen
    umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m) => b g 'Märklin -> m ()

    -- | Gebe allen Zügen den Befehl in einer bestimmen Richtung zu fahren
    fahrtrichtungEinstellen
        :: (I2CReader r m, PwmReader r m, MonadIO m) => b g 'Lego -> Fahrtrichtung -> m ()

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

class PwmZugtyp (z :: Zugtyp) where
    erhaltePwmWert :: a z -> Word8 -> PwmValueUnmodifiziert

instance PwmZugtyp 'Märklin where
    erhaltePwmWert :: a 'Märklin -> Word8 -> PwmValueUnmodifiziert
    erhaltePwmWert _bg = erhaltePwmWertReduziert

instance PwmZugtyp 'Lego where
    erhaltePwmWert :: a 'Lego -> Word8 -> PwmValueUnmodifiziert
    erhaltePwmWert _bg = erhaltePwmWertVoll

instance BahngeschwindigkeitKlasse Bahngeschwindigkeit where
    geschwindigkeit :: (I2CReader r m, PwmReader r m, PwmZugtyp z, MonadIO m)
                    => Bahngeschwindigkeit 'Pwm z
                    -> Word8
                    -> m ()
    geschwindigkeit
        bg@Bahngeschwindigkeit
        {bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin}}
        geschwindigkeit =
        befehlAusführen
            (pwmSetzeWert bg geschwindigkeitsPin $ erhaltePwmWert bg geschwindigkeit)
            ("Geschwindigkeit ("
             <> showText geschwindigkeitsPin
             <> ")->"
             <> showText geschwindigkeit)

    fahrstrom
        :: (I2CReader r m, MonadIO m) => Bahngeschwindigkeit 'KonstanteSpannung z -> Word8 -> m ()
    fahrstrom
        bg@Bahngeschwindigkeit
        { bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {fahrstromAnschlüsse}
        , bgFahrtrichtungsAnschluss}
        fahrstromAnschluss =
        flip
            befehlAusführen
            ("Fahrstrom (" <> showText fahrstromAnschlüsse <> ")->" <> showText fahrstromAnschluss)
        $ do
            case bgFahrtrichtungsAnschluss of
                UmdrehenAnschluss
                    {umdrehenAnschluss} -> anschlussWrite umdrehenAnschluss $ gesperrt bg
                _fahrtrichtungsAnschluss -> pure ()
            forM_ fahrstromPins $ \(pin, value) -> forkI2CReader $ anschlussWrite pin value
            forM_ (Map.toList fahrstromPortMapHigh)
                $ \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports HIGH
            forM_ (Map.toList fahrstromPortMapLow)
                $ \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports LOW
        where
            (fahrstromPins, fahrstromPcf8574PortsHigh, fahrstromPcf8574PortsLow) =
                foldl splitAnschlüsse ([], [], []) fahrstromAnschlüsse

            splitAnschlüsse
                :: ( [(Pin, Value)]
                   , [PCF8574Port 'OhneInterruptPin]
                   , [PCF8574Port 'OhneInterruptPin]
                   )
                -> AnschlussEither
                -> ( [(Pin, Value)]
                   , [PCF8574Port 'OhneInterruptPin]
                   , [PCF8574Port 'OhneInterruptPin]
                   )
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                anschluss@(AnschlussMit AnschlussPin {pin}) =
                ((pin, anschlussValue anschluss) : pins, portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                anschluss@(AnschlussOhne AnschlussPCF8574Port {pcf8574Port})
                | anschlussValue anschluss == HIGH = (pins, pcf8574Port : portsHigh, portsLow)
                | otherwise = (pins, portsHigh, pcf8574Port : portsLow)
            splitAnschlüsse acc (AnschlussMit AnschlussPCF8574Port {pcf8574Port}) =
                splitAnschlüsse acc
                $ AnschlussOhne
                $ AnschlussPCF8574Port
                $ ohneInterruptPin pcf8574Port

            fahrstromPortMapHigh = pcf8574Gruppieren fahrstromPcf8574PortsHigh

            fahrstromPortMapLow = pcf8574Gruppieren fahrstromPcf8574PortsLow

            anschlussValue :: AnschlussEither -> Value
            anschlussValue anschluss
                | positionOderLetztes fahrstromAnschluss fahrstromAnschlüsse == Just anschluss =
                    fließend bg
                | otherwise = gesperrt bg

    umdrehen
        :: (I2CReader r m, PwmReader r m, MonadIO m) => Bahngeschwindigkeit g 'Märklin -> m ()
    umdrehen
        bg@Bahngeschwindigkeit
        {bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin}} =
        flip befehlAusführen ("Umdrehen (" <> showText geschwindigkeitsPin <> ")") $ do
            stehenbleiben bg
            pwmSetzeWert bg geschwindigkeitsPin $ erhaltePwmWertVoll pwmGrenze
            warte umdrehenZeit
            pwmSetzeWert bg geschwindigkeitsPin $ erhaltePwmWertVoll (0 :: Word)
    umdrehen
        bg@Bahngeschwindigkeit {bgFahrtrichtungsAnschluss = UmdrehenAnschluss {umdrehenAnschluss}} =
        flip befehlAusführen ("Umdrehen (" <> showText umdrehenAnschluss <> ")") $ do
            stehenbleiben bg
            anschlussWrite umdrehenAnschluss $ fließend bg
            warte umdrehenZeit
            anschlussWrite umdrehenAnschluss $ gesperrt bg

    fahrtrichtungEinstellen :: (I2CReader r m, PwmReader r m, MonadIO m)
                            => Bahngeschwindigkeit g 'Lego
                            -> Fahrtrichtung
                            -> m ()
    fahrtrichtungEinstellen
        bg@Bahngeschwindigkeit
        { bgFahrtrichtungsAnschluss = FahrtrichtungsAnschluss {fahrtrichtungsAnschluss}
        , bgGeschwindigkeitsAnschlüsse}
        fahrtrichtung =
        flip
            befehlAusführen
            ("Umdrehen ("
             <> (showText bgGeschwindigkeitsAnschlüsse <^> showText fahrtrichtungsAnschluss
                 $ Language.Deutsch)
             <> ")->"
             <> showText fahrtrichtung)
        $ do
            stehenbleiben bg
            anschlussWrite fahrtrichtungsAnschluss
                $ (if fahrtrichtung == Vorwärts
                       then fließend
                       else gesperrt)
                    bg

stehenbleiben
    :: (I2CReader r m, PwmReader r m, PwmZugtyp z, MonadIO m) => Bahngeschwindigkeit g z -> m ()
stehenbleiben bg@Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {}} = do
    geschwindigkeit bg 0
    warte umdrehenZeit
stehenbleiben bg@Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {}} = do
    fahrstrom bg 0
    warte umdrehenZeit