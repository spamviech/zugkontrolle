{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Description: Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung.
module Zug.Anbindung.Bahngeschwindigkeit
  ( -- * Datentyp und Typ-Klasse
    Bahngeschwindigkeit (..),
    GeschwindigkeitsAnschlüsse (..),
    FahrtrichtungsAnschluss (..),
    BahngeschwindigkeitKlasse (..),
    PwmZugtyp (),

    -- * Hilfsfunktionen
    verwendetPwm,
    umdrehenZeit,
    positionOderLetztes,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO ())
import Data.Aeson.Types ((.:), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Zug.Anbindung.Anschluss
  ( Anschluss (..),
    AnschlussEither (..),
    AnschlussKlasse (zuPinGpio),
    AnschlussKlasse (anschlussWrite, zuAnschluss),
    I2CReader (forkI2CReader),
    MitInterruptPin (OhneInterruptPin),
    PCF8574Klasse (ohneInterruptPin),
    PCF8574Port (),
    Pin (Gpio),
    Value (..),
    parseAnschlussEither,
    parseFließend,
    pcf8574Gruppieren,
    pcf8574MultiPortWrite,
  )
import Zug.Anbindung.Klassen (StreckenAtom (..), StreckenObjekt (..), befehlAusführen)
import Zug.Anbindung.Pwm
  ( PwmReader (),
    PwmValueUnmodifiziert,
    erhaltePwmWertReduziert,
    erhaltePwmWertVoll,
    pwmGrenze,
    pwmSetzeWert,
  )
import Zug.Anbindung.Wartezeit (Wartezeit (..), warte)
import Zug.Derive.Ord (deriveOrd)
import Zug.Enums
  ( Fahrtrichtung (Vorwärts),
    GeschwindigkeitVariante (..),
    Zugtyp (..),
    ZugtypKlasse (zuZugtypEither),
    zugtyp,
  )
import qualified Zug.JSONStrings as JS
import Zug.Language ((<->), (<:>), (<=>), (<^>), Anzeige (..), Sprache (), showText)
import qualified Zug.Language as Language

-- | 'Anschluss'-Möglichkeiten um die Geschwindigkeit zu kontrollieren.
data GeschwindigkeitsAnschlüsse (g :: GeschwindigkeitVariante) where
  GeschwindigkeitsPin :: {geschwindigkeitsPin :: Pin} -> GeschwindigkeitsAnschlüsse 'Pwm
  FahrstromAnschlüsse ::
    {fahrstromAnschlüsse :: NonEmpty AnschlussEither} ->
    GeschwindigkeitsAnschlüsse 'KonstanteSpannung

deriving instance Eq (GeschwindigkeitsAnschlüsse g)

deriving instance Ord (GeschwindigkeitsAnschlüsse g)

deriving instance Show (GeschwindigkeitsAnschlüsse g)

instance Anzeige (GeschwindigkeitsAnschlüsse g) where
  anzeige :: GeschwindigkeitsAnschlüsse g -> Sprache -> Text
  anzeige GeschwindigkeitsPin {geschwindigkeitsPin} =
    Language.geschwindigkeit <-> Language.pin <=> geschwindigkeitsPin
  anzeige FahrstromAnschlüsse {fahrstromAnschlüsse} =
    Language.fahrstrom <-> Language.anschlüsse <=> fahrstromAnschlüsse

-- | 'Anschluss'-Möglichkeiten um die Fahrtrichtung einzustellen.
data FahrtrichtungsAnschluss (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
  KeinExpliziterAnschluss :: FahrtrichtungsAnschluss 'Pwm 'Märklin
  UmdrehenAnschluss ::
    {umdrehenAnschluss :: AnschlussEither} ->
    FahrtrichtungsAnschluss 'KonstanteSpannung 'Märklin
  FahrtrichtungsAnschluss ::
    {fahrtrichtungsAnschluss :: AnschlussEither} ->
    FahrtrichtungsAnschluss g 'Lego

deriving instance Eq (FahrtrichtungsAnschluss g z)

deriveOrd $ Left ''FahrtrichtungsAnschluss

deriving instance Show (FahrtrichtungsAnschluss g z)

instance Anzeige (FahrtrichtungsAnschluss g z) where
  anzeige :: FahrtrichtungsAnschluss g z -> Sprache -> Text
  anzeige KeinExpliziterAnschluss = const Text.empty
  anzeige UmdrehenAnschluss {umdrehenAnschluss} =
    Language.umdrehen <-> Language.anschluss <=> umdrehenAnschluss
  anzeige FahrtrichtungsAnschluss {fahrtrichtungsAnschluss} =
    Language.fahrtrichtung <-> Language.anschluss <=> fahrtrichtungsAnschluss

-- | Kontrolliere Geschwindigkeit einer Schiene und steuere die Fahrtrichtung.
data Bahngeschwindigkeit (g :: GeschwindigkeitVariante) (z :: Zugtyp) = Bahngeschwindigkeit
  { bgName :: Text,
    bgFließend :: Value,
    bgGeschwindigkeitsAnschlüsse :: GeschwindigkeitsAnschlüsse g,
    bgFahrtrichtungsAnschluss :: FahrtrichtungsAnschluss g z
  }
  deriving (Eq, Ord, Show)

instance Anzeige (Bahngeschwindigkeit g z) where
  anzeige :: Bahngeschwindigkeit g z -> Sprache -> Text
  anzeige bg@Bahngeschwindigkeit {bgName, bgFließend} =
    Language.märklin
      <-> Language.bahngeschwindigkeit
      <:> Language.name
      <=> bgName <^> Language.fließend
      <=> bgFließend <^> anzeigeAnschlüsse bg
    where
      anzeigeAnschlüsse :: Bahngeschwindigkeit g z -> Sprache -> Text
      anzeigeAnschlüsse
        Bahngeschwindigkeit
          { bgGeschwindigkeitsAnschlüsse,
            bgFahrtrichtungsAnschluss = KeinExpliziterAnschluss
          } =
          anzeige bgGeschwindigkeitsAnschlüsse
      anzeigeAnschlüsse Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse, bgFahrtrichtungsAnschluss} =
        bgGeschwindigkeitsAnschlüsse <^> bgFahrtrichtungsAnschluss

instance StreckenObjekt (Bahngeschwindigkeit b z) where
  anschlüsse :: Bahngeschwindigkeit b z -> Set AnschlussEither
  anschlüsse
    Bahngeschwindigkeit
      { bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin},
        bgFahrtrichtungsAnschluss = KeinExpliziterAnschluss
      } = [zuAnschluss geschwindigkeitsPin]
  anschlüsse
    Bahngeschwindigkeit
      { bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {fahrstromAnschlüsse},
        bgFahrtrichtungsAnschluss = UmdrehenAnschluss {umdrehenAnschluss}
      } =
      Set.insert umdrehenAnschluss $ Set.fromList $ NonEmpty.toList fahrstromAnschlüsse
  anschlüsse
    Bahngeschwindigkeit
      { bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin},
        bgFahrtrichtungsAnschluss = FahrtrichtungsAnschluss {fahrtrichtungsAnschluss}
      } =
      [zuAnschluss geschwindigkeitsPin, fahrtrichtungsAnschluss]
  anschlüsse
    Bahngeschwindigkeit
      { bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {fahrstromAnschlüsse},
        bgFahrtrichtungsAnschluss = FahrtrichtungsAnschluss {fahrtrichtungsAnschluss}
      } =
      Set.insert fahrtrichtungsAnschluss $ Set.fromList $ NonEmpty.toList fahrstromAnschlüsse

  erhalteName :: Bahngeschwindigkeit b z -> Text
  erhalteName = bgName

instance StreckenAtom (Bahngeschwindigkeit b z) where
  fließend :: Bahngeschwindigkeit b z -> Value
  fließend = bgFließend

-- | Verwendet die 'Bahngeschwindigkeit' PWM zur Geschwindigkeitskontrolle?
verwendetPwm :: Bahngeschwindigkeit g z -> GeschwindigkeitVariante
verwendetPwm Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {}} = Pwm
verwendetPwm Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {}} =
  KonstanteSpannung

-- | Sammel-Klasse für 'Bahngeschwindigkeit'-artige Typen.
class
  ( StreckenObjekt (b 'Pwm 'Märklin),
    StreckenObjekt (b 'Pwm 'Lego),
    StreckenObjekt (b 'KonstanteSpannung 'Märklin),
    StreckenObjekt (b 'KonstanteSpannung 'Lego)
  ) =>
  BahngeschwindigkeitKlasse b
  where
  -- | Geschwindigkeit einstellen (akzeptiere Werte von 0 bis 100)
  geschwindigkeit ::
    (I2CReader r m, PwmReader r m, PwmZugtyp z, MonadIO m) => b 'Pwm z -> Word8 -> m ()

  -- | Fahrstrom ein-/ausschalten
  fahrstrom :: (I2CReader r m, MonadIO m) => b 'KonstanteSpannung z -> Word8 -> m ()

  -- | Gebe allen Zügen den Befehl zum Umdrehen
  umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m) => b g 'Märklin -> m ()

  -- | Gebe allen Zügen den Befehl in einer bestimmen Richtung zu fahren
  fahrtrichtungEinstellen ::
    (I2CReader r m, PwmReader r m, MonadIO m) => b g 'Lego -> Fahrtrichtung -> m ()

-- | Erhalte das Element an Position /i/, angefangen bei /1/.
-- Ist die Position größer als die Länge der Liste wird das letzte Element zurückgegeben.
positionOderLetztes :: Word8 -> NonEmpty a -> Maybe a
positionOderLetztes 0 _nonEmpty = Nothing
positionOderLetztes 1 (a :| _t) = Just a
positionOderLetztes _i (a :| []) = Just a
positionOderLetztes i (_a :| (h : t)) = positionOderLetztes (pred i) $ h :| t

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
  geschwindigkeit ::
    (I2CReader r m, PwmReader r m, PwmZugtyp z, MonadIO m) =>
    Bahngeschwindigkeit 'Pwm z ->
    Word8 ->
    m ()
  geschwindigkeit
    bg@Bahngeschwindigkeit
      { bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin}
      }
    wert =
      befehlAusführen
        (pwmSetzeWert bg geschwindigkeitsPin $ erhaltePwmWert bg wert)
        ("Geschwindigkeit (" <> showText geschwindigkeitsPin <> ")->" <> showText wert)

  fahrstrom ::
    (I2CReader r m, MonadIO m) => Bahngeschwindigkeit 'KonstanteSpannung z -> Word8 -> m ()
  fahrstrom
    bg@Bahngeschwindigkeit
      { bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {fahrstromAnschlüsse},
        bgFahrtrichtungsAnschluss
      }
    fahrstromAnschluss =
      flip
        befehlAusführen
        ("Fahrstrom (" <> showText fahrstromAnschlüsse <> ")->" <> showText fahrstromAnschluss)
        $ do
          case bgFahrtrichtungsAnschluss of
            UmdrehenAnschluss
              { umdrehenAnschluss
              } -> anschlussWrite umdrehenAnschluss $ gesperrt bg
            _fahrtrichtungsAnschluss -> pure ()
          forM_ fahrstromPins $ \(pin, value) -> forkI2CReader $ anschlussWrite pin value
          forM_ (Map.toList fahrstromPortMapHigh) $
            \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports HIGH
          forM_ (Map.toList fahrstromPortMapLow) $
            \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports LOW
      where
        (fahrstromPins, fahrstromPcf8574PortsHigh, fahrstromPcf8574PortsLow) =
          foldl' splitAnschlüsse ([], [], []) fahrstromAnschlüsse
        splitAnschlüsse ::
          ( [(Pin, Value)],
            [PCF8574Port 'OhneInterruptPin],
            [PCF8574Port 'OhneInterruptPin]
          ) ->
          AnschlussEither ->
          ( [(Pin, Value)],
            [PCF8574Port 'OhneInterruptPin],
            [PCF8574Port 'OhneInterruptPin]
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

  umdrehen ::
    (I2CReader r m, PwmReader r m, MonadIO m) => Bahngeschwindigkeit g 'Märklin -> m ()
  umdrehen
    bg@Bahngeschwindigkeit
      { bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin}
      } =
      flip befehlAusführen ("Umdrehen (" <> showText geschwindigkeitsPin <> ")") $ do
        stehenbleiben bg
        pwmSetzeWert bg geschwindigkeitsPin $ erhaltePwmWertVoll pwmGrenze
        warte umdrehenZeit
        pwmSetzeWert bg geschwindigkeitsPin $ erhaltePwmWertVoll (0 :: Word)
  umdrehen bg@Bahngeschwindigkeit {bgFahrtrichtungsAnschluss = UmdrehenAnschluss {umdrehenAnschluss}} =
    flip befehlAusführen ("Umdrehen (" <> showText umdrehenAnschluss <> ")") $ do
      stehenbleiben bg
      anschlussWrite umdrehenAnschluss $ fließend bg
      warte umdrehenZeit
      anschlussWrite umdrehenAnschluss $ gesperrt bg

  fahrtrichtungEinstellen ::
    (I2CReader r m, PwmReader r m, MonadIO m) =>
    Bahngeschwindigkeit g 'Lego ->
    Fahrtrichtung ->
    m ()
  fahrtrichtungEinstellen
    bg@Bahngeschwindigkeit
      { bgFahrtrichtungsAnschluss = FahrtrichtungsAnschluss {fahrtrichtungsAnschluss},
        bgGeschwindigkeitsAnschlüsse
      }
    fahrtrichtung =
      flip
        befehlAusführen
        ( "Umdrehen ("
            <> ( showText bgGeschwindigkeitsAnschlüsse <^> showText fahrtrichtungsAnschluss $
                   Language.Deutsch
               )
            <> ")->"
            <> showText fahrtrichtung
        )
        $ do
          stehenbleiben bg
          anschlussWrite fahrtrichtungsAnschluss $
            ( if fahrtrichtung == Vorwärts
                then fließend
                else gesperrt
            )
              bg

-- | Setze die aktuelle Geschwindigkeit auf 0.
stehenbleiben ::
  (I2CReader r m, PwmReader r m, PwmZugtyp z, MonadIO m) => Bahngeschwindigkeit g z -> m ()
stehenbleiben bg@Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {}} = do
  geschwindigkeit bg 0
  warte umdrehenZeit
stehenbleiben bg@Bahngeschwindigkeit {bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {}} = do
  fahrstrom bg 0
  warte umdrehenZeit

-- JSON-Instanz-Deklarationen für Bahngeschwindigkeit
instance Aeson.FromJSON (Bahngeschwindigkeit 'Pwm 'Märklin) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Bahngeschwindigkeit 'Pwm 'Märklin)
  parseJSON (Aeson.Object v) = do
    Märklin <- v .: JS.zugtyp
    bgName <- v .: JS.name
    bgFließend <- parseFließend v
    geschwindigkeitsPin <- Gpio <$> v .: JS.geschwindigkeitsPin
    pure
      Bahngeschwindigkeit
        { bgName,
          bgFließend,
          bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin},
          bgFahrtrichtungsAnschluss = KeinExpliziterAnschluss
        }
  parseJSON _value = empty

instance Aeson.FromJSON (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
  parseJSON (Aeson.Object v) = do
    Märklin <- v .: JS.zugtyp
    bgName <- v .: JS.name
    bgFließend <- parseFließend v
    fahrstromAnschlüsse <- v .: JS.fahrstromAnschlüsse
    umdrehenAnschluss <- v .: JS.umdrehenAnschluss
    pure
      Bahngeschwindigkeit
        { bgName,
          bgFließend,
          bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {fahrstromAnschlüsse},
          bgFahrtrichtungsAnschluss = UmdrehenAnschluss {umdrehenAnschluss}
        }
  parseJSON _value = empty

instance Aeson.FromJSON (Bahngeschwindigkeit 'Pwm 'Lego) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Bahngeschwindigkeit 'Pwm 'Lego)
  parseJSON (Aeson.Object v) = do
    Lego <- v .: JS.zugtyp
    bgName <- v .: JS.name
    bgFließend <- parseFließend v
    geschwindigkeitsPin <- Gpio <$> v .: JS.geschwindigkeitsPin
    fahrtrichtungsAnschluss <-
      parseAnschlussEither v JS.fahrtrichtungsAnschluss JS.fahrtrichtungsPin
    pure
      Bahngeschwindigkeit
        { bgName,
          bgFließend,
          bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin},
          bgFahrtrichtungsAnschluss = FahrtrichtungsAnschluss {fahrtrichtungsAnschluss}
        }
  parseJSON _value = empty

instance Aeson.FromJSON (Bahngeschwindigkeit 'KonstanteSpannung 'Lego) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Bahngeschwindigkeit 'KonstanteSpannung 'Lego)
  parseJSON (Aeson.Object v) = do
    Lego <- v .: JS.zugtyp
    bgName <- v .: JS.name
    bgFließend <- parseFließend v
    fahrstromAnschlüsse <- v .: JS.fahrstromAnschlüsse
    fahrtrichtungsAnschluss <-
      parseAnschlussEither v JS.fahrtrichtungsAnschluss JS.fahrtrichtungsPin
    pure
      Bahngeschwindigkeit
        { bgName,
          bgFließend,
          bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {fahrstromAnschlüsse},
          bgFahrtrichtungsAnschluss = FahrtrichtungsAnschluss {fahrtrichtungsAnschluss}
        }
  parseJSON _value = empty

instance (ZugtypKlasse z) => Aeson.ToJSON (Bahngeschwindigkeit b z) where
  toJSON :: Bahngeschwindigkeit b z -> Aeson.Value
  toJSON
    bg@Bahngeschwindigkeit
      { bgName,
        bgFließend,
        bgGeschwindigkeitsAnschlüsse,
        bgFahrtrichtungsAnschluss
      } =
      Aeson.object $
        maybe
          id
          (:)
          (pairFahrtrichtungsAnschluss bgFahrtrichtungsAnschluss)
          [ JS.name .= bgName,
            JS.fließend .= bgFließend,
            JS.zugtyp .= zugtyp (zuZugtypEither bg),
            pairGeschwindigkeitsAnschlüsse bgGeschwindigkeitsAnschlüsse
          ]
      where
        pairGeschwindigkeitsAnschlüsse :: GeschwindigkeitsAnschlüsse g -> Aeson.Pair
        pairGeschwindigkeitsAnschlüsse GeschwindigkeitsPin {geschwindigkeitsPin} =
          JS.geschwindigkeitsPin .= (fromJust $ zuPinGpio geschwindigkeitsPin :: Word8)
        pairGeschwindigkeitsAnschlüsse FahrstromAnschlüsse {fahrstromAnschlüsse} =
          JS.fahrstromAnschlüsse .= fahrstromAnschlüsse
        pairFahrtrichtungsAnschluss :: FahrtrichtungsAnschluss g z -> Maybe Aeson.Pair
        pairFahrtrichtungsAnschluss KeinExpliziterAnschluss = Nothing
        pairFahrtrichtungsAnschluss UmdrehenAnschluss {umdrehenAnschluss} =
          Just $ JS.umdrehenAnschluss .= umdrehenAnschluss
        pairFahrtrichtungsAnschluss FahrtrichtungsAnschluss {fahrtrichtungsAnschluss} =
          Just $ JS.fahrtrichtungsAnschluss .= fahrtrichtungsAnschluss
