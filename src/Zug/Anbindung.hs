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
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Description : Low-Level-Definition der unterstützen Aktionen auf Anschluss-Ebene.
-}
module Zug.Anbindung
  ( -- * Anschluss-Repräsentation
    Anschluss(..)
  , AnschlussKlasse(..)
  , PCF8574Port(..)
  , PCF8574(..)
  , PCF8574Variant(..)
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
  , erhaltePwmWertReduziert
    -- ** Streckenabschnitte
  , Streckenabschnitt(..)
  , StreckenabschnittKlasse(..)
    -- ** Weichen
  , Weiche(..)
  , WeicheKlasse(..)
    -- ** Kupplungen
  , Kupplung(..)
  , KupplungKlasse(..)
    -- ** Kontakt
  , Kontakt(..)
  , kontaktNew
  , KontaktKlasse(..)
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
import Control.Concurrent.STM (atomically, readTVar, writeTVar, retry, newTVarIO, TVar)
import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import System.Hardware.WiringPi (Pin(..), Mode(..), pinMode, digitalWrite)

import Zug.Anbindung.Anschluss
       (Anschluss(..), AnschlussKlasse(..), PCF8574Port(..), PCF8574(..), PCF8574Variant(..)
      , anschlussWrite, Value(..), alleValues, I2CMap, i2cMapEmpty, MitI2CMap(..), I2CReader(..)
      , pcf8574Gruppieren, pcf8574MultiPortWrite, beiÄnderung, InterruptReader(..), IntEdge(..))
import Zug.Anbindung.Bahngeschwindigkeit (Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..)
                                        , verwendetPwm, umdrehenZeit, positionOderLetztes)
import Zug.Anbindung.Klassen (StreckenObjekt(..), StreckenAtom(..), befehlAusführen)
import Zug.Anbindung.Kontakt (Kontakt(..), kontaktNew, KontaktKlasse(..), SignalErhalten(..))
import Zug.Anbindung.Kupplung (Kupplung(..), KupplungKlasse(..), kuppelnZeit)
import Zug.Anbindung.Pwm (pwmServo, erhaltePwmWertVoll, erhaltePwmWertReduziert, pwmMöglich
                        , clockMöglich, PwmValueUnmodifiziert, pwmEingabeMaximal)
import Zug.Anbindung.SoftwarePWM (PwmMap, pwmMapEmpty, MitPwmMap(..), PwmReader(..))
import Zug.Anbindung.Streckenabschnitt (Streckenabschnitt(..), StreckenabschnittKlasse(..))
import Zug.Anbindung.Wartezeit
       (warte, Wartezeit(..), addition, differenz, multiplizieren, dividieren)
import Zug.Anbindung.Weiche (Weiche(..), WeicheKlasse(..), weicheZeit)
import Zug.Enums (Zugtyp(..), ZugtypEither(..), GeschwindigkeitVariante(..)
                , GeschwindigkeitEither(..), GeschwindigkeitPhantom(..), catPwm
                , catKonstanteSpannung, Strom(..), Fahrtrichtung(..), Richtung(..))
import qualified Zug.Language as Language
import Zug.Language (Anzeige(..), Sprache(), showText, (<^>), (<=>), (<->), (<|>), (<:>), (<°>))

-- | Zusammenfassung von Einzel-Elementen. Weichen haben eine vorgegebene Richtung.
data Wegstrecke (z :: Zugtyp) =
    Wegstrecke
    { wsName :: Text
    , wsBahngeschwindigkeiten :: Set (GeschwindigkeitEither Bahngeschwindigkeit z)
    , wsStreckenabschnitte :: Set Streckenabschnitt
    , wsWeichenRichtungen :: Set (Weiche z, Richtung)
    , wsKupplungen :: Set Kupplung
    }
    deriving (Eq, Ord, Show)

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
        <=> map (uncurry (<°>)) (Set.toList wsWeichenRichtungen)
        <^> Language.kupplungen <=> wsKupplungen

instance StreckenObjekt (Wegstrecke z) where
    anschlüsse :: Wegstrecke z -> Set Anschluss
    anschlüsse
        Wegstrecke
        {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsWeichenRichtungen, wsKupplungen} =
        Set.unions
        $ Set.toList
        $ Set.map anschlüsse wsBahngeschwindigkeiten
        <> Set.map anschlüsse wsStreckenabschnitte
        <> Set.map (anschlüsse . fst) wsWeichenRichtungen
        <> Set.map anschlüsse wsKupplungen

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
            forM_ (Map.toList umdrehenPortMapHigh) $ \(pcf8574, ports) -> do
                pcf8574MultiPortWrite pcf8574 ports HIGH
                warte umdrehenZeit
                pcf8574MultiPortWrite pcf8574 ports LOW
            forM_ (Map.toList umdrehenPortMapLow) $ \(pcf8574, ports) -> do
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
                {bgmkFließend = HIGH, bgmkUmdrehenAnschluss = AnschlussPCF8574Port {pcf8574Port}} =
                (pins, pcf8574Port : portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                MärklinBahngeschwindigkeitKonstanteSpannung
                {bgmkFließend = LOW, bgmkUmdrehenAnschluss = AnschlussPCF8574Port {pcf8574Port}} =
                (pins, portsHigh, pcf8574Port : portsLow)

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
            liftIO $ forM_ fahrstromPins $ \(pin, value) -> forkIO $ do
                pinMode pin OUTPUT
                digitalWrite pin value
            forM_ (Map.toList fahrstromPortMapHigh)
                $ \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports HIGH
            forM_ (Map.toList fahrstromPortMapLow)
                $ \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports LOW
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
            splitAnschlüsse bg (pins, portsHigh, portsLow) anschluss@AnschlussPin {pin} =
                ((pin, anschlussValue bg anschluss) : pins, portsHigh, portsLow)
            splitAnschlüsse
                bg
                (pins, portsHigh, portsLow)
                anschluss@AnschlussPCF8574Port {pcf8574Port}
                | anschlussValue bg anschluss == HIGH = (pins, pcf8574Port : portsHigh, portsLow)
                | otherwise = (pins, portsHigh, pcf8574Port : portsLow)

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
                     { bglFließend = HIGH
                     , bglFahrtrichtungsAnschluss = AnschlussPCF8574Port {pcf8574Port}}) =
                (pins, pcf8574Port : portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                (GeschwindigkeitPwm
                     LegoBahngeschwindigkeit
                     { bglFließend = LOW
                     , bglFahrtrichtungsAnschluss = AnschlussPCF8574Port {pcf8574Port}}) =
                (pins, portsHigh, pcf8574Port : portsLow)
            splitAnschlüsse _acc (GeschwindigkeitKonstanteSpannung _) =
                error "Lego-Bahngeschwindigkeit mit konstanter Spannung!"

            fahrtrichtungPortMapHigh = pcf8574Gruppieren fahrtrichtungPcf8574PortsHigh

            fahrtrichtungPortMapLow = pcf8574Gruppieren fahrtrichtungPcf8574PortsLow

instance StreckenabschnittKlasse (Wegstrecke z) where
    strom :: (I2CReader r m, MonadIO m) => Wegstrecke z -> Strom -> m ()
    strom ws@Wegstrecke {wsStreckenabschnitte} an =
        flip befehlAusführen ("Strom (" <> showText ws <> ")->" <> showText an) $ do
            liftIO $ forM_ stromPins $ \(pin, valueFunktion) -> do
                pinMode pin OUTPUT
                digitalWrite pin $ valueFunktion an
            forM_ (Map.toList stromPortMapHigh)
                $ \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports $ case an of
                    Fließend -> HIGH
                    Gesperrt -> LOW
            forM_ (Map.toList stromPortMapLow)
                $ \(pcf8574, ports) -> pcf8574MultiPortWrite pcf8574 ports $ case an of
                    Fließend -> LOW
                    Gesperrt -> HIGH
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
                Streckenabschnitt
                {stFließend = HIGH, stromAnschluss = AnschlussPCF8574Port {pcf8574Port}} =
                (pins, pcf8574Port : portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                Streckenabschnitt
                {stFließend = LOW, stromAnschluss = AnschlussPCF8574Port {pcf8574Port}} =
                (pins, portsHigh, pcf8574Port : portsLow)

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
            forM_ (Map.toList kupplungsPortMapHigh) $ \(pcf8574, ports) -> do
                pcf8574MultiPortWrite pcf8574 ports HIGH
                warte kuppelnZeit
                pcf8574MultiPortWrite pcf8574 ports LOW
            forM_ (Map.toList kupplungsPortMapLow) $ \(pcf8574, ports) -> do
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
                Kupplung
                {kuFließend = HIGH, kupplungsAnschluss = AnschlussPCF8574Port {pcf8574Port}} =
                (pins, pcf8574Port : portsHigh, portsLow)
            splitAnschlüsse
                (pins, portsHigh, portsLow)
                Kupplung
                {kuFließend = LOW, kupplungsAnschluss = AnschlussPCF8574Port {pcf8574Port}} =
                (pins, portsHigh, pcf8574Port : portsLow)

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
            forM_ (Map.toList richtungsPortMapHigh) $ \(pcf8574, ports) -> do
                pcf8574MultiPortWrite pcf8574 ports HIGH
                warte weicheZeit
                pcf8574MultiPortWrite pcf8574 ports LOW
            forM_ (Map.toList richtungsPortMapLow) $ \(pcf8574, ports) -> do
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
                    (Just AnschlussPin {pin})
                        -> ((pin, flip erhalteValue we) : pins, portsHigh, portsLow)
                    (Just AnschlussPCF8574Port {pcf8574Port}) -> case wemFließend of
                        HIGH -> (pins, pcf8574Port : portsHigh, portsLow)
                        LOW -> (pins, portsHigh, pcf8574Port : portsLow)
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