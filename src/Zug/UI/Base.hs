{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description : Grundlegende UI-Funktionen.
-}
module Zug.UI.Base
  ( -- * Zustands-Typ
    Status
  , StatusAllgemein(..)
  , statusLeer
  , TVarMaps(..)
  , MitTVarMaps(..)
  , TVarMapsReader(..)
  , tvarMapsNeu
  , ReaderFamilie
  , ObjektReader
  , auswertenLeererIOStatus
  , liftIOStatus
    -- ** Linsen
  , bahngeschwindigkeiten
  , streckenabschnitte
  , weichen
  , kupplungen
  , kontakte
  , wegstrecken
  , pläne
  , sprache
    -- * Zustands-Monade
  , IOStatus
  , MStatus
  , MStatusT
  , IOStatusAllgemein
  , MStatusAllgemein
  , MStatusAllgemeinT
    -- ** Anpassen des aktuellen Zustands
  , hinzufügenBahngeschwindigkeit
  , hinzufügenStreckenabschnitt
  , hinzufügenWeiche
  , hinzufügenKupplung
  , hinzufügenKontakt
  , hinzufügenWegstrecke
  , hinzufügenPlan
  , entfernenBahngeschwindigkeit
  , entfernenStreckenabschnitt
  , entfernenWeiche
  , entfernenKupplung
  , entfernenKontakt
  , entfernenWegstrecke
  , entfernenPlan
    -- ** Spezialisierte Funktionen der Zustands-Monade
  , getBahngeschwindigkeiten
  , getStreckenabschnitte
  , getWeichen
  , getKupplungen
  , getKontakte
  , getWegstrecken
  , getPläne
  , getSprache
  , putBahngeschwindigkeiten
  , putStreckenabschnitte
  , putWeichen
  , putKupplungen
  , putKontakte
  , putWegstrecken
  , putPläne
  , putSprache
    -- * Hilfsfunktionen
  , ausführenMöglich
  , AusführenMöglich(..)
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, TMVar, newTMVarIO)
import Control.Monad.RWS.Strict (RWST, runRWST, evalRWST, RWS)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.State.Class (MonadState(..), gets, modify)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (Foldable(..))
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Numeric.Natural (Natural)

import Zug.Anbindung
       (Anschluss(), PwmMap, pwmMapEmpty, MitPwmMap(..), I2CMap, i2cMapEmpty, MitI2CMap(..)
      , InterruptMap, MitInterruptMap(..), interruptMapEmpty, StreckenObjekt(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(), GeschwindigkeitEither())
import qualified Zug.Language as Language
import Zug.Language (Anzeige(..), Sprache(), (<=>), (<\>), (<#>))
import Zug.Objekt (ObjektKlasse(..), Objekt)
import Zug.Plan (Ausführend(..), Plan, MitAusführend(..), AusführendReader(..))

-- | Aktueller Status
data StatusAllgemein o =
    Status
    { _bahngeschwindigkeiten :: [ZugtypEither (GeschwindigkeitEither (BG o))]
    , _streckenabschnitte :: [ST o]
    , _weichen :: [ZugtypEither (WE o)]
    , _kupplungen :: [KU o]
    , _kontakte :: [KO o]
    , _wegstrecken :: [ZugtypEither (WS o)]
    , _pläne :: [PL o]
    , _sprache :: SP o
    }

-- | Spezialisierung von 'StatusAllgemein' auf minimal benötigte Typen
type Status = StatusAllgemein Objekt

deriving instance ( Eq (ZugtypEither (GeschwindigkeitEither (BG o)))
                  , Eq (ST o)
                  , Eq (ZugtypEither (WE o))
                  , Eq (KU o)
                  , Eq (KO o)
                  , Eq (ZugtypEither (WS o))
                  , Eq (PL o)
                  , Eq (SP o)
                  ) => Eq (StatusAllgemein o)

deriving instance ( Show (ZugtypEither (GeschwindigkeitEither (BG o)))
                  , Show (ST o)
                  , Show (ZugtypEither (WE o))
                  , Show (KU o)
                  , Show (KO o)
                  , Show (ZugtypEither (WS o))
                  , Show (PL o)
                  , Show (SP o)
                  ) => Show (StatusAllgemein o)

-- | Create a Lens' from a Getter and a Setter.
-- Simple Copy&Paste from the lens-definition (remove need for CPP).
-- lens :: forall f. Functor f => (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s
lens :: (s -> a) -> (s -> a -> s) -> (forall f. Functor f => (a -> f a) -> s -> f s)
lens sa sas afa s = sas s <$> afa (sa s)

{-# INLINE lens #-}

-- | Lens': 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'.
bahngeschwindigkeiten :: Functor f
                      => ([ZugtypEither (GeschwindigkeitEither (BG o))]
                          -> f [ZugtypEither (GeschwindigkeitEither (BG o))])
                      -> (StatusAllgemein o)
                      -> f (StatusAllgemein o)
bahngeschwindigkeiten =
    lens _bahngeschwindigkeiten $ \status bgs -> status { _bahngeschwindigkeiten = bgs }

-- | Lens': 'Streckenabschitt'e im aktuellen 'StatusAllgemein'.
streckenabschnitte
    :: Functor f => ([ST o] -> f [ST o]) -> (StatusAllgemein o) -> f (StatusAllgemein o)
streckenabschnitte = lens _streckenabschnitte $ \status sts -> status { _streckenabschnitte = sts }

-- | Lens': 'Weiche'n im aktuellen 'StatusAllgemein'.
weichen :: Functor f
        => ([ZugtypEither (WE o)] -> f [ZugtypEither (WE o)])
        -> (StatusAllgemein o)
        -> f (StatusAllgemein o)
weichen = lens _weichen $ \status wes -> status { _weichen = wes }

-- | Lens': 'Kupplung'en im aktuellen 'StatusAllgemein'.
kupplungen :: Functor f => ([KU o] -> f [KU o]) -> (StatusAllgemein o) -> f (StatusAllgemein o)
kupplungen = lens _kupplungen $ \status kus -> status { _kupplungen = kus }

-- | Lens': 'Kontakt'en im aktuellen 'StatusAllgemein'.
kontakte :: Functor f => ([KO o] -> f [KO o]) -> (StatusAllgemein o) -> f (StatusAllgemein o)
kontakte = lens _kontakte $ \status kus -> status { _kontakte = kus }

-- | Lens': 'Wegstrecke'n im aktuellen 'StatusAllgemein'.
wegstrecken :: Functor f
            => ([ZugtypEither (WS o)] -> f [ZugtypEither (WS o)])
            -> (StatusAllgemein o)
            -> f (StatusAllgemein o)
wegstrecken = lens _wegstrecken $ \status wss -> status { _wegstrecken = wss }

-- | Lens': Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'.
pläne :: Functor f => ([PL o] -> f [PL o]) -> (StatusAllgemein o) -> f (StatusAllgemein o)
pläne = lens _pläne $ \status pls -> status { _pläne = pls }

-- | Lens': 'Sprache' im aktuellen 'StatusAllgemein'.
sprache :: Functor f => (SP o -> f (SP o)) -> (StatusAllgemein o) -> f (StatusAllgemein o)
sprache = lens _sprache $ \status sp -> status { _sprache = sp }

instance ( Anzeige (ZugtypEither (GeschwindigkeitEither (BG o)))
         , Anzeige (ST o)
         , Anzeige (ZugtypEither (WE o))
         , Anzeige (KU o)
         , Anzeige (ZugtypEither (WS o))
         , Anzeige (PL o)
         ) => Anzeige (StatusAllgemein o) where
    anzeige :: StatusAllgemein o -> Sprache -> Text
    anzeige status =
        Language.bahngeschwindigkeiten
        <=> zeigeUnterliste (_bahngeschwindigkeiten status)
        <\> Language.streckenabschnitte
        <=> zeigeUnterliste (_streckenabschnitte status)
        <\> Language.weichen
        <=> zeigeUnterliste (_weichen status)
        <\> Language.kupplungen
        <=> zeigeUnterliste (_kupplungen status)
        <\> Language.wegstrecken
        <=> zeigeUnterliste (_wegstrecken status)
        <\> Language.pläne <=> zeigeUnterliste (_pläne status)
        where
            zeigeUnterliste :: (Anzeige a) => [a] -> Sprache -> Text

            --  Zeige Liste besser Lesbar, als normale Anzeige-Instanz (newlines und Index-Angabe).
            zeigeUnterliste = zeigeUnterlisteAux (const "[") 0

            zeigeUnterlisteAux
                :: (Anzeige a) => (Sprache -> Text) -> Natural -> [a] -> Sprache -> Text
            zeigeUnterlisteAux acc index [] =
                acc
                <#> (if index == 0
                         then "]"
                         else "\n]" :: Text)
            zeigeUnterlisteAux acc index (h:t) =
                zeigeUnterlisteAux
                    (acc <\> ("\t" :: Text) <#> index <#> (") " :: Text) <#> h)
                    (succ index)
                    t

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' unter Verwendung existierender 'TVar's.
statusLeer :: SP o -> StatusAllgemein o
statusLeer _sprache =
    Status
    { _bahngeschwindigkeiten = []
    , _streckenabschnitte = []
    , _weichen = []
    , _kupplungen = []
    , _kontakte = []
    , _wegstrecken = []
    , _pläne = []
    , _sprache
    }

-- | Sammlung aller benötigten 'TVar's
data TVarMaps =
    TVarMaps
    { tvarAusführend :: TVar (Set Ausführend)
    , tvarPwmMap :: TVar PwmMap
    , tvarI2CMap :: TVar I2CMap
    , tmvarInterruptMap :: TMVar InterruptMap
    }

-- | Erzeuge neue, leere 'TVarMaps'. Die übergebene Wartezeit gibt die Refresh-Rate der I2C-Kanäle an.
tvarMapsNeu :: IO TVarMaps
tvarMapsNeu =
    TVarMaps <$> newTVarIO Set.empty
    <*> newTVarIO pwmMapEmpty
    <*> newTVarIO i2cMapEmpty
    <*> newTMVarIO interruptMapEmpty

-- | Typ-Familie für Reader-Typ aus der 'RWST'-Monade
type family ReaderFamilie o

type instance ReaderFamilie Objekt = TVarMaps

-- | Abkürzung für Funktionen, die die zum Objekt gehörige 'ReaderFamilie' benötigen
class (MonadReader (ReaderFamilie o) m) => ObjektReader o m

instance (MonadReader (ReaderFamilie o) m) => ObjektReader o m

-- * Zustands-Monade mit Status als aktuellem Zustand
-- | Zustands-Monaden-Transformer spezialisiert auf 'Status' in der IO-Monade
type IOStatus a = IOStatusAllgemein Objekt a

-- | Reine Zustands-Monade spezialisiert auf 'Status'
type MStatus a = MStatusAllgemein Objekt a

-- | Zustands-Monaden-Transformer spezialisiert auf 'Status'
type MStatusT m a = MStatusAllgemeinT m Objekt a

-- | Zustands-Monaden-Transformer spezialisiert auf 'StatusAllgemein' in der IO-Monade
type IOStatusAllgemein o a = MStatusAllgemeinT IO o a

-- | Reine Zustands-Monade spezialisiert auf 'StatusAllgemein'
type MStatusAllgemein o a = RWS (ReaderFamilie o) () (StatusAllgemein o) a

-- | Zustands-Monaden-Transformer spezialisiert auf 'StatusAllgemein'
type MStatusAllgemeinT m o a = RWST (ReaderFamilie o) () (StatusAllgemein o) m a

-- | Führe 'IOStatusAllgemein'-Aktion mit initial leerem 'StatusAllgemein' aus
auswertenLeererIOStatus :: IOStatusAllgemein o a -> IO (ReaderFamilie o) -> SP o -> IO a
auswertenLeererIOStatus ioStatus readerNeu sprache = do
    tvarMaps <- readerNeu
    (a, ()) <- evalRWST ioStatus tvarMaps $ statusLeer sprache
    pure a

-- | Führe einen 'IOStatusAllgemein' in einer 'MStatusAllgemeinT' mit 'MonadIO' aus.
liftIOStatus :: (MonadIO m) => IOStatusAllgemein o a -> MStatusAllgemeinT m o a
liftIOStatus action = do
    reader <- ask
    state0 <- get
    (a, state1, ()) <- liftIO $ runRWST action reader state0
    put state1
    pure a

-- | Klasse für Typen mit 'TVarMaps'
class MitTVarMaps r where
    tvarMaps :: r -> TVarMaps

-- | Abkürzung für Funktionen, die 'TMVarMaps' benötigen
class (MonadReader r m, MitTVarMaps r) => TVarMapsReader r m | m -> r where
    erhalteTVarMaps :: m TVarMaps
    erhalteTVarMaps = asks tvarMaps

instance (MonadReader r m, MitTVarMaps r) => TVarMapsReader r m

instance MitTVarMaps TVarMaps where
    tvarMaps :: TVarMaps -> TVarMaps
    tvarMaps = id

instance (MitTVarMaps r) => MitI2CMap r where
    i2cMap :: r -> TVar I2CMap
    i2cMap = tvarI2CMap . tvarMaps

instance (MitTVarMaps r) => MitPwmMap r where
    pwmMap :: r -> TVar PwmMap
    pwmMap = tvarPwmMap . tvarMaps

instance (MitTVarMaps r) => MitAusführend r where
    mengeAusführend :: r -> TVar (Set Ausführend)
    mengeAusführend = tvarAusführend . tvarMaps

instance (MitTVarMaps r) => MitInterruptMap r where
    interruptMap :: r -> TMVar InterruptMap
    interruptMap = tmvarInterruptMap . tvarMaps

-- * Erhalte aktuellen Status.
-- | Erhalte 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'.
getBahngeschwindigkeiten
    :: (Monad m) => MStatusAllgemeinT m o [ZugtypEither (GeschwindigkeitEither (BG o))]
getBahngeschwindigkeiten = gets _bahngeschwindigkeiten

-- | Erhalte 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'.
getStreckenabschnitte :: (Monad m) => MStatusAllgemeinT m o [ST o]
getStreckenabschnitte = gets _streckenabschnitte

-- | Erhalte 'Weiche'n im aktuellen 'StatusAllgemein'.
getWeichen :: (Monad m) => MStatusAllgemeinT m o [ZugtypEither (WE o)]
getWeichen = gets _weichen

-- | Erhalte 'Kupplung'en im aktuellen 'StatusAllgemein'.
getKupplungen :: (Monad m) => MStatusAllgemeinT m o [KU o]
getKupplungen = gets _kupplungen

-- | Erhalte 'Kontakt'e im aktuellen 'StatusAllgemein'.
getKontakte :: (Monad m) => MStatusAllgemeinT m o [KO o]
getKontakte = gets _kontakte

-- | Erhalte 'Wegstrecke'n im aktuellen 'StatusAllgemein'
getWegstrecken :: (Monad m) => MStatusAllgemeinT m o [ZugtypEither (WS o)]
getWegstrecken = gets _wegstrecken

-- | Erhalte Pläne ('Plan') im aktuellen 'StatusAllgemein'.
getPläne :: (Monad m) => MStatusAllgemeinT m o [PL o]
getPläne = gets _pläne

-- | Erhalte 'Sprache' im aktuellen 'StatusAllgemein'.
getSprache :: (Monad m) => MStatusAllgemeinT m o (SP o)
getSprache = gets _sprache

-- * Ändere aktuellen Status
-- | Setze 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'.
putBahngeschwindigkeiten
    :: (Monad m) => [ZugtypEither (GeschwindigkeitEither (BG o))] -> MStatusAllgemeinT m o ()
putBahngeschwindigkeiten bgs = modify $ \status -> status { _bahngeschwindigkeiten = bgs }

-- | Setze 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'.
putStreckenabschnitte :: (Monad m) => [ST o] -> MStatusAllgemeinT m o ()
putStreckenabschnitte sts = modify $ \status -> status { _streckenabschnitte = sts }

-- | Setze 'Weiche'en im aktuellen 'StatusAllgemein'.
putWeichen :: (Monad m) => [ZugtypEither (WE o)] -> MStatusAllgemeinT m o ()
putWeichen wes = modify $ \status -> status { _weichen = wes }

-- | Setze 'Kupplung'en im aktuellen 'StatusAllgemein'.
putKupplungen :: (Monad m) => [KU o] -> MStatusAllgemeinT m o ()
putKupplungen kus = modify $ \status -> status { _kupplungen = kus }

-- | Setze 'Kontakt'e im aktuellen 'StatusAllgemein'.
putKontakte :: (Monad m) => [KO o] -> MStatusAllgemeinT m o ()
putKontakte kos = modify $ \status -> status { _kontakte = kos }

-- | Setze 'Wegstrecke'n im akutellen 'StatusAllgemein'.
putWegstrecken :: (Monad m) => [ZugtypEither (WS o)] -> MStatusAllgemeinT m o ()
putWegstrecken wss = modify $ \status -> status { _wegstrecken = wss }

-- | Setze Pläne ('Plan') im aktuellen 'StatusAllgemein'.
putPläne :: (Monad m) => [PL o] -> MStatusAllgemeinT m o ()
putPläne pls = modify $ \status -> status { _pläne = pls }

-- | Setze 'Sprache' im aktuellen 'StatusAllgemein'.
putSprache :: (Monad m) => SP o -> MStatusAllgemeinT m o ()
putSprache sprache = modify $ \status -> status { _sprache = sprache }

-- * Elemente hinzufügen
-- | Füge eine 'Bahngeschwindigkeit' zum aktuellen 'StatusAllgemein' hinzu.
hinzufügenBahngeschwindigkeit
    :: (Monad m) => ZugtypEither (GeschwindigkeitEither (BG o)) -> MStatusAllgemeinT m o ()
hinzufügenBahngeschwindigkeit bahngeschwindigkeit = do
    bahngeschwindigkeiten <- getBahngeschwindigkeiten
    putBahngeschwindigkeiten $ bahngeschwindigkeit : bahngeschwindigkeiten

-- | Füge einen 'Streckenabschnitt' zum aktuellen 'StatusAllgemein' hinzu.
hinzufügenStreckenabschnitt :: (Monad m) => ST o -> MStatusAllgemeinT m o ()
hinzufügenStreckenabschnitt streckenabschnitt = do
    streckenabschnitte <- getStreckenabschnitte
    putStreckenabschnitte $ streckenabschnitt : streckenabschnitte

-- | Füge eine 'Weiche' zum aktuellen 'StatusAllgemein' hinzu.
hinzufügenWeiche :: (Monad m) => ZugtypEither (WE o) -> MStatusAllgemeinT m o ()
hinzufügenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ weiche : weichen

-- | Füge eine 'Kupplung' zum aktuellen 'StatusAllgemein' hinzu.
hinzufügenKupplung :: (Monad m) => KU o -> MStatusAllgemeinT m o ()
hinzufügenKupplung kupplung = do
    kupplungen <- getKupplungen
    putKupplungen $ kupplung : kupplungen

-- | Füge einen 'Kontakt' zum aktuellen 'StatusAllgemein' hinzu.
hinzufügenKontakt :: (Monad m) => KO o -> MStatusAllgemeinT m o ()
hinzufügenKontakt kontakt = do
    kontakte <- getKontakte
    putKontakte $ kontakt : kontakte

-- | Füge eine 'Wegstrecke' zum aktuellen 'StatusAllgemein' hinzu.
hinzufügenWegstrecke :: (Monad m) => ZugtypEither (WS o) -> MStatusAllgemeinT m o ()
hinzufügenWegstrecke wegstrecke = do
    wegstrecken <- getWegstrecken
    putWegstrecken $ wegstrecke : wegstrecken

-- | Füge einen 'Plan' zum aktuellen 'StatusAllgemein' hinzu.
hinzufügenPlan :: (Monad m) => PL o -> MStatusAllgemeinT m o ()
hinzufügenPlan plan = do
    pläne <- getPläne
    putPläne $ plan : pläne

-- * Elemente entfernen
-- | Entferne eine 'Bahngeschwindigkeit' aus dem aktuellen 'StatusAllgemein'.
entfernenBahngeschwindigkeit
    :: ( Monad m
       , Eq ((GeschwindigkeitEither (BG o)) 'Märklin)
       , Eq ((GeschwindigkeitEither (BG o)) 'Lego)
       )
    => ZugtypEither (GeschwindigkeitEither (BG o))
    -> MStatusAllgemeinT m o ()
entfernenBahngeschwindigkeit
    bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten
    -> putBahngeschwindigkeiten $ delete bahngeschwindigkeit bahngeschwindigkeiten

-- | Entferne einen 'Streckenabschnitt' aus dem aktuellen 'StatusAllgemein'.
entfernenStreckenabschnitt :: (Monad m, Eq (ST o)) => ST o -> MStatusAllgemeinT m o ()
entfernenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte
    -> putStreckenabschnitte $ delete streckenabschnitt streckenabschnitte

-- | Entferne eine 'Weiche' aus dem aktuellen 'StatusAllgemein'.
entfernenWeiche :: (Monad m, Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego))
                => ZugtypEither (WE o)
                -> MStatusAllgemeinT m o ()
entfernenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ delete weiche weichen

-- | Entferne eine 'Kupplung' aus dem aktuellen 'StatusAllgemein'
entfernenKupplung :: (Monad m, Eq (KU o)) => KU o -> MStatusAllgemeinT m o ()
entfernenKupplung
    kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ delete kupplung kupplungen

-- | Entferne einen 'Kontakt' aus dem aktuellen 'StatusAllgemein'
entfernenKontakt :: (Monad m, Eq (KO o)) => KO o -> MStatusAllgemeinT m o ()
entfernenKontakt kontakt = do
    kontakte <- getKontakte
    putKontakte $ delete kontakt kontakte

-- | Entferne eine 'Wegstrecke' aus dem aktuellen 'StatusAllgemein'.
entfernenWegstrecke :: (Monad m, Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego))
                    => ZugtypEither (WS o)
                    -> MStatusAllgemeinT m o ()
entfernenWegstrecke
    wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ delete wegstrecke wegstrecken

-- | Entferne einen 'Plan' aus dem aktuellen 'StatusAllgemein'.
entfernenPlan :: (Monad m, Eq (PL o)) => PL o -> MStatusAllgemeinT m o ()
entfernenPlan plan = getPläne >>= \pläne -> putPläne $ delete plan pläne

-- | Überprüfe, ob ein Plan momentan ausgeführt werden kann.
ausführenMöglich
    :: ( MitAusführend (ReaderFamilie o)
       , MitPwmMap (ReaderFamilie o)
       , MitI2CMap (ReaderFamilie o)
       , MitInterruptMap (ReaderFamilie o)
       )
    => Plan
    -> IOStatusAllgemein o AusführenMöglich
ausführenMöglich plan = do
    tvarAusführend <- erhalteMengeAusführend
    ausführend <- liftIO $ readTVarIO tvarAusführend
    let belegtePins =
            mconcat (Set.toList $ Set.map anschlüsse ausführend)
            `Set.intersection` anschlüsse plan
    pure
        $ if
            | elem (Ausführend plan) ausführend -> WirdAusgeführt
            | not $ null belegtePins
                -> AnschlüsseBelegt $ NonEmpty.fromList $ Set.toList belegtePins
            | otherwise -> AusführenMöglich

-- | Ist ein Ausführen eines Plans möglich?
data AusführenMöglich
    = AusführenMöglich
    | WirdAusgeführt
    | AnschlüsseBelegt (NonEmpty Anschluss)