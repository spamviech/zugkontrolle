{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description : Grundlegende UI-Funktionen.
-}
module Zug.UI.Base (
    -- * Zustands-Typ
    Status, StatusAllgemein(..), statusLeer, ReaderFamilie, ObjektReader,
    TVarMaps(..), MitTVarMaps(..), TVarMapsReader(..), tvarMapsNeu,
#ifdef ZUGKONTROLLEGUI
    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, wegstrecken, pläne,
#endif
    -- * Zustands-Monade
    IOStatus, MStatus, MStatusT, IOStatusAllgemein, MStatusAllgemein, MStatusAllgemeinT,
    auswertenLeererIOStatus, auswertenTMVarIOStatus, auswertenTMVarMStatus,
    -- ** Anpassen des aktuellen Zustands
    hinzufügenBahngeschwindigkeit, hinzufügenStreckenabschnitt, hinzufügenWeiche, hinzufügenKupplung,
    hinzufügenWegstrecke, hinzufügenPlan,
    entfernenBahngeschwindigkeit, entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung, entfernenWegstrecke,
    entfernenPlan,
    -- ** Spezialisierte Funktionen der Zustands-Monade
    getBahngeschwindigkeiten, getStreckenabschnitte, getWeichen, getKupplungen, getWegstrecken, getPläne, getSprache,
    putBahngeschwindigkeiten, putStreckenabschnitte, putWeichen, putKupplungen, putWegstrecken, putPläne, putSprache,
    -- * Hilfsfunktionen
    ausführenMöglich, AusführenMöglich(..)) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, TMVar, takeTMVar, putTMVar)
import Control.Monad.RWS.Lazy (RWST, runRWST, evalRWST, RWS, runRWS)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..), asks)
import Control.Monad.State.Class (gets, modify)
#ifdef ZUGKONTROLLEGUI
import Control.Lens (Lens', lens)
#endif
import Data.Foldable (Foldable(..))
import Data.List (delete, intersect)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (Anschluss(), PwmMap, pwmMapEmpty, MitPwmMap(..),
                        I2CMap, i2cMapEmpty, MitI2CMap(..), StreckenObjekt(..))
import Zug.Klassen (Zugtyp(..), ZugtypEither())
import qualified Zug.Language as Language
import Zug.Language (Anzeige(..), Sprache(), (<=>), (<\>), (<#>))
import Zug.Menge (Menge, leer)
import Zug.Objekt (ObjektKlasse(..), Objekt)
import Zug.Plan (Ausführend(..), Plan, MitAusführend(..), AusführendReader(..))

-- | Aktueller Status
data StatusAllgemein o = Status {
    _bahngeschwindigkeiten :: [ZugtypEither (BG o)],
    _streckenabschnitte :: [ST o],
    _weichen :: [ZugtypEither (WE o)],
    _kupplungen :: [KU o],
    _wegstrecken :: [ZugtypEither (WS o)],
    _pläne :: [PL o],
    _sprache :: SP o}
-- | Spezialisierung von 'StatusAllgemein' auf minimal benötigte Typen
type Status = StatusAllgemein Objekt

deriving instance (Eq (ZugtypEither (BG o)), Eq (ST o), Eq (ZugtypEither (WE o)), Eq (KU o),
    Eq (ZugtypEither (WS o)), Eq (PL o), Eq (SP o)) => Eq (StatusAllgemein o)
deriving instance (Show (ZugtypEither (BG o)), Show (ST o), Show (ZugtypEither (WE o)), Show (KU o),
    Show (ZugtypEither (WS o)), Show (PL o), Show (SP o)) => Show (StatusAllgemein o)

#ifdef ZUGKONTROLLEGUI
-- Template-Haskell verträgt sich nicht mit CPP (makeLenses ''StatusAllgemein wirft dll-Fehler unter Windows)
-- Linsen werden daher per Hand erstellt
-- | 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
bahngeschwindigkeiten :: Lens' (StatusAllgemein o) [ZugtypEither (BG o)]
bahngeschwindigkeiten
    = lens
        _bahngeschwindigkeiten $
        \status bgs -> status {_bahngeschwindigkeiten=bgs}
-- | 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
streckenabschnitte :: Lens' (StatusAllgemein o) [ST o]
streckenabschnitte
    = lens
        _streckenabschnitte $
        \status sts -> status {_streckenabschnitte=sts}
-- | 'Weiche'n im aktuellen 'StatusAllgemein'
weichen :: Lens' (StatusAllgemein o) [ZugtypEither (WE o)]
weichen
    = lens
        _weichen $
        \status wes -> status {_weichen=wes}
-- | 'Kupplung'en im aktuellen 'StatusAllgemein'
kupplungen :: Lens' (StatusAllgemein o) [KU o]
kupplungen
    = lens
        _kupplungen $
        \status kus -> status {_kupplungen=kus}
-- | 'Wegstrecke'n im aktuellen 'StatusAllgemein'
wegstrecken :: Lens' (StatusAllgemein o) [ZugtypEither (WS o)]
wegstrecken
    = lens
        _wegstrecken $
        \status wss -> status {_wegstrecken=wss}
-- | Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
pläne :: Lens' (StatusAllgemein o) [PL o]
pläne
    = lens
        _pläne $
        \status pls -> status {_pläne=pls}
#endif

instance (Anzeige (ZugtypEither (BG o)), Anzeige (ST o), Anzeige (ZugtypEither (WE o)), Anzeige (KU o),
    Anzeige (ZugtypEither (WS o)), Anzeige (PL o), Anzeige (SP o)) => Anzeige (StatusAllgemein o) where
    anzeige :: StatusAllgemein o -> Sprache -> Text
    anzeige
        status
            = Language.bahngeschwindigkeiten <=>
                (zeigeUnterliste $ _bahngeschwindigkeiten status)
            <\> Language.streckenabschnitte <=>
                (zeigeUnterliste $ _streckenabschnitte status)
            <\> Language.weichen <=>
                (zeigeUnterliste $ _weichen status)
            <\> Language.kupplungen <=>
                (zeigeUnterliste $ _kupplungen status)
            <\> Language.wegstrecken <=>
                (zeigeUnterliste $ _wegstrecken status)
            <\> Language.pläne <=>
                (zeigeUnterliste $ _pläne status)
        where
            -- | Zeige Liste besser Lesbar, als normale Anzeige-Instanz (newlines und Index-Angabe).
            zeigeUnterliste :: (Anzeige a) => [a] -> Sprache -> Text
            zeigeUnterliste = zeigeUnterlisteAux (const "[") 0
            zeigeUnterlisteAux :: (Anzeige a) => (Sprache -> Text) -> Natural -> [a] -> Sprache -> Text
            zeigeUnterlisteAux
                acc
                index
                []
                    = acc <#> (if index == 0 then "]" else "\n]" :: Text)
            zeigeUnterlisteAux
                acc
                index
                (h : t)
                    = zeigeUnterlisteAux (acc <\> ("\t" :: Text) <#> index <#> (") " :: Text) <#> h) (succ index) t

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' unter Verwendung existierender 'TVar's.
statusLeer :: SP o -> StatusAllgemein o
statusLeer _sprache = Status {
    _bahngeschwindigkeiten = [],
    _streckenabschnitte = [],
    _weichen = [],
    _kupplungen = [],
    _wegstrecken = [],
    _pläne = [],
    _sprache}

-- | Sammlung aller benötigten 'TVar's
data TVarMaps = TVarMaps {
    tvarAusführend :: TVar (Menge Ausführend),
    tvarPwmMap :: TVar PwmMap,
    tvarI2CMap :: TVar I2CMap}

-- | Erzeuge neue, leere 'TVarMaps'
tvarMapsNeu :: IO TVarMaps
tvarMapsNeu = TVarMaps <$> newTVarIO leer <*> newTVarIO pwmMapEmpty <*> newTVarIO i2cMapEmpty

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
-- | Zustands-Monaden-Transformer spezialiert auf 'Status'
type MStatusT m a = MStatusAllgemeinT m Objekt a
-- | Zustands-Monaden-Transformer spezialisiert auf 'StatusAllgemein' in der IO-Monade
type IOStatusAllgemein o a = MStatusAllgemeinT IO o a
-- | Reine Zustands-Monade spezialiert auf 'StatusAllgemein'
type MStatusAllgemein o a = RWS (ReaderFamilie o) () (StatusAllgemein o) a
-- | Zustands-Monaden-Transformer spezialiert auf 'StatusAllgemein'
type MStatusAllgemeinT m o a = RWST (ReaderFamilie o) () (StatusAllgemein o) m a

-- | Führe 'IOStatusAllgemein'-Aktion mit initial leerem 'StatusAllgemein' aus
auswertenLeererIOStatus :: IOStatusAllgemein o a -> IO (ReaderFamilie o) -> SP o -> IO a
auswertenLeererIOStatus ioStatus readerNeu sprache = do
    tvarMaps <- readerNeu
    (a, ()) <- evalRWST ioStatus tvarMaps $ statusLeer sprache
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
    mengeAusführend :: r -> TVar (Menge Ausführend)
    mengeAusführend = tvarAusführend . tvarMaps

instance (Monad m) => ObjektReader Objekt (RWST TVarMaps () Status m)

-- | Führe IO-Aktion mit 'StatusAllgemein' in 'TMVar' aus
auswertenTMVarIOStatus :: (MonadReader (ReaderFamilie o) m, MonadIO m) => IOStatusAllgemein o a -> TMVar (StatusAllgemein o) -> m a
auswertenTMVarIOStatus action tmvarStatus = do
    reader <- ask
    liftIO $ do
        status0 <- atomically $ takeTMVar tmvarStatus
        (a, status1, ()) <- runRWST action reader status0
        atomically $ putTMVar tmvarStatus status1
        pure a

-- | Führe Aktion mit 'StatusAllgemein' in 'TMVar' aus
auswertenTMVarMStatus :: (MonadReader (ReaderFamilie o) m, MonadIO m) => MStatusAllgemein o a -> TMVar (StatusAllgemein o) -> m a
auswertenTMVarMStatus action tmvarStatus = do
    reader <- ask
    liftIO $ atomically $ do
        status0 <- takeTMVar tmvarStatus
        let (a, status1, ()) = runRWS action reader status0
        putTMVar tmvarStatus status1
        pure a

-- * Erhalte aktuellen Status.
-- | Erhalte 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
getBahngeschwindigkeiten :: (Monad m) => MStatusAllgemeinT m o [ZugtypEither (BG o)]
getBahngeschwindigkeiten = gets _bahngeschwindigkeiten
-- | Erhalte 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
getStreckenabschnitte :: (Monad m) => MStatusAllgemeinT m o [ST o]
getStreckenabschnitte = gets _streckenabschnitte
-- | Erhalte 'Weiche'n im aktuellen 'StatusAllgemein'
getWeichen :: (Monad m) => MStatusAllgemeinT m o [ZugtypEither (WE o)]
getWeichen = gets _weichen
-- | Erhalte 'Kupplung'en im aktuellen 'StatusAllgemein'
getKupplungen :: (Monad m) => MStatusAllgemeinT m o [KU o]
getKupplungen = gets _kupplungen
-- | Erhalte 'Wegstrecke'n im aktuellen 'StatusAllgemein'
getWegstrecken :: (Monad m) => MStatusAllgemeinT m o [ZugtypEither (WS o)]
getWegstrecken = gets _wegstrecken
-- | Erhalte Pläne ('Plan') im aktuellen 'StatusAllgemein'
getPläne :: (Monad m) => MStatusAllgemeinT m o [PL o]
getPläne = gets _pläne
-- | Erhalte 'Sprache' im aktuellen 'StatusAllgemein'
getSprache :: (Monad m) => MStatusAllgemeinT m o (SP o)
getSprache = gets _sprache

-- * Ändere aktuellen Status
-- | Setze 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
putBahngeschwindigkeiten :: (Monad m) => [ZugtypEither (BG o)] -> MStatusAllgemeinT m o ()
putBahngeschwindigkeiten bgs = modify $ \status -> status {_bahngeschwindigkeiten = bgs}
-- | Setze 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
putStreckenabschnitte :: (Monad m) => [ST o] -> MStatusAllgemeinT m o ()
putStreckenabschnitte sts = modify $ \status -> status {_streckenabschnitte = sts}
-- | Setze 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
putWeichen :: (Monad m) => [ZugtypEither (WE o)] -> MStatusAllgemeinT m o ()
putWeichen wes = modify $ \status -> status{_weichen = wes}
-- | Setze 'Weiche'n im aktuellen 'StatusAllgemein'
putKupplungen :: (Monad m) => [KU o] -> MStatusAllgemeinT m o ()
putKupplungen kus = modify $ \status -> status{_kupplungen = kus}
-- | Setze 'Kupplung'en im akutellen 'StatusAllgemein'
putWegstrecken :: (Monad m) => [ZugtypEither (WS o)] -> MStatusAllgemeinT m o ()
putWegstrecken wss = modify $ \status -> status{_wegstrecken = wss}
-- | Setze Pläne ('Plan') im aktuellen 'StatusAllgemein'
putPläne :: (Monad m) => [PL o] -> MStatusAllgemeinT m o ()
putPläne pls = modify $ \status -> status {_pläne = pls}
-- | Setze 'Sprache' im aktuellen 'StatusAllgemein'
putSprache :: (Monad m) => SP o -> MStatusAllgemeinT m o ()
putSprache sprache = modify $ \status -> status{_sprache = sprache}

-- * Elemente hinzufügen
-- | Füge eine 'Bahngeschwindigkeit' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenBahngeschwindigkeit :: (Monad m) => ZugtypEither (BG o) -> MStatusAllgemeinT m o ()
hinzufügenBahngeschwindigkeit bahngeschwindigkeit = do
    bahngeschwindigkeiten <- getBahngeschwindigkeiten
    putBahngeschwindigkeiten $ bahngeschwindigkeit : bahngeschwindigkeiten
-- | Füge einen 'Streckenabschnitt' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenStreckenabschnitt :: (Monad m) => ST o -> MStatusAllgemeinT m o ()
hinzufügenStreckenabschnitt streckenabschnitt = do
    streckenabschnitte <- getStreckenabschnitte
    putStreckenabschnitte $ streckenabschnitt : streckenabschnitte
-- | Füge eine 'Weiche' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenWeiche :: (Monad m) => ZugtypEither (WE o) -> MStatusAllgemeinT m o ()
hinzufügenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ weiche : weichen
-- | Füge eine 'Kupplung' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenKupplung :: (Monad m) => KU o -> MStatusAllgemeinT m o ()
hinzufügenKupplung kupplung = do
    kupplungen <- getKupplungen
    putKupplungen $ kupplung : kupplungen
-- | Füge eine 'Wegstrecke' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenWegstrecke :: (Monad m) => ZugtypEither (WS o) -> MStatusAllgemeinT m o ()
hinzufügenWegstrecke wegstrecke = do
    wegstrecken <- getWegstrecken
    putWegstrecken $ wegstrecke : wegstrecken
-- | Füge einen 'Plan' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenPlan :: (Monad m) => PL o -> MStatusAllgemeinT m o ()
hinzufügenPlan plan = do
    pläne <- getPläne
    putPläne $ plan : pläne
-- * Elemente entfernen
-- | Entferne eine 'Bahngeschwindigkeit' aus dem aktuellen 'StatusAllgemein'
entfernenBahngeschwindigkeit :: (Monad m, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego)) => ZugtypEither (BG o) -> MStatusAllgemeinT m o ()
entfernenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ delete bahngeschwindigkeit bahngeschwindigkeiten
-- | Entferne einen 'Streckenabschnitt' aus dem aktuellen 'StatusAllgemein'
entfernenStreckenabschnitt :: (Monad m, Eq (ST o)) => ST o -> MStatusAllgemeinT m o ()
entfernenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ delete streckenabschnitt streckenabschnitte
-- | Entferne eine 'Weiche' aus dem aktuellen 'StatusAllgemein'
entfernenWeiche :: (Monad m, Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego)) => ZugtypEither (WE o) -> MStatusAllgemeinT m o ()
entfernenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ delete weiche weichen
-- | Entferne eine 'Kupplung' aus dem aktuellen 'StatusAllgemein'
entfernenKupplung :: (Monad m, Eq (KU o)) => KU o -> MStatusAllgemeinT m o ()
entfernenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ delete kupplung kupplungen
-- | Entferne eine 'Wegstrecke' aus dem aktuellen 'StatusAllgemein'
entfernenWegstrecke :: (Monad m, Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego)) => ZugtypEither (WS o) -> MStatusAllgemeinT m o ()
entfernenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ delete wegstrecke wegstrecken
-- | Entferne einen 'Plan' aus dem aktuellen 'StatusAllgemein'
entfernenPlan :: (Monad m, Eq (PL o)) => PL o -> MStatusAllgemeinT m o ()
entfernenPlan plan = getPläne >>= \pläne -> putPläne $ delete plan pläne
-- * Aktuell ausgeführte Pläne
-- | Überprüfe, ob ein Plan momentan ausgeführt werden kann.
ausführenMöglich :: (MitAusführend (ReaderFamilie o), MitPwmMap (ReaderFamilie o), MitI2CMap (ReaderFamilie o))
                        => Plan -> IOStatusAllgemein o AusführenMöglich
ausführenMöglich plan = do
    tvarAusführend <- erhalteMengeAusführend
    ausführend <- liftIO $ readTVarIO tvarAusführend
    let belegtePins = intersect (concat $ anschlüsse <$> ausführend) (anschlüsse plan)
    pure $ if
        | elem (Ausführend plan) ausführend
            -> WirdAusgeführt
        | not $ null belegtePins
            -> AnschlüsseBelegt $ NonEmpty.fromList belegtePins
        | otherwise
            -> AusführenMöglich

-- | Ist ein Ausführen eines Plans möglich?
data AusführenMöglich   = AusführenMöglich
                        | WirdAusgeführt
                        | AnschlüsseBelegt (NonEmpty Anschluss)