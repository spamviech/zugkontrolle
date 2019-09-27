{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description : Grundlegende UI-Funktionen.
-}
module Zug.UI.Base (
    -- * Zustands-Typ
    Status, StatusAllgemein(..), statusLeer, TVarMaps(..), tvarMapsNeu, phantom,
#ifdef ZUGKONTROLLEGUI
    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, wegstrecken, pläne,
#endif
    -- * Zustands-Monade
    IOStatus, MStatus, MStatusT, IOStatusAllgemein, MStatusAllgemein, MStatusAllgemeinT,
    auswertenLeererIOStatus, auswertenTMVarIOStatus, auswertenTMVarMStatus,
    -- ** Anpassen des aktuellen Zustands
    hinzufügenBahngeschwindigkeit, hinzufügenStreckenabschnitt, hinzufügenWeiche, hinzufügenKupplung, hinzufügenWegstrecke, hinzufügenPlan,
    entfernenBahngeschwindigkeit, entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung, entfernenWegstrecke, entfernenPlan,
    -- ** Spezialisierte Funktionen der Zustands-Monade
    getBahngeschwindigkeiten, getStreckenabschnitte, getWeichen, getKupplungen, getWegstrecken, getPläne, getPhantom,
    putBahngeschwindigkeiten, putStreckenabschnitte, putWeichen, putKupplungen, putWegstrecken, putPläne,
    -- * Hilfsfunktionen
    liftIOFunction, ausführenMöglich, AusführenMöglich(..)) where

-- Bibliotheken
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, TMVar, takeTMVar, putTMVar)
import Control.Monad (void)
import Control.Monad.RWS.Lazy (RWST, runRWST, evalRWST, RWS, runRWS)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..), asks)
import Control.Monad.State.Class (MonadState(..), gets, modify)
#ifdef ZUGKONTROLLEGUI
import Control.Lens (Lens', lens)
#endif
import Data.Foldable (Foldable(..))
import Data.List (delete, intersect)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup(..))
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Anbindung (Anschluss(), PwmMap, pwmMapEmpty, PwmReader(..), I2CMap, i2cMapEmpty, I2CReader(..), StreckenObjekt(..))
import Zug.Klassen (Zugtyp(..), ZugtypEither())
import qualified Zug.Language as Language
import Zug.Language ((<=>), (<\>))
import Zug.Menge (Menge, leer)
import Zug.Plan (ObjektKlasse(..), Objekt, Phantom(..), Ausführend(..), Plan, PlanReader(..),
                ausBG, ausST, ausWE, ausKU, ausWS, ausPL)

-- | Aktueller Status
data StatusAllgemein o = Status {
    _bahngeschwindigkeiten :: [ZugtypEither (BG o)],
    _streckenabschnitte :: [ST o],
    _weichen :: [ZugtypEither (WE o)],
    _kupplungen :: [KU o],
    _wegstrecken :: [ZugtypEither (WS o)],
    _pläne :: [PL o]}
-- | Spezialisierung von 'StatusAllgemein' auf minimal benötigte Typen
type Status = StatusAllgemein Objekt

-- | Erzeuge eine Phantom-Typ, um Typ-Inferenzen zu ermöglichen.
phantom :: StatusAllgemein o -> Phantom o
phantom _status = Phantom

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

instance (Show o, ObjektKlasse o) => Show (StatusAllgemein o) where
    show :: StatusAllgemein o -> String
    show
        status
            = Language.bahngeschwindigkeiten <=>
                (zeigeUnterliste $ ausBG (phantom status) <$> _bahngeschwindigkeiten status)
            <\> Language.streckenabschnitte <=>
                (zeigeUnterliste $ ausST (phantom status) <$> _streckenabschnitte status)
            <\> Language.weichen <=>
                (zeigeUnterliste $ ausWE (phantom status) <$> _weichen status)
            <\> Language.kupplungen <=>
                (zeigeUnterliste $ ausKU (phantom status) <$> _kupplungen status)
            <\> Language.wegstrecken <=>
                (zeigeUnterliste $ ausWS (phantom status) <$> _wegstrecken status)
            <\> Language.pläne <=>
                (zeigeUnterliste $ ausPL (phantom status) <$> _pläne status)
        where
            -- | Zeige Liste besser Lesbar, als normale Show-Instanz (newlines und Index-Angabe).
            zeigeUnterliste :: (Show a) => [a] -> String
            zeigeUnterliste liste = '[' : zeigeUnterlisteAux "" 0 liste
            zeigeUnterlisteAux :: (Show a) => String -> Natural -> [a] -> String
            zeigeUnterlisteAux
                acc
                index
                []
                    = acc <> if (index == 0) then "]" else "\n]"
            zeigeUnterlisteAux
                acc
                index
                (h : t)
                    = zeigeUnterlisteAux (acc <\> "\t" <> (show index) <> ") " <> (show h)) (succ index) t

-- | Hebe eine IO-Funktion mit Argument in eine 'MonadIO'-Monade
liftIOFunction :: (MonadIO m) => (a -> IO b) -> (a -> m b)
liftIOFunction f = liftIO . f

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' unter Verwendung existierender 'TVar's.
statusLeer :: StatusAllgemein o
statusLeer = Status {
    _bahngeschwindigkeiten = [],
    _streckenabschnitte = [],
    _weichen = [],
    _kupplungen = [],
    _wegstrecken = [],
    _pläne = []}

-- | Sammlung aller benötigten 'TVar's
data TVarMaps = TVarMaps {
    tvarAusführend :: TVar (Menge Ausführend),
    tvarPwmMap :: TVar PwmMap,
    tvarI2CMap :: TVar I2CMap}

-- | Erzeuge neue, leere 'TVarMaps'
tvarMapsNeu :: IO TVarMaps
tvarMapsNeu = TVarMaps <$> newTVarIO leer <*> newTVarIO pwmMapEmpty <*> newTVarIO i2cMapEmpty

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
type MStatusAllgemein o a = RWS TVarMaps () (StatusAllgemein o) a
-- | Zustands-Monaden-Transformer spezialiert auf 'StatusAllgemein'
type MStatusAllgemeinT m o a = RWST TVarMaps () (StatusAllgemein o) m a

-- *Reader-Instances für MStatusAllgemeinT IO o
-- wenn beliebige Monaden-Instanz benötigt wird: unliftIO könnte helfen
-- ansonsten mach forkI2CReader Probleme
instance  I2CReader TVarMaps (RWST TVarMaps () (StatusAllgemein o) IO) where
    erhalteI2CMap :: RWST TVarMaps () (StatusAllgemein o) IO (TVar I2CMap)
    erhalteI2CMap = asks tvarI2CMap
    forkI2CReader :: RWST TVarMaps () (StatusAllgemein o) IO () -> RWST TVarMaps () (StatusAllgemein o) IO ThreadId
    forkI2CReader action = do
        tvarMaps <- ask
        status <- get
        liftIO $ forkIO $ void $ runRWST action tvarMaps status

instance PwmReader TVarMaps (RWST TVarMaps () (StatusAllgemein o) IO) where
    erhaltePwmMap :: RWST TVarMaps () (StatusAllgemein o) IO (TVar PwmMap)
    erhaltePwmMap = asks tvarPwmMap

instance PlanReader TVarMaps (RWST TVarMaps () (StatusAllgemein o) IO) where
    erhalteMengeAusführend :: RWST TVarMaps () (StatusAllgemein o) IO (TVar (Menge Ausführend))
    erhalteMengeAusführend = asks tvarAusführend

-- | Führe 'IOStatusAllgemein'-Aktion mit initial leerem 'StatusAllgemein' aus
auswertenLeererIOStatus :: IOStatusAllgemein o a -> IO a
auswertenLeererIOStatus ioStatus = do
    tvarMaps <- tvarMapsNeu
    (a, ()) <- evalRWST ioStatus tvarMaps statusLeer
    pure a

-- | Führe IO-Aktion mit 'StatusAllgemein' in 'TMVar' aus
auswertenTMVarIOStatus :: IOStatusAllgemein o a -> TVarMaps -> TMVar (StatusAllgemein o) -> IO a
auswertenTMVarIOStatus action tvarMaps mvarStatus = do
    status0 <- atomically $ takeTMVar mvarStatus
    (a, status1, ()) <- runRWST action tvarMaps status0
    atomically $ putTMVar mvarStatus status1
    pure a

-- | Führe Aktion mit 'StatusAllgemein' in 'TMVar' aus
auswertenTMVarMStatus :: MStatusAllgemein o a -> TVarMaps -> TMVar (StatusAllgemein o) -> IO a
auswertenTMVarMStatus action tvarMaps mvarStatus = atomically $ do
    status0 <- takeTMVar mvarStatus
    let (a, status1, ()) = runRWS action tvarMaps status0
    putTMVar mvarStatus status1
    pure a

-- * Erhalte aktuellen Status.
-- | Erhalte 'Phantom' passend zum zugehörigem Objekt
getPhantom :: (Monad m) => MStatusAllgemeinT m o (Phantom o)
getPhantom = get >>= pure . phantom
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
-- | Erhalte Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
getPläne :: (Monad m) => MStatusAllgemeinT m o [PL o]
getPläne = gets _pläne

-- * Ändere aktuellen Status
-- | Setze 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
putBahngeschwindigkeiten :: (Monad m) => [ZugtypEither (BG o)] -> MStatusAllgemeinT m o ()
putBahngeschwindigkeiten bgs = modify $ \status -> status {_bahngeschwindigkeiten=bgs}
-- | Setze 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
putStreckenabschnitte :: (Monad m) => [ST o] -> MStatusAllgemeinT m o ()
putStreckenabschnitte sts = modify $ \status -> status {_streckenabschnitte=sts}
-- | Setze 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
putWeichen :: (Monad m) => [ZugtypEither (WE o)] -> MStatusAllgemeinT m o ()
putWeichen wes = modify $ \status -> status{_weichen=wes}
-- | Setze 'Weiche'n im aktuellen 'StatusAllgemein'
putKupplungen :: (Monad m) => [KU o] -> MStatusAllgemeinT m o ()
putKupplungen kus = modify $ \status -> status{_kupplungen=kus}
-- | Setze 'Kupplung'en im akutellen 'StatusAllgemein'
putWegstrecken :: (Monad m) => [ZugtypEither (WS o)] -> MStatusAllgemeinT m o ()
putWegstrecken wss = modify $ \status -> status{_wegstrecken=wss}
-- | Setze Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
putPläne :: (Monad m) => [PL o] -> MStatusAllgemeinT m o ()
putPläne pls = modify $ \status -> status {_pläne=pls}

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
ausführenMöglich :: Plan -> IOStatusAllgemein o AusführenMöglich
ausführenMöglich plan = do
    tvarAusführend <- asks tvarAusführend
    ausführend <- liftIO $ readTVarIO tvarAusführend
    let belegtePins = intersect (concat $ anschlüsse <$> ausführend) (anschlüsse plan)
    pure $ if
        | elem (Ausführend plan) ausführend
            -> WirdAusgeführt
        | not $ null belegtePins
            -> PinsBelegt $ NE.fromList belegtePins
        | otherwise
            -> AusführenMöglich

-- | Ist ein Ausführen eines Plans möglich?
data AusführenMöglich   = AusführenMöglich
                        | WirdAusgeführt
                        | PinsBelegt (NonEmpty Anschluss)