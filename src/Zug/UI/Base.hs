{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Description : Grundlegende UI-Funktionen.
-}
module Zug.UI.Base (
    -- * Zustands-Typ
    Status, StatusAllgemein(..), statusLeer, statusLeerNeu, phantom,
#ifdef ZUGKONTROLLEGUI
    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, wegstrecken, pläne,
    tvarAusführend, tvarPwmMap, tvarI2CMap,
#endif
    -- * Zustands-Monade
    IOStatus, MStatus, MonadMStatus, IOStatusAllgemein, MStatusAllgemein, MonadMStatusAllgemein, auswertenLeererIOStatus, auswertenTMVarIOStatus, auswertenTMVarMStatus,
    -- ** Anpassen des aktuellen Zustands
    hinzufügenBahngeschwindigkeit, hinzufügenStreckenabschnitt, hinzufügenWeiche, hinzufügenKupplung, hinzufügenWegstrecke, hinzufügenPlan,
    entfernenBahngeschwindigkeit, entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung, entfernenWegstrecke, entfernenPlan,
    -- ** Spezialisierte Funktionen der Zustands-Monade
    getBahngeschwindigkeiten, getStreckenabschnitte, getWeichen, getKupplungen, getWegstrecken, getPläne, getTVarAusführend, getTVarPwmMap, getTVarI2CMap, getTVarMaps,
    putBahngeschwindigkeiten, putStreckenabschnitte, putWeichen, putKupplungen, putWegstrecken, putPläne, putTVarAusführend, putTVarPwmMap, putTVarI2CMap,
    -- * Hilfsfunktionen
    übergebeTVarMaps, liftIOFunction, ausführenMöglich, AusführenMöglich(..)) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, TMVar, takeTMVar, putTMVar)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.State (StateT, State, evalStateT, runStateT, runState, get, gets, modify)
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
import Zug.Anbindung (Anschluss(), PwmMap, PwmMapT, pwmMapEmpty, I2CMap, i2cMapEmpty, runPwmMapT, StreckenObjekt(..))
import Zug.Klassen (Zugtyp(..), ZugtypEither())
import qualified Zug.Language as Language
import Zug.Language ((<=>), (<\>))
import Zug.Menge (Menge, leer)
import Zug.Plan (ObjektKlasse(..), Objekt, Phantom(..), Ausführend(..), Plan, ausBG, ausST, ausWE, ausKU, ausWS, ausPL)

-- | Aktueller Status
data StatusAllgemein o = Status {
    _bahngeschwindigkeiten :: [ZugtypEither (BG o)],
    _streckenabschnitte :: [ST o],
    _weichen :: [ZugtypEither (WE o)],
    _kupplungen :: [KU o],
    _wegstrecken :: [ZugtypEither (WS o)],
    _pläne :: [PL o],
    _tvarAusführend :: TVar (Menge Ausführend),
    _tvarPwmMap :: TVar PwmMap,
    _tvarI2CMap :: TVar I2CMap}
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
bahngeschwindigkeiten   = lens _bahngeschwindigkeiten   $ \status bgs   -> status {_bahngeschwindigkeiten=bgs}
-- | 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
streckenabschnitte :: Lens' (StatusAllgemein o) [ST o]
streckenabschnitte      = lens _streckenabschnitte      $ \status sts   -> status {_streckenabschnitte=sts}
-- | 'Weiche'n im aktuellen 'StatusAllgemein'
weichen :: Lens' (StatusAllgemein o) [ZugtypEither (WE o)]
weichen                 = lens _weichen                 $ \status wes   -> status {_weichen=wes}
-- | 'Kupplung'en im aktuellen 'StatusAllgemein'
kupplungen :: Lens' (StatusAllgemein o) [KU o]
kupplungen              = lens _kupplungen              $ \status kus   -> status {_kupplungen=kus}
-- | 'Wegstrecke'n im aktuellen 'StatusAllgemein'
wegstrecken :: Lens' (StatusAllgemein o) [ZugtypEither (WS o)]
wegstrecken             = lens _wegstrecken             $ \status wss   -> status {_wegstrecken=wss}
-- | Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
pläne :: Lens' (StatusAllgemein o) [PL o]
pläne                   = lens _pläne                   $ \status pls   -> status {_pläne=pls}
-- | Aktuell ausführende Pläne ('PlanAllgemein')
tvarAusführend :: Lens' (StatusAllgemein o) (TVar (Menge Ausführend))
tvarAusführend          = lens _tvarAusführend          $ \status tv    -> status {_tvarAusführend=tv}
-- | Aktuell aktive PWM-Funktionen
tvarPwmMap :: Lens' (StatusAllgemein o) (TVar PwmMap)
tvarPwmMap              = lens _tvarPwmMap              $ \status tv    -> status {_tvarPwmMap=tv}
-- | Aktuelle I2C-Ausgabe
tvarI2CMap :: Lens' (StatusAllgemein o) (TVar I2CMap)
tvarI2CMap              = lens _tvarI2CMap              $ \status tv    -> status {_tvarI2CMap=tv}
#endif

instance (Show o, ObjektKlasse o) => Show (StatusAllgemein o) where
    show :: StatusAllgemein o -> String
    show status = Language.bahngeschwindigkeiten <=> (zeigeUnterliste $ ausBG (phantom status) <$> _bahngeschwindigkeiten status)
                <\> Language.streckenabschnitte <=> (zeigeUnterliste $ ausST (phantom status) <$> _streckenabschnitte status)
                <\> Language.weichen <=> (zeigeUnterliste $ ausWE (phantom status) <$> _weichen status)
                <\> Language.kupplungen <=> (zeigeUnterliste $ ausKU (phantom status) <$> _kupplungen status)
                <\> Language.wegstrecken <=> (zeigeUnterliste $ ausWS (phantom status) <$> _wegstrecken status)
                <\> Language.pläne <=> (zeigeUnterliste $ ausPL (phantom status) <$> _pläne status)
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

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' (inklusive 'TVar')
statusLeerNeu :: IO (StatusAllgemein o)
statusLeerNeu = statusLeer <$> newTVarIO leer <*> newTVarIO pwmMapEmpty <*> newTVarIO i2cMapEmpty

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' unter Verwendung existierender 'TVar's.
statusLeer :: TVar (Menge Ausführend) -> TVar PwmMap -> TVar I2CMap -> (StatusAllgemein o)
statusLeer tvarAusführend tvarPwmMap tvarI2CMap = Status {
    _bahngeschwindigkeiten = [],
    _streckenabschnitte = [],
    _weichen = [],
    _kupplungen = [],
    _wegstrecken = [],
    _pläne = [],
    _tvarAusführend = tvarAusführend,
    _tvarPwmMap = tvarPwmMap,
    _tvarI2CMap = tvarI2CMap}

-- | Übergebe 'tvarPwmMap' und 'tvarI2CMap' aus dem Status an eine 'PwmMapT' 'IO'-Aktion
übergebeTVarMaps :: PwmMapT IO a -> IOStatusAllgemein o a
übergebeTVarMaps aktion = do
    tvarPwmMap <- getTVarPwmMap
    tvarI2CMap <- getTVarI2CMap
    liftIO $ runPwmMapT aktion (tvarPwmMap, tvarI2CMap)

-- | Führe 'IOStatusAllgemein'-Aktion mit initial leerem 'StatusAllgemein' aus
auswertenLeererIOStatus :: IOStatusAllgemein o a -> IO a
auswertenLeererIOStatus ioStatus = statusLeerNeu >>= evalStateT ioStatus

-- | Führe IO-Aktion mit 'StatusAllgemein' in 'TMVar' aus
auswertenTMVarIOStatus :: IOStatusAllgemein o a -> TMVar (StatusAllgemein o) -> IO a
auswertenTMVarIOStatus action mvarStatus = do
    status0 <- atomically $ takeTMVar mvarStatus
    (a, status1) <- runStateT action status0
    atomically $ putTMVar mvarStatus status1
    pure a

-- | Führe Aktion mit 'StatusAllgemein' in 'TMVar' aus
auswertenTMVarMStatus :: MStatusAllgemein o a -> TMVar (StatusAllgemein o) -> IO a
auswertenTMVarMStatus action mvarStatus = do
    status0 <- atomically $ takeTMVar mvarStatus
    let (a, status1) = runState action status0
    atomically $ putTMVar mvarStatus status1
    pure a

-- * Zustands-Monade mit Status als aktuellem Zustand
-- | Zustands-Monaden-Transformer spezialisiert auf 'Status' in der IO-Monade
type IOStatus = StateT Status IO
-- | Reine Zustands-Monade spezialisiert auf 'Status'
type MStatus = State Status
-- | Zustands-Monaden-Transformer spezialiert auf 'Status'
type MonadMStatus m a = StateT Status m a
-- | Zustands-Monaden-Transformer spezialisiert auf 'StatusAllgemein' in der IO-Monade
type IOStatusAllgemein o = StateT (StatusAllgemein o) IO
-- | Reine Zustands-Monade spezialiert auf 'StatusAllgemein'
type MStatusAllgemein o = State (StatusAllgemein o)
-- | Zustands-Monaden-Transformer spezialiert auf 'StatusAllgemein'
type MonadMStatusAllgemein m o a = StateT (StatusAllgemein o) m a

-- * Erhalte aktuellen Status.
-- | Erhalte 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
getBahngeschwindigkeiten :: (Monad m) => MonadMStatusAllgemein m o [ZugtypEither (BG o)]
getBahngeschwindigkeiten = gets _bahngeschwindigkeiten
-- | Erhalte 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
getStreckenabschnitte :: (Monad m) => MonadMStatusAllgemein m o [ST o]
getStreckenabschnitte = gets _streckenabschnitte
-- | Erhalte 'Weiche'n im aktuellen 'StatusAllgemein'
getWeichen :: (Monad m) => MonadMStatusAllgemein m o [ZugtypEither (WE o)]
getWeichen = gets _weichen
-- | Erhalte 'Kupplung'en im aktuellen 'StatusAllgemein'
getKupplungen :: (Monad m) => MonadMStatusAllgemein m o [KU o]
getKupplungen = gets _kupplungen
-- | Erhalte 'Wegstrecke'n im aktuellen 'StatusAllgemein'
getWegstrecken :: (Monad m) => MonadMStatusAllgemein m o [ZugtypEither (WS o)]
getWegstrecken = gets _wegstrecken
-- | Erhalte Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
getPläne :: (Monad m) => MonadMStatusAllgemein m o [PL o]
getPläne = gets _pläne
-- | Erhalte 'TVar' mit Liste der aktuell ausführenden Pläne ('PlanAllgmein')
getTVarAusführend :: (Monad m) => MonadMStatusAllgemein m o (TVar (Menge Ausführend))
getTVarAusführend = gets _tvarAusführend
-- | Erhalte 'TVar' zur SoftwarePWM-Steuerung
getTVarPwmMap :: (Monad m) => MonadMStatusAllgemein m o (TVar PwmMap)
getTVarPwmMap = gets _tvarPwmMap
-- | Erhalte 'TVar' zur I2C-Steuerung
getTVarI2CMap :: (Monad m) => MonadMStatusAllgemein m o (TVar I2CMap)
getTVarI2CMap = gets _tvarI2CMap
-- | Kombination aus 'getTVarPwmMap' und 'getTVarI2CMap'
getTVarMaps :: (Monad m) => MonadMStatusAllgemein m o (TVar PwmMap, TVar I2CMap)
getTVarMaps = do
    Status {_tvarPwmMap, _tvarI2CMap} <- get
    pure (_tvarPwmMap, _tvarI2CMap)

-- * Ändere aktuellen Status
-- | Setze 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
putBahngeschwindigkeiten :: (Monad m) => [ZugtypEither (BG o)] -> MonadMStatusAllgemein m o ()
putBahngeschwindigkeiten bgs = modify $ \status -> status {_bahngeschwindigkeiten=bgs}
-- | Setze 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
putStreckenabschnitte :: (Monad m) => [ST o] -> MonadMStatusAllgemein m o ()
putStreckenabschnitte sts = modify $ \status -> status {_streckenabschnitte=sts}
-- | Setze 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
putWeichen :: (Monad m) => [ZugtypEither (WE o)] -> MonadMStatusAllgemein m o ()
putWeichen wes = modify $ \status -> status{_weichen=wes}
-- | Setze 'Weiche'n im aktuellen 'StatusAllgemein'
putKupplungen :: (Monad m) => [KU o] -> MonadMStatusAllgemein m o ()
putKupplungen kus = modify $ \status -> status{_kupplungen=kus}
-- | Setze 'Kupplung'en im akutellen 'StatusAllgemein'
putWegstrecken :: (Monad m) => [ZugtypEither (WS o)] -> MonadMStatusAllgemein m o ()
putWegstrecken wss = modify $ \status -> status{_wegstrecken=wss}
-- | Setze Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
putPläne :: (Monad m) => [PL o] -> MonadMStatusAllgemein m o ()
putPläne pls = modify $ \status -> status {_pläne=pls}
-- | Setzte 'TVar' mit Liste der aktuell ausgeführten Pläne ('PlanAllgemein').
-- 
-- __Achtung__: Die aktuelle Ausführung wird dadurch nicht beeinflusst!
putTVarAusführend :: (Monad m) => TVar (Menge Ausführend) -> MonadMStatusAllgemein m o ()
putTVarAusführend tv = modify $ \status -> status {_tvarAusführend=tv}
-- | Setzte 'TVar' zur SoftwarePWM-Kontrolle.
-- 
-- __Achtung__ : Aktuell laufende SoftwarePWM wird dadurch nicht beeinflusst.
putTVarPwmMap :: (Monad m) => TVar PwmMap -> MonadMStatusAllgemein m o ()
putTVarPwmMap tv = modify $ \status -> status{_tvarPwmMap=tv}
-- | Setzte 'TVar' zur I2C-Kontrolle.
-- 
-- __Achtung__ : Aktuell laufende I2C-Ausgabe wird dadurch nicht beeinflusst.
-- Ein Effekt wird erst bei neu setzen des I2C-Kanals sichtbar.
putTVarI2CMap :: (Monad m) => TVar I2CMap -> MonadMStatusAllgemein m o ()
putTVarI2CMap tv = modify $ \status -> status{_tvarI2CMap=tv}

-- * Elemente hinzufügen
-- | Füge eine 'Bahngeschwindigkeit' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenBahngeschwindigkeit :: (Monad m) => ZugtypEither (BG o) -> MonadMStatusAllgemein m o ()
hinzufügenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ bahngeschwindigkeit : bahngeschwindigkeiten
-- | Füge einen 'Streckenabschnitt' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenStreckenabschnitt :: (Monad m) => ST o -> MonadMStatusAllgemein m o ()
hinzufügenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ streckenabschnitt : streckenabschnitte
-- | Füge eine 'Weiche' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenWeiche :: (Monad m) => ZugtypEither (WE o) -> MonadMStatusAllgemein m o ()
hinzufügenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ weiche : weichen
-- | Füge eine 'Kupplung' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenKupplung :: (Monad m) => KU o -> MonadMStatusAllgemein m o ()
hinzufügenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ kupplung : kupplungen
-- | Füge eine 'Wegstrecke' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenWegstrecke :: (Monad m) => ZugtypEither (WS o) -> MonadMStatusAllgemein m o ()
hinzufügenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ wegstrecke : wegstrecken
-- | Füge einen 'Plan' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenPlan :: (Monad m) => PL o -> MonadMStatusAllgemein m o ()
hinzufügenPlan plan = getPläne >>= \pläne -> putPläne $ plan : pläne
-- * Elemente entfernen
-- | Entferne eine 'Bahngeschwindigkeit' aus dem aktuellen 'StatusAllgemein'
entfernenBahngeschwindigkeit :: (Monad m, Eq ((BG o) 'Märklin), Eq ((BG o) 'Lego)) => ZugtypEither (BG o) -> MonadMStatusAllgemein m o ()
entfernenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ delete bahngeschwindigkeit bahngeschwindigkeiten
-- | Entferne einen 'Streckenabschnitt' aus dem aktuellen 'StatusAllgemein'
entfernenStreckenabschnitt :: (Monad m, Eq (ST o)) => ST o -> MonadMStatusAllgemein m o ()
entfernenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ delete streckenabschnitt streckenabschnitte
-- | Entferne eine 'Weiche' aus dem aktuellen 'StatusAllgemein'
entfernenWeiche :: (Monad m, Eq ((WE o) 'Märklin), Eq ((WE o) 'Lego)) => ZugtypEither (WE o) -> MonadMStatusAllgemein m o ()
entfernenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ delete weiche weichen
-- | Entferne eine 'Kupplung' aus dem aktuellen 'StatusAllgemein'
entfernenKupplung :: (Monad m, Eq (KU o)) => KU o -> MonadMStatusAllgemein m o ()
entfernenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ delete kupplung kupplungen
-- | Entferne eine 'Wegstrecke' aus dem aktuellen 'StatusAllgemein'
entfernenWegstrecke :: (Monad m, Eq ((WS o) 'Märklin), Eq ((WS o) 'Lego)) => ZugtypEither (WS o) -> MonadMStatusAllgemein m o ()
entfernenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ delete wegstrecke wegstrecken
-- | Entferne einen 'Plan' aus dem aktuellen 'StatusAllgemein'
entfernenPlan :: (Monad m, Eq (PL o)) => PL o -> MonadMStatusAllgemein m o ()
entfernenPlan plan = getPläne >>= \pläne -> putPläne $ delete plan pläne
-- * Aktuell ausgeführte Pläne
-- | Überprüfe, ob ein Plan momentan ausgeführt werden kann.
ausführenMöglich :: Plan -> IOStatusAllgemein o AusführenMöglich
ausführenMöglich plan = do
    tvarAusführend <- getTVarAusführend
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