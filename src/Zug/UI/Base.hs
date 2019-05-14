{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Description : Grundlegende UI-Funktionen.
-}
module Zug.UI.Base (
    -- * Zustands-Typ
    Status, StatusAllgemein(..), statusLeer, statusLeerNeu, phantom,
#ifdef ZUGKONTROLLEGUI
    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, wegstrecken, pläne, mvarAusführend, mvarPinMap,
#endif
    -- * Zustands-Monade
    IOStatus, MStatus, MonadMStatus, IOStatusAllgemein, MStatusAllgemein, MonadMStatusAllgemein, auswertenLeererIOStatus, auswertenMVarIOStatus, auswertenMVarMStatus,
    -- ** Anpassen des aktuellen Zustands
    hinzufügenBahngeschwindigkeit, hinzufügenStreckenabschnitt, hinzufügenWeiche, hinzufügenKupplung, hinzufügenWegstrecke, hinzufügenPlan,
    entfernenBahngeschwindigkeit, entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung, entfernenWegstrecke, entfernenPlan,
    -- ** Spezialisierte Funktionen der Zustands-Monade
    getBahngeschwindigkeiten, getStreckenabschnitte, getWeichen, getKupplungen, getWegstrecken, getPläne, getMVarAusführend, getMVarPinMap,
    putBahngeschwindigkeiten, putStreckenabschnitte, putWeichen, putKupplungen, putWegstrecken, putPläne, putMVarAusführend, putMVarPinMap,
    -- * Hilfsfunktionen
    übergebeMVarPinMap, liftIOFunction, ausführenMöglich, AusführenMöglich(..)) where

-- Bibliotheken
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.State (StateT, State, evalStateT, runStateT, runState, gets, modify)
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
import Zug.Anbindung (Pin, PinMap, PinMapIO, pinMapEmpty, StreckenObjekt(..))
import qualified Zug.Language as Language
import Zug.Language ((<=>), (<\>))
import Zug.LinkedMVar
import Zug.Menge
import Zug.Plan

-- | Aktueller Status
data StatusAllgemein o = Status {
    _bahngeschwindigkeiten :: [BG o],
    _streckenabschnitte :: [ST o],
    _weichen :: [WE o],
    _kupplungen :: [KU o],
    _wegstrecken :: [WS o],
    _pläne :: [PL o],
    _mvarAusführend :: MVar (Menge Ausführend),
    _mvarPinMap :: MVar PinMap}
-- | Spezialisierung von 'StatusAllgemein' auf minimal benötigte Typen
type Status = StatusAllgemein Objekt

-- | Erzeuge eine Phantom-Typ, um Typ-Inferenzen zu ermöglichen.
phantom :: StatusAllgemein o -> Phantom o
phantom _status = Phantom

#ifdef ZUGKONTROLLEGUI
-- Template-Haskell verträgt sich nicht mit CPP (makeLenses ''StatusAllgemein wirft dll-Fehler unter Windows)
-- Linsen werden daher per Hand erstellt
-- | 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
bahngeschwindigkeiten :: Lens' (StatusAllgemein o) [BG o]
bahngeschwindigkeiten   = lens _bahngeschwindigkeiten   $ \status bgs -> status {_bahngeschwindigkeiten=bgs}
-- | 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
streckenabschnitte :: Lens' (StatusAllgemein o) [ST o]
streckenabschnitte      = lens _streckenabschnitte      $ \status sts -> status {_streckenabschnitte=sts}
-- | 'Weiche'n im aktuellen 'StatusAllgemein'
weichen :: Lens' (StatusAllgemein o) [WE o]
weichen                 = lens _weichen                 $ \status wes -> status {_weichen=wes}
-- | 'Kupplung'en im aktuellen 'StatusAllgemein'
kupplungen :: Lens' (StatusAllgemein o) [KU o]
kupplungen              = lens _kupplungen              $ \status kus -> status {_kupplungen=kus}
-- | 'Wegstrecke'n im aktuellen 'StatusAllgemein'
wegstrecken :: Lens' (StatusAllgemein o) [WS o]
wegstrecken             = lens _wegstrecken             $ \status wss -> status {_wegstrecken=wss}
-- | Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
pläne :: Lens' (StatusAllgemein o) [PL o]
pläne                   = lens _pläne                   $ \status pls -> status {_pläne=pls}
-- | Aktuell ausführende Pläne ('PlanAllgemein')
mvarAusführend :: Lens' (StatusAllgemein o) (MVar (Menge Ausführend))
mvarAusführend              = lens _mvarAusführend          $ \status mv -> status {_mvarAusführend=mv}
-- | Aktuell aktive PWM-Funktionen
mvarPinMap :: Lens' (StatusAllgemein o) (MVar PinMap)
mvarPinMap              = lens _mvarPinMap              $ \status mv  -> status {_mvarPinMap=mv}
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
            zeigeUnterlisteAux  acc index   ([])    = acc <> if (index == 0) then "]" else "\n]"
            zeigeUnterlisteAux  acc index   (h : t) = zeigeUnterlisteAux (acc <\> "\t" <> (show index) <> ") " <> (show h)) (succ index) t

-- | Hebe eine IO-Funktion mit Argument in eine 'MonadIO'-Monade
liftIOFunction :: (MonadIO m) => (a -> IO b) -> (a -> m b)
liftIOFunction f = \a -> liftIO $ f a

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' (inklusive MVar)
statusLeerNeu :: IO (StatusAllgemein o)
statusLeerNeu = statusLeer <$> newMVar leer <*> newMVar pinMapEmpty

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' unter Verwendung existierender 'MVar's.
statusLeer :: MVar (Menge Ausführend) -> MVar PinMap -> (StatusAllgemein o)
statusLeer mvarAusführend mvarPinMap = Status {_bahngeschwindigkeiten=[], _streckenabschnitte=[], _weichen=[], _kupplungen=[], _wegstrecken=[], _pläne=[], _mvarAusführend=mvarAusführend, _mvarPinMap=mvarPinMap}

-- | Übergebe mvarPinMap aus dem Status an eine 'PinMapIO'-Funktion
übergebeMVarPinMap :: PinMapIO a -> IOStatusAllgemein o a
übergebeMVarPinMap f = getMVarPinMap >>= liftIOFunction f

-- | Führe 'IOStatusAllgemein'-Aktion mit initial leerem 'StatusAllgemein' aus
auswertenLeererIOStatus :: IOStatusAllgemein o a -> IO a
auswertenLeererIOStatus ioStatus = statusLeerNeu >>= evalStateT ioStatus

-- | Führe IO-Aktion mit 'StatusAllgemein' in 'LikeMVar' aus
auswertenMVarIOStatus :: (LikeMVar lmvar) => IOStatusAllgemein o a -> lmvar (StatusAllgemein o) -> IO a
auswertenMVarIOStatus action mvarStatus = do
    status0 <- takeLMVar mvarStatus
    (a, status1) <- runStateT action status0
    putLMVar mvarStatus status1
    pure a

-- | Führe Aktion mit 'StatusAllgemein' in 'LikeMVar' aus
auswertenMVarMStatus :: (LikeMVar lmvar) => MStatusAllgemein o a -> lmvar (StatusAllgemein o) -> IO a
auswertenMVarMStatus action mvarStatus = do
    status0 <- takeLMVar mvarStatus
    let (a, status1) = runState action status0
    putLMVar mvarStatus status1
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
getBahngeschwindigkeiten :: (Monad m) => MonadMStatusAllgemein m o [BG o]
getBahngeschwindigkeiten = gets _bahngeschwindigkeiten
-- | Erhalte 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
getStreckenabschnitte :: (Monad m) => MonadMStatusAllgemein m o [ST o]
getStreckenabschnitte = gets _streckenabschnitte
-- | Erhalte 'Weiche'n im aktuellen 'StatusAllgemein'
getWeichen :: (Monad m) => MonadMStatusAllgemein m o [WE o]
getWeichen = gets _weichen
-- | Erhalte 'Kupplung'en im aktuellen 'StatusAllgemein'
getKupplungen :: (Monad m) => MonadMStatusAllgemein m o [KU o]
getKupplungen = gets _kupplungen
-- | Erhalte 'Wegstrecke'n im aktuellen 'StatusAllgemein'
getWegstrecken :: (Monad m) => MonadMStatusAllgemein m o [WS o]
getWegstrecken = gets _wegstrecken
-- | Erhalte Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
getPläne :: (Monad m) => MonadMStatusAllgemein m o [PL o]
getPläne = gets _pläne
-- | Erhalte 'MVar' mit Liste der aktuell ausführenden Pläne ('PlanAllgmein')
getMVarAusführend :: (Monad m) => MonadMStatusAllgemein m o (MVar (Menge Ausführend))
getMVarAusführend = gets _mvarAusführend
-- | Erhalte 'MVar' zur SoftwarePWM-Steuerung
getMVarPinMap :: (Monad m) => MonadMStatusAllgemein m o (MVar PinMap)
getMVarPinMap = gets _mvarPinMap

-- * Ändere aktuellen Status
-- | Setze 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
putBahngeschwindigkeiten :: (Monad m) => [BG o] -> MonadMStatusAllgemein m o ()
putBahngeschwindigkeiten bgs = modify $ \status -> status {_bahngeschwindigkeiten=bgs}
-- | Setze 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
putStreckenabschnitte :: (Monad m) => [ST o] -> MonadMStatusAllgemein m o ()
putStreckenabschnitte sts = modify $ \status -> status {_streckenabschnitte=sts}
-- | Setze 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
putWeichen :: (Monad m) => [WE o] -> MonadMStatusAllgemein m o ()
putWeichen wes = modify $ \status -> status{_weichen=wes}
-- | Setze 'Weiche'n im aktuellen 'StatusAllgemein'
putKupplungen :: (Monad m) => [KU o] -> MonadMStatusAllgemein m o ()
putKupplungen kus = modify $ \status -> status{_kupplungen=kus}
-- | Setze 'Kupplung'en im akutellen 'StatusAllgemein'
putWegstrecken :: (Monad m) => [WS o] -> MonadMStatusAllgemein m o ()
putWegstrecken wss = modify $ \status -> status{_wegstrecken=wss}
-- | Setze Pläne ('PlanAllgemein') im aktuellen 'StatusAllgemein'
putPläne :: (Monad m) => [PL o] -> MonadMStatusAllgemein m o ()
putPläne pls = modify $ \status -> status {_pläne=pls}
-- | Setzte 'MVar' mit Liste der aktuell ausgeführten Pläne ('PlanAllgemein').
-- 
-- __Achtung__: Die aktuelle Ausführung wird dadurch nicht beeinflusst!
putMVarAusführend :: (Monad m) => MVar (Menge Ausführend) -> MonadMStatusAllgemein m o ()
putMVarAusführend mv = modify $ \status -> status {_mvarAusführend=mv}
-- | Setzte 'MVar' zur SoftwarePWM-Kontrolle.
-- 
-- __Achtung__ : Aktuell laufende SoftwarePWM wird dadurch nicht beeinflusst.
putMVarPinMap :: (Monad m) => MVar PinMap -> MonadMStatusAllgemein m o ()
putMVarPinMap mv = modify $ \status -> status{_mvarPinMap=mv}

-- * Elemente hinzufügen
-- | Füge eine 'Bahngeschwindigkeit' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenBahngeschwindigkeit :: (Monad m) => BG o -> MonadMStatusAllgemein m o ()
hinzufügenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ bahngeschwindigkeit : bahngeschwindigkeiten
-- | Füge einen 'Streckenabschnitt' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenStreckenabschnitt :: (Monad m) => ST o -> MonadMStatusAllgemein m o ()
hinzufügenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ streckenabschnitt : streckenabschnitte
-- | Füge eine 'Weiche' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenWeiche :: (Monad m) => WE o -> MonadMStatusAllgemein m o ()
hinzufügenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ weiche : weichen
-- | Füge eine 'Kupplung' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenKupplung :: (Monad m) => KU o -> MonadMStatusAllgemein m o ()
hinzufügenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ kupplung : kupplungen
-- | Füge eine 'Wegstrecke' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenWegstrecke :: (Monad m) => WS o -> MonadMStatusAllgemein m o ()
hinzufügenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ wegstrecke : wegstrecken
-- | Füge einen 'Plan' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenPlan :: (Monad m) => PL o -> MonadMStatusAllgemein m o ()
hinzufügenPlan plan = getPläne >>= \pläne -> putPläne $ plan : pläne
-- * Elemente entfernen
-- | Entferne eine 'Bahngeschwindigkeit' aus dem aktuellen 'StatusAllgemein'
entfernenBahngeschwindigkeit :: (Monad m, Eq (BG o)) => BG o -> MonadMStatusAllgemein m o ()
entfernenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ delete bahngeschwindigkeit bahngeschwindigkeiten
-- | Entferne einen 'Streckenabschnitt' aus dem aktuellen 'StatusAllgemein'
entfernenStreckenabschnitt :: (Monad m, Eq (ST o)) => ST o -> MonadMStatusAllgemein m o ()
entfernenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ delete streckenabschnitt streckenabschnitte
-- | Entferne eine 'Weiche' aus dem aktuellen 'StatusAllgemein'
entfernenWeiche :: (Monad m, Eq (WE o)) => WE o -> MonadMStatusAllgemein m o ()
entfernenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ delete weiche weichen
-- | Entferne eine 'Kupplung' aus dem aktuellen 'StatusAllgemein'
entfernenKupplung :: (Monad m, Eq (KU o)) => KU o -> MonadMStatusAllgemein m o ()
entfernenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ delete kupplung kupplungen
-- | Entferne eine 'Wegstrecke' aus dem aktuellen 'StatusAllgemein'
entfernenWegstrecke :: (Monad m, Eq (WS o)) => WS o -> MonadMStatusAllgemein m o ()
entfernenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ delete wegstrecke wegstrecken
-- | Entferne einen 'Plan' aus dem aktuellen 'StatusAllgemein'
entfernenPlan :: (Monad m, Eq (PL o)) => PL o -> MonadMStatusAllgemein m o ()
entfernenPlan plan = getPläne >>= \pläne -> putPläne $ delete plan pläne
-- * Aktuell ausgeführte Pläne
-- | Überprüfe, ob ein Plan momentan ausgeführt werden kann.
ausführenMöglich :: Plan -> IOStatusAllgemein o AusführenMöglich
ausführenMöglich plan = do
    mvarAusführend <- getMVarAusführend
    ausführend <- liftIO $ readLMVar mvarAusführend
    let belegtePins = intersect (concat $ pins <$> ausführend) (pins plan)
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
                        | PinsBelegt (NonEmpty Pin)