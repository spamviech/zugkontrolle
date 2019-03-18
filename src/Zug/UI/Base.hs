{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-|
Description : Grundlegende UI-Funktionen.
-}
module Zug.UI.Base (
    -- * Zustands-Typ
    Status, StatusAllgemein(..), statusLeer, statusLeerNeu,
#ifdef ZUGKONTROLLEGUI
    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, wegstrecken, pläne, mvarPinMap,
#endif
    -- * Zustands-Monade
    IOStatus, MStatus, MonadMStatus, IOStatusAllgemein, MStatusAllgemein, MonadMStatusAllgemein, auswertenLeererIOStatus, auswertenMVarIOStatus, auswertenMVarMStatus,
    -- ** Anpassen des aktuellen Zustands
    hinzufügenBahngeschwindigkeit, hinzufügenStreckenabschnitt, hinzufügenWeiche, hinzufügenKupplung, hinzufügenWegstrecke, hinzufügenPlan,
    entfernenBahngeschwindigkeit, entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung, entfernenWegstrecke, entfernenPlan,
    -- ** Spezialisierte Funktionen der Zustands-Monade
    getBahngeschwindigkeiten, getStreckenabschnitte, getWeichen, getKupplungen, getWegstrecken, getPläne, getMVarPinMap,
    putBahngeschwindigkeiten, putStreckenabschnitte, putWeichen, putKupplungen, putWegstrecken, putPläne, putMVarPinMap,
    -- * Hilfsfunktionen
    übergebeMVarPinMap, liftIOFunction) where

-- Bibliotheken
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.State (StateT, State, evalStateT, runStateT, runState, gets, modify)
#ifdef ZUGKONTROLLEGUI
import Control.Lens (Lens, lens)
#endif
import Data.List (delete)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar
import qualified Zug.Language as Language
import Zug.Language ((<=>), (<\>), showText)
import Zug.Anbindung (Bahngeschwindigkeit, Streckenabschnitt, Weiche, Kupplung, Wegstrecke, PinMap, PinMapIO, pinMapEmpty)
import Zug.Plan

-- | Aktueller Status
data StatusAllgemein bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan = Status {
    _bahngeschwindigkeiten :: [bahngeschwindigkeit],
    _streckenabschnitte :: [streckenabschnitt],
    _weichen :: [weiche],
    _kupplungen :: [kupplung],
    _wegstrecken :: [wegstrecke],
    _pläne :: [plan],
    _mvarPinMap :: MVar PinMap}
-- | Spezialisierung von 'StatusAllgemein' auf minimal benötigte Typen
type Status = StatusAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

#ifdef ZUGKONTROLLEGUI
-- Template-Haskell verträgt sich nicht mit CPP (makeLenses ''StatusAllgemein wirft dll-Fehler unter Windows)
-- Linsen werden daher per Hand erstellt
-- | 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
bahngeschwindigkeiten :: Lens (StatusAllgemein bg0 st we ku ws pl) (StatusAllgemein bg1 st we ku ws pl) [bg0] [bg1]
bahngeschwindigkeiten   = lens _bahngeschwindigkeiten   $ \status bgs -> status {_bahngeschwindigkeiten=bgs}
-- | 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
streckenabschnitte :: Lens (StatusAllgemein bg st0 we ku ws pl) (StatusAllgemein bg st1 we ku ws pl) [st0] [st1]
streckenabschnitte      = lens _streckenabschnitte      $ \status sts -> status {_streckenabschnitte=sts}
-- | 'Weiche'en im aktuellen 'StatusAllgemein'
weichen :: Lens (StatusAllgemein bg st we0 ku ws pl) (StatusAllgemein bg st we1 ku ws pl) [we0] [we1]
weichen                 = lens _weichen                 $ \status wes -> status {_weichen=wes}
-- | 'Kupplung'en im aktuellen 'StatusAllgemein'
kupplungen :: Lens (StatusAllgemein bg st we ku0 ws pl) (StatusAllgemein bg st we ku1 ws pl) [ku0] [ku1]
kupplungen              = lens _kupplungen              $ \status kus -> status {_kupplungen=kus}
-- | 'Wegstrecke'en im aktuellen 'StatusAllgemein'
wegstrecken :: Lens (StatusAllgemein bg st we ku ws0 pl) (StatusAllgemein bg st we ku ws1 pl) [ws0] [ws1]
wegstrecken             = lens _wegstrecken             $ \status wss -> status {_wegstrecken=wss}
-- | 'Plän'e im aktuellen 'StatusAllgemein'
pläne :: Lens (StatusAllgemein bg st we ku ws pl0) (StatusAllgemein bg st we ku ws pl1) [pl0] [pl1]
pläne                   = lens _pläne                   $ \status pls -> status {_pläne=pls}
-- | Aktuell aktive PWM-Funktionen
mvarPinMap :: Lens (StatusAllgemein bg st we ku ws pl) (StatusAllgemein bg st we ku ws pl) (MVar PinMap) (MVar PinMap)
mvarPinMap              = lens _mvarPinMap              $ \status mv  -> status {_mvarPinMap=mv}
#endif

instance (Show bahngeschwindigkeit, Show streckenabschnitt, Show weiche, Show kupplung, Show wegstrecke, Show plan) => Show (StatusAllgemein bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan) where
    show :: StatusAllgemein bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan -> String
    show status = Language.bahngeschwindigkeiten <=> (zeigeUnterliste $ _bahngeschwindigkeiten status)
                <\> Language.streckenabschnitte <=> (zeigeUnterliste $ _streckenabschnitte status)
                <\> Language.weichen <=> (zeigeUnterliste $ _weichen status)
                <\> Language.kupplungen <=> (zeigeUnterliste $ _kupplungen status)
                <\> Language.wegstrecken <=> (zeigeUnterliste $ _wegstrecken status)
                <\> Language.pläne <=> (zeigeUnterliste $ _pläne status)

-- | Zeige Liste besser Lesbar, als normale Show-Instanz (newlines und Index-Angabe).
zeigeUnterliste :: (Show a, Semigroup s, IsString s) => [a] -> s
zeigeUnterliste liste = "[" <> (zeigeUnterlisteAux "" 0 liste)
    where
        zeigeUnterlisteAux :: (Show a, Semigroup s, IsString s) => s -> Natural -> [a] -> s
        zeigeUnterlisteAux  acc index   ([])    = acc <> if (index == 0) then "]" else "\n]"
        zeigeUnterlisteAux  acc index   (h:t)   = zeigeUnterlisteAux (acc <\> "\t" <> (showText index) <> ") " <> (showText h)) (index+1) t

-- | Hebe eine IO-Funktion mit Argument in eine 'MonadIO'-Monade
liftIOFunction :: (MonadIO m) => (a -> IO b) -> (a -> m b)
liftIOFunction f = \a -> liftIO $ f a

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' (inklusive MVar)
statusLeerNeu :: IO (StatusAllgemein bg st we ku ws pl)
statusLeerNeu = newMVar pinMapEmpty >>= pure . statusLeer

-- | Erzeuge einen neuen, leeren 'StatusAllgemein' unter Verwendung einer existieren 'MVar'
statusLeer :: MVar PinMap -> (StatusAllgemein bg st we ku ws pl)
statusLeer mvarPinMap = Status {_bahngeschwindigkeiten=[], _streckenabschnitte=[], _weichen=[], _kupplungen=[], _wegstrecken=[], _pläne=[], _mvarPinMap=mvarPinMap}

-- | Übergebe mvarPinMap aus dem Status an eine 'PinMapIO'-Funktion
übergebeMVarPinMap :: PinMapIO a -> IOStatusAllgemein bg st we ku ws pl a
übergebeMVarPinMap  f   = getMVarPinMap >>= liftIOFunction f

-- | Führe 'IOStatusAllgemein'-Aktion mit initial leerem 'StatusAllgemein' aus
auswertenLeererIOStatus :: IOStatusAllgemein bg st we ku ws pl a -> IO a
auswertenLeererIOStatus ioStatus = statusLeerNeu >>= evalStateT ioStatus

-- | Führe IO-Aktion mit 'StatusAllgemein' in 'LikeMVar' aus
auswertenMVarIOStatus :: (LikeMVar lmvar) => IOStatusAllgemein bg st we ku ws pl a -> lmvar (StatusAllgemein bg st we ku ws pl) -> IO a
auswertenMVarIOStatus action mvarStatus = do
    status0 <- takeLMVar mvarStatus
    (a, status1) <- runStateT action status0
    putLMVar mvarStatus status1
    pure a

-- | Führe Aktion mit 'StatusAllgemein' in 'LikeMVar' aus
auswertenMVarMStatus :: (LikeMVar lmvar) => MStatusAllgemein bg st we ku ws pl a -> lmvar (StatusAllgemein bg st we ku ws pl) -> IO a
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
type IOStatusAllgemein bg st we ku ws pl = StateT (StatusAllgemein bg st we ku ws pl) IO
-- | Reine Zustands-Monade spezialiert auf 'StatusAllgemein'
type MStatusAllgemein bg st we ku ws pl = State (StatusAllgemein bg st we ku ws pl)
-- | Zustands-Monaden-Transformer spezialiert auf 'StatusAllgemein'
type MonadMStatusAllgemein m bg st we ku ws pl a = StateT (StatusAllgemein bg st we ku ws pl) m a

-- * Erhalte aktuellen Status.
-- | Erhalte 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
getBahngeschwindigkeiten :: (Monad m) => MonadMStatusAllgemein m bg st we ku ws pl [bg]
getBahngeschwindigkeiten = gets _bahngeschwindigkeiten
-- | Erhalte 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
getStreckenabschnitte :: (Monad m) => MonadMStatusAllgemein m bg st we ku ws pl [st]
getStreckenabschnitte = gets _streckenabschnitte
-- | Erhalte 'Weiche'n im aktuellen 'StatusAllgemein'
getWeichen :: (Monad m) => MonadMStatusAllgemein m bg st we ku ws pl [we]
getWeichen = gets _weichen
-- | Erhalte 'Kupplung'en im aktuellen 'StatusAllgemein'
getKupplungen :: (Monad m) => MonadMStatusAllgemein m bg st we ku ws pl [ku]
getKupplungen = gets _kupplungen
-- | Erhalte 'Wegstrecke'n im aktuellen 'StatusAllgemein'
getWegstrecken :: (Monad m) => MonadMStatusAllgemein m bg st we ku ws pl [ws]
getWegstrecken = gets _wegstrecken
-- | Erhalte Pläne ('Plan') im aktuellen 'StatusAllgemein'
getPläne :: (Monad m) => MonadMStatusAllgemein m bg st we ku ws pl [pl]
getPläne = gets _pläne
-- | Erhalte 'MVar' zur SoftwarePWM-Steuerung
getMVarPinMap :: (Monad m) => MonadMStatusAllgemein m bg st we ku ws pl (MVar PinMap)
getMVarPinMap = gets _mvarPinMap

-- * Ändere aktuellen Status
-- | Setze 'Bahngeschwindigkeit'en im aktuellen 'StatusAllgemein'
putBahngeschwindigkeiten :: (Monad m) => [bg] -> MonadMStatusAllgemein m bg st we ku ws pl ()
putBahngeschwindigkeiten bgs = modify $ \status -> status {_bahngeschwindigkeiten=bgs}
-- | Setze 'Streckenabschnitt'e im aktuellen 'StatusAllgemein'
putStreckenabschnitte :: (Monad m) => [st] -> MonadMStatusAllgemein m bg st we ku ws pl ()
putStreckenabschnitte sts = modify $ \status -> status {_streckenabschnitte=sts}
-- | Setze 'Streckenabschitt'e im aktuellen 'StatusAllgemein'
putWeichen :: (Monad m) => [we] -> MonadMStatusAllgemein m bg st we ku ws pl ()
putWeichen wes = modify $ \status -> status{_weichen=wes}
-- | Setze 'Weiche'n im aktuellen 'StatusAllgemein'
putKupplungen :: (Monad m) => [ku] -> MonadMStatusAllgemein m bg st we ku ws pl ()
putKupplungen kus = modify $ \status -> status{_kupplungen=kus}
-- | Setze 'Kupplung'en im akutellen 'StatusAllgemein'
putWegstrecken :: (Monad m) => [ws] -> MonadMStatusAllgemein m bg st we ku ws pl ()
putWegstrecken wss = modify $ \status -> status{_wegstrecken=wss}
-- | Setze Pläne ('Plan') im aktuellen 'StatusAllgemein'
putPläne :: (Monad m) => [pl] -> MonadMStatusAllgemein m bg st we ku ws pl ()
putPläne pls = modify $ \status -> status{_pläne=pls}
-- | Setzte 'MVar' zur SoftwarePWM-Kontrolle.
-- 
-- __Achtung__: Aktuell laufende SoftwarePWM wird dadurch nicht beeinflusst.
putMVarPinMap :: (Monad m) => MVar PinMap -> MonadMStatusAllgemein m bg st we ku ws pl ()
putMVarPinMap mv = modify $ \status -> status{_mvarPinMap=mv}

-- * Elemente hinzufügen
-- | Füge eine 'Bahngeschwindigkeit' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenBahngeschwindigkeit :: (Monad m) => bg -> MonadMStatusAllgemein m bg st we ku ws pl ()
hinzufügenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ bahngeschwindigkeit:bahngeschwindigkeiten
-- | Füge einen 'Streckenabschnitt' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenStreckenabschnitt :: (Monad m) => st -> MonadMStatusAllgemein m bg st we ku ws pl ()
hinzufügenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ streckenabschnitt:streckenabschnitte
-- | Füge eine 'Weiche' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenWeiche :: (Monad m) => we -> MonadMStatusAllgemein m bg st we ku ws pl ()
hinzufügenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ weiche:weichen
-- | Füge eine 'Kupplung' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenKupplung :: (Monad m) => ku -> MonadMStatusAllgemein m bg st we ku ws pl ()
hinzufügenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ kupplung:kupplungen
-- | Füge eine 'Wegstrecke' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenWegstrecke :: (Monad m) => ws -> MonadMStatusAllgemein m bg st we ku ws pl ()
hinzufügenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ wegstrecke:wegstrecken
-- | Füge einen 'Plan' zum aktuellen 'StatusAllgemein' hinzu
hinzufügenPlan :: (Monad m) => pl -> MonadMStatusAllgemein m bg st we ku ws pl ()
hinzufügenPlan plan = getPläne >>= \pläne -> putPläne $ plan:pläne
-- * Elemente entfernen
-- | Entferne eine 'Bahngeschwindigkeit' aus dem aktuellen 'StatusAllgemein'
entfernenBahngeschwindigkeit :: (Monad m, Eq bg) => bg -> MonadMStatusAllgemein m bg st we ku ws pl ()
entfernenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ delete bahngeschwindigkeit bahngeschwindigkeiten
-- | Entferne einen 'Streckenabschnitt' aus dem aktuellen 'StatusAllgemein'
entfernenStreckenabschnitt :: (Monad m, Eq st) => st -> MonadMStatusAllgemein m bg st we ku ws pl ()
entfernenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ delete streckenabschnitt streckenabschnitte
-- | Entferne eine 'Weiche' aus dem aktuellen 'StatusAllgemein'
entfernenWeiche :: (Monad m, Eq we) => we -> MonadMStatusAllgemein m bg st we ku ws pl ()
entfernenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ delete weiche weichen
-- | Entferne eine 'Kupplung' aus dem aktuellen 'StatusAllgemein'
entfernenKupplung :: (Monad m, Eq ku) => ku -> MonadMStatusAllgemein m bg st we ku ws pl ()
entfernenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ delete kupplung kupplungen
-- | Entferne eine 'Wegstrecke' aus dem aktuellen 'StatusAllgemein'
entfernenWegstrecke :: (Monad m, Eq ws) => ws -> MonadMStatusAllgemein m bg st we ku ws pl ()
entfernenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ delete wegstrecke wegstrecken
-- | Entferne einen 'Plan' aus dem aktuellen 'StatusAllgemein'
entfernenPlan :: (Monad m, Eq pl) => pl -> MonadMStatusAllgemein m bg st we ku ws pl ()
entfernenPlan plan = getPläne >>= \pläne -> putPläne $ delete plan pläne