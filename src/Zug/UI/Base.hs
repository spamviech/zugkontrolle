{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-|
Description : Grundlegende UI-Funktionen.
-}
module Zug.UI.Base (
    -- * Zustands-Typ
    Status, StatusGeneral(..), emptyStatus, emptyStatusNew,
#ifdef ZUGKONTROLLEGUI
    bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, wegstrecken, pläne, mvarPinMap,
#endif
    -- * Zustands-Monade
    IOStatus, MStatus, MonadMStatus, IOStatusGeneral, MStatusGeneral, MonadMStatusGeneral, evalEmptyIOStatus, evalMVarIOStatus, evalMVarMStatus,
    -- ** Anpassen des aktuellen Zustands
    hinzufügenBahngeschwindigkeit, hinzufügenStreckenabschnitt, hinzufügenWeiche, hinzufügenKupplung, hinzufügenWegstrecke, hinzufügenPlan,
    entfernenBahngeschwindigkeit, entfernenStreckenabschnitt, entfernenWeiche, entfernenKupplung, entfernenWegstrecke, entfernenPlan,
    -- ** Spezialisierte Funktionen der Zustands-Monade
    getBahngeschwindigkeiten, getStreckenabschnitte, getWeichen, getKupplungen, getWegstrecken, getPläne, getMVarPinMap,
    putBahngeschwindigkeiten, putStreckenabschnitte, putWeichen, putKupplungen, putWegstrecken, putPläne, putMVarPinMap,
    -- * Hilfsfunktionen
    passMVarPinMap, liftIOFunction) where

-- Bibliotheken
import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.State
#ifdef ZUGKONTROLLEGUI
import Control.Lens (Lens, lens)
#endif
import Data.List
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Numeric.Natural
-- Abhängigkeiten von anderen Modulen
import Zug.LinkedMVar
import qualified Zug.Language as Language
import Zug.Language ((<=>), (<\>), showText)
import Zug.Anbindung (Bahngeschwindigkeit, Streckenabschnitt, Weiche, Kupplung, Wegstrecke, PinMap, PinMapIO, pinMapEmpty)
import Zug.Plan

-- | Aktueller Status
data StatusGeneral bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan = Status {
    _bahngeschwindigkeiten :: [bahngeschwindigkeit],
    _streckenabschnitte :: [streckenabschnitt],
    _weichen :: [weiche],
    _kupplungen :: [kupplung],
    _wegstrecken :: [wegstrecke],
    _pläne :: [plan],
    _mvarPinMap :: MVar PinMap}
-- | Spezialisierung von 'StatusGeneral' auf minimal benötigte Typen
type Status = StatusGeneral Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Wegstrecke Plan

#ifdef ZUGKONTROLLEGUI
-- Template-Haskell verträgt sich nicht mit CPP (makeLenses ''StatusGeneral wirft dll-Fehler unter Windows)
-- Linsen werden daher per Hand erstellt
-- | 'Bahngeschwindigkeit'en im aktuellen 'StatusGeneral'
bahngeschwindigkeiten :: Lens (StatusGeneral bg0 st we ku ws pl) (StatusGeneral bg1 st we ku ws pl) [bg0] [bg1]
bahngeschwindigkeiten   = lens _bahngeschwindigkeiten   $ \status bgs -> status {_bahngeschwindigkeiten=bgs}
-- | 'Streckenabschitt'e im aktuellen 'StatusGeneral'
streckenabschnitte :: Lens (StatusGeneral bg st0 we ku ws pl) (StatusGeneral bg st1 we ku ws pl) [st0] [st1]
streckenabschnitte      = lens _streckenabschnitte      $ \status sts -> status {_streckenabschnitte=sts}
-- | 'Weiche'en im aktuellen 'StatusGeneral'
weichen :: Lens (StatusGeneral bg st we0 ku ws pl) (StatusGeneral bg st we1 ku ws pl) [we0] [we1]
weichen                 = lens _weichen                 $ \status wes -> status {_weichen=wes}
-- | 'Kupplung'en im aktuellen 'StatusGeneral'
kupplungen :: Lens (StatusGeneral bg st we ku0 ws pl) (StatusGeneral bg st we ku1 ws pl) [ku0] [ku1]
kupplungen              = lens _kupplungen              $ \status kus -> status {_kupplungen=kus}
-- | 'Wegstrecke'en im aktuellen 'StatusGeneral'
wegstrecken :: Lens (StatusGeneral bg st we ku ws0 pl) (StatusGeneral bg st we ku ws1 pl) [ws0] [ws1]
wegstrecken             = lens _wegstrecken             $ \status wss -> status {_wegstrecken=wss}
-- | 'Plän'e im aktuellen 'StatusGeneral'
pläne :: Lens (StatusGeneral bg st we ku ws pl0) (StatusGeneral bg st we ku ws pl1) [pl0] [pl1]
pläne                   = lens _pläne                   $ \status pls -> status {_pläne=pls}
-- | Aktuell aktive PWM-Funktionen
mvarPinMap :: Lens (StatusGeneral bg st we ku ws pl) (StatusGeneral bg st we ku ws pl) (MVar PinMap) (MVar PinMap)
mvarPinMap              = lens _mvarPinMap              $ \status mv  -> status {_mvarPinMap=mv}
#endif

instance (Show bahngeschwindigkeit, Show streckenabschnitt, Show weiche, Show kupplung, Show wegstrecke, Show plan) => Show (StatusGeneral bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan) where
    show :: StatusGeneral bahngeschwindigkeit streckenabschnitt weiche kupplung wegstrecke plan -> String
    show status = Language.bahngeschwindigkeiten <=> (showSublist $ _bahngeschwindigkeiten status)
                <\> Language.streckenabschnitte <=> (showSublist $ _streckenabschnitte status)
                <\> Language.weichen <=> (showSublist $ _weichen status)
                <\> Language.kupplungen <=> (showSublist $ _kupplungen status)
                <\> Language.wegstrecken <=> (showSublist $ _wegstrecken status)
                <\> Language.pläne <=> (showSublist $ _pläne status)

-- | Zeige Liste besser Lesbar, als normale Show-Instanz.
showSublist :: (Show a, Semigroup s, IsString s) => [a] -> s
showSublist liste = "[" <> (showSublistAux "" 0 liste)
    where
        showSublistAux :: (Show a, Semigroup s, IsString s) => s -> Natural -> [a] -> s
        showSublistAux  acc index   ([])    = acc <> if (index == 0) then "]" else "\n]"
        showSublistAux  acc index   (h:t)   = showSublistAux (acc <\> "\t" <> (showText index) <> ") " <> (showText h)) (index+1) t

-- | Hebe eine IO-Funktion mit Argument in einen Monaden-Transformer
liftIOFunction :: (MonadIO m) => (a -> IO b) -> (a -> m b)
liftIOFunction f = \a -> liftIO $ f a

-- | Erzeuge einen neuen, leeren 'StatusGeneral' (inklusive MVar)
emptyStatusNew :: IO (StatusGeneral bg st we ku ws pl)
emptyStatusNew = newMVar pinMapEmpty >>= pure . emptyStatus

-- | Erzeuge einen neuen, leeren 'StatusGeneral' unter Verwendung einer existieren 'MVar'
emptyStatus :: MVar PinMap -> (StatusGeneral bg st we ku ws pl)
emptyStatus mvarPinMap = Status {_bahngeschwindigkeiten=[], _streckenabschnitte=[], _weichen=[], _kupplungen=[], _wegstrecken=[], _pläne=[], _mvarPinMap=mvarPinMap}

-- | Übergebe mvarPinMap aus dem Status an eine eine Funktion
passMVarPinMap :: PinMapIO a -> IOStatusGeneral bg st we ku ws pl a
passMVarPinMap  f   = getMVarPinMap >>= liftIOFunction f

-- | Führe IO-Aktion mit initialem 'StatusGeneral' aus
evalEmptyIOStatus :: IOStatusGeneral bg st we ku ws pl a -> IO a
evalEmptyIOStatus ioStatus = emptyStatusNew >>= evalStateT ioStatus

-- | Führe IO-Aktion mit 'StatusGeneral' in 'LikeMVar' aus
evalMVarIOStatus :: (LikeMVar lmvar) => IOStatusGeneral bg st we ku ws pl a -> lmvar (StatusGeneral bg st we ku ws pl) -> IO a
evalMVarIOStatus action mvarStatus = do
    status0 <- takeLMVar mvarStatus
    (a, status1) <- runStateT action status0
    putLMVar mvarStatus status1
    pure a

-- | Führe Aktion mit 'StatusGeneral' in 'LikeMVar' aus
evalMVarMStatus :: (LikeMVar lmvar) => MStatusGeneral bg st we ku ws pl a -> lmvar (StatusGeneral bg st we ku ws pl) -> IO a
evalMVarMStatus action mvarStatus = do
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
-- | Zustands-Monaden-Transformer spezialisiert auf 'StatusGeneral' in der IO-Monade
type IOStatusGeneral bg st we ku ws pl = StateT (StatusGeneral bg st we ku ws pl) IO
-- | Reine Zustands-Monade spezialiert auf 'StatusGeneral'
type MStatusGeneral bg st we ku ws pl = State (StatusGeneral bg st we ku ws pl)
-- | Zustands-Monaden-Transformer spezialiert auf 'StatusGeneral'
type MonadMStatusGeneral m bg st we ku ws pl a = StateT (StatusGeneral bg st we ku ws pl) m a

-- * Erhalte aktuellen Status.
-- | Erhalte 'Bahngeschwindigkeit'en im aktuellen 'StatusGeneral'
getBahngeschwindigkeiten :: (Monad m) => MonadMStatusGeneral m bg st we ku ws pl [bg]
getBahngeschwindigkeiten = gets _bahngeschwindigkeiten
-- | Erhalte 'Streckenabschnitt'e im aktuellen 'StatusGeneral'
getStreckenabschnitte :: (Monad m) => MonadMStatusGeneral m bg st we ku ws pl [st]
getStreckenabschnitte = gets _streckenabschnitte
-- | Erhalte 'Weiche'n im aktuellen 'StatusGeneral'
getWeichen :: (Monad m) => MonadMStatusGeneral m bg st we ku ws pl [we]
getWeichen = gets _weichen
-- | Erhalte 'Kupplung'en im aktuellen 'StatusGeneral'
getKupplungen :: (Monad m) => MonadMStatusGeneral m bg st we ku ws pl [ku]
getKupplungen = gets _kupplungen
-- | Erhalte 'Wegstrecke'n im aktuellen 'StatusGeneral'
getWegstrecken :: (Monad m) => MonadMStatusGeneral m bg st we ku ws pl [ws]
getWegstrecken = gets _wegstrecken
-- | Erhalte Pläne ('Plan') im aktuellen 'StatusGeneral'
getPläne :: (Monad m) => MonadMStatusGeneral m bg st we ku ws pl [pl]
getPläne = gets _pläne
-- | Erhalte 'MVar' zur SoftwarePWM-Steuerung
getMVarPinMap :: (Monad m) => MonadMStatusGeneral m bg st we ku ws pl (MVar PinMap)
getMVarPinMap = gets _mvarPinMap

-- * Ändere aktuellen Status
-- | Setze 'Bahngeschwindigkeit'en im aktuellen 'StatusGeneral'
putBahngeschwindigkeiten :: (Monad m) => [bg] -> MonadMStatusGeneral m bg st we ku ws pl ()
putBahngeschwindigkeiten bgs = modify $ \status -> status {_bahngeschwindigkeiten=bgs}
-- | Setze 'Streckenabschnitt'e im aktuellen 'StatusGeneral'
putStreckenabschnitte :: (Monad m) => [st] -> MonadMStatusGeneral m bg st we ku ws pl ()
putStreckenabschnitte sts = modify $ \status -> status {_streckenabschnitte=sts}
-- | Setze 'Streckenabschitt'e im aktuellen 'StatusGeneral'
putWeichen :: (Monad m) => [we] -> MonadMStatusGeneral m bg st we ku ws pl ()
putWeichen wes = modify $ \status -> status{_weichen=wes}
-- | Setze 'Weiche'n im aktuellen 'StatusGeneral'
putKupplungen :: (Monad m) => [ku] -> MonadMStatusGeneral m bg st we ku ws pl ()
putKupplungen kus = modify $ \status -> status{_kupplungen=kus}
-- | Setze 'Kupplung'en im akutellen 'StatusGeneral'
putWegstrecken :: (Monad m) => [ws] -> MonadMStatusGeneral m bg st we ku ws pl ()
putWegstrecken wss = modify $ \status -> status{_wegstrecken=wss}
-- | Setze Pläne ('Plan') im aktuellen 'StatusGeneral'
putPläne :: (Monad m) => [pl] -> MonadMStatusGeneral m bg st we ku ws pl ()
putPläne pls = modify $ \status -> status{_pläne=pls}
-- | Setzte 'MVar' zur SoftwarePWM-Kontrolle.
-- 
-- __Achtung__: Aktuell laufende SoftwarePWM wird dadurch nicht beeinflusst.
putMVarPinMap :: (Monad m) => MVar PinMap -> MonadMStatusGeneral m bg st we ku ws pl ()
putMVarPinMap mv = modify $ \status -> status{_mvarPinMap=mv}

-- * Elemente hinzufügen
-- | Füge eine 'Bahngeschwindigkeit' zum aktuellen 'StatusGeneral' hinzu
hinzufügenBahngeschwindigkeit :: (Monad m) => bg -> MonadMStatusGeneral m bg st we ku ws pl ()
hinzufügenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ bahngeschwindigkeit:bahngeschwindigkeiten
-- | Füge einen 'Streckenabschnitt' zum aktuellen 'StatusGeneral' hinzu
hinzufügenStreckenabschnitt :: (Monad m) => st -> MonadMStatusGeneral m bg st we ku ws pl ()
hinzufügenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ streckenabschnitt:streckenabschnitte
-- | Füge eine 'Weiche' zum aktuellen 'StatusGeneral' hinzu
hinzufügenWeiche :: (Monad m) => we -> MonadMStatusGeneral m bg st we ku ws pl ()
hinzufügenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ weiche:weichen
-- | Füge eine 'Kupplung' zum aktuellen 'StatusGeneral' hinzu
hinzufügenKupplung :: (Monad m) => ku -> MonadMStatusGeneral m bg st we ku ws pl ()
hinzufügenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ kupplung:kupplungen
-- | Füge eine 'Wegstrecke' zum aktuellen 'StatusGeneral' hinzu
hinzufügenWegstrecke :: (Monad m) => ws -> MonadMStatusGeneral m bg st we ku ws pl ()
hinzufügenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ wegstrecke:wegstrecken
-- | Füge einen 'Plan' zum aktuellen 'StatusGeneral' hinzu
hinzufügenPlan :: (Monad m) => pl -> MonadMStatusGeneral m bg st we ku ws pl ()
hinzufügenPlan plan = getPläne >>= \pläne -> putPläne $ plan:pläne
-- * Elemente entfernen
-- | Entferne eine 'Bahngeschwindigkeit' aus dem aktuellen 'StatusGeneral'
entfernenBahngeschwindigkeit :: (Monad m, Eq bg) => bg -> MonadMStatusGeneral m bg st we ku ws pl ()
entfernenBahngeschwindigkeit bahngeschwindigkeit = getBahngeschwindigkeiten >>= \bahngeschwindigkeiten -> putBahngeschwindigkeiten $ delete bahngeschwindigkeit bahngeschwindigkeiten
-- | Entferne einen 'Streckenabschnitt' aus dem aktuellen 'StatusGeneral'
entfernenStreckenabschnitt :: (Monad m, Eq st) => st -> MonadMStatusGeneral m bg st we ku ws pl ()
entfernenStreckenabschnitt streckenabschnitt = getStreckenabschnitte >>= \streckenabschnitte -> putStreckenabschnitte $ delete streckenabschnitt streckenabschnitte
-- | Entferne eine 'Weiche' aus dem aktuellen 'StatusGeneral'
entfernenWeiche :: (Monad m, Eq we) => we -> MonadMStatusGeneral m bg st we ku ws pl ()
entfernenWeiche weiche = getWeichen >>= \weichen -> putWeichen $ delete weiche weichen
-- | Entferne eine 'Kupplung' aus dem aktuellen 'StatusGeneral'
entfernenKupplung :: (Monad m, Eq ku) => ku -> MonadMStatusGeneral m bg st we ku ws pl ()
entfernenKupplung kupplung = getKupplungen >>= \kupplungen -> putKupplungen $ delete kupplung kupplungen
-- | Entferne eine 'Wegstrecke' aus dem aktuellen 'StatusGeneral'
entfernenWegstrecke :: (Monad m, Eq ws) => ws -> MonadMStatusGeneral m bg st we ku ws pl ()
entfernenWegstrecke wegstrecke = getWegstrecken >>= \wegstrecken -> putWegstrecken $ delete wegstrecke wegstrecken
-- | Entferne einen 'Plan' aus dem aktuellen 'StatusGeneral'
entfernenPlan :: (Monad m, Eq pl) => pl -> MonadMStatusGeneral m bg st we ku ws pl ()
entfernenPlan plan = getPläne >>= \pläne -> putPläne $ delete plan pläne