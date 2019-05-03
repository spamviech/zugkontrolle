{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

{-|
Description : 'MVar', welche beim setzen/updaten eine IO-Aktion ausführen.
-}
module Zug.LinkedMVar (
                    -- * Datentyp und Konstruktoren
                    LinkedMVar(), newEmptyLinkedMVar, newLinkedMVar, newEmptyUnlinkedMVar, newUnlinkedMVar, appendLinkedMVar,
                    -- * Funktionen
                    readLinkedMVar, takeLinkedMVar, putLinkedMVar, modifyLinkedMVar_, modifyLinkedMVar, swapLinkedMVar, isEmptyLinkedMVar, updateAktion,
                    -- * Klasse zur einfacheren Verwendung
                    LikeMVar(..)) where

-- Bibliotheken
import Control.Concurrent.MVar
import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Function ((&))
-- Abhängigkeit von anderen Modulen
import Zug.Warteschlange

-- | 'MVar', welche beim setzen des Werts eine Update-Aktion ausführt.
-- Das Ergebnis der Update-Aktion wird der neue Wert der 'LinkedMVar', ohne ein erneuten Aufruf der Update-Aktion auszulösen.
-- Wenn ein erneutes ausführen der Update-Aktion gewünscht wird kann dies mit der übergebenen IO-Aktion erzwungen werden.
data LinkedMVar a = LinkedMVar {linkedUpdate :: MVar (Warteschlange (a -> IO a)), mvar :: MVar a}

-- * Konstruktoren
-- | Erzeuge eine neue leere 'LinkedMVar', ausgehend von einer Update-Aktion.
-- Diese wird immer aufgerufen, wenn sich der Wert der 'LinkedMVar' ändert.
-- Der neue Wert wird an die Update-Aktion übergeben.
newEmptyLinkedMVar :: (a -> IO a)  -> IO (LinkedMVar a)
newEmptyLinkedMVar update = LinkedMVar <$> newMVar (einzelelement update) <*> newEmptyMVar

-- | Erzeuge eine neue 'LinkedMVar' und fülle sie umgehend mit einem Wert.
newLinkedMVar :: (a -> IO a) -> a -> IO (LinkedMVar a)
newLinkedMVar update a = newEmptyLinkedMVar update >>= \lmvar -> putLinkedMVar lmvar a >> pure lmvar

-- | Erzeuge eine neue leere 'LinkedMVar' ohne Update-Aktion.
newEmptyUnlinkedMVar :: IO (LinkedMVar a)
newEmptyUnlinkedMVar = LinkedMVar <$> newMVar leer <*> newEmptyMVar

-- | Erzeuge eine neue 'LinkedMVar' ohne Update-Aktion und fülle sie umgehen mit einem Wert.
newUnlinkedMVar :: a -> IO (LinkedMVar a)
newUnlinkedMVar a = LinkedMVar <$> newMVar leer <*> newMVar a

-- | Füge einer LinkedMVar eine neue Update-Aktion hinzu. 
-- Die Update-Aktionen werden in anhängen-Reihenfolge aufgerufen.
appendLinkedMVar :: LinkedMVar a -> (a -> IO a) -> IO ()
appendLinkedMVar (LinkedMVar {linkedUpdate}) newCommand = modifyMVar_ linkedUpdate $ pure . anhängen newCommand

-- * Funktionen
-- | Lese den aktuellen Wert einer 'LinkedMVar', ohne diese zu verändern.
-- Wenn die 'LinkedMVar' aktuell leer ist, warte bis sie gefüllt wird.
readLinkedMVar :: LinkedMVar a -> IO a
readLinkedMVar (LinkedMVar {mvar}) = readMVar mvar

-- | Leere eine 'LinkedMVar' und gebe den aktuellen Wert zurück.
-- Wenn die 'LinkedMVar' aktuell leer ist, warte bis sie gefüllt wird.
takeLinkedMVar :: LinkedMVar a -> IO a
takeLinkedMVar (LinkedMVar {mvar}) = takeMVar mvar

-- | Schreibe einen Wert in eine 'LinkedMVar' und rufe deren Update-Aktion auf.
-- Wenn die 'LinkedMVar' aktuell gefüllt ist, warte bis diese geleert wird.
putLinkedMVar :: LinkedMVar a -> a -> IO ()
putLinkedMVar (LinkedMVar {linkedUpdate, mvar}) v = do
    -- Stelle sicher, dass reverse nur einmal auf Update-Queue aufgerufen werden muss
    updateQueue <- modifyMVar linkedUpdate $ (\a -> pure (a, a)) . vonListe . toList
    vAfter <- foldM (&) v updateQueue
    putMVar mvar vAfter

-- | Modifiziere den aktuellen Wert einer 'LinkedMVar' und rufe deren Update-Aktion auf.
-- 'modifyLinkedMVar_' ist nur atomar, wenn es nur einen Producer für diese 'LinkedMVar' gibt.
modifyLinkedMVar_ :: LinkedMVar a -> (a -> IO a) -> IO ()
modifyLinkedMVar_ lmvar f = modifyLinkedMVar lmvar $ \a -> (\res -> (res, ())) <$> f a

-- | Eine leiche Variation zu 'modifyLinkedMVar_', welche einen Rückgabewert erlaubt.
modifyLinkedMVar :: LinkedMVar a -> (a -> IO (a, b)) -> IO b
modifyLinkedMVar lmvar f = takeLinkedMVar lmvar >>= f >>= \(a, b) -> putLinkedMVar lmvar a >> pure b

-- | Nehme einen Wert aus der aktullen 'LinkedMVar' und ersetzte ihn durch einen neuen. Gebe den ursprünglichen Wert zurück.
-- 
-- Die Funktion ist nur atomar, wenn es keinen anderen Producer für diese 'LinkedMVar' gibt.
swapLinkedMVar :: LinkedMVar a -> a -> IO a
swapLinkedMVar linkedMVar v = takeLinkedMVar linkedMVar >>= \old -> putLinkedMVar linkedMVar v >> pure old

-- | Prüfe, ob die 'LinkedMVar' aktuell leer ist.
-- 
-- Dabei ist zu berücksichtigen, dass es sich nur um einen Schnappschuss handelt. Wenn der Bool verarbeitet wird, kann sich der Zustand der 'LinkedMVar' bereits geändert haben.
isEmptyLinkedMVar :: LinkedMVar a -> IO Bool
isEmptyLinkedMVar (LinkedMVar {mvar}) = isEmptyMVar mvar

-- | Führe die mit einer 'LinkedMVar' assoziierte Update-Aktion aus. Verwende den aktuellen Wert dabei als Argument.
updateAktion :: LinkedMVar a -> IO ()
updateAktion = flip modifyLinkedMVar_ pure

-- * Klasse zur einfachen Verwendung
-- | Mitglieder dieser Klasse sollen sich wie eine 'MVar' verhalten
class LikeMVar m where
    readLMVar :: m a -> IO a
    takeLMVar :: m a -> IO a
    putLMVar :: m a -> a -> IO ()
    modifyLMVar_ :: m a -> (a -> IO a) -> IO ()
    modifyLMVar :: m a -> (a -> IO (a, b)) -> IO b
    swapLMVar :: m a -> a -> IO a
    isEmptyLMVar :: m a -> IO Bool

instance LikeMVar MVar where
    readLMVar :: MVar a -> IO a
    readLMVar = readMVar
    takeLMVar :: MVar a -> IO a
    takeLMVar = takeMVar
    putLMVar :: MVar a -> a -> IO ()
    putLMVar = putMVar
    modifyLMVar_ :: MVar a -> (a -> IO a) -> IO ()
    modifyLMVar_ = modifyMVar_
    modifyLMVar :: MVar a -> (a -> IO (a, b)) -> IO b
    modifyLMVar = modifyMVar
    swapLMVar :: MVar a -> a -> IO a
    swapLMVar = swapMVar
    isEmptyLMVar :: MVar a -> IO Bool
    isEmptyLMVar = isEmptyMVar

instance LikeMVar LinkedMVar where
    readLMVar :: LinkedMVar a -> IO a
    readLMVar = readLinkedMVar
    takeLMVar :: LinkedMVar a -> IO a
    takeLMVar = takeLinkedMVar
    putLMVar :: LinkedMVar a -> a -> IO ()
    putLMVar = putLinkedMVar
    modifyLMVar_ :: LinkedMVar a -> (a -> IO a) -> IO ()
    modifyLMVar_ = modifyLinkedMVar_
    modifyLMVar :: LinkedMVar a -> (a -> IO (a, b)) -> IO b
    modifyLMVar = modifyLinkedMVar
    swapLMVar :: LinkedMVar a -> a -> IO a
    swapLMVar = swapLinkedMVar
    isEmptyLMVar :: LinkedMVar a -> IO Bool
    isEmptyLMVar = isEmptyLinkedMVar