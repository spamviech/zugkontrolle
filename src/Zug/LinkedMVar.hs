{-# LANGUAGE NamedFieldPuns, InstanceSigs #-}

{-|
Description : 'MVar', welche beim setzen/updaten eine IO-Aktion ausführen.
-}
module Zug.LinkedMVar (
                    -- * Datentyp und Konstruktoren
                    LinkedMVar(), newEmptyLinkedMVar, newLinkedMVar, appendEmptyLinkedMVar, appendLinkedMVar,
                    -- * Funktionen
                    readLinkedMVar, takeLinkedMVar, putLinkedMVar, modifyLinkedMVar_, modifyLinkedMVar, swapLinkedMVar, isEmptyLinkedMVar, updateAktion,
                    -- * Klasse zur einfacheren Verwendung
                    LikeMVar(..)) where

-- Bibliotheken
import Control.Concurrent.MVar
import Control.Monad (void)

-- | 'MVar', welche beim setzen des Werts eine Update-Aktion ausführt.
-- Das Ergebnis der Update-Aktion wird der neue Wert der 'LinkedMVar', ohne ein erneuten Aufruf der Update-Aktion auszulösen.
-- Wenn ein erneutes ausführen der Update-Aktion gewünscht wird kann dies mit der übergebenen IO-Aktion erzwungen werden.
data LinkedMVar a = LinkedMVar {linkedUpdate :: IO () -> a -> IO a, mvar :: MVar a}

-- * Konstruktoren
-- | Erzeuge eine neue leere 'LinkedMVar', ausgehend von einer Update-Aktion.
-- Diese wird immer aufgerufen, wenn sich der Wert der 'LinkedMVar' ändert.
-- Der neue Wert wird an die Update-Aktion übergeben.
newEmptyLinkedMVar :: (IO () -> a -> IO a)  -> IO (LinkedMVar a)
newEmptyLinkedMVar update = newEmptyMVar >>= pure . LinkedMVar update

newLinkedMVar :: (IO () -> a -> IO a) -> a -> IO (LinkedMVar a)
newLinkedMVar update a = newEmptyLinkedMVar update >>= \lmvar -> putLinkedMVar lmvar a >> pure lmvar

-- | Füge einer LinkedMVar eine neue Update-Aktion hinzu. Der Wert der so erzeugten 'LinkedMVar' ist unabhängig von der ursprünglichen 'LinkedMVar'.
-- Ein ändern der ursprünglichen 'LinkedMVar' ruft nur die ursprüngliche Update-Aktion auf.
-- Die Update-Aktionen werden in append-Reihenfolge (realisiert über monadischen '>>='-Operator) aufgerufen.
appendEmptyLinkedMVar :: LinkedMVar a -> (IO () -> a -> IO a) -> IO (LinkedMVar a)
appendEmptyLinkedMVar old newCommand = newEmptyLinkedMVar $ \forceUpdate a -> (linkedUpdate old) forceUpdate a >>= newCommand forceUpdate

appendLinkedMVar :: LinkedMVar a -> (IO () -> a -> IO a) -> a -> IO (LinkedMVar a)
appendLinkedMVar old newCommand a = appendEmptyLinkedMVar old newCommand >>= \lmvar -> putLinkedMVar lmvar a >> pure lmvar

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
putLinkedMVar linkedMVar@(LinkedMVar {mvar, linkedUpdate}) v = void $ putMVar mvar v >> linkedUpdate (updateAktion linkedMVar) v

-- | Modifiziere den aktuellen Wert einer 'LinkedMVar' und rufe deren Update-Aktion auf.
-- 'modifyLinkedMVar_' ist nur atomar, wenn es nur einen Producer für diese 'LinkedMVar' gibt.
modifyLinkedMVar_ :: LinkedMVar a -> (a -> IO a) -> IO ()
modifyLinkedMVar_   lmvar f = modifyLinkedMVar lmvar $ \a -> f a >>= \res -> pure (res, ())

-- | Eine leiche Variation zu 'modifyLinkedMVar_', welche einen Rückgabewert erlaubt.
modifyLinkedMVar :: LinkedMVar a -> (a -> IO (a, b)) -> IO b
modifyLinkedMVar    lmvar f = readLinkedMVar lmvar >>= f >>= \(a, b) -> swapLinkedMVar lmvar a >> pure b

-- | Nehme einen Wert aus der aktullen 'LinkedMVar' und ersetzte ihn durch einen neuen. Gebe den ursprünglichen Wert zurück.
-- 
-- Die Funktion ist nur atomar, wenn es keinen anderen Producer für diese 'LinkedMVar' gibt.
swapLinkedMVar :: LinkedMVar a -> a -> IO a
swapLinkedMVar linkedMVar@(LinkedMVar {mvar, linkedUpdate}) v = swapMVar mvar v >>= \old -> linkedUpdate (updateAktion linkedMVar) v >> pure old

-- | Führe die mit einer 'LinkedMVar' assoziierte Update-Aktion aus. Verwende den aktuellen Wert dabei als Argument.
updateAktion :: LinkedMVar a -> IO ()
updateAktion = flip modifyLinkedMVar_ pure

-- | Prüfe, ob die 'LinkedMVar' aktuell leer ist.
-- 
-- Dabei ist zu berücksichtigen, dass es sich nur um einen Schnappschuss handelt. Wenn der Bool verarbeitet wird, kann sich der Zustand der 'LinkedMVar' bereits geändert haben.
isEmptyLinkedMVar :: LinkedMVar a -> IO Bool
isEmptyLinkedMVar (LinkedMVar {mvar}) = isEmptyMVar mvar

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