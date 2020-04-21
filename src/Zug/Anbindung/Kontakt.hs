{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonoLocalBinds #-}

module Zug.Anbindung.Kontakt
  ( Kontakt(Kontakt, koName, koFließend, kontaktAnschluss,
        koTVarSignal)
  , kontaktNew
  , KontaktKlasse(..)
  , SignalErhalten(..)
  ) where

import Control.Concurrent.STM (atomically, TVar, writeTVar, readTVar, retry, newTVarIO)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text

import Zug.Anbindung.Anschluss
       (Value(..), Anschluss(), InterruptReader(), I2CReader(), beiÄnderung, IntEdge(..))
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(..))
import Zug.Language (Anzeige(..), Sprache(), (<:>), (<=>), (<^>), (<->))
import qualified Zug.Language as Language

-- | Wurde ein Signal bei einem 'Kontakt' registriert.
data SignalErhalten
    = WarteAufSignal
    | SignalErhalten
    deriving (Show, Eq, Ord)

-- | Erhalte ein Signal, wenn ein Zug eine Kontaktschiene erreicht.
data Kontakt = MkKontakt Text Value Anschluss (TVar SignalErhalten)
    deriving (Eq)

pattern Kontakt :: Text -> Value -> Anschluss -> TVar SignalErhalten -> Kontakt
pattern Kontakt {koName, koFließend, kontaktAnschluss, koTVarSignal}
    <- MkKontakt koName koFließend kontaktAnschluss koTVarSignal

{-# COMPLETE Kontakt #-}

instance Show Kontakt where
    show :: Kontakt -> String
    show ko = Text.unpack $ anzeige ko Language.Deutsch

instance Ord Kontakt where
    compare :: Kontakt -> Kontakt -> Ordering
    compare
        Kontakt {koName = name0, koFließend = fließend0, kontaktAnschluss = anschluss0}
        Kontakt {koName = name1, koFließend = fließend1, kontaktAnschluss = anschluss1} =
        case compare name0 name1 of
            EQ -> case compare fließend0 fließend1 of
                EQ -> compare anschluss0 anschluss1
                ordering -> ordering
            ordering -> ordering

instance Anzeige Kontakt where
    anzeige :: Kontakt -> Sprache -> Text
    anzeige Kontakt {koName, kontaktAnschluss} =
        Language.kontakt
        <:> Language.name
        <=> koName <^> Language.kontakt <-> Language.anschluss <=> kontaktAnschluss

instance StreckenObjekt Kontakt where
    anschlüsse :: Kontakt -> Set Anschluss
    anschlüsse Kontakt {kontaktAnschluss} = [kontaktAnschluss]

    erhalteName :: Kontakt -> Text
    erhalteName = koName

instance StreckenAtom Kontakt where
    fließend :: Kontakt -> Value
    fließend = koFließend

-- | Sammel-Klasse für 'Kontakt'-artige Typen
class (StreckenObjekt k) => KontaktKlasse k where
    -- | Blockiere den aktuellen Thread, bis ein 'Kontakt'-Ereignis eintritt.
    warteAufSignal :: (MonadIO m) => k -> m ()

instance (KontaktKlasse (k 'Märklin), KontaktKlasse (k 'Lego))
    => KontaktKlasse (ZugtypEither k) where
    warteAufSignal :: (MonadIO m) => ZugtypEither k -> m ()
    warteAufSignal (ZugtypMärklin k) = warteAufSignal k
    warteAufSignal (ZugtypLego k) = warteAufSignal k

instance KontaktKlasse Kontakt where
    warteAufSignal :: (MonadIO m) => Kontakt -> m ()
    warteAufSignal Kontakt {koTVarSignal} = liftIO $ do
        -- Stelle sicher, dass nur neue Signale die Blockade aufheben.
        atomically $ writeTVar koTVarSignal WarteAufSignal
        -- Blockiere, bis das erste Signal registriert wird.
        atomically $ readTVar koTVarSignal >>= \case
            SignalErhalten -> pure ()
            WarteAufSignal -> retry

-- | Erzeuge einen neuen 'Kontakt' und stelle sicher, dass der zugehörige 'Anschluss' auf
-- Eingaben reagiert.
kontaktNew
    :: (InterruptReader r m, I2CReader r m, MonadIO m) => Text -> Value -> Anschluss -> m Kontakt
kontaktNew koName koFließend kontaktAnschluss = do
    koTVarSignal <- liftIO $ newTVarIO WarteAufSignal
    beiÄnderung
        kontaktAnschluss
        (if koFließend == LOW
             then INT_EDGE_FALLING
             else INT_EDGE_RISING)
        $ atomically
        $ writeTVar koTVarSignal SignalErhalten
    pure $ MkKontakt koName koFließend kontaktAnschluss koTVarSignal