{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonoLocalBinds #-}

module Zug.Anbindung.Kontakt (Kontakt(..), KontaktKlasse(..)) where

import Control.Monad.Trans (MonadIO())
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Zug.Anbindung.Anschluss
       (Value(..), Anschluss(), InterruptReader(), I2CReader(), warteAufÄnderung, IntEdge(..))
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(..))
import Zug.Language (Anzeige(..), Sprache(), (<:>), (<=>), (<^>), (<->))
import qualified Zug.Language as Language

-- | Erhalte ein Signal, wenn ein Zug eine Kontaktschiene erreicht.
data Kontakt = Kontakt { koName :: Text, koFließend :: Value, kontaktAnschluss :: Anschluss }
    deriving (Show, Eq, Ord)

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
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => k -> m ()

    -- | Alle enthaltenen Kontakte.
    enthalteneKontakte :: k -> Set Kontakt

instance (KontaktKlasse (k 'Märklin), KontaktKlasse (k 'Lego))
    => KontaktKlasse (ZugtypEither k) where
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => ZugtypEither k -> m ()
    warteAufSignal (ZugtypMärklin k) = warteAufSignal k
    warteAufSignal (ZugtypLego k) = warteAufSignal k

    enthalteneKontakte :: ZugtypEither k -> Set Kontakt
    enthalteneKontakte (ZugtypMärklin k) = enthalteneKontakte k
    enthalteneKontakte (ZugtypLego k) = enthalteneKontakte k

instance KontaktKlasse Kontakt where
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => Kontakt -> m ()
    warteAufSignal Kontakt {koFließend, kontaktAnschluss} =
        warteAufÄnderung
            [ ( kontaktAnschluss
                  , if koFließend == LOW
                    then INT_EDGE_FALLING
                    else INT_EDGE_RISING
                  )]

    enthalteneKontakte :: Kontakt -> Set Kontakt
    enthalteneKontakte = Set.singleton