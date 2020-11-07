{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Zug.Anbindung.Kontakt (Kontakt(..), KontaktKlasse(..)) where

import Control.Applicative (Alternative(..))
import Control.Monad.Trans (MonadIO())
import Data.Aeson.Types ((.:), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.Set (Set)
import Data.Text (Text)

import Zug.Anbindung.Anschluss
       (Value(..), Anschluss(), AnschlussEither(AnschlussMit), MitInterruptPin(MitInterruptPin)
      , InterruptReader(), I2CReader(), warteAufÄnderung, IntEdge(..))
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(..))
import qualified Zug.JSONStrings as JS
import Zug.Language (Anzeige(..), Sprache(), (<:>), (<=>), (<^>), (<->))
import qualified Zug.Language as Language

-- | Erhalte ein Signal, wenn ein Zug eine Kontaktschiene erreicht.
data Kontakt =
    Kontakt
    { koName :: Text
    , koFließend :: Value
    , kontaktAnschluss :: Anschluss 'MitInterruptPin
    }
    deriving (Show, Eq, Ord)

instance Anzeige Kontakt where
    anzeige :: Kontakt -> Sprache -> Text
    anzeige Kontakt {koName, kontaktAnschluss} =
        Language.kontakt
        <:> Language.name
        <=> koName <^> Language.kontakt <-> Language.anschluss <=> kontaktAnschluss

instance StreckenObjekt Kontakt where
    anschlüsse :: Kontakt -> Set AnschlussEither
    anschlüsse Kontakt {kontaktAnschluss} = [AnschlussMit kontaktAnschluss]

    erhalteName :: Kontakt -> Text
    erhalteName = koName

instance StreckenAtom Kontakt where
    fließend :: Kontakt -> Value
    fließend = koFließend

-- | Sammel-Klasse für 'Kontakt'-artige Typen.
class (StreckenObjekt k) => KontaktKlasse k where
    -- | Blockiere den aktuellen Thread, bis ein 'Kontakt'-Ereignis eintritt.
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => k -> m ()

instance (KontaktKlasse (k 'Märklin), KontaktKlasse (k 'Lego))
    => KontaktKlasse (ZugtypEither k) where
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => ZugtypEither k -> m ()
    warteAufSignal (ZugtypMärklin k) = warteAufSignal k
    warteAufSignal (ZugtypLego k) = warteAufSignal k

instance KontaktKlasse Kontakt where
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => Kontakt -> m ()
    warteAufSignal Kontakt {koFließend, kontaktAnschluss} =
        warteAufÄnderung
            [ ( kontaktAnschluss
                  , if koFließend == LOW
                    then INT_EDGE_FALLING
                    else INT_EDGE_RISING
                  )]

-- JSON-Instanz-Deklarationen für Kontakt
instance Aeson.FromJSON Kontakt where
    parseJSON :: Aeson.Value -> Aeson.Parser Kontakt
    parseJSON (Aeson.Object v) =
        Kontakt <$> v .: JS.name <*> v .: JS.fließend <*> v .: JS.kontaktAnschluss
    parseJSON _value = empty

instance Aeson.ToJSON Kontakt where
    toJSON :: Kontakt -> Aeson.Value
    toJSON Kontakt {koName, koFließend, kontaktAnschluss} =
        Aeson.object
            [ JS.name .= koName
            , JS.fließend .= koFließend
            , JS.kontaktAnschluss .= kontaktAnschluss]
