{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description: Steuere die Stromzufuhr einer Schiene.
-}
module Zug.Anbindung.Streckenabschnitt (Streckenabschnitt(..), StreckenabschnittKlasse(..)) where

import Control.Applicative (Alternative(..))
import Control.Monad.Trans (MonadIO())
import Data.Aeson.Types ((.:), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.Semigroup (Semigroup((<>)))
import Data.Set (Set)
import Data.Text (Text)

import Zug.Anbindung.Anschluss (Value(), AnschlussEither(), AnschlussKlasse(anschlussWrite)
                              , I2CReader(), parseAnschlussEither, parseFließend)
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..), befehlAusführen, VersionReader)
import Zug.Enums (Zugtyp(..), ZugtypEither(..), Strom())
import qualified Zug.JSONStrings as JS
import Zug.Language (Anzeige(..), Sprache(), showText, (<:>), (<=>), (<^>), (<->))
import qualified Zug.Language as Language

-- | Steuere die Stromzufuhr einer Schiene.
data Streckenabschnitt =
    Streckenabschnitt { stName :: Text, stFließend :: Value, stromAnschluss :: AnschlussEither }
    deriving (Eq, Ord, Show)

instance Anzeige Streckenabschnitt where
    anzeige :: Streckenabschnitt -> Sprache -> Text
    anzeige Streckenabschnitt {stName, stromAnschluss} =
        Language.streckenabschnitt
        <:> Language.name <=> stName <^> Language.strom <-> Language.anschluss <=> stromAnschluss

instance StreckenObjekt Streckenabschnitt where
    anschlüsse :: Streckenabschnitt -> Set AnschlussEither
    anschlüsse Streckenabschnitt {stromAnschluss} = [stromAnschluss]

    erhalteName :: Streckenabschnitt -> Text
    erhalteName Streckenabschnitt {stName} = stName

instance StreckenAtom Streckenabschnitt where
    fließend :: Streckenabschnitt -> Value
    fließend = stFließend

-- | Sammel-Klasse für 'Streckenabschnitt'-artige Typen.
class (StreckenObjekt s) => StreckenabschnittKlasse s where
    -- | Strom ein-/ausschalten
    strom :: (I2CReader r m, VersionReader r m, MonadIO m) => s -> Strom -> m ()

instance (StreckenabschnittKlasse (s 'Märklin), StreckenabschnittKlasse (s 'Lego))
    => StreckenabschnittKlasse (ZugtypEither s) where
    strom :: (I2CReader r m, VersionReader r m, MonadIO m) => ZugtypEither s -> Strom -> m ()
    strom (ZugtypMärklin a) = strom a
    strom (ZugtypLego a) = strom a

instance StreckenabschnittKlasse Streckenabschnitt where
    strom :: (I2CReader r m, VersionReader r m, MonadIO m) => Streckenabschnitt -> Strom -> m ()
    strom st@Streckenabschnitt {stromAnschluss} an =
        befehlAusführen
            (anschlussWrite stromAnschluss $ erhalteValue an st)
            ("Strom (" <> showText stromAnschluss <> ")->" <> showText an)

-- JSON-Instanz-Deklarationen für Streckenabschnitt
instance Aeson.FromJSON Streckenabschnitt where
    parseJSON :: Aeson.Value -> Aeson.Parser Streckenabschnitt
    parseJSON (Aeson.Object v) =
        Streckenabschnitt <$> v .: JS.name
        <*> parseFließend v
        <*> parseAnschlussEither v JS.stromAnschluss JS.stromPin
    parseJSON _value = empty

instance Aeson.ToJSON Streckenabschnitt where
    toJSON :: Streckenabschnitt -> Aeson.Value
    toJSON Streckenabschnitt {stName, stFließend, stromAnschluss} =
        Aeson.object
            [JS.name .= stName, JS.fließend .= stFließend, JS.stromAnschluss .= stromAnschluss]
