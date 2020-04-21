{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description: Steuere die Stromzufuhr einer Schiene.
-}
module Zug.Anbindung.Streckenabschnitt (Streckenabschnitt(..), StreckenabschnittKlasse(..)) where

import Control.Monad.Trans (MonadIO())
import Data.Semigroup (Semigroup((<>)))
import Data.Set (Set)
import Data.Text (Text)

import Zug.Anbindung.Anschluss (Value(), Anschluss(), AnschlussKlasse(anschlussWrite), I2CReader())
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..), befehlAusführen)
import Zug.Enums (Zugtyp(..), ZugtypEither(..), Strom())
import Zug.Language (Anzeige(..), Sprache(), showText, (<:>), (<=>), (<^>), (<->))
import qualified Zug.Language as Language

-- | Steuere die Stromzufuhr einer Schiene.
data Streckenabschnitt =
    Streckenabschnitt { stName :: Text, stFließend :: Value, stromAnschluss :: Anschluss }
    deriving (Eq, Ord, Show)

instance Anzeige Streckenabschnitt where
    anzeige :: Streckenabschnitt -> Sprache -> Text
    anzeige Streckenabschnitt {stName, stromAnschluss} =
        Language.streckenabschnitt
        <:> Language.name <=> stName <^> Language.strom <-> Language.anschluss <=> stromAnschluss

instance StreckenObjekt Streckenabschnitt where
    anschlüsse :: Streckenabschnitt -> Set Anschluss
    anschlüsse Streckenabschnitt {stromAnschluss} = [stromAnschluss]

    erhalteName :: Streckenabschnitt -> Text
    erhalteName Streckenabschnitt {stName} = stName

instance StreckenAtom Streckenabschnitt where
    fließend :: Streckenabschnitt -> Value
    fließend = stFließend

-- | Sammel-Klasse für 'Streckenabschnitt'-artige Typen
class (StreckenObjekt s) => StreckenabschnittKlasse s where
    -- | Strom ein-/ausschalten
    strom :: (I2CReader r m, MonadIO m) => s -> Strom -> m ()
    {-# MINIMAL strom #-}

instance (StreckenabschnittKlasse (s 'Märklin), StreckenabschnittKlasse (s 'Lego))
    => StreckenabschnittKlasse (ZugtypEither s) where
    strom :: (I2CReader r m, MonadIO m) => ZugtypEither s -> Strom -> m ()
    strom (ZugtypMärklin a) = strom a
    strom (ZugtypLego a) = strom a

instance StreckenabschnittKlasse Streckenabschnitt where
    strom :: (I2CReader r m, MonadIO m) => Streckenabschnitt -> Strom -> m ()
    strom st@Streckenabschnitt {stromAnschluss} an =
        befehlAusführen
            (anschlussWrite stromAnschluss $ erhalteValue an st)
            ("Strom (" <> showText stromAnschluss <> ")->" <> showText an)