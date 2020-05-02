{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description: Kontrolliere, wann Wagons über eine Kupplungs-Schiene abgekoppelt werden.
-}
module Zug.Anbindung.Kupplung (Kupplung(..), KupplungKlasse(..), kuppelnZeit) where

import Control.Monad.Trans (MonadIO())
import Data.Semigroup (Semigroup((<>)))
import Data.Set (Set)
import Data.Text (Text)

import Zug.Anbindung.Anschluss
       (Value(), AnschlussEither(), AnschlussKlasse(anschlussWrite), I2CReader())
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..), befehlAusführen)
import Zug.Anbindung.Wartezeit (Wartezeit(MilliSekunden), warte)
import Zug.Enums (Zugtyp(..), ZugtypEither(..))
import Zug.Language (Anzeige(..), Sprache(), showText, (<:>), (<=>), (<^>), (<->))
import qualified Zug.Language as Language

-- | Kontrolliere, wann Wagons über eine Kupplungs-Schiene abgekoppelt werden.
data Kupplung =
    Kupplung { kuName :: Text, kuFließend :: Value, kupplungsAnschluss :: AnschlussEither }
    deriving (Eq, Ord, Show)

instance Anzeige Kupplung where
    anzeige :: Kupplung -> Sprache -> Text
    anzeige Kupplung {kuName, kupplungsAnschluss} =
        Language.kupplung
        <:> Language.name
        <=> kuName <^> Language.kupplung <-> Language.anschluss <=> kupplungsAnschluss

instance StreckenObjekt Kupplung where
    anschlüsse :: Kupplung -> Set AnschlussEither
    anschlüsse Kupplung {kupplungsAnschluss} = [kupplungsAnschluss]

    erhalteName :: Kupplung -> Text
    erhalteName Kupplung {kuName} = kuName

instance StreckenAtom Kupplung where
    fließend :: Kupplung -> Value
    fließend = kuFließend

-- | Sammel-Klasse für 'Kupplung'-artige Typen.
class (StreckenObjekt k) => KupplungKlasse k where
    -- | Kupplung betätigen
    kuppeln :: (I2CReader r m, MonadIO m) => k -> m ()

instance (KupplungKlasse (ku 'Märklin), KupplungKlasse (ku 'Lego))
    => KupplungKlasse (ZugtypEither ku) where
    kuppeln :: (I2CReader r m, MonadIO m) => ZugtypEither ku -> m ()
    kuppeln (ZugtypMärklin a) = kuppeln a
    kuppeln (ZugtypLego a) = kuppeln a

-- | Zeit, die Strom beim Kuppeln anliegt
kuppelnZeit :: Wartezeit
kuppelnZeit = MilliSekunden 300

instance KupplungKlasse Kupplung where
    kuppeln :: (I2CReader r m, MonadIO m) => Kupplung -> m ()
    kuppeln ku@Kupplung {kupplungsAnschluss} =
        flip befehlAusführen ("Kuppeln (" <> showText kupplungsAnschluss <> ")") $ do
            anschlussWrite kupplungsAnschluss $ fließend ku
            warte kuppelnZeit
            anschlussWrite kupplungsAnschluss $ gesperrt ku
