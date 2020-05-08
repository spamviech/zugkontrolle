{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Description: Stellen einer Weiche.
-}
module Zug.Anbindung.Weiche (Weiche(..), WeicheKlasse(..), weicheZeit) where

import Control.Monad.Trans (MonadIO())
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup((<>)))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Zug.Anbindung.Anschluss (Value(), AnschlussEither(), Pin()
                              , AnschlussKlasse(anschlussWrite, zuAnschluss), I2CReader())
import Zug.Anbindung.Klassen (StreckenAtom(..), StreckenObjekt(..), befehlAusführen)
import Zug.Anbindung.Pwm (PwmReader, pwmServo)
import Zug.Anbindung.Wartezeit (Wartezeit(MilliSekunden), warte)
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(), Richtung())
import Zug.Language (Anzeige(..), Sprache(), showText, (<:>), (<=>), (<^>), (<->), (<|>))
import qualified Zug.Language as Language

-- | Stellen einer 'Weiche'.
data Weiche (z :: Zugtyp) where
    WeicheMärklin :: { wemName :: Text
                      , wemFließend :: Value
                      , wemRichtungsAnschlüsse :: NonEmpty (Richtung, AnschlussEither)
                      } -> Weiche 'Märklin
    WeicheLego :: { welName :: Text
                  , welFließend :: Value
                  , welRichtungsPin :: Pin
                  , welRichtungen :: (Richtung, Richtung)
                  } -> Weiche 'Lego

deriving instance Eq (Weiche z)

deriving instance Ord (Weiche z)

deriving instance Show (Weiche z)

instance Anzeige (Weiche z) where
    anzeige :: Weiche z -> Sprache -> Text
    anzeige WeicheLego {welName, welRichtungsPin, welRichtungen = (richtung1, richtung2)} =
        Language.lego
        <-> Language.weiche
        <:> Language.name
        <=> welName
        <^> Language.richtung
        <-> Language.pin <=> welRichtungsPin <^> Language.richtungen <=> richtung1 <|> richtung2
    anzeige WeicheMärklin {wemName, wemRichtungsAnschlüsse} =
        Language.märklin
        <-> Language.weiche
        <:> Language.name
        <=> wemName
        <^> foldl
            (\acc (anschluss, richtung) -> acc <^> richtung <=> anschluss)
            (const "")
            wemRichtungsAnschlüsse

instance StreckenObjekt (Weiche z) where
    anschlüsse :: Weiche z -> Set AnschlussEither
    anschlüsse WeicheLego {welRichtungsPin} = [zuAnschluss welRichtungsPin]
    anschlüsse WeicheMärklin {wemRichtungsAnschlüsse} =
        Set.fromList $ map snd $ NonEmpty.toList wemRichtungsAnschlüsse

    erhalteName :: Weiche z -> Text
    erhalteName WeicheLego {welName} = welName
    erhalteName WeicheMärklin {wemName} = wemName

instance StreckenAtom (Weiche z) where
    fließend :: Weiche z -> Value
    fließend WeicheLego {welFließend} = welFließend
    fließend WeicheMärklin {wemFließend} = wemFließend

-- | Sammel-Klasse für 'Weiche'n-artige Typen
class (StreckenObjekt w) => WeicheKlasse w where
    -- | Weiche einstellen
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => w -> Richtung -> m ()

    -- | Überprüfe, ob Weiche eine Richtung unterstützt
    hatRichtung :: w -> Richtung -> Bool
    hatRichtung weiche richtung = elem richtung $ erhalteRichtungen weiche

    -- | Erhalte alle Richtungen einer Weiche
    erhalteRichtungen :: w -> NonEmpty Richtung
    {-# MINIMAL stellen, erhalteRichtungen #-}

instance (WeicheKlasse (we 'Märklin), WeicheKlasse (we 'Lego))
    => WeicheKlasse (ZugtypEither we) where
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => ZugtypEither we -> Richtung -> m ()
    stellen (ZugtypMärklin a) = stellen a
    stellen (ZugtypLego a) = stellen a

    erhalteRichtungen :: ZugtypEither we -> NonEmpty Richtung
    erhalteRichtungen (ZugtypMärklin a) = erhalteRichtungen a
    erhalteRichtungen (ZugtypLego a) = erhalteRichtungen a

-- | Zeit, die Strom beim Stellen einer Märklin-Weiche anliegt
weicheZeit :: Wartezeit
weicheZeit = MilliSekunden 250

instance (ZugtypKlasse z) => WeicheKlasse (Weiche z) where
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => Weiche z -> Richtung -> m ()
    stellen we@WeicheLego {welRichtungsPin, welRichtungen} richtung
        | richtung == fst welRichtungen =
            flip
                befehlAusführen
                ("Stellen (" <> showText welRichtungsPin <> ") -> " <> showText richtung)
            $ do
                pwmServo we welRichtungsPin 25
                warte weicheZeit
                pwmServo we welRichtungsPin 0
        | richtung == snd welRichtungen =
            flip
                befehlAusführen
                ("stellen (" <> showText welRichtungsPin <> ") -> " <> showText richtung)
            $ do
                pwmServo we welRichtungsPin 75
                warte weicheZeit
                pwmServo we welRichtungsPin 0
        | otherwise = pure ()
    stellen we@WeicheMärklin {wemRichtungsAnschlüsse} richtung =
        befehlAusführen
            richtungStellen
            ("Stellen ("
             <> showText (lookup richtung $ NonEmpty.toList wemRichtungsAnschlüsse)
             <> ") -> "
             <> showText richtung)
        where
            richtungStellen :: (I2CReader r m, MonadIO m) => m ()
            richtungStellen = case lookup richtung $ NonEmpty.toList wemRichtungsAnschlüsse of
                Nothing -> pure ()
                (Just richtungsAnschluss) -> do
                    anschlussWrite richtungsAnschluss $ fließend we
                    warte weicheZeit
                    anschlussWrite richtungsAnschluss $ gesperrt we

    hatRichtung :: Weiche z -> Richtung -> Bool
    hatRichtung WeicheLego {welRichtungen = (erste, zweite)} richtung =
        (erste == richtung) || (zweite == richtung)
    hatRichtung WeicheMärklin {wemRichtungsAnschlüsse} richtung =
        any ((richtung ==) . fst) wemRichtungsAnschlüsse

    erhalteRichtungen :: Weiche z -> NonEmpty Richtung
    erhalteRichtungen
        WeicheLego {welRichtungen = (richtung1, richtung2)} = richtung1 :| [richtung2]
    erhalteRichtungen WeicheMärklin {wemRichtungsAnschlüsse} = fst <$> wemRichtungsAnschlüsse
