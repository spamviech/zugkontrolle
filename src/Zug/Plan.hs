{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}

{-|
Description : Pläne sind nacheinander auszuführende Aktionen, welche mit StreckenObjekten möglich sind.

Jede Art von 'StreckenObjekt' ('Bahngeschwindigkeit', 'Streckenabschnitt', 'Weiche', 'Wegstrecke') unterstützt unterschiedliche Aktionen.
Ein 'Plan' ist eine Zusammenfassung mehrerer dieser Aktionen und Wartezeiten, welche nacheinander ausgeführt werden können.
-}
module Zug.Plan
  ( -- * Allgemeine Datentypen
    PlanKlasse(..)
  , MitAusführend(..)
  , AusführendReader(..)
  , Plan(..)
  , AktionKlasse(..)
  , Aktion(..)
  , Ausführend(..)
    -- * Spezialisierte Aktionen
  , AktionWeiche(..)
  , AktionBahngeschwindigkeit(..)
  , AktionStreckenabschnitt(..)
  , AktionKupplung(..)
  , AktionWegstrecke(..)
  ) where

import Control.Concurrent.STM (atomically, TVar, readTVarIO, modifyTVar)
import Control.Monad (void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (MonadIO(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Zug.Anbindung
       (Anschluss(), StreckenObjekt(..), PwmReader(..), I2CReader(..), InterruptReader()
      , Bahngeschwindigkeit(), BahngeschwindigkeitKlasse(..), Streckenabschnitt()
      , StreckenabschnittKlasse(..), Weiche(), WeicheKlasse(..), Kupplung(), KupplungKlasse(..)
      , Wegstrecke(), WegstreckeKlasse(..), warte, Wartezeit(..), Kontakt(..), KontaktKlasse(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(), GeschwindigkeitVariante(..), GeschwindigkeitEither(..)
                , GeschwindigkeitPhantom(..), Richtung(), Fahrtrichtung(), Strom(..))
import qualified Zug.Language as Language
import Zug.Language (Anzeige(..), Sprache(), showText, (<~>), (<^>), (<=>), (<:>), (<°>))

-- | Klasse für Typen mit den aktuell 'Ausführend'en Plänen
class MitAusführend r where
    mengeAusführend :: r -> TVar (Set Ausführend)

-- | Abkürzung für Funktionen, die die aktuelle 'Ausführend'-'Set' benötigen
class (I2CReader r m, PwmReader r m, InterruptReader r m, MitAusführend r)
    => AusführendReader r m where
    -- | Erhalte die aktuelle 'Ausführend'-'Menge' aus der Umgebung.
    erhalteMengeAusführend :: m (TVar (Set Ausführend))
    erhalteMengeAusführend = asks mengeAusführend

instance (I2CReader r m, PwmReader r m, InterruptReader r m, MitAusführend r)
    => AusführendReader r m

-- | Mitglieder dieser Klasse sind ausführbar (können in IO-Aktionen übersetzt werden).  
-- Sie können selbst entscheiden, wann sie die mitgegebene Update-Funktion über den Fortschritt informieren.  
-- Nach der kompletten Ausführung soll der End-Aktion ausgeführt werden.  
-- Die Ausführung soll abgebrochen werden, sobald der Plan nicht mehr in der 'TVar'-'Set' vorhanden ist.
class PlanKlasse pl where
    ausführenPlan
        :: (AusführendReader r m, MonadIO m) => pl -> (Natural -> IO ()) -> IO () -> m ()
    {-# MINIMAL ausführenPlan #-}

-- | Pläne: Benannte IO-Aktionen mit StreckenObjekten, bzw. Wartezeiten.
-- Die Update-Funktion wird mit Index der aktuellen Aktion vor dessen Ausführung aufgerufen.
data Plan = Plan { plName :: Text, plAktionen :: [Aktion] }
    deriving (Eq, Ord)

deriving instance ( Show (GeschwindigkeitPhantom Wegstrecke 'Pwm 'Märklin)
                  , Show (GeschwindigkeitPhantom Wegstrecke 'KonstanteSpannung 'Märklin)
                  ) => Show Plan

-- | newtype für ausführende Pläne ('Plan')
newtype Ausführend = Ausführend Plan
    deriving (Eq, Ord, StreckenObjekt)

deriving instance ( Show (GeschwindigkeitPhantom Wegstrecke 'Pwm 'Märklin)
                  , Show (GeschwindigkeitPhantom Wegstrecke 'KonstanteSpannung 'Märklin)
                  ) => Show Ausführend

instance Anzeige Plan where
    anzeige :: Plan -> Sprache -> Text
    anzeige Plan {plName, plAktionen} =
        Language.plan <:> Language.name <=> plName <^> Language.aktionen <=> plAktionen

instance StreckenObjekt Plan where
    anschlüsse :: Plan -> Set Anschluss
    anschlüsse Plan {plAktionen} = foldMap anschlüsse plAktionen

    erhalteName :: Plan -> Text
    erhalteName Plan {plName} = plName

instance PlanKlasse Plan where
    ausführenPlan
        :: (AusführendReader r m, MonadIO m) => Plan -> (Natural -> IO ()) -> IO () -> m ()
    ausführenPlan plan@Plan {plAktionen} showAktion endAktion = void $ forkI2CReader $ void $ do
        tvarAusführend <- erhalteMengeAusführend
        liftIO $ atomically $ modifyTVar tvarAusführend $ Set.insert $ Ausführend plan
        ausführenAux 0 plAktionen
        liftIO $ do
            showAktion $ fromIntegral $ length plAktionen
            endAktion
        where
            ausführenAux :: (AusführendReader r m, MonadIO m) => Natural -> [Aktion] -> m ()
            ausführenAux _i [] = do
                tvarAusführend <- erhalteMengeAusführend
                liftIO $ atomically $ modifyTVar tvarAusführend $ Set.delete $ Ausführend plan
            ausführenAux _i [AktionAusführen Plan {plAktionen = plAktionen1}] =
                ausführenAux 0 plAktionen1
            ausführenAux i (h:t) = do
                tvarAusführend <- erhalteMengeAusführend
                ausführend <- liftIO $ readTVarIO tvarAusführend
                when (Ausführend plan `elem` ausführend) $ do
                    liftIO $ showAktion i
                    ausführenAktion h
                    ausführenAux (succ i) t

-- | Mitglieder dieser Klasse sind ausführbar.
class AktionKlasse a where
    ausführenAktion :: (AusführendReader r m, MonadIO m) => a -> m ()

-- | Eine Aktion eines 'StreckenObjekt's oder eine Wartezeit.
-- Die Update-Funktion wird nicht aufgerufen.
data Aktion
    = Warten Wartezeit
    | WartenAuf Kontakt
    | AWegstreckeMärklin (AktionWegstrecke Wegstrecke 'Märklin)
    | AWegstreckeLego (AktionWegstrecke Wegstrecke 'Lego)
    | AWeiche (AktionWeiche (ZugtypEither Weiche))
    | ABahngeschwindigkeitMärklinPwm (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Pwm 'Märklin)
    | ABahngeschwindigkeitMärklinKonstanteSpannung (AktionBahngeschwindigkeit Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
    | ABahngeschwindigkeitLegoPwm (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Pwm 'Lego)
    | ABahngeschwindigkeitLegoKonstanteSpannung (AktionBahngeschwindigkeit Bahngeschwindigkeit 'KonstanteSpannung 'Lego)
    | AStreckenabschnitt (AktionStreckenabschnitt Streckenabschnitt)
    | AKupplung (AktionKupplung Kupplung)
    | AktionAusführen Plan

deriving instance ( Show (GeschwindigkeitPhantom Wegstrecke 'Pwm 'Märklin)
                  , Show (GeschwindigkeitPhantom Wegstrecke 'KonstanteSpannung 'Märklin)
                  ) => Show Aktion

instance Eq Aktion where
    (==) :: Aktion -> Aktion -> Bool
    (==) a0 a1 = compare a0 a1 == EQ

instance Ord Aktion where
    compare :: Aktion -> Aktion -> Ordering
    compare (Warten w0) (Warten w1) = compare w0 w1
    compare (Warten _w0) (WartenAuf _k1) = GT
    compare (Warten _w0) (AWegstreckeMärklin _a1) = GT
    compare (Warten _w0) (AWegstreckeLego _a1) = GT
    compare (Warten _w0) (AWeiche _a1) = GT
    compare (Warten _w0) (ABahngeschwindigkeitMärklinPwm _a1) = GT
    compare (Warten _w0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = GT
    compare (Warten _w0) (ABahngeschwindigkeitLegoPwm _a1) = GT
    compare (Warten _w0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = GT
    compare (Warten _w0) (AStreckenabschnitt _a1) = GT
    compare (Warten _w0) (AKupplung _a1) = GT
    compare (Warten _w0) (AktionAusführen _p1) = GT
    compare (WartenAuf _k0) (Warten _w1) = LT
    compare (WartenAuf k0) (WartenAuf k1) = compare k0 k1
    compare (WartenAuf _k0) (AWegstreckeMärklin _a1) = GT
    compare (WartenAuf _k0) (AWegstreckeLego _a1) = GT
    compare (WartenAuf _k0) (AWeiche _a1) = GT
    compare (WartenAuf _k0) (ABahngeschwindigkeitMärklinPwm _a1) = GT
    compare (WartenAuf _k0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = GT
    compare (WartenAuf _k0) (ABahngeschwindigkeitLegoPwm _a1) = GT
    compare (WartenAuf _k0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = GT
    compare (WartenAuf _k0) (AStreckenabschnitt _a1) = GT
    compare (WartenAuf _k0) (AKupplung _a1) = GT
    compare (WartenAuf _k0) (AktionAusführen _p1) = GT
    compare (AWegstreckeMärklin _w0) (Warten _w1) = LT
    compare (AWegstreckeMärklin _w0) (WartenAuf _k1) = LT
    compare (AWegstreckeMärklin a0) (AWegstreckeMärklin a1) = compare a0 a1
    compare (AWegstreckeMärklin _a0) (AWegstreckeLego _a1) = GT
    compare (AWegstreckeMärklin _a0) (AWeiche _a1) = GT
    compare (AWegstreckeMärklin _a0) (ABahngeschwindigkeitMärklinPwm _a1) = GT
    compare (AWegstreckeMärklin _a0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = GT
    compare (AWegstreckeMärklin _a0) (ABahngeschwindigkeitLegoPwm _a1) = GT
    compare (AWegstreckeMärklin _a0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = GT
    compare (AWegstreckeMärklin _a0) (AStreckenabschnitt _a1) = GT
    compare (AWegstreckeMärklin _a0) (AKupplung _a1) = GT
    compare (AWegstreckeMärklin _a0) (AktionAusführen _p1) = GT
    compare (AWegstreckeLego _a0) (Warten _w1) = LT
    compare (AWegstreckeLego _a0) (WartenAuf _k1) = LT
    compare (AWegstreckeLego _a0) (AWegstreckeMärklin _a1) = LT
    compare (AWegstreckeLego a0) (AWegstreckeLego a1) = compare a0 a1
    compare (AWegstreckeLego _a0) (AWeiche _a1) = GT
    compare (AWegstreckeLego _a0) (ABahngeschwindigkeitMärklinPwm _a1) = GT
    compare (AWegstreckeLego _a0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = GT
    compare (AWegstreckeLego _a0) (ABahngeschwindigkeitLegoPwm _a1) = GT
    compare (AWegstreckeLego _a0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = GT
    compare (AWegstreckeLego _a0) (AStreckenabschnitt _a1) = GT
    compare (AWegstreckeLego _a0) (AKupplung _a1) = GT
    compare (AWegstreckeLego _a0) (AktionAusführen _p1) = GT
    compare (AWeiche _a0) (Warten _w1) = LT
    compare (AWeiche _a0) (WartenAuf _k1) = LT
    compare (AWeiche _a0) (AWegstreckeMärklin _a1) = LT
    compare (AWeiche _a0) (AWegstreckeLego _a1) = LT
    compare (AWeiche a0) (AWeiche a1) = compare a0 a1
    compare (AWeiche _a0) (ABahngeschwindigkeitMärklinPwm _a1) = GT
    compare (AWeiche _a0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = GT
    compare (AWeiche _a0) (ABahngeschwindigkeitLegoPwm _a1) = GT
    compare (AWeiche _a0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = GT
    compare (AWeiche _a0) (AStreckenabschnitt _a1) = GT
    compare (AWeiche _a0) (AKupplung _a1) = GT
    compare (AWeiche _a0) (AktionAusführen _p1) = GT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (Warten _w1) = LT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (WartenAuf _k1) = LT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (AWegstreckeMärklin _a1) = LT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (AWegstreckeLego _a1) = LT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (AWeiche _a1) = LT
    compare (ABahngeschwindigkeitMärklinPwm a0) (ABahngeschwindigkeitMärklinPwm a1) =
        compare a0 a1
    compare
        (ABahngeschwindigkeitMärklinPwm _a0)
        (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = GT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (ABahngeschwindigkeitLegoPwm _a1) = GT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) =
        GT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (AStreckenabschnitt _a1) = GT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (AKupplung _a1) = GT
    compare (ABahngeschwindigkeitMärklinPwm _a0) (AktionAusführen _p1) = GT
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (Warten _w1) = LT
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (WartenAuf _k1) = LT
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (AWegstreckeMärklin _a1) = LT
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (AWegstreckeLego _a1) = LT
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (AWeiche _a1) = LT
    compare
        (ABahngeschwindigkeitMärklinKonstanteSpannung _a0)
        (ABahngeschwindigkeitMärklinPwm _a1) = LT
    compare
        (ABahngeschwindigkeitMärklinKonstanteSpannung a0)
        (ABahngeschwindigkeitMärklinKonstanteSpannung a1) = compare a0 a1
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (ABahngeschwindigkeitLegoPwm _a1) =
        GT
    compare
        (ABahngeschwindigkeitMärklinKonstanteSpannung _a0)
        (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = GT
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (AStreckenabschnitt _a1) = GT
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (AKupplung _a1) = GT
    compare (ABahngeschwindigkeitMärklinKonstanteSpannung _a0) (AktionAusführen _p1) = GT
    compare (ABahngeschwindigkeitLegoPwm _a0) (Warten _w1) = LT
    compare (ABahngeschwindigkeitLegoPwm _a0) (WartenAuf _k1) = LT
    compare (ABahngeschwindigkeitLegoPwm _a0) (AWegstreckeMärklin _a1) = LT
    compare (ABahngeschwindigkeitLegoPwm _a0) (AWegstreckeLego _a1) = LT
    compare (ABahngeschwindigkeitLegoPwm _a0) (AWeiche _a1) = LT
    compare (ABahngeschwindigkeitLegoPwm _a0) (ABahngeschwindigkeitMärklinPwm _a1) = LT
    compare (ABahngeschwindigkeitLegoPwm _a0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) =
        LT
    compare (ABahngeschwindigkeitLegoPwm a0) (ABahngeschwindigkeitLegoPwm a1) = compare a0 a1
    compare (ABahngeschwindigkeitLegoPwm _a0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = GT
    compare (ABahngeschwindigkeitLegoPwm _a0) (AStreckenabschnitt _a1) = GT
    compare (ABahngeschwindigkeitLegoPwm _a0) (AKupplung _a1) = GT
    compare (ABahngeschwindigkeitLegoPwm _a0) (AktionAusführen _p1) = GT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (Warten _w1) = LT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (WartenAuf _k1) = LT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (AWegstreckeMärklin _a1) = LT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (AWegstreckeLego _a1) = LT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (AWeiche _a1) = LT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (ABahngeschwindigkeitMärklinPwm _a1) =
        LT
    compare
        (ABahngeschwindigkeitLegoKonstanteSpannung _a0)
        (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = LT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (ABahngeschwindigkeitLegoPwm _a1) = LT
    compare
        (ABahngeschwindigkeitLegoKonstanteSpannung a0)
        (ABahngeschwindigkeitLegoKonstanteSpannung a1) = compare a0 a1
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (AStreckenabschnitt _a1) = GT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (AKupplung _a1) = GT
    compare (ABahngeschwindigkeitLegoKonstanteSpannung _a0) (AktionAusführen _p1) = GT
    compare (AStreckenabschnitt _a0) (Warten _w1) = LT
    compare (AStreckenabschnitt _a0) (WartenAuf _k1) = LT
    compare (AStreckenabschnitt _a0) (AWegstreckeMärklin _a1) = LT
    compare (AStreckenabschnitt _a0) (AWegstreckeLego _a1) = LT
    compare (AStreckenabschnitt _a0) (AWeiche _a1) = LT
    compare (AStreckenabschnitt _a0) (ABahngeschwindigkeitMärklinPwm _a1) = LT
    compare (AStreckenabschnitt _a0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = LT
    compare (AStreckenabschnitt _a0) (ABahngeschwindigkeitLegoPwm _a1) = LT
    compare (AStreckenabschnitt _a0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = LT
    compare (AStreckenabschnitt a0) (AStreckenabschnitt a1) = compare a0 a1
    compare (AStreckenabschnitt _a0) (AKupplung _a1) = GT
    compare (AStreckenabschnitt _a0) (AktionAusführen _a1) = GT
    compare (AKupplung _a0) (Warten _w1) = LT
    compare (AKupplung _a0) (WartenAuf _k1) = LT
    compare (AKupplung _a0) (AWegstreckeMärklin _a1) = LT
    compare (AKupplung _a0) (AWegstreckeLego _a1) = LT
    compare (AKupplung _a0) (AWeiche _a1) = LT
    compare (AKupplung _a0) (ABahngeschwindigkeitMärklinPwm _a1) = LT
    compare (AKupplung _a0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = LT
    compare (AKupplung _a0) (ABahngeschwindigkeitLegoPwm _a1) = LT
    compare (AKupplung _a0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = LT
    compare (AKupplung _a0) (AStreckenabschnitt _a1) = LT
    compare (AKupplung a0) (AKupplung a1) = compare a0 a1
    compare (AKupplung _a0) (AktionAusführen _p1) = GT
    compare (AktionAusführen _p0) (Warten _w1) = LT
    compare (AktionAusführen _p0) (WartenAuf _k1) = LT
    compare (AktionAusführen _p0) (AWegstreckeMärklin _a1) = LT
    compare (AktionAusführen _p0) (AWegstreckeLego _a1) = LT
    compare (AktionAusführen _p0) (AWeiche _a1) = LT
    compare (AktionAusführen _p0) (ABahngeschwindigkeitMärklinPwm _a1) = LT
    compare (AktionAusführen _p0) (ABahngeschwindigkeitMärklinKonstanteSpannung _a1) = LT
    compare (AktionAusführen _p0) (ABahngeschwindigkeitLegoPwm _a1) = LT
    compare (AktionAusführen _p0) (ABahngeschwindigkeitLegoKonstanteSpannung _a1) = LT
    compare (AktionAusführen _p0) (AStreckenabschnitt _a1) = LT
    compare (AktionAusführen _p0) (AKupplung _a1) = LT
    compare (AktionAusführen Plan {plName = p0}) (AktionAusführen Plan {plName = p1}) =
        compare p0 p1

instance Anzeige Aktion where
    anzeige :: Aktion -> Sprache -> Text
    anzeige (Warten zeit) = Language.warten <:> zeit
    anzeige (WartenAuf kontakt) = Language.warten <:> kontakt
    anzeige (AWegstreckeMärklin aktion) = Language.wegstrecke <~> aktion
    anzeige (AWegstreckeLego aktion) = Language.wegstrecke <~> aktion
    anzeige (AWeiche aktion) = Language.weiche <~> aktion
    anzeige (ABahngeschwindigkeitMärklinPwm aktion) = Language.bahngeschwindigkeit <~> aktion
    anzeige (ABahngeschwindigkeitMärklinKonstanteSpannung aktion) =
        Language.bahngeschwindigkeit <~> aktion
    anzeige (ABahngeschwindigkeitLegoPwm aktion) = Language.bahngeschwindigkeit <~> aktion
    anzeige (ABahngeschwindigkeitLegoKonstanteSpannung aktion) =
        Language.bahngeschwindigkeit <~> aktion
    anzeige (AStreckenabschnitt aktion) = Language.streckenabschnitt <~> aktion
    anzeige (AKupplung aktion) = Language.kupplung <~> aktion
    anzeige (AktionAusführen Plan {plName}) = Language.ausführen <:> plName

instance StreckenObjekt Aktion where
    anschlüsse :: Aktion -> Set Anschluss
    anschlüsse (Warten _zeit) = Set.empty
    anschlüsse (WartenAuf kontakt) = anschlüsse kontakt
    anschlüsse (AWegstreckeMärklin aktion) = anschlüsse aktion
    anschlüsse (AWegstreckeLego aktion) = anschlüsse aktion
    anschlüsse (AWeiche aktion) = anschlüsse aktion
    anschlüsse (ABahngeschwindigkeitMärklinPwm aktion) = anschlüsse aktion
    anschlüsse (ABahngeschwindigkeitMärklinKonstanteSpannung aktion) = anschlüsse aktion
    anschlüsse (ABahngeschwindigkeitLegoPwm aktion) = anschlüsse aktion
    anschlüsse (ABahngeschwindigkeitLegoKonstanteSpannung aktion) = anschlüsse aktion
    anschlüsse (AStreckenabschnitt aktion) = anschlüsse aktion
    anschlüsse (AKupplung aktion) = anschlüsse aktion
    anschlüsse (AktionAusführen plan) = anschlüsse plan

    erhalteName :: Aktion -> Text
    erhalteName = showText

instance AktionKlasse Aktion where
    ausführenAktion :: (AusführendReader r m, MonadIO m) => Aktion -> m ()
    ausführenAktion (Warten time) = warte time
    ausführenAktion (WartenAuf kontakt) = warteAufSignal kontakt
    ausführenAktion (AWegstreckeMärklin aktion) = ausführenAktion aktion
    ausführenAktion (AWegstreckeLego aktion) = ausführenAktion aktion
    ausführenAktion (AWeiche aktion) = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitMärklinPwm aktion) = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitMärklinKonstanteSpannung aktion) =
        ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitLegoPwm aktion) = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitLegoKonstanteSpannung aktion) = ausführenAktion aktion
    ausführenAktion (AStreckenabschnitt aktion) = ausführenAktion aktion
    ausführenAktion (AKupplung aktion) = ausführenAktion aktion
    ausführenAktion (AktionAusführen plan) = ausführenPlan plan (const $ pure ()) (pure ())

-- | Bekannte 'Aktion'en einer 'Wegstrecke'
data AktionWegstrecke ws (z :: Zugtyp)
    = Einstellen (ws z)
    | AWSBahngeschwindigkeit (GeschwindigkeitEither (AktionBahngeschwindigkeit (GeschwindigkeitPhantom ws)) z)
    | AWSStreckenabschnitt (AktionStreckenabschnitt (ws z))
    | AWSKupplung (AktionKupplung (ws z))

deriving instance ( Eq (ws z)
                  , Eq (GeschwindigkeitPhantom ws 'Pwm z)
                  , Eq (GeschwindigkeitPhantom ws 'KonstanteSpannung z)
                  ) => Eq (AktionWegstrecke ws z)

deriving instance ( Ord (ws z)
                  , Ord (GeschwindigkeitPhantom ws 'Pwm z)
                  , Ord (GeschwindigkeitPhantom ws 'KonstanteSpannung z)
                  ) => Ord (AktionWegstrecke ws z)

deriving instance ( Show (ws z)
                  , Show (GeschwindigkeitPhantom ws 'Pwm z)
                  , Show (GeschwindigkeitPhantom ws 'KonstanteSpannung z)
                  ) => Show (AktionWegstrecke ws z)

instance (StreckenObjekt (ws z)) => Anzeige (AktionWegstrecke ws z) where
    anzeige :: AktionWegstrecke ws z -> Sprache -> Text
    anzeige (Einstellen wegstrecke) = erhalteName wegstrecke <°> Language.einstellen
    anzeige (AWSBahngeschwindigkeit aktion) = anzeige aktion
    anzeige (AWSStreckenabschnitt aktion) = anzeige aktion
    anzeige (AWSKupplung aktion) = anzeige aktion

instance ( BahngeschwindigkeitKlasse (GeschwindigkeitPhantom ws)
         , WegstreckeKlasse (ws z)
         , Show (ws z)
         ) => StreckenObjekt (AktionWegstrecke ws z) where
    anschlüsse :: AktionWegstrecke ws z -> Set Anschluss
    anschlüsse (Einstellen ws) = anschlüsse ws
    anschlüsse (AWSBahngeschwindigkeit aktion) = anschlüsse aktion
    anschlüsse (AWSStreckenabschnitt aktion) = anschlüsse aktion
    anschlüsse (AWSKupplung aktion) = anschlüsse aktion

    erhalteName :: AktionWegstrecke ws z -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse (GeschwindigkeitPhantom ws), WegstreckeKlasse (ws z))
    => AktionKlasse (AktionWegstrecke ws z) where
    ausführenAktion :: (AusführendReader r m, MonadIO m) => AktionWegstrecke ws z -> m ()
    ausführenAktion (Einstellen ws) = einstellen ws
    ausführenAktion (AWSBahngeschwindigkeit (GeschwindigkeitPwm aktion)) = ausführenAktion aktion
    ausführenAktion (AWSBahngeschwindigkeit (GeschwindigkeitKonstanteSpannung aktion)) =
        ausführenAktion aktion
    ausführenAktion (AWSStreckenabschnitt aktion) = ausführenAktion aktion
    ausführenAktion (AWSKupplung aktion) = ausführenAktion aktion

-- | Bekannte 'Aktion'en einer 'Weiche'
data AktionWeiche we = Stellen we Richtung
    deriving (Eq, Ord, Show)

instance (StreckenObjekt we) => Anzeige (AktionWeiche we) where
    anzeige :: AktionWeiche we -> Sprache -> Text
    anzeige (Stellen we richtung) = erhalteName we <°> Language.stellen <=> showText richtung

instance (WeicheKlasse we, Show we) => StreckenObjekt (AktionWeiche we) where
    anschlüsse :: AktionWeiche we -> Set Anschluss
    anschlüsse (Stellen we _richtung) = anschlüsse we

    erhalteName :: AktionWeiche we -> Text
    erhalteName = showText

instance (WeicheKlasse w) => AktionKlasse (AktionWeiche w) where
    ausführenAktion :: (I2CReader r m, PwmReader r m, MonadIO m) => AktionWeiche w -> m ()
    ausführenAktion (Stellen we richtung) = stellen we richtung

-- | Aktionen einer Bahngeschwindigkeit
data AktionBahngeschwindigkeit bg (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
    Geschwindigkeit :: bg 'Pwm z -> Word8 -> AktionBahngeschwindigkeit bg 'Pwm z
    Fahrstrom
        :: bg 'KonstanteSpannung z -> Word8 -> AktionBahngeschwindigkeit bg 'KonstanteSpannung z
    Umdrehen :: bg g 'Märklin -> AktionBahngeschwindigkeit bg g 'Märklin
    FahrtrichtungEinstellen :: bg g 'Lego -> Fahrtrichtung -> AktionBahngeschwindigkeit bg g 'Lego

deriving instance (Eq (bg g z)) => Eq (AktionBahngeschwindigkeit bg g z)

instance (Ord (bg g z)) => Ord (AktionBahngeschwindigkeit bg g z) where
    compare :: AktionBahngeschwindigkeit bg g z -> AktionBahngeschwindigkeit bg g z -> Ordering
    compare (Geschwindigkeit bg0 wert0) (Geschwindigkeit bg1 wert1) = case compare bg0 bg1 of
        EQ -> compare wert0 wert1
        ordering -> ordering
    compare (Geschwindigkeit _bg0 _wert0) (Umdrehen _bg1) = GT
    compare (Geschwindigkeit _bg0 _wert0) (FahrtrichtungEinstellen _bg1 _fahrtrichtung1) = GT
    compare (Fahrstrom bg0 wert0) (Fahrstrom bg1 wert1) = case compare bg0 bg1 of
        EQ -> compare wert0 wert1
        ordering -> ordering
    compare (Fahrstrom _bg0 _wert0) (Umdrehen _bg1) = GT
    compare (Fahrstrom _bg0 _wert0) (FahrtrichtungEinstellen _bg1 _fahrtrichtung) = GT
    compare (Umdrehen bg0) (Umdrehen bg1) = compare bg0 bg1
    compare (Umdrehen _bg0) (Geschwindigkeit _bg1 _wert1) = LT
    compare (Umdrehen _bg0) (Fahrstrom _bg1 _wert1) = LT
    compare
        (FahrtrichtungEinstellen bg0 fahrtrichtung0)
        (FahrtrichtungEinstellen bg1 fahrtrichtung1) = case compare bg0 bg1 of
        EQ -> compare fahrtrichtung0 fahrtrichtung1
        ordering -> ordering
    compare (FahrtrichtungEinstellen _bg0 _fahrtrichtung0) (Geschwindigkeit _bg1 _wert1) = LT
    compare (FahrtrichtungEinstellen _bg0 _fahrtrichtung0) (Fahrstrom _bg1 _wert1) = LT

deriving instance (Show (bg g z)) => Show (AktionBahngeschwindigkeit bg g z)

instance (StreckenObjekt (bg g z)) => Anzeige (AktionBahngeschwindigkeit bg g z) where
    anzeige :: AktionBahngeschwindigkeit bg g z -> Sprache -> Text
    anzeige (Geschwindigkeit bg wert) =
        erhalteName bg <°> Language.geschwindigkeit <=> showText wert
    anzeige (Fahrstrom bg strom) = erhalteName bg <°> Language.fahrstrom <=> strom
    anzeige (Umdrehen bg) = erhalteName bg <°> Language.umdrehen
    anzeige (FahrtrichtungEinstellen bg fahrtrichtung) =
        erhalteName bg <°> Language.umdrehen <=> showText fahrtrichtung

instance ( BahngeschwindigkeitKlasse bg
         , Show (bg g z)
         , StreckenObjekt (bg g 'Märklin)
         , StreckenObjekt (bg g 'Lego)
         , StreckenObjekt (bg g z)
         ) => StreckenObjekt (AktionBahngeschwindigkeit bg g z) where
    anschlüsse :: AktionBahngeschwindigkeit bg g z -> Set Anschluss
    anschlüsse (Geschwindigkeit bg _wert) = anschlüsse bg
    anschlüsse (Fahrstrom bg _strom) = anschlüsse bg
    anschlüsse (Umdrehen bg) = anschlüsse bg
    anschlüsse (FahrtrichtungEinstellen bg _fahrtrichtung) = anschlüsse bg

    erhalteName :: AktionBahngeschwindigkeit bg g z -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse bg) => AktionKlasse (AktionBahngeschwindigkeit bg g z) where
    ausführenAktion
        :: (I2CReader r m, PwmReader r m, MonadIO m) => AktionBahngeschwindigkeit bg g z -> m ()
    ausführenAktion (Geschwindigkeit bg wert) = geschwindigkeit bg wert
    ausführenAktion (Fahrstrom bg strom) = fahrstrom bg strom
    ausführenAktion (Umdrehen bg) = umdrehen bg
    ausführenAktion (FahrtrichtungEinstellen bg fahrtrichtung) =
        fahrtrichtungEinstellen bg fahrtrichtung

-- | Aktionen eines Streckenabschnitts
data AktionStreckenabschnitt st = Strom st Strom
    deriving (Eq, Ord, Show)

instance (StreckenObjekt st) => Anzeige (AktionStreckenabschnitt st) where
    anzeige :: AktionStreckenabschnitt st -> Sprache -> Text
    anzeige (Strom st Fließend) = erhalteName st <°> Language.strom <=> Language.fließend
    anzeige (Strom st Gesperrt) = erhalteName st <°> Language.strom <=> Language.gesperrt

instance (StreckenabschnittKlasse st, Show st) => StreckenObjekt (AktionStreckenabschnitt st) where
    anschlüsse :: AktionStreckenabschnitt st -> Set Anschluss
    anschlüsse (Strom st _an) = anschlüsse st

    erhalteName :: AktionStreckenabschnitt st -> Text
    erhalteName = showText

instance (StreckenabschnittKlasse st) => AktionKlasse (AktionStreckenabschnitt st) where
    ausführenAktion :: (I2CReader r m, MonadIO m) => AktionStreckenabschnitt st -> m ()
    ausführenAktion (Strom st an) = strom st an

-- | Aktionen einer Kupplung
newtype AktionKupplung ku = Kuppeln ku
    deriving (Eq, Ord, Show)

instance (StreckenObjekt ku) => Anzeige (AktionKupplung ku) where
    anzeige :: AktionKupplung ku -> Sprache -> Text
    anzeige (Kuppeln ku) = erhalteName ku <°> Language.kuppeln

instance (KupplungKlasse ku, Show ku) => StreckenObjekt (AktionKupplung ku) where
    anschlüsse :: AktionKupplung ku -> Set Anschluss
    anschlüsse (Kuppeln ku) = anschlüsse ku

    erhalteName :: AktionKupplung ku -> Text
    erhalteName = showText

instance (KupplungKlasse ku) => AktionKlasse (AktionKupplung ku) where
    ausführenAktion :: (I2CReader r m, MonadIO m) => AktionKupplung ku -> m ()
    ausführenAktion (Kuppeln ku) = kuppeln ku