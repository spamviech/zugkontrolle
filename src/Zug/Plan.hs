{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , Ausführend(..)
  , Plan(..)
  , AktionKlasse(..)
  , Aktion(..)
    -- * Spezialisierte Aktionen
  , AktionBahngeschwindigkeit(..)
  , AktionStreckenabschnitt(..)
  , AktionWeiche(..)
  , AktionKupplung(..)
  , AktionKontakt(..)
  , AktionWegstrecke(..)
  ) where

import Control.Concurrent.STM (atomically, TVar, readTVarIO, modifyTVar)
import Control.Monad (void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Zug.Anbindung
       (AnschlussEither(), StreckenObjekt(..), PwmReader(..), I2CReader(..), InterruptReader()
      , Bahngeschwindigkeit(), BahngeschwindigkeitKlasse(..), Streckenabschnitt()
      , StreckenabschnittKlasse(..), Weiche(), WeicheKlasse(..), Kupplung(), KupplungKlasse(..)
      , Wegstrecke(), WegstreckeKlasse(..), warte, Wartezeit(..), Kontakt(..), KontaktKlasse(..))
import Zug.Derive.Ord (deriveOrd)
import Zug.Enums (Zugtyp(..), ZugtypEither(), GeschwindigkeitVariante(..), GeschwindigkeitEither(..)
                , GeschwindigkeitPhantom(..), Richtung(), Fahrtrichtung(), Strom(..))
import qualified Zug.Language as Language
import Zug.Language (Anzeige(..), Sprache(), showText, (<~>), (<^>), (<=>), (<:>), (<°>))
import {-# SOURCE #-} Zug.Objekt
       (Objekt, ObjektKlasse(..), ObjektAllgemein(OPlan), ObjektElement(..))
import Zug.Plan.TemplateHaskell (aktionBahngeschwindigkeitCxtType)

-- | 'Aktion'en einer 'Bahngeschwindigkeit'.
data AktionBahngeschwindigkeit bg (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
    Geschwindigkeit :: bg 'Pwm z -> Word8 -> AktionBahngeschwindigkeit bg 'Pwm z
    Fahrstrom
        :: bg 'KonstanteSpannung z -> Word8 -> AktionBahngeschwindigkeit bg 'KonstanteSpannung z
    Umdrehen :: bg g 'Märklin -> AktionBahngeschwindigkeit bg g 'Märklin
    FahrtrichtungEinstellen :: bg g 'Lego -> Fahrtrichtung -> AktionBahngeschwindigkeit bg g 'Lego

deriving instance ( Eq (bg 'Pwm z)
                  , Eq (bg 'KonstanteSpannung z)
                  , Eq (bg g 'Märklin)
                  , Eq (bg g 'Lego)
                  ) => Eq (AktionBahngeschwindigkeit bg g z)

deriving instance (Show (bg g z)) => Show (AktionBahngeschwindigkeit bg g z)

instance (StreckenObjekt (bg g z)) => Anzeige (AktionBahngeschwindigkeit bg g z) where
    anzeige :: AktionBahngeschwindigkeit bg g z -> Sprache -> Text
    anzeige (Geschwindigkeit bg wert) =
        erhalteName bg <°> Language.geschwindigkeit <=> showText wert
    anzeige (Fahrstrom bg strom) = erhalteName bg <°> Language.fahrstrom <=> strom
    anzeige (Umdrehen bg) = erhalteName bg <°> Language.umdrehen
    anzeige (FahrtrichtungEinstellen bg fahrtrichtung) =
        erhalteName bg <°> Language.umdrehen <=> showText fahrtrichtung

instance ( Show (bg g z)
         , StreckenObjekt (bg g 'Märklin)
         , StreckenObjekt (bg g 'Lego)
         , StreckenObjekt (bg g z)
         ) => StreckenObjekt (AktionBahngeschwindigkeit bg g z) where
    anschlüsse :: AktionBahngeschwindigkeit bg g z -> Set AnschlussEither
    anschlüsse (Geschwindigkeit bg _wert) = anschlüsse bg
    anschlüsse (Fahrstrom bg _strom) = anschlüsse bg
    anschlüsse (Umdrehen bg) = anschlüsse bg
    anschlüsse (FahrtrichtungEinstellen bg _fahrtrichtung) = anschlüsse bg

    erhalteName :: AktionBahngeschwindigkeit bg g z -> Text
    erhalteName = showText

-- | 'Aktion'en eines 'Streckenabschnitt's.
data AktionStreckenabschnitt st = Strom st Strom
    deriving (Eq, Ord, Show)

instance (StreckenObjekt st) => Anzeige (AktionStreckenabschnitt st) where
    anzeige :: AktionStreckenabschnitt st -> Sprache -> Text
    anzeige (Strom st Fließend) = erhalteName st <°> Language.strom <=> Language.fließend
    anzeige (Strom st Gesperrt) = erhalteName st <°> Language.strom <=> Language.gesperrt

instance (StreckenObjekt st, Show st) => StreckenObjekt (AktionStreckenabschnitt st) where
    anschlüsse :: AktionStreckenabschnitt st -> Set AnschlussEither
    anschlüsse (Strom st _an) = anschlüsse st

    erhalteName :: AktionStreckenabschnitt st -> Text
    erhalteName = showText

-- | 'Aktion'en einer 'Weiche'.
data AktionWeiche we = Stellen we Richtung
    deriving (Eq, Ord, Show)

instance (StreckenObjekt we) => Anzeige (AktionWeiche we) where
    anzeige :: AktionWeiche we -> Sprache -> Text
    anzeige (Stellen we richtung) = erhalteName we <°> Language.stellen <=> showText richtung

instance (StreckenObjekt we, Show we) => StreckenObjekt (AktionWeiche we) where
    anschlüsse :: AktionWeiche we -> Set AnschlussEither
    anschlüsse (Stellen we _richtung) = anschlüsse we

    erhalteName :: AktionWeiche we -> Text
    erhalteName = showText

-- | Aktionen einer 'Kupplung'.
newtype AktionKupplung ku = Kuppeln ku
    deriving (Eq, Ord, Show)

instance (StreckenObjekt ku) => Anzeige (AktionKupplung ku) where
    anzeige :: AktionKupplung ku -> Sprache -> Text
    anzeige (Kuppeln ku) = erhalteName ku <°> Language.kuppeln

instance (StreckenObjekt ku, Show ku) => StreckenObjekt (AktionKupplung ku) where
    anschlüsse :: AktionKupplung ku -> Set AnschlussEither
    anschlüsse (Kuppeln ku) = anschlüsse ku

    erhalteName :: AktionKupplung ku -> Text
    erhalteName = showText

-- | Aktionen eines 'Kontakt's.
newtype AktionKontakt ko = WartenAuf ko
    deriving (Eq, Ord, Show)

instance (StreckenObjekt ko) => Anzeige (AktionKontakt ko) where
    anzeige :: AktionKontakt ko -> Sprache -> Text
    anzeige (WartenAuf ko) = Language.warten <:> erhalteName ko

instance (StreckenObjekt ko, Show ko) => StreckenObjekt (AktionKontakt ko) where
    anschlüsse :: AktionKontakt ko -> Set AnschlussEither
    anschlüsse (WartenAuf ko) = anschlüsse ko

    erhalteName :: AktionKontakt ko -> Text
    erhalteName = showText

-- | Bekannte 'Aktion'en einer 'Wegstrecke'.
data AktionWegstrecke ws (z :: Zugtyp)
    = Einstellen (ws z)
    | AWSBahngeschwindigkeit (GeschwindigkeitEither (AktionBahngeschwindigkeit (GeschwindigkeitPhantom ws)) z)
    | AWSStreckenabschnitt (AktionStreckenabschnitt (ws z))
    | AWSKupplung (AktionKupplung (ws z))
    | AWSKontakt (AktionKontakt (ws z))

deriving instance ( Eq (ws z)
                  , Ord (ws z)
                  , Ord (ws 'Märklin)
                  , Ord (ws 'Lego)
                  , Eq (GeschwindigkeitPhantom ws 'Pwm z)
                  , Eq (GeschwindigkeitPhantom ws 'KonstanteSpannung z)
                  ) => Eq (AktionWegstrecke ws z)

deriving instance ( Eq (ws z)
                  , Ord (ws z)
                  , Ord (ws 'Märklin)
                  , Ord (ws 'Lego)
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
    anzeige (AWSKontakt aktion) = anzeige aktion

instance ( StreckenObjekt (ws 'Märklin)
         , StreckenObjekt (ws 'Lego)
         , StreckenObjekt (ws z)
         , Show (ws z)
         ) => StreckenObjekt (AktionWegstrecke ws z) where
    anschlüsse :: AktionWegstrecke ws z -> Set AnschlussEither
    anschlüsse (Einstellen ws) = anschlüsse ws
    anschlüsse (AWSBahngeschwindigkeit aktion) = anschlüsse aktion
    anschlüsse (AWSStreckenabschnitt aktion) = anschlüsse aktion
    anschlüsse (AWSKupplung aktion) = anschlüsse aktion
    anschlüsse (AWSKontakt aktion) = anschlüsse aktion

    erhalteName :: AktionWegstrecke ws z -> Text
    erhalteName = showText

-- | Eine Aktion eines 'StreckenObjekt's oder eine Wartezeit.
-- Die Update-Funktion wird nicht aufgerufen.
data AktionAllgemein bg st we ku ko ws
    = Warten Wartezeit
    | ABahngeschwindigkeitMärklinPwm (AktionBahngeschwindigkeit bg 'Pwm 'Märklin)
    | ABahngeschwindigkeitMärklinKonstanteSpannung (AktionBahngeschwindigkeit bg 'KonstanteSpannung 'Märklin)
    | ABahngeschwindigkeitLegoPwm (AktionBahngeschwindigkeit bg 'Pwm 'Lego)
    | ABahngeschwindigkeitLegoKonstanteSpannung (AktionBahngeschwindigkeit bg 'KonstanteSpannung 'Lego)
    | AStreckenabschnitt (AktionStreckenabschnitt st)
    | AWeiche (AktionWeiche (ZugtypEither we))
    | AKupplung (AktionKupplung ku)
    | AKontakt (AktionKontakt ko)
    | AWegstreckeMärklin (AktionWegstrecke ws 'Märklin)
    | AWegstreckeLego (AktionWegstrecke ws 'Lego)
    | AktionAusführen (PlanAllgemein bg st we ku ko ws)

deriving instance ( Show (bg 'Pwm 'Märklin)
                  , Show (bg 'KonstanteSpannung 'Märklin)
                  , Show (bg 'Pwm 'Lego)
                  , Show (bg 'KonstanteSpannung 'Lego)
                  , Show st
                  , Show (we 'Märklin)
                  , Show (we 'Lego)
                  , Show ku
                  , Show ko
                  , Show (ws 'Lego)
                  , Show (ws 'Märklin)
                  ) => Show (AktionAllgemein bg st we ku ko ws)

deriving instance ( Eq (bg 'Pwm 'Märklin)
                  , Eq (bg 'KonstanteSpannung 'Märklin)
                  , Eq (bg 'Pwm 'Lego)
                  , Eq (bg 'KonstanteSpannung 'Lego)
                  , Eq st
                  , Eq (we 'Märklin)
                  , Eq (we 'Lego)
                  , Eq ku
                  , Eq ko
                  , Ord (ws 'Märklin)
                  , Ord (ws 'Lego)
                  ) => Eq (AktionAllgemein bg st we ku ko ws)

-- | 'AktionAllgemein' spezialisiert auf 'Zug.Objekt.Objekt'.
type Aktion =
    AktionAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Kontakt Wegstrecke

instance ( StreckenObjekt (bg 'Pwm 'Märklin)
         , StreckenObjekt (bg 'KonstanteSpannung 'Märklin)
         , StreckenObjekt (bg 'Pwm 'Lego)
         , StreckenObjekt (bg 'KonstanteSpannung 'Lego)
         , StreckenObjekt st
         , StreckenObjekt (we 'Märklin)
         , StreckenObjekt (we 'Lego)
         , StreckenObjekt ku
         , StreckenObjekt ko
         , StreckenObjekt (ws 'Märklin)
         , StreckenObjekt (ws 'Lego)
         ) => Anzeige (AktionAllgemein bg st we ku ko ws) where
    anzeige :: AktionAllgemein bg st we ku ko ws -> Sprache -> Text
    anzeige (Warten zeit) = Language.warten <:> zeit
    anzeige (ABahngeschwindigkeitMärklinPwm aktion) = Language.bahngeschwindigkeit <~> aktion
    anzeige (ABahngeschwindigkeitMärklinKonstanteSpannung aktion) =
        Language.bahngeschwindigkeit <~> aktion
    anzeige (ABahngeschwindigkeitLegoPwm aktion) = Language.bahngeschwindigkeit <~> aktion
    anzeige (ABahngeschwindigkeitLegoKonstanteSpannung aktion) =
        Language.bahngeschwindigkeit <~> aktion
    anzeige (AWeiche aktion) = Language.weiche <~> aktion
    anzeige (AStreckenabschnitt aktion) = Language.streckenabschnitt <~> aktion
    anzeige (AKupplung aktion) = Language.kupplung <~> aktion
    anzeige (AKontakt aktion) = Language.kontakt <~> aktion
    anzeige (AWegstreckeMärklin aktion) = Language.wegstrecke <~> aktion
    anzeige (AWegstreckeLego aktion) = Language.wegstrecke <~> aktion
    anzeige (AktionAusführen Plan {plName}) = Language.ausführen <:> plName

instance ( Show (bg 'Pwm 'Märklin)
         , Show (bg 'KonstanteSpannung 'Märklin)
         , Show (bg 'Pwm 'Lego)
         , Show (bg 'KonstanteSpannung 'Lego)
         , Show st
         , Show (we 'Märklin)
         , Show (we 'Lego)
         , Show ku
         , Show ko
         , Show (ws 'Lego)
         , Show (ws 'Märklin)
         , StreckenObjekt (bg 'Pwm 'Märklin)
         , StreckenObjekt (bg 'KonstanteSpannung 'Märklin)
         , StreckenObjekt (bg 'Pwm 'Lego)
         , StreckenObjekt (bg 'KonstanteSpannung 'Lego)
         , StreckenObjekt st
         , StreckenObjekt (we 'Märklin)
         , StreckenObjekt (we 'Lego)
         , StreckenObjekt ku
         , StreckenObjekt ko
         , StreckenObjekt (ws 'Märklin)
         , StreckenObjekt (ws 'Lego)
         ) => StreckenObjekt (AktionAllgemein bg st we ku ko ws) where
    anschlüsse :: AktionAllgemein bg st we ku ko ws -> Set AnschlussEither
    anschlüsse (Warten _zeit) = Set.empty
    anschlüsse (ABahngeschwindigkeitMärklinPwm aktion) = anschlüsse aktion
    anschlüsse (ABahngeschwindigkeitMärklinKonstanteSpannung aktion) = anschlüsse aktion
    anschlüsse (ABahngeschwindigkeitLegoPwm aktion) = anschlüsse aktion
    anschlüsse (ABahngeschwindigkeitLegoKonstanteSpannung aktion) = anschlüsse aktion
    anschlüsse (AWeiche aktion) = anschlüsse aktion
    anschlüsse (AStreckenabschnitt aktion) = anschlüsse aktion
    anschlüsse (AKupplung aktion) = anschlüsse aktion
    anschlüsse (AKontakt aktion) = anschlüsse aktion
    anschlüsse (AWegstreckeMärklin aktion) = anschlüsse aktion
    anschlüsse (AWegstreckeLego aktion) = anschlüsse aktion
    anschlüsse (AktionAusführen plan) = anschlüsse plan

    erhalteName :: AktionAllgemein bg st we ku ko ws -> Text
    erhalteName = showText

-- | Pläne: Benannte IO-Aktionen mit StreckenObjekten, bzw. Wartezeiten.
-- Die Update-Funktion wird mit Index der aktuellen Aktion vor dessen Ausführung aufgerufen.
data PlanAllgemein bg st we ku ko ws =
    Plan { plName :: Text, plAktionen :: NonEmpty (AktionAllgemein bg st we ku ko ws) }

deriving instance ( Show (bg 'Pwm 'Märklin)
                  , Show (bg 'KonstanteSpannung 'Märklin)
                  , Show (bg 'Pwm 'Lego)
                  , Show (bg 'KonstanteSpannung 'Lego)
                  , Show st
                  , Show (we 'Märklin)
                  , Show (we 'Lego)
                  , Show ku
                  , Show ko
                  , Show (ws 'Lego)
                  , Show (ws 'Märklin)
                  ) => Show (PlanAllgemein bg st we ku ko ws)

instance ( Eq (bg 'Pwm 'Märklin)
         , Eq (bg 'KonstanteSpannung 'Märklin)
         , Eq (bg 'Pwm 'Lego)
         , Eq (bg 'KonstanteSpannung 'Lego)
         , Eq st
         , Eq (we 'Märklin)
         , Eq (we 'Lego)
         , Eq ku
         , Eq ko
         , Ord (ws 'Märklin)
         , Ord (ws 'Lego)
         ) => Eq (PlanAllgemein bg st we ku ko ws) where
    (==) :: PlanAllgemein bg st we ku ko ws -> PlanAllgemein bg st we ku ko ws -> Bool
    (==)
        Plan {plName = plName0, plAktionen = plAktionen0}
        Plan {plName = plName1, plAktionen = plAktionen1} =
        plName0 == plName1 && vergleicheAktionen plAktionen0 plAktionen1
        where
            vergleicheAktionen
                :: ( Eq (bg 'Pwm 'Märklin)
                   , Eq (bg 'KonstanteSpannung 'Märklin)
                   , Eq (bg 'Pwm 'Lego)
                   , Eq (bg 'KonstanteSpannung 'Lego)
                   , Eq st
                   , Eq (we 'Märklin)
                   , Eq (we 'Lego)
                   , Eq ku
                   , Eq ko
                   , Ord (ws 'Märklin)
                   , Ord (ws 'Lego)
                   )
                => NonEmpty (AktionAllgemein bg st we ku ko ws)
                -> NonEmpty (AktionAllgemein bg st we ku ko ws)
                -> Bool
            vergleicheAktionen
                [AktionAusführen Plan {plName = n0}]
                [AktionAusführen Plan {plName = n1}]
                | plName0 == n0, plName1 == n1 = n0 == n1
            vergleicheAktionen (h0 :| t0) (h1 :| t1) = h0 == h1 && case (t0, t1) of
                (hh0:tt0, hh1:tt1) -> vergleicheAktionen (hh0 :| tt0) (hh1 :| tt1)
                ([], []) -> True
                _differentSizes -> False

-- | 'PlanAllgemein', spezialisiert auf 'Zug.Objekt.Objekt'.
type Plan = PlanAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Kontakt Wegstrecke

-- | newtype für ausführende Pläne ('Plan')
newtype Ausführend = Ausführend Plan
    deriving (Eq, Show, StreckenObjekt)

-- | Klasse für Typen mit den aktuell 'Ausführend'en Plänen
class MitAusführend r where
    mengeAusführend :: r -> TVar (Set Ausführend)

-- | Abkürzung für Funktionen, die die aktuelle 'Ausführend'-'Set' benötigen
class (I2CReader r m, PwmReader r m, InterruptReader r m, MitAusführend r)
    => AusführendReader r m | m -> r where
    -- | Erhalte die aktuelle 'Ausführend'-'Menge' aus der Umgebung.
    erhalteMengeAusführend :: m (TVar (Set Ausführend))
    erhalteMengeAusführend = asks mengeAusführend

instance (I2CReader r m, PwmReader r m, InterruptReader r m, MitAusführend r)
    => AusführendReader r m

-- | Mitglieder dieser Klasse sind ausführbar (können in IO-Aktionen übersetzt werden).  
-- Sie können selbst entscheiden, wann sie die mitgegebene Update-Funktion über den
-- Fortschritt informieren.  
-- Nach der kompletten Ausführung soll der End-Aktion ausgeführt werden.  
-- Die Ausführung soll abgebrochen werden, sobald der Plan nicht mehr im 'AusführenReader' enthalten ist.
class PlanKlasse pl where
    ausführenPlan
        :: (AusführendReader r m, MonadIO m) => pl -> (Natural -> IO ()) -> IO () -> m ()

instance ( StreckenObjekt (bg 'Pwm 'Märklin)
         , StreckenObjekt (bg 'KonstanteSpannung 'Märklin)
         , StreckenObjekt (bg 'Pwm 'Lego)
         , StreckenObjekt (bg 'KonstanteSpannung 'Lego)
         , StreckenObjekt st
         , StreckenObjekt (we 'Märklin)
         , StreckenObjekt (we 'Lego)
         , StreckenObjekt ku
         , StreckenObjekt ko
         , StreckenObjekt (ws 'Märklin)
         , StreckenObjekt (ws 'Lego)
         ) => Anzeige (PlanAllgemein bg st we ku ko ws) where
    anzeige :: PlanAllgemein bg st we ku ko ws -> Sprache -> Text
    anzeige Plan {plName, plAktionen} =
        Language.plan <:> Language.name <=> plName <^> Language.aktionen <=> plAktionen

instance ( Show (bg 'Pwm 'Märklin)
         , Show (bg 'KonstanteSpannung 'Märklin)
         , Show (bg 'Pwm 'Lego)
         , Show (bg 'KonstanteSpannung 'Lego)
         , Show st
         , Show (we 'Märklin)
         , Show (we 'Lego)
         , Show ku
         , Show ko
         , Show (ws 'Lego)
         , Show (ws 'Märklin)
         , StreckenObjekt (bg 'Pwm 'Märklin)
         , StreckenObjekt (bg 'KonstanteSpannung 'Märklin)
         , StreckenObjekt (bg 'Pwm 'Lego)
         , StreckenObjekt (bg 'KonstanteSpannung 'Lego)
         , StreckenObjekt st
         , StreckenObjekt (we 'Märklin)
         , StreckenObjekt (we 'Lego)
         , StreckenObjekt ku
         , StreckenObjekt ko
         , StreckenObjekt (ws 'Märklin)
         , StreckenObjekt (ws 'Lego)
         ) => StreckenObjekt (PlanAllgemein bg st we ku ko ws) where
    anschlüsse :: PlanAllgemein bg st we ku ko ws -> Set AnschlussEither
    anschlüsse Plan {plAktionen} = foldMap anschlüsse plAktionen

    erhalteName :: PlanAllgemein bg st we ku ko ws -> Text
    erhalteName Plan {plName} = plName

instance PlanKlasse (PlanAllgemein bg st we ku ko ws) where
    ausführenPlan :: (AusführendReader r m, MonadIO m)
                   => PlanAllgemein bg st we ku ko ws
                   -> (Natural -> IO ())
                   -> IO ()
                   -> m ()
    ausführenPlan plan@Plan {plName, plAktionen} showAktion endAktion =
        void $ forkI2CReader $ void $ do
            tvarAusführend <- erhalteMengeAusführend
            liftIO $ atomically $ modifyTVar tvarAusführend $ Set.insert ausführendPlan
            ausführenAux 0 plAktionen
            liftIO $ do
                showAktion $ fromIntegral $ length plAktionen
                endAktion
        where
            ausführendPlan :: Ausführend
            ausführendPlan = Ausführend $ zuObjektTyp plan

            ausführenAux :: (AusführendReader r m, MonadIO m)
                          => Natural
                          -> NonEmpty (AktionAllgemein bg st we ku ko ws)
                          -> m ()
            ausführenAux _i [AktionAusführen Plan {plName = plName1, plAktionen = plAktionen1}]
                | plName == plName1 = ausführenAux 0 plAktionen1
            ausführenAux i (h :| t) = do
                tvarAusführend <- erhalteMengeAusführend
                ausführend <- liftIO $ readTVarIO tvarAusführend
                when (ausführendPlan `elem` ausführend) $ do
                    liftIO $ showAktion i
                    ausführenAktion h
                    case t of
                        (hh:tt) -> ausführenAux (succ i) $ hh :| tt
                        [] -> liftIO
                            $ atomically
                            $ modifyTVar tvarAusführend
                            $ Set.delete ausführendPlan

instance ObjektElement (PlanAllgemein bg st we ku ko ws) where
    type ObjektTyp (PlanAllgemein bg st we ku ko ws) = Plan

    zuObjektTyp :: PlanAllgemein bg st we ku ko ws -> Plan
    zuObjektTyp = _

    zuObjekt :: PlanAllgemein bg st we ku ko ws -> Objekt
    zuObjekt = OPlan . zuObjektTyp

-- | Mitglieder dieser Klasse sind ausführbar.
class AktionKlasse a where
    ausführenAktion :: (AusführendReader r m, MonadIO m) => a -> m ()

instance (BahngeschwindigkeitKlasse bg) => AktionKlasse (AktionBahngeschwindigkeit bg g z) where
    ausführenAktion
        :: (I2CReader r m, PwmReader r m, MonadIO m) => AktionBahngeschwindigkeit bg g z -> m ()
    ausführenAktion (Geschwindigkeit bg wert) = geschwindigkeit bg wert
    ausführenAktion (Fahrstrom bg strom) = fahrstrom bg strom
    ausführenAktion (Umdrehen bg) = umdrehen bg
    ausführenAktion (FahrtrichtungEinstellen bg fahrtrichtung) =
        fahrtrichtungEinstellen bg fahrtrichtung

instance (StreckenabschnittKlasse st) => AktionKlasse (AktionStreckenabschnitt st) where
    ausführenAktion :: (I2CReader r m, MonadIO m) => AktionStreckenabschnitt st -> m ()
    ausführenAktion (Strom st an) = strom st an

instance (WeicheKlasse w) => AktionKlasse (AktionWeiche w) where
    ausführenAktion :: (I2CReader r m, PwmReader r m, MonadIO m) => AktionWeiche w -> m ()
    ausführenAktion (Stellen we richtung) = stellen we richtung

instance (KupplungKlasse ku) => AktionKlasse (AktionKupplung ku) where
    ausführenAktion :: (I2CReader r m, MonadIO m) => AktionKupplung ku -> m ()
    ausführenAktion (Kuppeln ku) = kuppeln ku

instance (KontaktKlasse ko) => AktionKlasse (AktionKontakt ko) where
    ausführenAktion :: (I2CReader r m, InterruptReader r m, MonadIO m) => AktionKontakt ko -> m ()
    ausführenAktion (WartenAuf ko) = warteAufSignal ko

instance ( BahngeschwindigkeitKlasse (GeschwindigkeitPhantom ws)
         , KontaktKlasse (ws z)
         , WegstreckeKlasse (ws z)
         ) => AktionKlasse (AktionWegstrecke ws z) where
    ausführenAktion
        :: (AusführendReader r m, InterruptReader r m, MonadIO m) => AktionWegstrecke ws z -> m ()
    ausführenAktion (Einstellen ws) = einstellen ws
    ausführenAktion (AWSBahngeschwindigkeit (GeschwindigkeitPwm aktion)) = ausführenAktion aktion
    ausführenAktion (AWSBahngeschwindigkeit (GeschwindigkeitKonstanteSpannung aktion)) =
        ausführenAktion aktion
    ausführenAktion (AWSStreckenabschnitt aktion) = ausführenAktion aktion
    ausführenAktion (AWSKupplung aktion) = ausführenAktion aktion
    ausführenAktion (AWSKontakt aktion) = ausführenAktion aktion

instance ( BahngeschwindigkeitKlasse bg
         , StreckenabschnittKlasse st
         , WeicheKlasse (we 'Märklin)
         , WeicheKlasse (we 'Lego)
         , KupplungKlasse ku
         , KontaktKlasse ko
         , WegstreckeKlasse (ws 'Märklin)
         , WegstreckeKlasse (ws 'Lego)
         , BahngeschwindigkeitKlasse (GeschwindigkeitPhantom ws)
         , KontaktKlasse (ws 'Märklin)
         , KontaktKlasse (ws 'Lego)
         ) => AktionKlasse (AktionAllgemein bg st we ku ko ws) where
    ausführenAktion
        :: (AusführendReader r m, MonadIO m) => AktionAllgemein bg st we ku ko ws -> m ()
    ausführenAktion (Warten time) = warte time
    ausführenAktion (ABahngeschwindigkeitMärklinPwm aktion) = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitMärklinKonstanteSpannung aktion) =
        ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitLegoPwm aktion) = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitLegoKonstanteSpannung aktion) = ausführenAktion aktion
    ausführenAktion (AStreckenabschnitt aktion) = ausführenAktion aktion
    ausführenAktion (AWeiche aktion) = ausführenAktion aktion
    ausführenAktion (AKupplung aktion) = ausführenAktion aktion
    ausführenAktion (AKontakt aktion) = ausführenAktion aktion
    ausführenAktion (AWegstreckeMärklin aktion) = ausführenAktion aktion
    ausführenAktion (AWegstreckeLego aktion) = ausführenAktion aktion
    ausführenAktion (AktionAusführen plan) = ausführenPlan plan (const $ pure ()) (pure ())

-- Template-Haskell: Ord-Deriving
deriveOrd $ Right aktionBahngeschwindigkeitCxtType

deriveOrd $ Left ''AktionAllgemein

deriveOrd $ Left ''PlanAllgemein

deriveOrd $ Left ''Ausführend