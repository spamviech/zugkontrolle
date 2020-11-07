{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Pläne sind nacheinander auszuführende Aktionen, welche mit StreckenObjekten möglich sind.

Jede Art von 'StreckenObjekt' ('Bahngeschwindigkeit', 'Streckenabschnitt', 'Weiche', 'Wegstrecke')
unterstützt unterschiedliche Aktionen. Ein 'PlanAllgemein' ist eine Zusammenfassung mehrerer dieser
Aktionen und Wartezeiten, welche nacheinander ausgeführt werden können.
-}
module Zug.Plan
  ( -- * Allgemeine Datentypen
    PlanKlasse(..)
  , MitAusführend(..)
  , AusführendReader(..)
  , Ausführend(..)
  , PlanAllgemein(..)
  , Plan
  , AktionKlasse(..)
  , AktionAllgemein(..)
  , Aktion
  , AktionA
    -- * Spezialisierte Aktionen
  , AktionBahngeschwindigkeit(..)
  , AktionStreckenabschnitt(..)
  , AktionWeiche(..)
  , AktionKupplung(..)
  , AktionKontakt(..)
  , AktionWegstrecke(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Concurrent.STM (atomically, TVar, readTVarIO, modifyTVar)
import Control.Monad (void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (MonadIO(..))
import Data.Aeson.Types ((.:), (.:?), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Zug.Anbindung
       (AnschlussEither(), StreckenObjekt(..), PwmReader(..), I2CReader(..), InterruptReader()
      , VersionReader, Bahngeschwindigkeit(), BahngeschwindigkeitKlasse(..), PwmZugtyp()
      , Streckenabschnitt(), StreckenabschnittKlasse(..), Weiche(), WeicheKlasse(..), Kupplung()
      , KupplungKlasse(..), Wegstrecke(), WegstreckeKlasse(..), warte, Wartezeit(..), Kontakt(..)
      , KontaktKlasse(..))
import Zug.Derive.Ord (deriveOrd)
import Zug.Enums
       (Zugtyp(..), ZugtypEither(..), GeschwindigkeitVariante(..), GeschwindigkeitEither(..)
      , GeschwindigkeitPhantom(..), Richtung(), Fahrtrichtung(), Strom(..))
import qualified Zug.JSONStrings as JS
import qualified Zug.Language as Language
import Zug.Language (Anzeige(..), Sprache(), showText, (<~>), (<^>), (<=>), (<:>), (<°>))
import {-# SOURCE #-} Zug.Objekt (Objekt, ObjektAllgemein(OPlan)
                                , ObjektElement(ObjektTyp, zuObjektTyp, zuObjekt), ObjektKlasse(..))
import Zug.Plan.TemplateHaskell
       (aktionBahngeschwindigkeitCxtType, aktionAllgemeinCxtType, planAllgemeinCxtType)

-- | Bekannte 'AktionAllgemein' einer 'Bahngeschwindigkeit'.
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
    anzeige (Fahrstrom bg wert) = erhalteName bg <°> Language.fahrstrom <=> wert
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

-- | Bekannte 'AktionAllgemein' eines 'Streckenabschnitt's.
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

-- | Bekannte 'AktionAllgemein' einer 'Weiche'.
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

-- | Bekannte 'AktionAllgemein' einer 'Kupplung'.
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

-- | Bekannte 'AktionAllgemein' eines 'Kontakt's.
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

-- | Bekannte 'AktionAllgemein' einer 'Wegstrecke'.
data AktionWegstrecke ws (z :: Zugtyp)
    = Einstellen (ws z)
    | AWSBahngeschwindigkeit (GeschwindigkeitEither
                                  (AktionBahngeschwindigkeit (GeschwindigkeitPhantom ws))
                                  z)
    | AWSStreckenabschnitt (AktionStreckenabschnitt (ws z))
    | AWSKupplung (AktionKupplung (ws z))
    | AWSKontakt (AktionKontakt (ws z))

deriving instance ( Eq (ws z)
                  , Eq (ws 'Märklin)
                  , Eq (ws 'Lego)
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
    | ABahngeschwindigkeitMärklinKonstanteSpannung (AktionBahngeschwindigkeit
                                                         bg
                                                         'KonstanteSpannung
                                                         'Märklin)
    | ABahngeschwindigkeitLegoPwm (AktionBahngeschwindigkeit bg 'Pwm 'Lego)
    | ABahngeschwindigkeitLegoKonstanteSpannung (AktionBahngeschwindigkeit
                                                     bg
                                                     'KonstanteSpannung
                                                     'Lego)
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
                  , Eq (ws 'Märklin)
                  , Eq (ws 'Lego)
                  ) => Eq (AktionAllgemein bg st we ku ko ws)

-- | 'AktionAllgemein' spezialisiert auf 'Zug.Objekt.Objekt'.
type Aktion =
    AktionAllgemein Bahngeschwindigkeit Streckenabschnitt Weiche Kupplung Kontakt Wegstrecke

-- | 'AktionAllgemein' spezialisiert auf einen 'Zug.Objekt.ObjektKlasse'-Typ.
type AktionA o = AktionAllgemein (BG o) (ST o) (WE o) (KU o) (KO o) (WS o)

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
         , Eq (ws 'Märklin)
         , Eq (ws 'Lego)
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
                   , Eq (ws 'Märklin)
                   , Eq (ws 'Lego)
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

-- | newtype für ausführende Pläne.
newtype Ausführend = Ausführend Plan
    deriving (Eq, Show, StreckenObjekt)

-- | Klasse für Typen mit den aktuell 'Ausführend'en Plänen.
class MitAusführend r where
    mengeAusführend :: r -> TVar (Set Ausführend)

-- | Abkürzung für Funktionen, die die aktuelle 'Ausführend'-'Set' benötigen.
class (I2CReader r m, PwmReader r m, InterruptReader r m, VersionReader r m, MitAusführend r)
    => AusführendReader r m | m -> r where
    -- | Erhalte die aktuelle 'Ausführend'-Menge aus der Umgebung.
    erhalteMengeAusführend :: m (TVar (Set Ausführend))
    erhalteMengeAusführend = asks mengeAusführend

instance (I2CReader r m, PwmReader r m, InterruptReader r m, VersionReader r m, MitAusführend r)
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
         , ObjektElement (bg 'Pwm 'Märklin)
         , ObjektTyp (bg 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
         , ObjektElement (bg 'KonstanteSpannung 'Märklin)
         , ObjektTyp (bg 'KonstanteSpannung 'Märklin)
           ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
         , ObjektElement (bg 'Pwm 'Lego)
         , ObjektTyp (bg 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
         , ObjektElement (bg 'KonstanteSpannung 'Lego)
         , ObjektTyp (bg 'KonstanteSpannung 'Lego) ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
         , ObjektElement st
         , ObjektTyp st ~ Streckenabschnitt
         , ObjektElement (we 'Märklin)
         , ObjektTyp (we 'Märklin) ~ Weiche 'Märklin
         , ObjektElement (we 'Lego)
         , ObjektTyp (we 'Lego) ~ Weiche 'Lego
         , ObjektElement ku
         , ObjektTyp ku ~ Kupplung
         , ObjektElement ko
         , ObjektTyp ko ~ Kontakt
         , ObjektElement (ws 'Märklin)
         , ObjektTyp (ws 'Märklin) ~ Wegstrecke 'Märklin
         , ObjektElement (ws 'Lego)
         , ObjektTyp (ws 'Lego) ~ Wegstrecke 'Lego
         ) => PlanKlasse (PlanAllgemein bg st we ku ko ws) where
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

            ausführenAux
                :: ( BahngeschwindigkeitKlasse bg
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
                   , AusführendReader r m
                   , VersionReader r m
                   , MonadIO m
                   , ObjektElement (bg 'Pwm 'Märklin)
                   , ObjektTyp (bg 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
                   , ObjektElement (bg 'KonstanteSpannung 'Märklin)
                   , ObjektTyp (bg 'KonstanteSpannung 'Märklin)
                     ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
                   , ObjektElement (bg 'Pwm 'Lego)
                   , ObjektTyp (bg 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
                   , ObjektElement (bg 'KonstanteSpannung 'Lego)
                   , ObjektTyp (bg 'KonstanteSpannung 'Lego)
                     ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
                   , ObjektElement st
                   , ObjektTyp st ~ Streckenabschnitt
                   , ObjektElement (we 'Märklin)
                   , ObjektTyp (we 'Märklin) ~ Weiche 'Märklin
                   , ObjektElement (we 'Lego)
                   , ObjektTyp (we 'Lego) ~ Weiche 'Lego
                   , ObjektElement ku
                   , ObjektTyp ku ~ Kupplung
                   , ObjektElement ko
                   , ObjektTyp ko ~ Kontakt
                   , ObjektElement (ws 'Märklin)
                   , ObjektTyp (ws 'Märklin) ~ Wegstrecke 'Märklin
                   , ObjektElement (ws 'Lego)
                   , ObjektTyp (ws 'Lego) ~ Wegstrecke 'Lego
                   )
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

instance ( ObjektElement (bg 'Pwm 'Märklin)
         , ObjektTyp (bg 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
         , ObjektElement (bg 'KonstanteSpannung 'Märklin)
         , ObjektTyp (bg 'KonstanteSpannung 'Märklin)
           ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
         , ObjektElement (bg 'Pwm 'Lego)
         , ObjektTyp (bg 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
         , ObjektElement (bg 'KonstanteSpannung 'Lego)
         , ObjektTyp (bg 'KonstanteSpannung 'Lego) ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
         , ObjektElement st
         , ObjektTyp st ~ Streckenabschnitt
         , ObjektElement (we 'Märklin)
         , ObjektTyp (we 'Märklin) ~ Weiche 'Märklin
         , ObjektElement (we 'Lego)
         , ObjektTyp (we 'Lego) ~ Weiche 'Lego
         , ObjektElement ku
         , ObjektTyp ku ~ Kupplung
         , ObjektElement ko
         , ObjektTyp ko ~ Kontakt
         , ObjektElement (ws 'Märklin)
         , ObjektTyp (ws 'Märklin) ~ Wegstrecke 'Märklin
         , ObjektElement (ws 'Lego)
         , ObjektTyp (ws 'Lego) ~ Wegstrecke 'Lego
         ) => ObjektElement (PlanAllgemein bg st we ku ko ws) where
    type ObjektTyp (PlanAllgemein bg st we ku ko ws) = Plan

    zuObjektTyp :: PlanAllgemein bg st we ku ko ws -> Plan
    zuObjektTyp
        Plan {plName, plAktionen} = Plan { plName, plAktionen = konvertiereAktion <$> plAktionen }
        where
            konvertiereAktion
                :: ( ObjektElement (bg 'Pwm 'Märklin)
                   , ObjektTyp (bg 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
                   , ObjektElement (bg 'KonstanteSpannung 'Märklin)
                   , ObjektTyp (bg 'KonstanteSpannung 'Märklin)
                     ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
                   , ObjektElement (bg 'Pwm 'Lego)
                   , ObjektTyp (bg 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
                   , ObjektElement (bg 'KonstanteSpannung 'Lego)
                   , ObjektTyp (bg 'KonstanteSpannung 'Lego)
                     ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
                   , ObjektElement st
                   , ObjektTyp st ~ Streckenabschnitt
                   , ObjektElement (we 'Märklin)
                   , ObjektTyp (we 'Märklin) ~ Weiche 'Märklin
                   , ObjektElement (we 'Lego)
                   , ObjektTyp (we 'Lego) ~ Weiche 'Lego
                   , ObjektElement ku
                   , ObjektTyp ku ~ Kupplung
                   , ObjektElement ko
                   , ObjektTyp ko ~ Kontakt
                   , ObjektElement (ws 'Märklin)
                   , ObjektTyp (ws 'Märklin) ~ Wegstrecke 'Märklin
                   , ObjektElement (ws 'Lego)
                   , ObjektTyp (ws 'Lego) ~ Wegstrecke 'Lego
                   )
                => AktionAllgemein bg st we ku ko ws
                -> Aktion
            konvertiereAktion (Warten wartezeit) = Warten wartezeit
            konvertiereAktion (ABahngeschwindigkeitMärklinPwm aktion) =
                ABahngeschwindigkeitMärklinPwm $ konvertiereAktionBahngeschwindigkeit aktion
            konvertiereAktion (ABahngeschwindigkeitMärklinKonstanteSpannung aktion) =
                ABahngeschwindigkeitMärklinKonstanteSpannung
                $ konvertiereAktionBahngeschwindigkeit aktion
            konvertiereAktion (ABahngeschwindigkeitLegoPwm aktion) =
                ABahngeschwindigkeitLegoPwm $ konvertiereAktionBahngeschwindigkeit aktion
            konvertiereAktion (ABahngeschwindigkeitLegoKonstanteSpannung aktion) =
                ABahngeschwindigkeitLegoKonstanteSpannung
                $ konvertiereAktionBahngeschwindigkeit aktion
            konvertiereAktion (AStreckenabschnitt (Strom st fließend)) =
                AStreckenabschnitt $ Strom (zuObjektTyp st) fließend
            konvertiereAktion (AWeiche (Stellen (ZugtypMärklin we) richtung)) =
                AWeiche $ Stellen (ZugtypMärklin $ zuObjektTyp we) richtung
            konvertiereAktion (AWeiche (Stellen (ZugtypLego we) richtung)) =
                AWeiche $ Stellen (ZugtypLego $ zuObjektTyp we) richtung
            konvertiereAktion (AKupplung (Kuppeln ku)) = AKupplung $ Kuppeln $ zuObjektTyp ku
            konvertiereAktion (AKontakt (WartenAuf ko)) = AKontakt $ WartenAuf $ zuObjektTyp ko
            konvertiereAktion (AWegstreckeMärklin aktion) =
                AWegstreckeMärklin $ konvertiereAktionWegstrecke aktion
            konvertiereAktion (AWegstreckeLego aktion) =
                AWegstreckeLego $ konvertiereAktionWegstrecke aktion
            konvertiereAktion (AktionAusführen pl) = AktionAusführen $ zuObjektTyp pl

            konvertiereAktionBahngeschwindigkeit
                :: (ObjektElement (bg g z), ObjektTyp (bg g z) ~ Bahngeschwindigkeit g z)
                => AktionBahngeschwindigkeit bg g z
                -> AktionBahngeschwindigkeit Bahngeschwindigkeit g z
            konvertiereAktionBahngeschwindigkeit (Geschwindigkeit bg wert) =
                Geschwindigkeit (zuObjektTyp bg) wert
            konvertiereAktionBahngeschwindigkeit (Fahrstrom bg wert) =
                Fahrstrom (zuObjektTyp bg) wert
            konvertiereAktionBahngeschwindigkeit (Umdrehen bg) = Umdrehen $ zuObjektTyp bg
            konvertiereAktionBahngeschwindigkeit (FahrtrichtungEinstellen bg fahrtrichtung) =
                FahrtrichtungEinstellen (zuObjektTyp bg) fahrtrichtung

            konvertiereAktionWegstrecke :: (ObjektElement (ws z), ObjektTyp (ws z) ~ Wegstrecke z)
                                        => AktionWegstrecke ws z
                                        -> AktionWegstrecke Wegstrecke z
            konvertiereAktionWegstrecke (Einstellen ws) = Einstellen $ zuObjektTyp ws
            konvertiereAktionWegstrecke
                (AWSBahngeschwindigkeit
                     (GeschwindigkeitPwm (Geschwindigkeit (GeschwindigkeitPhantom ws) wert))) =
                AWSBahngeschwindigkeit
                $ GeschwindigkeitPwm
                $ Geschwindigkeit (GeschwindigkeitPhantom $ zuObjektTyp ws) wert
            konvertiereAktionWegstrecke
                (AWSBahngeschwindigkeit
                     (GeschwindigkeitKonstanteSpannung (Fahrstrom (GeschwindigkeitPhantom ws) wert))) =
                AWSBahngeschwindigkeit
                $ GeschwindigkeitKonstanteSpannung
                $ Fahrstrom (GeschwindigkeitPhantom $ zuObjektTyp ws) wert
            konvertiereAktionWegstrecke
                (AWSBahngeschwindigkeit (GeschwindigkeitPwm (Umdrehen (GeschwindigkeitPhantom ws)))) =
                AWSBahngeschwindigkeit
                $ GeschwindigkeitPwm
                $ Umdrehen
                $ GeschwindigkeitPhantom
                $ zuObjektTyp ws
            konvertiereAktionWegstrecke
                (AWSBahngeschwindigkeit
                     (GeschwindigkeitKonstanteSpannung (Umdrehen (GeschwindigkeitPhantom ws)))) =
                AWSBahngeschwindigkeit
                $ GeschwindigkeitKonstanteSpannung
                $ Umdrehen
                $ GeschwindigkeitPhantom
                $ zuObjektTyp ws
            konvertiereAktionWegstrecke
                (AWSBahngeschwindigkeit
                     (GeschwindigkeitPwm
                          (FahrtrichtungEinstellen (GeschwindigkeitPhantom ws) fahrtrichtung))) =
                AWSBahngeschwindigkeit
                $ GeschwindigkeitPwm
                $ FahrtrichtungEinstellen (GeschwindigkeitPhantom $ zuObjektTyp ws) fahrtrichtung
            konvertiereAktionWegstrecke
                (AWSBahngeschwindigkeit
                     (GeschwindigkeitKonstanteSpannung
                          (FahrtrichtungEinstellen (GeschwindigkeitPhantom ws) fahrtrichtung))) =
                AWSBahngeschwindigkeit
                $ GeschwindigkeitKonstanteSpannung
                $ FahrtrichtungEinstellen (GeschwindigkeitPhantom $ zuObjektTyp ws) fahrtrichtung
            konvertiereAktionWegstrecke (AWSStreckenabschnitt (Strom ws fließend)) =
                AWSStreckenabschnitt $ Strom (zuObjektTyp ws) fließend
            konvertiereAktionWegstrecke (AWSKupplung (Kuppeln ws)) =
                AWSKupplung $ Kuppeln $ zuObjektTyp ws
            konvertiereAktionWegstrecke (AWSKontakt (WartenAuf ws)) =
                AWSKontakt $ WartenAuf $ zuObjektTyp ws

    zuObjekt :: PlanAllgemein bg st we ku ko ws -> Objekt
    zuObjekt = OPlan . zuObjektTyp

-- | Mitglieder dieser Klasse sind ausführbar.
class AktionKlasse a where
    ausführenAktion :: (AusführendReader r m, VersionReader r m, MonadIO m) => a -> m ()

instance (BahngeschwindigkeitKlasse bg, PwmZugtyp z)
    => AktionKlasse (AktionBahngeschwindigkeit bg g z) where
    ausführenAktion :: (I2CReader r m, PwmReader r m, VersionReader r m, MonadIO m)
                     => AktionBahngeschwindigkeit bg g z
                     -> m ()
    ausführenAktion (Geschwindigkeit bg wert) = geschwindigkeit bg wert
    ausführenAktion (Fahrstrom bg wert) = fahrstrom bg wert
    ausführenAktion (Umdrehen bg) = umdrehen bg
    ausführenAktion (FahrtrichtungEinstellen bg fahrtrichtung) =
        fahrtrichtungEinstellen bg fahrtrichtung

instance (StreckenabschnittKlasse st) => AktionKlasse (AktionStreckenabschnitt st) where
    ausführenAktion
        :: (I2CReader r m, VersionReader r m, MonadIO m) => AktionStreckenabschnitt st -> m ()
    ausführenAktion (Strom st an) = strom st an

instance (WeicheKlasse w) => AktionKlasse (AktionWeiche w) where
    ausführenAktion
        :: (I2CReader r m, PwmReader r m, VersionReader r m, MonadIO m) => AktionWeiche w -> m ()
    ausführenAktion (Stellen we richtung) = stellen we richtung

instance (KupplungKlasse ku) => AktionKlasse (AktionKupplung ku) where
    ausführenAktion :: (I2CReader r m, VersionReader r m, MonadIO m) => AktionKupplung ku -> m ()
    ausführenAktion (Kuppeln ku) = kuppeln ku

instance (KontaktKlasse ko) => AktionKlasse (AktionKontakt ko) where
    ausführenAktion :: (I2CReader r m, InterruptReader r m, MonadIO m) => AktionKontakt ko -> m ()
    ausführenAktion (WartenAuf ko) = warteAufSignal ko

instance ( BahngeschwindigkeitKlasse (GeschwindigkeitPhantom ws)
         , KontaktKlasse (ws z)
         , WegstreckeKlasse (ws z)
         , PwmZugtyp z
         ) => AktionKlasse (AktionWegstrecke ws z) where
    ausführenAktion :: (AusführendReader r m, InterruptReader r m, VersionReader r m, MonadIO m)
                     => AktionWegstrecke ws z
                     -> m ()
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
         , ObjektElement (bg 'Pwm 'Märklin)
         , ObjektTyp (bg 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
         , ObjektElement (bg 'KonstanteSpannung 'Märklin)
         , ObjektTyp (bg 'KonstanteSpannung 'Märklin)
           ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
         , ObjektElement (bg 'Pwm 'Lego)
         , ObjektTyp (bg 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
         , ObjektElement (bg 'KonstanteSpannung 'Lego)
         , ObjektTyp (bg 'KonstanteSpannung 'Lego) ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
         , ObjektElement st
         , ObjektTyp st ~ Streckenabschnitt
         , ObjektElement (we 'Märklin)
         , ObjektTyp (we 'Märklin) ~ Weiche 'Märklin
         , ObjektElement (we 'Lego)
         , ObjektTyp (we 'Lego) ~ Weiche 'Lego
         , ObjektElement ku
         , ObjektTyp ku ~ Kupplung
         , ObjektElement ko
         , ObjektTyp ko ~ Kontakt
         , ObjektElement (ws 'Märklin)
         , ObjektTyp (ws 'Märklin) ~ Wegstrecke 'Märklin
         , ObjektElement (ws 'Lego)
         , ObjektTyp (ws 'Lego) ~ Wegstrecke 'Lego
         ) => AktionKlasse (AktionAllgemein bg st we ku ko ws) where
    ausführenAktion :: (AusführendReader r m, VersionReader r m, MonadIO m)
                     => AktionAllgemein bg st we ku ko ws
                     -> m ()
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

deriveOrd $ Right aktionAllgemeinCxtType

deriveOrd $ Right planAllgemeinCxtType

deriveOrd $ Left ''Ausführend

-- JSON-Instanz-Deklaration für Plan
instance Aeson.FromJSON Aktion where
    parseJSON :: Aeson.Value -> Aeson.Parser Aktion
    parseJSON (Aeson.Object v) = (v .: JS.aktion) >>= \s -> if
        | s == JS.warten -> (Warten <$> v .: JS.wert)
            <|> parseMaybeWegstrecke
                (\w -> pure $ AWSKontakt $ WartenAuf w)
                (\w -> pure $ AWSKontakt $ WartenAuf w)
                (AKontakt . WartenAuf <$> v .: JS.kontakt)
        | s == JS.einstellen -> (AWegstreckeMärklin . Einstellen <$> v .: JS.wegstrecke)
            <|> (AWegstreckeLego . Einstellen <$> v .: JS.wegstrecke)
        | s == JS.stellen -> AWeiche <$> (Stellen <$> v .: JS.weiche <*> v .: JS.richtung)
        | s == JS.geschwindigkeit -> parseMaybeWegstrecke
            (\w -> AWSBahngeschwindigkeit
             . GeschwindigkeitPwm
             . Geschwindigkeit (GeschwindigkeitPhantom w)
             <$> v .: JS.wert)
            (\w -> AWSBahngeschwindigkeit
             . GeschwindigkeitPwm
             . Geschwindigkeit (GeschwindigkeitPhantom w)
             <$> v .: JS.wert)
            ((ABahngeschwindigkeitMärklinPwm <$> geschwindigkeitParserMärklin)
             <|> (ABahngeschwindigkeitLegoPwm <$> geschwindigkeitParserLego))
        | s == JS.fahrstrom -> parseMaybeWegstrecke
            (\w -> AWSBahngeschwindigkeit
             . GeschwindigkeitKonstanteSpannung
             . Fahrstrom (GeschwindigkeitPhantom w)
             <$> v .: JS.strom)
            (\w -> AWSBahngeschwindigkeit
             . GeschwindigkeitKonstanteSpannung
             . Fahrstrom (GeschwindigkeitPhantom w)
             <$> v .: JS.strom)
            (fmap ABahngeschwindigkeitMärklinKonstanteSpannung
             $ Fahrstrom <$> v .: JS.bahngeschwindigkeit <*> v .: JS.strom)
        | s == JS.umdrehen -> parseMaybeWegstrecke
            (\w -> pure
             $ AWSBahngeschwindigkeit
             $ GeschwindigkeitPwm
             $ Umdrehen
             $ GeschwindigkeitPhantom w)
            parseFail
            ((ABahngeschwindigkeitMärklinPwm . Umdrehen <$> v .: JS.bahngeschwindigkeit)
             <|> (ABahngeschwindigkeitMärklinKonstanteSpannung . Umdrehen
                  <$> v .: JS.bahngeschwindigkeit))
        | s == JS.fahrtrichtungEinstellen -> parseMaybeWegstrecke
            parseFail
            (\w -> AWSBahngeschwindigkeit
             . GeschwindigkeitPwm
             . FahrtrichtungEinstellen (GeschwindigkeitPhantom w)
             <$> v .: JS.fahrtrichtung)
            ((ABahngeschwindigkeitLegoPwm
              <$> (FahrtrichtungEinstellen <$> v .: JS.bahngeschwindigkeit
                   <*> v .: JS.fahrtrichtung))
             <|> (ABahngeschwindigkeitLegoKonstanteSpannung
                  <$> (FahrtrichtungEinstellen <$> v .: JS.bahngeschwindigkeit
                       <*> v .: JS.fahrtrichtung)))
        | s == JS.strom -> parseMaybeWegstrecke
            (\w -> AWSStreckenabschnitt . Strom w <$> v .: JS.an)
            (\w -> AWSStreckenabschnitt . Strom w <$> v .: JS.an)
            (AStreckenabschnitt <$> (Strom <$> v .: JS.streckenabschnitt <*> v .: JS.an))
        | s == JS.kuppeln -> parseMaybeWegstrecke
            (\w -> pure $ AWSKupplung $ Kuppeln w)
            (\w -> pure $ AWSKupplung $ Kuppeln w)
            (AKupplung . Kuppeln <$> v .: JS.kupplung)
        | s == JS.ausführen -> AktionAusführen <$> v .: JS.plan
        | otherwise -> empty
        where
            parseMaybeWegstrecke
                :: (Wegstrecke 'Märklin -> Aeson.Parser (AktionWegstrecke Wegstrecke 'Märklin))
                -> (Wegstrecke 'Lego -> Aeson.Parser (AktionWegstrecke Wegstrecke 'Lego))
                -> Aeson.Parser Aktion
                -> Aeson.Parser Aktion
            parseMaybeWegstrecke wsParserMärklin wsParserLego altParser = do
                maybeWegstreckeMärklin <- v .:? JS.wegstrecke
                    :: Aeson.Parser (Maybe (Wegstrecke 'Märklin))
                case maybeWegstreckeMärklin of
                    (Just wsMärklin) -> AWegstreckeMärklin <$> wsParserMärklin wsMärklin
                    Nothing -> do
                        maybeWegstreckeLego <- v .:? JS.wegstrecke
                            :: Aeson.Parser (Maybe (Wegstrecke 'Lego))
                        case maybeWegstreckeLego of
                            (Just wsLego) -> AWegstreckeLego <$> wsParserLego wsLego
                            Nothing -> altParser

            parseFail :: Wegstrecke z -> Aeson.Parser (AktionWegstrecke Wegstrecke z)
            parseFail _w = empty

            geschwindigkeitParserMärklin
                :: Aeson.Parser (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Pwm 'Märklin)
            geschwindigkeitParserMärklin =
                Geschwindigkeit <$> v .: JS.bahngeschwindigkeit <*> v .: JS.wert

            geschwindigkeitParserLego
                :: Aeson.Parser (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Pwm 'Lego)
            geschwindigkeitParserLego =
                Geschwindigkeit <$> v .: JS.bahngeschwindigkeit <*> v .: JS.wert
    parseJSON _value = empty

aktionToJSON
    :: ( ObjektElement (bg 'Pwm 'Märklin)
       , ObjektTyp (bg 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
       , ObjektElement (bg 'KonstanteSpannung 'Märklin)
       , ObjektTyp (bg 'KonstanteSpannung 'Märklin)
         ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
       , ObjektElement (bg 'Pwm 'Lego)
       , ObjektTyp (bg 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
       , ObjektElement (bg 'KonstanteSpannung 'Lego)
       , ObjektTyp (bg 'KonstanteSpannung 'Lego) ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
       , ObjektElement st
       , ObjektTyp st ~ Streckenabschnitt
       , ObjektElement (we 'Märklin)
       , ObjektTyp (we 'Märklin) ~ Weiche 'Märklin
       , ObjektElement (we 'Lego)
       , ObjektTyp (we 'Lego) ~ Weiche 'Lego
       , ObjektElement ku
       , ObjektTyp ku ~ Kupplung
       , ObjektElement ko
       , ObjektTyp ko ~ Kontakt
       , ObjektElement (ws 'Märklin)
       , ObjektTyp (ws 'Märklin) ~ Wegstrecke 'Märklin
       , ObjektElement (ws 'Lego)
       , ObjektTyp (ws 'Lego) ~ Wegstrecke 'Lego
       )
    => Text
    -> AktionAllgemein bg st we ku ko ws
    -> Aeson.Value
aktionToJSON _name (Warten wert) = Aeson.object [JS.aktion .= JS.warten, JS.wert .= wert]
aktionToJSON _name (AWegstreckeMärklin (Einstellen w)) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.einstellen]
aktionToJSON _name (AWegstreckeLego (Einstellen w)) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.einstellen]
aktionToJSON
    _name
    (AWegstreckeMärklin
         (AWSBahngeschwindigkeit
              (GeschwindigkeitPwm (Geschwindigkeit (GeschwindigkeitPhantom w) wert)))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.geschwindigkeit, JS.wert .= wert]
aktionToJSON
    _name
    (AWegstreckeMärklin
         (AWSBahngeschwindigkeit
              (GeschwindigkeitKonstanteSpannung (Fahrstrom (GeschwindigkeitPhantom w) wert)))) =
    Aeson.object
        [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.geschwindigkeit, JS.strom .= wert]
aktionToJSON
    _name
    (AWegstreckeLego
         (AWSBahngeschwindigkeit
              (GeschwindigkeitPwm (Geschwindigkeit (GeschwindigkeitPhantom w) wert)))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.geschwindigkeit, JS.wert .= wert]
aktionToJSON
    _name
    (AWegstreckeLego
         (AWSBahngeschwindigkeit
              (GeschwindigkeitKonstanteSpannung (Fahrstrom (GeschwindigkeitPhantom w) wert)))) =
    Aeson.object
        [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.geschwindigkeit, JS.strom .= wert]
aktionToJSON
    _name
    (AWegstreckeMärklin
         (AWSBahngeschwindigkeit (GeschwindigkeitPwm (Umdrehen (GeschwindigkeitPhantom w))))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.umdrehen]
aktionToJSON
    _name
    (AWegstreckeMärklin
         (AWSBahngeschwindigkeit
              (GeschwindigkeitKonstanteSpannung (Umdrehen (GeschwindigkeitPhantom w))))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.umdrehen]
aktionToJSON
    _name
    (AWegstreckeLego
         (AWSBahngeschwindigkeit
              (GeschwindigkeitPwm
                   (FahrtrichtungEinstellen (GeschwindigkeitPhantom w) fahrtrichtung)))) =
    Aeson.object
        [ JS.wegstrecke .= zuObjektTyp w
        , JS.aktion .= JS.umdrehen
        , JS.fahrtrichtung .= fahrtrichtung]
aktionToJSON
    _name
    (AWegstreckeLego
         (AWSBahngeschwindigkeit
              (GeschwindigkeitKonstanteSpannung
                   (FahrtrichtungEinstellen (GeschwindigkeitPhantom w) fahrtrichtung)))) =
    Aeson.object
        [ JS.wegstrecke .= zuObjektTyp w
        , JS.aktion .= JS.umdrehen
        , JS.fahrtrichtung .= fahrtrichtung]
aktionToJSON _name (AWegstreckeMärklin (AWSStreckenabschnitt (Strom w an))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.strom, JS.an .= an]
aktionToJSON _name (AWegstreckeLego (AWSStreckenabschnitt (Strom w an))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.strom, JS.an .= an]
aktionToJSON _name (AWegstreckeMärklin (AWSKupplung (Kuppeln w))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.kuppeln]
aktionToJSON _name (AWegstreckeLego (AWSKupplung (Kuppeln w))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.kuppeln]
aktionToJSON _name (AWegstreckeMärklin (AWSKontakt (WartenAuf w))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.warten]
aktionToJSON _name (AWegstreckeLego (AWSKontakt (WartenAuf w))) =
    Aeson.object [JS.wegstrecke .= zuObjektTyp w, JS.aktion .= JS.warten]
aktionToJSON _name (AWeiche (Stellen (ZugtypMärklin w) richtung)) =
    Aeson.object [JS.weiche .= zuObjektTyp w, JS.aktion .= JS.stellen, JS.richtung .= richtung]
aktionToJSON _name (AWeiche (Stellen (ZugtypLego w) richtung)) =
    Aeson.object [JS.weiche .= zuObjektTyp w, JS.aktion .= JS.stellen, JS.richtung .= richtung]
aktionToJSON _name (ABahngeschwindigkeitMärklinPwm (Geschwindigkeit b wert)) =
    Aeson.object
        [JS.bahngeschwindigkeit .= zuObjektTyp b, JS.aktion .= JS.geschwindigkeit, JS.wert .= wert]
aktionToJSON _name (ABahngeschwindigkeitLegoPwm (Geschwindigkeit b wert)) =
    Aeson.object
        [JS.bahngeschwindigkeit .= zuObjektTyp b, JS.aktion .= JS.geschwindigkeit, JS.wert .= wert]
aktionToJSON _name (ABahngeschwindigkeitMärklinKonstanteSpannung (Fahrstrom b wert)) =
    Aeson.object
        [JS.bahngeschwindigkeit .= zuObjektTyp b, JS.aktion .= JS.fahrstrom, JS.strom .= wert]
aktionToJSON _name (ABahngeschwindigkeitLegoKonstanteSpannung (Fahrstrom b wert)) =
    Aeson.object
        [ JS.bahngeschwindigkeit .= zuObjektTyp b
        , JS.aktion .= JS.geschwindigkeit
        , JS.strom .= wert]
aktionToJSON _name (ABahngeschwindigkeitMärklinPwm (Umdrehen b)) =
    Aeson.object [JS.bahngeschwindigkeit .= zuObjektTyp b, JS.aktion .= JS.umdrehen]
aktionToJSON _name (ABahngeschwindigkeitMärklinKonstanteSpannung (Umdrehen b)) =
    Aeson.object [JS.bahngeschwindigkeit .= zuObjektTyp b, JS.aktion .= JS.umdrehen]
aktionToJSON _name (ABahngeschwindigkeitLegoPwm (FahrtrichtungEinstellen b fahrtrichtung)) =
    Aeson.object
        [ JS.bahngeschwindigkeit .= zuObjektTyp b
        , JS.aktion .= JS.umdrehen
        , JS.fahrtrichtung .= fahrtrichtung]
aktionToJSON
    _name
    (ABahngeschwindigkeitLegoKonstanteSpannung (FahrtrichtungEinstellen b fahrtrichtung)) =
    Aeson.object
        [ JS.bahngeschwindigkeit .= zuObjektTyp b
        , JS.aktion .= JS.umdrehen
        , JS.fahrtrichtung .= fahrtrichtung]
aktionToJSON _name (AStreckenabschnitt (Strom s an)) =
    Aeson.object [JS.streckenabschnitt .= zuObjektTyp s, JS.aktion .= JS.strom, JS.an .= an]
aktionToJSON _name (AKupplung (Kuppeln k)) =
    Aeson.object [JS.kupplung .= zuObjektTyp k, JS.aktion .= JS.kuppeln]
aktionToJSON _name (AKontakt (WartenAuf k)) =
    Aeson.object [JS.kontakt .= zuObjektTyp k, JS.aktion .= JS.warten]
aktionToJSON name (AktionAusführen plan@Plan {plName})
    | name == plName =
        -- Verwende Name als Marker für Namensgleichheit (== angenommene Dauerschleife)
        -- Explizit gehandhabt, da sonst die Berechnung des Value nicht terminiert.
        Aeson.String name
    | otherwise = Aeson.object [JS.aktion .= JS.ausführen, JS.plan .= plan]

instance Aeson.FromJSON Plan where
    parseJSON :: Aeson.Value -> Aeson.Parser Plan
    parseJSON (Aeson.Object v) = do
        plName <- v .: JS.name
        aktionen <- v .: JS.aktionen
        let erzeugeDauerschleife :: Maybe Bool -> Plan
            erzeugeDauerschleife (Just True) =
                let plan = Plan { plName, plAktionen = aktionen <> (AktionAusführen plan :| []) }
                in plan
            erzeugeDauerschleife _NothingOderFalse = Plan { plName, plAktionen = aktionen }
        erzeugeDauerschleife <$> (v .:? JS.dauerschleife)
    parseJSON _value = empty

instance ( ObjektElement (bg 'Pwm 'Märklin)
         , ObjektTyp (bg 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
         , ObjektElement (bg 'KonstanteSpannung 'Märklin)
         , ObjektTyp (bg 'KonstanteSpannung 'Märklin)
           ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
         , ObjektElement (bg 'Pwm 'Lego)
         , ObjektTyp (bg 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
         , ObjektElement (bg 'KonstanteSpannung 'Lego)
         , ObjektTyp (bg 'KonstanteSpannung 'Lego) ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
         , ObjektElement st
         , ObjektTyp st ~ Streckenabschnitt
         , ObjektElement (we 'Märklin)
         , ObjektTyp (we 'Märklin) ~ Weiche 'Märklin
         , ObjektElement (we 'Lego)
         , ObjektTyp (we 'Lego) ~ Weiche 'Lego
         , ObjektElement ku
         , ObjektTyp ku ~ Kupplung
         , ObjektElement ko
         , ObjektTyp ko ~ Kontakt
         , ObjektElement (ws 'Märklin)
         , ObjektTyp (ws 'Märklin) ~ Wegstrecke 'Märklin
         , ObjektElement (ws 'Lego)
         , ObjektTyp (ws 'Lego) ~ Wegstrecke 'Lego
         ) => Aeson.ToJSON (PlanAllgemein bg st we ku ko ws) where
    toJSON :: PlanAllgemein bg st we ku ko ws -> Aeson.Value
    toJSON Plan {plName, plAktionen} =
        let aktionen =
                NonEmpty.takeWhile ((/=) $ Aeson.String plName)
                $ fmap (aktionToJSON plName) plAktionen
        in Aeson.object
               [ JS.name .= plName
               , JS.aktionen .= aktionen
               , JS.dauerschleife .= (length aktionen /= length plAktionen)]
