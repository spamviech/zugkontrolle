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

{-|
Description : Pläne sind nacheinander auszuführende Aktionen, welche mit StreckenObjekten möglich sind.

Jede Art von 'StreckenObjekt' ('Bahngeschwindigkeit', 'Streckenabschnitt', 'Weiche', 'Wegstrecke') unterstützt unterschiedliche Aktionen.
Ein 'Plan' ist eine Zusammenfassung mehrerer dieser Aktionen und Wartezeiten, welche nacheinander ausgeführt werden können.
-}
module Zug.Plan (
    -- * Allgemeine Datentypen
    PlanKlasse(..), MitAusführend(..), AusführendReader(..), Plan(..), AktionKlasse(..), Aktion(..), Ausführend(..),
    -- * Spezialisierte Aktionen
    AktionWeiche(..), AktionBahngeschwindigkeit(..), AktionStreckenabschnitt(..),
    AktionKupplung(..), AktionWegstrecke(..)) where

-- Bibliotheken
import Control.Concurrent.STM (atomically, TVar, readTVarIO, modifyTVar)
import Control.Monad (void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (MonadIO(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeiten von anderen Modulen
import Zug.Klassen (Zugtyp(..), ZugtypEither(), Richtung(), Fahrtrichtung(), Strom(..))
import Zug.Anbindung (Anschluss(), StreckenObjekt(..),
                    PwmReader(..), I2CReader(..),
                    Bahngeschwindigkeit(), BahngeschwindigkeitKlasse(..),
                    Streckenabschnitt(), StreckenabschnittKlasse(..),
                    Weiche(), WeicheKlasse(..),
                    Kupplung(), KupplungKlasse(..),
                    Wegstrecke(), WegstreckeKlasse(..),
                    warte, Wartezeit(..))
import qualified Zug.Language as Language
import Zug.Language (showText, (<~>), (<^>), (<=>), (<:>), (<°>))
import Zug.Menge (Menge(), hinzufügen, entfernen)

-- | Klasse für Typen mit den aktuell 'Ausführend'en Plänen
class MitAusführend r where
    mengeAusführend :: r -> TVar (Menge Ausführend)
-- | Abkürzung für Funktionen, die die aktuelle 'Ausführend'-'Menge' benötigen
class (PwmReader r m, MitAusführend r) => AusführendReader r m where
    -- | Erhalte die aktuelle 'Ausführend'-'Menge' aus der Umgebung.
    erhalteMengeAusführend :: m (TVar (Menge Ausführend))
    erhalteMengeAusführend = asks mengeAusführend
instance (PwmReader r m, MitAusführend r) => AusführendReader r m

-- | Mitglieder dieser Klasse sind ausführbar (können in IO-Aktionen übersetzt werden).  
-- Sie können selbst entscheiden, wann sie die mitgegebene Update-Funktion über den Fortschritt informieren.  
-- Nach der kompletten Ausführung soll der End-Aktion ausgeführt werden.  
-- Die Ausführung soll abgebrochen werden, sobald der Plan nicht mehr in der 'TVar'-'Menge' vorhanden ist.
class PlanKlasse pl where
    ausführenPlan :: (AusführendReader r m, MonadIO m) => pl -> (Natural -> IO ()) -> IO () -> m ()
    {-# MINIMAL ausführenPlan #-}

-- | Pläne: Benannte IO-Aktionen mit StreckenObjekten, bzw. Wartezeiten.
-- Die Update-Funktion wird mit Index der aktuellen Aktion vor dessen Ausführung aufgerufen.
data Plan = Plan {
    plName :: Text,
    plAktionen :: [Aktion]}
        deriving (Eq)

-- | newtype für ausführende Pläne ('Plan')
newtype Ausführend
    = Ausführend Plan
        deriving (Eq, StreckenObjekt)

instance Show Plan where
    show :: Plan -> String
    show
        Plan {plName, plAktionen}
            = Language.plan
            <:> Language.name <=> unpack plName
            <^> Language.aktionen <=> show plAktionen

instance StreckenObjekt Plan where
    anschlüsse :: Plan -> [Anschluss]
    anschlüsse Plan {plAktionen} = foldMap anschlüsse plAktionen
    erhalteName :: Plan -> Text
    erhalteName Plan {plName} = plName

instance PlanKlasse Plan where
    ausführenPlan :: (AusführendReader r m, MonadIO m) => Plan -> (Natural -> IO ()) -> IO () -> m ()
    ausführenPlan
        plan@Plan {plAktionen}
        showAktion
        endAktion
            = void $ forkI2CReader $ void $ do
                tvarAusführend <- erhalteMengeAusführend
                liftIO $ atomically $ modifyTVar tvarAusführend $ hinzufügen (Ausführend plan)
                ausführenAux 0 plAktionen
                liftIO $ do
                    showAktion $ fromIntegral $ length plAktionen
                    endAktion
        where
            ausführenAux :: (AusführendReader r m, MonadIO m) => Natural -> [Aktion] -> m ()
            ausführenAux    _i  []      = do
                tvarAusführend <- erhalteMengeAusführend
                liftIO $ atomically $ modifyTVar tvarAusführend $ entfernen $ Ausführend plan
            ausführenAux    i   (h:t)   = do
                tvarAusführend <- erhalteMengeAusführend
                ausführend <- liftIO $ readTVarIO tvarAusführend
                when (elem (Ausführend plan) ausführend) $ do
                    liftIO $ showAktion i
                    ausführenAktion h
                    ausführenAux (succ i) t

-- | Mitglieder dieser Klasse sind ausführbar.
class AktionKlasse a where
    ausführenAktion :: (AusführendReader r m, MonadIO m) => a -> m ()

-- | Eine Aktion eines 'StreckenObjekt's oder eine Wartezeit.
-- Die Update-Funktion wird nicht aufgerufen.
data Aktion
    = Warten
        Wartezeit
    | AWegstreckeMärklin
        (AktionWegstrecke Wegstrecke 'Märklin)
    | AWegstreckeLego
        (AktionWegstrecke Wegstrecke 'Lego)
    | AWeiche
        (AktionWeiche (ZugtypEither Weiche))
    | ABahngeschwindigkeitMärklin
        (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Märklin)
    | ABahngeschwindigkeitLego
        (AktionBahngeschwindigkeit Bahngeschwindigkeit 'Lego)
    | AStreckenabschnitt
        (AktionStreckenabschnitt Streckenabschnitt)
    | AKupplung
        (AktionKupplung Kupplung)
    | AktionAusführen
        Plan
    deriving (Eq)

instance Show Aktion where
    show :: Aktion -> String
    show    (Warten time)                           = Language.warten <:> show time <> Language.wartenEinheit
    show    (AWegstreckeMärklin aktion)             = Language.wegstrecke <~> show aktion
    show    (AWegstreckeLego aktion)                = Language.wegstrecke <~> show aktion
    show    (AWeiche aktion)                        = Language.weiche <~> show aktion
    show    (ABahngeschwindigkeitMärklin aktion)    = Language.bahngeschwindigkeit <~> show aktion
    show    (ABahngeschwindigkeitLego aktion)       = Language.bahngeschwindigkeit <~> show aktion
    show    (AStreckenabschnitt aktion)             = Language.streckenabschnitt <~> show aktion
    show    (AKupplung aktion)                      = Language.kupplung <~> show aktion
    show    (AktionAusführen Plan {plName})         = Language.ausführen  <:> unpack plName

instance StreckenObjekt Aktion where
    anschlüsse :: Aktion -> [Anschluss]
    anschlüsse  (Warten _zeit)                          = []
    anschlüsse  (AWegstreckeMärklin aktion)             = anschlüsse aktion
    anschlüsse  (AWegstreckeLego aktion)                = anschlüsse aktion
    anschlüsse  (AWeiche aktion)                        = anschlüsse aktion
    anschlüsse  (ABahngeschwindigkeitMärklin aktion)    = anschlüsse aktion
    anschlüsse  (ABahngeschwindigkeitLego aktion)       = anschlüsse aktion
    anschlüsse  (AStreckenabschnitt aktion)             = anschlüsse aktion
    anschlüsse  (AKupplung aktion)                      = anschlüsse aktion
    anschlüsse  (AktionAusführen plan)                  = anschlüsse plan
    erhalteName :: Aktion -> Text
    erhalteName = showText

instance AktionKlasse Aktion where
    ausführenAktion :: (AusführendReader r m, MonadIO m) => Aktion -> m ()
    ausführenAktion (Warten time)                           = warte time
    ausführenAktion (AWegstreckeMärklin aktion)             = ausführenAktion aktion
    ausführenAktion (AWegstreckeLego aktion)                = ausführenAktion aktion
    ausführenAktion (AWeiche aktion)                        = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitMärklin aktion)    = ausführenAktion aktion
    ausführenAktion (ABahngeschwindigkeitLego aktion)       = ausführenAktion aktion
    ausführenAktion (AStreckenabschnitt aktion)             = ausführenAktion aktion
    ausführenAktion (AKupplung aktion)                      = ausführenAktion aktion
    ausführenAktion (AktionAusführen plan)                  = ausführenPlan plan (const $ pure ()) (pure ())

-- | Bekannte 'Aktion'en einer 'Wegstrecke'
data AktionWegstrecke ws (z :: Zugtyp)
    = Einstellen
        (ws z)
    | AWSBahngeschwindigkeit
        (AktionBahngeschwindigkeit ws z)
    | AWSStreckenabschnitt
        (AktionStreckenabschnitt (ws z))
    | AWSKupplung
        (AktionKupplung (ws z))
    deriving (Eq)

instance (StreckenObjekt (ws z))  => Show (AktionWegstrecke ws z) where
    show :: AktionWegstrecke ws z -> String
    show    (Einstellen wegstrecke)         = unpack $ erhalteName wegstrecke <°> Language.einstellen
    show    (AWSBahngeschwindigkeit aktion) = show aktion
    show    (AWSStreckenabschnitt aktion)   = show aktion
    show    (AWSKupplung aktion)            = show aktion

instance (BahngeschwindigkeitKlasse ws, WegstreckeKlasse (ws z), Show (ws z)) => StreckenObjekt (AktionWegstrecke ws z) where
    anschlüsse :: AktionWegstrecke ws z -> [Anschluss]
    anschlüsse  (Einstellen ws)                 = anschlüsse ws
    anschlüsse  (AWSBahngeschwindigkeit aktion) = anschlüsse aktion
    anschlüsse  (AWSStreckenabschnitt aktion)   = anschlüsse aktion
    anschlüsse  (AWSKupplung aktion)            = anschlüsse aktion
    erhalteName :: AktionWegstrecke ws z -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse ws, WegstreckeKlasse (ws z)) => AktionKlasse (AktionWegstrecke ws z) where
    ausführenAktion :: (AusführendReader r m, MonadIO m) => AktionWegstrecke ws z -> m ()
    ausführenAktion (Einstellen ws)                 = einstellen ws
    ausführenAktion (AWSBahngeschwindigkeit aktion) = ausführenAktion aktion
    ausführenAktion (AWSStreckenabschnitt aktion)   = ausführenAktion aktion
    ausführenAktion (AWSKupplung aktion)            = ausführenAktion aktion

-- | Bekannte 'Aktion'en einer 'Weiche'
data AktionWeiche we 
    = Stellen
        we
        Richtung
    deriving (Eq)

instance (StreckenObjekt we)  => Show (AktionWeiche we) where
    show :: AktionWeiche we -> String
    show (Stellen we richtung) = unpack $ erhalteName we <°> Language.stellen <=> showText richtung

instance (WeicheKlasse we, Show we) => StreckenObjekt (AktionWeiche we) where
    anschlüsse :: AktionWeiche we -> [Anschluss]
    anschlüsse (Stellen we _richtung) = anschlüsse we
    erhalteName :: AktionWeiche we -> Text
    erhalteName = showText

instance (WeicheKlasse w) => AktionKlasse (AktionWeiche w) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionWeiche w -> m ()
    ausführenAktion (Stellen we richtung) = stellen we richtung

-- | Aktionen einer Bahngeschwindigkeit
data AktionBahngeschwindigkeit bg (z :: Zugtyp) where
    Geschwindigkeit ::
        bg z ->
        Natural ->
            AktionBahngeschwindigkeit bg z
    Umdrehen ::
        bg 'Märklin ->
            AktionBahngeschwindigkeit bg 'Märklin
    FahrtrichtungEinstellen ::
        bg 'Lego ->
        Fahrtrichtung ->
            AktionBahngeschwindigkeit bg 'Lego

deriving instance (Eq (bg z)) => (Eq (AktionBahngeschwindigkeit bg z))

instance (StreckenObjekt (bg z)) => Show (AktionBahngeschwindigkeit bg z) where
    show :: AktionBahngeschwindigkeit bg  z-> String
    show
        (Geschwindigkeit bg wert)
            = unpack $ erhalteName bg <°> Language.geschwindigkeit <=> showText wert
    show
        (Umdrehen bg)
            = unpack $ erhalteName bg <°> Language.umdrehen
    show
        (FahrtrichtungEinstellen bg fahrtrichtung)
            = unpack $ erhalteName bg <°> Language.umdrehen <=> showText fahrtrichtung

instance (BahngeschwindigkeitKlasse bg, Show (bg z), StreckenObjekt (bg 'Märklin), StreckenObjekt (bg 'Lego), StreckenObjekt (bg z))
            => StreckenObjekt (AktionBahngeschwindigkeit bg z) where
    anschlüsse :: AktionBahngeschwindigkeit bg z -> [Anschluss]
    anschlüsse  (Geschwindigkeit bg _wert)                  = anschlüsse bg
    anschlüsse  (Umdrehen bg)                               = anschlüsse bg
    anschlüsse  (FahrtrichtungEinstellen bg _fahrtrichtung) = anschlüsse bg
    erhalteName :: AktionBahngeschwindigkeit bg z -> Text
    erhalteName = showText

instance (BahngeschwindigkeitKlasse bg) => AktionKlasse (AktionBahngeschwindigkeit bg z) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionBahngeschwindigkeit bg z -> m ()
    ausführenAktion (Geschwindigkeit bg wert)                   = geschwindigkeit bg wert
    ausführenAktion (Umdrehen bg)                               = umdrehen bg
    ausführenAktion (FahrtrichtungEinstellen bg fahrtrichtung)  = fahrtrichtungEinstellen bg fahrtrichtung

-- | Aktionen eines Streckenabschnitts
data AktionStreckenabschnitt st
    = Strom
        st
        Strom
    deriving (Eq)

instance (StreckenObjekt st) => Show (AktionStreckenabschnitt st) where
    show :: AktionStreckenabschnitt st -> String
    show    (Strom st Fließend)  = unpack $ erhalteName st <°> Language.strom <=> Language.an
    show    (Strom st Gesperrt)  = unpack $ erhalteName st <°> Language.strom <=> Language.aus

instance (StreckenabschnittKlasse st, Show st) => StreckenObjekt (AktionStreckenabschnitt st) where
    anschlüsse :: AktionStreckenabschnitt st -> [Anschluss]
    anschlüsse (Strom st _an) = anschlüsse st
    erhalteName :: AktionStreckenabschnitt st -> Text
    erhalteName = showText

instance (StreckenabschnittKlasse st) => AktionKlasse (AktionStreckenabschnitt st) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionStreckenabschnitt st -> m ()
    ausführenAktion (Strom st an) = strom st an

-- | Aktionen einer Kupplung
data AktionKupplung ku
    = Kuppeln
        ku
    deriving (Eq)

instance (StreckenObjekt ku) => Show (AktionKupplung ku) where
    show :: AktionKupplung ku -> String
    show (Kuppeln ku) = unpack $ erhalteName ku <°> Language.kuppeln

instance (KupplungKlasse ku, Show ku) => StreckenObjekt (AktionKupplung ku) where
    anschlüsse :: AktionKupplung ku -> [Anschluss]
    anschlüsse (Kuppeln ku) = anschlüsse ku
    erhalteName :: AktionKupplung ku -> Text
    erhalteName = showText

instance (KupplungKlasse ku) => AktionKlasse (AktionKupplung ku) where
    ausführenAktion :: (PwmReader r m, MonadIO m) => AktionKupplung ku -> m ()
    ausführenAktion (Kuppeln ku) = kuppeln ku