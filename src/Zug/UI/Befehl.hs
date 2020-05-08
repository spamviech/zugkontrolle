{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : Alle durch ein UI unterstützten Befehle, inklusive der Implementierung.
-}
module Zug.UI.Befehl
  ( -- * Klasse
    BefehlKlasse(..)
  , BefehlConstraints
    -- * Typen
  , Befehl
  , BefehlAllgemein(..)
  , BefehlListe
  , BefehlListeAllgemein(..)
  , UIBefehl
  , UIBefehlAllgemein(..)
  ) where

import Control.Concurrent.STM (atomically, writeTVar, modifyTVar)
import qualified Control.Monad.RWS.Strict as RWS
import Control.Monad.Trans (MonadIO(..))
import Data.Aeson (ToJSON)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Zug.Anbindung
       (pwmMapEmpty, i2cMapEmpty, Bahngeschwindigkeit(), BahngeschwindigkeitKlasse()
      , Streckenabschnitt(), StreckenabschnittKlasse(), Weiche(), WeicheKlasse(), Kupplung()
      , KupplungKlasse(), Kontakt(), KontaktKlasse(), Wegstrecke(), WegstreckeKlasse())
import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..), GeschwindigkeitPhantom())
import Zug.Language (Sprache(), MitSprache(..))
import Zug.Objekt (ObjektKlasse(..), ObjektAllgemein(..), Objekt, ObjektElement(ObjektTyp))
import Zug.Plan (PlanKlasse(..), Plan, PlanAllgemein(), AusführendReader(..), Ausführend(..)
               , AktionKlasse(..), Aktion())
import Zug.UI.Base
       (StatusAllgemein(), Status, IOStatusAllgemein, MStatusAllgemeinT, ReaderFamilie, TVarMaps(..)
      , MitTVarMaps(), TVarMapsReader(..), liftIOStatus, hinzufügenBahngeschwindigkeit
      , entfernenBahngeschwindigkeit, hinzufügenStreckenabschnitt, entfernenStreckenabschnitt
      , hinzufügenWeiche, entfernenWeiche, hinzufügenKupplung, entfernenKupplung
      , hinzufügenKontakt, entfernenKontakt, hinzufügenWegstrecke, entfernenWegstrecke
      , hinzufügenPlan, entfernenPlan, getSprache, putSprache)
import qualified Zug.UI.Save as Save

-- | Ausführen eines Befehls
class BefehlKlasse b o where
    -- | Gibt True zurück, falls das UI beendet werden soll
    ausführenBefehl :: (MonadIO m) => b o -> MStatusAllgemeinT m o Bool

-- | Alle unterstützten Befehle
data BefehlAllgemein o
    = UI (UIBefehlAllgemein o)
    | SprachWechsel (SP o)
    | Hinzufügen o
    | Entfernen o
    | Speichern FilePath
    | Laden
          { ladenPfad :: FilePath
          , erfolgsAktion :: Status -> IOStatusAllgemein o ()
          , fehlerbehandlung :: IOStatusAllgemein o ()
          }
    | Ausführen
          { auszuführenderPlan :: PlanAllgemein (BG o) (ST o) (WE o) (KU o) (KO o) (WS o)
          , fortschrittsanzeige :: Natural -> Sprache -> IO ()
          , abschlussAnzeige :: IO ()
          }
    | AusführenAbbrechen Plan
    | AktionBefehl Aktion

-- | 'BefehlAllgemein' spezialisiert auf 'Objekt'
type Befehl = BefehlAllgemein Objekt

-- | UI-spezifische Befehle. Phantomtyp, um eine 'BefehlKlasse'-Instanz zu erhalten.
data UIBefehlAllgemein o
    = Beenden
    | Abbrechen
    deriving (Show)

-- | 'UIBefehlAllgemein' spezialisiert auf 'Objekt'
type UIBefehl = UIBefehlAllgemein Objekt

instance BefehlKlasse UIBefehlAllgemein o where
    ausführenBefehl :: (MonadIO m) => UIBefehlAllgemein o -> MStatusAllgemeinT m o Bool
    ausführenBefehl Beenden = pure True
    ausführenBefehl Abbrechen = pure False

-- | Voraussetzungen, die ein 'ObjektKlasse'-Typ erfüllen muss,
-- damit ein 'BefehlAllgemein' ausgeführt werden kann.
class ( ObjektKlasse o
      , ToJSON o
      , ObjektElement (BG o 'Pwm 'Märklin)
      , ObjektTyp (BG o 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
      , Eq (BG o 'Pwm 'Märklin)
      , BahngeschwindigkeitKlasse (BG o)
      , ObjektElement (BG o 'KonstanteSpannung 'Märklin)
      , ObjektTyp (BG o 'KonstanteSpannung 'Märklin)
            ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
      , Eq (BG o 'KonstanteSpannung 'Märklin)
      , ObjektElement (BG o 'Pwm 'Lego)
      , ObjektTyp (BG o 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
      , Eq (BG o 'Pwm 'Lego)
      , ObjektElement (BG o 'KonstanteSpannung 'Lego)
      , ObjektTyp (BG o 'KonstanteSpannung 'Lego) ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
      , Eq (BG o 'KonstanteSpannung 'Lego)
      , ObjektElement (ST o)
      , ObjektTyp (ST o) ~ Streckenabschnitt
      , Eq (ST o)
      , StreckenabschnittKlasse (ST o)
      , ObjektElement (WE o 'Märklin)
      , ObjektTyp (WE o 'Märklin) ~ Weiche 'Märklin
      , Eq (WE o 'Märklin)
      , WeicheKlasse (WE o 'Märklin)
      , ObjektElement (WE o 'Lego)
      , ObjektTyp (WE o 'Lego) ~ Weiche 'Lego
      , Eq (WE o 'Lego)
      , WeicheKlasse (WE o 'Lego)
      , ObjektElement (KU o)
      , ObjektTyp (KU o) ~ Kupplung
      , Eq (KU o)
      , KupplungKlasse (KU o)
      , ObjektElement (KO o)
      , ObjektTyp (KO o) ~ Kontakt
      , Eq (KO o)
      , KontaktKlasse (KO o)
      , ObjektElement (WS o 'Märklin)
      , ObjektTyp (WS o 'Märklin) ~ Wegstrecke 'Märklin
      , Eq (WS o 'Märklin)
      , KontaktKlasse (WS o 'Märklin)
      , WegstreckeKlasse (WS o 'Märklin)
      , ObjektElement (WS o 'Lego)
      , ObjektTyp (WS o 'Lego) ~ Wegstrecke 'Lego
      , Eq (WS o 'Lego)
      , KontaktKlasse (WS o 'Lego)
      , WegstreckeKlasse (WS o 'Lego)
      , BahngeschwindigkeitKlasse (GeschwindigkeitPhantom (WS o))
      , Eq (PL o)
      , MitSprache (SP o)
      , MitTVarMaps (ReaderFamilie o)
      ) => BefehlConstraints o

instance ( ObjektKlasse o
         , ToJSON o
         , ObjektElement (BG o 'Pwm 'Märklin)
         , ObjektTyp (BG o 'Pwm 'Märklin) ~ Bahngeschwindigkeit 'Pwm 'Märklin
         , Eq (BG o 'Pwm 'Märklin)
         , BahngeschwindigkeitKlasse (BG o)
         , ObjektElement (BG o 'KonstanteSpannung 'Märklin)
         , ObjektTyp (BG o 'KonstanteSpannung 'Märklin)
               ~ Bahngeschwindigkeit 'KonstanteSpannung 'Märklin
         , Eq (BG o 'KonstanteSpannung 'Märklin)
         , ObjektElement (BG o 'Pwm 'Lego)
         , ObjektTyp (BG o 'Pwm 'Lego) ~ Bahngeschwindigkeit 'Pwm 'Lego
         , Eq (BG o 'Pwm 'Lego)
         , ObjektElement (BG o 'KonstanteSpannung 'Lego)
         , ObjektTyp (BG o 'KonstanteSpannung 'Lego) ~ Bahngeschwindigkeit 'KonstanteSpannung 'Lego
         , Eq (BG o 'KonstanteSpannung 'Lego)
         , ObjektElement (ST o)
         , ObjektTyp (ST o) ~ Streckenabschnitt
         , Eq (ST o)
         , StreckenabschnittKlasse (ST o)
         , ObjektElement (WE o 'Märklin)
         , ObjektTyp (WE o 'Märklin) ~ Weiche 'Märklin
         , Eq (WE o 'Märklin)
         , WeicheKlasse (WE o 'Märklin)
         , ObjektElement (WE o 'Lego)
         , ObjektTyp (WE o 'Lego) ~ Weiche 'Lego
         , Eq (WE o 'Lego)
         , WeicheKlasse (WE o 'Lego)
         , ObjektElement (KU o)
         , ObjektTyp (KU o) ~ Kupplung
         , Eq (KU o)
         , KupplungKlasse (KU o)
         , ObjektElement (KO o)
         , ObjektTyp (KO o) ~ Kontakt
         , Eq (KO o)
         , KontaktKlasse (KO o)
         , ObjektElement (WS o 'Märklin)
         , ObjektTyp (WS o 'Märklin) ~ Wegstrecke 'Märklin
         , Eq (WS o 'Märklin)
         , KontaktKlasse (WS o 'Märklin)
         , WegstreckeKlasse (WS o 'Märklin)
         , ObjektElement (WS o 'Lego)
         , ObjektTyp (WS o 'Lego) ~ Wegstrecke 'Lego
         , Eq (WS o 'Lego)
         , KontaktKlasse (WS o 'Lego)
         , WegstreckeKlasse (WS o 'Lego)
         , BahngeschwindigkeitKlasse (GeschwindigkeitPhantom (WS o))
         , Eq (PL o)
         , MitSprache (SP o)
         , MitTVarMaps (ReaderFamilie o)
         ) => BefehlConstraints o

instance forall o. (BefehlConstraints o) => BefehlKlasse BefehlAllgemein o where
    ausführenBefehl :: forall m. (MonadIO m) => BefehlAllgemein o -> MStatusAllgemeinT m o Bool
    ausführenBefehl befehl = ausführenBefehlAux befehl >> pure (istBeenden befehl)
        where
            istBeenden :: BefehlAllgemein o -> Bool
            istBeenden (UI Beenden) = True
            istBeenden _befehl = False

            ausführenBefehlAux :: BefehlAllgemein o -> MStatusAllgemeinT m o ()
            ausführenBefehlAux (UI _uiAction) = pure ()
            ausführenBefehlAux (SprachWechsel sprache) = putSprache sprache
            ausführenBefehlAux (Hinzufügen objekt) = case erhalteObjekt objekt of
                (OBahngeschwindigkeit bahngeschwindigkeit)
                    -> hinzufügenBahngeschwindigkeit bahngeschwindigkeit
                (OStreckenabschnitt streckenabschnitt)
                    -> hinzufügenStreckenabschnitt streckenabschnitt
                (OWeiche weiche) -> hinzufügenWeiche weiche
                (OKupplung kupplung) -> hinzufügenKupplung kupplung
                (OKontakt kontakt) -> hinzufügenKontakt kontakt
                (OWegstrecke wegstrecke) -> hinzufügenWegstrecke wegstrecke
                (OPlan plan) -> hinzufügenPlan plan
            ausführenBefehlAux (Entfernen objekt) = case erhalteObjekt objekt of
                (OBahngeschwindigkeit bahngeschwindigkeit)
                    -> entfernenBahngeschwindigkeit bahngeschwindigkeit
                (OStreckenabschnitt streckenabschnitt)
                    -> entfernenStreckenabschnitt streckenabschnitt
                (OWeiche weiche) -> entfernenWeiche weiche
                (OKupplung kupplung) -> entfernenKupplung kupplung
                (OKontakt kontakt) -> entfernenKontakt kontakt
                (OWegstrecke wegstrecke) -> entfernenWegstrecke wegstrecke
                (OPlan plan) -> entfernenPlan plan
            ausführenBefehlAux (Speichern dateipfad) =
                RWS.get >>= liftIO . flip Save.speichern dateipfad
            ausführenBefehlAux (Laden dateipfad erfolgsAktion fehlerbehandlung) = do
                mitSprache <- getSprache
                reader <- RWS.ask
                state0 <- RWS.get
                let erfolgsIO :: Status -> IO (StatusAllgemein o)
                    erfolgsIO statusNeu =
                        fst <$> RWS.execRWST (erfolgsAktion statusNeu) reader state0
                liftIO (flip leseSprache mitSprache $ Save.laden dateipfad erfolgsIO) >>= \case
                    Nothing -> liftIOStatus fehlerbehandlung
                    (Just statusNeu) -> do
                        TVarMaps {tvarPwmMap, tvarI2CMap} <- erhalteTVarMaps
                        liftIO $ do
                            atomically $ writeTVar tvarPwmMap pwmMapEmpty
                            atomically $ writeTVar tvarI2CMap i2cMapEmpty
                        RWS.put statusNeu
            ausführenBefehlAux (Ausführen plan showAction endAktion) = do
                mitSprache <- getSprache
                ausführenPlan plan (leseSprache (flip showAction) mitSprache) endAktion
            ausführenBefehlAux (AusführenAbbrechen plan) = do
                tvarAusführend <- erhalteMengeAusführend
                liftIO $ atomically $ modifyTVar tvarAusführend $ Set.delete $ Ausführend plan
            ausführenBefehlAux (AktionBefehl aktion) = ausführenAktion aktion

-- | Normale Listen von 'BefehlAllgemein' haben den falschen Kind um eine 'BefehlKlasse'-Instanz zu erhalten.
-- Der hier bereitgestellte ein newtype löst das Problem über einen Phantomtyp.
newtype BefehlListeAllgemein o = BefehlListe { getBefehlListe :: [BefehlAllgemein o] }

-- | 'BefehlListeAllgemein' spezialisiert auf 'Objekt'
type BefehlListe = BefehlListeAllgemein Objekt

instance (BefehlConstraints o) => BefehlKlasse BefehlListeAllgemein o where
    ausführenBefehl
        :: forall m. (MonadIO m) => BefehlListeAllgemein o -> MStatusAllgemeinT m o Bool
    ausführenBefehl (BefehlListe liste) = ausführenBefehlAux liste
        where
            ausführenBefehlAux :: [BefehlAllgemein o] -> MStatusAllgemeinT m o Bool
            ausführenBefehlAux [] = pure False
            ausführenBefehlAux (h:t) = do
                ende <- ausführenBefehl h
                if ende
                    then pure True
                    else ausführenBefehlAux t