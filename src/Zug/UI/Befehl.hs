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

{-|
Description : Alle durch ein UI unterstützten Befehle, inklusive der Implementierung.
-}
module Zug.UI.Befehl
  ( -- * Klasse
    BefehlKlasse(..)
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

import Zug.Anbindung (pwmMapEmpty, i2cMapEmpty)
import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..))
import Zug.Language (Sprache(), MitSprache(..))
import Zug.Objekt (ObjektKlasse(..), ObjektAllgemein(..), Objekt)
import Zug.Plan
       (PlanKlasse(..), Plan(), AusführendReader(..), Ausführend(..), AktionKlasse(..), Aktion())
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
          , erfolgsAktion :: (Status -> IOStatusAllgemein o ())
          , fehlerbehandlung :: (IOStatusAllgemein o ())
          }
    | Ausführen
          { auszuführenderPlan :: Plan
          , fortschrittsanzeige :: (Natural -> Sprache -> IO ())
          , abschlussAnzeige :: (IO ())
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

instance ( ObjektKlasse o
         , ToJSON o
         , Eq ((BG o) 'Pwm 'Märklin)
         , Eq ((BG o) 'KonstanteSpannung 'Märklin)
         , Eq ((BG o) 'Pwm 'Lego)
         , Eq ((BG o) 'KonstanteSpannung 'Lego)
         , Eq (ST o)
         , Eq ((WE o) 'Märklin)
         , Eq ((WE o) 'Lego)
         , Eq (KU o)
         , Eq (KO o)
         , Eq ((WS o) 'Märklin)
         , Eq ((WS o) 'Lego)
         , Eq (PL o)
         , MitSprache (SP o)
         , MitTVarMaps (ReaderFamilie o)
         ) => BefehlKlasse BefehlAllgemein o where
    ausführenBefehl :: (MonadIO m) => BefehlAllgemein o -> MStatusAllgemeinT m o Bool
    ausführenBefehl befehl = ausführenBefehlAux befehl >> pure (istBeenden befehl)
        where
            istBeenden :: BefehlAllgemein o -> Bool
            istBeenden (UI Beenden) = True
            istBeenden _befehl = False

            ausführenBefehlAux
                :: forall o m.
                ( ObjektKlasse o
                , ToJSON o
                , Eq ((BG o) 'Pwm 'Märklin)
                , Eq ((BG o) 'KonstanteSpannung 'Märklin)
                , Eq ((BG o) 'Pwm 'Lego)
                , Eq ((BG o) 'KonstanteSpannung 'Lego)
                , Eq (ST o)
                , Eq ((WE o) 'Märklin)
                , Eq ((WE o) 'Lego)
                , Eq (KU o)
                , Eq (KO o)
                , Eq ((WS o) 'Märklin)
                , Eq ((WS o) 'Lego)
                , Eq (PL o)
                , MitSprache (SP o)
                , MitTVarMaps (ReaderFamilie o)
                , MonadIO m
                )
                => BefehlAllgemein o
                -> MStatusAllgemeinT m o ()
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

instance ( ObjektKlasse o
         , ToJSON o
         , Eq ((BG o) 'Pwm 'Märklin)
         , Eq ((BG o) 'KonstanteSpannung 'Märklin)
         , Eq ((BG o) 'Pwm 'Lego)
         , Eq ((BG o) 'KonstanteSpannung 'Lego)
         , Eq (ST o)
         , Eq ((WE o) 'Märklin)
         , Eq ((WE o) 'Lego)
         , Eq (KU o)
         , Eq (KO o)
         , Eq ((WS o) 'Märklin)
         , Eq ((WS o) 'Lego)
         , Eq (PL o)
         , MitSprache (SP o)
         , MitTVarMaps (ReaderFamilie o)
         ) => BefehlKlasse BefehlListeAllgemein o where
    ausführenBefehl :: (MonadIO m) => BefehlListeAllgemein o -> MStatusAllgemeinT m o Bool
    ausführenBefehl (BefehlListe liste) = ausführenBefehlAux liste
        where
            ausführenBefehlAux
                :: ( ObjektKlasse o
                   , ToJSON o
                   , Eq ((BG o) 'Pwm 'Märklin)
                   , Eq ((BG o) 'KonstanteSpannung 'Märklin)
                   , Eq ((BG o) 'Pwm 'Lego)
                   , Eq ((BG o) 'KonstanteSpannung 'Lego)
                   , Eq (ST o)
                   , Eq ((WE o) 'Märklin)
                   , Eq ((WE o) 'Lego)
                   , Eq (KU o)
                   , Eq (KO o)
                   , Eq ((WS o) 'Märklin)
                   , Eq ((WS o) 'Lego)
                   , Eq (PL o)
                   , MitSprache (SP o)
                   , MitTVarMaps (ReaderFamilie o)
                   , MonadIO m
                   )
                => [BefehlAllgemein o]
                -> MStatusAllgemeinT m o Bool
            ausführenBefehlAux [] = pure False
            ausführenBefehlAux (h:t) = do
                ende <- ausführenBefehl h
                if ende
                    then pure True
                    else ausführenBefehlAux t