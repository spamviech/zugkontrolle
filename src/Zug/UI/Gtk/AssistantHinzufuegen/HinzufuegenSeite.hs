{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
#endif

{-|
Description: Seiten eines AssistantHinzufügen.
-}
module Zug.UI.Gtk.AssistantHinzufuegen.HinzufuegenSeite
  (
#ifdef ZUGKONTROLLEGUI
    HinzufügenSeite()
  , ButtonHinzufügen(..)
  , spezifischerButtonHinzufügen
  , seiteErgebnis
  , setzeSeite
  , hinzufügenBahngeschwindigkeitNew
  , hinzufügenStreckenabschnittNew
  , hinzufügenWeicheNew
  , hinzufügenKupplungNew
  , hinzufügenKontaktNew
  , hinzufügenWegstreckeNew
  , hinzufügenPlanNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM
       (TVar, atomically, readTVarIO, newTVarIO, writeTVar, modifyTVar, swapTVar, putTMVar)
import Control.Monad (void, forM, forM_, foldM, when)
import Control.Monad.Fix (MonadFix())
import Control.Monad.Reader (runReaderT, MonadReader(ask))
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (Foldable(..))
import Data.Function ((&))
import Data.Int (Int32)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, catMaybes, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified GI.Gtk as Gtk
import GI.Gtk (AttrOp((:=)))

import Zug.Anbindung
       (Bahngeschwindigkeit(..), GeschwindigkeitsAnschlüsse(..), FahrtrichtungsAnschluss(..)
      , BahngeschwindigkeitContainer(..), Streckenabschnitt(..), StreckenabschnittContainer(..)
      , Weiche(..), Kupplung(..), KupplungContainer(..), Kontakt(..), KontaktContainer(..)
      , Wegstrecke(..), Wartezeit(..), StreckenObjekt(erhalteName), StreckenAtom(fließend)
      , AnschlussEither(), InterruptPinBenötigt(..))
import Zug.Enums (Richtung(..), unterstützteRichtungen, Zugtyp(..), ZugtypKlasse(..)
                , ZugtypEither(..), zugtyp, ausZugtypEither, GeschwindigkeitVariante(..)
                , GeschwindigkeitEither(..), geschwindigkeitVariante)
import qualified Zug.Language as Language
import Zug.Language (MitSprache(..), Anzeige(..), (<:>))
import Zug.Objekt (ObjektAllgemein(..), Objekt, ObjektElement(..))
import Zug.Plan (PlanAllgemein(..), AktionAllgemein(..), Aktion)
import Zug.UI.Base (bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen, kontakte)
import Zug.UI.Gtk.Anschluss
       (PinAuswahlWidget, pinAuswahlNew, aktuellerPin, setzePin, AnschlussAuswahlWidget
      , anschlussAuswahlNew, anschlussAuswahlInterruptPinNew, aktuellerAnschluss, setzeAnschluss)
import Zug.UI.Gtk.AssistantHinzufuegen.AktionBahngeschwindigkeit
       (aktionBahngeschwindigkeitAuswahlPackNew)
import Zug.UI.Gtk.AssistantHinzufuegen.AktionKontakt (aktionKontaktAuswahlPackNew)
import Zug.UI.Gtk.AssistantHinzufuegen.AktionKupplung (aktionKupplungAuswahlPackNew)
import Zug.UI.Gtk.AssistantHinzufuegen.AktionPlan (aktionPlanAuswahlPackNew)
import Zug.UI.Gtk.AssistantHinzufuegen.AktionStreckenabschnitt
       (aktionStreckenabschnittAuswahlPackNew)
import Zug.UI.Gtk.AssistantHinzufuegen.AktionWegstrecke (aktionWegstreckeAuswahlPackNew)
import Zug.UI.Gtk.AssistantHinzufuegen.AktionWeiche (aktionWeicheAuswahlPackNew)
import Zug.UI.Gtk.Auswahl (AuswahlWidget, auswahlComboBoxNew, auswahlComboBoxNamedNew
                         , MitAuswahlWidget(), aktuelleAuswahl, setzeAuswahl)
import Zug.UI.Gtk.Fliessend (FließendAuswahlWidget, aktuellerFließendValue, setzeFließendValue)
import Zug.UI.Gtk.FortfahrenWennToggled
       (fortfahrenWennToggledNew, checkButtons, FortfahrenWennToggledVar, RegistrierterCheckButton
      , registrierterCheckButtonToggled, registrierterCheckButtonSetToggled)
import Zug.UI.Gtk.Hilfsfunktionen
       (widgetShowNew, widgetShowIf, boxPackWidgetNewDefault, boxPackWidgetNew, boxPack
      , containerAddWidgetNew, labelSpracheNew, buttonNewWithEventLabel, Packing(PackGrow)
      , paddingDefault, positionDefault, notebookAppendPageNew, NameAuswahlWidget
      , nameAuswahlPackNew, aktuellerName, setzeName)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitButton(..), MitContainer(..), MitGrid(..)
                         , mitContainerRemove, MitWindow(..))
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), verwendeSpracheGui, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt
       (StatusGui, StatusVarGui, StatusVarGuiReader, BGWidgets(), WEWidgets()
      , WegstreckenElement(..), WegstreckeCheckButton(), WegstreckeCheckButtonVoid, WidgetsTyp(..)
      , widgetHinzufügenToggled, widgetHinzufügenSetToggled, widgetHinzufügenAktuelleAuswahl
      , widgetHinzufügenSetzeAuswahl, DynamischeWidgets(..), DynamischeWidgetsReader(..)
      , BGWidgetsBoxen(..), STWidgetsBoxen(..), WEWidgetsBoxen(..), KUWidgetsBoxen(..)
      , KOWidgetsBoxen(..), WSWidgetsBoxen(..), PLWidgetsBoxen(..))
import Zug.UI.Gtk.ZugtypSpezifisch
       (ZugtypSpezifisch(), zugtypSpezifischNew, zugtypSpezifischButtonNew)
import Zug.UI.StatusVar (StatusVarReader(..), readStatusVar)
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes)

-- | Seiten des Hinzufügen-'Assistant'
data HinzufügenSeite
    = HinzufügenSeiteBahngeschwindigkeit
          { vBox :: Gtk.Box
          , maybeTVarSprache :: Maybe TVarSprachewechselAktionen
          , nameAuswahl :: NameAuswahlWidget
          , notebookGeschwindigkeit :: Gtk.Notebook
          , indexSeiten :: Map Int32 GeschwindigkeitVariante
          , geschwindigkeitAuswahl :: PinAuswahlWidget
          , vBoxFahrstrom :: ScrollbaresWidget Gtk.Box
            -- Märklin
          , tvarFahrstromAuswahlWidgets
                :: TVar (NonEmpty (AnschlussAuswahlWidget 'InterruptPinEgal))
          , umdrehenAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
            -- Lego
          , pwmFahrtrichtungsAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
          , konstanteSpannungFahrtrichtungsAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
          }
    | HinzufügenSeiteStreckenabschnitt
          { vBox :: Gtk.Box
          , nameAuswahl :: NameAuswahlWidget
          , stromAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
          }
    | HinzufügenSeiteWeiche
          { vBox :: Gtk.Box
          , nameAuswahl :: NameAuswahlWidget
          , buttonHinzufügenWeiche :: ZugtypSpezifisch Gtk.Button
            -- Märklin
          , märklinRichtungsAuswahl :: NonEmpty ( Richtung
                                                 , RegistrierterCheckButton
                                                 , AnschlussAuswahlWidget 'InterruptPinEgal
                                                 )
            -- Lego
          , legoRichtungsAuswahl :: PinAuswahlWidget
          , legoRichtungenAuswahl :: AuswahlWidget (Richtung, Richtung)
          }
    | HinzufügenSeiteKupplung
          { vBox :: Gtk.Box
          , nameAuswahl :: NameAuswahlWidget
          , kupplungsAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
          }
    | HinzufügenSeiteKontakt
          { vBox :: Gtk.Box
          , nameAuswahl :: NameAuswahlWidget
          , kontaktAuswahl :: AnschlussAuswahlWidget 'InterruptPinBenötigt
          }
    | HinzufügenSeiteWegstrecke
          { vBox :: Gtk.Box
          , nameAuswahl :: NameAuswahlWidget
          , buttonHinzufügenWegstrecke
                :: FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid
          }
    | HinzufügenSeitePlan
          { vBox :: Gtk.Box
          , nameAuswahl :: NameAuswahlWidget
          , buttonHinzufügenPlan :: Gtk.Button
          , expanderAktionen :: Gtk.Expander
          , tvarExpander :: TVarSprachewechselAktionen
          , vBoxAktionen :: ScrollbaresWidget Gtk.Box
          , tvarAktionen :: TVar (Warteschlange (Aktion, Gtk.Label, TVarSprachewechselAktionen))
          , checkButtonDauerschleife :: Gtk.CheckButton
          }
    deriving (Eq)

instance MitWidget HinzufügenSeite where
    erhalteWidget :: (MonadIO m) => HinzufügenSeite -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . vBox

-- | Varianten eines 'Gtk.Button', um das Hinzufügen auszulösen.
data ButtonHinzufügen
    = ButtonHinzufügen Gtk.Button
    | ButtonHinzufügenZugtypSpezifisch (ZugtypSpezifisch Gtk.Button)
    | ButtonHinzufügenFortfahrenWennToggledVar (FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid)
    deriving (Eq)

instance MitWidget ButtonHinzufügen where
    erhalteWidget :: (MonadIO m) => ButtonHinzufügen -> m Gtk.Widget
    erhalteWidget (ButtonHinzufügen button) = erhalteWidget button
    erhalteWidget (ButtonHinzufügenZugtypSpezifisch button) = erhalteWidget button
    erhalteWidget (ButtonHinzufügenFortfahrenWennToggledVar button) = erhalteWidget button

instance MitContainer ButtonHinzufügen where
    erhalteContainer :: (MonadIO m) => ButtonHinzufügen -> m Gtk.Container
    erhalteContainer (ButtonHinzufügen button) = erhalteContainer button
    erhalteContainer (ButtonHinzufügenZugtypSpezifisch button) = erhalteContainer button
    erhalteContainer (ButtonHinzufügenFortfahrenWennToggledVar button) = erhalteContainer button

instance MitButton ButtonHinzufügen where
    erhalteButton :: (MonadIO m) => ButtonHinzufügen -> m Gtk.Button
    erhalteButton (ButtonHinzufügen button) = pure button
    erhalteButton (ButtonHinzufügenZugtypSpezifisch button) = erhalteButton button
    erhalteButton (ButtonHinzufügenFortfahrenWennToggledVar button) = erhalteButton button

-- | Erhalte den Seiten-spezifischen Hinzufügen-Button (sofern vorhanden).
spezifischerButtonHinzufügen :: HinzufügenSeite -> Maybe ButtonHinzufügen
spezifischerButtonHinzufügen HinzufügenSeiteWeiche {buttonHinzufügenWeiche} =
    Just $ ButtonHinzufügenZugtypSpezifisch buttonHinzufügenWeiche
spezifischerButtonHinzufügen HinzufügenSeiteWegstrecke {buttonHinzufügenWegstrecke} =
    Just $ ButtonHinzufügenFortfahrenWennToggledVar buttonHinzufügenWegstrecke
spezifischerButtonHinzufügen
    HinzufügenSeitePlan {buttonHinzufügenPlan} = Just $ ButtonHinzufügen buttonHinzufügenPlan
spezifischerButtonHinzufügen _hinzufügenSeite = Nothing

-- | Erhalte das Ergebnis einer 'HinzufügenSeite'.
seiteErgebnis :: forall r m.
              (StatusVarGuiReader r m, MonadIO m)
              => FließendAuswahlWidget
              -> AuswahlWidget Zugtyp
              -> HinzufügenSeite
              -> m Objekt
seiteErgebnis
    fließendAuswahl
    zugtypAuswahl
    HinzufügenSeiteBahngeschwindigkeit
    { nameAuswahl
    , notebookGeschwindigkeit
    , indexSeiten
    , geschwindigkeitAuswahl
    , tvarFahrstromAuswahlWidgets
    , umdrehenAuswahl
    , pwmFahrtrichtungsAuswahl
    , konstanteSpannungFahrtrichtungsAuswahl} = liftIO $ do
    bgName <- aktuellerName nameAuswahl
    bgFließend <- aktuellerFließendValue fließendAuswahl
    aktuelleAuswahl zugtypAuswahl >>= \case
        Märklin -> do
            (`Map.lookup` indexSeiten) <$> Gtk.get notebookGeschwindigkeit Gtk.notebookPage >>= \case
                (Just Pwm) -> do
                    geschwindigkeitsPin <- aktuellerPin geschwindigkeitAuswahl
                    pure
                        $ OBahngeschwindigkeit
                        $ ZugtypMärklin
                        $ GeschwindigkeitPwm
                            Bahngeschwindigkeit
                            { bgName
                            , bgFließend
                            , bgGeschwindigkeitsAnschlüsse =
                                  GeschwindigkeitsPin { geschwindigkeitsPin }
                            , bgFahrtrichtungsAnschluss = KeinExpliziterAnschluss
                            }
                (Just KonstanteSpannung) -> do
                    fahrstromAuswahlWidgets <- readTVarIO tvarFahrstromAuswahlWidgets
                    fahrstromAnschlüsse <- mapM aktuellerAnschluss fahrstromAuswahlWidgets
                    umdrehenAnschluss <- aktuellerAnschluss umdrehenAuswahl
                    pure
                        $ OBahngeschwindigkeit
                        $ ZugtypMärklin
                        $ GeschwindigkeitKonstanteSpannung
                            Bahngeschwindigkeit
                            { bgName
                            , bgFließend
                            , bgGeschwindigkeitsAnschlüsse =
                                  FahrstromAnschlüsse { fahrstromAnschlüsse }
                            , bgFahrtrichtungsAnschluss = UmdrehenAnschluss { umdrehenAnschluss }
                            }
                Nothing -> error
                    "Unbekannte GeschwindigkeitVariante beim Hinzufügen einer Bahngeschwindigkeit!"
        Lego -> do
            (`Map.lookup` indexSeiten) <$> Gtk.get notebookGeschwindigkeit Gtk.notebookPage >>= \case
                (Just Pwm) -> do
                    geschwindigkeitsPin <- aktuellerPin geschwindigkeitAuswahl
                    fahrtrichtungsAnschluss <- aktuellerAnschluss pwmFahrtrichtungsAuswahl
                    pure
                        $ OBahngeschwindigkeit
                        $ ZugtypLego
                        $ GeschwindigkeitPwm
                            Bahngeschwindigkeit
                            { bgName
                            , bgFließend
                            , bgGeschwindigkeitsAnschlüsse =
                                  GeschwindigkeitsPin { geschwindigkeitsPin }
                            , bgFahrtrichtungsAnschluss =
                                  FahrtrichtungsAnschluss { fahrtrichtungsAnschluss }
                            }
                (Just KonstanteSpannung) -> do
                    fahrstromAuswahlWidgets <- readTVarIO tvarFahrstromAuswahlWidgets
                    fahrstromAnschlüsse <- mapM aktuellerAnschluss fahrstromAuswahlWidgets
                    fahrtrichtungsAnschluss
                        <- aktuellerAnschluss konstanteSpannungFahrtrichtungsAuswahl
                    pure
                        $ OBahngeschwindigkeit
                        $ ZugtypLego
                        $ GeschwindigkeitKonstanteSpannung
                            Bahngeschwindigkeit
                            { bgName
                            , bgFließend
                            , bgGeschwindigkeitsAnschlüsse =
                                  FahrstromAnschlüsse { fahrstromAnschlüsse }
                            , bgFahrtrichtungsAnschluss =
                                  FahrtrichtungsAnschluss { fahrtrichtungsAnschluss }
                            }
                Nothing -> error
                    "Unbekannte GeschwindigkeitVariante beim Hinzufügen einer Bahngeschwindigkeit!"
seiteErgebnis
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteStreckenabschnitt {nameAuswahl, stromAuswahl} = do
    stName <- aktuellerName nameAuswahl
    stFließend <- aktuellerFließendValue fließendAuswahl
    stromAnschluss <- aktuellerAnschluss stromAuswahl
    pure $ OStreckenabschnitt Streckenabschnitt { stName, stFließend, stromAnschluss }
seiteErgebnis
    fließendAuswahl
    zugtypAuswahl
    HinzufügenSeiteWeiche
    {nameAuswahl, märklinRichtungsAuswahl, legoRichtungsAuswahl, legoRichtungenAuswahl} = do
    name <- aktuellerName nameAuswahl
    weFließend <- aktuellerFließendValue fließendAuswahl
    aktuelleAuswahl zugtypAuswahl >>= \case
        Märklin -> do
            -- Nicht-Leerheit garantiert durch FortfahrenWennToggled
            wemRichtungsAnschlüsse <- fmap
                (NonEmpty.fromList . map fromJust . NonEmpty.filter isJust)
                $ forM märklinRichtungsAuswahl
                $ \(richtung, rcb, anschlussAuswahl)
                -> registrierterCheckButtonToggled rcb >>= \case
                    True -> Just . (\anschluss -> (richtung, anschluss))
                        <$> aktuellerAnschluss anschlussAuswahl
                    False -> pure Nothing
            pure
                $ OWeiche
                $ ZugtypMärklin
                    WeicheMärklin
                    { wemName = name
                    , wemFließend = weFließend
                    , wemRichtungsAnschlüsse
                    }
        Lego -> do
            welRichtungen <- aktuelleAuswahl legoRichtungenAuswahl
            welRichtungsPin <- aktuellerPin legoRichtungsAuswahl
            pure
                $ OWeiche
                $ ZugtypLego
                    WeicheLego
                    { welName = name
                    , welFließend = weFließend
                    , welRichtungen
                    , welRichtungsPin
                    }
seiteErgebnis
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteKupplung {nameAuswahl, kupplungsAuswahl} = do
    kuName <- aktuellerName nameAuswahl
    kuFließend <- aktuellerFließendValue fließendAuswahl
    kupplungsAnschluss <- aktuellerAnschluss kupplungsAuswahl
    pure $ OKupplung Kupplung { kuName, kuFließend, kupplungsAnschluss }
seiteErgebnis
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteKontakt {nameAuswahl, kontaktAuswahl} = do
    koName <- aktuellerName nameAuswahl
    koFließend <- aktuellerFließendValue fließendAuswahl
    kontaktAnschluss <- aktuellerAnschluss kontaktAuswahl
    pure $ OKontakt Kontakt { koName, koFließend, kontaktAnschluss }
seiteErgebnis _fließendAuswahl zugtypAuswahl HinzufügenSeiteWegstrecke {nameAuswahl} = do
    statusVar <- erhalteStatusVar :: m StatusVarGui
    aktuellerStatus <- liftIO $ atomically $ readStatusVar statusVar
    wsName <- aktuellerName nameAuswahl
    let gewählteWegstrecke
            :: ( MonadIO m
               , ZugtypKlasse z
               , WegstreckenElement (GeschwindigkeitEither BGWidgets z)
               , WegstreckenElement (WEWidgets z)
               , MitAuswahlWidget (WegstreckeCheckButton (CheckButtonAuswahl (WEWidgets z))) Richtung
               )
            => m (Wegstrecke z)
        gewählteWegstrecke = do
            wsBahngeschwindigkeiten <- foldM anhängenWennToggled Set.empty
                $ catMaybes
                $ map vonZugtypEither $bahngeschwindigkeiten aktuellerStatus
            wsStreckenabschnitte
                <- foldM anhängenWennToggled Set.empty $streckenabschnitte aktuellerStatus
            wsWeichenRichtungen <- foldM weichenRichtungAnhängenWennToggled Set.empty
                $ catMaybes
                $ map vonZugtypEither $weichen aktuellerStatus
            wsKupplungen <- foldM anhängenWennToggled Set.empty $kupplungen aktuellerStatus
            wsKontakte <- foldM anhängenWennToggled Set.empty $kontakte aktuellerStatus
            pure
                Wegstrecke
                { wsName
                , wsBahngeschwindigkeiten
                , wsStreckenabschnitte
                , wsWeichenRichtungen
                , wsKupplungen
                , wsKontakte
                }
        anhängenWennToggled :: (WidgetsTyp a, Ord (ObjektTyp a), WegstreckenElement a, MonadIO m)
                             => Set (ObjektTyp a)
                             -> a
                             -> m (Set (ObjektTyp a))
        anhängenWennToggled acc a = widgetHinzufügenToggled (checkButtonWegstrecke a) >>= \case
            True -> pure $ Set.insert (zuObjektTyp a) acc
            False -> pure acc
        weichenRichtungAnhängenWennToggled
            :: ( WegstreckenElement (WEWidgets z)
               , MonadIO m
               , MitAuswahlWidget (WegstreckeCheckButton (CheckButtonAuswahl (WEWidgets z))) Richtung
               )
            => Set (Weiche z, Richtung)
            -> WEWidgets z
            -> m (Set (Weiche z, Richtung))
        weichenRichtungAnhängenWennToggled acc weiche = do
            let widgetHinzufügen = checkButtonWegstrecke weiche
            toggled <- widgetHinzufügenToggled widgetHinzufügen
            if toggled
                then do
                    richtung <- widgetHinzufügenAktuelleAuswahl widgetHinzufügen
                    pure $ Set.insert (zuObjektTyp weiche, richtung) acc
                else pure acc
    -- Explizite Zugtyp-Auswahl notwendig für den Typ-Checker
    -- Dieser kann sonst Typ-Klassen nicht überprüfen
    aktuelleAuswahl zugtypAuswahl >>= \case
        Märklin -> OWegstrecke . ZugtypMärklin <$> gewählteWegstrecke
        Lego -> OWegstrecke . ZugtypLego <$> gewählteWegstrecke
seiteErgebnis
    _fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeitePlan {nameAuswahl, tvarAktionen, checkButtonDauerschleife} = liftIO $ do
    plName <- aktuellerName nameAuswahl
    aktionenWarteschlange <- readTVarIO tvarAktionen
    Gtk.get checkButtonDauerschleife Gtk.toggleButtonActive >>= pure . OPlan . \case
        True -> let plan =
                        Plan
                        { plName
                        , plAktionen = NonEmpty.fromList
                              $ toList
                              $ anhängen (AktionAusführen plan)
                              $ fstOf3 <$> aktionenWarteschlange
                        }
                in plan
        False -> Plan
            { plName
            , plAktionen = NonEmpty.fromList $ toList $ fstOf3 <$> aktionenWarteschlange
            }

-- small little helper functions
fstOf3 :: (a, b, c) -> a
fstOf3 (a, _b, _c) = a

sndOf3 :: (a, b, c) -> b
sndOf3 (_a, b, _c) = b

-- | Setze den aktuellen Wert einer 'HinzufügenSeite'.
--
-- Der Rückgabewert gibt an, ob das Objekt zur Seite gepasst hat ('True').
-- Ansonsten ('False') hat diese Aktion keinen Auswirkungen.
setzeSeite :: forall r m.
           (StatusVarGuiReader r m, SpracheGuiReader r m, MonadIO m)
           => FließendAuswahlWidget
           -> AuswahlWidget Zugtyp
           -> HinzufügenSeite
           -> Objekt
           -> m Bool
setzeSeite
    fließendAuswahl
    zugtypAuswahl
    HinzufügenSeiteBahngeschwindigkeit
    { nameAuswahl
    , maybeTVarSprache
    , notebookGeschwindigkeit
    , indexSeiten
    , geschwindigkeitAuswahl
    , vBoxFahrstrom
    , tvarFahrstromAuswahlWidgets
    , umdrehenAuswahl
    , pwmFahrtrichtungsAuswahl
    , konstanteSpannungFahrtrichtungsAuswahl}
    (OBahngeschwindigkeit bg) = do
    setzeName nameAuswahl $ erhalteName bg
    setzeFließendValue fließendAuswahl $ fließend bg
    setzeAuswahl zugtypAuswahl $ zugtyp bg
    liftIO $ forM (Map.toList indexSeiten) $ \(index, variante) -> when
        (variante == ausZugtypEither geschwindigkeitVariante bg)
        $ Gtk.set notebookGeschwindigkeit [Gtk.notebookPage := index]
    let erstelleFahrstromAnschluss
            :: AnschlussEither -> Int -> m (AnschlussAuswahlWidget 'InterruptPinEgal)
        erstelleFahrstromAnschluss anschluss n = do
            anschlussAuswahlWidget <- boxPackWidgetNewDefault vBoxFahrstrom
                $ anschlussAuswahlNew maybeTVarSprache
                $ Language.fahrstrom <:> n
            setzeAnschluss anschlussAuswahlWidget anschluss
            pure anschlussAuswahlWidget
    case bg of
        (ZugtypMärklin
             (GeschwindigkeitPwm
                  Bahngeschwindigkeit
                  { bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin}
                  , bgFahrtrichtungsAnschluss = KeinExpliziterAnschluss}))
            -> setzePin geschwindigkeitAuswahl geschwindigkeitsPin
        (ZugtypMärklin
             (GeschwindigkeitKonstanteSpannung
                  Bahngeschwindigkeit
                  { bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {fahrstromAnschlüsse}
                  , bgFahrtrichtungsAnschluss = UmdrehenAnschluss {umdrehenAnschluss}})) -> do
            auswahlFahrstromAnschlüsse <- NonEmpty.fromList
                <$> foldM (\widgets anschluss -> fmap (: widgets)
                           $ erstelleFahrstromAnschluss anschluss
                           $ succ
                           $ length widgets) [] fahrstromAnschlüsse
            liftIO $ do
                alteAuswahlWidgets <- atomically
                    $ swapTVar tvarFahrstromAuswahlWidgets auswahlFahrstromAnschlüsse
                forM_ alteAuswahlWidgets $ (>>= Gtk.widgetDestroy) . erhalteWidget
                setzeAnschluss umdrehenAuswahl umdrehenAnschluss
        (ZugtypLego
             (GeschwindigkeitPwm
                  Bahngeschwindigkeit
                  { bgGeschwindigkeitsAnschlüsse = GeschwindigkeitsPin {geschwindigkeitsPin}
                  , bgFahrtrichtungsAnschluss = FahrtrichtungsAnschluss {fahrtrichtungsAnschluss}}))
            -> do
                setzePin geschwindigkeitAuswahl geschwindigkeitsPin
                setzeAnschluss pwmFahrtrichtungsAuswahl fahrtrichtungsAnschluss
        (ZugtypLego
             (GeschwindigkeitKonstanteSpannung
                  Bahngeschwindigkeit
                  { bgGeschwindigkeitsAnschlüsse = FahrstromAnschlüsse {fahrstromAnschlüsse}
                  , bgFahrtrichtungsAnschluss = FahrtrichtungsAnschluss {fahrtrichtungsAnschluss}}))
            -> do
                auswahlFahrstromAnschlüsse <- NonEmpty.fromList
                    <$> foldM (\widgets anschluss -> fmap (: widgets)
                               $ erstelleFahrstromAnschluss anschluss
                               $ succ
                               $ length widgets) [] fahrstromAnschlüsse
                liftIO $ do
                    alteAuswahlWidgets <- atomically
                        $ swapTVar tvarFahrstromAuswahlWidgets auswahlFahrstromAnschlüsse
                    forM_ alteAuswahlWidgets $ (>>= Gtk.widgetDestroy) . erhalteWidget
                    setzeAnschluss konstanteSpannungFahrtrichtungsAuswahl fahrtrichtungsAnschluss
    pure True
setzeSeite
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteStreckenabschnitt {nameAuswahl, stromAuswahl}
    (OStreckenabschnitt Streckenabschnitt {stName, stFließend, stromAnschluss}) = do
    setzeName nameAuswahl stName
    setzeFließendValue fließendAuswahl stFließend
    setzeAnschluss stromAuswahl stromAnschluss
    pure True
setzeSeite
    fließendAuswahl
    zugtypAuswahl
    HinzufügenSeiteWeiche
    {nameAuswahl, märklinRichtungsAuswahl, legoRichtungsAuswahl, legoRichtungenAuswahl}
    (OWeiche we) = do
    setzeName nameAuswahl $ erhalteName we
    setzeFließendValue fließendAuswahl $ fließend we
    setzeAuswahl zugtypAuswahl $ zugtyp we
    liftIO $ case we of
        (ZugtypMärklin WeicheMärklin {wemRichtungsAnschlüsse})
            -> forM_ märklinRichtungsAuswahl $ \(richtung, rcb, anschlussAuswahl)
            -> case List.lookup richtung $ NonEmpty.toList wemRichtungsAnschlüsse of
                (Just anschluss) -> do
                    registrierterCheckButtonSetToggled rcb True
                    setzeAnschluss anschlussAuswahl anschluss
                Nothing -> registrierterCheckButtonSetToggled rcb False
        (ZugtypLego WeicheLego {welRichtungen, welRichtungsPin}) -> do
            setzeAuswahl legoRichtungenAuswahl welRichtungen
            setzePin legoRichtungsAuswahl welRichtungsPin
    pure True
setzeSeite
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteKupplung {nameAuswahl, kupplungsAuswahl}
    (OKupplung Kupplung {kuName, kuFließend, kupplungsAnschluss}) = do
    setzeName nameAuswahl kuName
    setzeFließendValue fließendAuswahl kuFließend
    setzeAnschluss kupplungsAuswahl kupplungsAnschluss
    pure True
setzeSeite
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteKontakt {nameAuswahl, kontaktAuswahl}
    (OKontakt Kontakt {koName, koFließend, kontaktAnschluss}) = do
    setzeName nameAuswahl koName
    setzeFließendValue fließendAuswahl koFließend
    setzeAnschluss kontaktAuswahl kontaktAnschluss
    pure True
setzeSeite
    _fließendAuswahl
    zugtypAuswahl
    HinzufügenSeiteWegstrecke {nameAuswahl}
    (OWegstrecke ws) = do
    setzeName nameAuswahl $ erhalteName ws
    setzeAuswahl zugtypAuswahl $ zugtyp ws
    statusVar <- erhalteStatusVar :: m StatusVarGui
    aktuellerStatus <- liftIO $ atomically $ readStatusVar statusVar
    forM_ (bahngeschwindigkeiten aktuellerStatus) $ \bgWidgets -> widgetHinzufügenSetToggled
        (checkButtonWegstrecke bgWidgets)
        $ elem (zuObjektTyp bgWidgets)
        $ enthalteneBahngeschwindigkeiten ws
    forM_ (streckenabschnitte aktuellerStatus) $ \stWidgets -> widgetHinzufügenSetToggled
        (checkButtonWegstrecke stWidgets)
        $ elem (zuObjektTyp stWidgets)
        $ enthalteneStreckenabschnitte ws
    let getWeichenRichtungen :: ZugtypEither Wegstrecke -> Map (ZugtypEither Weiche) Richtung
        getWeichenRichtungen (ZugtypMärklin wsMärklin) =
            foldl
                (\acc (weiche, richtung) -> Map.insert (ZugtypMärklin weiche) richtung acc)
                Map.empty
            $ wsWeichenRichtungen wsMärklin
        getWeichenRichtungen (ZugtypLego wsLego) =
            foldl
                (\acc (weiche, richtung) -> Map.insert (ZugtypLego weiche) richtung acc)
                Map.empty
            $ wsWeichenRichtungen wsLego
    forM_ (weichen aktuellerStatus)
        $ \weWidgets -> case Map.lookup (zuObjektTyp weWidgets) $ getWeichenRichtungen ws of
            (Just richtung) -> do
                let widgetHinzufügen = checkButtonWegstrecke weWidgets
                widgetHinzufügenSetToggled widgetHinzufügen True
                widgetHinzufügenSetzeAuswahl widgetHinzufügen richtung
            Nothing -> widgetHinzufügenSetToggled (checkButtonWegstrecke weWidgets) False
    forM_ (kupplungen aktuellerStatus) $ \kuWidgets -> widgetHinzufügenSetToggled
        (checkButtonWegstrecke kuWidgets)
        $ elem (zuObjektTyp kuWidgets)
        $ enthalteneKupplungen ws
    forM_ (kontakte aktuellerStatus) $ \koWidgets -> widgetHinzufügenSetToggled
        (checkButtonWegstrecke koWidgets)
        $ elem (zuObjektTyp koWidgets)
        $ enthalteneKontakte ws
    pure True
setzeSeite
    _fließendAuswahl
    _zugtypAuswahl
    seite@HinzufügenSeitePlan {nameAuswahl, tvarAktionen, checkButtonDauerschleife}
    (OPlan Plan {plName, plAktionen}) = do
    setzeName nameAuswahl plName
    let führtSelbstAus :: Aktion -> Bool
        führtSelbstAus (AktionAusführen Plan {plName = name}) = plName == name
        führtSelbstAus _aktion = False
        aktionen :: [Aktion]
        aktionen = NonEmpty.takeWhile (not . führtSelbstAus) plAktionen
    liftIO $ do
        alteAktionsWidgets <- fmap (fmap sndOf3) $ atomically $ swapTVar tvarAktionen leer
        mapM_ Gtk.widgetDestroy alteAktionsWidgets
        Gtk.set
            checkButtonDauerschleife
            [Gtk.toggleButtonActive := (length aktionen /= length plAktionen)]
    forM_ aktionen $ aktionHinzufügen seite
    pure True
setzeSeite _fließendAuswahl _zugtypAuswahl _hinzufügenSeite _objekt = pure False

-- | Erzeuge eine Seite zum hinzufügen einer 'Bahngeschwindigkeit'.
hinzufügenBahngeschwindigkeitNew
    :: (SpracheGuiReader r m, MonadFix m, MonadIO m)
    => AuswahlWidget Zugtyp
    -> Maybe TVarSprachewechselAktionen
    -> m HinzufügenSeite
hinzufügenBahngeschwindigkeitNew auswahlZugtyp maybeTVarSprache = mdo
    reader <- ask
    vBox <- liftIO $ widgetShowNew $ Gtk.boxNew Gtk.OrientationVertical 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVarSprache
    notebookGeschwindigkeit
        <- liftIO $ boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    (vBoxPwm, indexPwm) <- notebookAppendPageNew
        notebookGeschwindigkeit
        maybeTVarSprache
        Language.geschwindigkeitPwm
        $ liftIO
        $ Gtk.boxNew Gtk.OrientationVertical 0
    geschwindigkeitAuswahl <- boxPackWidgetNewDefault vBoxPwm
        $ pinAuswahlNew maybeTVarSprache Language.geschwindigkeit
    (vBoxKonstanteSpannung, indexKonstanteSpannung) <- notebookAppendPageNew
        notebookGeschwindigkeit
        maybeTVarSprache
        Language.geschwindigkeitKonstanteSpannung
        $ liftIO
        $ Gtk.boxNew Gtk.OrientationVertical 0
    legoVBoxPwm <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    pwmFahrtrichtungsAuswahl <- boxPackWidgetNewDefault legoVBoxPwm
        $ anschlussAuswahlNew maybeTVarSprache Language.fahrtrichtung
    boxPackWidgetNew vBoxPwm PackGrow paddingDefault positionDefault
        $ zugtypSpezifischNew [(Lego, legoVBoxPwm)] auswahlZugtyp
    let indexSeiten = Map.fromList [(indexPwm, Pwm), (indexKonstanteSpannung, KonstanteSpannung)]
    (vBoxFahrstrom, tvarFahrstromAuswahlWidgets, functionBoxFahrstrom) <- liftIO $ do
        functionBoxFahrstromIO <- boxPackWidgetNewDefault vBoxKonstanteSpannung
            $ Gtk.boxNew Gtk.OrientationHorizontal 0
        vBoxFahrstromIO
            <- boxPackWidgetNew vBoxKonstanteSpannung PackGrow paddingDefault positionDefault
            $ scrollbaresWidgetNew
            $ Gtk.boxNew Gtk.OrientationVertical 0
        tvarFahrstromAuswahlWidgetsIO <- newTVarIO $ [fahrstromAuswahlWidget1]
        pure (vBoxFahrstromIO, tvarFahrstromAuswahlWidgetsIO, functionBoxFahrstromIO)
    boxPackWidgetNewDefault functionBoxFahrstrom
        $ buttonNewWithEventLabel maybeTVarSprache (const "+")
        $ do
            fahrstromAuswahlWidgets <- readTVarIO tvarFahrstromAuswahlWidgets
            fahrstromAuswahlWidgetN <- boxPackWidgetNewDefault vBoxFahrstrom
                $ flip runReaderT reader
                $ anschlussAuswahlNew maybeTVarSprache
                $ Language.fahrstrom <:> (succ $ length fahrstromAuswahlWidgets)
            atomically $ modifyTVar tvarFahrstromAuswahlWidgets $ (<> [fahrstromAuswahlWidgetN])
    boxPackWidgetNewDefault functionBoxFahrstrom
        $ buttonNewWithEventLabel maybeTVarSprache (const "-")
        $ do
            fahrstromAuswahlWidgets <- readTVarIO tvarFahrstromAuswahlWidgets
            when (length fahrstromAuswahlWidgets > 1) $ do
                Gtk.widgetDestroy =<< (erhalteWidget $ NonEmpty.last fahrstromAuswahlWidgets)
                atomically
                    $ modifyTVar tvarFahrstromAuswahlWidgets
                    $ NonEmpty.fromList . NonEmpty.init
    fahrstromAuswahlWidget1 <- boxPackWidgetNewDefault vBoxFahrstrom
        $ anschlussAuswahlNew maybeTVarSprache
        $ Language.fahrstrom <:> (1 :: Word8)
    märklinVBoxKonstanteSpannung <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    umdrehenAuswahl <- boxPackWidgetNewDefault märklinVBoxKonstanteSpannung
        $ anschlussAuswahlNew maybeTVarSprache Language.umdrehen
    legoVBoxKonstanteSpannung <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    konstanteSpannungFahrtrichtungsAuswahl <- boxPackWidgetNewDefault legoVBoxKonstanteSpannung
        $ anschlussAuswahlNew maybeTVarSprache Language.fahrtrichtung
    boxPackWidgetNewDefault vBoxKonstanteSpannung
        $ zugtypSpezifischNew
            [(Märklin, märklinVBoxKonstanteSpannung), (Lego, legoVBoxKonstanteSpannung)]
            auswahlZugtyp
    pure
        HinzufügenSeiteBahngeschwindigkeit
        { vBox
        , maybeTVarSprache
        , nameAuswahl
          -- Märklin
        , notebookGeschwindigkeit
        , indexSeiten
        , geschwindigkeitAuswahl
        , vBoxFahrstrom
        , tvarFahrstromAuswahlWidgets
        , umdrehenAuswahl
          -- Lego
        , pwmFahrtrichtungsAuswahl
        , konstanteSpannungFahrtrichtungsAuswahl
        }

-- | Erzeuge eine Seite zum hinzufügen eines 'Streckenabschnitt'.
hinzufügenStreckenabschnittNew
    :: (SpracheGuiReader r m, MonadIO m) => Maybe TVarSprachewechselAktionen -> m HinzufügenSeite
hinzufügenStreckenabschnittNew maybeTVar = do
    vBox <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    stromAuswahl <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.strom
    pure HinzufügenSeiteStreckenabschnitt { vBox, nameAuswahl, stromAuswahl }

-- | Erzeuge eine Seite zum hinzufügen einer 'Weiche'.
hinzufügenWeicheNew :: (SpracheGuiReader r m, MonadIO m)
                     => AuswahlWidget Zugtyp
                     -> Maybe TVarSprachewechselAktionen
                     -> m HinzufügenSeite
hinzufügenWeicheNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    -- Märklin
    märklinFortfahrenWennToggled <- fortfahrenWennToggledNew maybeTVar Language.hinzufügen
        $ anzeige <$> unterstützteRichtungen
    märklinButtonHinzufügen <- erhalteButton märklinFortfahrenWennToggled
    let richtungenCheckButtons =
            NonEmpty.zip unterstützteRichtungen $ checkButtons märklinFortfahrenWennToggled
    märklinGridScrollbar <- liftIO $ scrollbaresWidgetNew Gtk.gridNew
    märklinGrid <- erhalteGrid märklinGridScrollbar
    märklinWidget <- erhalteWidget märklinGridScrollbar
    let foldFn :: (SpracheGuiReader r m, MonadIO m)
               => [(Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget 'InterruptPinEgal)]
               -> (Richtung, RegistrierterCheckButton)
               -> m [(Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget 'InterruptPinEgal)]
        foldFn acc (richtung, registrierterCheckButton) = do
            maybeTop <- case sndOf3 <$> listToMaybe acc of
                (Just top) -> Just <$> erhalteWidget top
                Nothing -> pure Nothing
            registrierterCheckButtonWidget <- erhalteWidget registrierterCheckButton
            liftIO
                $ Gtk.gridAttachNextTo
                    märklinGrid
                    registrierterCheckButtonWidget
                    maybeTop
                    Gtk.PositionTypeBottom
                    1
                    1
            anschlussAuswahlWidget
                <- widgetShowNew $ anschlussAuswahlNew maybeTVar $ anzeige richtung
            widget <- erhalteWidget anschlussAuswahlWidget
            liftIO
                $ Gtk.gridAttachNextTo
                    märklinGrid
                    widget
                    (Just registrierterCheckButtonWidget)
                    Gtk.PositionTypeRight
                    1
                    1
            pure $ (richtung, registrierterCheckButton, anschlussAuswahlWidget) : acc
    märklinRichtungsAuswahl
        <- NonEmpty.fromList . reverse <$> foldM foldFn [] richtungenCheckButtons
    -- Lego
    legoButtonHinzufügen <- liftIO Gtk.buttonNew
    legoVBox <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    legoWidget <- erhalteWidget legoVBox
    verwendeSpracheGui maybeTVar $ \sprache
        -> Gtk.set legoButtonHinzufügen [Gtk.buttonLabel := Language.hinzufügen sprache]
    legoRichtungsAuswahl
        <- boxPackWidgetNewDefault legoVBox $ pinAuswahlNew maybeTVar Language.richtungen
    legoRichtungenAuswahl <- boxPackWidgetNewDefault legoVBox
        $ auswahlComboBoxNew
            (NonEmpty.fromList
             $ NonEmpty.filter (uncurry (/=))
             $ (,) <$> unterstützteRichtungen <*> unterstützteRichtungen)
            maybeTVar
            Language.richtungen
    -- ZugtypSpezifisch
    buttonHinzufügenWeiche <- zugtypSpezifischButtonNew
        [(Märklin, märklinButtonHinzufügen), (Lego, legoButtonHinzufügen)]
        auswahlZugtyp
    boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
        $ zugtypSpezifischNew [(Märklin, märklinWidget), (Lego, legoWidget)] auswahlZugtyp
    pure
        HinzufügenSeiteWeiche
        { vBox
        , nameAuswahl
        , buttonHinzufügenWeiche
          -- Märklin
        , märklinRichtungsAuswahl
          -- Lego
        , legoRichtungsAuswahl
        , legoRichtungenAuswahl
        }

-- | Erzeuge eine Seite zum hinzufügen einer 'Kupplung'.
hinzufügenKupplungNew
    :: (SpracheGuiReader r m, MonadIO m) => Maybe TVarSprachewechselAktionen -> m HinzufügenSeite
hinzufügenKupplungNew maybeTVar = do
    vBox <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    kupplungsAuswahl
        <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.kupplung
    pure HinzufügenSeiteKupplung { vBox, nameAuswahl, kupplungsAuswahl }

-- | Erzeuge eine Seite zum hinzufügen eines 'Kontakt's.
hinzufügenKontaktNew :: (SpracheGuiReader r m, MonadIO m, MonadFail m)
                      => Maybe TVarSprachewechselAktionen
                      -> m HinzufügenSeite
hinzufügenKontaktNew maybeTVar = do
    vBox <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    kontaktAuswahl <- boxPackWidgetNewDefault vBox
        $ anschlussAuswahlInterruptPinNew maybeTVar Language.kontakt
    pure HinzufügenSeiteKontakt { vBox, nameAuswahl, kontaktAuswahl }

-- | Erzeuge eine Seite zum hinzufügen einer 'Wegstrecke'.
hinzufügenWegstreckeNew :: (SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
                         => AuswahlWidget Zugtyp
                         -> Maybe TVarSprachewechselAktionen
                         -> m HinzufügenSeite
hinzufügenWegstreckeNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    DynamischeWidgets
        { dynBGWidgetsBoxen = BGWidgetsBoxen
              { vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
              , vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego}
        , dynSTWidgetsBoxen = STWidgetsBoxen {vBoxHinzufügenWegstreckeStreckenabschnitte}
        , dynWEWidgetsBoxen = WEWidgetsBoxen
              {vBoxHinzufügenWegstreckeWeichenMärklin, vBoxHinzufügenWegstreckeWeichenLego}
        , dynKUWidgetsBoxen = KUWidgetsBoxen {vBoxHinzufügenWegstreckeKupplungen}
        , dynKOWidgetsBoxen = KOWidgetsBoxen {vBoxHinzufügenWegstreckeKontakte}
        , dynFortfahrenWennToggledWegstrecke} <- erhalteDynamischeWidgets
    notebook
        <- liftIO $ boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    widgetBahngeschwindigkeitenMärklin
        <- erhalteWidget vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
    widgetBahngeschwindigkeitenLego
        <- erhalteWidget vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
    notebookAppendPageNew notebook maybeTVar Language.bahngeschwindigkeiten
        $ zugtypSpezifischNew
            [ (Märklin, widgetBahngeschwindigkeitenMärklin)
            , (Lego, widgetBahngeschwindigkeitenLego)]
            auswahlZugtyp
    notebookAppendPageNew notebook maybeTVar Language.streckenabschnitte
        $ pure vBoxHinzufügenWegstreckeStreckenabschnitte
    widgetWeichenMärklin <- erhalteWidget vBoxHinzufügenWegstreckeWeichenMärklin
    widgetWeichenLego <- erhalteWidget vBoxHinzufügenWegstreckeWeichenLego
    notebookAppendPageNew notebook maybeTVar Language.weichen
        $ zugtypSpezifischNew
            [(Märklin, widgetWeichenMärklin), (Lego, widgetWeichenLego)]
            auswahlZugtyp
    notebookAppendPageNew notebook maybeTVar Language.kupplungen
        $ pure vBoxHinzufügenWegstreckeKupplungen
    notebookAppendPageNew notebook maybeTVar Language.kupplungen
        $ pure vBoxHinzufügenWegstreckeKontakte
    pure
        HinzufügenSeiteWegstrecke
        { vBox
        , nameAuswahl
        , buttonHinzufügenWegstrecke = dynFortfahrenWennToggledWegstrecke
        }

-- | Erzeuge eine Seite zum hinzufügen eines 'Plans'.
hinzufügenPlanNew
    :: (MitWindow p, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadFix m, MonadIO m)
    => p
    -> AuswahlWidget Zugtyp
    -> Maybe TVarSprachewechselAktionen
    -> m HinzufügenSeite
hinzufügenPlanNew parent auswahlZugtyp maybeTVar = mdo
    parentWindow <- erhalteWindow parent
    vBox <- liftIO $ Gtk.boxNew Gtk.OrientationVertical 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    vBoxAktionenWidgets <- liftIO
        $ boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.boxNew Gtk.OrientationVertical 0
    DynamischeWidgets
        { dynBGWidgetsBoxen = BGWidgetsBoxen
              { vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
              , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
              , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
              , vBoxHinzufügenPlanBahngeschwindigkeitenLego
              , vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
              , vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung}
        , dynSTWidgetsBoxen = STWidgetsBoxen {vBoxHinzufügenPlanStreckenabschnitte}
        , dynWEWidgetsBoxen = WEWidgetsBoxen
              { vBoxHinzufügenPlanWeichenGeradeMärklin
              , vBoxHinzufügenPlanWeichenKurveMärklin
              , vBoxHinzufügenPlanWeichenLinksMärklin
              , vBoxHinzufügenPlanWeichenRechtsMärklin
              , vBoxHinzufügenPlanWeichenGeradeLego
              , vBoxHinzufügenPlanWeichenKurveLego
              , vBoxHinzufügenPlanWeichenLinksLego
              , vBoxHinzufügenPlanWeichenRechtsLego}
        , dynKUWidgetsBoxen = KUWidgetsBoxen {vBoxHinzufügenPlanKupplungen}
        , dynKOWidgetsBoxen = KOWidgetsBoxen {vBoxHinzufügenPlanKontakte}
        , dynWSWidgetsBoxen = WSWidgetsBoxen
              { vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
              , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
              , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
              , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
              , vBoxHinzufügenPlanWegstreckenKupplungMärklin
              , vBoxHinzufügenPlanWegstreckenKontaktMärklin
              , vBoxHinzufügenPlanWegstreckenMärklin
              , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
              , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm
              , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
              , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
              , vBoxHinzufügenPlanWegstreckenKupplungLego
              , vBoxHinzufügenPlanWegstreckenKontaktLego
              , vBoxHinzufügenPlanWegstreckenLego}
        , dynPLWidgetsBoxen = PLWidgetsBoxen {vBoxHinzufügenPlanPläne}
        , dynTMVarPlanObjekt} <- erhalteDynamischeWidgets
    spracheGui <- erhalteSpracheGui
    hBoxWartezeit <- liftIO
        $ boxPackWidgetNewDefault vBoxAktionenWidgets
        $ Gtk.boxNew Gtk.OrientationHorizontal 0
    boxPackWidgetNewDefault hBoxWartezeit $ buttonNewWithEventLabel maybeTVar Language.warten $ do
        wert <- floor <$> Gtk.get spinButtonWartezeit Gtk.spinButtonValue
        einheit <- aktuelleAuswahl comboBoxWartezeit
        void $ flip runReaderT spracheGui $ do
            aktionHinzufügen seite $ Warten $ wert & case einheit of
                "µs" -> MikroSekunden
                "ms" -> MilliSekunden
                "s" -> Sekunden
                "min" -> Minuten
                "h" -> Stunden
                "d" -> Tage
                zeiteinheit
                    -> error $ "Unbekannte Zeiteinheit für Wartezeit gewählt: " ++ zeiteinheit
    spinButtonWartezeit
        <- liftIO $ boxPackWidgetNewDefault hBoxWartezeit $ Gtk.spinButtonNewWithRange 1 999 1
    comboBoxWartezeit <- boxPackWidgetNewDefault hBoxWartezeit
        $ auswahlComboBoxNamedNew ["µs", "ms", "s", "min", "h", "d"] maybeTVar (const Text.empty)
        $ const . Text.pack
    ( windowObjektAuswahl
        , showBG
        , showST
        , showGerade
        , showKurve
        , showLinks
        , showRechts
        , showKU
        , showKO
        , showWS
        , showPL
        ) <- liftIO $ do
        windowObjektAuswahlIO <- Gtk.windowNew Gtk.WindowTypeToplevel
        Gtk.set
            windowObjektAuswahlIO
            [ Gtk.windowTransientFor := parentWindow
            , Gtk.windowModal := True
            , Gtk.windowDefaultHeight := 400
            , Gtk.windowDefaultWidth := 300]
        Gtk.onWidgetDeleteEvent windowObjektAuswahlIO $ \_event -> do
            atomically $ putTMVar dynTMVarPlanObjekt Nothing
            pure True
        vBoxObjektAuswahl
            <- containerAddWidgetNew windowObjektAuswahlIO $ Gtk.boxNew Gtk.OrientationVertical 0
        ztBahngeschwindigkeiten
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanBahngeschwindigkeitenMärklin)
                , (Lego, Right vBoxHinzufügenPlanBahngeschwindigkeitenLego)]
                auswahlZugtyp
        ztBahngeschwindigkeitenPwm
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm)
                , (Lego, Right vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm)]
                auswahlZugtyp
        ztBahngeschwindigkeitenKonstanteSpannung
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ ( Märklin
                      , Left vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
                      )
                , (Lego, Right vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung)]
                auswahlZugtyp
        boxPack
            vBoxObjektAuswahl
            vBoxHinzufügenPlanStreckenabschnitte
            PackGrow
            paddingDefault
            positionDefault
        ztWeichenGerade
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWeichenGeradeMärklin)
                , (Lego, Right vBoxHinzufügenPlanWeichenGeradeLego)]
                auswahlZugtyp
        ztWeichenKurve
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWeichenKurveMärklin)
                , (Lego, Right vBoxHinzufügenPlanWeichenKurveLego)]
                auswahlZugtyp
        ztWeichenLinks
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWeichenLinksMärklin)
                , (Lego, Right vBoxHinzufügenPlanWeichenLinksLego)]
                auswahlZugtyp
        ztWeichenRechts
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWeichenRechtsMärklin)
                , (Lego, Right vBoxHinzufügenPlanWeichenRechtsLego)]
                auswahlZugtyp
        boxPack
            vBoxObjektAuswahl
            vBoxHinzufügenPlanKupplungen
            PackGrow
            paddingDefault
            positionDefault
        boxPack
            vBoxObjektAuswahl
            vBoxHinzufügenPlanKontakte
            PackGrow
            paddingDefault
            positionDefault
        ztWegstreckenBG
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin)
                , (Lego, Right vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego)]
                auswahlZugtyp
        ztWegstreckenBGPwm
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm)
                , (Lego, Right vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm)]
                auswahlZugtyp
        ztWegstreckenBGKonstanteSpannung
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ ( Märklin
                      , Left
                        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
                      )
                , ( Lego
                      , Right
                        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
                      )]
                auswahlZugtyp
        ztWegstreckenST
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin)
                , (Lego, Right vBoxHinzufügenPlanWegstreckenStreckenabschnittLego)]
                auswahlZugtyp
        ztWegstreckenKU
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWegstreckenKupplungLego)
                , (Lego, Right vBoxHinzufügenPlanWegstreckenKupplungMärklin)]
                auswahlZugtyp
        ztWegstreckenKO
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWegstreckenKontaktLego)
                , (Lego, Right vBoxHinzufügenPlanWegstreckenKontaktMärklin)]
                auswahlZugtyp
        ztWegstreckenWS
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, Left vBoxHinzufügenPlanWegstreckenMärklin)
                , (Lego, Right vBoxHinzufügenPlanWegstreckenLego)]
                auswahlZugtyp
        boxPack vBoxObjektAuswahl vBoxHinzufügenPlanPläne PackGrow paddingDefault positionDefault
        let hideExcept :: [Gtk.Widget] -> IO ()
            hideExcept shownWidgets =
                mapM_
                    (\konstruktor -> konstruktor >>= \widget
                     -> widgetShowIf (widget `elem` shownWidgets) widget)
                    ([ erhalteWidget ztBahngeschwindigkeiten
                     , erhalteWidget ztBahngeschwindigkeitenPwm
                     , erhalteWidget ztBahngeschwindigkeitenKonstanteSpannung
                     , erhalteWidget vBoxHinzufügenPlanStreckenabschnitte
                     , erhalteWidget ztWeichenGerade
                     , erhalteWidget ztWeichenKurve
                     , erhalteWidget ztWeichenLinks
                     , erhalteWidget ztWeichenRechts
                     , erhalteWidget vBoxHinzufügenPlanKupplungen
                     , erhalteWidget vBoxHinzufügenPlanKontakte
                     , erhalteWidget ztWegstreckenBG
                     , erhalteWidget ztWegstreckenBGPwm
                     , erhalteWidget ztWegstreckenBGKonstanteSpannung
                     , erhalteWidget ztWegstreckenST
                     , erhalteWidget ztWegstreckenKU
                     , erhalteWidget ztWegstreckenKO
                     , erhalteWidget ztWegstreckenWS
                     , erhalteWidget vBoxHinzufügenPlanPläne] :: [IO Gtk.Widget])
        widgetsBG
            <- sequence [erhalteWidget ztBahngeschwindigkeiten, erhalteWidget ztWegstreckenBG]
        widgetsBGPwm <- sequence
            [erhalteWidget ztBahngeschwindigkeitenPwm, erhalteWidget ztWegstreckenBGPwm]
        widgetsBGKonstanteSpannung <- sequence
            [ erhalteWidget ztBahngeschwindigkeitenKonstanteSpannung
            , erhalteWidget ztWegstreckenBGKonstanteSpannung]
        let showBGIO :: Maybe GeschwindigkeitVariante -> IO ()
            showBGIO Nothing = hideExcept widgetsBG
            showBGIO (Just Pwm) = hideExcept widgetsBGPwm
            showBGIO (Just KonstanteSpannung) = hideExcept widgetsBGKonstanteSpannung
        widgetsST <- sequence
            [erhalteWidget vBoxHinzufügenPlanStreckenabschnitte, erhalteWidget ztWegstreckenST]
        let showSTIO :: IO ()
            showSTIO = hideExcept widgetsST
        widgetWeicheGerade <- erhalteWidget ztWeichenGerade
        let showGeradeIO :: IO ()
            showGeradeIO = hideExcept [widgetWeicheGerade]
        widgetWeicheKurve <- erhalteWidget ztWeichenKurve
        let showKurveIO :: IO ()
            showKurveIO = hideExcept [widgetWeicheKurve]
        widgetWeicheLinks <- erhalteWidget ztWeichenLinks
        let showLinksIO :: IO ()
            showLinksIO = hideExcept [widgetWeicheLinks]
        widgetWeicheRechts <- erhalteWidget ztWeichenRechts
        let showRechtsIO :: IO ()
            showRechtsIO = hideExcept [widgetWeicheRechts]
        widgetsKU <- sequence
            [erhalteWidget vBoxHinzufügenPlanKupplungen, erhalteWidget ztWegstreckenKU]
        let showKUIO :: IO ()
            showKUIO = hideExcept widgetsKU
        widgetsKO
            <- sequence [erhalteWidget vBoxHinzufügenPlanKontakte, erhalteWidget ztWegstreckenKO]
        let showKOIO :: IO ()
            showKOIO = hideExcept widgetsKO
        widgetWS <- erhalteWidget ztWegstreckenWS
        let showWSIO :: IO ()
            showWSIO = hideExcept [widgetWS]
        widgetPL <- erhalteWidget vBoxHinzufügenPlanPläne
        let showPLIO :: IO ()
            showPLIO = hideExcept [widgetPL]
        pure
            ( windowObjektAuswahlIO
            , showBGIO
            , showSTIO
            , showGeradeIO
            , showKurveIO
            , showLinksIO
            , showRechtsIO
            , showKUIO
            , showKOIO
            , showWSIO
            , showPLIO
            )
    aktionBahngeschwindigkeitAuswahlPackNew
        vBoxAktionenWidgets
        windowObjektAuswahl
        auswahlZugtyp
        maybeTVar
        showBG
        $ aktionHinzufügen seite
    aktionStreckenabschnittAuswahlPackNew vBoxAktionenWidgets windowObjektAuswahl maybeTVar showST
        $ aktionHinzufügen seite
    aktionWeicheAuswahlPackNew
        vBoxAktionenWidgets
        windowObjektAuswahl
        maybeTVar
        [(Gerade, showGerade), (Kurve, showKurve), (Links, showLinks), (Rechts, showRechts)]
        $ aktionHinzufügen seite
    aktionKupplungAuswahlPackNew vBoxAktionenWidgets windowObjektAuswahl maybeTVar showKU
        $ aktionHinzufügen seite
    aktionKontaktAuswahlPackNew vBoxAktionenWidgets windowObjektAuswahl maybeTVar showKO
        $ aktionHinzufügen seite
    aktionWegstreckeAuswahlPackNew vBoxAktionenWidgets windowObjektAuswahl maybeTVar showWS
        $ aktionHinzufügen seite
    aktionPlanAuswahlPackNew vBoxAktionenWidgets windowObjektAuswahl maybeTVar showPL
        $ aktionHinzufügen seite
    (expanderAktionen, tvarAktionen, vBoxAktionen, tvarExpander, buttonHinzufügenPlan, resetBox)
        <- liftIO $ do
            expanderAktionenIO <- boxPackWidgetNewDefault vBoxAktionenWidgets
                $ flip leseSprache spracheGui
                $ \sprache -> Gtk.expanderNew $ Just $ Language.aktionen <:> (0 :: Word) $ sprache
            tvarAktionenIO <- newTVarIO leer
            vBoxAktionenIO <- containerAddWidgetNew expanderAktionenIO
                $ scrollbaresWidgetNew
                $ Gtk.boxNew Gtk.OrientationVertical 0
            tvarExpanderIO <- newTVarIO $ Just []
            buttonHinzufügenPlanIO <- Gtk.buttonNew
            Gtk.set buttonHinzufügenPlanIO [Gtk.widgetSensitive := False]
            resetBoxIO <- boxPackWidgetNewDefault vBoxAktionenWidgets
                $ Gtk.boxNew Gtk.OrientationHorizontal 0
            pure
                ( expanderAktionenIO
                , tvarAktionenIO
                , vBoxAktionenIO
                , tvarExpanderIO
                , buttonHinzufügenPlanIO
                , resetBoxIO
                )
    boxPackWidgetNew resetBox PackGrow paddingDefault positionDefault
        $ buttonNewWithEventLabel maybeTVar Language.rückgängig
        $ do
            aktuelleAktionen <- readTVarIO tvarAktionen
            neueAktionen <- case zeigeLetztes aktuelleAktionen of
                Leer -> pure leer
                Gefüllt (_aktion, widget, tvarSprachwechselAktionen) t -> do
                    mitContainerRemove vBoxAktionen widget
                    Gtk.widgetDestroy widget
                    atomically $ writeTVar tvarSprachwechselAktionen Nothing
                    pure t
            Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := not (null neueAktionen)]
            atomically $ writeTVar tvarAktionen neueAktionen
            flip runReaderT spracheGui $ aktualisiereExpanderText seite neueAktionen
    boxPackWidgetNew resetBox PackGrow paddingDefault positionDefault
        $ buttonNewWithEventLabel maybeTVar Language.zurücksetzen
        $ do
            aktuelleAktionen <- readTVarIO tvarAktionen
            forM_ aktuelleAktionen $ \(_aktion, widget, tvarSprachwechselAktionen) -> do
                mitContainerRemove vBoxAktionen widget
                Gtk.widgetDestroy widget
                atomically $ writeTVar tvarSprachwechselAktionen Nothing
            Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := False]
            atomically $ writeTVar tvarAktionen leer
            flip runReaderT spracheGui $ aktualisiereExpanderText seite leer
    checkButtonDauerschleife
        <- liftIO $ boxPackWidgetNewDefault vBoxAktionenWidgets Gtk.checkButtonNew
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.set checkButtonDauerschleife [Gtk.buttonLabel := Language.dauerschleife sprache]
        Gtk.set buttonHinzufügenPlan [Gtk.buttonLabel := Language.hinzufügen sprache]
    let seite =
            HinzufügenSeitePlan
            { vBox
            , nameAuswahl
            , tvarAktionen
            , expanderAktionen
            , tvarExpander
            , vBoxAktionen
            , checkButtonDauerschleife
            , buttonHinzufügenPlan
            }
    pure seite

aktualisiereExpanderText
    :: (SpracheGuiReader r m, MonadIO m) => HinzufügenSeite -> Warteschlange a -> m ()
aktualisiereExpanderText HinzufügenSeitePlan {tvarExpander, expanderAktionen} aktionen = do
    liftIO $ atomically $ writeTVar tvarExpander $ Just []
    verwendeSpracheGui (Just tvarExpander) $ \sprache -> Gtk.set
        expanderAktionen
        [Gtk.expanderLabel := (Language.aktionen <:> length aktionen) sprache]
aktualisiereExpanderText _hinzufügenSeite _aktionen =
    error "aktualisiereExpanderText mit falscher Seite aufgerufen!"

aktionHinzufügen :: (SpracheGuiReader r m, MonadIO m) => HinzufügenSeite -> Aktion -> m ()
aktionHinzufügen
    seite@HinzufügenSeitePlan {vBoxAktionen, tvarAktionen, buttonHinzufügenPlan}
    aktion = do
    (aktuelleAktionen, tvarSprachwechselAktionen) <- liftIO $ do
        aktuelleAktionen <- readTVarIO tvarAktionen
        tvarSprachwechselAktionen <- newTVarIO $ Just []
        pure (aktuelleAktionen, tvarSprachwechselAktionen)
    label <- boxPackWidgetNewDefault vBoxAktionen
        $ labelSpracheNew (Just tvarSprachwechselAktionen)
        $ anzeige aktion
    let neueAktionen = anhängen (aktion, label, tvarSprachwechselAktionen) aktuelleAktionen
    liftIO $ do
        atomically $ writeTVar tvarAktionen neueAktionen
        Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := True]
    aktualisiereExpanderText seite neueAktionen
aktionHinzufügen _hinzufügenSeite _aktion =
    error "aktionHinzufügen mit falscher Seite aufgerufen!"
#endif
--
