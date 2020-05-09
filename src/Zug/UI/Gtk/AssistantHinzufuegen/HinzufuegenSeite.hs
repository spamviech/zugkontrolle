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
import Control.Lens ((^.), Field1(_1), Field2(_2))
import qualified Control.Lens as Lens
import Control.Monad (void, forM, forM_, foldM, when)
import Control.Monad.Fix (MonadFix())
import Control.Monad.Reader (runReaderT, MonadReader(ask))
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (Foldable(..))
import Data.Function ((&))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, catMaybes, listToMaybe)
import Data.Semigroup (Semigroup((<>)))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

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
import Zug.Language (Sprache(), MitSprache(..), Anzeige(..), (<:>))
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
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), verwendeSpracheGui)
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
          { vBox :: Gtk.VBox
          , maybeTVarSprache :: Maybe (TVar (Maybe [Sprache -> IO ()]))
          , nameAuswahl :: NameAuswahlWidget
          , notebookGeschwindigkeit :: Gtk.Notebook
          , indexSeiten :: Map Int GeschwindigkeitVariante
          , geschwindigkeitAuswahl :: PinAuswahlWidget
          , vBoxFahrstrom :: ScrollbaresWidget Gtk.VBox
            -- Märklin
          , tvarFahrstromAuswahlWidgets
                :: TVar (NonEmpty (AnschlussAuswahlWidget 'InterruptPinEgal))
          , umdrehenAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
            -- Lego
          , pwmFahrtrichtungsAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
          , konstanteSpannungFahrtrichtungsAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
          }
    | HinzufügenSeiteStreckenabschnitt
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , stromAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
          }
    | HinzufügenSeiteWeiche
          { vBox :: Gtk.VBox
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
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , kupplungsAuswahl :: AnschlussAuswahlWidget 'InterruptPinEgal
          }
    | HinzufügenSeiteKontakt
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , kontaktAuswahl :: AnschlussAuswahlWidget 'InterruptPinBenötigt
          }
    | HinzufügenSeiteWegstrecke
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , buttonHinzufügenWegstrecke
                :: FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid
          }
    | HinzufügenSeitePlan
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , buttonHinzufügenPlan :: Gtk.Button
          , expanderAktionen :: Gtk.Expander
          , tvarExpander :: TVar (Maybe [Sprache -> IO ()])
          , vBoxAktionen :: ScrollbaresWidget Gtk.VBox
          , tvarAktionen
                :: TVar (Warteschlange (Aktion, Gtk.Label, TVar (Maybe [Sprache -> IO ()])))
          , checkButtonDauerschleife :: Gtk.CheckButton
          }
    deriving (Eq)

instance MitWidget HinzufügenSeite where
    erhalteWidget :: HinzufügenSeite -> Gtk.Widget
    erhalteWidget = Gtk.toWidget . vBox

-- | Varianten eines 'Gtk.Button', um das Hinzufügen auszulösen.
data ButtonHinzufügen
    = ButtonHinzufügen Gtk.Button
    | ButtonHinzufügenZugtypSpezifisch (ZugtypSpezifisch Gtk.Button)
    | ButtonHinzufügenFortfahrenWennToggledVar (FortfahrenWennToggledVar StatusGui StatusVarGui WegstreckeCheckButtonVoid)
    deriving (Eq)

instance MitWidget ButtonHinzufügen where
    erhalteWidget :: ButtonHinzufügen -> Gtk.Widget
    erhalteWidget (ButtonHinzufügen button) = erhalteWidget button
    erhalteWidget (ButtonHinzufügenZugtypSpezifisch button) = erhalteWidget button
    erhalteWidget (ButtonHinzufügenFortfahrenWennToggledVar button) = erhalteWidget button

instance MitContainer ButtonHinzufügen where
    erhalteContainer :: ButtonHinzufügen -> Gtk.Container
    erhalteContainer (ButtonHinzufügen button) = erhalteContainer button
    erhalteContainer (ButtonHinzufügenZugtypSpezifisch button) = erhalteContainer button
    erhalteContainer (ButtonHinzufügenFortfahrenWennToggledVar button) = erhalteContainer button

instance MitButton ButtonHinzufügen where
    erhalteButton :: ButtonHinzufügen -> Gtk.Button
    erhalteButton (ButtonHinzufügen button) = button
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
    fließend <- aktuellerFließendValue fließendAuswahl
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
                    , wemFließend = fließend
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
                    , welFließend = fließend
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
                $ map vonZugtypEither
                $ aktuellerStatus ^. bahngeschwindigkeiten
            wsStreckenabschnitte
                <- foldM anhängenWennToggled Set.empty $ aktuellerStatus ^. streckenabschnitte
            wsWeichenRichtungen <- foldM weichenRichtungAnhängenWennToggled Set.empty
                $ catMaybes
                $ map vonZugtypEither
                $ aktuellerStatus ^. weichen
            wsKupplungen <- foldM anhängenWennToggled Set.empty $ aktuellerStatus ^. kupplungen
            wsKontakte <- foldM anhängenWennToggled Set.empty $ aktuellerStatus ^. kontakte
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
        anhängenWennToggled acc a = widgetHinzufügenToggled (a ^. getterWegstrecke) >>= \case
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
            let widgetHinzufügen = weiche ^. getterWegstrecke
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
                              $ Lens.view _1 <$> aktionenWarteschlange
                        }
                in plan
        False -> Plan
            { plName
            , plAktionen = NonEmpty.fromList $ toList $ Lens.view _1 <$> aktionenWarteschlange
            }

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
                forM_ alteAuswahlWidgets $ Gtk.widgetDestroy . erhalteWidget
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
                    forM_ alteAuswahlWidgets $ Gtk.widgetDestroy . erhalteWidget
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
    forM_ (aktuellerStatus ^. bahngeschwindigkeiten) $ \bgWidgets -> widgetHinzufügenSetToggled
        (bgWidgets ^. getterWegstrecke)
        $ elem (zuObjektTyp bgWidgets)
        $ enthalteneBahngeschwindigkeiten ws
    forM_ (aktuellerStatus ^. streckenabschnitte) $ \stWidgets -> widgetHinzufügenSetToggled
        (stWidgets ^. getterWegstrecke)
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
    forM_ (aktuellerStatus ^. weichen)
        $ \weWidgets -> case Map.lookup (zuObjektTyp weWidgets) $ getWeichenRichtungen ws of
            (Just richtung) -> do
                let widgetHinzufügen = weWidgets ^. getterWegstrecke
                widgetHinzufügenSetToggled widgetHinzufügen True
                widgetHinzufügenSetzeAuswahl widgetHinzufügen richtung
            Nothing -> widgetHinzufügenSetToggled (weWidgets ^. getterWegstrecke) False
    forM_ (aktuellerStatus ^. kupplungen) $ \kuWidgets -> widgetHinzufügenSetToggled
        (kuWidgets ^. getterWegstrecke)
        $ elem (zuObjektTyp kuWidgets)
        $ enthalteneKupplungen ws
    forM_ (aktuellerStatus ^. kontakte) $ \koWidgets -> widgetHinzufügenSetToggled
        (koWidgets ^. getterWegstrecke)
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
        alteAktionsWidgets <- fmap (fmap $ Lens.view _2) $ atomically $ swapTVar tvarAktionen leer
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
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> m HinzufügenSeite
hinzufügenBahngeschwindigkeitNew auswahlZugtyp maybeTVarSprache = mdo
    reader <- ask
    vBox <- liftIO $ widgetShowNew $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVarSprache
    notebookGeschwindigkeit
        <- liftIO $ boxPackWidgetNew vBox PackGrow paddingDefault positionDefault Gtk.notebookNew
    (vBoxPwm, indexPwm) <- notebookAppendPageNew
        notebookGeschwindigkeit
        maybeTVarSprache
        Language.geschwindigkeitPwm
        $ liftIO
        $ Gtk.vBoxNew False 0
    geschwindigkeitAuswahl
        <- boxPackWidgetNewDefault vBox $ pinAuswahlNew maybeTVarSprache Language.geschwindigkeit
    (vBoxKonstanteSpannung, indexKonstanteSpannung) <- notebookAppendPageNew
        notebookGeschwindigkeit
        maybeTVarSprache
        Language.geschwindigkeitKonstanteSpannung
        $ liftIO
        $ Gtk.vBoxNew False 0
    legoVBoxPwm <- liftIO $ Gtk.vBoxNew False 0
    pwmFahrtrichtungsAuswahl <- boxPackWidgetNewDefault legoVBoxPwm
        $ anschlussAuswahlNew maybeTVarSprache Language.fahrtrichtung
    boxPackWidgetNew vBoxPwm PackGrow paddingDefault positionDefault
        $ zugtypSpezifischNew [(Lego, legoVBoxPwm)] auswahlZugtyp
    let indexSeiten = Map.fromList [(indexPwm, Pwm), (indexKonstanteSpannung, KonstanteSpannung)]
    (vBoxFahrstrom, tvarFahrstromAuswahlWidgets, functionBoxFahrstrom) <- liftIO $ do
        functionBoxFahrstrom <- boxPackWidgetNewDefault vBoxKonstanteSpannung $ Gtk.hBoxNew False 0
        vBoxFahrstrom
            <- boxPackWidgetNew vBoxKonstanteSpannung PackGrow paddingDefault positionDefault
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        tvarFahrstromAuswahlWidgets <- newTVarIO $ [fahrstromAuswahlWidget1]
        pure (vBoxFahrstrom, tvarFahrstromAuswahlWidgets, functionBoxFahrstrom)
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
                Gtk.widgetDestroy $ erhalteWidget $ NonEmpty.last fahrstromAuswahlWidgets
                atomically
                    $ modifyTVar tvarFahrstromAuswahlWidgets
                    $ NonEmpty.fromList . NonEmpty.init
    fahrstromAuswahlWidget1 <- boxPackWidgetNewDefault vBoxFahrstrom
        $ anschlussAuswahlNew maybeTVarSprache
        $ Language.fahrstrom <:> (1 :: Word8)
    märklinVBoxKonstanteSpannung <- liftIO $ Gtk.vBoxNew False 0
    umdrehenAuswahl <- boxPackWidgetNewDefault märklinVBoxKonstanteSpannung
        $ anschlussAuswahlNew maybeTVarSprache Language.umdrehen
    legoVBoxKonstanteSpannung <- liftIO $ Gtk.vBoxNew False 0
    konstanteSpannungFahrtrichtungsAuswahl <- boxPackWidgetNewDefault legoVBoxKonstanteSpannung
        $ anschlussAuswahlNew maybeTVarSprache Language.fahrtrichtung
    boxPackWidgetNew vBoxKonstanteSpannung PackGrow paddingDefault positionDefault
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
hinzufügenStreckenabschnittNew :: (SpracheGuiReader r m, MonadIO m)
                                => Maybe (TVar (Maybe [Sprache -> IO ()]))
                                -> m HinzufügenSeite
hinzufügenStreckenabschnittNew maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    stromAuswahl <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.strom
    pure HinzufügenSeiteStreckenabschnitt { vBox, nameAuswahl, stromAuswahl }

-- | Erzeuge eine Seite zum hinzufügen einer 'Weiche'.
hinzufügenWeicheNew :: (SpracheGuiReader r m, MonadIO m)
                     => AuswahlWidget Zugtyp
                     -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                     -> m HinzufügenSeite
hinzufügenWeicheNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    -- Märklin
    märklinFortfahrenWennToggled <- fortfahrenWennToggledNew maybeTVar Language.hinzufügen
        $ anzeige <$> unterstützteRichtungen
    let richtungenCheckButtons =
            NonEmpty.zip unterstützteRichtungen $ checkButtons märklinFortfahrenWennToggled
    märklinGrid <- liftIO $ scrollbaresWidgetNew Gtk.gridNew
    let foldFn :: (SpracheGuiReader r m, MonadIO m)
               => [(Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget 'InterruptPinEgal)]
               -> (Richtung, RegistrierterCheckButton)
               -> m [(Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget 'InterruptPinEgal)]
        foldFn acc (richtung, registrierterCheckButton) = do
            let maybeTop = erhalteWidget . Lens.view _2 <$> listToMaybe acc
                registrierterCheckButtonWidget = erhalteWidget registrierterCheckButton
            liftIO
                $ Gtk.gridAttachNextTo
                    (erhalteGrid märklinGrid)
                    registrierterCheckButtonWidget
                    maybeTop
                    Gtk.PosBottom
                    1
                    1
            anschlussAuswahlWidget
                <- widgetShowNew $ anschlussAuswahlNew maybeTVar $ anzeige richtung
            liftIO
                $ Gtk.gridAttachNextTo
                    (erhalteGrid märklinGrid)
                    (erhalteWidget anschlussAuswahlWidget)
                    (Just registrierterCheckButtonWidget)
                    Gtk.PosRight
                    1
                    1
            pure $ (richtung, registrierterCheckButton, anschlussAuswahlWidget) : acc
    märklinRichtungsAuswahl
        <- NonEmpty.fromList . reverse <$> foldM foldFn [] richtungenCheckButtons
    -- Lego
    legoButtonHinzufügen <- liftIO Gtk.buttonNew
    legoVBox <- liftIO $ Gtk.vBoxNew False 0
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
        [(Märklin, erhalteButton märklinFortfahrenWennToggled), (Lego, legoButtonHinzufügen)]
        auswahlZugtyp
    boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
        $ zugtypSpezifischNew
            [(Märklin, erhalteWidget märklinGrid), (Lego, erhalteWidget legoVBox)]
            auswahlZugtyp
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
hinzufügenKupplungNew :: (SpracheGuiReader r m, MonadIO m)
                       => Maybe (TVar (Maybe [Sprache -> IO ()]))
                       -> m HinzufügenSeite
hinzufügenKupplungNew maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    kupplungsAuswahl
        <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.kupplung
    pure HinzufügenSeiteKupplung { vBox, nameAuswahl, kupplungsAuswahl }

-- | Erzeuge eine Seite zum hinzufügen eines 'Kontakt's.
hinzufügenKontaktNew :: (SpracheGuiReader r m, MonadIO m)
                      => Maybe (TVar (Maybe [Sprache -> IO ()]))
                      -> m HinzufügenSeite
hinzufügenKontaktNew maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    kontaktAuswahl <- boxPackWidgetNewDefault vBox
        $ anschlussAuswahlInterruptPinNew maybeTVar Language.kontakt
    pure HinzufügenSeiteKontakt { vBox, nameAuswahl, kontaktAuswahl }

-- | Erzeuge eine Seite zum hinzufügen einer 'Wegstrecke'.
hinzufügenWegstreckeNew :: (SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
                         => AuswahlWidget Zugtyp
                         -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                         -> m HinzufügenSeite
hinzufügenWegstreckeNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
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
    notebookAppendPageNew notebook maybeTVar Language.bahngeschwindigkeiten
        $ zugtypSpezifischNew
            [ (Märklin, erhalteWidget vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin)
            , (Lego, erhalteWidget vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego)]
            auswahlZugtyp
    notebookAppendPageNew notebook maybeTVar Language.streckenabschnitte
        $ pure vBoxHinzufügenWegstreckeStreckenabschnitte
    notebookAppendPageNew notebook maybeTVar Language.weichen
        $ zugtypSpezifischNew
            [ (Märklin, erhalteWidget vBoxHinzufügenWegstreckeWeichenMärklin)
            , (Lego, erhalteWidget vBoxHinzufügenWegstreckeWeichenLego)]
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
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> m HinzufügenSeite
hinzufügenPlanNew parent auswahlZugtyp maybeTVar = mdo
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    vBoxAktionenWidgets <- liftIO
        $ boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
        $ scrollbaresWidgetNew
        $ Gtk.vBoxNew False 0
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
    hBoxWartezeit <- liftIO $ boxPackWidgetNewDefault vBoxAktionenWidgets $ Gtk.hBoxNew False 0
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
        windowObjektAuswahl <- Gtk.windowNew
        Gtk.set
            windowObjektAuswahl
            [ Gtk.windowTransientFor := erhalteWindow parent
            , Gtk.windowModal := True
            , Gtk.windowDefaultHeight := 400
            , Gtk.windowDefaultWidth := 300]
        Gtk.on windowObjektAuswahl Gtk.deleteEvent $ liftIO $ do
            atomically $ putTMVar dynTMVarPlanObjekt Nothing
            pure True
        vBoxObjektAuswahl <- containerAddWidgetNew windowObjektAuswahl $ Gtk.vBoxNew False 0
        ztBahngeschwindigkeiten
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenLego)]
                auswahlZugtyp
        ztBahngeschwindigkeitenPwm
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm)
                , (Lego, erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm)]
                auswahlZugtyp
        ztBahngeschwindigkeitenKonstanteSpannung
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ ( Märklin
                      , erhalteWidget
                        vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
                      )
                , ( Lego
                      , erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
                      )]
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
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWeichenGeradeMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWeichenGeradeLego)]
                auswahlZugtyp
        ztWeichenKurve
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWeichenKurveMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWeichenKurveLego)]
                auswahlZugtyp
        ztWeichenLinks
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWeichenLinksMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWeichenLinksLego)]
                auswahlZugtyp
        ztWeichenRechts
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWeichenRechtsMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWeichenRechtsLego)]
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
                [ ( Märklin
                      , erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                      )
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego)]
                auswahlZugtyp
        ztWegstreckenBGPwm
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ ( Märklin
                      , erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinPwm
                      )
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoPwm)]
                auswahlZugtyp
        ztWegstreckenBGKonstanteSpannung
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ ( Märklin
                      , erhalteWidget
                        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklinKonstanteSpannung
                      )
                , ( Lego
                      , erhalteWidget
                        vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLegoKonstanteSpannung
                      )]
                auswahlZugtyp
        ztWegstreckenST
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenStreckenabschnittLego)]
                auswahlZugtyp
        ztWegstreckenKU
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWegstreckenKupplungLego)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenKupplungMärklin)]
                auswahlZugtyp
        ztWegstreckenKO
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWegstreckenKontaktLego)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenKontaktMärklin)]
                auswahlZugtyp
        ztWegstreckenWS
            <- boxPackWidgetNew vBoxObjektAuswahl PackGrow paddingDefault positionDefault
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWegstreckenMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenLego)]
                auswahlZugtyp
        boxPack vBoxObjektAuswahl vBoxHinzufügenPlanPläne PackGrow paddingDefault positionDefault
        let hideExcept :: [Gtk.Widget] -> IO ()
            hideExcept shownWidgets =
                mapM_
                    (\widget -> widgetShowIf (erhalteWidget widget `elem` shownWidgets) widget)
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
                     , erhalteWidget vBoxHinzufügenPlanPläne] :: [Gtk.Widget])
        let showBG :: Maybe GeschwindigkeitVariante -> IO ()
            showBG Nothing =
                hideExcept [erhalteWidget ztBahngeschwindigkeiten, erhalteWidget ztWegstreckenBG]
            showBG (Just Pwm) =
                hideExcept
                    [erhalteWidget ztBahngeschwindigkeitenPwm, erhalteWidget ztWegstreckenBGPwm]
            showBG (Just KonstanteSpannung) =
                hideExcept
                    [ erhalteWidget ztBahngeschwindigkeitenKonstanteSpannung
                    , erhalteWidget ztWegstreckenBGKonstanteSpannung]
            showST :: IO ()
            showST =
                hideExcept
                    [ erhalteWidget vBoxHinzufügenPlanStreckenabschnitte
                    , erhalteWidget ztWegstreckenST]
            showGerade :: IO ()
            showGerade = hideExcept [erhalteWidget ztWeichenGerade]
            showKurve :: IO ()
            showKurve = hideExcept [erhalteWidget ztWeichenKurve]
            showLinks :: IO ()
            showLinks = hideExcept [erhalteWidget ztWeichenLinks]
            showRechts :: IO ()
            showRechts = hideExcept [erhalteWidget ztWeichenRechts]
            showKU :: IO ()
            showKU =
                hideExcept
                    [erhalteWidget vBoxHinzufügenPlanKupplungen, erhalteWidget ztWegstreckenKU]
            showKO :: IO ()
            showKO =
                hideExcept
                    [erhalteWidget vBoxHinzufügenPlanKontakte, erhalteWidget ztWegstreckenKO]
            showWS :: IO ()
            showWS = hideExcept [erhalteWidget ztWegstreckenWS]
            showPL :: IO ()
            showPL = hideExcept [erhalteWidget vBoxHinzufügenPlanPläne]
        pure
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
            expanderAktionen <- boxPackWidgetNewDefault vBoxAktionenWidgets
                $ Gtk.expanderNew (leseSprache (Language.aktionen <:> (0 :: Int)) spracheGui)
            tvarAktionen <- newTVarIO leer
            vBoxAktionen <- containerAddWidgetNew expanderAktionen
                $ scrollbaresWidgetNew
                $ Gtk.vBoxNew False 0
            tvarExpander <- newTVarIO $ Just []
            buttonHinzufügenPlan <- Gtk.buttonNew
            Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := False]
            resetBox <- boxPackWidgetNewDefault vBoxAktionenWidgets $ Gtk.hBoxNew False 0
            pure
                ( expanderAktionen
                , tvarAktionen
                , vBoxAktionen
                , tvarExpander
                , buttonHinzufügenPlan
                , resetBox
                )
    boxPackWidgetNew resetBox PackGrow paddingDefault positionDefault
        $ buttonNewWithEventLabel maybeTVar Language.rückgängig
        $ do
            aktuelleAktionen <- readTVarIO tvarAktionen
            neueAktionen <- case zeigeLetztes aktuelleAktionen of
                Leer -> pure leer
                Gefüllt (_aktion, widget, tvarSprache) t -> do
                    mitContainerRemove vBoxAktionen widget
                    Gtk.widgetDestroy widget
                    atomically $ writeTVar tvarSprache Nothing
                    pure t
            Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := not (null neueAktionen)]
            atomically $ writeTVar tvarAktionen neueAktionen
            flip runReaderT spracheGui $ aktualisiereExpanderText seite neueAktionen
    boxPackWidgetNew resetBox PackGrow paddingDefault positionDefault
        $ buttonNewWithEventLabel maybeTVar Language.zurücksetzen
        $ do
            aktuelleAktionen <- readTVarIO tvarAktionen
            forM_ aktuelleAktionen $ \(_aktion, widget, tvarAktionen) -> do
                mitContainerRemove vBoxAktionen widget
                Gtk.widgetDestroy widget
                atomically $ writeTVar tvarAktionen Nothing
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
    (aktuelleAktionen, tvarSprache) <- liftIO $ do
        aktuelleAktionen <- readTVarIO tvarAktionen
        tvarSprache <- newTVarIO $ Just []
        pure (aktuelleAktionen, tvarSprache)
    label <- boxPackWidgetNewDefault vBoxAktionen
        $ labelSpracheNew (Just tvarSprache)
        $ anzeige aktion
    let neueAktionen = anhängen (aktion, label, tvarSprache) aktuelleAktionen
    liftIO $ do
        atomically $ writeTVar tvarAktionen neueAktionen
        Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := True]
    aktualisiereExpanderText seite neueAktionen
aktionHinzufügen _hinzufügenSeite _aktion =
    error "aktionHinzufügen mit falscher Seite aufgerufen!"
#endif
--
