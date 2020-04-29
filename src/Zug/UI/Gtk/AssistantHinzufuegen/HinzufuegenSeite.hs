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
       (Bahngeschwindigkeit(..), BahngeschwindigkeitContainer(..), Streckenabschnitt(..)
      , StreckenabschnittContainer(..), Weiche(..), Kupplung(..), KupplungContainer(..), Kontakt(..)
      , KontaktContainer(..), Wegstrecke(..), Wartezeit(..), StreckenObjekt(erhalteName)
      , StreckenAtom(fließend), Anschluss())
import Zug.Enums (Richtung(..), unterstützteRichtungen, Zugtyp(..), ZugtypKlasse(..)
                , ZugtypEither(..), zugtyp, ausZugtypEither, GeschwindigkeitVariante(..)
                , GeschwindigkeitEither(..), geschwindigkeitVariante)
import qualified Zug.Language as Language
import Zug.Language (Sprache(), MitSprache(..), Anzeige(..), (<:>))
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (Plan(..), Aktion(..))
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
       (widgetShowNew, widgetShowIf, boxPackWidgetNewDefault, boxPackDefault, boxPackWidgetNew
      , boxPack, containerAddWidgetNew, labelSpracheNew, buttonNewWithEventLabel, Packing(PackGrow)
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
            -- Märklin
          , notebookGeschwindigkeit :: Gtk.Notebook
          , indexSeiten :: Map Int GeschwindigkeitVariante
          , märklinGeschwindigkeitAuswahl :: PinAuswahlWidget
          , vBoxMärklinFahrstrom :: ScrollbaresWidget Gtk.VBox
          , tvarFahrstromAuswahlWidgets :: TVar (NonEmpty AnschlussAuswahlWidget)
          , umdrehenAuswahl :: AnschlussAuswahlWidget
            -- Lego
          , legoGeschwindigkeitAuswahl :: PinAuswahlWidget
          , fahrtrichtungsAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteStreckenabschnitt
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , stromAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteWeiche
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , buttonHinzufügenWeiche :: ZugtypSpezifisch Gtk.Button
            -- Märklin
          , märklinRichtungsAuswahl
                :: NonEmpty (Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget)
            -- Lego
          , legoRichtungsAuswahl :: PinAuswahlWidget
          , legoRichtungenAuswahl :: AuswahlWidget (Richtung, Richtung)
          }
    | HinzufügenSeiteKupplung
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , kupplungsAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteKontakt
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , kontaktAuswahl :: AnschlussAuswahlWidget
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
    , märklinGeschwindigkeitAuswahl
    , tvarFahrstromAuswahlWidgets
    , umdrehenAuswahl
    , legoGeschwindigkeitAuswahl
    , fahrtrichtungsAuswahl} = liftIO $ do
    name <- aktuellerName nameAuswahl
    fließend <- aktuellerFließendValue fließendAuswahl
    aktuelleAuswahl zugtypAuswahl >>= \case
        Märklin -> do
            (`Map.lookup` indexSeiten) <$> Gtk.get notebookGeschwindigkeit Gtk.notebookPage >>= \case
                (Just Pwm) -> do
                    bgmpGeschwindigkeitsPin <- aktuellerPin märklinGeschwindigkeitAuswahl
                    pure
                        $ OBahngeschwindigkeit
                        $ ZugtypMärklin
                        $ GeschwindigkeitPwm
                            MärklinBahngeschwindigkeitPwm
                            { bgmpName = name
                            , bgmpFließend = fließend
                            , bgmpGeschwindigkeitsPin
                            }
                (Just KonstanteSpannung) -> do
                    fahrstromAuswahlWidgets <- readTVarIO tvarFahrstromAuswahlWidgets
                    bgmkFahrstromAnschlüsse <- mapM aktuellerAnschluss fahrstromAuswahlWidgets
                    bgmkUmdrehenAnschluss <- aktuellerAnschluss umdrehenAuswahl
                    pure
                        $ OBahngeschwindigkeit
                        $ ZugtypMärklin
                        $ GeschwindigkeitKonstanteSpannung
                            MärklinBahngeschwindigkeitKonstanteSpannung
                            { bgmkName = name
                            , bgmkFließend = fließend
                            , bgmkFahrstromAnschlüsse
                            , bgmkUmdrehenAnschluss
                            }
                Nothing -> error
                    "Unbekannte GeschwindigkeitVariante beim Hinzufügen einer Bahngeschwindigkeit!"
        Lego -> do
            bglGeschwindigkeitsPin <- aktuellerPin legoGeschwindigkeitAuswahl
            bglFahrtrichtungsAnschluss <- aktuellerAnschluss fahrtrichtungsAuswahl
            pure
                $ OBahngeschwindigkeit
                $ ZugtypLego
                $ GeschwindigkeitPwm
                    LegoBahngeschwindigkeit
                    { bglName = name
                    , bglFließend = fließend
                    , bglGeschwindigkeitsPin
                    , bglFahrtrichtungsAnschluss
                    }
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
                    MärklinWeiche
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
                    LegoWeiche
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
            True -> pure $ Set.insert (erhalteObjektTyp a) acc
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
                    pure $ Set.insert (erhalteObjektTyp weiche, richtung) acc
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
setzeSeite :: forall r m.
           (StatusVarGuiReader r m, SpracheGuiReader r m, MonadIO m)
           => FließendAuswahlWidget
           -> AuswahlWidget Zugtyp
           -> HinzufügenSeite
           -> Objekt
           -> m ()
setzeSeite
    fließendAuswahl
    zugtypAuswahl
    HinzufügenSeiteBahngeschwindigkeit
    { nameAuswahl
    , maybeTVarSprache
    , notebookGeschwindigkeit
    , indexSeiten
    , märklinGeschwindigkeitAuswahl
    , vBoxMärklinFahrstrom
    , tvarFahrstromAuswahlWidgets
    , umdrehenAuswahl
    , legoGeschwindigkeitAuswahl
    , fahrtrichtungsAuswahl}
    (OBahngeschwindigkeit bg) = do
    setzeName nameAuswahl $ erhalteName bg
    setzeFließendValue fließendAuswahl $ fließend bg
    setzeAuswahl zugtypAuswahl $ zugtyp bg
    liftIO $ forM (Map.toList indexSeiten) $ \(index, variante) -> when
        (variante == ausZugtypEither geschwindigkeitVariante bg)
        $ Gtk.set notebookGeschwindigkeit [Gtk.notebookPage := index]
    case bg of
        (ZugtypMärklin
             (GeschwindigkeitPwm MärklinBahngeschwindigkeitPwm {bgmpGeschwindigkeitsPin}))
            -> setzePin märklinGeschwindigkeitAuswahl bgmpGeschwindigkeitsPin
        (ZugtypMärklin
             (GeschwindigkeitKonstanteSpannung
                  MärklinBahngeschwindigkeitKonstanteSpannung
                  {bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss})) -> do
            let erstelleFahrstromAnschluss :: Anschluss -> Int -> m AnschlussAuswahlWidget
                erstelleFahrstromAnschluss anschluss n = do
                    anschlussAuswahlWidget <- boxPackWidgetNewDefault vBoxMärklinFahrstrom
                        $ anschlussAuswahlNew maybeTVarSprache
                        $ Language.fahrstrom <:> n
                    setzeAnschluss anschlussAuswahlWidget anschluss
                    pure anschlussAuswahlWidget
            auswahlFahrstromAnschlüsse <- NonEmpty.fromList
                <$> foldM (\widgets anschluss -> fmap (: widgets)
                           $ erstelleFahrstromAnschluss anschluss
                           $ succ
                           $ length widgets) [] bgmkFahrstromAnschlüsse
            liftIO $ do
                alteAuswahlWidgets <- atomically
                    $ swapTVar tvarFahrstromAuswahlWidgets auswahlFahrstromAnschlüsse
                forM_ alteAuswahlWidgets $ Gtk.widgetDestroy . erhalteWidget
                setzeAnschluss umdrehenAuswahl bgmkUmdrehenAnschluss
        (ZugtypLego
             (GeschwindigkeitPwm
                  LegoBahngeschwindigkeit {bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss}))
            -> do
                setzePin legoGeschwindigkeitAuswahl bglGeschwindigkeitsPin
                setzeAnschluss fahrtrichtungsAuswahl bglFahrtrichtungsAnschluss
        (ZugtypLego (GeschwindigkeitKonstanteSpannung bg))
            -> error $ "Lego-Bahngeschwindigkeit mit konstanter Spannung: " ++ show bg
setzeSeite
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteStreckenabschnitt {nameAuswahl, stromAuswahl}
    (OStreckenabschnitt Streckenabschnitt {stName, stFließend, stromAnschluss}) = do
    setzeName nameAuswahl stName
    setzeFließendValue fließendAuswahl stFließend
    setzeAnschluss stromAuswahl stromAnschluss
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
        (ZugtypMärklin MärklinWeiche {wemRichtungsAnschlüsse})
            -> forM_ märklinRichtungsAuswahl $ \(richtung, rcb, anschlussAuswahl)
            -> case List.lookup richtung $ NonEmpty.toList wemRichtungsAnschlüsse of
                (Just anschluss) -> do
                    registrierterCheckButtonSetToggled rcb True
                    setzeAnschluss anschlussAuswahl anschluss
                Nothing -> registrierterCheckButtonSetToggled rcb False
        (ZugtypLego LegoWeiche {welRichtungen, welRichtungsPin}) -> do
            setzeAuswahl legoRichtungenAuswahl welRichtungen
            setzePin legoRichtungsAuswahl welRichtungsPin
setzeSeite
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteKupplung {nameAuswahl, kupplungsAuswahl}
    (OKupplung Kupplung {kuName, kuFließend, kupplungsAnschluss}) = do
    setzeName nameAuswahl kuName
    setzeFließendValue fließendAuswahl kuFließend
    setzeAnschluss kupplungsAuswahl kupplungsAnschluss
setzeSeite
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteKontakt {nameAuswahl, kontaktAuswahl}
    (OKontakt Kontakt {koName, koFließend, kontaktAnschluss}) = do
    setzeName nameAuswahl koName
    setzeFließendValue fließendAuswahl koFließend
    setzeAnschluss kontaktAuswahl kontaktAnschluss
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
        $ elem (erhalteObjektTyp bgWidgets)
        $ enthalteneBahngeschwindigkeiten ws
    forM_ (aktuellerStatus ^. streckenabschnitte) $ \stWidgets -> widgetHinzufügenSetToggled
        (stWidgets ^. getterWegstrecke)
        $ elem (erhalteObjektTyp stWidgets)
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
        $ \weWidgets -> case Map.lookup (erhalteObjektTyp weWidgets) $ getWeichenRichtungen ws of
            (Just richtung) -> do
                let widgetHinzufügen = weWidgets ^. getterWegstrecke
                widgetHinzufügenSetToggled widgetHinzufügen True
                widgetHinzufügenSetzeAuswahl widgetHinzufügen richtung
            Nothing -> widgetHinzufügenSetToggled (weWidgets ^. getterWegstrecke) False
    forM_ (aktuellerStatus ^. kupplungen) $ \kuWidgets -> widgetHinzufügenSetToggled
        (kuWidgets ^. getterWegstrecke)
        $ elem (erhalteObjektTyp kuWidgets)
        $ enthalteneKupplungen ws
    forM_ (aktuellerStatus ^. kontakte) $ \koWidgets -> widgetHinzufügenSetToggled
        (koWidgets ^. getterWegstrecke)
        $ elem (erhalteObjektTyp koWidgets)
        $ enthalteneKontakte ws
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
setzeSeite _fließendAuswahl _zugtypAuswahl _hinzufügenSeite _objekt = pure ()

-- | Erzeuge eine Seite zum hinzufügen einer 'Bahngeschwindigkeit'.
hinzufügenBahngeschwindigkeitNew
    :: (SpracheGuiReader r m, MonadIO m)
    => AuswahlWidget Zugtyp
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> m HinzufügenSeite
hinzufügenBahngeschwindigkeitNew auswahlZugtyp maybeTVarSprache = do
    reader <- ask
    vBox <- liftIO $ widgetShowNew $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVarSprache
    (märklinVBox, notebookGeschwindigkeit) <- liftIO $ do
        märklinVBox <- Gtk.vBoxNew False 0
        notebookGeschwindigkeit <- boxPackWidgetNew
            märklinVBox
            PackGrow
            paddingDefault
            positionDefault
            Gtk.notebookNew
        pure (märklinVBox, notebookGeschwindigkeit)
    (vBoxMärklinPwm, indexPwm) <- notebookAppendPageNew
        notebookGeschwindigkeit
        maybeTVarSprache
        Language.geschwindigkeitPwm
        $ liftIO
        $ Gtk.vBoxNew False 0
    märklinGeschwindigkeitAuswahl <- boxPackWidgetNewDefault vBoxMärklinPwm
        $ pinAuswahlNew maybeTVarSprache Language.geschwindigkeit
    (vBoxMärklinKonstanteSpannung, indexKonstanteSpannung) <- notebookAppendPageNew
        notebookGeschwindigkeit
        maybeTVarSprache
        Language.geschwindigkeitKonstanteSpannung
        $ liftIO
        $ Gtk.vBoxNew False 0
    let indexSeiten = Map.fromList [(indexPwm, Pwm), (indexKonstanteSpannung, KonstanteSpannung)]
    fahrstromAuswahlWidget1 <- widgetShowNew
        $ anschlussAuswahlNew maybeTVarSprache
        $ Language.fahrstrom <:> (1 :: Word8)
    (vBoxMärklinFahrstrom, tvarFahrstromAuswahlWidgets, hBoxMärklinFahrstrom) <- liftIO $ do
        hBoxMärklinFahrstrom
            <- boxPackWidgetNewDefault vBoxMärklinKonstanteSpannung $ Gtk.hBoxNew False 0
        vBoxMärklinFahrstrom <- boxPackWidgetNew
            vBoxMärklinKonstanteSpannung
            PackGrow
            paddingDefault
            positionDefault
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        tvarFahrstromAuswahlWidgets <- newTVarIO $ [fahrstromAuswahlWidget1]
        pure (vBoxMärklinFahrstrom, tvarFahrstromAuswahlWidgets, hBoxMärklinFahrstrom)
    boxPackWidgetNewDefault hBoxMärklinFahrstrom
        $ buttonNewWithEventLabel maybeTVarSprache (const "+")
        $ do
            fahrstromAuswahlWidgets <- readTVarIO tvarFahrstromAuswahlWidgets
            fahrstromAuswahlWidgetN <- boxPackWidgetNewDefault vBoxMärklinFahrstrom
                $ flip runReaderT reader
                $ anschlussAuswahlNew maybeTVarSprache
                $ Language.fahrstrom <:> (succ $ length fahrstromAuswahlWidgets)
            atomically $ modifyTVar tvarFahrstromAuswahlWidgets $ (<> [fahrstromAuswahlWidgetN])
    boxPackWidgetNewDefault hBoxMärklinFahrstrom
        $ buttonNewWithEventLabel maybeTVarSprache (const "-")
        $ do
            fahrstromAuswahlWidgets <- readTVarIO tvarFahrstromAuswahlWidgets
            when (length fahrstromAuswahlWidgets > 1) $ do
                Gtk.widgetDestroy $ erhalteWidget $ NonEmpty.last fahrstromAuswahlWidgets
                atomically
                    $ modifyTVar tvarFahrstromAuswahlWidgets
                    $ NonEmpty.fromList . NonEmpty.init
    boxPackDefault vBoxMärklinFahrstrom fahrstromAuswahlWidget1
    umdrehenAuswahl <- boxPackWidgetNewDefault vBoxMärklinKonstanteSpannung
        $ anschlussAuswahlNew maybeTVarSprache Language.umdrehen
    legoVBox <- liftIO $ Gtk.vBoxNew False 0
    legoGeschwindigkeitAuswahl <- boxPackWidgetNewDefault legoVBox
        $ pinAuswahlNew maybeTVarSprache Language.geschwindigkeit
    fahrtrichtungsAuswahl <- boxPackWidgetNewDefault legoVBox
        $ anschlussAuswahlNew maybeTVarSprache Language.fahrtrichtung
    boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
        $ zugtypSpezifischNew [(Märklin, märklinVBox), (Lego, legoVBox)] auswahlZugtyp
    pure
        HinzufügenSeiteBahngeschwindigkeit
        { vBox
        , maybeTVarSprache
        , nameAuswahl
          -- Märklin
        , notebookGeschwindigkeit
        , indexSeiten
        , märklinGeschwindigkeitAuswahl
        , vBoxMärklinFahrstrom
        , tvarFahrstromAuswahlWidgets
        , umdrehenAuswahl
          -- Lego
        , legoGeschwindigkeitAuswahl
        , fahrtrichtungsAuswahl
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
               => [(Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget)]
               -> (Richtung, RegistrierterCheckButton)
               -> m [(Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget)]
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
    (tvarAktionen, expanderAktionen, vBoxAktionen, tvarExpander, hBoxWartezeit, sbWartezeit)
        <- liftIO $ do
            tvarAktionen <- newTVarIO leer
            expanderAktionen <- widgetShowNew
                $ Gtk.expanderNew (leseSprache (Language.aktionen <:> (0 :: Int)) spracheGui)
            vBoxAktionen <- containerAddWidgetNew expanderAktionen
                $ scrollbaresWidgetNew
                $ Gtk.vBoxNew False 0
            tvarExpander <- newTVarIO $ Just []
            hBoxWartezeit <- boxPackWidgetNewDefault vBoxAktionenWidgets $ Gtk.hBoxNew False 0
            spinButtonWartezeit <- widgetShowNew $ Gtk.spinButtonNewWithRange 1 999 1
            pure
                ( tvarAktionen
                , expanderAktionen
                , vBoxAktionen
                , tvarExpander
                , hBoxWartezeit
                , spinButtonWartezeit
                )
    comboBoxWartezeit <- widgetShowNew
        $ auswahlComboBoxNamedNew ["µs", "ms", "s", "min", "h", "d"] maybeTVar (const Text.empty)
        $ const . Text.pack
    boxPackWidgetNewDefault hBoxWartezeit $ buttonNewWithEventLabel maybeTVar Language.warten $ do
        wert <- floor <$> Gtk.get sbWartezeit Gtk.spinButtonValue
        void $ flip runReaderT spracheGui $ aktuelleAuswahl comboBoxWartezeit >>= \case
            "µs" -> aktionHinzufügen seite $ Warten $ MikroSekunden wert
            "ms" -> aktionHinzufügen seite $ Warten $ MilliSekunden wert
            "s" -> aktionHinzufügen seite $ Warten $ Sekunden wert
            "min" -> aktionHinzufügen seite $ Warten $ Minuten wert
            "h" -> aktionHinzufügen seite $ Warten $ Stunden wert
            "d" -> aktionHinzufügen seite $ Warten $ Tage wert
            zeiteinheit
                -> error $ "Unbekannte Zeiteinheit für Wartezeit gewählt: " ++ zeiteinheit
    boxPackDefault hBoxWartezeit sbWartezeit
    boxPackDefault hBoxWartezeit comboBoxWartezeit
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
    boxPackDefault vBoxAktionenWidgets expanderAktionen
    (buttonHinzufügenPlan, resetBox) <- liftIO $ do
        buttonHinzufügenPlan <- Gtk.buttonNew
        Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := False]
        resetBox <- boxPackWidgetNewDefault vBoxAktionenWidgets $ Gtk.hBoxNew False 0
        pure (buttonHinzufügenPlan, resetBox)
    boxPackWidgetNew resetBox PackGrow paddingDefault positionDefault
        $ buttonNewWithEventLabel maybeTVar Language.rückgängig
        $ do
            aktuelleAktionen <- readTVarIO tvarAktionen
            neueAktionen <- case zeigeLetztes aktuelleAktionen of
                Leer -> do
                    Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := False]
                    pure leer
                Gefüllt (_aktion, widget, tvarSprache) t -> do
                    mitContainerRemove vBoxAktionen widget
                    Gtk.widgetDestroy widget
                    atomically $ writeTVar tvarSprache Nothing
                    pure t
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
aktionHinzufügen seite@HinzufügenSeitePlan {vBoxAktionen, tvarAktionen} aktion = do
    (aktuelleAktionen, tvarSprache) <- liftIO $ do
        aktuelleAktionen <- readTVarIO tvarAktionen
        tvarSprache <- newTVarIO $ Just []
        pure (aktuelleAktionen, tvarSprache)
    label <- boxPackWidgetNewDefault vBoxAktionen
        $ labelSpracheNew (Just tvarSprache)
        $ anzeige aktion
    let neueAktionen = anhängen (aktion, label, tvarSprache) aktuelleAktionen
    liftIO $ atomically $ writeTVar tvarAktionen neueAktionen
    aktualisiereExpanderText seite neueAktionen
aktionHinzufügen _hinzufügenSeite _aktion =
    error "aktionHinzufügen mit falscher Seite aufgerufen!"
#endif
--
