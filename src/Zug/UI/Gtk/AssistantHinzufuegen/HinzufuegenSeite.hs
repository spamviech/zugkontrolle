{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
#endif

module Zug.UI.Gtk.AssistantHinzufuegen.HinzufuegenSeite
  (
#ifdef ZUGKONTROLLEGUI
    HinzufügenSeite()
  , ButtonHinzufügen(..)
  , spezifischerButtonHinzufügen
  , seiteErgebnis
  , hinzufügenBahngeschwindigkeitNew
  , hinzufügenStreckenabschnittNew
  , hinzufügenWeicheNew
  , hinzufügenKupplungNew
  , hinzufügenWegstreckeNew
  , hinzufügenPlanNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
-- Bibliotheken
import Control.Concurrent.STM
       (TVar, atomically, readTVarIO, newTVarIO, writeTVar, putTMVar, takeTMVar)
import Control.Lens ((^.), Field2(..))
import qualified Control.Lens as Lens
import Control.Monad (forM, foldM)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (Foldable(..))
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust, isJust, catMaybes, listToMaybe)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

-- Abhängigkeit von anderen Modulen
import Zug.Anbindung
       (Bahngeschwindigkeit(..), Streckenabschnitt(..), Weiche(..), Kupplung(..), Wegstrecke(..))
import Zug.Enums
       (Richtung(), unterstützteRichtungen, Zugtyp(..), ZugtypKlasse(..), ZugtypEither(..))
import qualified Zug.Language as Language
import Zug.Language (Sprache(), MitSprache(..), Anzeige(..), (<:>))
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (Plan(..), Aktion(..), AktionWegstrecke(..), AktionBahngeschwindigkeit(..)
               , AktionStreckenabschnitt(..), AktionWeiche(..), AktionKupplung(..))
import Zug.UI.Base (bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen)
import Zug.UI.Gtk.Anschluss (AnschlussAuswahlWidget, anschlussAuswahlNew, aktuellerAnschluss)
import Zug.UI.Gtk.Auswahl (AuswahlWidget, auswahlComboBoxNew, MitAuswahlWidget(), aktuelleAuswahl)
import Zug.UI.Gtk.Fliessend (FließendAuswahlWidget, aktuellerFließendValue)
import Zug.UI.Gtk.FortfahrenWennToggled
       (fortfahrenWennToggledNew, checkButtons, FortfahrenWennToggledVar, RegistrierterCheckButton
      , registrierterCheckButtonToggled)
import Zug.UI.Gtk.Hilfsfunktionen
       (widgetShowNew, widgetShowIf, boxPackWidgetNewDefault, boxPackDefault, boxPackWidgetNew
      , containerAddWidgetNew, buttonNewWithEventLabel, Packing(PackGrow), paddingDefault
      , positionDefault, notebookAppendPageNew, NameAuswahlWidget, nameAuswahlPackNew, aktuellerName)
import Zug.UI.Gtk.Klassen
       (MitWidget(..), mitWidgetShow, mitWidgetHide, MitButton(..), MitContainer(..), MitWindow(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt
       (StatusGui, StatusVarGui, StatusVarGuiReader, WegstreckenElement(..), WegstreckeCheckButton()
      , WegstreckeCheckButtonVoid, WidgetsTyp(..), BGWidgets(), WEWidgets()
      , widgetHinzufügenToggled, widgetHinzufügenAktuelleAuswahl, DynamischeWidgets(..)
      , DynamischeWidgetsReader(..))
import Zug.UI.Gtk.ZugtypSpezifisch
       (ZugtypSpezifisch(), zugtypSpezifischNew, zugtypSpezifischButtonNew)
import Zug.UI.StatusVar (StatusVarReader(..), readStatusVar)
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes)

-- | Seiten des Hinzufügen-'Assistant'
data HinzufügenSeite
    = HinzufügenSeiteBahngeschwindigkeit
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , geschwindigkeitAuswahl :: AnschlussAuswahlWidget
            -- Lego
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
          , legoRichtungsAuswahl :: AnschlussAuswahlWidget
          , legoRichtungenAuswahl :: AuswahlWidget (Richtung, Richtung)
          }
    | HinzufügenSeiteKupplung
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , kupplungsAuswahl :: AnschlussAuswahlWidget
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
          , tvarAktionen :: TVar (Warteschlange (Aktion, Gtk.Label))
          , checkButtonDauerschleife :: Gtk.CheckButton
          }
    deriving (Eq)

instance MitWidget HinzufügenSeite where
    erhalteWidget :: HinzufügenSeite -> Gtk.Widget
    erhalteWidget = Gtk.toWidget . vBox

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
        {nameAuswahl, geschwindigkeitAuswahl, fahrtrichtungsAuswahl} = do
    name <- aktuellerName nameAuswahl
    fließend <- aktuellerFließendValue fließendAuswahl
    geschwindigkeitsAnschluss <- aktuellerAnschluss geschwindigkeitAuswahl
    aktuelleAuswahl zugtypAuswahl >>= \case
        Märklin -> pure
            $ OBahngeschwindigkeit
            $ ZugtypMärklin
                MärklinBahngeschwindigkeit
                    { bgmName = name
                    , bgmFließend = fließend
                    , bgmGeschwindigkeitsAnschluss = geschwindigkeitsAnschluss
                    }
        Lego -> do
            bglFahrtrichtungsAnschluss <- aktuellerAnschluss fahrtrichtungsAuswahl
            pure
                $ OBahngeschwindigkeit
                $ ZugtypLego
                    LegoBahngeschwindigkeit
                        { bglName = name
                        , bglFließend = fließend
                        , bglGeschwindigkeitsAnschluss = geschwindigkeitsAnschluss
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
            welRichtungsAnschluss <- aktuellerAnschluss legoRichtungsAuswahl
            pure
                $ OWeiche
                $ ZugtypLego
                    LegoWeiche
                        { welName = name
                        , welFließend = fließend
                        , welRichtungen
                        , welRichtungsAnschluss
                        }
seiteErgebnis
    fließendAuswahl
    _zugtypAuswahl
    HinzufügenSeiteKupplung {nameAuswahl, kupplungsAuswahl} = do
    kuName <- aktuellerName nameAuswahl
    kuFließend <- aktuellerFließendValue fließendAuswahl
    kupplungsAnschluss <- aktuellerAnschluss kupplungsAuswahl
    pure $ OKupplung Kupplung { kuName, kuFließend, kupplungsAnschluss }
seiteErgebnis _fließendAuswahl zugtypAuswahl HinzufügenSeiteWegstrecke {nameAuswahl} = do
    statusVar <- erhalteStatusVar :: m StatusVarGui
    aktuellerStatus <- liftIO $ atomically $ readStatusVar statusVar
    wsName <- aktuellerName nameAuswahl
    let gewählteWegstrecke
            :: ( MonadIO m
               , ZugtypKlasse z
               , WegstreckenElement (BGWidgets z)
               , WegstreckenElement (WEWidgets z)
               , MitAuswahlWidget (WegstreckeCheckButton (CheckButtonAuswahl (WEWidgets z))) Richtung
               )
            => m (Wegstrecke z)
        gewählteWegstrecke = do
            wsBahngeschwindigkeiten <- foldM anhängenWennToggled []
                $ catMaybes
                $ map vonZugtypEither
                $ aktuellerStatus ^. bahngeschwindigkeiten
            wsStreckenabschnitte
                <- foldM anhängenWennToggled [] $ aktuellerStatus ^. streckenabschnitte
            wsWeichenRichtungen <- foldM weichenRichtungAnhängenWennToggled []
                $ catMaybes
                $ map vonZugtypEither
                $ aktuellerStatus ^. weichen
            wsKupplungen <- foldM anhängenWennToggled [] $ aktuellerStatus ^. kupplungen
            pure
                Wegstrecke
                    { wsName
                    , wsBahngeschwindigkeiten
                    , wsStreckenabschnitte
                    , wsWeichenRichtungen
                    , wsKupplungen
                    }
        anhängenWennToggled :: (WidgetsTyp a, WegstreckenElement a, MonadIO m)
                             => [ObjektTyp a]
                             -> a
                             -> m [ObjektTyp a]
        anhängenWennToggled acc a = widgetHinzufügenToggled (a ^. getterWegstrecke) >>= \case
            True -> pure $ erhalteObjektTyp a : acc
            False -> pure acc
        weichenRichtungAnhängenWennToggled
            :: ( WegstreckenElement (WEWidgets z)
               , MonadIO m
               , MitAuswahlWidget (WegstreckeCheckButton (CheckButtonAuswahl (WEWidgets z))) Richtung
               )
            => [(Weiche z, Richtung)]
            -> WEWidgets z
            -> m [(Weiche z, Richtung)]
        weichenRichtungAnhängenWennToggled acc weiche = do
            let widgetHinzufügen = weiche ^. getterWegstrecke
            toggled <- widgetHinzufügenToggled widgetHinzufügen
            if toggled
                then do
                    richtung <- widgetHinzufügenAktuelleAuswahl widgetHinzufügen
                    pure $ (erhalteObjektTyp weiche, richtung) : acc
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
                        , plAktionen = toList
                              $ anhängen (AktionAusführen plan)
                              $ fst <$> aktionenWarteschlange
                        }
                in plan
        False -> Plan { plName, plAktionen = toList $ fst <$> aktionenWarteschlange }

hinzufügenBahngeschwindigkeitNew
    :: (SpracheGuiReader r m, MonadIO m)
    => AuswahlWidget Zugtyp
    -> Maybe (TVar (Maybe [Sprache -> IO ()]))
    -> m HinzufügenSeite
hinzufügenBahngeschwindigkeitNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ widgetShowNew $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    geschwindigkeitAuswahl
        <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.geschwindigkeit
    fahrtrichtungsAuswahl <- anschlussAuswahlNew maybeTVar Language.fahrtrichtung
    boxPackWidgetNewDefault vBox
        $ zugtypSpezifischNew [(Lego, fahrtrichtungsAuswahl)] auswahlZugtyp
    pure
        HinzufügenSeiteBahngeschwindigkeit
            { vBox
            , nameAuswahl
            , geschwindigkeitAuswahl
              -- Lego
            , fahrtrichtungsAuswahl
            }

hinzufügenStreckenabschnittNew :: (SpracheGuiReader r m, MonadIO m)
                                => Maybe (TVar (Maybe [Sprache -> IO ()]))
                                -> m HinzufügenSeite
hinzufügenStreckenabschnittNew maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    stromAuswahl <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.strom
    pure HinzufügenSeiteStreckenabschnitt { vBox, nameAuswahl, stromAuswahl }

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
    märklinGrid <- liftIO Gtk.gridNew
    let foldFn :: (SpracheGuiReader r m, MonadIO m)
               => [(Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget)]
               -> (Richtung, RegistrierterCheckButton)
               -> m [(Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget)]
        foldFn acc (richtung, registrierterCheckButton) = do
            let maybeTop = erhalteWidget . Lens.view _2 <$> listToMaybe acc
                registrierterCheckButtonWidget = erhalteWidget registrierterCheckButton
            liftIO
                $ Gtk.gridAttachNextTo
                    märklinGrid
                    registrierterCheckButtonWidget
                    maybeTop
                    Gtk.PosBottom
                    1
                    1
            anschlussAuswahlWidget
                <- widgetShowNew $ anschlussAuswahlNew maybeTVar $ anzeige richtung
            liftIO
                $ Gtk.gridAttachNextTo
                    märklinGrid
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
        <- boxPackWidgetNewDefault legoVBox $ anschlussAuswahlNew maybeTVar Language.richtungen
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
    boxPackWidgetNewDefault vBox
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

hinzufügenKupplungNew :: (SpracheGuiReader r m, MonadIO m)
                       => Maybe (TVar (Maybe [Sprache -> IO ()]))
                       -> m HinzufügenSeite
hinzufügenKupplungNew maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    kupplungsAuswahl
        <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.kupplung
    pure HinzufügenSeiteKupplung { vBox, nameAuswahl, kupplungsAuswahl }

hinzufügenWegstreckeNew :: (SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
                         => AuswahlWidget Zugtyp
                         -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                         -> m HinzufügenSeite
hinzufügenWegstreckeNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    DynamischeWidgets { vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
                      , vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
                      , vBoxHinzufügenWegstreckeStreckenabschnitte
                      , vBoxHinzufügenWegstreckeWeichenMärklin
                      , vBoxHinzufügenWegstreckeWeichenLego
                      , vBoxHinzufügenWegstreckeKupplungen
                      , fortfahrenWennToggledWegstrecke} <- erhalteDynamischeWidgets
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
    pure
        HinzufügenSeiteWegstrecke
            { vBox
            , nameAuswahl
            , buttonHinzufügenWegstrecke = fortfahrenWennToggledWegstrecke
            }

hinzufügenPlanNew :: (MitWindow p, SpracheGuiReader r m, DynamischeWidgetsReader r m, MonadIO m)
                   => p
                   -> AuswahlWidget Zugtyp
                   -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                   -> m HinzufügenSeite
hinzufügenPlanNew parent auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    DynamischeWidgets
        { vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
        , vBoxHinzufügenPlanBahngeschwindigkeitenLego
        , vBoxHinzufügenPlanStreckenabschnitte
        , vBoxHinzufügenPlanWeichenGeradeMärklin
        , vBoxHinzufügenPlanWeichenKurveMärklin
        , vBoxHinzufügenPlanWeichenLinksMärklin
        , vBoxHinzufügenPlanWeichenRechtsMärklin
        , vBoxHinzufügenPlanWeichenGeradeLego
        , vBoxHinzufügenPlanWeichenKurveLego
        , vBoxHinzufügenPlanWeichenLinksLego
        , vBoxHinzufügenPlanWeichenRechtsLego
        , vBoxHinzufügenPlanKupplungen
        , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
        , vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
        , vBoxHinzufügenPlanWegstreckenKupplungMärklin
        , vBoxHinzufügenPlanWegstreckenMärklin
        , vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
        , vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
        , vBoxHinzufügenPlanWegstreckenKupplungLego
        , vBoxHinzufügenPlanWegstreckenLego
        , vBoxHinzufügenPlanPläne
        , tmvarPlanObjekt} <- erhalteDynamischeWidgets
    spracheGui <- erhalteSpracheGui
    (tvarAktionen, expanderAktionen, vBoxAktionen) <- liftIO $ do
        tvarAktionen <- newTVarIO leer
        expanderAktionen <- widgetShowNew
            $ Gtk.expanderNew (leseSprache (Language.aktionen <:> (0 :: Int)) spracheGui)
        vBoxAktionen <- containerAddWidgetNew expanderAktionen $ Gtk.vBoxNew False 0
        pure (tvarAktionen, expanderAktionen, vBoxAktionen)
    let aktualisiereExpanderText :: Warteschlange (Aktion, Gtk.Label) -> IO ()
        aktualisiereExpanderText aktionen = do
            Gtk.set
                expanderAktionen
                [ Gtk.expanderLabel
                      := leseSprache (Language.aktionen <:> length aktionen) spracheGui]
    (windowObjektAuswahl, sBG, sST, sGerade, sKurve, sLinks, sRechts, sKU, sWS, sPL) <- liftIO $ do
        windowObjektAuswahl <- Gtk.windowNew
        Gtk.set
            windowObjektAuswahl
            [Gtk.windowTransientFor := erhalteWindow parent, Gtk.windowModal := True]
        Gtk.on windowObjektAuswahl Gtk.deleteEvent $ liftIO $ do
            atomically $ putTMVar tmvarPlanObjekt Nothing
            pure True
        vBox <- containerAddWidgetNew windowObjektAuswahl $ Gtk.vBoxNew False 0
        ztBahngeschwindigkeiten <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenLego)]
                auswahlZugtyp
        boxPackDefault vBox vBoxHinzufügenPlanStreckenabschnitte
        ztWeichenGerade <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWeichenGeradeMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWeichenGeradeLego)]
                auswahlZugtyp
        ztWeichenKurve <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWeichenKurveMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWeichenKurveLego)]
                auswahlZugtyp
        ztWeichenLinks <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWeichenLinksMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWeichenLinksLego)]
                auswahlZugtyp
        ztWeichenRechts <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWeichenRechtsMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWeichenRechtsLego)]
                auswahlZugtyp
        boxPackDefault vBox vBoxHinzufügenPlanKupplungen
        ztWegstreckenBG <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ ( Märklin
                  , erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
                  )
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego)]
                auswahlZugtyp
        ztWegstreckenST <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenStreckenabschnittLego)]
                auswahlZugtyp
        ztWegstreckenKU <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWegstreckenKupplungLego)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenKupplungMärklin)]
                auswahlZugtyp
        ztWegstreckenWS <- boxPackWidgetNewDefault vBox
            $ zugtypSpezifischNew
                [ (Märklin, erhalteWidget vBoxHinzufügenPlanWegstreckenMärklin)
                , (Lego, erhalteWidget vBoxHinzufügenPlanWegstreckenLego)]
                auswahlZugtyp
        boxPackDefault vBox vBoxHinzufügenPlanPläne
        let hideExcept :: [Gtk.Widget] -> IO ()
            hideExcept shownWidgets =
                mapM_
                    (\widget -> widgetShowIf (erhalteWidget widget `elem` shownWidgets) widget)
                    ([ erhalteWidget ztBahngeschwindigkeiten
                     , erhalteWidget vBoxHinzufügenPlanStreckenabschnitte
                     , erhalteWidget ztWeichenGerade
                     , erhalteWidget ztWeichenKurve
                     , erhalteWidget ztWeichenLinks
                     , erhalteWidget ztWeichenRechts
                     , erhalteWidget vBoxHinzufügenPlanKupplungen
                     , erhalteWidget ztWegstreckenBG
                     , erhalteWidget ztWegstreckenST
                     , erhalteWidget ztWegstreckenKU
                     , erhalteWidget ztWegstreckenWS
                     , erhalteWidget vBoxHinzufügenPlanPläne] :: [Gtk.Widget])
            showBG :: IO ()
            showBG =
                hideExcept [erhalteWidget ztBahngeschwindigkeiten, erhalteWidget ztWegstreckenBG]
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
            , showWS
            , showPL
            )
    -- TODO Aktions-Auswahl; StreckenObjekt-Auswahl
    -- evtl. über ComboBox?
    -- TODO Rückgängig-Button
    boxPackDefault vBox expanderAktionen
    buttonHinzufügenPlan <- liftIO $ do
        buttonHinzufügenPlan <- Gtk.buttonNew
        Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := False]
        pure buttonHinzufügenPlan
    boxPackWidgetNewDefault vBox $ buttonNewWithEventLabel maybeTVar Language.rückgängig $ do
        aktuelleAktionen <- readTVarIO tvarAktionen
        neueAktionen <- case zeigeLetztes aktuelleAktionen of
            Leer -> do
                Gtk.set buttonHinzufügenPlan [Gtk.widgetSensitive := False]
                pure leer
            Gefüllt (_aktion, widget) t -> do
                Gtk.containerRemove vBoxAktionen widget
                Gtk.widgetDestroy widget
                pure t
        atomically $ writeTVar tvarAktionen neueAktionen
        aktualisiereExpanderText neueAktionen
    checkButtonDauerschleife <- liftIO $ boxPackWidgetNewDefault vBox Gtk.checkButtonNew
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.set checkButtonDauerschleife [Gtk.buttonLabel := Language.dauerschleife sprache]
        Gtk.set buttonHinzufügenPlan [Gtk.buttonLabel := Language.hinzufügen sprache]
    pure
        HinzufügenSeitePlan
            { vBox
            , nameAuswahl
            , tvarAktionen
            , checkButtonDauerschleife
            , buttonHinzufügenPlan
            }
#endif
--
