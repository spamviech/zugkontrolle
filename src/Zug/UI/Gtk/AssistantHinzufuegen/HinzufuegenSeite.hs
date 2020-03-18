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
import Control.Concurrent.STM (TVar, atomically, readTVarIO)
import Control.Lens ((^.))
import Control.Monad (forM, foldM)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (Foldable(..))
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust, isJust, catMaybes)
import qualified Graphics.UI.Gtk as Gtk

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
import Zug.UI.Gtk.Auswahl (AuswahlWidget, MitAuswahlWidget(), aktuelleAuswahl)
import Zug.UI.Gtk.Fliessend (FließendAuswahlWidget, aktuellerFließendValue)
import Zug.UI.Gtk.FortfahrenWennToggled
       (RegistrierterCheckButton, registrierterCheckButtonNew, registrierterCheckButtonToggled)
import Zug.UI.Gtk.Hilfsfunktionen (widgetShowNew, boxPackWidgetNewDefault, NameAuswahlWidget
                                 , nameAuswahlPackNew, aktuellerName)
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader())
import Zug.UI.Gtk.StreckenObjekt
       (ObjektGui, StatusVarGui, WegstreckenElement(..), WegstreckeCheckButton(), WidgetsTyp(..)
      , BGWidgets(), WEWidgets(), widgetHinzufügenToggled, widgetHinzufügenAktuelleAuswahl)
import Zug.UI.Gtk.ZugtypSpezifisch (ZugtypSpezifisch(), zugtypSpezifischNew)
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
    | HinzufügenSeiteWegstrecke { vBox :: Gtk.VBox, nameAuswahl :: NameAuswahlWidget }
    | HinzufügenSeitePlan
          { vBox :: Gtk.VBox
          , nameAuswahl :: NameAuswahlWidget
          , tvarAktionen :: TVar (Warteschlange Aktion)
          , checkButtonDauerschleife :: Gtk.CheckButton
          }
    deriving (Eq)

instance MitWidget HinzufügenSeite where
    erhalteWidget :: HinzufügenSeite -> Gtk.Widget
    erhalteWidget = Gtk.toWidget . vBox

-- | Erhalte das Ergebnis einer 'HinzufügenSeite'.
seiteErgebnis :: forall r m.
              (StatusVarReader r ObjektGui m, MonadIO m)
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
        True
         -> let plan =
                    Plan
                    { plName
                    , plAktionen = toList $ anhängen (AktionAusführen plan) aktionenWarteschlange
                    }
            in plan
        False -> Plan { plName, plAktionen = toList aktionenWarteschlange }

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
    stromAuswahl
        <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.geschwindigkeit
    pure HinzufügenSeiteStreckenabschnitt { vBox, nameAuswahl, stromAuswahl }

hinzufügenWeicheNew :: (SpracheGuiReader r m, MonadIO m)
                     => AuswahlWidget Zugtyp
                     -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                     -> m HinzufügenSeite
hinzufügenWeicheNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    märklinRichtungsAuswahl <- _undefined --TODO
    legoRichtungsAuswahl <- _undefined --TODO
    legoRichtungenAuswahl <- _undefined --TODO
    pure
        HinzufügenSeiteWeiche
            { vBox
            , nameAuswahl
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
        <- boxPackWidgetNewDefault vBox $ anschlussAuswahlNew maybeTVar Language.geschwindigkeit
    pure HinzufügenSeiteKupplung { vBox, nameAuswahl, kupplungsAuswahl }

hinzufügenWegstreckeNew :: (SpracheGuiReader r m, MonadIO m)
                         => AuswahlWidget Zugtyp
                         -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                         -> m HinzufügenSeite
hinzufügenWegstreckeNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    -- TODO Boxen mit CheckButton
    pure HinzufügenSeiteWegstrecke { vBox, nameAuswahl }

hinzufügenPlanNew :: (SpracheGuiReader r m, MonadIO m)
                   => AuswahlWidget Zugtyp
                   -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                   -> m HinzufügenSeite
hinzufügenPlanNew auswahlZugtyp maybeTVar = do
    vBox <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahl <- nameAuswahlPackNew vBox maybeTVar
    tvarAktionen <- _undefined --TODO
    checkButtonDauerschleife <- _undefined --TODO
    pure HinzufügenSeitePlan { vBox, nameAuswahl, tvarAktionen, checkButtonDauerschleife }
#endif
--
