{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
#endif

{-|
Description : Erstellen eines Assistant zum Hinzufügen eines 'StreckenObjekt'es.
-}
module Zug.UI.Gtk.Fenster.AssistantHinzufuegen
  (
#ifdef ZUGKONTROLLEGUI
    assistantHinzufügenNew
  , HinzufügenSeite()
  , hinzufügenErgebnis
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
       (atomically, takeTMVar, putTMVar, TVar, newTVarIO, readTVarIO, readTVar, writeTVar)
import Control.Lens ((^.))
import Control.Monad (void, foldM, forM, forM_)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybe, fromJust, isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(), Streckenabschnitt(..)
                    , StreckenabschnittKlasse(), Weiche(..), Kupplung(..), KupplungKlasse()
                    , Wegstrecke(..), WegstreckeKlasse(), Wartezeit(..))
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(..), Richtung(..)
                , unterstützteRichtungen, Fahrtrichtung(..), Strom(..))
import qualified Zug.Language as Language
import Zug.Language (Sprache(), MitSprache(..), Anzeige(..), (<:>))
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (Plan(..), Aktion(..), AktionWegstrecke(..), AktionBahngeschwindigkeit(..)
               , AktionStreckenabschnitt(..), AktionWeiche(..), AktionKupplung(..))
import Zug.UI.Base (ObjektReader, bahngeschwindigkeiten, streckenabschnitte, weichen, kupplungen)
import Zug.UI.Gtk.Anschluss (AnschlussAuswahlWidget, anschlussAuswahlNew, aktuellerAnschluss)
import Zug.UI.Gtk.Assistant
       (Assistant, AssistantSeite(..), SeitenAbschluss(..), AssistantSeitenBaum(..), assistantNew)
import Zug.UI.Gtk.Auswahl (AuswahlWidget, auswahlComboBoxNew, boundedEnumAuswahlComboBoxNew
                         , boundedEnumAuswahlRadioButtonNew, aktuelleAuswahl, MitAuswahlWidget())
import Zug.UI.Gtk.Fliessend (FließendAuswahlWidget, fließendAuswahlNew, aktuellerFließendValue)
import Zug.UI.Gtk.FortfahrenWennToggled
       (checkButtons, fortfahrenWennToggledNew, aktiviereWennToggledVar, RegistrierterCheckButton
      , registrierterCheckButtonToggled)
import Zug.UI.Gtk.Hilfsfunktionen
       (boxPackWidgetNewDefault, boxPackDefault, widgetShowNew, containerAddWidgetNew
      , boxPackWidgetNew, Packing(..), paddingDefault, positionDefault, buttonNewWithEventLabel
      , labelSpracheNew, widgetShowIf, NameAuswahlWidget, nameAuswahlPackNew, aktuellerName)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitWidgetShow, mitWidgetHide, MitBox(..), mitContainerAdd
                         , mitContainerRemove, MitEntry(..), MitButton(..), MitWindow(..))
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(..), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt
       (ObjektGui, StatusVarGui, DynamischeWidgets(..), DynamischeWidgetsReader(..)
      , BoxPlanHinzufügen, WegstreckenElement(..), WegstreckeCheckButton(), WidgetsTyp(..)
      , widgetHinzufügenToggled, widgetHinzufügenAktuelleAuswahl
      , widgetHinzufügenContainerGefüllt, BGWidgets, WEWidgets)
import Zug.UI.Gtk.ZugtypSpezifisch (zugtypSpezifischNew, zugtypSpezifischButtonNew)
import Zug.UI.StatusVar (readStatusVar, StatusVarReader(..))
-- Abhängigkeiten von anderen Modulen
import Zug.Warteschlange (Warteschlange, Anzeige(..), leer, anhängen, zeigeLetztes)

-- | Seiten des Hinzufügen-'Assistant'
data HinzufügenSeite
    = HinzufügenSeiteAuswahl
          { widget :: Gtk.Widget
          }
    | HinzufügenSeiteBahngeschwindigkeit
          { widget :: Gtk.Widget,
            nameAuswahl :: NameAuswahlWidget,
            geschwindigkeitAuswahl :: AnschlussAuswahlWidget
            -- Lego
            ,
            fahrtrichtungsAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteStreckenabschnitt
          { widget :: Gtk.Widget,
            nameAuswahl :: NameAuswahlWidget,
            stromAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteWeiche
          { widget :: Gtk.Widget,
            nameAuswahl :: NameAuswahlWidget
            -- Märklin
            ,
            märklinRichtungsAuswahl
                :: NonEmpty (Richtung, RegistrierterCheckButton, AnschlussAuswahlWidget)
            -- Lego
            ,
            legoRichtungsAuswahl :: AnschlussAuswahlWidget,
            legoRichtungenAuswahl :: AuswahlWidget (Richtung, Richtung)
          }
    | HinzufügenSeiteKupplung
          { widget :: Gtk.Widget,
            nameAuswahl :: NameAuswahlWidget,
            kupplungsAuswahl :: AnschlussAuswahlWidget
          }
    | HinzufügenSeiteWegstrecke
          { widget :: Gtk.Widget,
            nameAuswahl :: NameAuswahlWidget
          }
    | HinzufügenSeitePlan
          { widget :: Gtk.Widget,
            nameAuswahl :: NameAuswahlWidget,
            tvarAktionen :: TVar (Warteschlange Aktion),
            checkButtonDauerschleife :: Gtk.CheckButton
          }
    deriving (Eq)

instance MitWidget HinzufügenSeite where
    erhalteWidget :: HinzufügenSeite -> Gtk.Widget
    erhalteWidget = widget

-- | Erhalte das Ergebnis einer 'HinzufügenSeite'.
hinzufügenErgebnis :: forall r m.
                    (StatusVarReader r ObjektGui m, MonadIO m)
                    => AuswahlWidget Zugtyp
                    -> FließendAuswahlWidget
                    -> NonEmpty HinzufügenSeite
                    -> m Objekt
hinzufügenErgebnis
    zugtypAuswahl
    fließendAuswahl
    gezeigteSeiten = case NonEmpty.last gezeigteSeiten of
    HinzufügenSeiteAuswahl {} -> error "Auswahl-Seite zum Hinzufügen als letzte Seite angezeigt"
    HinzufügenSeiteBahngeschwindigkeit
        {nameAuswahl, geschwindigkeitAuswahl, fahrtrichtungsAuswahl} -> do
            name <- aktuellerName nameAuswahl
            fließend <- aktuellerFließendValue fließendAuswahl
            geschwindigkeitsAnschluss <- aktuellerAnschluss geschwindigkeitAuswahl
            aktuelleAuswahl zugtypAuswahl >>= \case
                Märklin -> pure
                    $ OBahngeschwindigkeit
                    $ ZugtypMärklin
                        MärklinBahngeschwindigkeit
                            { bgmName = name,
                              bgmFließend = fließend,
                              bgmGeschwindigkeitsAnschluss = geschwindigkeitsAnschluss
                            }
                Lego -> do
                    bglFahrtrichtungsAnschluss <- aktuellerAnschluss fahrtrichtungsAuswahl
                    pure
                        $ OBahngeschwindigkeit
                        $ ZugtypLego
                            LegoBahngeschwindigkeit
                                { bglName = name,
                                  bglFließend = fließend,
                                  bglGeschwindigkeitsAnschluss = geschwindigkeitsAnschluss,
                                  bglFahrtrichtungsAnschluss
                                }
    HinzufügenSeiteStreckenabschnitt {nameAuswahl, stromAuswahl} -> do
        stName <- aktuellerName nameAuswahl
        stFließend <- aktuellerFließendValue fließendAuswahl
        stromAnschluss <- aktuellerAnschluss stromAuswahl
        pure
            $ OStreckenabschnitt
                Streckenabschnitt
                    { stName,
                      stFließend,
                      stromAnschluss
                    }
    HinzufügenSeiteWeiche
        {nameAuswahl, märklinRichtungsAuswahl, legoRichtungsAuswahl, legoRichtungenAuswahl} -> do
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
                                { wemName = name,
                                  wemFließend = fließend,
                                  wemRichtungsAnschlüsse
                                }
                Lego -> do
                    welRichtungen <- aktuelleAuswahl legoRichtungenAuswahl
                    welRichtungsAnschluss <- aktuellerAnschluss legoRichtungsAuswahl
                    pure
                        $ OWeiche
                        $ ZugtypLego
                            LegoWeiche
                                { welName = name,
                                  welFließend = fließend,
                                  welRichtungen,
                                  welRichtungsAnschluss
                                }
    HinzufügenSeiteKupplung {nameAuswahl, kupplungsAuswahl} -> do
        kuName <- aktuellerName nameAuswahl
        kuFließend <- aktuellerFließendValue fließendAuswahl
        kupplungsAnschluss <- aktuellerAnschluss kupplungsAuswahl
        pure
            $ OKupplung
                Kupplung
                    { kuName,
                      kuFließend,
                      kupplungsAnschluss
                    }
    HinzufügenSeiteWegstrecke {nameAuswahl} -> do
        statusVar <- erhalteStatusVar :: m StatusVarGui
        aktuellerStatus <- liftIO $ atomically $ readStatusVar statusVar
        wsName <- aktuellerName nameAuswahl
        let gewählteWegstrecke
                :: (MonadIO m,
                    ZugtypKlasse z,
                    WegstreckenElement (BGWidgets z),
                    WegstreckenElement (WEWidgets z),
                    MitAuswahlWidget (WegstreckeCheckButton (CheckButtonAuswahl (WEWidgets z))) Richtung
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
                        { wsName,
                          wsBahngeschwindigkeiten,
                          wsStreckenabschnitte,
                          wsWeichenRichtungen,
                          wsKupplungen
                        }
            anhängenWennToggled :: (WidgetsTyp a, WegstreckenElement a, MonadIO m)
                                 => [ObjektTyp a]
                                 -> a
                                 -> m [ObjektTyp a]
            anhängenWennToggled acc a = widgetHinzufügenToggled (a ^. getterWegstrecke) >>= \case
                True -> pure $ erhalteObjektTyp a : acc
                False -> pure acc
            weichenRichtungAnhängenWennToggled
                :: (WegstreckenElement (WEWidgets z),
                    MonadIO m,
                    MitAuswahlWidget (WegstreckeCheckButton (CheckButtonAuswahl (WEWidgets z))) Richtung
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
    HinzufügenSeitePlan {nameAuswahl, tvarAktionen, checkButtonDauerschleife} -> liftIO $ do
        plName <- aktuellerName nameAuswahl
        aktionen <- toList <$> readTVarIO tvarAktionen
        Gtk.get checkButtonDauerschleife Gtk.toggleButtonActive >>= pure . OPlan . \case
            True -> let plan =
                            Plan
                            { plName,
                              plAktionen = aktionen ++ [AktionAusführen plan]
                            }
                    in plan
            False -> Plan
                { plName,
                  plAktionen = aktionen
                }

-- | Erstelle einen neuen Hinzufügen-'Assistant'.
--
-- Wird eine 'TVar' übergeben kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
assistantHinzufügenNew :: (MitWindow w, ObjektReader ObjektGui m, MonadIO m)
                        => w
                        -> Maybe (TVar (Maybe [Sprache -> IO ()]))
                        -> m (Assistant HinzufügenSeite Objekt)
assistantHinzufügenNew parent maybeTVar = do
    objektReader <- ask
    DynamischeWidgets
        {fortfahrenWennToggledWegstrecke,
         tmvarPlanObjekt,
         vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin,
         vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego,
         vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
         vBoxHinzufügenPlanBahngeschwindigkeitenLego,
         vBoxHinzufügenWegstreckeStreckenabschnitte,
         vBoxHinzufügenPlanStreckenabschnitte,
         vBoxHinzufügenWegstreckeWeichenMärklin,
         vBoxHinzufügenWegstreckeWeichenLego,
         vBoxHinzufügenPlanWeichenGeradeMärklin,
         vBoxHinzufügenPlanWeichenKurveMärklin,
         vBoxHinzufügenPlanWeichenLinksMärklin,
         vBoxHinzufügenPlanWeichenRechtsMärklin,
         vBoxHinzufügenPlanWeichenGeradeLego,
         vBoxHinzufügenPlanWeichenKurveLego,
         vBoxHinzufügenPlanWeichenLinksLego,
         vBoxHinzufügenPlanWeichenRechtsLego,
         vBoxHinzufügenWegstreckeKupplungen,
         vBoxHinzufügenPlanKupplungen,
         vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
         vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
         vBoxHinzufügenPlanWegstreckenKupplungMärklin,
         vBoxHinzufügenPlanWegstreckenMärklin,
         vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
         vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
         vBoxHinzufügenPlanWegstreckenKupplungLego,
         vBoxHinzufügenPlanWegstreckenLego,
         vBoxHinzufügenPlanPläne} <- erhalteDynamischeWidgets
    -- Globale Widgets
    zugtypAuswahl <- boundedEnumAuswahlComboBoxNew Märklin maybeTVar Language.zugtyp
    fließendAuswahl <- fließendAuswahlNew maybeTVar
    let globaleWidgets = [Left zugtypAuswahl, Right fließendAuswahl]
    -- Dummy-Widget zur Seitenauswahl. Auswahl wird durch Assistant übernommen.
    auswahl <- liftIO $ erhalteWidget <$> Gtk.labelNew (Nothing :: Maybe Text)
    let seiteAuswahl :: AssistantSeite HinzufügenSeite
        seiteAuswahl =
            AssistantSeite
            { seite = HinzufügenSeiteAuswahl
                  { widget = auswahl
                  },
              name = Language.hinzufügen,
              seiteZurücksetzen = pure (),
              seitenAbschluss = SeitenAbschluss Language.weiter
            }
    -- Bahngeschwindigkeit
    boxBahngeschwindigkeit <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahlBahngeschwindigkeit <- nameAuswahlPackNew boxBahngeschwindigkeit maybeTVar
    geschwindigkeitAuswahl <- boxPackWidgetNewDefault boxBahngeschwindigkeit
        $ anschlussAuswahlNew maybeTVar Language.geschwindigkeit
    legoBoxBahngeschwindigkeit <- liftIO $ erhalteBox <$> Gtk.vBoxNew False 0
    fahrtrichtungsAuswahl <- boxPackWidgetNewDefault legoBoxBahngeschwindigkeit
        $ anschlussAuswahlNew maybeTVar Language.fahrtrichtung
    boxPackWidgetNewDefault boxBahngeschwindigkeit
        $ zugtypSpezifischNew ((Lego, legoBoxBahngeschwindigkeit) :| []) zugtypAuswahl
    let seiteBahngeschwindigkeit :: AssistantSeite HinzufügenSeite
        seiteBahngeschwindigkeit =
            AssistantSeite
            { seite = HinzufügenSeiteBahngeschwindigkeit
                  { widget = erhalteWidget boxBahngeschwindigkeit,
                    nameAuswahl = nameAuswahlBahngeschwindigkeit,
                    geschwindigkeitAuswahl,
                    fahrtrichtungsAuswahl
                  },
              name = Language.bahngeschwindigkeit,
              seiteZurücksetzen = Gtk.set
                  (erhalteEntry nameAuswahlBahngeschwindigkeit)
                  [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True],
              seitenAbschluss = SeitenAbschluss Language.hinzufügen
            }
    -- Streckenabschnitt
    boxStreckenabschnitt <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahlStreckenabschnitt <- nameAuswahlPackNew boxStreckenabschnitt maybeTVar
    stromAuswahl <- boxPackWidgetNewDefault boxStreckenabschnitt
        $ anschlussAuswahlNew maybeTVar Language.strom
    let seiteStreckenabschnitt :: AssistantSeite HinzufügenSeite
        seiteStreckenabschnitt =
            AssistantSeite
            { seite = HinzufügenSeiteStreckenabschnitt
                  { widget = erhalteWidget boxStreckenabschnitt,
                    nameAuswahl = nameAuswahlStreckenabschnitt,
                    stromAuswahl
                  },
              name = Language.streckenabschnitt,
              seiteZurücksetzen = Gtk.set
                  (erhalteEntry nameAuswahlStreckenabschnitt)
                  [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True],
              seitenAbschluss = SeitenAbschluss Language.hinzufügen
            }
    -- Weiche
    boxWeiche <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahlWeiche <- nameAuswahlPackNew boxWeiche maybeTVar
    märklinBoxWeiche <- liftIO $ erhalteBox <$> Gtk.vBoxNew False 0
    märklinFortfahrenWennToggledTMVar <- fortfahrenWennToggledNew maybeTVar Language.hinzufügen
        $ anzeige <$> unterstützteRichtungen
    let richtungsCheckButtons :: NonEmpty (Richtung, RegistrierterCheckButton)
        richtungsCheckButtons =
            NonEmpty.zip unterstützteRichtungen $ checkButtons märklinFortfahrenWennToggledTMVar
    märklinRichtungsAuswahl <- forM richtungsCheckButtons $ \(richtung, checkButton) -> do
        box <- liftIO $ boxPackWidgetNewDefault märklinBoxWeiche $ Gtk.hBoxNew False 0
        boxPackDefault box checkButton
        anschlussAuswahl
            <- boxPackWidgetNewDefault box $ anschlussAuswahlNew maybeTVar $ anzeige richtung
        pure (richtung, checkButton, anschlussAuswahl)
    legoBoxWeiche <- liftIO $ erhalteBox <$> Gtk.vBoxNew False 0
    legoSeitenAbschluss <- liftIO $ Gtk.buttonNew
    verwendeSpracheGui maybeTVar
        $ \sprache -> Gtk.set legoSeitenAbschluss [Gtk.buttonLabel := Language.hinzufügen sprache]
    legoRichtungsAuswahl
        <- boxPackWidgetNewDefault legoBoxWeiche $ anschlussAuswahlNew maybeTVar Language.richtung
    let richtungen =
            (Gerade, Kurve)
            :| [(Gerade, Links),
                (Gerade, Rechts),
                (Kurve, Links),
                (Kurve, Rechts),
                (Links, Rechts)]
    legoRichtungenAuswahl <- boxPackWidgetNewDefault legoBoxWeiche
        $ auswahlComboBoxNew richtungen maybeTVar Language.richtungen
    seitenAbschlussWeiche <- SeitenAbschlussZugtyp
        <$> zugtypSpezifischButtonNew
            ((Märklin, erhalteButton märklinFortfahrenWennToggledTMVar)
             :| [(Lego, legoSeitenAbschluss)])
            zugtypAuswahl
    boxPackWidgetNewDefault boxWeiche
        $ zugtypSpezifischNew
            ((Märklin, märklinBoxWeiche) :| [(Lego, legoBoxWeiche)])
            zugtypAuswahl
    let seiteWeiche :: AssistantSeite HinzufügenSeite
        seiteWeiche =
            AssistantSeite
            { seite = HinzufügenSeiteWeiche
                  { widget = erhalteWidget boxWeiche,
                    nameAuswahl = nameAuswahlWeiche,
                    märklinRichtungsAuswahl,
                    legoRichtungsAuswahl,
                    legoRichtungenAuswahl
                  },
              name = Language.weiche,
              seiteZurücksetzen = Gtk.set
                  (erhalteEntry nameAuswahlWeiche)
                  [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True],
              seitenAbschluss = seitenAbschlussWeiche
            }
    -- Kupplung
    boxKupplung <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahlKupplung <- nameAuswahlPackNew boxKupplung maybeTVar
    kupplungsAuswahl
        <- boxPackWidgetNewDefault boxKupplung $ anschlussAuswahlNew maybeTVar Language.kuppeln
    let seiteKupplung :: AssistantSeite HinzufügenSeite
        seiteKupplung =
            AssistantSeite
            { seite = HinzufügenSeiteKupplung
                  { widget = erhalteWidget boxKupplung,
                    nameAuswahl = nameAuswahlKupplung,
                    kupplungsAuswahl
                  },
              name = Language.kupplung,
              seiteZurücksetzen = Gtk.set
                  (erhalteEntry nameAuswahlKupplung)
                  [Gtk.entryText := ("" :: Text), Gtk.widgetHasFocus := True],
              seitenAbschluss = SeitenAbschluss Language.hinzufügen
            }
    -- Wegstrecke
    boxWegstrecke <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahlWegstrecke <- nameAuswahlPackNew boxWegstrecke maybeTVar
    liftIO $ do
        hPaned <- boxPackWidgetNewDefault boxWegstrecke Gtk.hPanedNew
        vPanedLeft <- widgetShowNew Gtk.vPanedNew
        Gtk.panedAdd1 hPaned vPanedLeft
        frameLeftTop <- widgetShowNew Gtk.frameNew
        Gtk.set frameLeftTop [Gtk.frameShadowType := Gtk.ShadowIn]
        Gtk.panedAdd1 vPanedLeft frameLeftTop
        frameLeftBot <- widgetShowNew Gtk.frameNew
        Gtk.set frameLeftBot [Gtk.frameShadowType := Gtk.ShadowIn]
        Gtk.panedAdd2 vPanedLeft frameLeftBot
        vPanedRight <- widgetShowNew Gtk.vPanedNew
        Gtk.panedAdd2 hPaned vPanedRight
        frameRightTop <- widgetShowNew Gtk.frameNew
        Gtk.set frameRightTop [Gtk.frameShadowType := Gtk.ShadowIn]
        Gtk.panedAdd1 vPanedRight frameRightTop
        frameRightBot <- widgetShowNew Gtk.frameNew
        Gtk.set frameRightBot [Gtk.frameShadowType := Gtk.ShadowIn]
        Gtk.panedAdd2 vPanedRight frameRightBot
        containerAddWidgetNew frameLeftTop
            $ flip zugtypSpezifischNew zugtypAuswahl
            $ (Märklin, erhalteWidget vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin)
            :| [(Lego, erhalteWidget vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego)]
        mitContainerAdd frameLeftBot vBoxHinzufügenWegstreckeStreckenabschnitte
        containerAddWidgetNew frameRightTop
            $ flip zugtypSpezifischNew zugtypAuswahl
            $ (Märklin, erhalteWidget vBoxHinzufügenWegstreckeWeichenMärklin)
            :| [(Lego, erhalteWidget vBoxHinzufügenWegstreckeWeichenLego)]
        mitContainerAdd frameRightBot vBoxHinzufügenWegstreckeKupplungen
    let seiteZurücksetzenWegstrecke :: IO ()
        seiteZurücksetzenWegstrecke = do
            aktiviereWennToggledVar fortfahrenWennToggledWegstrecke
            Gtk.set
                (erhalteEntry nameAuswahlWegstrecke)
                [Gtk.entryText := Text.empty, Gtk.widgetHasFocus := True]
        seiteWegstrecke :: AssistantSeite HinzufügenSeite
        seiteWegstrecke =
            AssistantSeite
            { seite = HinzufügenSeiteWegstrecke
                  { widget = erhalteWidget boxWegstrecke,
                    nameAuswahl = nameAuswahlWegstrecke
                  },
              name = Language.wegstrecke,
              seiteZurücksetzen = seiteZurücksetzenWegstrecke,
              seitenAbschluss = SeitenAbschlussToggledVar fortfahrenWennToggledWegstrecke
            }
    -- Hilfsdialog für Plan
    windowAktionObjektAuswahl <- liftIO Gtk.windowNew
    liftIO $ Gtk.on windowAktionObjektAuswahl Gtk.deleteEvent $ liftIO $ do
        atomically $ putTMVar tmvarPlanObjekt Nothing
        mitWidgetHide windowAktionObjektAuswahl
        pure True
    boxAktionObjektAuswahl
        <- liftIO $ containerAddWidgetNew windowAktionObjektAuswahl $ Gtk.vBoxNew False 0
    -- Anzeige der Boxen explizit beim Anzeigen des Fensters
    mapM_
        (boxPackDefault boxAktionObjektAuswahl)
        [erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenMärklin,
         erhalteWidget vBoxHinzufügenPlanBahngeschwindigkeitenLego,
         erhalteWidget vBoxHinzufügenPlanStreckenabschnitte,
         erhalteWidget vBoxHinzufügenPlanWeichenGeradeMärklin,
         erhalteWidget vBoxHinzufügenPlanWeichenKurveMärklin,
         erhalteWidget vBoxHinzufügenPlanWeichenLinksMärklin,
         erhalteWidget vBoxHinzufügenPlanWeichenRechtsMärklin,
         erhalteWidget vBoxHinzufügenPlanWeichenGeradeLego,
         erhalteWidget vBoxHinzufügenPlanWeichenKurveLego,
         erhalteWidget vBoxHinzufügenPlanWeichenLinksLego,
         erhalteWidget vBoxHinzufügenPlanWeichenRechtsLego,
         erhalteWidget vBoxHinzufügenPlanKupplungen,
         erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin,
         erhalteWidget vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin,
         erhalteWidget vBoxHinzufügenPlanWegstreckenKupplungMärklin,
         erhalteWidget vBoxHinzufügenPlanWegstreckenMärklin,
         erhalteWidget vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego,
         erhalteWidget vBoxHinzufügenPlanWegstreckenStreckenabschnittLego,
         erhalteWidget vBoxHinzufügenPlanWegstreckenKupplungLego,
         erhalteWidget vBoxHinzufügenPlanWegstreckenLego,
         erhalteWidget vBoxHinzufügenPlanPläne]
    boxPackWidgetNewDefault boxAktionObjektAuswahl
        $ buttonNewWithEventLabel maybeTVar Language.abbrechen
        $ atomically
        $ putTMVar tmvarPlanObjekt Nothing
    -- Plan
    boxPlan <- liftIO $ Gtk.vBoxNew False 0
    nameAuswahlPlan <- nameAuswahlPackNew boxPlan maybeTVar
    (expanderAktionen, boxAktionen, seitenAbschlussPlan, tvarAktionen, tvarWidgets) <- liftIO $ do
        expanderAktionen <- widgetShowNew $ Gtk.expanderNew Text.empty
        boxAktionen <- containerAddWidgetNew expanderAktionen $ Gtk.vBoxNew False 0
        seitenAbschlussPlan <- Gtk.buttonNew
        tvarAktionen <- newTVarIO leer
        tvarWidgets <- newTVarIO []
        pure (expanderAktionen, boxAktionen, seitenAbschlussPlan, tvarAktionen, tvarWidgets)
    verwendeSpracheGui maybeTVar $ \sprache -> do
        Gtk.set expanderAktionen [Gtk.expanderLabel := Language.aktionen sprache]
        Gtk.set seitenAbschlussPlan [Gtk.buttonLabel := Language.hinzufügen sprache]
    spracheGui <- erhalteSpracheGui
    let zeigeAktionen :: (Foldable t, MonadIO m) => t Aktion -> m ()
        zeigeAktionen aktionen = liftIO $ do
            widgets <- readTVarIO tvarWidgets
            forM_ widgets $ mitContainerRemove boxAktionen
            -- Verwende aktuelle Sprache, nachdem die Widgets/ExpanderLabel kurzlebig sind
            widgetsNeu <- mapM
                (boxPackWidgetNewDefault boxAktionen
                 . Gtk.labelNew
                 . Just
                 . flip leseSprache spracheGui
                 . anzeige)
                $ toList aktionen
            Gtk.set
                expanderAktionen
                [Gtk.expanderLabel
                     := leseSprache (Language.aktionen <:> length aktionen) spracheGui]
            atomically $ writeTVar tvarWidgets widgetsNeu
            Gtk.set seitenAbschlussPlan [Gtk.widgetSensitive := not $ null aktionen]
        aktionHinzufügen :: Aktion -> IO ()
        aktionHinzufügen aktion = do
            aktionenDanach <- atomically $ do
                aktionen <- readTVar tvarAktionen
                let aktionenDanach = anhängen aktion aktionen
                writeTVar tvarAktionen aktionenDanach
                pure aktionenDanach
            zeigeAktionen aktionenDanach
    -- Warten
    boxAktionWarten <- liftIO $ boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
    let wartenMinµs = 0
        wartenMaxµs = 10000000  -- 10 s
        wartenStepµs = 1000     -- 1 ms
    wartenSpinButton <- liftIO
        $ widgetShowNew
        $ Gtk.spinButtonNewWithRange wartenMinµs wartenMaxµs wartenStepµs
    boxPackWidgetNewDefault boxAktionWarten $ buttonNewWithEventLabel maybeTVar Language.warten $ do
        wartezeit <- MikroSekunden . fromIntegral <$> Gtk.spinButtonGetValueAsInt wartenSpinButton
        aktionHinzufügen $ Warten wartezeit
    boxPackDefault boxAktionWarten wartenSpinButton
    boxPackWidgetNewDefault boxAktionWarten $ labelSpracheNew maybeTVar Language.wartenEinheit
    -- AktionBahngeschwindigkeit 'Märklin
    boxAktionBahngeschwindigkeitMärklin <- liftIO $ Gtk.hBoxNew False 0
    let zeigeMärklinBahngeschwindigkeitAktionAuswahl :: IO ()
        zeigeMärklinBahngeschwindigkeitAktionAuswahl = do
            mitWidgetShow vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
            mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
            mitWidgetHide vBoxHinzufügenPlanKupplungen
            mitWidgetShow vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
            mitWidgetHide vBoxHinzufügenPlanPläne
            mitWidgetShow windowAktionObjektAuswahl
        märklinBahngeschwindigkeitAktionHinzufügen
            :: (forall b.
                (BahngeschwindigkeitKlasse b)
                => b 'Märklin
                -> IO (AktionBahngeschwindigkeit b 'Märklin))
            -> IO ()
        märklinBahngeschwindigkeitAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
            Gtk.postGUIAsync $ zeigeMärklinBahngeschwindigkeitAktionAuswahl
            atomically (takeTMVar tmvarPlanObjekt) >>= \case
                (Just (OBahngeschwindigkeit (ZugtypMärklin bgMärklin))) -> aktionKonstruktor
                    bgMärklin
                    >>= aktionHinzufügen . ABahngeschwindigkeitMärklin
                (Just (OWegstrecke (ZugtypMärklin wsMärklin))) -> aktionKonstruktor wsMärklin
                    >>= aktionHinzufügen . AWegstreckeMärklin . AWSBahngeschwindigkeit
                (Just anderesObjekt) -> error
                    $ "unerwartetes Objekt für Märklin-Bahngeschwindigkeit-Aktion erhalten: "
                    ++ show anderesObjekt
                Nothing -> pure ()
            Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
    märklinGeschwindigkeitsScale <- liftIO
        $ boxPackWidgetNew
            boxAktionBahngeschwindigkeitMärklin
            PackGrow
            paddingDefault
            positionDefault
        $ Gtk.hScaleNewWithRange 0 100 1
    boxPackWidgetNewDefault boxAktionBahngeschwindigkeitMärklin
        $ buttonNewWithEventLabel maybeTVar Language.geschwindigkeit
        $ märklinBahngeschwindigkeitAktionHinzufügen
        $ \bg -> do
            Geschwindigkeit bg . floor <$> Gtk.get märklinGeschwindigkeitsScale Gtk.rangeValue
    boxPackWidgetNewDefault boxAktionBahngeschwindigkeitMärklin
        $ buttonNewWithEventLabel maybeTVar Language.umdrehen
        $ märklinBahngeschwindigkeitAktionHinzufügen
        $ pure . Umdrehen
    -- AktionBahngeschwindigkeit 'Lego
    boxAktionBahngeschwindigkeitLego <- liftIO $ Gtk.hBoxNew False 0
    let zeigeLegoBahngeschwindigkeitAktionAuswahl :: IO ()
        zeigeLegoBahngeschwindigkeitAktionAuswahl = do
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            mitWidgetShow vBoxHinzufügenPlanBahngeschwindigkeitenLego
            mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
            mitWidgetHide vBoxHinzufügenPlanKupplungen
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
            mitWidgetShow vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
            mitWidgetHide vBoxHinzufügenPlanPläne
            mitWidgetShow windowAktionObjektAuswahl
        legoBahngeschwindigkeitAktionHinzufügen
            :: (forall b.
                (BahngeschwindigkeitKlasse b)
                => b 'Lego
                -> IO (AktionBahngeschwindigkeit b 'Lego))
            -> IO ()
        legoBahngeschwindigkeitAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
            Gtk.postGUIAsync $ zeigeLegoBahngeschwindigkeitAktionAuswahl
            atomically (takeTMVar tmvarPlanObjekt) >>= \case
                (Just (OBahngeschwindigkeit (ZugtypLego bgLego)))
                    -> aktionKonstruktor bgLego >>= aktionHinzufügen . ABahngeschwindigkeitLego
                (Just (OWegstrecke (ZugtypLego wsLego))) -> aktionKonstruktor wsLego
                    >>= aktionHinzufügen . AWegstreckeLego . AWSBahngeschwindigkeit
                (Just anderesObjekt) -> error
                    $ "unerwartetes Objekt für Lego-Bahngeschwindigkeit-Aktion erhalten: "
                    ++ show anderesObjekt
                Nothing -> pure ()
            Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
    legoGeschwindigkeitsScale <- liftIO
        $ boxPackWidgetNew boxAktionBahngeschwindigkeitLego PackGrow paddingDefault positionDefault
        $ Gtk.hScaleNewWithRange 0 100 1
    boxPackWidgetNewDefault boxAktionBahngeschwindigkeitLego
        $ buttonNewWithEventLabel maybeTVar Language.geschwindigkeit
        $ legoBahngeschwindigkeitAktionHinzufügen
        $ \bg -> Geschwindigkeit bg . floor <$> Gtk.get legoGeschwindigkeitsScale Gtk.rangeValue
    legoFahrtrichtungAuswahl
        <- widgetShowNew $ boundedEnumAuswahlRadioButtonNew Vorwärts maybeTVar $ const Text.empty
    boxPackWidgetNewDefault boxAktionBahngeschwindigkeitLego
        $ buttonNewWithEventLabel maybeTVar Language.fahrtrichtungEinstellen
        $ legoBahngeschwindigkeitAktionHinzufügen
        $ \bg -> FahrtrichtungEinstellen bg <$> aktuelleAuswahl legoFahrtrichtungAuswahl
    boxPackDefault boxAktionBahngeschwindigkeitLego legoFahrtrichtungAuswahl
    -- ZugtypSpezifisch Bahngeschwindigkeit
    boxPackWidgetNewDefault boxPlan
        $ zugtypSpezifischNew
            ((Märklin, boxAktionBahngeschwindigkeitMärklin)
             :| [(Lego, boxAktionBahngeschwindigkeitLego)])
            zugtypAuswahl
    -- AktionStreckenabschnitt
    boxAktionStreckenabschnitt <- liftIO $ boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
    let zeigeStreckenabschnittAktionAuswahl :: IO ()
        zeigeStreckenabschnittAktionAuswahl = do
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
            mitWidgetShow vBoxHinzufügenPlanStreckenabschnitte
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
            mitWidgetHide vBoxHinzufügenPlanKupplungen
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            mitWidgetShow vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            mitWidgetShow vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
            mitWidgetHide vBoxHinzufügenPlanPläne
            mitWidgetShow windowAktionObjektAuswahl
        streckenabschnittAktionHinzufügen
            :: (forall s. (StreckenabschnittKlasse s) => s -> IO (AktionStreckenabschnitt s))
            -> IO ()
        streckenabschnittAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
            Gtk.postGUIAsync $ zeigeStreckenabschnittAktionAuswahl
            atomically (takeTMVar tmvarPlanObjekt) >>= \case
                (Just (OStreckenabschnitt st))
                    -> aktionKonstruktor st >>= aktionHinzufügen . AStreckenabschnitt
                (Just (OWegstrecke (ZugtypMärklin wsMärklin))) -> aktionKonstruktor wsMärklin
                    >>= aktionHinzufügen . AWegstreckeMärklin . AWSStreckenabschnitt
                (Just (OWegstrecke (ZugtypLego wsLego))) -> aktionKonstruktor wsLego
                    >>= aktionHinzufügen . AWegstreckeLego . AWSStreckenabschnitt
                (Just anderesObjekt) -> error
                    $ "unerwartetes Objekt für Streckenabschnitt-Aktion erhalten: "
                    ++ show anderesObjekt
                Nothing -> pure ()
            Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
    auswahlStrom
        <- widgetShowNew $ boundedEnumAuswahlRadioButtonNew Fließend maybeTVar $ const Text.empty
    boxPackWidgetNewDefault boxAktionStreckenabschnitt
        $ buttonNewWithEventLabel maybeTVar Language.strom
        $ streckenabschnittAktionHinzufügen
        $ \st -> Strom st <$> aktuelleAuswahl auswahlStrom
    boxPackDefault boxAktionStreckenabschnitt auswahlStrom
    -- AktionWeiche
    boxAktionWeiche <- liftIO $ boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
    let zeigeWeicheAktionAuswahl :: Richtung -> IO ()
        zeigeWeicheAktionAuswahl richtung = do
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
            mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
            widgetShowIf (richtung == Gerade) vBoxHinzufügenPlanWeichenGeradeMärklin
            widgetShowIf (richtung == Kurve) vBoxHinzufügenPlanWeichenKurveMärklin
            widgetShowIf (richtung == Links) vBoxHinzufügenPlanWeichenLinksMärklin
            widgetShowIf (richtung == Rechts) vBoxHinzufügenPlanWeichenRechtsMärklin
            widgetShowIf (richtung == Gerade) vBoxHinzufügenPlanWeichenGeradeLego
            widgetShowIf (richtung == Kurve) vBoxHinzufügenPlanWeichenKurveLego
            widgetShowIf (richtung == Links) vBoxHinzufügenPlanWeichenLinksLego
            widgetShowIf (richtung == Rechts) vBoxHinzufügenPlanWeichenRechtsLego
            mitWidgetHide vBoxHinzufügenPlanKupplungen
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
            mitWidgetHide vBoxHinzufügenPlanPläne
            mitWidgetShow windowAktionObjektAuswahl
        weicheAktionHinzufügen :: Richtung -> IO ()
        weicheAktionHinzufügen richtung = void $ forkIO $ do
            Gtk.postGUIAsync $ zeigeWeicheAktionAuswahl richtung
            atomically (takeTMVar tmvarPlanObjekt) >>= \case
                (Just (OWeiche we)) -> aktionHinzufügen $ AWeiche $ Stellen we richtung
                (Just anderesObjekt) -> error
                    $ "unerwartetes Objekt zum Weiche stellen erhalten: " ++ show anderesObjekt
                Nothing -> pure ()
            Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
    buttonAktionWeicheGerade <- boxPackWidgetNewDefault boxAktionWeiche
        $ buttonNewWithEventLabel maybeTVar (Language.stellen <:> Language.gerade)
        $ weicheAktionHinzufügen Gerade
    buttonAktionWeicheKurve <- boxPackWidgetNewDefault boxAktionWeiche
        $ buttonNewWithEventLabel maybeTVar (Language.stellen <:> Language.kurve)
        $ weicheAktionHinzufügen Kurve
    buttonAktionWeicheLinks <- boxPackWidgetNewDefault boxAktionWeiche
        $ buttonNewWithEventLabel maybeTVar (Language.stellen <:> Language.links)
        $ weicheAktionHinzufügen Links
    buttonAktionWeicheRechts <- boxPackWidgetNewDefault boxAktionWeiche
        $ buttonNewWithEventLabel maybeTVar (Language.stellen <:> Language.rechts)
        $ weicheAktionHinzufügen Rechts
    -- AktionKupplung
    boxAktionKupplung <- liftIO $ boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
    let zeigeKupplungAktionAuswahl :: IO ()
        zeigeKupplungAktionAuswahl = do
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
            mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
            mitWidgetShow vBoxHinzufügenPlanKupplungen
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            mitWidgetShow vBoxHinzufügenPlanWegstreckenKupplungMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            mitWidgetShow vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            mitWidgetShow vBoxHinzufügenPlanWegstreckenKupplungLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
            mitWidgetHide vBoxHinzufügenPlanPläne
            mitWidgetShow windowAktionObjektAuswahl
        kupplungAktionHinzufügen :: (forall k. (KupplungKlasse k) => k -> IO (AktionKupplung k))
                                  -> IO ()
        kupplungAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
            Gtk.postGUIAsync $ zeigeKupplungAktionAuswahl
            atomically (takeTMVar tmvarPlanObjekt) >>= \case
                (Just (OKupplung ku)) -> aktionKonstruktor ku >>= aktionHinzufügen . AKupplung
                (Just (OWegstrecke (ZugtypMärklin wsMärklin))) -> aktionKonstruktor wsMärklin
                    >>= aktionHinzufügen . AWegstreckeMärklin . AWSKupplung
                (Just (OWegstrecke (ZugtypLego wsLego))) -> aktionKonstruktor wsLego
                    >>= aktionHinzufügen . AWegstreckeLego . AWSKupplung
                (Just anderesObjekt) -> error
                    $ "unerwartetes Objekt für Streckenabschnitt-Aktion erhalten: "
                    ++ show anderesObjekt
                Nothing -> pure ()
            Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
    boxPackWidgetNewDefault boxAktionKupplung
        $ buttonNewWithEventLabel maybeTVar Language.kuppeln
        $ kupplungAktionHinzufügen
        $ pure . Kuppeln
    -- AktionWegstrecke
    boxAktionWegstrecke <- liftIO $ boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
    let zeigeWegstreckeAktionAuswahl :: IO ()
        zeigeWegstreckeAktionAuswahl = do
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
            mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
            mitWidgetHide vBoxHinzufügenPlanKupplungen
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
            mitWidgetShow vBoxHinzufügenPlanWegstreckenMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
            mitWidgetShow vBoxHinzufügenPlanWegstreckenLego
            mitWidgetHide vBoxHinzufügenPlanPläne
            mitWidgetShow windowAktionObjektAuswahl
        wegstreckeAktionHinzufügen
            :: (forall w z. (WegstreckeKlasse (w z)) => w z -> IO (AktionWegstrecke w z)) -> IO ()
        wegstreckeAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
            Gtk.postGUIAsync $ zeigeWegstreckeAktionAuswahl
            atomically (takeTMVar tmvarPlanObjekt) >>= \case
                (Just (OWegstrecke (ZugtypMärklin wsMärklin)))
                    -> aktionKonstruktor wsMärklin >>= aktionHinzufügen . AWegstreckeMärklin
                (Just (OWegstrecke (ZugtypLego wsLego)))
                    -> aktionKonstruktor wsLego >>= aktionHinzufügen . AWegstreckeLego
                (Just anderesObjekt) -> error
                    $ "unerwartetes Objekt für Wegstrecke-Aktion erhalten: " ++ show anderesObjekt
                Nothing -> pure ()
            Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
    boxPackWidgetNewDefault boxAktionWegstrecke
        $ buttonNewWithEventLabel maybeTVar Language.einstellen
        $ wegstreckeAktionHinzufügen
        $ pure . Einstellen
    -- AktionPlan
    boxAktionPlan <- liftIO $ boxPackWidgetNewDefault boxPlan $ Gtk.hBoxNew False 0
    let zeigePlanAktionAuswahl :: IO ()
        zeigePlanAktionAuswahl = do
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
            mitWidgetHide vBoxHinzufügenPlanBahngeschwindigkeitenLego
            mitWidgetHide vBoxHinzufügenPlanStreckenabschnitte
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsMärklin
            mitWidgetHide vBoxHinzufügenPlanWeichenGeradeLego
            mitWidgetHide vBoxHinzufügenPlanWeichenKurveLego
            mitWidgetHide vBoxHinzufügenPlanWeichenLinksLego
            mitWidgetHide vBoxHinzufügenPlanWeichenRechtsLego
            mitWidgetHide vBoxHinzufügenPlanKupplungen
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenMärklin
            mitWidgetHide vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenStreckenabschnittLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenKupplungLego
            mitWidgetHide vBoxHinzufügenPlanWegstreckenLego
            mitWidgetShow vBoxHinzufügenPlanPläne
            mitWidgetShow windowAktionObjektAuswahl
        planAktionHinzufügen :: (Plan -> IO Aktion) -> IO ()
        planAktionHinzufügen aktionKonstruktor = void $ forkIO $ do
            Gtk.postGUIAsync zeigePlanAktionAuswahl
            atomically (takeTMVar tmvarPlanObjekt) >>= \case
                (Just (OPlan plan)) -> aktionKonstruktor plan >>= aktionHinzufügen
                (Just anderesObjekt) -> error
                    $ "unerwartetes Objekt für Plan-Aktion erhalten: " ++ show anderesObjekt
                Nothing -> pure ()
            Gtk.postGUIAsync $ mitWidgetHide windowAktionObjektAuswahl
    boxPackWidgetNewDefault boxAktionPlan
        $ buttonNewWithEventLabel maybeTVar Language.aktionAusführen
        $ planAktionHinzufügen
        $ pure . AktionAusführen
    -- Zeige aktuelle Aktionen an
    boxPackDefault boxPlan expanderAktionen
    boxPackWidgetNewDefault boxPlan $ buttonNewWithEventLabel maybeTVar Language.rückgängig $ do
        aktionenVorher <- atomically $ do
            aktionen <- readTVar tvarAktionen
            let aktionenVorher = case zeigeLetztes aktionen of
                    Leer -> leer
                    Gefüllt _letztes warteschlange -> warteschlange
            writeTVar tvarAktionen aktionenVorher
            pure aktionenVorher
        zeigeAktionen aktionenVorher
    -- Dauerschleife
    checkButtonDauerschleife <- liftIO $ boxPackWidgetNewDefault boxPlan Gtk.checkButtonNew
    verwendeSpracheGui maybeTVar $ \sprache
        -> Gtk.set checkButtonDauerschleife [Gtk.buttonLabel := Language.dauerschleife sprache]
    let seiteZurücksetzenPlan :: IO ()
        seiteZurücksetzenPlan = do
            -- aktuelle Aktionen zurücksetzen
            atomically $ writeTVar tvarAktionen leer
            zeigeAktionen leer
            -- Entry zurücksetzten
            Gtk.set
                (erhalteEntry nameAuswahlPlan)
                [Gtk.entryText := Text.empty, Gtk.widgetHasFocus := True]
            let versteckeWennLeer :: (MitWidget w)
                                  => BoxPlanHinzufügen a
                                  -> Maybe (BoxPlanHinzufügen b)
                                  -> Maybe (BoxPlanHinzufügen c)
                                  -> w
                                  -> IO ()
                versteckeWennLeer boxPlanA maybeBoxPlanB maybeBoxPlanC widget = do
                    a <- widgetHinzufügenContainerGefüllt boxPlanA
                    b <- maybe (pure False) widgetHinzufügenContainerGefüllt maybeBoxPlanB
                    c <- maybe (pure False) widgetHinzufügenContainerGefüllt maybeBoxPlanC
                    widgetShowIf (a || b || c) widget
            versteckeWennLeer
                vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
                (Just vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin)
                Nothing
                boxAktionBahngeschwindigkeitMärklin
            versteckeWennLeer
                vBoxHinzufügenPlanBahngeschwindigkeitenLego
                (Just vBoxHinzufügenPlanWegstreckenBahngeschwindigkeitMärklin)
                Nothing
                boxAktionBahngeschwindigkeitLego
            versteckeWennLeer
                vBoxHinzufügenPlanStreckenabschnitte
                (Just vBoxHinzufügenPlanWegstreckenStreckenabschnittMärklin)
                (Just vBoxHinzufügenPlanWegstreckenStreckenabschnittLego)
                boxAktionStreckenabschnitt
            versteckeWennLeer
                vBoxHinzufügenPlanWeichenGeradeMärklin
                (Just vBoxHinzufügenPlanWeichenGeradeLego)
                Nothing
                buttonAktionWeicheGerade
            versteckeWennLeer
                vBoxHinzufügenPlanWeichenKurveMärklin
                (Just vBoxHinzufügenPlanWeichenKurveLego)
                Nothing
                buttonAktionWeicheKurve
            versteckeWennLeer
                vBoxHinzufügenPlanWeichenLinksMärklin
                (Just vBoxHinzufügenPlanWeichenLinksLego)
                Nothing
                buttonAktionWeicheLinks
            versteckeWennLeer
                vBoxHinzufügenPlanWeichenRechtsMärklin
                (Just vBoxHinzufügenPlanWeichenRechtsLego)
                Nothing
                buttonAktionWeicheRechts
            versteckeWennLeer
                vBoxHinzufügenPlanKupplungen
                (Just vBoxHinzufügenPlanWegstreckenKupplungMärklin)
                (Just vBoxHinzufügenPlanWegstreckenKupplungLego)
                boxAktionKupplung
            versteckeWennLeer
                vBoxHinzufügenPlanWegstreckenMärklin
                (Just vBoxHinzufügenPlanWegstreckenLego)
                Nothing
                boxAktionWegstrecke
            versteckeWennLeer vBoxHinzufügenPlanPläne Nothing Nothing boxAktionPlan
        seitePlan :: AssistantSeite HinzufügenSeite
        seitePlan =
            AssistantSeite
            { seite = HinzufügenSeitePlan
                  { widget = erhalteWidget boxPlan,
                    nameAuswahl = nameAuswahlPlan,
                    tvarAktionen,
                    checkButtonDauerschleife
                  },
              name = Language.plan,
              seiteZurücksetzen = seiteZurücksetzenPlan,
              seitenAbschluss = SeitenAbschlussButton seitenAbschlussPlan
            }
    -- konstruiere SeitenBaum
    let seitenBaum :: AssistantSeitenBaum HinzufügenSeite
        seitenBaum =
            AssistantSeiteAuswahl
            { node = seiteAuswahl,
              nachfolgerFrage = Language.welchesObjektHinzufügen,
              nachfolgerListe = AssistantSeiteLetzte
                  <$> seiteBahngeschwindigkeit
                  :| [seiteStreckenabschnitt,
                      seiteWeiche,
                      seiteKupplung,
                      seiteWegstrecke,
                      seitePlan]
            }
    assistant <- assistantNew parent globaleWidgets seitenBaum maybeTVar
        $ flip runReaderT objektReader . hinzufügenErgebnis zugtypAuswahl fließendAuswahl
    liftIO
        $ Gtk.set
            windowAktionObjektAuswahl
            [Gtk.windowTransientFor := erhalteWindow assistant, Gtk.windowModal := True]
    pure assistant
#endif


