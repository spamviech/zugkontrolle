{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Zug.UI.Gtk.StreckenObjekt.WEWidgets
  ( WEWidgets()
  , weichePackNew
  , WEWidgetsBoxen(..)
  , MitWEWidgetsBoxen(..)
  , WEWidgetsBoxenReader(..)
  ) where

import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import Control.Lens ((??), (^..))
import qualified Control.Lens as Lens
import Control.Monad (forM_, void)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty())
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung
       (StreckenObjekt(..), Weiche(..), WeicheKlasse(..), Anschluss(), I2CReader(), PwmReader())
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(zuZugtypEither), mapZugtypEither
                , ausZugtypEither, GeschwindigkeitVariante(..), Richtung(..))
import Zug.Language (Sprache(), MitSprache(), Anzeige(anzeige))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OWeiche), ObjektKlasse(..))
import Zug.Plan (AktionWeiche(..))
import Zug.UI.Base (StatusAllgemein(), ObjektReader(), MStatusAllgemeinT, IOStatusAllgemein
                  , entfernenWeiche, ReaderFamilie, MitTVarMaps())
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen))
import Zug.UI.Gtk.Anschluss (anschlussNew, pinNew)
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledVar)
import Zug.UI.Gtk.Hilfsfunktionen
       (containerAddWidgetNew, boxPackWidgetNewDefault, boxPackWidgetNew, Packing(PackGrow)
      , paddingDefault, positionDefault, buttonNewWithEventLabel, namePackNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitBox(..), mitContainerRemove)
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (MitSpracheGui(), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (WegstreckenElement(..), entferneHinzufügenWegstreckeWidgets
      , hinzufügenWidgetWegstreckeRichtungPackNew, PlanElement(..), entferneHinzufügenPlanWidgets
      , hinzufügenWidgetPlanPackNew, MitFortfahrenWennToggledWegstrecke()
      , WegstreckeCheckButtonVoid, FortfahrenWennToggledWegstreckeReader(..), MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen
       (Kategorie(..), KategorieText(..), CheckButtonWegstreckeHinzufügen, BoxWegstreckeHinzufügen
      , ButtonPlanHinzufügen, BoxPlanHinzufügen, widgetHinzufügenZugtypEither)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), buttonEntfernenPackNew
      , eventAusführen)
import Zug.UI.StatusVar
       (StatusVar, StatusVarReader(erhalteStatusVar), MitStatusVar(), ausführenStatusVarAktion)

instance Kategorie (WEWidgets z) where
    kategorie :: KategorieText (WEWidgets z)
    kategorie = KategorieText Language.weichen

-- | 'Weiche' mit zugehörigen Widgets
data WEWidgets (z :: Zugtyp) =
    WEWidgets
    { we :: Weiche z
    , weWidget :: Gtk.VBox
    , weFunctionBox :: Gtk.HBox
    , weHinzWS :: CheckButtonWegstreckeHinzufügen Richtung (WEWidgets z)
    , weHinzPL :: WeichePlanHinzufügenWidgets z
    , weTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , weTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

-- | Widgets zum Hinzufügen einer 'Weiche' zu einem 'Plan'
data WeichePlanHinzufügenWidgets z =
    WeichePlanHinzufügenWidgets
    { gerade :: Maybe (ButtonPlanHinzufügen (WEWidgets z))
    , kurve :: Maybe (ButtonPlanHinzufügen (WEWidgets z))
    , links :: Maybe (ButtonPlanHinzufügen (WEWidgets z))
    , rechts :: Maybe (ButtonPlanHinzufügen (WEWidgets z))
    }
    deriving (Eq)

data WEWidgetsBoxen =
    WEWidgetsBoxen
    { vBoxWeichen :: ScrollbaresWidget Gtk.VBox
    , vBoxHinzufügenWegstreckeWeichenMärklin :: BoxWegstreckeHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenWegstreckeWeichenLego :: BoxWegstreckeHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenPlanWeichenGeradeMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenPlanWeichenKurveMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenPlanWeichenLinksMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenPlanWeichenRechtsMärklin :: BoxPlanHinzufügen (WEWidgets 'Märklin)
    , vBoxHinzufügenPlanWeichenGeradeLego :: BoxPlanHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenPlanWeichenKurveLego :: BoxPlanHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenPlanWeichenLinksLego :: BoxPlanHinzufügen (WEWidgets 'Lego)
    , vBoxHinzufügenPlanWeichenRechtsLego :: BoxPlanHinzufügen (WEWidgets 'Lego)
    }

class MitWEWidgetsBoxen r where
    weWidgetsBoxen :: r -> WEWidgetsBoxen

instance MitWEWidgetsBoxen WEWidgetsBoxen where
    weWidgetsBoxen :: WEWidgetsBoxen -> WEWidgetsBoxen
    weWidgetsBoxen = id

class (MonadReader r m, MitWEWidgetsBoxen r) => WEWidgetsBoxenReader r m | m -> r where
    erhalteWEWidgetsBoxen :: m WEWidgetsBoxen
    erhalteWEWidgetsBoxen = asks weWidgetsBoxen

instance (MonadReader r m, MitWEWidgetsBoxen r) => WEWidgetsBoxenReader r m

instance MitWidget (WEWidgets z) where
    erhalteWidget :: WEWidgets z -> Gtk.Widget
    erhalteWidget = erhalteWidget . weWidget

instance (WegstreckenElement (WEWidgets z), PlanElement (WEWidgets z))
    => WidgetsTyp (WEWidgets z) where
    type ObjektTyp (WEWidgets z) = Weiche z

    type ReaderConstraint (WEWidgets z) = MitWEWidgetsBoxen

    erhalteObjektTyp :: WEWidgets z -> Weiche z
    erhalteObjektTyp = we

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (WEWidgets z) m) => WEWidgets z -> m ()
    entferneWidgets weWidgets@WEWidgets {weTVarSprache} = do
        WEWidgetsBoxen {vBoxWeichen} <- erhalteWEWidgetsBoxen
        mitContainerRemove vBoxWeichen weWidgets
        entferneHinzufügenWegstreckeWidgets weWidgets
        entferneHinzufügenPlanWidgets weWidgets
        liftIO $ atomically $ writeTVar weTVarSprache Nothing

    boxButtonEntfernen :: WEWidgets z -> Gtk.Box
    boxButtonEntfernen = erhalteBox . weFunctionBox

    tvarSprache :: WEWidgets z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = weTVarSprache

    tvarEvent :: WEWidgets z -> TVar EventAusführen
    tvarEvent = weTVarEvent

instance WidgetsTyp (ZugtypEither WEWidgets) where
    type ObjektTyp (ZugtypEither WEWidgets) = ZugtypEither Weiche

    type ReaderConstraint (ZugtypEither WEWidgets) = MitWEWidgetsBoxen

    erhalteObjektTyp :: ZugtypEither WEWidgets -> ZugtypEither Weiche
    erhalteObjektTyp = mapZugtypEither we

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (ZugtypEither WEWidgets) m)
                    => ZugtypEither WEWidgets
                    -> m ()
    entferneWidgets (ZugtypMärklin weWidgets) = entferneWidgets weWidgets
    entferneWidgets (ZugtypLego weWidgets) = entferneWidgets weWidgets

    boxButtonEntfernen :: ZugtypEither WEWidgets -> Gtk.Box
    boxButtonEntfernen (ZugtypMärklin weWidgets) = boxButtonEntfernen weWidgets
    boxButtonEntfernen (ZugtypLego weWidgets) = boxButtonEntfernen weWidgets

    tvarSprache :: ZugtypEither WEWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (ZugtypMärklin weWidgets) = tvarSprache weWidgets
    tvarSprache (ZugtypLego weWidgets) = tvarSprache weWidgets

    tvarEvent :: ZugtypEither WEWidgets -> TVar EventAusführen
    tvarEvent (ZugtypMärklin weWidgets) = tvarEvent weWidgets
    tvarEvent (ZugtypLego weWidgets) = tvarEvent weWidgets

instance WegstreckenElement (WEWidgets 'Märklin) where
    type CheckButtonAuswahl (WEWidgets 'Märklin) = Richtung

    getterWegstrecke
        :: Lens.Getter (WEWidgets 'Märklin) (CheckButtonWegstreckeHinzufügen Richtung (WEWidgets 'Märklin))
    getterWegstrecke = Lens.to weHinzWS

    boxWegstrecke :: (ReaderConstraint (WEWidgets 'Märklin) r)
                  => Weiche 'Märklin
                  -> Lens.Getter r (BoxWegstreckeHinzufügen (WEWidgets 'Märklin))
    boxWegstrecke _weWidgets = Lens.to $ vBoxHinzufügenWegstreckeWeichenMärklin . weWidgetsBoxen

instance WegstreckenElement (WEWidgets 'Lego) where
    type CheckButtonAuswahl (WEWidgets 'Lego) = Richtung

    getterWegstrecke
        :: Lens.Getter (WEWidgets 'Lego) (CheckButtonWegstreckeHinzufügen Richtung (WEWidgets 'Lego))
    getterWegstrecke = Lens.to weHinzWS

    boxWegstrecke :: (ReaderConstraint (WEWidgets 'Lego) r)
                  => Weiche 'Lego
                  -> Lens.Getter r (BoxWegstreckeHinzufügen (WEWidgets 'Lego))
    boxWegstrecke _weWidgets = Lens.to $ vBoxHinzufügenWegstreckeWeichenLego . weWidgetsBoxen

instance WegstreckenElement (ZugtypEither WEWidgets) where
    type CheckButtonAuswahl (ZugtypEither WEWidgets) = Richtung

    getterWegstrecke
        :: Lens.Getter (ZugtypEither WEWidgets) (CheckButtonWegstreckeHinzufügen Richtung (ZugtypEither WEWidgets))
    getterWegstrecke = Lens.to $ ausZugtypEither $ widgetHinzufügenZugtypEither . weHinzWS

    boxWegstrecke :: (ReaderConstraint (ZugtypEither WEWidgets) r)
                  => ZugtypEither Weiche
                  -> Lens.Getter r (BoxWegstreckeHinzufügen (ZugtypEither WEWidgets))
    boxWegstrecke (ZugtypMärklin _weWidgets) =
        Lens.to
        $ widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeWeichenMärklin . weWidgetsBoxen
    boxWegstrecke (ZugtypLego _weWidgets) =
        Lens.to
        $ widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeWeichenLego . weWidgetsBoxen

instance PlanElement (WEWidgets 'Märklin) where
    foldPlan
        :: Lens.Fold (WEWidgets 'Märklin) (Maybe (ButtonPlanHinzufügen (WEWidgets 'Märklin)))
    foldPlan =
        Lens.folding
        $ \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
        -> [gerade, kurve, links, rechts]

    boxenPlan :: (ReaderConstraint (WEWidgets 'Märklin) r)
              => Weiche 'Märklin
              -> Lens.Fold r (BoxPlanHinzufügen (WEWidgets 'Märklin))
    boxenPlan _weWidgets =
        Lens.folding
        $ (??)
            [ vBoxHinzufügenPlanWeichenGeradeMärklin
            , vBoxHinzufügenPlanWeichenKurveMärklin
            , vBoxHinzufügenPlanWeichenLinksMärklin
            , vBoxHinzufügenPlanWeichenRechtsMärklin]
        . weWidgetsBoxen

instance PlanElement (WEWidgets 'Lego) where
    foldPlan :: Lens.Fold (WEWidgets 'Lego) (Maybe (ButtonPlanHinzufügen (WEWidgets 'Lego)))
    foldPlan =
        Lens.folding
        $ \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
        -> [gerade, kurve, links, rechts]

    boxenPlan :: (ReaderConstraint (WEWidgets 'Lego) r)
              => Weiche 'Lego
              -> Lens.Fold r (BoxPlanHinzufügen (WEWidgets 'Lego))
    boxenPlan _weWidgets =
        Lens.folding
        $ (??)
            [ vBoxHinzufügenPlanWeichenGeradeLego
            , vBoxHinzufügenPlanWeichenKurveLego
            , vBoxHinzufügenPlanWeichenLinksLego
            , vBoxHinzufügenPlanWeichenRechtsLego]
        . weWidgetsBoxen

instance PlanElement (ZugtypEither WEWidgets) where
    foldPlan :: Lens.Fold (ZugtypEither WEWidgets) (Maybe (ButtonPlanHinzufügen (ZugtypEither WEWidgets)))
    foldPlan =
        Lens.folding
        $ ausZugtypEither
        $ \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
        -> fmap widgetHinzufügenZugtypEither <$> [gerade, kurve, links, rechts]

    boxenPlan :: (ReaderConstraint (ZugtypEither WEWidgets) r)
              => ZugtypEither Weiche
              -> Lens.Fold r (BoxPlanHinzufügen (ZugtypEither WEWidgets))
    boxenPlan (ZugtypMärklin _weiche) =
        Lens.folding
        $ fmap widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWeichenGeradeMärklin
            , vBoxHinzufügenPlanWeichenKurveMärklin
            , vBoxHinzufügenPlanWeichenLinksMärklin
            , vBoxHinzufügenPlanWeichenRechtsMärklin]
        . weWidgetsBoxen
    boxenPlan (ZugtypLego _weiche) =
        Lens.folding
        $ fmap widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWeichenGeradeLego
            , vBoxHinzufügenPlanWeichenKurveLego
            , vBoxHinzufügenPlanWeichenLinksLego
            , vBoxHinzufügenPlanWeichenRechtsLego]
        . weWidgetsBoxen

instance StreckenObjekt (WEWidgets z) where
    anschlüsse :: WEWidgets z -> Set Anschluss
    anschlüsse WEWidgets {we} = anschlüsse we

    erhalteName :: WEWidgets z -> Text
    erhalteName WEWidgets {we} = erhalteName we

instance Aeson.ToJSON (WEWidgets z) where
    toJSON :: WEWidgets z -> Aeson.Value
    toJSON WEWidgets {we} = Aeson.toJSON we

instance (ZugtypKlasse z) => WeicheKlasse (WEWidgets z) where
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => WEWidgets z -> Richtung -> m ()
    stellen WEWidgets {we} = stellen we

    erhalteRichtungen :: WEWidgets z -> NonEmpty Richtung
    erhalteRichtungen WEWidgets {we} = erhalteRichtungen we

-- | 'Weiche' darstellen und zum Status hinzufügen
weichePackNew
    :: forall o m z.
    ( WegstreckenElement (WEWidgets z)
    , PlanElement (WEWidgets z)
    , Eq (BG o 'Pwm 'Märklin)
    , Eq (BG o 'KonstanteSpannung 'Märklin)
    , Eq (BG o 'Pwm 'Lego)
    , Eq (BG o 'KonstanteSpannung 'Lego)
    , Eq (ST o)
    , WE o ~ WEWidgets
    , Eq (KU o)
    , Eq (KO o)
    , Eq (WS o 'Märklin)
    , Eq (WS o 'Lego)
    , Eq (PL o)
    , MitSprache (SP o)
    , ObjektKlasse o
    , Aeson.ToJSON o
    , ObjektReader o m
    , MitWEWidgetsBoxen (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , MitTVarMaps (ReaderFamilie o)
    , MitStatusVar (ReaderFamilie o) o
    , MitFortfahrenWennToggledWegstrecke (ReaderFamilie o) o
    , MitTMVarPlanObjekt (ReaderFamilie o)
    , MonadIO m
    , ZugtypKlasse z
    )
    => Weiche z
    -> MStatusAllgemeinT m o (WEWidgets z)
weichePackNew weiche = do
    weWidgetsBoxen@WEWidgetsBoxen {vBoxWeichen} <- erhalteWEWidgetsBoxen
    (weTVarSprache, weTVarEvent) <- liftIO $ do
        weTVarSprache <- newTVarIO $ Just []
        weTVarEvent <- newTVarIO EventAusführen
        pure (weTVarSprache, weTVarEvent)
    let justTVarSprache = Just weTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    fortfahrenWennToggledWegstrecke <- erhalteFortfahrenWennToggledWegstrecke
        :: MStatusAllgemeinT m o (FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid)
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckeRichtungPackNew
        weiche
        (erhalteRichtungen weiche)
        weTVarSprache
        fortfahrenWennToggledWegstrecke
    let [boxGerade, boxKurve, boxLinks, boxRechts] = weWidgetsBoxen ^.. boxenPlan weiche
    hinzufügenPlanWidgetGerade <- if hatRichtung weiche Gerade
        then Just <$> hinzufügenWidgetPlanPackNew boxGerade weiche weTVarSprache
        else pure Nothing
    hinzufügenPlanWidgetKurve <- if hatRichtung weiche Kurve
        then Just <$> hinzufügenWidgetPlanPackNew boxKurve weiche weTVarSprache
        else pure Nothing
    hinzufügenPlanWidgetLinks <- if hatRichtung weiche Links
        then Just <$> hinzufügenWidgetPlanPackNew boxLinks weiche weTVarSprache
        else pure Nothing
    hinzufügenPlanWidgetRechts <- if hatRichtung weiche Rechts
        then Just <$> hinzufügenWidgetPlanPackNew boxRechts weiche weTVarSprache
        else pure Nothing
    let hinzufügenPlanWidget =
            WeichePlanHinzufügenWidgets
            { gerade = hinzufügenPlanWidgetGerade
            , kurve = hinzufügenPlanWidgetKurve
            , links = hinzufügenPlanWidgetLinks
            , rechts = hinzufügenPlanWidgetRechts
            }
    -- Widget erstellen
    vBox <- liftIO $ boxPackWidgetNewDefault vBoxWeichen $ Gtk.vBoxNew False 0
    namePackNew vBox weiche
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    weFunctionBox <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
    let weWidgets =
            WEWidgets
            { we = weiche
            , weWidget = vBox
            , weFunctionBox
            , weHinzWS = hinzufügenWegstreckeWidget
            , weHinzPL = hinzufügenPlanWidget
            , weTVarSprache
            , weTVarEvent
            }
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    richtungsButtonsPackNew weWidgets weFunctionBox vBoxAnschlüsse
    fließendPackNew vBoxAnschlüsse weiche justTVarSprache
    buttonEntfernenPackNew weWidgets
        $ (entfernenWeiche $ zuZugtypEither weWidgets :: IOStatusAllgemein o ())
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OWeiche $ zuZugtypEither weWidgets
    pure weWidgets
    where
        richtungsButtonsPackNew
            :: WEWidgets z -> Gtk.HBox -> ScrollbaresWidget Gtk.VBox -> MStatusAllgemeinT m o ()
        richtungsButtonsPackNew
            WEWidgets {we = MärklinWeiche {wemRichtungsAnschlüsse}, weTVarSprache, weTVarEvent}
            box
            vBoxAktionen = do
            statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
            objektReader <- ask
            forM_ wemRichtungsAnschlüsse $ \(richtung, anschluss) -> do
                let justTVarSprache = Just weTVarSprache
                    anzeigeRichtung = anzeige richtung
                boxPackWidgetNewDefault box
                    $ buttonNewWithEventLabel justTVarSprache anzeigeRichtung
                    $ eventAusführen weTVarEvent
                    $ flip runReaderT objektReader
                    $ ausführenStatusVarAktion (Stellen weiche richtung) statusVar
                boxPackWidgetNewDefault vBoxAktionen
                    $ anschlussNew justTVarSprache anzeigeRichtung anschluss
        richtungsButtonsPackNew
            WEWidgets { we = LegoWeiche {welRichtungsPin, welRichtungen = (richtung1, richtung2)}
                      , weTVarSprache
                      , weTVarEvent}
            box
            vBoxAktionen = void $ do
            statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
            objektReader <- ask
            let justTVar = Just weTVarSprache
            boxPackWidgetNewDefault vBoxAktionen
                $ pinNew justTVar Language.richtung welRichtungsPin
            boxPackWidgetNewDefault box
                $ buttonNewWithEventLabel justTVar (anzeige richtung1)
                $ eventAusführen weTVarEvent
                $ flip runReaderT objektReader
                $ ausführenStatusVarAktion (Stellen weiche richtung1) statusVar
            boxPackWidgetNewDefault box
                $ buttonNewWithEventLabel justTVar (anzeige richtung2)
                $ eventAusführen weTVarEvent
                $ flip runReaderT objektReader
                $ ausführenStatusVarAktion (Stellen weiche richtung2) statusVar