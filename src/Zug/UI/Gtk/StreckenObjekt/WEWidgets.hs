{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
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
#endif

module Zug.UI.Gtk.StreckenObjekt.WEWidgets
  (
#ifdef ZUGKONTROLLEGUI
    WEWidgets()
  , weichePackNew
  , WEWidgetsBoxen(..)
  , MitWEWidgetsBoxen(..)
  , WEWidgetsBoxenReader(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GI.Gtk (AttrOp((:=)))
import qualified GI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(..), Weiche(..), WeicheKlasse(..), WeicheContainer(..)
                    , AnschlussEither(), I2CReader(), PwmReader())
import Zug.Enums (Zugtyp(..), ZugtypEither(..), ZugtypKlasse(zuZugtypEither), mapZugtypEither
                , ausZugtypEither, Richtung(..))
import Zug.Language (Anzeige(anzeige))
import qualified Zug.Language as Language
import Zug.Objekt (Objekt, ObjektAllgemein(OWeiche), ObjektKlasse(..), ObjektElement(..))
import Zug.Plan (AktionWeiche(..))
import Zug.UI.Base (StatusAllgemein(), ObjektReader(), MStatusAllgemeinT, IOStatusAllgemein
                  , entfernenWeiche, ReaderFamilie, MitTVarMaps())
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen), BefehlConstraints)
import Zug.UI.Gtk.Anschluss (anschlussNew, pinNew)
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledVar)
import Zug.UI.Gtk.Hilfsfunktionen
       (containerAddWidgetNew, boxPackWidgetNewDefault, boxPackWidgetNew, Packing(PackGrow)
      , paddingDefault, positionDefault, buttonNewWithEventLabel, namePackNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove)
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui
       (SpracheGui, MitSpracheGui(), verwendeSpracheGui, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (WegstreckenElement(..), entferneHinzufügenWegstreckeWidgets
      , hinzufügenWidgetWegstreckeRichtungPackNew, PlanElement(..), entferneHinzufügenPlanWidgets
      , hinzufügenWidgetPlanPackNew, MitFortfahrenWennToggledWegstrecke()
      , WegstreckeCheckButtonVoid, FortfahrenWennToggledWegstreckeReader(..), MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufuegen
       (Kategorie(..), KategorieText(..), CheckButtonWegstreckeHinzufügen, BoxWegstreckeHinzufügen
      , ButtonPlanHinzufügen, BoxPlanHinzufügen, widgetHinzufügenZugtypEither)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), eventAusführen
      , buttonEntfernenPackNew, buttonBearbeitenPackNew, MitAktionBearbeiten())
import Zug.UI.StatusVar
       (StatusVar, StatusVarReader(erhalteStatusVar), MitStatusVar(), ausführenStatusVarAktion)

instance Kategorie (WEWidgets z) where
    kategorie :: KategorieText (WEWidgets z)
    kategorie = KategorieText Language.weichen

-- | 'Weiche' mit zugehörigen Widgets
data WEWidgets (z :: Zugtyp) =
    WEWidgets
    { we :: Weiche z
    , weWidget :: Gtk.Box
    , weFunctionBox :: Gtk.Box
    , weHinzWS :: CheckButtonWegstreckeHinzufügen Richtung (WEWidgets z)
    , weHinzPL :: WeichePlanHinzufügenWidgets z
    , weTVarSprache :: TVarSprachewechselAktionen
    , weTVarEvent :: TVar EventAusführen
    , weRichtungsButtons :: NonEmpty (Richtung, Gtk.Button)
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
    { vBoxWeichen :: ScrollbaresWidget Gtk.Box
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
    erhalteWidget :: (MonadIO m) => WEWidgets z -> m Gtk.Widget
    erhalteWidget = erhalteWidget . weWidget

instance (ZugtypKlasse z) => ObjektElement (WEWidgets z) where
    type ObjektTyp (WEWidgets z) = Weiche z

    zuObjektTyp :: WEWidgets z -> Weiche z
    zuObjektTyp = we

instance (WegstreckenElement (WEWidgets z), PlanElement (WEWidgets z), ZugtypKlasse z)
    => WidgetsTyp (WEWidgets z) where
    type ReaderConstraint (WEWidgets z) = MitWEWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (WEWidgets z) m) => WEWidgets z -> m ()
    entferneWidgets weWidgets@WEWidgets {weTVarSprache} = do
        WEWidgetsBoxen {vBoxWeichen} <- erhalteWEWidgetsBoxen
        mitContainerRemove vBoxWeichen weWidgets
        entferneHinzufügenWegstreckeWidgets weWidgets
        entferneHinzufügenPlanWidgets weWidgets
        liftIO $ atomically $ writeTVar weTVarSprache Nothing

    boxButtonEntfernen :: WEWidgets z -> Gtk.Box
    boxButtonEntfernen = weFunctionBox

    tvarSprache :: WEWidgets z -> TVarSprachewechselAktionen
    tvarSprache = weTVarSprache

    tvarEvent :: WEWidgets z -> TVar EventAusführen
    tvarEvent = weTVarEvent

instance ObjektElement (ZugtypEither WEWidgets) where
    type ObjektTyp (ZugtypEither WEWidgets) = ZugtypEither Weiche

    zuObjektTyp :: ZugtypEither WEWidgets -> ZugtypEither Weiche
    zuObjektTyp = mapZugtypEither we

    zuObjekt :: ZugtypEither WEWidgets -> Objekt
    zuObjekt = OWeiche . mapZugtypEither we

instance WidgetsTyp (ZugtypEither WEWidgets) where
    type ReaderConstraint (ZugtypEither WEWidgets) = MitWEWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (ZugtypEither WEWidgets) m)
                    => ZugtypEither WEWidgets
                    -> m ()
    entferneWidgets (ZugtypMärklin weWidgets) = entferneWidgets weWidgets
    entferneWidgets (ZugtypLego weWidgets) = entferneWidgets weWidgets

    boxButtonEntfernen :: ZugtypEither WEWidgets -> Gtk.Box
    boxButtonEntfernen (ZugtypMärklin weWidgets) = boxButtonEntfernen weWidgets
    boxButtonEntfernen (ZugtypLego weWidgets) = boxButtonEntfernen weWidgets

    tvarSprache :: ZugtypEither WEWidgets -> TVarSprachewechselAktionen
    tvarSprache (ZugtypMärklin weWidgets) = tvarSprache weWidgets
    tvarSprache (ZugtypLego weWidgets) = tvarSprache weWidgets

    tvarEvent :: ZugtypEither WEWidgets -> TVar EventAusführen
    tvarEvent (ZugtypMärklin weWidgets) = tvarEvent weWidgets
    tvarEvent (ZugtypLego weWidgets) = tvarEvent weWidgets

instance WegstreckenElement (WEWidgets 'Märklin) where
    type CheckButtonAuswahl (WEWidgets 'Märklin) = Richtung

    checkButtonWegstrecke
        :: WEWidgets 'Märklin -> CheckButtonWegstreckeHinzufügen Richtung (WEWidgets 'Märklin)
    checkButtonWegstrecke = weHinzWS

    boxWegstrecke :: (ReaderConstraint (WEWidgets 'Märklin) r)
                  => Weiche 'Märklin
                  -> r
                  -> BoxWegstreckeHinzufügen (WEWidgets 'Märklin)
    boxWegstrecke _weiche = vBoxHinzufügenWegstreckeWeichenMärklin . weWidgetsBoxen

instance WegstreckenElement (WEWidgets 'Lego) where
    type CheckButtonAuswahl (WEWidgets 'Lego) = Richtung

    checkButtonWegstrecke
        :: WEWidgets 'Lego -> CheckButtonWegstreckeHinzufügen Richtung (WEWidgets 'Lego)
    checkButtonWegstrecke = weHinzWS

    boxWegstrecke :: (ReaderConstraint (WEWidgets 'Lego) r)
                  => Weiche 'Lego
                  -> r
                  -> BoxWegstreckeHinzufügen (WEWidgets 'Lego)
    boxWegstrecke _weiche = vBoxHinzufügenWegstreckeWeichenLego . weWidgetsBoxen

instance WegstreckenElement (ZugtypEither WEWidgets) where
    type CheckButtonAuswahl (ZugtypEither WEWidgets) = Richtung

    checkButtonWegstrecke :: ZugtypEither WEWidgets
                          -> CheckButtonWegstreckeHinzufügen Richtung (ZugtypEither WEWidgets)
    checkButtonWegstrecke = ausZugtypEither $ widgetHinzufügenZugtypEither . weHinzWS

    boxWegstrecke :: (ReaderConstraint (ZugtypEither WEWidgets) r)
                  => ZugtypEither Weiche
                  -> r
                  -> BoxWegstreckeHinzufügen (ZugtypEither WEWidgets)
    boxWegstrecke (ZugtypMärklin _weWidgets) =
        widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeWeichenMärklin . weWidgetsBoxen
    boxWegstrecke (ZugtypLego _weWidgets) =
        widgetHinzufügenZugtypEither . vBoxHinzufügenWegstreckeWeichenLego . weWidgetsBoxen

-- little helper function originally defined in the lens package
-- redefined here, so I don't have to rename everything
(??) :: [a -> b] -> a -> [b]
(??) [] _a = []
(??) (h:t) a = h a : (??) t a

instance PlanElement (WEWidgets 'Märklin) where
    buttonsPlan :: WEWidgets 'Märklin -> [Maybe (ButtonPlanHinzufügen (WEWidgets 'Märklin))]
    buttonsPlan WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}} =
        [gerade, kurve, links, rechts]

    boxenPlan :: (ReaderConstraint (WEWidgets 'Märklin) r)
              => Weiche 'Märklin
              -> r
              -> [BoxPlanHinzufügen (WEWidgets 'Märklin)]
    boxenPlan _weiche =
        (??)
            [ vBoxHinzufügenPlanWeichenGeradeMärklin
            , vBoxHinzufügenPlanWeichenKurveMärklin
            , vBoxHinzufügenPlanWeichenLinksMärklin
            , vBoxHinzufügenPlanWeichenRechtsMärklin]
        . weWidgetsBoxen

instance PlanElement (WEWidgets 'Lego) where
    buttonsPlan :: WEWidgets 'Lego -> [Maybe (ButtonPlanHinzufügen (WEWidgets 'Lego))]
    buttonsPlan WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}} =
        [gerade, kurve, links, rechts]

    boxenPlan :: (ReaderConstraint (WEWidgets 'Lego) r)
              => Weiche 'Lego
              -> r
              -> [BoxPlanHinzufügen (WEWidgets 'Lego)]
    boxenPlan _weiche =
        (??)
            [ vBoxHinzufügenPlanWeichenGeradeLego
            , vBoxHinzufügenPlanWeichenKurveLego
            , vBoxHinzufügenPlanWeichenLinksLego
            , vBoxHinzufügenPlanWeichenRechtsLego]
        . weWidgetsBoxen

instance PlanElement (ZugtypEither WEWidgets) where
    buttonsPlan :: ZugtypEither WEWidgets
                -> [Maybe (ButtonPlanHinzufügen (ZugtypEither WEWidgets))]
    buttonsPlan =
        ausZugtypEither
        $ \WEWidgets {weHinzPL = WeichePlanHinzufügenWidgets {gerade, kurve, links, rechts}}
        -> fmap widgetHinzufügenZugtypEither <$> [gerade, kurve, links, rechts]

    boxenPlan :: (ReaderConstraint (ZugtypEither WEWidgets) r)
              => ZugtypEither Weiche
              -> r
              -> [BoxPlanHinzufügen (ZugtypEither WEWidgets)]
    boxenPlan (ZugtypMärklin _weiche) =
        fmap widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWeichenGeradeMärklin
            , vBoxHinzufügenPlanWeichenKurveMärklin
            , vBoxHinzufügenPlanWeichenLinksMärklin
            , vBoxHinzufügenPlanWeichenRechtsMärklin]
        . weWidgetsBoxen
    boxenPlan (ZugtypLego _weiche) =
        fmap widgetHinzufügenZugtypEither
        . (??)
            [ vBoxHinzufügenPlanWeichenGeradeLego
            , vBoxHinzufügenPlanWeichenKurveLego
            , vBoxHinzufügenPlanWeichenLinksLego
            , vBoxHinzufügenPlanWeichenRechtsLego]
        . weWidgetsBoxen

instance StreckenObjekt (WEWidgets z) where
    anschlüsse :: WEWidgets z -> Set AnschlussEither
    anschlüsse = anschlüsse . we

    erhalteName :: WEWidgets z -> Text
    erhalteName = erhalteName . we

instance Aeson.ToJSON (WEWidgets z) where
    toJSON :: WEWidgets z -> Aeson.Value
    toJSON = Aeson.toJSON . we

instance (ZugtypKlasse z) => WeicheKlasse (WEWidgets z) where
    stellen :: (I2CReader r m, PwmReader r m, MonadIO m) => WEWidgets z -> Richtung -> m ()
    stellen WEWidgets {weRichtungsButtons} richtung =
        case lookup richtung $ NonEmpty.toList weRichtungsButtons of
            (Just button) -> liftIO $ Gtk.buttonClicked button
            Nothing -> pure ()

    erhalteRichtungen :: WEWidgets z -> NonEmpty Richtung
    erhalteRichtungen = erhalteRichtungen . we

instance (ZugtypKlasse z) => WeicheContainer (WEWidgets z) where
    enthalteneWeichen :: WEWidgets z -> Set (ZugtypEither Weiche)
    enthalteneWeichen = Set.singleton . zuZugtypEither . we

-- | 'Weiche' darstellen und zum Status hinzufügen
weichePackNew
    :: forall o m z.
    ( WegstreckenElement (WEWidgets z)
    , PlanElement (WEWidgets z)
    , BefehlConstraints o
    , WE o ~ WEWidgets
    , SP o ~ SpracheGui
    , ObjektReader o m
    , MitWEWidgetsBoxen (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , MitTVarMaps (ReaderFamilie o)
    , MitStatusVar (ReaderFamilie o) o
    , MitFortfahrenWennToggledWegstrecke (ReaderFamilie o) o
    , MitTMVarPlanObjekt (ReaderFamilie o)
    , MitAktionBearbeiten (ReaderFamilie o)
    , MonadIO m
    , ZugtypKlasse z
    )
    => Weiche z
    -> MStatusAllgemeinT m o (WEWidgets z)
weichePackNew weiche = do
    widgetsBoxen@WEWidgetsBoxen {vBoxWeichen} <- erhalteWEWidgetsBoxen
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
    let [boxGerade, boxKurve, boxLinks, boxRechts] = boxenPlan weiche widgetsBoxen
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
    vBox <- liftIO $ boxPackWidgetNewDefault vBoxWeichen $ Gtk.boxNew Gtk.OrientationVertical 0
    namePackNew vBox weiche
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Nothing
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.boxNew Gtk.OrientationVertical 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    weFunctionBox <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.boxNew Gtk.OrientationHorizontal 0
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    weRichtungsButtons
        <- richtungsButtonsPackNew weiche weFunctionBox vBoxAnschlüsse weTVarSprache weTVarEvent
    let weWidgets =
            WEWidgets
            { we = weiche
            , weWidget = vBox
            , weFunctionBox
            , weHinzWS = hinzufügenWegstreckeWidget
            , weHinzPL = hinzufügenPlanWidget
            , weTVarSprache
            , weTVarEvent
            , weRichtungsButtons
            }
    fließendPackNew vBoxAnschlüsse weiche justTVarSprache
    buttonEntfernenPackNew weWidgets
        $ (entfernenWeiche $ zuZugtypEither weWidgets :: IOStatusAllgemein o ())
    buttonBearbeitenPackNew weWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OWeiche $ zuZugtypEither weWidgets
    pure weWidgets
    where
        richtungsButtonsPackNew
            :: Weiche z
            -> Gtk.Box
            -> ScrollbaresWidget Gtk.Box
            -> TVarSprachewechselAktionen
            -> TVar EventAusführen
            -> MStatusAllgemeinT m o (NonEmpty (Richtung, Gtk.Button))
        richtungsButtonsPackNew
            WeicheMärklin {wemRichtungsAnschlüsse}
            box
            vBoxAktionen
            weTVarSprache
            weTVarEvent = do
            statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
            objektReader <- ask
            forM wemRichtungsAnschlüsse $ \(richtung, anschluss) -> do
                let justTVarSprache = Just weTVarSprache
                    anzeigeRichtung = anzeige richtung
                button <- boxPackWidgetNewDefault box
                    $ buttonNewWithEventLabel justTVarSprache anzeigeRichtung
                    $ eventAusführen weTVarEvent
                    $ flip runReaderT objektReader
                    $ ausführenStatusVarAktion (Stellen weiche richtung) statusVar
                boxPackWidgetNewDefault vBoxAktionen
                    $ anschlussNew justTVarSprache anzeigeRichtung anschluss
                pure (richtung, button)
        richtungsButtonsPackNew
            WeicheLego {welRichtungsPin, welRichtungen = (richtung1, richtung2)}
            box
            vBoxAktionen
            weTVarSprache
            weTVarEvent = do
            statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
            objektReader <- ask
            let justTVar = Just weTVarSprache
            boxPackWidgetNewDefault vBoxAktionen
                $ pinNew justTVar Language.richtung welRichtungsPin
            button1 <- boxPackWidgetNewDefault box
                $ buttonNewWithEventLabel justTVar (anzeige richtung1)
                $ eventAusführen weTVarEvent
                $ flip runReaderT objektReader
                $ ausführenStatusVarAktion (Stellen weiche richtung1) statusVar
            button2 <- boxPackWidgetNewDefault box
                $ buttonNewWithEventLabel justTVar (anzeige richtung2)
                $ eventAusführen weTVarEvent
                $ flip runReaderT objektReader
                $ ausführenStatusVarAktion (Stellen weiche richtung2) statusVar
            pure $ (richtung1, button1) :| [(richtung2, button2)]
#endif
--
