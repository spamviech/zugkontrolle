{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zug.UI.Gtk.StreckenObjekt.STWidgets (STWidgets(), STWidgetsKlasse(..)) where

import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import qualified Control.Lens as Lens
import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (Streckenabschnitt(..), StreckenabschnittKlasse(..), StreckenObjekt(..)
                    , Anschluss(), I2CReader())
import Zug.Enums (Strom(..), ZugtypEither(..), Zugtyp(..))
import Zug.Language (Sprache())
import qualified Zug.Language as Language
import Zug.Objekt (ObjektKlasse(..), ObjektAllgemein(OStreckenabschnitt))
import Zug.Plan (AktionKlasse(ausführenAktion), AktionStreckenabschnitt(..))
import Zug.UI.Base (MStatusAllgemeinT, IOStatusAllgemein, entfernenStreckenabschnitt
                  , getStreckenabschnitte, getWegstrecken, ReaderFamilie, MitTVarMaps)
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen))
import Zug.UI.Gtk.Anschluss (anschlussNew)
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.Hilfsfunktionen
       (boxPackWidgetNewDefault, boxPackWidgetNew, Packing(PackGrow), paddingDefault
      , positionDefault, containerAddWidgetNew, namePackNew, toggleButtonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove, MitBox(..))
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (SpracheGuiReader(), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (WegstreckenElement(..), entferneHinzufügenWegstreckeWidgets
      , hinzufügenWidgetWegstreckePackNew, PlanElement(..), entferneHinzufügenPlanWidgets
      , hinzufügenWidgetPlanPackNew)
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen
       (Kategorie(..), KategorieText(..), BoxWegstreckeHinzufügen, CheckButtonWegstreckeHinzufügen
      , BoxPlanHinzufügen, ButtonPlanHinzufügen)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), eventAusführen
      , ohneEvent, buttonEntfernenPackNew)
import Zug.UI.StatusVar (StatusVar, StatusVarReader(erhalteStatusVar), auswertenStatusVarMStatusT)

instance Kategorie STWidgets where
    kategorie :: KategorieText STWidgets
    kategorie = KategorieText Language.streckenabschnitte

-- | 'Streckenabschnitt' mit zugehörigen Widgets
data STWidgets =
    STWidgets
    { st :: Streckenabschnitt
    , stWidget :: Gtk.VBox
    , stFunctionBox :: Gtk.HBox
    , stHinzWS :: CheckButtonWegstreckeHinzufügen Void STWidgets
    , stHinzPL :: ButtonPlanHinzufügen STWidgets
    , stTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , stTVarEvent :: TVar EventAusführen
    , stToggleButtonStrom :: Gtk.ToggleButton
    }
    deriving (Eq)

data STWidgetsBoxen =
    STWidgetsBoxen
    { vBoxStreckenabschnitte :: ScrollbaresWidget Gtk.VBox
    , vBoxHinzufügenWegstreckeStreckenabschnitte :: BoxWegstreckeHinzufügen STWidgets
    , vBoxHinzufügenPlanStreckenabschnitte :: BoxPlanHinzufügen STWidgets
    }

class MitSTWidgetsBoxen r where
    stWidgetsBoxen :: r -> STWidgetsBoxen

instance MitWidget STWidgets where
    erhalteWidget :: STWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . stWidget

instance WidgetsTyp STWidgets where
    type ObjektTyp STWidgets = Streckenabschnitt

    type ReaderConstraint STWidgets = MitSTWidgetsBoxen

    erhalteObjektTyp :: STWidgets -> Streckenabschnitt
    erhalteObjektTyp = st

    entferneWidgets :: (MonadIO m, WidgetsTypReader r STWidgets m) => STWidgets -> m ()
    entferneWidgets stWidgets@STWidgets {stTVarSprache} = do
        STWidgetsBoxen {vBoxStreckenabschnitte} <- asks stWidgetsBoxen
        mitContainerRemove vBoxStreckenabschnitte stWidgets
        entferneHinzufügenWegstreckeWidgets stWidgets
        entferneHinzufügenPlanWidgets stWidgets
        liftIO $ atomically $ writeTVar stTVarSprache Nothing

    boxButtonEntfernen :: STWidgets -> Gtk.Box
    boxButtonEntfernen = erhalteBox . stFunctionBox

    tvarSprache :: STWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = stTVarSprache

    tvarEvent :: STWidgets -> TVar EventAusführen
    tvarEvent = stTVarEvent

instance WegstreckenElement STWidgets where
    getterWegstrecke :: Lens.Getter STWidgets (CheckButtonWegstreckeHinzufügen Void STWidgets)
    getterWegstrecke = Lens.to stHinzWS

    boxWegstrecke :: (ReaderConstraint STWidgets r)
                  => Streckenabschnitt
                  -> Lens.Getter r (BoxWegstreckeHinzufügen STWidgets)
    boxWegstrecke
        _stWidgets = Lens.to $ vBoxHinzufügenWegstreckeStreckenabschnitte . stWidgetsBoxen

instance PlanElement STWidgets where
    foldPlan :: Lens.Fold STWidgets (Maybe (ButtonPlanHinzufügen STWidgets))
    foldPlan = Lens.folding $ (: []) . Just . stHinzPL

    boxenPlan :: (ReaderConstraint STWidgets r)
              => Streckenabschnitt
              -> Lens.Fold r (BoxPlanHinzufügen STWidgets)
    boxenPlan _stWidgets = Lens.to $ vBoxHinzufügenPlanStreckenabschnitte . stWidgetsBoxen

instance StreckenObjekt STWidgets where
    anschlüsse :: STWidgets -> Set Anschluss
    anschlüsse STWidgets {st} = anschlüsse st

    erhalteName :: STWidgets -> Text
    erhalteName STWidgets {st} = erhalteName st

instance Aeson.ToJSON STWidgets where
    toJSON :: STWidgets -> Aeson.Value
    toJSON STWidgets {st} = Aeson.toJSON st

instance StreckenabschnittKlasse STWidgets where
    strom :: (I2CReader r m, MonadIO m) => STWidgets -> Strom -> m ()
    strom STWidgets {st, stToggleButtonStrom, stTVarEvent} wert = do
        eventAusführen stTVarEvent $ strom st wert
        liftIO
            $ ohneEvent stTVarEvent
            $ Gtk.set stToggleButtonStrom [Gtk.toggleButtonActive := (wert == Fließend)]

-- | 'Streckenabschnitt' darstellen und zum Status hinzufügen
streckenabschnittPackNew
    :: forall r o m.
    ( WidgetsTypReader r STWidgets m
    , StatusVarReader r o m
    , ObjektKlasse o
    , ST o ~ STWidgets
    , MonadIO m
    )
    => Streckenabschnitt
    -> MStatusAllgemeinT m o STWidgets
streckenabschnittPackNew streckenabschnitt@Streckenabschnitt {stromAnschluss} = do
    STWidgetsBoxen
        {vBoxStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte} <- asks stWidgetsBoxen
    statusVar <- erhalteStatusVar :: m (StatusVar o)
    (stTVarSprache, stTVarEvent) <- liftIO $ do
        stTVarSprache <- newTVarIO $ Just []
        stTVarEvent <- newTVarIO EventAusführen
        pure (stTVarSprache, stTVarEvent)
    let justTVarSprache = Just stTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget
        <- hinzufügenWidgetWegstreckePackNew streckenabschnitt stTVarSprache
    hinzufügenPlanWidget <- hinzufügenWidgetPlanPackNew
        vBoxHinzufügenPlanStreckenabschnitte
        streckenabschnitt
        stTVarSprache
    -- Widget erstellen
    vBox <- boxPackWidgetNewDefault vBoxStreckenabschnitte $ liftIO $ Gtk.vBoxNew False 0
    namePackNew vBox streckenabschnitt
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    boxPackWidgetNewDefault vBoxAnschlüsse
        $ anschlussNew justTVarSprache Language.strom stromAnschluss
    stFunctionBox <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
    stToggleButtonStrom <- toggleButtonStromPackNew
        stFunctionBox
        streckenabschnitt
        stTVarSprache
        stTVarEvent
        statusVar
    fließendPackNew vBoxAnschlüsse streckenabschnitt justTVarSprache
    let stWidgets =
            STWidgets
            { st = streckenabschnitt
            , stWidget = vBox
            , stFunctionBox
            , stHinzPL = hinzufügenPlanWidget
            , stHinzWS = hinzufügenWegstreckeWidget
            , stTVarSprache
            , stTVarEvent
            , stToggleButtonStrom
            }
    buttonEntfernenPackNew stWidgets
        $ (entfernenStreckenabschnitt stWidgets :: IOStatusAllgemein o ())
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OStreckenabschnitt stWidgets
    pure stWidgets

class (StreckenabschnittKlasse st, WidgetsTyp st) => STWidgetsKlasse st where
    toggleButtonStrom :: st -> Maybe Gtk.ToggleButton

instance STWidgetsKlasse STWidgets where
    toggleButtonStrom :: STWidgets -> Maybe Gtk.ToggleButton
    toggleButtonStrom = Just . stToggleButtonStrom

-- | Füge 'Gtk.ToggleButton' zum einstellen des Stroms zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
toggleButtonStromPackNew
    :: forall m b s r o.
    ( WidgetsTypReader r s m
    , SpracheGuiReader r m
    , MitTVarMaps r
    , r ~ ReaderFamilie o
    , MonadIO m
    , MitBox b
    , StreckenabschnittKlasse s
    , STWidgetsKlasse (ST o)
    , STWidgetsKlasse (WS o 'Märklin)
    , STWidgetsKlasse (WS o 'Lego)
    )
    => b
    -> s
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> StatusVar o
    -> m Gtk.ToggleButton
toggleButtonStromPackNew box streckenabschnitt tvarSprachwechsel tvarEventAusführen statusVar = do
    objektReader <- ask
    boxPackWidgetNewDefault box
        $ toggleButtonNewWithEventLabel (Just tvarSprachwechsel) Language.strom
        $ \an -> eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ flip auswertenStatusVarMStatusT statusVar
        $ do
            let fließend =
                    if an
                        then Fließend
                        else Gesperrt
            ausführenAktion $ Strom streckenabschnitt fließend
            -- Widgets synchronisieren
            streckenabschnitte <- getStreckenabschnitte
            liftIO $ forM_ streckenabschnitte $ flip stWidgetsSynchronisieren fließend
            wegstrecken <- getWegstrecken
            liftIO $ forM_ wegstrecken $ flip wsWidgetsSynchronisieren fließend
    where
        stWidgetsSynchronisieren :: ST o -> Strom -> IO ()
        stWidgetsSynchronisieren st@(toggleButtonStrom -> Just toggleButtonStrom) fließend
            | Set.isSubsetOf (enthalteneStreckenabschnitte st)
                $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent (tvarEvent st)
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        stWidgetsSynchronisieren _stWidgets _fließend = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither (WS o) -> Strom -> IO ()
        wsWidgetsSynchronisieren
            (ZugtypMärklin ws@(toggleButtonStrom -> Just toggleButtonStrom))
            fließend
            | Set.isSubsetOf (enthalteneStreckenabschnitte ws)
                $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent (tvarEvent ws)
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        wsWidgetsSynchronisieren
            (ZugtypLego ws@(toggleButtonStrom -> Just toggleButtonStrom))
            fließend
            | Set.isSubsetOf (enthalteneStreckenabschnitte ws)
                $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent (tvarEvent ws)
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        wsWidgetsSynchronisieren _wsWidget _fließend = pure ()