{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Zug.UI.Gtk.StreckenObjekt.STWidgets (STWidgets(), STWidgetsKlasse(..)) where

import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Lens as Lens
import Control.Monad.Trans (MonadIO())
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (Streckenabschnitt(), StreckenabschnittKlasse(..), StreckenObjekt(..)
                    , Anschluss(), I2CReader())
import Zug.Enums (Strom())
import Zug.Language (Sprache())
import qualified Zug.Language as Language
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen (WegstreckenElement(..), PlanElement(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen
       (Kategorie(..), KategorieText(..), BoxWegstreckeHinzufügen, CheckButtonWegstreckeHinzufügen
      , BoxPlanHinzufügen, ButtonPlanHinzufügen)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp (WidgetsTyp(..), EventAusführen())

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
    , stTogglebuttonStrom :: Gtk.ToggleButton
    }
    deriving (Eq)

instance MitWidget STWidgets where
    erhalteWidget :: STWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . stWidget

instance WidgetsTyp STWidgets where
    type ObjektTyp STWidgets = Streckenabschnitt

    erhalteObjektTyp :: STWidgets -> Streckenabschnitt
    erhalteObjektTyp = st

    entferneWidgets :: (MonadIO m, DynamischeWidgetsReader r m) => STWidgets -> m ()
    entferneWidgets stWidgets@STWidgets {stTVarSprache} = do
        DynamischeWidgets {vBoxStreckenabschnitte} <- erhalteDynamischeWidgets
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

    boxWegstrecke :: Streckenabschnitt
                  -> Lens.Getter DynamischeWidgets (BoxWegstreckeHinzufügen STWidgets)
    boxWegstrecke _stWidgets = Lens.to vBoxHinzufügenWegstreckeStreckenabschnitte

instance PlanElement STWidgets where
    foldPlan :: Lens.Fold STWidgets (Maybe (ButtonPlanHinzufügen STWidgets))
    foldPlan = Lens.folding $ (: []) . Just . stHinzPL

    boxenPlan :: Streckenabschnitt -> Lens.Fold DynamischeWidgets (BoxPlanHinzufügen STWidgets)
    boxenPlan _stWidgets = Lens.to vBoxHinzufügenPlanStreckenabschnitte

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
    strom STWidgets {st, stTogglebuttonStrom, stTVarEvent} wert = do
        eventAusführen stTVarEvent $ strom st wert
        liftIO
            $ ohneEvent stTVarEvent
            $ Gtk.set stTogglebuttonStrom [Gtk.toggleButtonActive := (wert == Fließend)]

-- | 'Streckenabschnitt' darstellen und zum Status hinzufügen
streckenabschnittPackNew
    :: forall m. (ObjektGuiReader m, MonadIO m) => Streckenabschnitt -> MStatusGuiT m STWidgets
streckenabschnittPackNew streckenabschnitt@Streckenabschnitt {stromAnschluss} = do
    DynamischeWidgets
        {vBoxStreckenabschnitte, vBoxHinzufügenPlanStreckenabschnitte} <- erhalteDynamischeWidgets
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
    stTogglebuttonStrom
        <- toggleButtonStromPackNew stFunctionBox streckenabschnitt stTVarSprache stTVarEvent
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
            , stTogglebuttonStrom
            }
    buttonEntfernenPackNew stWidgets $ entfernenStreckenabschnitt stWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OStreckenabschnitt stWidgets
    pure stWidgets

class (WidgetsTyp st) => STWidgetsKlasse st where
    toggleButtonStrom :: st -> Maybe Gtk.ToggleButton

-- | Füge 'Gtk.ToggleButton' zum einstellen des Stroms zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
toggleButtonStromPackNew
    :: forall m b s r o.
    ( WidgetsTypReader r s m
    , MonadIO m
    , MitBox b
    , StreckenabschnittKlasse s
    , StatusVarReader r o m
    , STWidgetsKlasse (ST o)
    , STWidgetsKlasse (WS o)
    )
    => b
    -> s
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> m Gtk.ToggleButton
toggleButtonStromPackNew box streckenabschnitt tvarSprachwechsel tvarEventAusführen = do
    statusVar <- erhalteStatusVar :: m (StatusVar o)
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
        stWidgetsSynchronisieren :: STWidgets -> Strom -> IO ()
        stWidgetsSynchronisieren STWidgets {st, stTVarEvent, stTogglebuttonStrom} fließend
            | elem st $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent stTVarEvent
                $ Gtk.set stTogglebuttonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        stWidgetsSynchronisieren _stWidgets _fließend = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither WSWidgets -> Strom -> IO ()
        wsWidgetsSynchronisieren
            (ZugtypMärklin
                 WSWidgets { ws = Wegstrecke {wsStreckenabschnitte}
                           , wsTVarEvent
                           , wsToggleButtonStrom = Just toggleButtonStrom})
            fließend
            | Set.isSubsetOf wsStreckenabschnitte $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent wsTVarEvent
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        wsWidgetsSynchronisieren
            (ZugtypLego
                 WSWidgets { ws = Wegstrecke {wsStreckenabschnitte}
                           , wsTVarEvent
                           , wsToggleButtonStrom = Just toggleButtonStrom})
            fließend
            | Set.isSubsetOf wsStreckenabschnitte $ enthalteneStreckenabschnitte streckenabschnitt =
                ohneEvent wsTVarEvent
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := (fließend == Fließend)]
        wsWidgetsSynchronisieren _wsWidget _fließend = pure ()