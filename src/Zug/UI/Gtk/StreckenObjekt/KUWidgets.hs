{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Zug.UI.Gtk.StreckenObjekt.KUWidgets (KUWidgets(), kupplungPackNew) where

import qualified Data.Aeson as Aeson
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (Kupplung(..), KupplungKlasse(..))
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen (WegstreckenElement(..), PlanElement(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen (Kategorie(..), KategorieText(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp (WidgetsTyp(..))

instance Kategorie KUWidgets where
    kategorie :: KategorieText KUWidgets
    kategorie = KategorieText Language.kupplungen

-- | 'Kupplung' mit zugehörigen Widgets
data KUWidgets =
    KUWidgets
    { ku :: Kupplung
    , kuWidget :: Gtk.VBox
    , kuFunctionBox :: Gtk.HBox
    , kuHinzWS :: CheckButtonWegstreckeHinzufügen Void KUWidgets
    , kuHinzPL :: ButtonPlanHinzufügen KUWidgets
    , kuTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , kuTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

instance MitWidget KUWidgets where
    erhalteWidget :: KUWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . kuWidget

instance WidgetsTyp KUWidgets where
    type ObjektTyp KUWidgets = Kupplung

    erhalteObjektTyp :: KUWidgets -> Kupplung
    erhalteObjektTyp = ku

    entferneWidgets :: (MonadIO m, WidgetsTypReader r KUWidgets m) => KUWidgets -> m ()
    entferneWidgets kuWidgets@KUWidgets {kuTVarSprache} = do
        DynamischeWidgets {vBoxKupplungen} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxKupplungen kuWidgets
        entferneHinzufügenWegstreckeWidgets kuWidgets
        entferneHinzufügenPlanWidgets kuWidgets
        liftIO $ atomically $ writeTVar kuTVarSprache Nothing

    boxButtonEntfernen :: KUWidgets -> Gtk.Box
    boxButtonEntfernen = erhalteBox . kuFunctionBox

    tvarSprache :: KUWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = kuTVarSprache

    tvarEvent :: KUWidgets -> TVar EventAusführen
    tvarEvent = kuTVarEvent

instance WegstreckenElement KUWidgets where
    getterWegstrecke :: Lens.Getter KUWidgets (CheckButtonWegstreckeHinzufügen Void KUWidgets)
    getterWegstrecke = Lens.to kuHinzWS

    boxWegstrecke :: (ReaderConstraint KUWidgets r)
                  => Kupplung
                  -> Lens.Getter r (BoxWegstreckeHinzufügen KUWidgets)
    boxWegstrecke _kuWidgets = Lens.to vBoxHinzufügenWegstreckeKupplungen

instance PlanElement KUWidgets where
    foldPlan :: Lens.Fold KUWidgets (Maybe (ButtonPlanHinzufügen KUWidgets))
    foldPlan = Lens.to $ Just . kuHinzPL

    boxenPlan
        :: (ReaderConstraint KUWidgets r) => Kupplung -> Lens.Fold r (BoxPlanHinzufügen KUWidgets)
    boxenPlan _kuWidgets = Lens.to vBoxHinzufügenPlanKupplungen

instance StreckenObjekt KUWidgets where
    anschlüsse :: KUWidgets -> Set Anschluss
    anschlüsse KUWidgets {ku} = anschlüsse ku

    erhalteName :: KUWidgets -> Text
    erhalteName KUWidgets {ku} = erhalteName ku

instance Aeson.ToJSON KUWidgets where
    toJSON :: KUWidgets -> Aeson.Value
    toJSON KUWidgets {ku} = Aeson.toJSON ku

instance KupplungKlasse KUWidgets where
    kuppeln :: (I2CReader r m, MonadIO m) => KUWidgets -> m ()
    kuppeln KUWidgets {ku} = kuppeln ku

-- | 'Kupplung' darstellen und zum Status hinzufügen
kupplungPackNew :: forall m. (ObjektGuiReader m, MonadIO m) => Kupplung -> MStatusGuiT m KUWidgets
kupplungPackNew kupplung@Kupplung {kupplungsAnschluss} = do
    DynamischeWidgets {vBoxKupplungen, vBoxHinzufügenPlanKupplungen} <- erhalteDynamischeWidgets
    (kuTVarSprache, kuTVarEvent) <- liftIO $ do
        kuTVarSprache <- newTVarIO $ Just []
        kuTVarEvent <- newTVarIO EventAusführen
        pure (kuTVarSprache, kuTVarEvent)
    let justTVarSprache = Just kuTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckePackNew kupplung kuTVarSprache
    hinzufügenPlanWidget
        <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanKupplungen kupplung kuTVarSprache
    -- Widget erstellen
    (vBox, kuFunctionBox) <- liftIO $ do
        vBox <- boxPackWidgetNewDefault vBoxKupplungen $ Gtk.vBoxNew False 0
        kuFunctionBox <- boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
        pure (vBox, kuFunctionBox)
    let kuWidgets =
            KUWidgets
            { ku = kupplung
            , kuWidget = vBox
            , kuFunctionBox
            , kuHinzPL = hinzufügenPlanWidget
            , kuHinzWS = hinzufügenWegstreckeWidget
            , kuTVarSprache
            , kuTVarEvent
            }
    namePackNew kuFunctionBox kupplung
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse
            <- boxPackWidgetNew kuFunctionBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    boxPackWidgetNewDefault vBoxAnschlüsse
        $ anschlussNew justTVarSprache Language.kupplung kupplungsAnschluss
    buttonKuppelnPackNew kuFunctionBox kupplung kuTVarSprache kuTVarEvent
    fließendPackNew vBoxAnschlüsse kupplung justTVarSprache
    buttonEntfernenPackNew kuWidgets $ entfernenKupplung kuWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OKupplung kuWidgets
    pure kuWidgets

-- | Füge 'Gtk.Button' zum kuppeln zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus
-- 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonKuppelnPackNew
    :: forall b k m.
    (MitBox b, KupplungKlasse k, ObjektGuiReader m, MonadIO m)
    => b
    -> k
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> m Gtk.Button
buttonKuppelnPackNew box kupplung tvarSprachwechsel tvarEventAusführen = do
    statusVar <- erhalteStatusVar :: m StatusVarGui
    objektReader <- ask
    boxPackWidgetNewDefault box
        $ buttonNewWithEventLabel (Just tvarSprachwechsel) Language.kuppeln
        $ eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ ausführenStatusVarAktion (Kuppeln kupplung) statusVar