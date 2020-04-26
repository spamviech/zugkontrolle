{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Zug.UI.Gtk.StreckenObjekt.PLWidgets (PLWidgets(), planPackNew) where

import qualified Data.Aeson as Aeson
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(..))
import Zug.Plan (Plan(..), PlanKlasse(..))
import Zug.UI.Gtk.Klassen (MitWidget(..))
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen (PlanElement(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp (WidgetsTyp(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen (Kategorie(..), KategorieText(..))
import qualified Zug.Language as Language

instance Kategorie PLWidgets where
    kategorie :: KategorieText PLWidgets
    kategorie = KategorieText Language.pläne

-- | 'Plan' mit zugehörigen Widgets
data PLWidgets =
    PLWidgets
    { pl :: Plan
    , plWidget :: Gtk.Frame
    , plFunktionBox :: Gtk.Box
    , plHinzPL :: ButtonPlanHinzufügen PLWidgets
    , plTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , plTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

instance MitWidget PLWidgets where
    erhalteWidget :: PLWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . plWidget

instance WidgetsTyp PLWidgets where
    type ObjektTyp PLWidgets = Plan

    erhalteObjektTyp :: PLWidgets -> Plan
    erhalteObjektTyp = pl

    entferneWidgets :: (MonadIO m, WidgetsTypReader r PLWidgets m) => PLWidgets -> m ()
    entferneWidgets plWidgets@PLWidgets {plTVarSprache} = do
        DynamischeWidgets {vBoxPläne} <- erhalteDynamischeWidgets
        mitContainerRemove vBoxPläne plWidgets
        liftIO $ atomically $ writeTVar plTVarSprache Nothing

    boxButtonEntfernen :: PLWidgets -> Gtk.Box
    boxButtonEntfernen = plFunktionBox

    tvarSprache :: PLWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = plTVarSprache

    tvarEvent :: PLWidgets -> TVar EventAusführen
    tvarEvent = plTVarEvent

instance PlanElement PLWidgets where
    foldPlan :: Lens.Fold PLWidgets (Maybe (ButtonPlanHinzufügen PLWidgets))
    foldPlan = Lens.to $ Just . plHinzPL

    boxenPlan
        :: (ReaderConstraint PLWidgets r) => Plan -> Lens.Fold r (BoxPlanHinzufügen PLWidgets)
    boxenPlan _kuWidgets = Lens.to vBoxHinzufügenPlanPläne

instance StreckenObjekt PLWidgets where
    anschlüsse :: PLWidgets -> Set Anschluss
    anschlüsse = anschlüsse . pl

    erhalteName :: PLWidgets -> Text
    erhalteName = erhalteName . pl

instance Aeson.ToJSON PLWidgets where
    toJSON :: PLWidgets -> Aeson.Value
    toJSON = Aeson.toJSON . pl

instance PlanKlasse PLWidgets where
    ausführenPlan
        :: (AusführendReader r m, MonadIO m) => PLWidgets -> (Natural -> IO ()) -> IO () -> m ()
    ausführenPlan PLWidgets {pl, plTVarEvent} anzeigeAktion abschlussAktion =
        eventAusführen plTVarEvent $ ausführenPlan pl anzeigeAktion abschlussAktion

-- | 'Plan' darstellen
planPackNew :: forall m. (ObjektGuiReader m, MonadIO m) => Plan -> MStatusGuiT m PLWidgets
planPackNew plan@Plan {plAktionen} = do
    statusVar <- erhalteStatusVar :: MStatusGuiT m StatusVarGui
    objektReader <- ask
    spracheGui <- erhalteSpracheGui
    DynamischeWidgets
        {vBoxPläne, windowMain, vBoxHinzufügenPlanPläne} <- erhalteDynamischeWidgets
    (plTVarSprache, plTVarEvent, frame, fnBox, expander, btAusführen, btAbbrechen, dialogGesperrt)
        <- liftIO $ do
            plTVarSprache <- newTVarIO $ Just []
            plTVarEvent <- newTVarIO EventAusführen
            -- Widget erstellen
            frame <- boxPackWidgetNewDefault vBoxPläne $ Gtk.frameNew
            vBox <- containerAddWidgetNew frame $ Gtk.vBoxNew False 0
            namePackNew vBox plan
            expander <- boxPackWidgetNewDefault vBox $ Gtk.expanderNew $ Text.empty
            vBoxExpander <- containerAddWidgetNew expander $ Gtk.vBoxNew False 0
            forM_ plAktionen
                $ boxPackWidgetNewDefault vBoxExpander
                . Gtk.labelNew
                . Just
                . flip leseSprache spracheGui
                . anzeige
            functionBox <- boxPackWidgetNewDefault vBox Gtk.hButtonBoxNew
            buttonAusführen
                <- boxPackWidgetNew functionBox PackNatural paddingDefault positionDefault
                $ Gtk.buttonNew
            buttonAbbrechen
                <- boxPackWidgetNew functionBox PackNatural paddingDefault positionDefault
                $ Gtk.buttonNew
            Gtk.widgetHide buttonAbbrechen
            dialogGesperrt <- Gtk.messageDialogNew
                (Just windowMain)
                []
                Gtk.MessageError
                Gtk.ButtonsOk
                ("" :: Text)
            progressBar <- boxPackWidgetNew
                functionBox
                PackGrow
                paddingDefault
                positionDefault
                Gtk.progressBarNew
            Gtk.on buttonAusführen Gtk.buttonActivated
                $ flip runReaderT objektReader
                $ auswertenStatusVarIOStatus (ausführenMöglich plan) statusVar >>= \case
                    AusführenMöglich -> void $ do
                        liftIO $ do
                            Gtk.widgetHide buttonAusführen
                            Gtk.widgetShow buttonAbbrechen
                        ausführenStatusVarBefehl
                            (Ausführen plan (const . anzeigeAktion) abschlussAktion)
                            statusVar
                        where
                            anzeigeAktion :: Natural -> IO ()
                            anzeigeAktion wert =
                                Gtk.set
                                    progressBar
                                    [ Gtk.progressBarFraction := (fromIntegral wert)
                                          / (fromIntegral $ length plAktionen)]

                            abschlussAktion :: IO ()
                            abschlussAktion = do
                                Gtk.widgetShow buttonAusführen
                                Gtk.widgetHide buttonAbbrechen
                    WirdAusgeführt -> error "Ausführen in GTK-UI erneut gestartet."
                    (AnschlüsseBelegt anschlüsse) -> void $ do
                        liftIO $ flip leseSprache spracheGui $ \sprache -> Gtk.set
                            dialogGesperrt
                            [ Gtk.messageDialogText := Just
                                  $ (Language.ausführenGesperrt $# anschlüsse) sprache]
                        dialogEval dialogGesperrt
            Gtk.on buttonAbbrechen Gtk.buttonActivated $ do
                flip runReaderT objektReader
                    $ ausführenStatusVarBefehl (AusführenAbbrechen plan) statusVar
                Gtk.widgetShow buttonAusführen
                Gtk.widgetHide buttonAbbrechen
            pure
                ( plTVarSprache
                , plTVarEvent
                , frame
                , functionBox
                , expander
                , buttonAusführen
                , buttonAbbrechen
                , dialogGesperrt
                )
    plHinzPL <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanPläne plan plTVarSprache
    let plWidgets =
            PLWidgets
            { pl = plan
            , plWidget = frame
            , plFunktionBox = erhalteBox fnBox
            , plHinzPL
            , plTVarSprache
            , plTVarEvent
            }
    buttonEntfernenPackNew plWidgets $ entfernenPlan plWidgets
    let justTVarSprache = Just plTVarSprache
    verwendeSpracheGui justTVarSprache $ \sprache -> do
        Gtk.set expander [Gtk.expanderLabel := (Language.aktionen <:> length plAktionen $ sprache)]
        Gtk.set btAusführen [Gtk.buttonLabel := Language.ausführen sprache]
        Gtk.set btAbbrechen [Gtk.buttonLabel := Language.ausführenAbbrechen sprache]
        Gtk.set dialogGesperrt [Gtk.windowTitle := Language.aktionGesperrt sprache]
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ OPlan plWidgets
    pure plWidgets