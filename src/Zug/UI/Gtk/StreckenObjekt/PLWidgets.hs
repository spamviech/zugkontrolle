{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zug.UI.Gtk.StreckenObjekt.PLWidgets
  ( PLWidgets()
  , planPackNew
  , PLWidgetsBoxen(..)
  , MitPLWidgetsBoxen(..)
  , PLWidgetsBoxenReader(..)
  , MitWindowMain(..)
  , WindowMainReader(..)
  ) where

import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import qualified Control.Lens as Lens
import Control.Monad (void, forM_)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk
import Numeric.Natural (Natural)

import Zug.Anbindung (StreckenObjekt(..), Anschluss())
import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..))
import Zug.Language (Sprache(), MitSprache(leseSprache), Anzeige(anzeige), (<:>), ($#))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OPlan), ObjektKlasse(..))
import Zug.Plan (Plan(..), PlanKlasse(..), AusführendReader())
import Zug.UI.Base (MStatusAllgemeinT, IOStatusAllgemein, entfernenPlan, AusführenMöglich(..)
                  , ausführenMöglich, ReaderFamilie, MitTVarMaps())
import Zug.UI.Befehl
       (ausführenBefehl, BefehlAllgemein(Hinzufügen, Ausführen, AusführenAbbrechen))
import Zug.UI.Gtk.Hilfsfunktionen
       (containerAddWidgetNew, boxPackWidgetNewDefault, boxPackWidgetNew, Packing(..)
      , paddingDefault, positionDefault, namePackNew, dialogEval)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove, MitBox(..))
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget)
import Zug.UI.Gtk.SpracheGui
       (MitSpracheGui(), SpracheGuiReader(erhalteSpracheGui), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (PlanElement(..), hinzufügenWidgetPlanPackNew, MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufügen
       (Kategorie(..), KategorieText(..), ButtonPlanHinzufügen, BoxPlanHinzufügen)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), eventAusführen
      , buttonEntfernenPackNew)
import Zug.UI.StatusVar (StatusVar, MitStatusVar(), StatusVarReader(erhalteStatusVar)
                       , auswertenStatusVarIOStatus, ausführenStatusVarBefehl)

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

data PLWidgetsBoxen =
    PLWidgetsBoxen
    { vBoxPläne :: ScrollbaresWidget Gtk.VBox
    , vBoxHinzufügenPlanPläne :: BoxPlanHinzufügen PLWidgets
    }

class MitPLWidgetsBoxen r where
    plWidgetsBoxen :: r -> PLWidgetsBoxen

instance MitPLWidgetsBoxen PLWidgetsBoxen where
    plWidgetsBoxen :: PLWidgetsBoxen -> PLWidgetsBoxen
    plWidgetsBoxen = id

class (MonadReader r m, MitPLWidgetsBoxen r) => PLWidgetsBoxenReader r m | m -> r where
    erhaltePLWidgetsBoxen :: m PLWidgetsBoxen
    erhaltePLWidgetsBoxen = asks plWidgetsBoxen

instance (MonadReader r m, MitPLWidgetsBoxen r) => PLWidgetsBoxenReader r m

instance WidgetsTyp PLWidgets where
    type ObjektTyp PLWidgets = Plan

    type ReaderConstraint PLWidgets = MitPLWidgetsBoxen

    erhalteObjektTyp :: PLWidgets -> Plan
    erhalteObjektTyp = pl

    entferneWidgets :: (MonadIO m, WidgetsTypReader r PLWidgets m) => PLWidgets -> m ()
    entferneWidgets plWidgets@PLWidgets {plTVarSprache} = do
        PLWidgetsBoxen {vBoxPläne} <- erhaltePLWidgetsBoxen
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
    boxenPlan _kuWidgets = Lens.to $ vBoxHinzufügenPlanPläne . plWidgetsBoxen

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

class MitWindowMain r where
    windowMain :: r -> Gtk.Window

class (MonadReader r m, MitWindowMain r) => WindowMainReader r m | m -> r where
    erhalteWindowMain :: m Gtk.Window
    erhalteWindowMain = asks windowMain

instance (MonadReader r m, MitWindowMain r ) => WindowMainReader r m

-- | 'Plan' darstellen.
planPackNew
    :: forall o m.
    ( Eq (BG o 'Pwm 'Märklin)
    , Eq (BG o 'KonstanteSpannung 'Märklin)
    , Eq (BG o 'Pwm 'Lego)
    , Eq (BG o 'KonstanteSpannung 'Lego)
    , Eq (ST o)
    , Eq (WE o 'Märklin)
    , Eq (WE o 'Lego)
    , Eq (KU o)
    , Eq (KO o)
    , Eq (WS o 'Märklin)
    , Eq (WS o 'Lego)
    , PL o ~ PLWidgets
    , MitSprache (SP o)
    , ObjektKlasse o
    , Aeson.ToJSON o
    , MitTVarMaps (ReaderFamilie o)
    , MitStatusVar (ReaderFamilie o) o
    , MitSpracheGui (ReaderFamilie o)
    , MitPLWidgetsBoxen (ReaderFamilie o)
    , MitWindowMain (ReaderFamilie o)
    , MitTMVarPlanObjekt (ReaderFamilie o)
    , MonadIO m
    )
    => Plan
    -> MStatusAllgemeinT m o PLWidgets
planPackNew plan@Plan {plAktionen} = do
    statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
    objektReader <- ask
    spracheGui <- erhalteSpracheGui
    windowMain <- erhalteWindowMain
    PLWidgetsBoxen {vBoxPläne, vBoxHinzufügenPlanPläne} <- erhaltePLWidgetsBoxen
    ( plTVarSprache
        , plTVarEvent
        , frame
        , functionBox
        , expander
        , buttonAusführen
        , buttonAbbrechen
        , dialogGesperrt
        ) <- liftIO $ do
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
        functionBox <- boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
        buttonAusführen <- boxPackWidgetNew functionBox PackNatural paddingDefault positionDefault
            $ Gtk.buttonNew
        buttonAbbrechen <- boxPackWidgetNew functionBox PackNatural paddingDefault positionDefault
            $ Gtk.buttonNew
        Gtk.widgetHide buttonAbbrechen
        dialogGesperrt
            <- Gtk.messageDialogNew (Just windowMain) [] Gtk.MessageError Gtk.ButtonsOk Text.empty
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
            , plFunktionBox = erhalteBox functionBox
            , plHinzPL
            , plTVarSprache
            , plTVarEvent
            }
    buttonEntfernenPackNew plWidgets $ (entfernenPlan plWidgets :: IOStatusAllgemein o ())
    let justTVarSprache = Just plTVarSprache
    verwendeSpracheGui justTVarSprache $ \sprache -> do
        Gtk.set expander [Gtk.expanderLabel := (Language.aktionen <:> length plAktionen $ sprache)]
        Gtk.set buttonAusführen [Gtk.buttonLabel := Language.ausführen sprache]
        Gtk.set buttonAbbrechen [Gtk.buttonLabel := Language.ausführenAbbrechen sprache]
        Gtk.set dialogGesperrt [Gtk.windowTitle := Language.aktionGesperrt sprache]
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OPlan plWidgets
    pure plWidgets