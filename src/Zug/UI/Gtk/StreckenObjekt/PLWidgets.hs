{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
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
{-# LANGUAGE StandaloneDeriving #-}
#endif

module Zug.UI.Gtk.StreckenObjekt.PLWidgets
  (
#ifdef ZUGKONTROLLEGUI
    -- * PLWidgets
    PLWidgets()
  , PlanGui
  , AktionGui
  , planPackNew
  , planGui
  , PLWidgetsBoxen(..)
  , MitPLWidgetsBoxen(..)
  , PLWidgetsBoxenReader(..)
  , MitWindowMain(..)
  , WindowMainReader(..)
    -- * ObjektGui (hier definiert um Orphan-Instances zu vermeiden)
  , ObjektGui
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import qualified Control.Lens as Lens
import Control.Monad (void, forM_)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import Data.Text (Text)
import GI.Gtk (AttrOp((:=)))
import qualified GI.Gtk as Gtk
import Numeric.Natural (Natural)

import Zug.Anbindung (StreckenObjekt(..), AnschlussEither())
import Zug.Language (MitSprache(leseSprache), Anzeige(anzeige), (<:>), ($#))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OPlan), ObjektKlasse(..), ObjektElement(..))
import Zug.Plan (PlanAllgemein(..), Plan, PlanKlasse(..), AusführendReader(), AktionAllgemein())
import Zug.UI.Base (MStatusAllgemeinT, IOStatusAllgemein, entfernenPlan, AusführenMöglich(..)
                  , ausführenMöglich, ReaderFamilie, MitTVarMaps())
import Zug.UI.Befehl
       (ausführenBefehl, BefehlAllgemein(Hinzufügen, Ausführen, AusführenAbbrechen))
import Zug.UI.Gtk.Hilfsfunktionen
       (containerAddWidgetNew, boxPackWidgetNewDefault, boxPackWidgetNew, Packing(..)
      , paddingDefault, positionDefault, namePackNew, dialogEval, labelSpracheNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove)
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget)
import Zug.UI.Gtk.SpracheGui (SpracheGui, MitSpracheGui(), SpracheGuiReader(erhalteSpracheGui)
                            , verwendeSpracheGui, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt.BGWidgets (BGWidgets)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (PlanElement(..), hinzufügenWidgetPlanPackNew, MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.KOWidgets (KOWidgets)
import Zug.UI.Gtk.StreckenObjekt.KUWidgets (KUWidgets)
import Zug.UI.Gtk.StreckenObjekt.STWidgets (STWidgets)
import Zug.UI.Gtk.StreckenObjekt.WEWidgets (WEWidgets)
import Zug.UI.Gtk.StreckenObjekt.WSWidgets (WSWidgets)
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufuegen
       (Kategorie(..), KategorieText(..), ButtonPlanHinzufügen, BoxPlanHinzufügen)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), eventAusführen
      , buttonEntfernenPackNew, buttonBearbeitenPackNew, MitAktionBearbeiten())
import Zug.UI.StatusVar (StatusVar, MitStatusVar(), StatusVarReader(erhalteStatusVar)
                       , auswertenStatusVarIOStatus, ausführenStatusVarBefehl)

instance Kategorie PLWidgets where
    kategorie :: KategorieText PLWidgets
    kategorie = KategorieText Language.pläne

-- | 'PlanAllgemein' spezialisiert auf Gui-Typen.
type PlanGui = PlanAllgemein BGWidgets STWidgets WEWidgets KUWidgets KOWidgets WSWidgets

-- | 'AktionAllgemein' spezialisier auf Gui-Typen.
type AktionGui = AktionAllgemein BGWidgets STWidgets WEWidgets KUWidgets KOWidgets WSWidgets

-- | 'Plan' mit zugehörigen Widgets
data PLWidgets =
    PLWidgets
    { pl :: PlanGui
    , plWidget :: Gtk.Frame
    , plFunctionBox :: Gtk.Box
    , plHinzPL :: ButtonPlanHinzufügen PLWidgets
    , plTVarSprache :: TVarSprachewechselAktionen
    , plTVarEvent :: TVar EventAusführen
    }

-- | Erhalte den 'PlanGui' eines 'PLWidgets'.
planGui :: PLWidgets -> PlanGui
planGui = pl

deriving instance Eq PLWidgets

instance MitWidget PLWidgets where
    erhalteWidget :: (MonadIO m) => PLWidgets -> m Gtk.Widget
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

instance ObjektElement PLWidgets where
    type ObjektTyp PLWidgets = Plan

    zuObjektTyp :: PLWidgets -> Plan
    zuObjektTyp = zuObjektTyp . pl

instance WidgetsTyp PLWidgets where
    type ReaderConstraint PLWidgets = MitPLWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r PLWidgets m) => PLWidgets -> m ()
    entferneWidgets plWidgets@PLWidgets {plTVarSprache} = do
        PLWidgetsBoxen {vBoxPläne} <- erhaltePLWidgetsBoxen
        mitContainerRemove vBoxPläne plWidgets
        liftIO $ atomically $ writeTVar plTVarSprache Nothing

    boxButtonEntfernen :: PLWidgets -> Gtk.Box
    boxButtonEntfernen = plFunctionBox

    tvarSprache :: PLWidgets -> TVarSprachewechselAktionen
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
    anschlüsse :: PLWidgets -> Set AnschlussEither
    anschlüsse = anschlüsse . zuObjektTyp . pl

    erhalteName :: PLWidgets -> Text
    erhalteName = erhalteName . zuObjektTyp . pl

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

instance (MonadReader r m, MitWindowMain r) => WindowMainReader r m

-- | 'Plan' darstellen.
planPackNew :: forall m.
            ( MitTVarMaps (ReaderFamilie ObjektGui)
            , MitStatusVar (ReaderFamilie ObjektGui) ObjektGui
            , MitSpracheGui (ReaderFamilie ObjektGui)
            , MitPLWidgetsBoxen (ReaderFamilie ObjektGui)
            , MitWindowMain (ReaderFamilie ObjektGui)
            , MitTMVarPlanObjekt (ReaderFamilie ObjektGui)
            , MitAktionBearbeiten (ReaderFamilie ObjektGui)
            , MonadIO m
            )
            => PlanGui
            -> MStatusAllgemeinT m ObjektGui PLWidgets
planPackNew plan@Plan {plAktionen} = do
    statusVar <- erhalteStatusVar :: MStatusAllgemeinT m ObjektGui (StatusVar ObjektGui)
    objektReader <- ask
    spracheGui <- erhalteSpracheGui
    parent <- erhalteWindowMain
    PLWidgetsBoxen {vBoxPläne, vBoxHinzufügenPlanPläne} <- erhaltePLWidgetsBoxen
    ( plTVarSprache
        , plTVarEvent
        , frame
        , plFunctionBox
        , expander
        , vBoxExpander
        , buttonAusführen
        , buttonAbbrechen
        , dialogGesperrt
        ) <- liftIO $ do
        plTVarSprache <- newTVarIO $ Just []
        plTVarEvent <- newTVarIO EventAusführen
        -- Widget erstellen
        frame <- boxPackWidgetNewDefault vBoxPläne $ Gtk.frameNew Nothing
        vBox <- containerAddWidgetNew frame $ Gtk.boxNew Gtk.OrientationVertical 0
        namePackNew vBox $ zuObjektTyp plan
        expander <- boxPackWidgetNewDefault vBox $ Gtk.expanderNew Nothing
        vBoxExpander <- containerAddWidgetNew expander $ Gtk.boxNew Gtk.OrientationVertical 0
        plFunctionBox <- boxPackWidgetNewDefault vBox $ Gtk.boxNew Gtk.OrientationHorizontal 0
        buttonAusführen
            <- boxPackWidgetNew plFunctionBox PackNatural paddingDefault positionDefault
            $ Gtk.buttonNew
        buttonAbbrechen
            <- boxPackWidgetNew plFunctionBox PackNatural paddingDefault positionDefault
            $ Gtk.buttonNew
        Gtk.widgetHide buttonAbbrechen
        dialogGesperrt <- Gtk.new
            Gtk.MessageDialog
            [ Gtk.windowTransientFor := parent
            , Gtk.messageDialogMessageType := Gtk.MessageTypeError
            , Gtk.messageDialogButtons := Gtk.ButtonsTypeOk]
        progressBar <- boxPackWidgetNew
            plFunctionBox
            PackGrow
            paddingDefault
            positionDefault
            Gtk.progressBarNew
        Gtk.onButtonClicked buttonAusführen
            $ flip runReaderT objektReader
            $ auswertenStatusVarIOStatus (ausführenMöglich $ zuObjektTyp plan) statusVar >>= \case
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
                (AnschlüsseBelegt belegteAnschlüsse) -> void $ do
                    liftIO $ flip leseSprache spracheGui $ \sprache -> Gtk.set
                        dialogGesperrt
                        [ Gtk.messageDialogText
                              := (Language.ausführenGesperrt $# belegteAnschlüsse) sprache]
                    dialogEval dialogGesperrt
        Gtk.onButtonClicked buttonAbbrechen $ do
            flip runReaderT objektReader
                $ ausführenStatusVarBefehl (AusführenAbbrechen $ zuObjektTyp plan) statusVar
            Gtk.widgetShow buttonAusführen
            Gtk.widgetHide buttonAbbrechen
        pure
            ( plTVarSprache
            , plTVarEvent
            , frame
            , plFunctionBox
            , expander
            , vBoxExpander
            , buttonAusführen
            , buttonAbbrechen
            , dialogGesperrt
            )
    forM_ plAktionen
        $ boxPackWidgetNewDefault vBoxExpander . labelSpracheNew (Just plTVarSprache) . anzeige
    plHinzPL
        <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanPläne (zuObjektTyp plan) plTVarSprache
    let plWidgets =
            PLWidgets
            { pl = plan
            , plWidget = frame
            , plFunctionBox
            , plHinzPL
            , plTVarSprache
            , plTVarEvent
            }
    buttonEntfernenPackNew plWidgets $ (entfernenPlan plWidgets :: IOStatusAllgemein ObjektGui ())
    buttonBearbeitenPackNew plWidgets
    let justTVarSprache = Just plTVarSprache
    verwendeSpracheGui justTVarSprache $ \sprache -> do
        Gtk.set expander [Gtk.expanderLabel := (Language.aktionen <:> length plAktionen $ sprache)]
        Gtk.set buttonAusführen [Gtk.buttonLabel := Language.ausführen sprache]
        Gtk.set buttonAbbrechen [Gtk.buttonLabel := Language.ausführenAbbrechen sprache]
        Gtk.set dialogGesperrt [Gtk.windowTitle := Language.aktionGesperrt sprache]
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OPlan plWidgets
    pure plWidgets

instance ObjektKlasse ObjektGui where
    type BG ObjektGui = BGWidgets

    type ST ObjektGui = STWidgets

    type WE ObjektGui = WEWidgets

    type KU ObjektGui = KUWidgets

    type KO ObjektGui = KOWidgets

    type WS ObjektGui = WSWidgets

    type PL ObjektGui = PLWidgets

    type SP ObjektGui = SpracheGui

    erhalteObjekt :: ObjektGui -> ObjektGui
    erhalteObjekt = id

    ausObjekt :: ObjektGui -> ObjektGui
    ausObjekt = id

type ObjektGui =
    ObjektAllgemein BGWidgets STWidgets WEWidgets KUWidgets KOWidgets WSWidgets PLWidgets
#endif
--
