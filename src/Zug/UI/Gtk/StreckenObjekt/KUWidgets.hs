{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
#endif

module Zug.UI.Gtk.StreckenObjekt.KUWidgets
  (
#ifdef ZUGKONTROLLEGUI
    KUWidgets()
  , kupplungPackNew
  , buttonKuppelnPackNew
  , KUWidgetsBoxen(..)
  , MitKUWidgetsBoxen(..)
  , KUWidgetsBoxenReader(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import qualified Control.Lens as Lens
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import GI.Gtk (AttrOp((:=)))
import qualified GI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(..), Kupplung(..), KupplungKlasse(..), KupplungContainer(..)
                    , AnschlussEither(), I2CReader)
import Zug.Language (Sprache())
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OKupplung), ObjektKlasse(..), ObjektElement(..))
import Zug.Plan (AktionKupplung(..))
import Zug.UI.Base (StatusAllgemein(), ObjektReader(), MStatusAllgemeinT, IOStatusAllgemein
                  , entfernenKupplung, ReaderFamilie, MitTVarMaps())
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen), BefehlConstraints)
import Zug.UI.Gtk.Anschluss (anschlussNew)
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledVar)
import Zug.UI.Gtk.Hilfsfunktionen
       (containerAddWidgetNew, boxPackWidgetNewDefault, boxPackWidgetNew, Packing(PackGrow)
      , paddingDefault, positionDefault, namePackNew, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove, MitBox(..))
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (SpracheGui, MitSpracheGui(), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (WegstreckenElement(..), entferneHinzufügenWegstreckeWidgets
      , hinzufügenWidgetWegstreckePackNew, PlanElement(..), entferneHinzufügenPlanWidgets
      , hinzufügenWidgetPlanPackNew, MitFortfahrenWennToggledWegstrecke()
      , WegstreckeCheckButtonVoid, FortfahrenWennToggledWegstreckeReader(..), MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufuegen
       (Kategorie(..), KategorieText(..), CheckButtonWegstreckeHinzufügen, BoxWegstreckeHinzufügen
      , ButtonPlanHinzufügen, BoxPlanHinzufügen)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), eventAusführen
      , buttonEntfernenPackNew, buttonBearbeitenPackNew, MitAktionBearbeiten())
import Zug.UI.StatusVar
       (StatusVar, MitStatusVar(), StatusVarReader(erhalteStatusVar), ausführenStatusVarAktion)

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
    , kuButtonKuppeln :: Gtk.Button
    }
    deriving (Eq)

instance MitWidget KUWidgets where
    erhalteWidget :: KUWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . kuWidget

data KUWidgetsBoxen =
    KUWidgetsBoxen
    { vBoxKupplungen :: ScrollbaresWidget Gtk.VBox
    , vBoxHinzufügenWegstreckeKupplungen :: BoxWegstreckeHinzufügen KUWidgets
    , vBoxHinzufügenPlanKupplungen :: BoxPlanHinzufügen KUWidgets
    }

class MitKUWidgetsBoxen r where
    kuWidgetsBoxen :: r -> KUWidgetsBoxen

instance MitKUWidgetsBoxen KUWidgetsBoxen where
    kuWidgetsBoxen :: KUWidgetsBoxen -> KUWidgetsBoxen
    kuWidgetsBoxen = id

class (MonadReader r m, MitKUWidgetsBoxen r) => KUWidgetsBoxenReader r m | m -> r where
    erhalteKUWidgetsBoxen :: m KUWidgetsBoxen
    erhalteKUWidgetsBoxen = asks kuWidgetsBoxen

instance (MonadReader r m, MitKUWidgetsBoxen r) => KUWidgetsBoxenReader r m

instance ObjektElement KUWidgets where
    type ObjektTyp KUWidgets = Kupplung

    zuObjektTyp :: KUWidgets -> Kupplung
    zuObjektTyp = ku

instance WidgetsTyp KUWidgets where
    type ReaderConstraint KUWidgets = MitKUWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r KUWidgets m) => KUWidgets -> m ()
    entferneWidgets kuWidgets@KUWidgets {kuTVarSprache} = do
        KUWidgetsBoxen {vBoxKupplungen} <- erhalteKUWidgetsBoxen
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
    boxWegstrecke _kuWidgets = Lens.to $ vBoxHinzufügenWegstreckeKupplungen . kuWidgetsBoxen

instance PlanElement KUWidgets where
    foldPlan :: Lens.Fold KUWidgets (Maybe (ButtonPlanHinzufügen KUWidgets))
    foldPlan = Lens.to $ Just . kuHinzPL

    boxenPlan
        :: (ReaderConstraint KUWidgets r) => Kupplung -> Lens.Fold r (BoxPlanHinzufügen KUWidgets)
    boxenPlan _kuWidgets = Lens.to $ vBoxHinzufügenPlanKupplungen . kuWidgetsBoxen

instance StreckenObjekt KUWidgets where
    anschlüsse :: KUWidgets -> Set AnschlussEither
    anschlüsse = anschlüsse . ku

    erhalteName :: KUWidgets -> Text
    erhalteName = erhalteName . ku

instance Aeson.ToJSON KUWidgets where
    toJSON :: KUWidgets -> Aeson.Value
    toJSON = Aeson.toJSON . ku

instance KupplungKlasse KUWidgets where
    kuppeln :: (I2CReader r m, MonadIO m) => KUWidgets -> m ()
    kuppeln = liftIO . Gtk.buttonPressed . kuButtonKuppeln

instance KupplungContainer KUWidgets where
    enthalteneKupplungen :: KUWidgets -> Set Kupplung
    enthalteneKupplungen = Set.singleton . ku

-- | 'Kupplung' darstellen und zum Status hinzufügen
kupplungPackNew
    :: forall o m.
    ( BefehlConstraints o
    , KU o ~ KUWidgets
    , SP o ~ SpracheGui
    , MitKUWidgetsBoxen (ReaderFamilie o)
    , MitStatusVar (ReaderFamilie o) o
    , MitFortfahrenWennToggledWegstrecke (ReaderFamilie o) o
    , MitTMVarPlanObjekt (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , MitAktionBearbeiten (ReaderFamilie o)
    , MitTVarMaps (ReaderFamilie o)
    , MonadIO m
    )
    => Kupplung
    -> MStatusAllgemeinT m o KUWidgets
kupplungPackNew kupplung@Kupplung {kupplungsAnschluss} = do
    KUWidgetsBoxen {vBoxKupplungen, vBoxHinzufügenPlanKupplungen} <- erhalteKUWidgetsBoxen
    statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
    (kuTVarSprache, kuTVarEvent) <- liftIO $ do
        kuTVarSprache <- newTVarIO $ Just []
        kuTVarEvent <- newTVarIO EventAusführen
        pure (kuTVarSprache, kuTVarEvent)
    let justTVarSprache = Just kuTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    fortfahrenWennToggledWegstrecke <- erhalteFortfahrenWennToggledWegstrecke
        :: MStatusAllgemeinT m o (FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid)
    hinzufügenWegstreckeWidget <- hinzufügenWidgetWegstreckePackNew
        kupplung
        kuTVarSprache
        fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget
        <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanKupplungen kupplung kuTVarSprache
    -- Widget erstellen
    vBox <- liftIO $ boxPackWidgetNewDefault vBoxKupplungen $ Gtk.vBoxNew False 0
    namePackNew vBox kupplung
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
        $ anschlussNew justTVarSprache Language.kupplung kupplungsAnschluss
    kuFunctionBox <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
    kuButtonKuppeln
        <- buttonKuppelnPackNew kuFunctionBox kupplung kuTVarSprache kuTVarEvent statusVar
    let kuWidgets =
            KUWidgets
            { ku = kupplung
            , kuWidget = vBox
            , kuFunctionBox
            , kuHinzPL = hinzufügenPlanWidget
            , kuHinzWS = hinzufügenWegstreckeWidget
            , kuTVarSprache
            , kuTVarEvent
            , kuButtonKuppeln
            }
    fließendPackNew vBoxAnschlüsse kupplung justTVarSprache
    buttonEntfernenPackNew kuWidgets $ (entfernenKupplung kuWidgets :: IOStatusAllgemein o ())
    buttonBearbeitenPackNew kuWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OKupplung kuWidgets
    pure kuWidgets

-- | Füge 'Gtk.Button' zum kuppeln zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus
-- 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonKuppelnPackNew
    :: forall b k o m.
    ( MitBox b
    , KupplungKlasse k
    , MitSpracheGui (ReaderFamilie o)
    , MitTVarMaps (ReaderFamilie o)
    , ObjektReader o m
    , MonadIO m
    )
    => b
    -> k
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> StatusVar o
    -> m Gtk.Button
buttonKuppelnPackNew box kupplung tvarSprachwechsel tvarEventAusführen statusVar = do
    objektReader <- ask
    boxPackWidgetNewDefault box
        $ buttonNewWithEventLabel (Just tvarSprachwechsel) Language.kuppeln
        $ eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ ausführenStatusVarAktion (Kuppeln kupplung) statusVar
#endif
--
