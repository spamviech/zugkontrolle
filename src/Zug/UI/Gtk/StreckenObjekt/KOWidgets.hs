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

module Zug.UI.Gtk.StreckenObjekt.KOWidgets
  (
#ifdef ZUGKONTROLLEGUI
    KOWidgets()
  , kontaktPackNew
  , KOWidgetsBoxen(..)
  , MitKOWidgetsBoxen(..)
  , KOWidgetsBoxenReader(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (forever)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.GI.Gtk.Threading as Gtk
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as Text
import Data.Void (Void)
import qualified GI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(..), Kontakt(..), KontaktKlasse(..), KontaktContainer(..)
                    , AnschlussEither(AnschlussMit), Value(..), InterruptReader(), I2CReader())
import Zug.Language (Sprache(), MitSprache(leseSprache))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OKontakt), ObjektKlasse(..), ObjektElement(..))
import Zug.UI.Base (StatusAllgemein(), MStatusAllgemeinT, IOStatusAllgemein, entfernenKontakt
                  , ReaderFamilie, MitTVarMaps())
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen), BefehlConstraints)
import Zug.UI.Gtk.Anschluss (anschlussNew)
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledVar)
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNewDefault, boxPackWidgetNew
                                 , Packing(PackGrow), paddingDefault, positionDefault, namePackNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove)
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (SpracheGui, SpracheGuiReader(erhalteSpracheGui), MitSpracheGui()
                            , verwendeSpracheGui, TVarSprachewechselAktionen)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (WegstreckenElement(..), entferneHinzufügenWegstreckeWidgets
      , hinzufügenWidgetWegstreckePackNew, PlanElement(..), entferneHinzufügenPlanWidgets
      , hinzufügenWidgetPlanPackNew, MitFortfahrenWennToggledWegstrecke()
      , WegstreckeCheckButtonVoid, FortfahrenWennToggledWegstreckeReader(..), MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufuegen
       (Kategorie(..), KategorieText(..), CheckButtonWegstreckeHinzufügen, BoxWegstreckeHinzufügen
      , ButtonPlanHinzufügen, BoxPlanHinzufügen)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), buttonEntfernenPackNew
      , buttonBearbeitenPackNew, MitAktionBearbeiten())
import Zug.UI.StatusVar (StatusVar, MitStatusVar())
import Zug.Util (forkIOSilent)

instance Kategorie KOWidgets where
    kategorie :: KategorieText KOWidgets
    kategorie = KategorieText Language.kontakte

data KOWidgets =
    KOWidgets
    { ko :: Kontakt
    , koWidget :: Gtk.Box
    , koFunctionBox :: Gtk.Box
    , koHinzWS :: CheckButtonWegstreckeHinzufügen Void KOWidgets
    , koHinzPL :: ButtonPlanHinzufügen KOWidgets
    , koTVarSprache :: TVarSprachewechselAktionen
    , koTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

instance Aeson.ToJSON KOWidgets where
    toJSON :: KOWidgets -> Aeson.Value
    toJSON = Aeson.toJSON . ko

instance MitWidget KOWidgets where
    erhalteWidget :: (MonadIO m) => KOWidgets -> m Gtk.Widget
    erhalteWidget = erhalteWidget . koWidget

data KOWidgetsBoxen =
    KOWidgetsBoxen
    { vBoxKontakte :: ScrollbaresWidget Gtk.Box
    , vBoxHinzufügenWegstreckeKontakte :: BoxWegstreckeHinzufügen KOWidgets
    , vBoxHinzufügenPlanKontakte :: BoxPlanHinzufügen KOWidgets
    }

class MitKOWidgetsBoxen r where
    koWidgetsBoxen :: r -> KOWidgetsBoxen

instance MitKOWidgetsBoxen KOWidgetsBoxen where
    koWidgetsBoxen :: KOWidgetsBoxen -> KOWidgetsBoxen
    koWidgetsBoxen = id

class (MonadReader r m, MitKOWidgetsBoxen r) => KOWidgetsBoxenReader r m | m -> r where
    erhalteKOWidgetsBoxen :: m KOWidgetsBoxen
    erhalteKOWidgetsBoxen = asks koWidgetsBoxen

instance (MonadReader r m, MitKOWidgetsBoxen r) => KOWidgetsBoxenReader r m

instance ObjektElement KOWidgets where
    type ObjektTyp KOWidgets = Kontakt

    zuObjektTyp :: KOWidgets -> Kontakt
    zuObjektTyp = ko

instance WidgetsTyp KOWidgets where
    type ReaderConstraint KOWidgets = MitKOWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r KOWidgets m) => KOWidgets -> m ()
    entferneWidgets koWidgets@KOWidgets {koTVarSprache} = do
        KOWidgetsBoxen {vBoxKontakte} <- erhalteKOWidgetsBoxen
        mitContainerRemove vBoxKontakte koWidgets
        entferneHinzufügenWegstreckeWidgets koWidgets
        entferneHinzufügenPlanWidgets koWidgets
        liftIO $ atomically $ writeTVar koTVarSprache Nothing

    boxButtonEntfernen :: KOWidgets -> Gtk.Box
    boxButtonEntfernen = koFunctionBox

    tvarSprache :: KOWidgets -> TVarSprachewechselAktionen
    tvarSprache = koTVarSprache

    tvarEvent :: KOWidgets -> TVar EventAusführen
    tvarEvent = koTVarEvent

instance WegstreckenElement KOWidgets where
    checkButtonWegstrecke :: KOWidgets -> CheckButtonWegstreckeHinzufügen Void KOWidgets
    checkButtonWegstrecke = koHinzWS

    boxWegstrecke
        :: (ReaderConstraint KOWidgets r) => Kontakt -> r -> BoxWegstreckeHinzufügen KOWidgets
    boxWegstrecke _kontakt = vBoxHinzufügenWegstreckeKontakte . koWidgetsBoxen

instance PlanElement KOWidgets where
    buttonsPlan :: KOWidgets -> [Maybe (ButtonPlanHinzufügen KOWidgets)]
    buttonsPlan = (: []) . Just . koHinzPL

    boxenPlan :: (ReaderConstraint KOWidgets r) => Kontakt -> r -> [BoxPlanHinzufügen KOWidgets]
    boxenPlan _KOWidgets = (: []) . vBoxHinzufügenPlanKontakte . koWidgetsBoxen

instance StreckenObjekt KOWidgets where
    anschlüsse :: KOWidgets -> Set AnschlussEither
    anschlüsse = anschlüsse . ko

    erhalteName :: KOWidgets -> Text
    erhalteName = erhalteName . ko

instance KontaktKlasse KOWidgets where
    warteAufSignal :: (InterruptReader r m, I2CReader r m, MonadIO m) => KOWidgets -> m ()
    warteAufSignal = warteAufSignal . ko

instance KontaktContainer KOWidgets where
    enthalteneKontakte :: KOWidgets -> Set Kontakt
    enthalteneKontakte = Set.singleton . ko

kontaktPackNew
    :: forall o m.
    ( BefehlConstraints o
    , KO o ~ KOWidgets
    , SP o ~ SpracheGui
    , MitKOWidgetsBoxen (ReaderFamilie o)
    , MitFortfahrenWennToggledWegstrecke (ReaderFamilie o) o
    , MitTMVarPlanObjekt (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , MitStatusVar (ReaderFamilie o) o
    , MitAktionBearbeiten (ReaderFamilie o)
    , MitTVarMaps (ReaderFamilie o)
    , MonadIO m
    )
    => Kontakt
    -> MStatusAllgemeinT m o KOWidgets
kontaktPackNew kontakt@Kontakt {koFließend, kontaktAnschluss} = do
    objektReader <- ask
    KOWidgetsBoxen {vBoxKontakte, vBoxHinzufügenPlanKontakte} <- erhalteKOWidgetsBoxen
    (koTVarSprache, koTVarEvent) <- liftIO $ do
        koTVarSprache <- newTVarIO $ Just []
        koTVarEvent <- newTVarIO EventAusführen
        pure (koTVarSprache, koTVarEvent)
    let justTVarSprache = Just koTVarSprache
    -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
    fortfahrenWennToggledWegstrecke <- erhalteFortfahrenWennToggledWegstrecke
        :: MStatusAllgemeinT
            m
            o
            (FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid)
    hinzufügenWegstreckeWidget
        <- hinzufügenWidgetWegstreckePackNew kontakt koTVarSprache fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget
        <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanKontakte kontakt koTVarSprache
    -- Widget erstellen
    vBox <- liftIO $ boxPackWidgetNewDefault vBoxKontakte $ Gtk.boxNew Gtk.OrientationVertical 0
    namePackNew vBox kontakt
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Nothing
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.boxNew Gtk.OrientationVertical 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache
        $ \sprache -> Gtk.setExpanderLabel expanderAnschlüsse $ Language.anschlüsse sprache
    koFunctionBox <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.boxNew Gtk.OrientationHorizontal 0
    let koWidgets =
            KOWidgets
            { ko = kontakt
            , koWidget = vBox
            , koFunctionBox
            , koHinzPL = hinzufügenPlanWidget
            , koHinzWS = hinzufügenWegstreckeWidget
            , koTVarSprache
            , koTVarEvent
            }
    boxPackWidgetNewDefault vBoxAnschlüsse
        $ anschlussNew justTVarSprache Language.kontakt
        $ AnschlussMit kontaktAnschluss
    (labelSignal, tvarSignal) <- liftIO $ do
        labelSignal <- boxPackWidgetNewDefault koFunctionBox $ Gtk.labelNew (Nothing :: Maybe Text)
        tvarSignal <- newTVarIO Language.aus
        pure (labelSignal, tvarSignal)
    let aktualisiereLabelSignal :: Sprache -> IO ()
        aktualisiereLabelSignal sprache = do
            text <- readTVarIO tvarSignal
            Gtk.setLabelLabel labelSignal $ text sprache
    verwendeSpracheGui justTVarSprache aktualisiereLabelSignal
    liftIO $ forkIOSilent $ forever $ flip runReaderT objektReader $ do
        warteAufSignal kontakt
        spracheGuiAn <- erhalteSpracheGui
        liftIO $ do
            atomically $ writeTVar tvarSignal Language.an
            Gtk.postGUIASync $ leseSprache aktualisiereLabelSignal spracheGuiAn
        warteAufSignal kontakt { koFließend = case koFließend of
            HIGH -> LOW
            LOW -> HIGH }
        spracheGuiAus <- erhalteSpracheGui
        liftIO $ do
            atomically $ writeTVar tvarSignal Language.aus
            Gtk.postGUIASync $ leseSprache aktualisiereLabelSignal spracheGuiAus
    fließendPackNew vBoxAnschlüsse kontakt justTVarSprache
    buttonEntfernenPackNew koWidgets $ (entfernenKontakt koWidgets :: IOStatusAllgemein o ())
    buttonBearbeitenPackNew koWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OKontakt koWidgets
    pure koWidgets
#endif
--
