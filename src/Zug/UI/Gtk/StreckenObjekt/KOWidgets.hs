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
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, writeTVar)
import qualified Control.Lens as Lens
import Control.Monad (forever)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as Text
import Data.Void (Void)
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk

import Zug.Anbindung (StreckenObjekt(..), Kontakt(..), KontaktKlasse(..), KontaktContainer(..)
                    , Anschluss(), Value(..), InterruptReader(), I2CReader())
import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..))
import Zug.Language (Sprache(), MitSprache(leseSprache))
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(OKontakt), ObjektKlasse(..))
import Zug.UI.Base (StatusAllgemein(), MStatusAllgemeinT, IOStatusAllgemein, entfernenKontakt
                  , ReaderFamilie, MitTVarMaps())
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen))
import Zug.UI.Gtk.Anschluss (anschlussNew)
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledVar)
import Zug.UI.Gtk.Hilfsfunktionen (containerAddWidgetNew, boxPackWidgetNewDefault, boxPackWidgetNew
                                 , Packing(PackGrow), paddingDefault, positionDefault, namePackNew)
import Zug.UI.Gtk.Klassen (MitWidget(..), mitContainerRemove, MitBox(..))
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui
       (SpracheGuiReader(erhalteSpracheGui), MitSpracheGui(), verwendeSpracheGui)
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

instance Kategorie KOWidgets where
    kategorie :: KategorieText KOWidgets
    kategorie = KategorieText Language.kontakte

data KOWidgets =
    KOWidgets
    { ko :: Kontakt
    , koWidget :: Gtk.VBox
    , koFunctionBox :: Gtk.HBox
    , koHinzWS :: CheckButtonWegstreckeHinzufügen Void KOWidgets
    , koHinzPL :: ButtonPlanHinzufügen KOWidgets
    , koTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , koTVarEvent :: TVar EventAusführen
    }
    deriving (Eq)

instance Aeson.ToJSON KOWidgets where
    toJSON :: KOWidgets -> Aeson.Value
    toJSON = Aeson.toJSON . ko

instance MitWidget KOWidgets where
    erhalteWidget :: KOWidgets -> Gtk.Widget
    erhalteWidget = erhalteWidget . koWidget

data KOWidgetsBoxen =
    KOWidgetsBoxen
    { vBoxKontakte :: ScrollbaresWidget Gtk.VBox
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

instance WidgetsTyp KOWidgets where
    type ObjektTyp KOWidgets = Kontakt

    type ReaderConstraint KOWidgets = MitKOWidgetsBoxen

    erhalteObjektTyp :: KOWidgets -> Kontakt
    erhalteObjektTyp = ko

    entferneWidgets :: (MonadIO m, WidgetsTypReader r KOWidgets m) => KOWidgets -> m ()
    entferneWidgets koWidgets@KOWidgets {koTVarSprache} = do
        KOWidgetsBoxen {vBoxKontakte} <- erhalteKOWidgetsBoxen
        mitContainerRemove vBoxKontakte koWidgets
        entferneHinzufügenWegstreckeWidgets koWidgets
        entferneHinzufügenPlanWidgets koWidgets
        liftIO $ atomically $ writeTVar koTVarSprache Nothing

    boxButtonEntfernen :: KOWidgets -> Gtk.Box
    boxButtonEntfernen = erhalteBox . koFunctionBox

    tvarSprache :: KOWidgets -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = koTVarSprache

    tvarEvent :: KOWidgets -> TVar EventAusführen
    tvarEvent = koTVarEvent

instance WegstreckenElement KOWidgets where
    getterWegstrecke :: Lens.Getter KOWidgets (CheckButtonWegstreckeHinzufügen Void KOWidgets)
    getterWegstrecke = Lens.to koHinzWS

    boxWegstrecke :: (ReaderConstraint KOWidgets r)
                  => Kontakt
                  -> Lens.Getter r (BoxWegstreckeHinzufügen KOWidgets)
    boxWegstrecke _KOWidgets = Lens.to $ vBoxHinzufügenWegstreckeKontakte . koWidgetsBoxen

instance PlanElement KOWidgets where
    foldPlan :: Lens.Fold KOWidgets (Maybe (ButtonPlanHinzufügen KOWidgets))
    foldPlan = Lens.to $ Just . koHinzPL

    boxenPlan
        :: (ReaderConstraint KOWidgets r) => Kontakt -> Lens.Fold r (BoxPlanHinzufügen KOWidgets)
    boxenPlan _KOWidgets = Lens.to $ vBoxHinzufügenPlanKontakte . koWidgetsBoxen

instance StreckenObjekt KOWidgets where
    anschlüsse :: KOWidgets -> Set Anschluss
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
    ( Eq (BG o 'Pwm 'Märklin)
    , Eq (BG o 'KonstanteSpannung 'Märklin)
    , Eq (BG o 'Pwm 'Lego)
    , Eq (BG o 'KonstanteSpannung 'Lego)
    , Eq (ST o)
    , Eq (WE o 'Märklin)
    , Eq (WE o 'Lego)
    , KO o ~ KOWidgets
    , Eq (KU o)
    , Eq (WS o 'Märklin)
    , Eq (WS o 'Lego)
    , Eq (PL o)
    , MitSprache (SP o)
    , ObjektKlasse o
    , Aeson.ToJSON o
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
        :: MStatusAllgemeinT m o (FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid)
    hinzufügenWegstreckeWidget
        <- hinzufügenWidgetWegstreckePackNew kontakt koTVarSprache fortfahrenWennToggledWegstrecke
    hinzufügenPlanWidget
        <- hinzufügenWidgetPlanPackNew vBoxHinzufügenPlanKontakte kontakt koTVarSprache
    -- Widget erstellen
    vBox <- liftIO $ boxPackWidgetNewDefault vBoxKontakte $ Gtk.vBoxNew False 0
    namePackNew vBox kontakt
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    koFunctionBox <- liftIO $ boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
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
        $ anschlussNew justTVarSprache Language.kontakt kontaktAnschluss
    (labelSignal, tvarSignal) <- liftIO $ do
        labelSignal <- boxPackWidgetNewDefault koFunctionBox $ Gtk.labelNew (Nothing :: Maybe Text)
        tvarSignal <- newTVarIO Language.aus
        pure (labelSignal, tvarSignal)
    let aktualisiereLabelSignal :: Sprache -> IO ()
        aktualisiereLabelSignal sprache = do
            text <- readTVarIO tvarSignal
            Gtk.set labelSignal [Gtk.labelText := text sprache]
    verwendeSpracheGui justTVarSprache aktualisiereLabelSignal
    liftIO $ forkIO $ forever $ flip runReaderT objektReader $ do
        warteAufSignal kontakt
        spracheGuiAn <- erhalteSpracheGui
        liftIO $ do
            atomically $ writeTVar tvarSignal Language.an
            Gtk.postGUIAsync $ leseSprache aktualisiereLabelSignal spracheGuiAn
        warteAufSignal kontakt { koFließend = case koFließend of
            HIGH -> LOW
            LOW -> HIGH }
        spracheGuiAus <- erhalteSpracheGui
        liftIO $ do
            atomically $ writeTVar tvarSignal Language.aus
            Gtk.postGUIAsync $ leseSprache aktualisiereLabelSignal spracheGuiAus
    fließendPackNew vBoxAnschlüsse kontakt justTVarSprache
    buttonEntfernenPackNew koWidgets $ (entfernenKontakt koWidgets :: IOStatusAllgemein o ())
    buttonBearbeitenPackNew koWidgets
    -- Widgets merken
    ausführenBefehl $ Hinzufügen $ ausObjekt $ OKontakt koWidgets
    pure koWidgets
#endif
--
