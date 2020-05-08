{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
#endif

module Zug.UI.Gtk.StreckenObjekt.BGWidgets
  (
#ifdef ZUGKONTROLLEGUI
    BGWidgets()
  , bahngeschwindigkeitPackNew
  , BGWidgetsKlasse(..)
  , hScaleGeschwindigkeitPackNew
  , auswahlFahrstromPackNew
  , buttonUmdrehenPackNew
  , auswahlFahrtrichtungEinstellenPackNew
  , BGWidgetsBoxen(..)
  , MitBGWidgetsBoxen(..)
  , BGWidgetsBoxenReader(..)
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, newTVarIO, writeTVar)
import Control.Lens ((^.))
import qualified Control.Lens as Lens
import Control.Monad (forM_, foldM_)
import Control.Monad.Reader (MonadReader(ask), asks, runReaderT)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import Data.Semigroup (Semigroup((<>)))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word8)
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk as Gtk
import Numeric.Natural (Natural)

import Zug.Anbindung
       (StreckenObjekt(..), Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..)
      , BahngeschwindigkeitContainer(..), StreckenabschnittContainer(enthalteneStreckenabschnitte)
      , AnschlussEither(), I2CReader(), PwmReader())
import Zug.Enums (GeschwindigkeitEither(..), GeschwindigkeitVariante(..), GeschwindigkeitKlasse(..)
                , GeschwindigkeitPhantom(..), ausGeschwindigkeitEither, Zugtyp(..), ZugtypEither(..)
                , ZugtypKlasse(..), ausZugtypEither, Fahrtrichtung(Vorwärts))
import Zug.Language (Sprache(), Anzeige(anzeige))
import qualified Zug.Language as Language
import Zug.Objekt
       (Objekt, ObjektAllgemein(OBahngeschwindigkeit), ObjektElement(..), ObjektKlasse(..))
import Zug.Plan (AktionKlasse(..), AktionBahngeschwindigkeit(..))
import Zug.UI.Base (StatusAllgemein(), MStatusAllgemeinT, IOStatusAllgemein
                  , entfernenBahngeschwindigkeit, ReaderFamilie, getBahngeschwindigkeiten
                  , getStreckenabschnitte, getWegstrecken, ObjektReader(), MitTVarMaps())
import Zug.UI.Befehl (ausführenBefehl, BefehlAllgemein(Hinzufügen), BefehlConstraints)
import Zug.UI.Gtk.Anschluss (anschlussNew, pinNew)
import Zug.UI.Gtk.Auswahl (AuswahlWidget, auswahlComboBoxNew, auswahlRadioButtonNew
                         , boundedEnumAuswahlRadioButtonNew, beiAuswahl, setzeAuswahl)
import Zug.UI.Gtk.Fliessend (fließendPackNew)
import Zug.UI.Gtk.FortfahrenWennToggled (FortfahrenWennToggledVar)
import Zug.UI.Gtk.Hilfsfunktionen
       (containerAddWidgetNew, boxPackWidgetNewDefault, boxPackWidgetNew, Packing(PackGrow)
      , paddingDefault, positionDefault, namePackNew, buttonNewWithEventLabel)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitBox(..), mitContainerRemove)
import Zug.UI.Gtk.ScrollbaresWidget (ScrollbaresWidget, scrollbaresWidgetNew)
import Zug.UI.Gtk.SpracheGui (SpracheGui(), MitSpracheGui(), verwendeSpracheGui)
import Zug.UI.Gtk.StreckenObjekt.ElementKlassen
       (WegstreckenElement(..), entferneHinzufügenWegstreckeWidgets
      , hinzufügenWidgetWegstreckePackNew, PlanElement(..), entferneHinzufügenPlanWidgets
      , hinzufügenWidgetPlanPackNew, MitFortfahrenWennToggledWegstrecke()
      , WegstreckeCheckButtonVoid, FortfahrenWennToggledWegstreckeReader(..), MitTMVarPlanObjekt())
import Zug.UI.Gtk.StreckenObjekt.STWidgets (STWidgets, STWidgetsKlasse(..))
import Zug.UI.Gtk.StreckenObjekt.WidgetHinzufuegen
       (Kategorie(..), KategorieText(..), BoxWegstreckeHinzufügen, CheckButtonWegstreckeHinzufügen
      , BoxPlanHinzufügen, ButtonPlanHinzufügen, widgetHinzufügenGeschwindigkeitVariante
      , widgetHinzufügenGeschwindigkeitEither, widgetHinzufügenZugtypEither)
import Zug.UI.Gtk.StreckenObjekt.WidgetsTyp
       (WidgetsTyp(..), WidgetsTypReader, EventAusführen(EventAusführen), eventAusführen
      , ohneEvent, buttonEntfernenPackNew, buttonBearbeitenPackNew, MitAktionBearbeiten())
import Zug.UI.StatusVar
       (StatusVar, StatusVarReader(erhalteStatusVar), MitStatusVar(), auswertenStatusVarMStatusT)

-- | Widgets zur Steuerung der Geschwindigkeit.
data GeschwindigkeitsWidgets (g :: GeschwindigkeitVariante) where
    ScaleGeschwindigkeit :: { wScaleGeschwindigkeit :: Gtk.HScale } -> GeschwindigkeitsWidgets 'Pwm
    AuswahlFahrstrom :: { wAuswahlFahrstrom :: AuswahlWidget Word8 }
        -> GeschwindigkeitsWidgets 'KonstanteSpannung

deriving instance Eq (GeschwindigkeitsWidgets g)

-- | Widgets zur Steuerung der Fahrtrichtung.
data FahrtrichtungsWidgets (z :: Zugtyp) where
    ButtonUmdrehen :: { wButtonUmdrehen :: Gtk.Button } -> FahrtrichtungsWidgets 'Märklin
    AuswahlFahrtrichtung :: { wAuswahlFahrtrichtung :: AuswahlWidget Fahrtrichtung }
        -> FahrtrichtungsWidgets 'Lego

deriving instance Eq (FahrtrichtungsWidgets z)

instance Kategorie (BGWidgets g z) where
    kategorie :: KategorieText (BGWidgets g z)
    kategorie = KategorieText Language.bahngeschwindigkeiten

instance Kategorie (GeschwindigkeitEither BGWidgets z) where
    kategorie :: KategorieText (GeschwindigkeitEither BGWidgets z)
    kategorie = KategorieText Language.bahngeschwindigkeiten

-- | 'Bahngeschwindigkeit' mit zugehörigen Widgets.
data BGWidgets (g :: GeschwindigkeitVariante) (z :: Zugtyp) =
    BGWidgets
    { bg :: Bahngeschwindigkeit g z
    , bgWidget :: Gtk.VBox
    , bgFunctionBox :: Gtk.HBox
    , bgHinzWS :: CheckButtonWegstreckeHinzufügen Void (BGWidgets g z)
    , bgHinzPL :: ( ButtonPlanHinzufügen (BGWidgets g z)
                  , ButtonPlanHinzufügen (GeschwindigkeitEither BGWidgets z)
                  )
    , bgTVarSprache :: TVar (Maybe [Sprache -> IO ()])
    , bgTVarEvent :: TVar EventAusführen
    , bgGeschwindigkeitsWidgets :: GeschwindigkeitsWidgets g
    , bgFahrtrichtungsWidgets :: FahrtrichtungsWidgets z
    }
    deriving (Eq)

instance MitWidget (BGWidgets g z) where
    erhalteWidget :: BGWidgets g z -> Gtk.Widget
    erhalteWidget = erhalteWidget . bgWidget

data BGWidgetsBoxen =
    BGWidgetsBoxen
    { vBoxBahngeschwindigkeiten :: ScrollbaresWidget Gtk.VBox
    , vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
          :: BoxWegstreckeHinzufügen (GeschwindigkeitEither BGWidgets 'Märklin)
    , vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
          :: BoxWegstreckeHinzufügen (GeschwindigkeitEither BGWidgets 'Lego)
    , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin
          :: BoxPlanHinzufügen (GeschwindigkeitEither BGWidgets 'Märklin)
    , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
          :: BoxPlanHinzufügen (BGWidgets 'Pwm 'Märklin)
    , vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
          :: BoxPlanHinzufügen (BGWidgets 'KonstanteSpannung 'Märklin)
    , vBoxHinzufügenPlanBahngeschwindigkeitenLego
          :: BoxPlanHinzufügen (GeschwindigkeitEither BGWidgets 'Lego)
    , vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm :: BoxPlanHinzufügen (BGWidgets 'Pwm 'Lego)
    , vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
          :: BoxPlanHinzufügen (BGWidgets 'KonstanteSpannung 'Lego)
    }

class MitBGWidgetsBoxen r where
    bgWidgetsBoxen :: r -> BGWidgetsBoxen

instance MitBGWidgetsBoxen BGWidgetsBoxen where
    bgWidgetsBoxen :: BGWidgetsBoxen -> BGWidgetsBoxen
    bgWidgetsBoxen = id

class (MonadReader r m, MitBGWidgetsBoxen r) => BGWidgetsBoxenReader r m | m -> r where
    erhalteBGWidgetsBoxen :: m BGWidgetsBoxen
    erhalteBGWidgetsBoxen = asks bgWidgetsBoxen

instance (MonadReader r m, MitBGWidgetsBoxen r) => BGWidgetsBoxenReader r m

instance (ZugtypKlasse z, GeschwindigkeitKlasse g) => ObjektElement (BGWidgets g z) where
    type ObjektTyp (BGWidgets g z) = Bahngeschwindigkeit g z

    zuObjektTyp :: BGWidgets g z -> Bahngeschwindigkeit g z
    zuObjektTyp = bg

instance ( WegstreckenElement (BGWidgets g z)
         , PlanElement (BGWidgets g z)
         , ZugtypKlasse z
         , GeschwindigkeitKlasse g
         ) => WidgetsTyp (BGWidgets g z) where
    type ReaderConstraint (BGWidgets g z) = MitBGWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (BGWidgets g z) m) => BGWidgets g z -> m ()
    entferneWidgets bgWidgets = do
        BGWidgetsBoxen {vBoxBahngeschwindigkeiten} <- erhalteBGWidgetsBoxen
        mitContainerRemove vBoxBahngeschwindigkeiten bgWidgets
        entferneHinzufügenWegstreckeWidgets bgWidgets
        entferneHinzufügenPlanWidgets bgWidgets
        liftIO $ atomically $ writeTVar (tvarSprache bgWidgets) Nothing

    boxButtonEntfernen :: BGWidgets g z -> Gtk.Box
    boxButtonEntfernen = erhalteBox . bgFunctionBox

    tvarSprache :: BGWidgets g z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache = bgTVarSprache

    tvarEvent :: BGWidgets g z -> TVar EventAusführen
    tvarEvent = bgTVarEvent

instance (GeschwindigkeitKlasse g) => WegstreckenElement (BGWidgets g 'Märklin) where
    getterWegstrecke
        :: Lens.Getter (BGWidgets g 'Märklin) (CheckButtonWegstreckeHinzufügen Void (BGWidgets g 'Märklin))
    getterWegstrecke = Lens.to bgHinzWS

    boxWegstrecke :: (ReaderConstraint (BGWidgets g 'Märklin) r)
                  => Bahngeschwindigkeit g 'Märklin
                  -> Lens.Getter r (BoxWegstreckeHinzufügen (BGWidgets g 'Märklin))
    boxWegstrecke _bahngeschwindigkeit =
        Lens.to
        $ widgetHinzufügenGeschwindigkeitVariante
        . vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
        . bgWidgetsBoxen

instance (GeschwindigkeitKlasse g) => WegstreckenElement (BGWidgets g 'Lego) where
    getterWegstrecke
        :: Lens.Getter (BGWidgets g 'Lego) (CheckButtonWegstreckeHinzufügen Void (BGWidgets g 'Lego))
    getterWegstrecke = Lens.to bgHinzWS

    boxWegstrecke :: (ReaderConstraint (BGWidgets g 'Lego) r)
                  => Bahngeschwindigkeit g 'Lego
                  -> Lens.Getter r (BoxWegstreckeHinzufügen (BGWidgets g 'Lego))
    boxWegstrecke _bahngeschwindigkeit =
        Lens.to
        $ widgetHinzufügenGeschwindigkeitVariante
        . vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
        . bgWidgetsBoxen

instance MitWidget (GeschwindigkeitEither BGWidgets z) where
    erhalteWidget :: GeschwindigkeitEither BGWidgets z -> Gtk.Widget
    erhalteWidget = ausGeschwindigkeitEither erhalteWidget

instance WegstreckenElement (GeschwindigkeitEither BGWidgets 'Märklin) where
    getterWegstrecke
        :: Lens.Getter (GeschwindigkeitEither BGWidgets 'Märklin) (CheckButtonWegstreckeHinzufügen Void (GeschwindigkeitEither BGWidgets 'Märklin))
    getterWegstrecke = Lens.to checkButtonWegstreckeVoid
        where
            checkButtonWegstreckeVoid
                :: GeschwindigkeitEither BGWidgets 'Märklin
                -> CheckButtonWegstreckeHinzufügen Void (GeschwindigkeitEither BGWidgets 'Märklin)
            checkButtonWegstreckeVoid (GeschwindigkeitPwm bgWidgets) =
                widgetHinzufügenGeschwindigkeitEither $ bgWidgets ^. getterWegstrecke
            checkButtonWegstreckeVoid (GeschwindigkeitKonstanteSpannung bgWidgets) =
                widgetHinzufügenGeschwindigkeitEither $ bgWidgets ^. getterWegstrecke

    boxWegstrecke
        :: (ReaderConstraint (GeschwindigkeitEither BGWidgets 'Märklin) r)
        => GeschwindigkeitEither Bahngeschwindigkeit 'Märklin
        -> Lens.Getter r (BoxWegstreckeHinzufügen (GeschwindigkeitEither BGWidgets 'Märklin))
    boxWegstrecke _bgWidgets =
        Lens.to $ vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin . bgWidgetsBoxen

instance WegstreckenElement (GeschwindigkeitEither BGWidgets 'Lego) where
    getterWegstrecke
        :: Lens.Getter (GeschwindigkeitEither BGWidgets 'Lego) (CheckButtonWegstreckeHinzufügen Void (GeschwindigkeitEither BGWidgets 'Lego))
    getterWegstrecke = Lens.to checkButtonWegstreckeVoid
        where
            checkButtonWegstreckeVoid
                :: GeschwindigkeitEither BGWidgets 'Lego
                -> CheckButtonWegstreckeHinzufügen Void (GeschwindigkeitEither BGWidgets 'Lego)
            checkButtonWegstreckeVoid (GeschwindigkeitPwm bgWidgets) =
                widgetHinzufügenGeschwindigkeitEither $ bgWidgets ^. getterWegstrecke
            checkButtonWegstreckeVoid (GeschwindigkeitKonstanteSpannung bgWidgets) =
                widgetHinzufügenGeschwindigkeitEither $ bgWidgets ^. getterWegstrecke

    boxWegstrecke
        :: (ReaderConstraint (GeschwindigkeitEither BGWidgets 'Lego) r)
        => GeschwindigkeitEither Bahngeschwindigkeit 'Lego
        -> Lens.Getter r (BoxWegstreckeHinzufügen (GeschwindigkeitEither BGWidgets 'Lego))
    boxWegstrecke
        _bgWidgets = Lens.to $ vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego . bgWidgetsBoxen

instance (ZugtypKlasse z) => ObjektElement (GeschwindigkeitEither BGWidgets z) where
    type ObjektTyp (GeschwindigkeitEither BGWidgets z) =
        GeschwindigkeitEither Bahngeschwindigkeit z

    zuObjektTyp :: GeschwindigkeitEither BGWidgets z -> GeschwindigkeitEither Bahngeschwindigkeit z
    zuObjektTyp (GeschwindigkeitPwm bg) = GeschwindigkeitPwm $ zuObjektTyp bg
    zuObjektTyp (GeschwindigkeitKonstanteSpannung bg) =
        GeschwindigkeitKonstanteSpannung $ zuObjektTyp bg

    zuObjekt :: GeschwindigkeitEither BGWidgets z -> Objekt
    zuObjekt = OBahngeschwindigkeit . zuZugtypEither . zuObjektTyp

instance ( WegstreckenElement (BGWidgets 'Pwm z)
         , WegstreckenElement (BGWidgets 'KonstanteSpannung z)
         , ZugtypKlasse z
         ) => WidgetsTyp (GeschwindigkeitEither BGWidgets z) where
    type ReaderConstraint (GeschwindigkeitEither BGWidgets z) = MitBGWidgetsBoxen

    entferneWidgets :: (MonadIO m, WidgetsTypReader r (BGWidgets 'Pwm z) m)
                    => GeschwindigkeitEither BGWidgets z
                    -> m ()
    entferneWidgets (GeschwindigkeitPwm bgWidgets) = entferneWidgets bgWidgets
    entferneWidgets (GeschwindigkeitKonstanteSpannung bgWidgets) = entferneWidgets bgWidgets

    boxButtonEntfernen :: GeschwindigkeitEither BGWidgets z -> Gtk.Box
    boxButtonEntfernen (GeschwindigkeitPwm bgWidgets) = boxButtonEntfernen bgWidgets
    boxButtonEntfernen (GeschwindigkeitKonstanteSpannung bgWidgets) = boxButtonEntfernen bgWidgets

    tvarSprache :: GeschwindigkeitEither BGWidgets z -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (GeschwindigkeitPwm bgWidgets) = tvarSprache bgWidgets
    tvarSprache (GeschwindigkeitKonstanteSpannung bgWidgets) = tvarSprache bgWidgets

    tvarEvent :: GeschwindigkeitEither BGWidgets z -> TVar EventAusführen
    tvarEvent (GeschwindigkeitPwm bgWidgets) = tvarEvent bgWidgets
    tvarEvent (GeschwindigkeitKonstanteSpannung bgWidgets) = tvarEvent bgWidgets

instance ObjektElement (ZugtypEither (GeschwindigkeitEither BGWidgets)) where
    type ObjektTyp (ZugtypEither (GeschwindigkeitEither BGWidgets)) =
        ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)

    zuObjektTyp :: ZugtypEither (GeschwindigkeitEither BGWidgets)
                -> ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)
    zuObjektTyp (ZugtypMärklin (GeschwindigkeitPwm bg)) =
        ZugtypMärklin $ GeschwindigkeitPwm $ zuObjektTyp bg
    zuObjektTyp (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bg)) =
        ZugtypMärklin $ GeschwindigkeitKonstanteSpannung $ zuObjektTyp bg
    zuObjektTyp (ZugtypLego (GeschwindigkeitPwm bg)) =
        ZugtypLego $ GeschwindigkeitPwm $ zuObjektTyp bg
    zuObjektTyp (ZugtypLego (GeschwindigkeitKonstanteSpannung bg)) =
        ZugtypLego $ GeschwindigkeitKonstanteSpannung $ zuObjektTyp bg

    zuObjekt :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Objekt
    zuObjekt = OBahngeschwindigkeit . zuObjektTyp

instance WidgetsTyp (ZugtypEither (GeschwindigkeitEither BGWidgets)) where
    type ReaderConstraint (ZugtypEither (GeschwindigkeitEither BGWidgets)) = MitBGWidgetsBoxen

    entferneWidgets
        :: (MonadIO m, WidgetsTypReader r (ZugtypEither (GeschwindigkeitEither BGWidgets)) m)
        => ZugtypEither (GeschwindigkeitEither BGWidgets)
        -> m ()
    entferneWidgets (ZugtypMärklin (GeschwindigkeitPwm bgWidgets)) = entferneWidgets bgWidgets
    entferneWidgets (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        entferneWidgets bgWidgets
    entferneWidgets (ZugtypLego (GeschwindigkeitPwm bgWidgets)) = entferneWidgets bgWidgets
    entferneWidgets (ZugtypLego (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        entferneWidgets bgWidgets

    boxButtonEntfernen :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Gtk.Box
    boxButtonEntfernen (ZugtypMärklin (GeschwindigkeitPwm bgWidgets)) =
        boxButtonEntfernen bgWidgets
    boxButtonEntfernen (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        boxButtonEntfernen bgWidgets
    boxButtonEntfernen (ZugtypLego (GeschwindigkeitPwm bgWidgets)) = boxButtonEntfernen bgWidgets
    boxButtonEntfernen (ZugtypLego (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        boxButtonEntfernen bgWidgets

    tvarSprache :: ZugtypEither (GeschwindigkeitEither BGWidgets)
                -> TVar (Maybe [Sprache -> IO ()])
    tvarSprache (ZugtypMärklin (GeschwindigkeitPwm bgWidgets)) = tvarSprache bgWidgets
    tvarSprache (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bgWidgets)) =
        tvarSprache bgWidgets
    tvarSprache (ZugtypLego (GeschwindigkeitPwm bgWidgets)) = tvarSprache bgWidgets
    tvarSprache (ZugtypLego (GeschwindigkeitKonstanteSpannung bgWidgets)) = tvarSprache bgWidgets

    tvarEvent :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> TVar EventAusführen
    tvarEvent (ZugtypMärklin (GeschwindigkeitPwm bgWidgets)) = tvarEvent bgWidgets
    tvarEvent (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bgWidgets)) = tvarEvent bgWidgets
    tvarEvent (ZugtypLego (GeschwindigkeitPwm bgWidgets)) = tvarEvent bgWidgets
    tvarEvent (ZugtypLego (GeschwindigkeitKonstanteSpannung bgWidgets)) = tvarEvent bgWidgets

instance WegstreckenElement (ZugtypEither (GeschwindigkeitEither BGWidgets)) where
    getterWegstrecke
        :: Lens.Getter (ZugtypEither (GeschwindigkeitEither BGWidgets)) (CheckButtonWegstreckeHinzufügen Void (ZugtypEither (GeschwindigkeitEither BGWidgets)))
    getterWegstrecke = Lens.to erhalteCheckbuttonWegstrecke
        where
            erhalteCheckbuttonWegstrecke
                :: ZugtypEither (GeschwindigkeitEither BGWidgets)
                -> CheckButtonWegstreckeHinzufügen Void (ZugtypEither (GeschwindigkeitEither BGWidgets))
            erhalteCheckbuttonWegstrecke (ZugtypMärklin bg) =
                widgetHinzufügenZugtypEither $ bg ^. getterWegstrecke
            erhalteCheckbuttonWegstrecke (ZugtypLego bg) =
                widgetHinzufügenZugtypEither $ bg ^. getterWegstrecke

    boxWegstrecke
        :: (ReaderConstraint (ZugtypEither (GeschwindigkeitEither BGWidgets)) r)
        => ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)
        -> Lens.Getter r (BoxWegstreckeHinzufügen (ZugtypEither (GeschwindigkeitEither BGWidgets)))
    boxWegstrecke (ZugtypMärklin _bgWidgets) =
        Lens.to
        $ widgetHinzufügenZugtypEither
        . vBoxHinzufügenWegstreckeBahngeschwindigkeitenMärklin
        . bgWidgetsBoxen
    boxWegstrecke (ZugtypLego _bgWidgets) =
        Lens.to
        $ widgetHinzufügenZugtypEither
        . vBoxHinzufügenWegstreckeBahngeschwindigkeitenLego
        . bgWidgetsBoxen

instance (GeschwindigkeitKlasse g) => PlanElement (BGWidgets g 'Märklin) where
    foldPlan
        :: Lens.Fold (BGWidgets g 'Märklin) (Maybe (ButtonPlanHinzufügen (BGWidgets g 'Märklin)))
    foldPlan = Lens.folding $ map Just . erhalteButtonPlanHinzufügen
        where
            erhalteButtonPlanHinzufügen
                :: BGWidgets g 'Märklin -> [ButtonPlanHinzufügen (BGWidgets g 'Märklin)]
            erhalteButtonPlanHinzufügen
                BGWidgets {bgHinzPL = (buttonSpezifisch, buttonAllgemein)} =
                [buttonSpezifisch, widgetHinzufügenGeschwindigkeitVariante buttonAllgemein]

    boxenPlan :: (ReaderConstraint (BGWidgets g 'Märklin) r)
              => Bahngeschwindigkeit g 'Märklin
              -> Lens.Fold r (BoxPlanHinzufügen (BGWidgets g 'Märklin))
    boxenPlan MärklinBahngeschwindigkeitPwm {} =
        Lens.folding
        $ (\BGWidgetsBoxen { vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
                           , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin}
           -> [ vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
              , widgetHinzufügenGeschwindigkeitVariante
                    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin])
        . bgWidgetsBoxen
    boxenPlan MärklinBahngeschwindigkeitKonstanteSpannung {} =
        Lens.folding
        $ (\BGWidgetsBoxen { vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
                           , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin}
           -> [ vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
              , widgetHinzufügenGeschwindigkeitVariante
                    vBoxHinzufügenPlanBahngeschwindigkeitenMärklin])
        . bgWidgetsBoxen

instance (GeschwindigkeitKlasse g) => PlanElement (BGWidgets g 'Lego) where
    foldPlan :: Lens.Fold (BGWidgets g 'Lego) (Maybe (ButtonPlanHinzufügen (BGWidgets g 'Lego)))
    foldPlan = Lens.folding $ map Just . erhalteButtonPlanHinzufügen
        where
            erhalteButtonPlanHinzufügen
                :: BGWidgets g 'Lego -> [ButtonPlanHinzufügen (BGWidgets g 'Lego)]
            erhalteButtonPlanHinzufügen
                BGWidgets {bgHinzPL = (buttonSpezifisch, buttonAllgemein)} =
                [buttonSpezifisch, widgetHinzufügenGeschwindigkeitVariante buttonAllgemein]

    boxenPlan :: (ReaderConstraint (BGWidgets g 'Lego) r)
              => Bahngeschwindigkeit g 'Lego
              -> Lens.Fold r (BoxPlanHinzufügen (BGWidgets g 'Lego))
    boxenPlan LegoBahngeschwindigkeit {} =
        Lens.folding
        $ (\BGWidgetsBoxen { vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
                           , vBoxHinzufügenPlanBahngeschwindigkeitenLego}
           -> [ vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
              , widgetHinzufügenGeschwindigkeitVariante
                    vBoxHinzufügenPlanBahngeschwindigkeitenLego])
        . bgWidgetsBoxen

instance PlanElement (ZugtypEither (GeschwindigkeitEither BGWidgets)) where
    foldPlan :: Lens.Fold (ZugtypEither (GeschwindigkeitEither BGWidgets)) (Maybe (ButtonPlanHinzufügen (ZugtypEither (GeschwindigkeitEither BGWidgets))))
    foldPlan = Lens.folding $ \bgWidgets -> Just <$> ausZugtypEither buttonList bgWidgets
        where
            buttonList :: (GeschwindigkeitEither BGWidgets) z
                       -> [ButtonPlanHinzufügen (ZugtypEither (GeschwindigkeitEither BGWidgets))]
            buttonList
                (GeschwindigkeitPwm BGWidgets {bgHinzPL = (buttonSpezifisch, buttonAllgemein)}) =
                widgetHinzufügenZugtypEither
                <$> [widgetHinzufügenGeschwindigkeitEither buttonSpezifisch, buttonAllgemein]
            buttonList
                (GeschwindigkeitKonstanteSpannung
                     BGWidgets {bgHinzPL = (buttonSpezifisch, buttonAllgemein)}) =
                widgetHinzufügenZugtypEither
                <$> [widgetHinzufügenGeschwindigkeitEither buttonSpezifisch, buttonAllgemein]

    boxenPlan :: (ReaderConstraint (ZugtypEither (GeschwindigkeitEither BGWidgets)) r)
              => ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit)
              -> Lens.Fold r (BoxPlanHinzufügen (ZugtypEither (GeschwindigkeitEither BGWidgets)))
    boxenPlan (ZugtypMärklin (GeschwindigkeitPwm _bahngeschwindigkeit)) =
        Lens.folding
        $ (\BGWidgetsBoxen
           { vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
           , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin} -> widgetHinzufügenZugtypEither
           <$> [ widgetHinzufügenGeschwindigkeitEither
                     vBoxHinzufügenPlanBahngeschwindigkeitenMärklinPwm
               , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin])
        . bgWidgetsBoxen
    boxenPlan (ZugtypMärklin (GeschwindigkeitKonstanteSpannung _bahngeschwindigkeit)) =
        Lens.folding
        $ (\BGWidgetsBoxen
           { vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
           , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin} -> widgetHinzufügenZugtypEither
           <$> [ widgetHinzufügenGeschwindigkeitEither
                     vBoxHinzufügenPlanBahngeschwindigkeitenMärklinKonstanteSpannung
               , vBoxHinzufügenPlanBahngeschwindigkeitenMärklin])
        . bgWidgetsBoxen
    boxenPlan (ZugtypLego (GeschwindigkeitPwm _bahngeschwindigkeit)) =
        Lens.folding
        $ (\BGWidgetsBoxen
           { vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
           , vBoxHinzufügenPlanBahngeschwindigkeitenLego} -> widgetHinzufügenZugtypEither
           <$> [ widgetHinzufügenGeschwindigkeitEither
                     vBoxHinzufügenPlanBahngeschwindigkeitenLegoPwm
               , vBoxHinzufügenPlanBahngeschwindigkeitenLego])
        . bgWidgetsBoxen
    boxenPlan (ZugtypLego (GeschwindigkeitKonstanteSpannung _bahngeschwindigkeit)) =
        Lens.folding
        $ (\BGWidgetsBoxen
           { vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
           , vBoxHinzufügenPlanBahngeschwindigkeitenLego} -> widgetHinzufügenZugtypEither
           <$> [ widgetHinzufügenGeschwindigkeitEither
                     vBoxHinzufügenPlanBahngeschwindigkeitenLegoKonstanteSpannung
               , vBoxHinzufügenPlanBahngeschwindigkeitenLego])
        . bgWidgetsBoxen

instance StreckenObjekt (BGWidgets g z) where
    anschlüsse :: BGWidgets g z -> Set AnschlussEither
    anschlüsse = anschlüsse . bg

    erhalteName :: BGWidgets g z -> Text
    erhalteName = erhalteName . bg

instance Aeson.ToJSON (BGWidgets g z) where
    toJSON :: BGWidgets g z -> Aeson.Value
    toJSON = Aeson.toJSON . bg

instance BahngeschwindigkeitKlasse BGWidgets where
    geschwindigkeit
        :: (I2CReader r m, PwmReader r m, MonadIO m) => BGWidgets 'Pwm z -> Word8 -> m ()
    geschwindigkeit
        BGWidgets {bgGeschwindigkeitsWidgets = ScaleGeschwindigkeit {wScaleGeschwindigkeit}}
        wert = liftIO $ Gtk.set wScaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]

    fahrstrom :: (I2CReader r m, MonadIO m) => BGWidgets 'KonstanteSpannung z -> Word8 -> m ()
    fahrstrom BGWidgets {bgGeschwindigkeitsWidgets = AuswahlFahrstrom {wAuswahlFahrstrom}} =
        liftIO . setzeAuswahl wAuswahlFahrstrom

    umdrehen :: (I2CReader r m, PwmReader r m, MonadIO m) => BGWidgets b 'Märklin -> m ()
    umdrehen BGWidgets {bgFahrtrichtungsWidgets = ButtonUmdrehen {wButtonUmdrehen}} =
        liftIO $ Gtk.buttonPressed wButtonUmdrehen

    fahrtrichtungEinstellen
        :: (I2CReader r m, PwmReader r m, MonadIO m) => BGWidgets b 'Lego -> Fahrtrichtung -> m ()
    fahrtrichtungEinstellen
        BGWidgets {bgFahrtrichtungsWidgets = AuswahlFahrtrichtung {wAuswahlFahrtrichtung}} =
        liftIO . setzeAuswahl wAuswahlFahrtrichtung

instance (ZugtypKlasse z, GeschwindigkeitKlasse g)
    => BahngeschwindigkeitContainer (BGWidgets g z) where
    enthalteneBahngeschwindigkeiten
        :: BGWidgets g z -> Set (ZugtypEither (GeschwindigkeitEither Bahngeschwindigkeit))
    enthalteneBahngeschwindigkeiten = Set.singleton . zuZugtypEither . zuGeschwindigkeitEither . bg

-- | 'Bahngeschwindigkeit' darstellen und zum Status hinzufügen
bahngeschwindigkeitPackNew
    :: forall o g z m.
    ( GeschwindigkeitKlasse g
    , WegstreckenElement (BGWidgets g z)
    , PlanElement (BGWidgets g z)
    , BefehlConstraints o
    , BG o ~ BGWidgets
    , ST o ~ STWidgets
    , BGWidgetsKlasse (GeschwindigkeitPhantom (WS o))
    , BahngeschwindigkeitContainer (WS o 'Märklin)
    , BahngeschwindigkeitContainer (WS o 'Lego)
    , STWidgetsKlasse (WS o 'Märklin)
    , STWidgetsKlasse (WS o 'Lego)
    , StreckenabschnittContainer (WS o 'Märklin)
    , StreckenabschnittContainer (WS o 'Lego)
    , SP o ~ SpracheGui
    , MitBGWidgetsBoxen (ReaderFamilie o)
    , MitStatusVar (ReaderFamilie o) o
    , MitTVarMaps (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , MitFortfahrenWennToggledWegstrecke (ReaderFamilie o) o
    , MitTMVarPlanObjekt (ReaderFamilie o)
    , MitAktionBearbeiten (ReaderFamilie o)
    , MonadIO m
    , ZugtypKlasse z
    )
    => Bahngeschwindigkeit g z
    -> MStatusAllgemeinT m o (BGWidgets g z)
bahngeschwindigkeitPackNew bahngeschwindigkeit = do
    BGWidgetsBoxen {vBoxBahngeschwindigkeiten} <- erhalteBGWidgetsBoxen
    (tvarSprache, tvarEvent) <- liftIO $ do
        tvarSprache <- newTVarIO $ Just []
        tvarEvent <- newTVarIO EventAusführen
        pure (tvarSprache, tvarEvent)
    let justTVarSprache = Just tvarSprache
    -- Widget erstellen
    vBox <- liftIO $ boxPackWidgetNewDefault vBoxBahngeschwindigkeiten $ Gtk.vBoxNew False 0
    namePackNew vBox bahngeschwindigkeit
    (expanderAnschlüsse, vBoxAnschlüsse) <- liftIO $ do
        expanderAnschlüsse <- boxPackWidgetNew vBox PackGrow paddingDefault positionDefault
            $ Gtk.expanderNew Text.empty
        vBoxAnschlüsse <- containerAddWidgetNew expanderAnschlüsse
            $ scrollbaresWidgetNew
            $ Gtk.vBoxNew False 0
        pure (expanderAnschlüsse, vBoxAnschlüsse)
    verwendeSpracheGui justTVarSprache $ \sprache
        -> Gtk.set expanderAnschlüsse [Gtk.expanderLabel := Language.anschlüsse sprache]
    bgWidgets <- geschwindigkeitsWidgetsPackNew
        vBox
        bahngeschwindigkeit
        vBoxAnschlüsse
        tvarSprache
        tvarEvent
    fließendPackNew vBoxAnschlüsse bahngeschwindigkeit justTVarSprache
    buttonEntfernenPackNew
        bgWidgets
        (entfernenBahngeschwindigkeit $ zuZugtypEither $ zuGeschwindigkeitEither bgWidgets
         :: IOStatusAllgemein o ())
    buttonBearbeitenPackNew bgWidgets
    -- Widgets merken
    ausführenBefehl
        $ Hinzufügen
        $ ausObjekt
        $ OBahngeschwindigkeit
        $ zuZugtypEither
        $ zuGeschwindigkeitEither bgWidgets
    pure bgWidgets
    where
        hinzufügenWidgetsPackNew
            :: Bahngeschwindigkeit g z
            -> TVar (Maybe [Sprache -> IO ()])
            -> MStatusAllgemeinT m o ( CheckButtonWegstreckeHinzufügen Void (BGWidgets g z)
                                     , ButtonPlanHinzufügen (BGWidgets g z)
                                     , ButtonPlanHinzufügen (GeschwindigkeitEither BGWidgets z)
                                     )
        hinzufügenWidgetsPackNew bahngeschwindigkeit tvarSprache = do
            objektReader <- ask
            fortfahrenWennToggledWegstrecke <- erhalteFortfahrenWennToggledWegstrecke
                :: MStatusAllgemeinT m o (FortfahrenWennToggledVar (StatusAllgemein o) (StatusVar o) WegstreckeCheckButtonVoid)
            hinzufügenWidgetWegstrecke <- hinzufügenWidgetWegstreckePackNew
                bahngeschwindigkeit
                tvarSprache
                fortfahrenWennToggledWegstrecke
            hinzufügenWidgetPlanSpezifisch <- hinzufügenWidgetPlanPackNew
                (fromJust $ Lens.firstOf (boxenPlan bahngeschwindigkeit) objektReader)
                bahngeschwindigkeit
                tvarSprache
            hinzufügenWidgetPlanAllgemein <- widgetHinzufügenGeschwindigkeitEither
                <$> hinzufügenWidgetPlanPackNew
                    (fromJust $ Lens.lastOf (boxenPlan bahngeschwindigkeit) objektReader)
                    bahngeschwindigkeit
                    tvarSprache
            pure
                ( hinzufügenWidgetWegstrecke
                , hinzufügenWidgetPlanSpezifisch
                , hinzufügenWidgetPlanAllgemein
                )

        geschwindigkeitsWidgetsPackNew
            :: Gtk.VBox
            -> Bahngeschwindigkeit g z
            -> ScrollbaresWidget Gtk.VBox
            -> TVar (Maybe [Sprache -> IO ()])
            -> TVar EventAusführen
            -> MStatusAllgemeinT m o (BGWidgets g z)
        geschwindigkeitsWidgetsPackNew
            box
            bahngeschwindigkeit@MärklinBahngeschwindigkeitPwm {bgmpGeschwindigkeitsPin}
            vBoxAnschlüsse
            bgTVarSprache
            bgTVarEvent = do
            statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
            boxPackWidgetNewDefault vBoxAnschlüsse
                $ pinNew (Just bgTVarSprache) Language.geschwindigkeit bgmpGeschwindigkeitsPin
            bgFunctionBox <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
            wScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew
                bgFunctionBox
                bahngeschwindigkeit
                bgTVarEvent
                statusVar
            wButtonUmdrehen <- buttonUmdrehenPackNew
                bgFunctionBox
                bahngeschwindigkeit
                bgTVarSprache
                bgTVarEvent
                statusVar
            -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
            (bgHinzWS, hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                <- hinzufügenWidgetsPackNew bahngeschwindigkeit bgTVarSprache
            pure
                BGWidgets
                { bg = bahngeschwindigkeit
                , bgWidget = box
                , bgFunctionBox
                , bgHinzWS
                , bgHinzPL = (hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                , bgTVarSprache
                , bgTVarEvent
                , bgGeschwindigkeitsWidgets = ScaleGeschwindigkeit { wScaleGeschwindigkeit }
                , bgFahrtrichtungsWidgets = ButtonUmdrehen { wButtonUmdrehen }
                }
        geschwindigkeitsWidgetsPackNew
            box
            bahngeschwindigkeit@MärklinBahngeschwindigkeitKonstanteSpannung
            {bgmkFahrstromAnschlüsse, bgmkUmdrehenAnschluss}
            vBoxAnschlüsse
            bgTVarSprache
            bgTVarEvent = do
            statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
            let justTVarSprache = Just bgTVarSprache
            let erstelleFahrstromAnschlussWidget
                    :: Natural -> AnschlussEither -> MStatusAllgemeinT m o Natural
                erstelleFahrstromAnschlussWidget i anschluss = do
                    boxPackWidgetNewDefault vBoxAnschlüsse
                        $ anschlussNew justTVarSprache (Language.fahrstrom <> anzeige i) anschluss
                    pure $ succ i
            foldM_ erstelleFahrstromAnschlussWidget 1 bgmkFahrstromAnschlüsse
            bgFunctionBox <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
            wAuswahlFahrstrom <- auswahlFahrstromPackNew
                bgFunctionBox
                bahngeschwindigkeit
                (fromIntegral $ length bgmkFahrstromAnschlüsse)
                bgTVarSprache
                bgTVarEvent
                statusVar
            boxPackWidgetNewDefault vBoxAnschlüsse
                $ anschlussNew justTVarSprache Language.umdrehen bgmkUmdrehenAnschluss
            wButtonUmdrehen <- buttonUmdrehenPackNew
                bgFunctionBox
                bahngeschwindigkeit
                bgTVarSprache
                bgTVarEvent
                statusVar
            -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
            (bgHinzWS, hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                <- hinzufügenWidgetsPackNew bahngeschwindigkeit bgTVarSprache
            pure
                BGWidgets
                { bg = bahngeschwindigkeit
                , bgWidget = box
                , bgFunctionBox
                , bgHinzWS
                , bgHinzPL = (hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                , bgTVarSprache
                , bgTVarEvent
                , bgGeschwindigkeitsWidgets = AuswahlFahrstrom { wAuswahlFahrstrom }
                , bgFahrtrichtungsWidgets = ButtonUmdrehen { wButtonUmdrehen }
                }
        geschwindigkeitsWidgetsPackNew
            box
            bahngeschwindigkeit@LegoBahngeschwindigkeit
            {bglGeschwindigkeitsPin, bglFahrtrichtungsAnschluss}
            vBoxAnschlüsse
            bgTVarSprache
            bgTVarEvent = do
            statusVar <- erhalteStatusVar :: MStatusAllgemeinT m o (StatusVar o)
            let justTVarSprache = Just bgTVarSprache
            boxPackWidgetNewDefault vBoxAnschlüsse
                $ pinNew justTVarSprache Language.geschwindigkeit bglGeschwindigkeitsPin
            bgFunctionBox <- liftIO $ boxPackWidgetNewDefault box $ Gtk.hBoxNew False 0
            wScaleGeschwindigkeit <- hScaleGeschwindigkeitPackNew
                bgFunctionBox
                bahngeschwindigkeit
                bgTVarEvent
                statusVar
            boxPackWidgetNewDefault vBoxAnschlüsse
                $ anschlussNew justTVarSprache Language.fahrtrichtung bglFahrtrichtungsAnschluss
            wAuswahlFahrtrichtung <- auswahlFahrtrichtungEinstellenPackNew
                bgFunctionBox
                bahngeschwindigkeit
                bgTVarSprache
                bgTVarEvent
                statusVar
            -- Zum Hinzufügen-Dialog von Wegstrecke/Plan hinzufügen
            (bgHinzWS, hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                <- hinzufügenWidgetsPackNew bahngeschwindigkeit bgTVarSprache
            pure
                BGWidgets
                { bg = bahngeschwindigkeit
                , bgWidget = box
                , bgFunctionBox
                , bgHinzWS
                , bgHinzPL = (hinzufügenWidgetPlanSpezifisch, hinzufügenWidgetPlanAllgemein)
                , bgTVarSprache
                , bgTVarEvent
                , bgGeschwindigkeitsWidgets = ScaleGeschwindigkeit { wScaleGeschwindigkeit }
                , bgFahrtrichtungsWidgets = AuswahlFahrtrichtung { wAuswahlFahrtrichtung }
                }

-- | Hilfsklasse um Widgets zu synchronisieren.
class ( WidgetsTyp (bg 'Pwm 'Märklin)
      , WidgetsTyp (bg 'KonstanteSpannung 'Märklin)
      , WidgetsTyp (bg 'Pwm 'Lego)
      , WidgetsTyp (bg 'KonstanteSpannung 'Lego)
      , BahngeschwindigkeitKlasse bg
      ) => BGWidgetsKlasse bg where
    scaleGeschwindigkeit :: bg 'Pwm z -> Maybe Gtk.HScale
    auswahlFahrstrom :: bg 'KonstanteSpannung z -> Maybe (AuswahlWidget Word8)
    auswahlFahrtrichtung :: bg g 'Lego -> Maybe (AuswahlWidget Fahrtrichtung)

-- | Füge 'Scale' zum einstellen der Geschwindigkeit zur Box hinzu
hScaleGeschwindigkeitPackNew
    :: forall b bg o m z.
    ( MitBox b
    , BahngeschwindigkeitKlasse bg
    , BahngeschwindigkeitContainer (bg 'Pwm z)
    , BG o ~ BGWidgets
    , BGWidgetsKlasse (GeschwindigkeitPhantom (WS o))
    , BahngeschwindigkeitContainer (WS o 'Märklin)
    , BahngeschwindigkeitContainer (WS o 'Lego)
    , STWidgetsKlasse (WS o 'Märklin)
    , STWidgetsKlasse (WS o 'Lego)
    , MitTVarMaps (ReaderFamilie o)
    , ObjektReader o m
    , MonadIO m
    , ZugtypKlasse z
    )
    => b
    -> bg 'Pwm z
    -> TVar EventAusführen
    -> StatusVar o
    -> m Gtk.HScale
hScaleGeschwindigkeitPackNew box bahngeschwindigkeit tvarEventAusführen statusVar = do
    objektReader <- ask
    liftIO $ do
        scale <- boxPackWidgetNew box PackGrow paddingDefault positionDefault
            $ Gtk.hScaleNewWithRange 0 (fromIntegral (maxBound :: Word8)) 1
        Gtk.widgetSetSizeRequest scale 100 (-1)
        Gtk.on scale Gtk.valueChanged $ eventAusführen tvarEventAusführen $ do
            wert <- floor <$> Gtk.get scale Gtk.rangeValue
            flip runReaderT objektReader $ flip auswertenStatusVarMStatusT statusVar $ do
                ausführenAktion $ Geschwindigkeit bahngeschwindigkeit wert
                -- Widgets synchronisieren
                bahngeschwindigkeiten <- getBahngeschwindigkeiten
                liftIO $ forM_ bahngeschwindigkeiten $ flip bgWidgetsSynchronisieren wert
                wegstrecken <- getWegstrecken
                liftIO $ forM_ wegstrecken $ flip wsWidgetsSynchronisieren wert
        pure scale
    where
        bgWidgetsSynchronisieren
            :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Word8 -> IO ()
        bgWidgetsSynchronisieren
            (ZugtypMärklin
                 (GeschwindigkeitPwm
                      BGWidgets
                      { bg
                      , bgTVarEvent
                      , bgGeschwindigkeitsWidgets = ScaleGeschwindigkeit {wScaleGeschwindigkeit}}))
            wert
            | elem (ZugtypMärklin $ GeschwindigkeitPwm bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent
                $ Gtk.set wScaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitPwm
                      BGWidgets
                      { bg
                      , bgTVarEvent
                      , bgGeschwindigkeitsWidgets = ScaleGeschwindigkeit {wScaleGeschwindigkeit}}))
            wert
            | elem (ZugtypLego $ GeschwindigkeitPwm bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent
                $ Gtk.set wScaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
        bgWidgetsSynchronisieren _bgWidgets _wert = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither (WS o) -> Word8 -> IO ()
        wsWidgetsSynchronisieren
            (ZugtypMärklin
                 ws@(scaleGeschwindigkeit . GeschwindigkeitPhantom -> Just scaleGeschwindigkeit))
            wert
            | Set.isSubsetOf (enthalteneBahngeschwindigkeiten ws)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent (tvarEvent ws)
                $ Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
        wsWidgetsSynchronisieren
            (ZugtypLego
                 ws@(scaleGeschwindigkeit . GeschwindigkeitPhantom -> Just scaleGeschwindigkeit))
            wert
            | Set.isSubsetOf (enthalteneBahngeschwindigkeiten ws)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent (tvarEvent ws)
                $ Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := fromIntegral wert]
        wsWidgetsSynchronisieren _wsWidget _wert = pure ()

-- | Füge 'AuswahlWidget' zum einstellen des Fahrstroms zur Box hinzu
auswahlFahrstromPackNew
    :: forall b bg o m z.
    ( MitBox b
    , Show (bg 'KonstanteSpannung z)
    , BahngeschwindigkeitKlasse bg
    , BahngeschwindigkeitContainer (bg 'KonstanteSpannung z)
    , BG o ~ BGWidgets
    , WidgetsTyp (WS o 'Märklin)
    , BahngeschwindigkeitContainer (WS o 'Märklin)
    , WidgetsTyp (WS o 'Lego)
    , BGWidgetsKlasse (GeschwindigkeitPhantom (WS o))
    , BahngeschwindigkeitContainer (WS o 'Lego)
    , MitSpracheGui (ReaderFamilie o)
    , MitTVarMaps (ReaderFamilie o)
    , ObjektReader o m
    , MonadIO m
    , ZugtypKlasse z
    )
    => b
    -> bg 'KonstanteSpannung z
    -> Word8
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> StatusVar o
    -> m (AuswahlWidget Word8)
auswahlFahrstromPackNew
    box
    bahngeschwindigkeit
    maxWert
    tvarSprachwechsel
    tvarEventAusführen
    statusVar = do
    objektReader <- ask
    auswahlWidget <- boxPackWidgetNewDefault box
        $ (if maxWert < 5
               then auswahlRadioButtonNew
               else auswahlComboBoxNew)
            (NonEmpty.fromList $ [maxWert, pred maxWert .. 0])
            (Just tvarSprachwechsel)
            Language.fahrstrom
    setzeAuswahl auswahlWidget 0
    beiAuswahl auswahlWidget $ \wert -> eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ flip auswertenStatusVarMStatusT statusVar
        $ do
            ausführenAktion $ Fahrstrom bahngeschwindigkeit wert
            -- Widgets synchronisieren
            bahngeschwindigkeiten <- getBahngeschwindigkeiten
            liftIO $ forM_ bahngeschwindigkeiten $ flip bgWidgetsSynchronisieren wert
            wegstrecken <- getWegstrecken
            liftIO $ forM_ wegstrecken $ flip wsWidgetsSynchronisieren wert
    pure auswahlWidget
    where
        bgWidgetsSynchronisieren
            :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Word8 -> IO ()
        bgWidgetsSynchronisieren
            (ZugtypMärklin
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgets { bg
                                , bgTVarEvent
                                , bgGeschwindigkeitsWidgets = AuswahlFahrstrom {wAuswahlFahrstrom}}))
            wert
            | elem (ZugtypMärklin $ GeschwindigkeitKonstanteSpannung bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent $ setzeAuswahl wAuswahlFahrstrom wert
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgets { bg
                                , bgTVarEvent
                                , bgGeschwindigkeitsWidgets = AuswahlFahrstrom {wAuswahlFahrstrom}}))
            wert
            | elem (ZugtypLego $ GeschwindigkeitKonstanteSpannung bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent $ setzeAuswahl wAuswahlFahrstrom wert
        bgWidgetsSynchronisieren _bgWidgets _wert = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither (WS o) -> Word8 -> IO ()
        wsWidgetsSynchronisieren
            (ZugtypMärklin ws@(auswahlFahrstrom . GeschwindigkeitPhantom -> Just auswahlFahrstrom))
            wert
            | Set.isSubsetOf (enthalteneBahngeschwindigkeiten ws)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent (tvarEvent ws) $ setzeAuswahl auswahlFahrstrom wert
            | otherwise =
                putStrLn
                $ show bahngeschwindigkeit
                ++ "->"
                ++ show (enthalteneBahngeschwindigkeiten bahngeschwindigkeit)
        wsWidgetsSynchronisieren
            (ZugtypLego ws@(auswahlFahrstrom . GeschwindigkeitPhantom -> Just auswahlFahrstrom))
            wert
            | Set.isSubsetOf (enthalteneBahngeschwindigkeiten ws)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent (tvarEvent ws) $ setzeAuswahl auswahlFahrstrom wert
            | otherwise =
                putStrLn
                $ show bahngeschwindigkeit
                ++ "->"
                ++ show (enthalteneBahngeschwindigkeiten bahngeschwindigkeit)
        wsWidgetsSynchronisieren _wsWidget _wert = pure ()

-- | Füge 'Gtk.Button' zum 'umdrehen' zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
buttonUmdrehenPackNew
    :: forall b bg o g m.
    ( MitBox b
    , BahngeschwindigkeitKlasse bg
    , BahngeschwindigkeitContainer (bg g 'Märklin)
    , BahngeschwindigkeitContainer (bg g 'Lego)
    , StreckenabschnittContainer (bg g 'Märklin)
    , StreckenabschnittContainer (bg g 'Lego)
    , BG o ~ BGWidgets
    , ST o ~ STWidgets
    , BahngeschwindigkeitKlasse (GeschwindigkeitPhantom (WS o))
    , BahngeschwindigkeitContainer (WS o 'Märklin)
    , BahngeschwindigkeitContainer (WS o 'Lego)
    , BGWidgetsKlasse (GeschwindigkeitPhantom (WS o))
    , STWidgetsKlasse (WS o 'Märklin)
    , StreckenabschnittContainer (WS o 'Märklin)
    , STWidgetsKlasse (WS o 'Lego)
    , StreckenabschnittContainer (WS o 'Lego)
    , MitTVarMaps (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , ObjektReader o m
    , MonadIO m
    , GeschwindigkeitKlasse g
    )
    => b
    -> bg g 'Märklin
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> StatusVar o
    -> m Gtk.Button
buttonUmdrehenPackNew box bahngeschwindigkeit tvarSprachwechsel tvarEventAusführen statusVar = do
    objektReader <- ask
    boxPackWidgetNewDefault box
        $ buttonNewWithEventLabel (Just tvarSprachwechsel) Language.umdrehen
        $ eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ flip auswertenStatusVarMStatusT statusVar
        $ do
            ausführenAktion $ Umdrehen bahngeschwindigkeit
            -- Widgets synchronisieren
            bahngeschwindigkeiten <- getBahngeschwindigkeiten
            liftIO $ forM_ bahngeschwindigkeiten $ bgWidgetsSynchronisieren
            streckenabschnitte <- getStreckenabschnitte
            liftIO $ forM_ streckenabschnitte $ stWidgetsSynchronisieren
            wegstrecken <- getWegstrecken
            liftIO $ forM_ wegstrecken $ wsWidgetsSynchronisieren
    where
        bgWidgetsSynchronisieren :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> IO ()
        bgWidgetsSynchronisieren
            (ZugtypMärklin
                 (GeschwindigkeitPwm
                      BGWidgets
                      { bg
                      , bgTVarEvent
                      , bgGeschwindigkeitsWidgets = ScaleGeschwindigkeit {wScaleGeschwindigkeit}}))
            | elem (ZugtypMärklin $ GeschwindigkeitPwm bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent $ Gtk.set wScaleGeschwindigkeit [Gtk.rangeValue := 0]
        bgWidgetsSynchronisieren
            (ZugtypMärklin
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgets { bg
                                , bgTVarEvent
                                , bgGeschwindigkeitsWidgets = AuswahlFahrstrom {wAuswahlFahrstrom}}))
            | elem (ZugtypMärklin $ GeschwindigkeitKonstanteSpannung bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent $ setzeAuswahl wAuswahlFahrstrom 0
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitPwm
                      BGWidgets
                      { bg
                      , bgTVarEvent
                      , bgGeschwindigkeitsWidgets = ScaleGeschwindigkeit {wScaleGeschwindigkeit}}))
            | elem (ZugtypLego $ GeschwindigkeitPwm bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent $ Gtk.set wScaleGeschwindigkeit [Gtk.rangeValue := 0]
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgets { bg
                                , bgTVarEvent
                                , bgGeschwindigkeitsWidgets = AuswahlFahrstrom {wAuswahlFahrstrom}}))
            | elem (ZugtypLego $ GeschwindigkeitKonstanteSpannung bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent $ setzeAuswahl wAuswahlFahrstrom 0
        bgWidgetsSynchronisieren _bgWidgets = pure ()

        stWidgetsSynchronisieren :: STWidgets -> IO ()
        stWidgetsSynchronisieren st@(toggleButtonStrom -> Just toggleButtonStrom)
            | Set.isSubsetOf (enthalteneStreckenabschnitte st)
                $ enthalteneStreckenabschnitte bahngeschwindigkeit =
                ohneEvent (tvarEvent st)
                $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := True]
        stWidgetsSynchronisieren _stWidgets = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither (WS o) -> IO ()
        wsWidgetsSynchronisieren (ZugtypMärklin ws) = wsWidgetsSynchronisierenAux ws
        wsWidgetsSynchronisieren (ZugtypLego ws) = wsWidgetsSynchronisierenAux ws

        wsWidgetsSynchronisierenAux
            :: ( BahngeschwindigkeitContainer (WS o z)
               , StreckenabschnittContainer (WS o z)
               , STWidgetsKlasse (WS o z)
               , WidgetsTyp (WS o z)
               )
            => WS o z
            -> IO ()
        wsWidgetsSynchronisierenAux ws = do
            let wsBahngeschwindigkeiten = enthalteneBahngeschwindigkeiten ws
                wsStreckenabschnitte = enthalteneStreckenabschnitte ws
                wsTVarEvent = tvarEvent ws
            case toggleButtonStrom ws of
                (Just toggleButtonStrom)
                    | Set.isSubsetOf wsStreckenabschnitte
                        $ enthalteneStreckenabschnitte bahngeschwindigkeit -> ohneEvent wsTVarEvent
                        $ Gtk.set toggleButtonStrom [Gtk.toggleButtonActive := True]
                _otherwise -> pure ()
            let istBahngeschwindigkeitTeilmenge =
                    Set.isSubsetOf wsBahngeschwindigkeiten
                    $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit
            case scaleGeschwindigkeit (GeschwindigkeitPhantom ws) of
                (Just scaleGeschwindigkeit)
                    | istBahngeschwindigkeitTeilmenge -> ohneEvent wsTVarEvent
                        $ Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := 0]
                _otherwise -> pure ()
            case auswahlFahrstrom (GeschwindigkeitPhantom ws) of
                (Just auswahlFahrstrom)
                    | istBahngeschwindigkeitTeilmenge
                        -> ohneEvent wsTVarEvent $ setzeAuswahl auswahlFahrstrom 0
                _otherwise -> pure ()

-- | Füge 'AuswahlWidget' zum Fahrtrichtung einstellen zur Box hinzu.
--
-- Mit der übergebenen 'TVar' kann das Anpassen der Label aus 'Zug.UI.Gtk.SpracheGui.sprachwechsel' gelöscht werden.
-- Dazu muss deren Inhalt auf 'Nothing' gesetzt werden.
auswahlFahrtrichtungEinstellenPackNew
    :: forall b bg o g m.
    ( MitTVarMaps (ReaderFamilie o)
    , MitSpracheGui (ReaderFamilie o)
    , ObjektReader o m
    , MitBox b
    , BahngeschwindigkeitKlasse bg
    , BahngeschwindigkeitContainer (bg g 'Lego)
    , BahngeschwindigkeitKlasse (GeschwindigkeitPhantom (WS o))
    , BahngeschwindigkeitContainer (WS o 'Lego)
    , BGWidgetsKlasse (GeschwindigkeitPhantom (WS o))
    , WidgetsTyp (WS o 'Lego)
    , BG o ~ BGWidgets
    , MonadIO m
    , GeschwindigkeitKlasse g
    )
    => b
    -> bg g 'Lego
    -> TVar (Maybe [Sprache -> IO ()])
    -> TVar EventAusführen
    -> StatusVar o
    -> m (AuswahlWidget Fahrtrichtung)
auswahlFahrtrichtungEinstellenPackNew
    box
    bahngeschwindigkeit
    tvarSprachwechsel
    tvarEventAusführen
    statusVar = do
    objektReader <- ask
    auswahlFahrtrichtung <- boxPackWidgetNewDefault box
        $ boundedEnumAuswahlRadioButtonNew
            Vorwärts
            (Just tvarSprachwechsel)
            Language.fahrtrichtung
    beiAuswahl auswahlFahrtrichtung $ \fahrtrichtung -> eventAusführen tvarEventAusführen
        $ flip runReaderT objektReader
        $ flip auswertenStatusVarMStatusT statusVar
        $ do
            ausführenAktion $ FahrtrichtungEinstellen bahngeschwindigkeit fahrtrichtung
            -- Widgets synchronisieren
            bahngeschwindigkeiten <- getBahngeschwindigkeiten
            liftIO $ forM_ bahngeschwindigkeiten $ flip bgWidgetsSynchronisieren fahrtrichtung
            wegstrecken <- getWegstrecken
            liftIO $ forM_ wegstrecken $ flip wsWidgetsSynchronisieren fahrtrichtung
    pure auswahlFahrtrichtung
    where
        bgWidgetsSynchronisieren
            :: ZugtypEither (GeschwindigkeitEither BGWidgets) -> Fahrtrichtung -> IO ()
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitPwm
                      BGWidgets
                      { bg
                      , bgTVarEvent
                      , bgGeschwindigkeitsWidgets = ScaleGeschwindigkeit {wScaleGeschwindigkeit}
                      , bgFahrtrichtungsWidgets = AuswahlFahrtrichtung {wAuswahlFahrtrichtung}}))
            fahrtrichtung
            | elem (ZugtypLego $ GeschwindigkeitPwm bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent $ do
                    Gtk.set wScaleGeschwindigkeit [Gtk.rangeValue := 0]
                    setzeAuswahl wAuswahlFahrtrichtung fahrtrichtung
        bgWidgetsSynchronisieren
            (ZugtypLego
                 (GeschwindigkeitKonstanteSpannung
                      BGWidgets
                      { bg
                      , bgTVarEvent
                      , bgGeschwindigkeitsWidgets = AuswahlFahrstrom {wAuswahlFahrstrom}
                      , bgFahrtrichtungsWidgets = AuswahlFahrtrichtung {wAuswahlFahrtrichtung}}))
            fahrtrichtung
            | elem (ZugtypLego $ GeschwindigkeitKonstanteSpannung bg)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent bgTVarEvent $ do
                    setzeAuswahl wAuswahlFahrstrom 0
                    setzeAuswahl wAuswahlFahrtrichtung fahrtrichtung
        bgWidgetsSynchronisieren _bgWidgets _wert = pure ()

        wsWidgetsSynchronisieren :: ZugtypEither (WS o) -> Fahrtrichtung -> IO ()
        wsWidgetsSynchronisieren (ZugtypLego ws) fahrtrichtung
            | Set.isSubsetOf (enthalteneBahngeschwindigkeiten ws)
                $ enthalteneBahngeschwindigkeiten bahngeschwindigkeit =
                ohneEvent (tvarEvent ws) $ do
                    case scaleGeschwindigkeit (GeschwindigkeitPhantom ws) of
                        (Just scaleGeschwindigkeit)
                            -> Gtk.set scaleGeschwindigkeit [Gtk.rangeValue := 0]
                        Nothing -> pure ()
                    case auswahlFahrstrom (GeschwindigkeitPhantom ws) of
                        (Just auswahlFahrstrom) -> setzeAuswahl auswahlFahrstrom 0
                        Nothing -> pure ()
                    case auswahlFahrtrichtung (GeschwindigkeitPhantom ws) of
                        (Just auswahlFahrtrichtung)
                            -> setzeAuswahl auswahlFahrtrichtung fahrtrichtung
                        Nothing -> pure ()
        wsWidgetsSynchronisieren _wsWidget _wert = pure ()
#endif
--
