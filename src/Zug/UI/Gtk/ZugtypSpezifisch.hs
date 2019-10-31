{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

{-|
Description: Widget zur Auswahl eines Bounded Enums
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.ZugtypSpezifisch () where
#else
module Zug.UI.Gtk.ZugtypSpezifisch (ZugtypSpezifisch(), zugtypSpezifischNew, zugtypSpezifischButtonNew) where

-- Bibliotheken
import Control.Monad (forM_, forM)
import Control.Monad.Trans (MonadIO(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.Klassen (Zugtyp(..))
import Zug.UI.Gtk.Auswahl (AuswahlWidget, beiAuswahl)
import Zug.UI.Gtk.Hilfsfunktionen (boxPackWidgetNewDefault, boxPackDefault, widgetShowIf)
import Zug.UI.Gtk.Klassen (MitWidget(..), MitButton(..), MitContainer(..))

-- | Widgets, die nur bei passender 'Zugtyp'-Auswahl angezeigt werden.
data ZugtypSpezifisch w where
    ZugtypSpezifisch :: {
        vBox :: Gtk.VBox}
            -> ZugtypSpezifisch Gtk.Widget
    ZugtypSpezifischButton :: {
        buttonVBox :: Gtk.VBox,
        buttonDummy :: Gtk.Button}
            -> ZugtypSpezifisch Gtk.Button

deriving instance (Eq w) => Eq (ZugtypSpezifisch w)

instance (MitWidget w) => MitWidget (ZugtypSpezifisch w) where
    erhalteWidget :: ZugtypSpezifisch w -> Gtk.Widget
    erhalteWidget   ZugtypSpezifisch {vBox}             = erhalteWidget vBox
    erhalteWidget   ZugtypSpezifischButton {buttonVBox} = erhalteWidget buttonVBox

instance MitContainer (ZugtypSpezifisch Gtk.Button) where
    erhalteContainer :: ZugtypSpezifisch Gtk.Button -> Gtk.Container
    erhalteContainer = erhalteContainer . buttonDummy
instance MitButton (ZugtypSpezifisch Gtk.Button) where
    erhalteButton :: ZugtypSpezifisch Gtk.Button -> Gtk.Button
    erhalteButton = buttonDummy

-- | Erzeuge ein 'ZugtypSpezifisch' aus den übergebenen Widgets und dem 'AuswahlWidget'
zugtypSpezifischNew :: (MitWidget w, MonadIO m) =>
    NonEmpty (Zugtyp, w) -> AuswahlWidget Zugtyp -> m (ZugtypSpezifisch Gtk.Widget)
zugtypSpezifischNew eingabeWidgets auswahlWidget = liftIO $ do
    vBox <- Gtk.vBoxNew False 0
    zugtypWidgets <- forM eingabeWidgets $ \(zugtyp, mitWidget) -> do
        hiddenBox <- boxPackWidgetNewDefault vBox $ Gtk.hBoxNew False 0
        let widget = erhalteWidget mitWidget
        Gtk.widgetShow widget
        boxPackDefault hiddenBox widget
        pure (zugtyp, widget)
    beiAuswahl auswahlWidget $ \gewählterZugtyp -> forM_ zugtypWidgets $ \(zugtyp, widget) ->
        widgetShowIf (gewählterZugtyp == zugtyp) widget
    pure ZugtypSpezifisch {vBox}

-- | Erzeuge ein 'ZugtypSpezifisch' aus den übergebenen Widgets und dem 'AuswahlWidget'.
-- Das erzeugte 'ZugtypSpezifisch' implementiert eine 'MitButton'-Instanz.
zugtypSpezifischButtonNew :: (MitButton w, MonadIO m) =>
    NonEmpty (Zugtyp, w) -> AuswahlWidget Zugtyp -> m (ZugtypSpezifisch Gtk.Button)
zugtypSpezifischButtonNew eingabeWidgets buttonAuswahlWidget = liftIO $ do
    buttonVBox <- Gtk.vBoxNew False 0
    buttonDummy <- Gtk.buttonNew
    buttonZugtypWidgets <- forM eingabeWidgets $ \(zugtyp, mitButton) -> do
        hiddenBox <- boxPackWidgetNewDefault buttonVBox $ Gtk.hBoxNew False 0
        let widget = erhalteWidget mitButton
        Gtk.widgetShow widget
        boxPackDefault hiddenBox widget
        let button = erhalteButton mitButton
        Gtk.on button Gtk.buttonActivated $ Gtk.buttonClicked buttonDummy
        pure (zugtyp, button)
    beiAuswahl buttonAuswahlWidget $ \gewählterZugtyp -> forM_ buttonZugtypWidgets $ \(zugtyp, widget) ->
        widgetShowIf (gewählterZugtyp == zugtyp) widget
    pure ZugtypSpezifischButton {buttonVBox, buttonDummy}
#endif