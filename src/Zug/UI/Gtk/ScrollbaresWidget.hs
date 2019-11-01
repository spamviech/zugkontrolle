{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}

{-|
Description: Scrollbare Widgetw
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.ScrollbaresWidget () where
#else
module Zug.UI.Gtk.ScrollbaresWidget (
    ScrollbaresWidget(), scrollbaresWidgetNew, scrollbaresWidgetAddNew,
    scrollbaresWidgetPackNew, scrollbaresWidgetNotebookAppendPageNew) where

import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp(..))
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Hilfsfunktionen (widgetShowNew, containerAddWidgetNew, boxPackWidgetNew, Packing(..),
                                    paddingDefault, positionDefault, notebookAppendPageNew)
import Zug.UI.Gtk.Klassen (MitWidget(..),  MitContainer(..), MitBox(..), MitGrid(..), MitFixed(..), MitLabel(..),
                                    MitNotebook(..), MitPaned(..), MitComboBox(..), MitWindow(..), MitDialog(..),
                                    MitButton(..), MitToggleButton(..), MitCheckButton(..), MitRadioButton(..))

-- | 'Gtk.ScrolledWindow' mit automatisch erstelltem Viewport
data ScrollbaresWidget w = ScrollbaresWidget {swScrolledWindow :: Gtk.ScrolledWindow, swWidget :: w}

instance MitWidget (ScrollbaresWidget w) where
    erhalteWidget :: ScrollbaresWidget w -> Gtk.Widget
    erhalteWidget = erhalteWidget . swScrolledWindow

instance (MitContainer w) => MitContainer (ScrollbaresWidget w) where
    erhalteContainer :: ScrollbaresWidget w -> Gtk.Container
    erhalteContainer = erhalteContainer . swWidget
instance (MitBox w) => MitBox (ScrollbaresWidget w) where
    erhalteBox :: ScrollbaresWidget w -> Gtk.Box
    erhalteBox = erhalteBox . swWidget
instance (MitGrid w) => MitGrid (ScrollbaresWidget w) where
    erhalteGrid :: ScrollbaresWidget w -> Gtk.Grid
    erhalteGrid = erhalteGrid . swWidget
instance (MitFixed w) => MitFixed (ScrollbaresWidget w) where
    erhalteFixed :: ScrollbaresWidget w -> Gtk.Fixed
    erhalteFixed = erhalteFixed . swWidget
instance (MitLabel w) => MitLabel (ScrollbaresWidget w) where
    erhalteLabel :: ScrollbaresWidget w -> Gtk.Label
    erhalteLabel = erhalteLabel . swWidget
instance (MitNotebook w) => MitNotebook (ScrollbaresWidget w) where
    erhalteNotebook :: ScrollbaresWidget w -> Gtk.Notebook
    erhalteNotebook = erhalteNotebook . swWidget
instance (MitPaned w) => MitPaned (ScrollbaresWidget w) where
    erhaltePaned :: ScrollbaresWidget w -> Gtk.Paned
    erhaltePaned = erhaltePaned . swWidget
instance (MitComboBox w) => MitComboBox (ScrollbaresWidget w) where
    erhalteComboBox :: ScrollbaresWidget w -> Gtk.ComboBox
    erhalteComboBox = erhalteComboBox . swWidget
instance (MitWindow w) => MitWindow (ScrollbaresWidget w) where
    erhalteWindow :: ScrollbaresWidget w -> Gtk.Window
    erhalteWindow = erhalteWindow . swWidget
instance (MitDialog w) => MitDialog (ScrollbaresWidget w) where
    erhalteDialog :: ScrollbaresWidget w -> Gtk.Dialog
    erhalteDialog = erhalteDialog . swWidget
instance (MitButton w) => MitButton (ScrollbaresWidget w) where
    erhalteButton :: ScrollbaresWidget w -> Gtk.Button
    erhalteButton = erhalteButton . swWidget
instance (MitToggleButton w) => MitToggleButton (ScrollbaresWidget w) where
    erhalteToggleButton :: ScrollbaresWidget w -> Gtk.ToggleButton
    erhalteToggleButton = erhalteToggleButton . swWidget
instance (MitCheckButton w) => MitCheckButton (ScrollbaresWidget w) where
    erhalteCheckButton :: ScrollbaresWidget w -> Gtk.CheckButton
    erhalteCheckButton = erhalteCheckButton . swWidget
instance (MitRadioButton w) => MitRadioButton (ScrollbaresWidget w) where
    erhalteRadioButton :: ScrollbaresWidget w -> Gtk.RadioButton
    erhalteRadioButton = erhalteRadioButton . swWidget

-- | Erstelle neues 'ScrollbaresWidget'
scrollbaresWidgetNew :: (MonadIO m, MitWidget w) => m w -> m (ScrollbaresWidget w)
scrollbaresWidgetNew konstruktor = do
    widget <- widgetShowNew konstruktor
    liftIO $ do
        swScrolledWindow <- widgetShowNew $ Gtk.scrolledWindowNew Nothing Nothing
        Gtk.set swScrolledWindow [
            Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever,
            Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAlways]
        Gtk.scrolledWindowAddWithViewport swScrolledWindow $ erhalteWidget widget
        pure ScrollbaresWidget {swScrolledWindow, swWidget = widget}

-- | Erstelle neues 'ScrollbaresWidget'und füge sie zu 'MitContainer' hinzu
scrollbaresWidgetAddNew :: (MonadIO m, MitContainer c, MitWidget w) => c -> m w -> m (ScrollbaresWidget w)
scrollbaresWidgetAddNew container konstruktor =containerAddWidgetNew container $ scrollbaresWidgetNew konstruktor

-- | Erstelle neues 'ScrollbaresWidget' und packe sie in eine 'MitBox'
scrollbaresWidgetPackNew :: (MonadIO m, MitBox b, MitWidget w) => b -> m w -> m (ScrollbaresWidget w)
scrollbaresWidgetPackNew box konstruktor
    = boxPackWidgetNew box PackGrow paddingDefault positionDefault $ scrollbaresWidgetNew konstruktor

-- | Erstelle neues 'ScrollbaresWidget' und füge sie als neue Seite einem 'MitNotebook' hinzu
scrollbaresWidgetNotebookAppendPageNew :: (MonadIO m, MitNotebook n, MitWidget w) =>
    n -> Text -> m w -> m (ScrollbaresWidget w, Int)
scrollbaresWidgetNotebookAppendPageNew notebook name konstruktor
    = notebookAppendPageNew notebook name $ scrollbaresWidgetNew konstruktor
#endif