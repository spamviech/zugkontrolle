{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

{-|
Description: Scrollbare Widgetw
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Widget.ScrollbaresWidget () where
#else
module Zug.UI.Gtk.Widget.ScrollbaresWidget (
    ScrollbaresWidget(), scrollbaresWidgetNew, scrollbaresWidgetAddNew,
    scrollbaresWidgetPackNew, scrollbaresWidgetNotebookAppendPageNew) where

import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeit von anderen Modulen
import Zug.UI.Gtk.Widget.Klassen (MitWidget(..),  MitContainer(..), MitBox(..), MitGrid(..), MitFixed(..), MitLabel(..),
                                    MitNotebook(..), MitPaned(..), MitComboBox(..), MitWindow(..), MitDialog(..),
                                    MitButton(..), MitToggleButton(..), MitCheckButton(..), MitRadioButton(..))

-- | ScrolledWindow mit automatisch erstelltem Viewport
data ScrollbaresWidget w = ScrollbaresWidget {swScrolledWindow :: Gtk.ScrolledWindow, swWidget :: w}

instance MitWidget (ScrolledWidget w) where
    erhalteWidget :: ScrolledWidget w -> Gtk.Widget
    erhalteWidget = erhalteWidget . swScrolledWindow

instance (MitContainer w) => MitContainer (ScrollbaresWidget w) where
    erhalteContainer :: ScrollbaresWidget w -> Gtk.
    erhalteContainer = erhalteContainer . swWidget
instance (MitBox w) => MitBox (ScrollbaresWidget w) where
    erhalteBox :: ScrollbaresWidget w -> Gtk.
    erhalteBox = erhalteBox . swWidget
instance (Mit w) => MitGrid (ScrollbaresWidget w) where
    erhalteGrid :: ScrollbaresWidget w -> Gtk.
    erhalteGrid = erhalteGrid . swWidget
instance (MitFixed w) => MitFixed (ScrollbaresWidget w) where
    erhalteFixed :: ScrollbaresWidget w -> Gtk.
    erhalteFixed = erhalteFixed . swWidget
instance (MitLabel w) => MitLabel (ScrollbaresWidget w) where
    erhalteLabel :: ScrollbaresWidget w -> Gtk.
    erhalteLabel = erhalteLabel . swWidget
instance (MitNotebook w) => MitNotebook (ScrollbaresWidget w) where
    erhalteNotebook :: ScrollbaresWidget w -> Gtk.
    erhalteNotebook = erhalteNotebook . swWidget
instance (Mit w) => MitPaned (ScrollbaresWidget w) where
    erhaltePaned :: ScrollbaresWidget w -> Gtk.
    erhaltePaned = erhaltePaned . swWidget
instance (MitComboBox w) => MitComboBox (ScrollbaresWidget w) where
    erhalteComboBox :: ScrollbaresWidget w -> Gtk.
    erhalteComboBox = erhalteComboBox . swWidget
instance (MitWindow w) => MitWindow (ScrollbaresWidget w) where
    erhalteWindow :: ScrollbaresWidget w -> Gtk.
    erhalteWindow = erhalteWindow . swWidget
instance (MitDialog w) => MitDialog (ScrollbaresWidget w) where
    erhalteDialog :: ScrollbaresWidget w -> Gtk.
    erhalteDialog = erhalteDialog . swWidget
instance (MitButton w) => MitButton (ScrollbaresWidget w) where
    erhalteButton :: ScrollbaresWidget w -> Gtk.
    erhalteButton = erhalteButton . swWidget
instance (MitToggleButton w) => MitToggleButton (ScrollbaresWidget w) where
    erhalteToggleButton :: ScrollbaresWidget w -> Gtk.
    erhalteToggleButton = erhalteToggleButton . swWidget
instance (MitCheckButton w) => MitCheckButton (ScrollbaresWidget w) where
    erhalteCheckButton :: ScrollbaresWidget w -> Gtk.
    erhalteCheckButton = erhalteCheckButton . swWidget
instance (MitRadioButton w) => MitRadioButton (ScrollbaresWidget w) where
    erhalteRadioButton :: ScrollbaresWidget w -> Gtk.
    erhalteRadioButton = erhalteRadioButton . swWidget

-- | Erstelle neues 'Scrollbares
scrollbaresWidgetNew :: (MitWidget w) => IO w -> IO (Gtk.ScrolledWindow, w)
scrollbaresWidgetNew konstruktor = do
    widget <- widgetShowNew konstruktor
    scrolledWindow <- widgetShowNew $ scrolledWindowNew Nothing Nothing
    set scrolledWindow [scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyAlways]
    scrolledWindowAddWithViewport scrolledWindow widget
    pure (scrolledWindow, widget)

-- | Erstelle neues 'ScrolledWindow' mit automatisch erstelltem Viewport und füge sie zu 'Container' hinzu
scrollbaresWidgetAddNew :: (MitContainer c, MitWidget w) => c -> IO w -> IO (ScrolledWindow, w)
scrollbaresWidgetAddNew container konstruktor = do
    (scrolledWindow, widget) <- scrolledWidgetNew konstruktor
    containerAddWidgetNew container $ pure scrolledWindow
    pure (scrolledWindow, widget)

-- | Erstelle neues 'ScrolledWindow' mit automatisch erstelltem Viewport und packe sie in eine 'Box'
scrollbaresWidgetPackNew :: (MitBox b, MitWidget w) => b -> IO w -> IO (ScrolledWindow, w)
scrollbaresWidgetPackNew box konstruktor = do
    (scrolledWindow, widget) <- scrolledWidgetNew konstruktor
    boxPackWidgetNew box PackGrow paddingDefault positionDefault $ pure scrolledWindow
    pure (scrolledWindow, widget)

-- | Seite mit scrollbarer VBox einem Notebook hinzufügen
scrollbaresWidgetNotebookAppendPageNew :: (MitNotebook n, MitWidget w) => n -> Text -> IO w -> IO (ScrolledWindow, w)
scrollbaresWidgetNotebookAppendPageNew notebook name konstruktor = do
    widget <- widgetShowNew konstruktor
    scrolledWindow <- notebookAppendPageNew notebook name $ widgetShowNew $ scrolledWindowNew Nothing Nothing
    set scrolledWindow [scrolledWindowHscrollbarPolicy := PolicyNever, scrolledWindowVscrollbarPolicy := PolicyAlways]
    scrolledWindowAddWithViewport scrolledWindow widget
    pure (scrolledWindow, widget)