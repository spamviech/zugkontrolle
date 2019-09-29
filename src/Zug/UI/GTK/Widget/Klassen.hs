{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{-|
Description: Erweiterbare Typklassen angelehnt an "Graphics.UI.Gtk"-Typklassen
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Widget.Klassen () where
#else
module Zug.UI.Gtk.Widget.Klassen (
    -- * Widget
    MitWidget(..), mitWidgetShow, mitWidgetHide,
    -- * Container
    MitContainer(..), mitContainerAdd, mitContainerRemove,
    -- ** Box
    MitBox(..), mitBoxPackStart, mitBoxPackEnd,
    -- ** Grid
    MitGrid(..), mitGridAttach, mitGridAttachNextTo,
    -- ** Fixed
    MitFixed(..), mitFixedPut, mitFixedMove,
    -- ** Notebook
    MitNotebook(..), mitNotebookAppendPage, mitNotebookPrependPage,
    mitNotebookRemovePage, mitNotebookSetCurrentPage,
    -- ** Paned
    MitPaned(..),
    -- ** ComboBox
    MitComboBox(..),
    -- * Window
    MitWindow(..),
    -- ** Dialog
    MitDialog(..),
    -- * Label
    MitLabel(..),
    -- * Button
    MitButton(..),
    -- ** ToggleButton
    MitToggleButton(..),
    -- ** CheckButton
    MitCheckButton(..),
    -- ** RadioButton
    MitRadioButton(..)) where

import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import Graphics.UI.Gtk (Widget, WidgetClass(), toWidget, Container, ContainerClass(), toContainer,
                        Box, BoxClass(), toBox, Grid, GridClass(), toGrid,
                        Fixed, FixedClass(), toFixed, Notebook, NotebookClass(), toNotebook,
                        Paned, PanedClass(), toPaned, ComboBox, ComboBoxClass(), toComboBox,
                        Window, WindowClass(), toWindow, Dialog, DialogClass(), toDialog,
                        Label, LabelClass(), toLabel,
                        Button, ButtonClass(), toButton, ToggleButton, ToggleButtonClass(), toToggleButton,
                        CheckButton, CheckButtonClass(), toCheckButton, RadioButton, RadioButtonClass(), toRadioButton)
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.UI.Gtk.Widget.TemplateHaskell (erzeugeKlasse)

-- * Widget
erzeugeKlasse [] "Widget"

-- | Zeige ein 'MitWidget'
mitWidgetShow :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetShow = liftIO . mitWidget Gtk.widgetShow

-- | Verstecke ein 'MitWidget'
mitWidgetHide :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetHide = liftIO . mitWidget Gtk.widgetHide

erzeugeKlasse [''MitWidget] "Container"

-- * Container
-- | Füge ein 'MitWidget' zu einem 'MitContainer' hinzu
mitContainerAdd :: (MonadIO m, MitContainer c, MitWidget w) => c -> w -> m ()
mitContainerAdd container widget = liftIO $ Gtk.containerAdd (erhalteContainer container) (erhalteWidget widget)

-- | Entferne ein 'MitWidget' aus einem 'MitContainer'
mitContainerRemove :: (MonadIO m, MitContainer c, MitWidget w) => c -> w -> m ()
mitContainerRemove container widget = liftIO $ Gtk.containerRemove (erhalteContainer container) (erhalteWidget widget)

erzeugeKlasse [''MitContainer] "Box"

-- | Füge ein 'MitWidget' zum Anfang einer 'MitBox' hinzu
mitBoxPackStart :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Gtk.Packing -> Int -> m ()
mitBoxPackStart box widget packing padding = liftIO $ Gtk.boxPackStart (erhalteBox box) (erhalteWidget widget) packing padding

-- | Füge ein 'MitWidget' zum Ende einer 'MitBox' hinzu
mitBoxPackEnd :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Gtk.Packing -> Int -> m ()
mitBoxPackEnd box widget packing padding = liftIO $ Gtk.boxPackEnd (erhalteBox box) (erhalteWidget widget) packing padding

erzeugeKlasse [''MitContainer] "Grid"

-- | Füge ein 'MitWidget' zu einem 'MitGrid' hinzu
mitGridAttach :: (MonadIO m, MitGrid g, MitWidget w) => g -> w -> Int -> Int -> Int -> Int -> m ()
mitGridAttach grid widget left top width height
    = liftIO $ Gtk.gridAttach (erhalteGrid grid) (erhalteWidget widget) left top width height

-- | Füge ein 'MitWidget' zu einem 'MitGrid' direkt neben einem 'MitWidget' hinzu
mitGridAttachNextTo :: (MonadIO m, MitGrid g, MitWidget w, MitWidget sibling)
    => g -> w -> Maybe sibling -> Gtk.PositionType -> Int -> Int -> m ()
mitGridAttachNextTo grid widget sibling position width height
    = liftIO $ Gtk.gridAttachNextTo
        (erhalteGrid grid)
        (erhalteWidget widget)
        (sibling >>= pure . erhalteWidget)
        position
        width
        height

erzeugeKlasse [''MitContainer] "Fixed"

-- | Füge ein 'MitWidget' an den spezifizierten Koordinaten zu einem 'MitFixed' hinzu
mitFixedPut :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Int -> Int -> m ()
mitFixedPut fixed widget x y = liftIO $ Gtk.fixedPut (erhalteFixed fixed) (erhalteWidget widget) (x, y)

-- | Bewege ein 'MitWidget' zu den spezifizierten Koordinaten ein einem 'MitFixed'
mitFixedMove :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Int -> Int -> m ()
mitFixedMove fixed widget x y = liftIO $ Gtk.fixedMove (erhalteFixed fixed) (erhalteWidget widget) (x, y)

erzeugeKlasse [''MitContainer] "Notebook"

-- | Füge eine neue Seite am Ende eines 'MitNotebook' hinzu
mitNotebookAppendPage :: (MonadIO m, MitNotebook n, MitWidget w) => n -> w -> Text -> m Int
mitNotebookAppendPage notebook widget
    = liftIO . Gtk.notebookAppendPage (erhalteNotebook notebook) (erhalteWidget widget)

-- | Füge eine neue Seite am Anfang eines 'MitNotebook' hinzu
mitNotebookPrependPage :: (MonadIO m, MitNotebook n, MitWidget w) => n -> w -> Text -> m Int
mitNotebookPrependPage notebook widget
    = liftIO . Gtk.notebookPrependPage (erhalteNotebook notebook) (erhalteWidget widget)

-- | Entferne eine Seite aus einem 'MitNotebook' hinzu
mitNotebookRemovePage :: (MonadIO m, MitNotebook n) => n -> Int -> m ()
mitNotebookRemovePage notebook
    = liftIO . mitNotebook Gtk.notebookRemovePage notebook

-- | Setzte die aktuell angezeigte Seite eines 'MitNotebook'
mitNotebookSetCurrentPage :: (MonadIO m, MitNotebook n) => n -> Int -> m ()
mitNotebookSetCurrentPage notebook
    = liftIO . mitNotebook Gtk.notebookSetCurrentPage notebook

erzeugeKlasse [''MitContainer] "Paned"

erzeugeKlasse [''MitContainer] "ComboBox"

-- * Window
erzeugeKlasse [''MitContainer] "Window"

erzeugeKlasse [''MitWindow] "Dialog"

-- * Label
erzeugeKlasse [''MitWidget] "Label"

-- * Button
erzeugeKlasse [''MitContainer] "Button"

erzeugeKlasse [''MitButton] "ToggleButton"

erzeugeKlasse [''MitToggleButton] "CheckButton"

erzeugeKlasse [''MitCheckButton] "RadioButton"
#endif