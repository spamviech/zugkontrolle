{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}

{-|
Description: Erweiterbare Typklassen angelehnt an "Graphics.UI.Gtk"-Typklassen

Typklassen aus "Graphics.UI.Gtk" lassen eine Instanziierung eigener Typen nicht ohne Probleme zu.
Diese Modul stellt via Template Haskell erstellte alternative Mit-Typklassen um dieses Problem zu umgehen.

Außerdem wird eine /Overlappable/-Standard Instanz für "Graphics.UI.Gtk"-Typen bereitgestellt.
Aufgrund dieser wird in Modulen mit Funktionen, die diese Typklassen verwenden die Verwendung der Spracherweiterung /MonoLocalBinds/ empfohlen.
-}
#ifndef ZUGKONTROLLEGUI
module Zug.UI.Gtk.Klassen () where
#else
module Zug.UI.Gtk.Klassen (
    -- * Widget
    MitWidget(..), mitWidgetHide, mitWidgetShow,
    -- ** Label
    MitLabel(..),
    -- ** Entry
    MitEntry(..),
    -- ** Range
    MitRange(..),
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
import qualified Graphics.UI.Gtk as Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.Enums (Zugtyp(..), ZugtypEither(..))

-- * Widget
-- | Klasse für Typen, die ein 'Gtk.Widget' enthalten.
class MitWidget w where
    erhalteWidget :: w -> Gtk.Widget

instance {-# Overlappable #-} (Gtk.WidgetClass a) => MitWidget a where
    erhalteWidget :: a -> Gtk.Widget
    erhalteWidget = Gtk.toWidget

instance (MitWidget (a 'Märklin), MitWidget (a 'Lego)) => MitWidget (ZugtypEither a) where
    erhalteWidget :: ZugtypEither a -> Gtk.Widget
    erhalteWidget   (ZugtypMärklin a)   = erhalteWidget a
    erhalteWidget   (ZugtypLego a)      = erhalteWidget a

instance (MitWidget a, MitWidget b) => MitWidget (Either a b) where
    erhalteWidget :: Either a b -> Gtk.Widget
    erhalteWidget = either erhalteWidget erhalteWidget

-- | Zeige ein 'MitWidget'
mitWidgetShow :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetShow = liftIO . Gtk.widgetShow . erhalteWidget

-- | Verstecke ein 'MitWidget'
mitWidgetHide :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetHide = liftIO . Gtk.widgetHide . erhalteWidget

-- ** Label
-- | Klasse für Typen, die ein 'Gtk.Label' enthalten
class (MitWidget a) => MitLabel a where
    erhalteLabel :: a -> Gtk.Label

instance {-# Overlappable #-} (Gtk.LabelClass a) => MitLabel a where
    erhalteLabel :: a -> Gtk.Label
    erhalteLabel = Gtk.toLabel

-- ** Entry
-- | Klasse für Typen, die ein 'Gtk.Entry' enthalten
class (MitWidget a) => MitEntry a where
    erhalteEntry :: a -> Gtk.Entry

instance {-# Overlappable #-} (Gtk.EntryClass a) => MitEntry a where
    erhalteEntry :: a -> Gtk.Entry
    erhalteEntry = Gtk.toEntry

-- ** Range
-- | Klasse für Typen, die ein 'Gtk.Range' enthalten
class (MitWidget a) => MitRange a where
    erhalteRange :: a -> Gtk.Range

instance {-# Overlappable #-} (Gtk.RangeClass a) => MitRange a where
    erhalteRange :: a -> Gtk.Range
    erhalteRange = Gtk.toRange

-- ** Container
-- | Klasse für Typen, die einen 'Gtk.Container' enthalten
class (MitWidget a) => MitContainer a where
    erhalteContainer :: a -> Gtk.Container

instance {-# Overlappable #-} (Gtk.ContainerClass a) => MitContainer a where
    erhalteContainer :: a -> Gtk.Container
    erhalteContainer = Gtk.toContainer

-- | Füge ein 'MitWidget' zu einem 'MitContainer' hinzu
mitContainerAdd :: (MonadIO m, MitContainer c, MitWidget w) => c -> w -> m ()
mitContainerAdd container widget = liftIO $ Gtk.containerAdd (erhalteContainer container) (erhalteWidget widget)

-- | Entferne ein 'MitWidget' aus einem 'MitContainer'
mitContainerRemove :: (MonadIO m, MitContainer c, MitWidget w) => c -> w -> m ()
mitContainerRemove container widget = liftIO $ Gtk.containerRemove (erhalteContainer container) (erhalteWidget widget)

-- *** Box
-- | Klasse für Typen, die eine 'Gtk.Box' enthalten
class (MitContainer a) => MitBox a where
    erhalteBox :: a -> Gtk.Box

instance {-# Overlappable #-} (Gtk.BoxClass a) => MitBox a where
    erhalteBox :: a -> Gtk.Box
    erhalteBox = Gtk.toBox

-- | Füge ein 'MitWidget' zum Anfang einer 'MitBox' hinzu
mitBoxPackStart :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Gtk.Packing -> Int -> m ()
mitBoxPackStart box widget packing padding = liftIO $ 
    Gtk.boxPackStart (erhalteBox box) (erhalteWidget widget) packing padding

-- | Füge ein 'MitWidget' zum Ende einer 'MitBox' hinzu
mitBoxPackEnd :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Gtk.Packing -> Int -> m ()
mitBoxPackEnd box widget packing padding = liftIO $ 
    Gtk.boxPackEnd (erhalteBox box) (erhalteWidget widget) packing padding

-- *** Grid
-- | Klasse für Typen, die ein 'Gtk.Grid' enthalten
class (MitContainer a) => MitGrid a where
    erhalteGrid :: a -> Gtk.Grid

instance {-# Overlappable #-} (Gtk.GridClass a) => MitGrid a where
    erhalteGrid :: a -> Gtk.Grid
    erhalteGrid = Gtk.toGrid

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

-- *** Fixed
-- | Klasse für Typen, die ein 'Gtk.Fixed' enthalten
class (MitContainer a) => MitFixed a where
    erhalteFixed :: a -> Gtk.Fixed

instance {-# Overlappable #-} (Gtk.FixedClass a) => MitFixed a where
    erhalteFixed :: a -> Gtk.Fixed
    erhalteFixed = Gtk.toFixed

-- | Füge ein 'MitWidget' an den spezifizierten Koordinaten zu einem 'MitFixed' hinzu
mitFixedPut :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Int -> Int -> m ()
mitFixedPut fixed widget x y = liftIO $ Gtk.fixedPut (erhalteFixed fixed) (erhalteWidget widget) (x, y)

-- | Bewege ein 'MitWidget' zu den spezifizierten Koordinaten ein einem 'MitFixed'
mitFixedMove :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Int -> Int -> m ()
mitFixedMove fixed widget x y = liftIO $ Gtk.fixedMove (erhalteFixed fixed) (erhalteWidget widget) (x, y)

-- *** Notebook
-- | Klasse für Typen, die ein 'Gtk.Notebook' enthalten
class (MitContainer a) => MitNotebook a where
    erhalteNotebook :: a -> Gtk.Notebook

instance {-# Overlappable #-} (Gtk.NotebookClass a) => MitNotebook a where
    erhalteNotebook :: a -> Gtk.Notebook
    erhalteNotebook = Gtk.toNotebook

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
    = liftIO . Gtk.notebookRemovePage (erhalteNotebook notebook)

-- | Setzte die aktuell angezeigte Seite eines 'MitNotebook'
mitNotebookSetCurrentPage :: (MonadIO m, MitNotebook n) => n -> Int -> m ()
mitNotebookSetCurrentPage notebook
    = liftIO . Gtk.notebookSetCurrentPage (erhalteNotebook notebook)

-- *** Paned
-- | Klasse für Typen, die ein 'Gtk.Paned' enthalten
class (MitContainer a) => MitPaned a where
    erhaltePaned :: a -> Gtk.Paned

instance {-# Overlappable #-} (Gtk.PanedClass a) => MitPaned a where
    erhaltePaned :: a -> Gtk.Paned
    erhaltePaned = Gtk.toPaned

-- *** ComboBox
-- | Klasse für Typen, die eine 'Gtk.ComboBox' enthalten
class (MitContainer a) => MitComboBox a where
    erhalteComboBox :: a -> Gtk.ComboBox

instance {-# Overlappable #-} (Gtk.ComboBoxClass a) => MitComboBox a where
    erhalteComboBox :: a -> Gtk.ComboBox
    erhalteComboBox = Gtk.toComboBox

-- ** Window
-- | Klasse für Typen, die ein 'Gtk.Window' enthalten
class (MitContainer a) => MitWindow a where
    erhalteWindow :: a -> Gtk.Window

instance {-# Overlappable #-} (Gtk.WindowClass a) => MitWindow a where
    erhalteWindow :: a -> Gtk.Window
    erhalteWindow = Gtk.toWindow

-- *** Dialog
-- | Klasse für Typen, die einen 'Gtk.Dialog' enthalten
class (MitWindow a) => MitDialog a where
    erhalteDialog :: a -> Gtk.Dialog

instance {-# Overlappable #-} (Gtk.DialogClass a) => MitDialog a where
    erhalteDialog :: a -> Gtk.Dialog
    erhalteDialog = Gtk.toDialog

-- ** Button
-- | Klasse für Typen, die einen 'Gtk.Button' enthalten
class (MitContainer a) => MitButton a where
    erhalteButton :: a -> Gtk.Button

instance {-# Overlappable #-} (Gtk.ButtonClass a) => MitButton a where
    erhalteButton :: a -> Gtk.Button
    erhalteButton = Gtk.toButton

-- *** ToggleButton
-- | Klasse für Typen, die einen 'Gtk.ToggleButton' enthalten
class (MitButton a) => MitToggleButton a where
    erhalteToggleButton :: a -> Gtk.ToggleButton

instance {-# Overlappable #-} (Gtk.ToggleButtonClass a) => MitToggleButton a where
    erhalteToggleButton :: a -> Gtk.ToggleButton
    erhalteToggleButton = Gtk.toToggleButton

-- *** CheckButton
-- | Klasse für Typen, die einen 'Gtk.CheckButton' enthalten
class (MitToggleButton a) => MitCheckButton a where
    erhalteCheckButton :: a -> Gtk.CheckButton

instance {-# Overlappable #-} (Gtk.CheckButtonClass a) => MitCheckButton a where
    erhalteCheckButton :: a -> Gtk.CheckButton
    erhalteCheckButton = Gtk.toCheckButton

-- *** RadioButton
-- | Klasse für Typen, die einen 'Gtk.RadioButton' enthalten
class (MitCheckButton a) => MitRadioButton a where
    erhalteRadioButton :: a -> Gtk.RadioButton

instance {-# Overlappable #-} (Gtk.RadioButtonClass a) => MitRadioButton a where
    erhalteRadioButton :: a -> Gtk.RadioButton
    erhalteRadioButton = Gtk.toRadioButton
#endif