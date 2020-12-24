{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -ddump-splices #-}
{-|
Description: Erweiterbare Typklassen angelehnt an "GI.Gtk"-Typklassen.

Typklassen aus "GI.Gtk" lassen eine Instanziierung eigener Typen nicht ohne Probleme zu.
Diese Modul stellt via Template Haskell erstellte alternative Mit-Typklassen um dieses Problem zu umgehen.

Außerdem wird eine /Overlappable/-Standard Instanz für "GI.Gtk"-Typen bereitgestellt.
Aufgrund dieser wird in Modulen mit Funktionen, die diese Typklassen verwenden die Verwendung der Spracherweiterung /MonoLocalBinds/ empfohlen.
-}
module Zug.UI.Gtk.Klassen where

import Control.Monad.Trans (MonadIO())
import Data.Int (Int32)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import Zug.Enums (Zugtyp(..), ZugtypEither(..))
import Zug.UI.Gtk.Klassen.TemplateHaskell (erzeugeKlasse)

-- * Widget
erzeugeKlasse [] "Widget"

instance (MitWidget (a 'Märklin), MitWidget (a 'Lego)) => MitWidget (ZugtypEither a) where
    erhalteWidget :: (MonadIO m) => ZugtypEither a -> m Gtk.Widget
    erhalteWidget (ZugtypMärklin a) = erhalteWidget a
    erhalteWidget (ZugtypLego a) = erhalteWidget a

-- | Zeige ein 'MitWidget'
mitWidgetShow :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetShow w = Gtk.widgetShow =<< erhalteWidget w

-- | Verstecke ein 'MitWidget'
mitWidgetHide :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetHide w = Gtk.widgetHide =<< erhalteWidget w

-- ** Label
erzeugeKlasse ["Widget"] "Label"

-- ** Entry
erzeugeKlasse ["Widget"] "Entry"

-- ** Range
erzeugeKlasse ["Widget"] "Range"

erzeugeKlasse ["Widget"] "Box"

-- | Füge ein 'MitWidget' zum Ende einer 'MitBox' hinzu.
mitBoxAppend :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> m ()
mitBoxAppend mitBox mitWidget = do
    box <- erhalteBox mitBox
    widget <- erhalteWidget mitWidget
    Gtk.boxAppend box widget

-- | Füge ein 'MitWidget' zum Anfang einer 'MitBox' hinzu.
mitBoxPrepend :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> m ()
mitBoxPrepend mitBox mitWidget = do
    box <- erhalteBox mitBox
    widget <- erhalteWidget mitWidget
    Gtk.boxPrepend box widget

-- | Entferne ein 'MitWidget' aus einer 'MitBox'.
mitBoxRemove :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> m ()
mitBoxRemove mitBox mitWidget = do
    box <- erhalteBox mitBox
    widget <- erhalteWidget mitWidget
    Gtk.boxRemove box widget

erzeugeKlasse ["Widget"] "Grid"

-- | Füge ein 'MitWidget' zu einem 'MitGrid' hinzu
mitGridAttach
    :: (MonadIO m, MitGrid g, MitWidget w) => g -> w -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
mitGridAttach mitGrid mitWidget left top width height = do
    grid <- erhalteGrid mitGrid
    widget <- erhalteWidget mitWidget
    Gtk.gridAttach grid widget left top width height

-- | Füge ein 'MitWidget' zu einem 'MitGrid' direkt neben einem 'MitWidget' hinzu
mitGridAttachNextTo
    :: (MonadIO m, MitGrid g, MitWidget w, MitWidget sibling)
    => g
    -> w
    -> Maybe sibling
    -> Gtk.PositionType
    -> Int32
    -> Int32
    -> m ()
mitGridAttachNextTo mitGrid mitWidget maybeSibling position width height = do
    grid <- erhalteGrid mitGrid
    widget <- erhalteWidget mitWidget
    maybeSiblingWidget <- case maybeSibling of
        (Just sibling) -> Just <$> erhalteWidget sibling
        Nothing -> pure Nothing
    Gtk.gridAttachNextTo grid widget maybeSiblingWidget position width height

mitGridRemove :: (MonadIO m, MitGrid g, MitWidget w) => g -> w -> m ()
mitGridRemove mitGrid mitWidget = do
    grid <- erhalteGrid mitGrid
    widget <- erhalteWidget mitWidget
    Gtk.gridRemove grid widget

erzeugeKlasse ["Widget"] "Fixed"

-- | Füge ein 'MitWidget' an den spezifizierten Koordinaten zu einem 'MitFixed' hinzu.
mitFixedPut :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Double -> Double -> m ()
mitFixedPut mitFixed mitWidget x y = do
    fixed <- erhalteFixed mitFixed
    widget <- erhalteWidget mitWidget
    Gtk.fixedPut fixed widget x y

-- | Bewege ein 'MitWidget' zu den spezifizierten Koordinaten ein einem 'MitFixed'.
mitFixedMove :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Double -> Double -> m ()
mitFixedMove mitFixed mitWidget x y = do
    fixed <- erhalteFixed mitFixed
    widget <- erhalteWidget mitWidget
    Gtk.fixedMove fixed widget x y

mitFixedRemove :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> m ()
mitFixedRemove mitFixed mitWidget = do
    fixed <- erhalteFixed mitFixed
    widget <- erhalteWidget mitWidget
    Gtk.fixedRemove fixed widget

erzeugeKlasse ["Widget"] "Notebook"

-- | Füge eine neue Seite am Ende eines 'MitNotebook' hinzu
mitNotebookAppendPage :: (MonadIO m, MitNotebook n, MitWidget w) => n -> w -> Text -> m Int32
mitNotebookAppendPage mitNotebook mitWidget name = do
    notebook <- erhalteNotebook mitNotebook
    widget <- erhalteWidget mitWidget
    label <- Gtk.labelNew $ Just name
    Gtk.notebookAppendPage notebook widget $ Just label

-- | Füge eine neue Seite am Anfang eines 'MitNotebook' hinzu
mitNotebookPrependPage :: (MonadIO m, MitNotebook n, MitWidget w) => n -> w -> Text -> m Int32
mitNotebookPrependPage mitNotebook mitWidget name = do
    notebook <- erhalteNotebook mitNotebook
    widget <- erhalteWidget mitWidget
    label <- Gtk.labelNew $ Just name
    Gtk.notebookPrependPage notebook widget $ Just label

-- | Entferne eine Seite aus einem 'MitNotebook' hinzu
mitNotebookRemovePage :: (MonadIO m, MitNotebook n) => n -> Int32 -> m ()
mitNotebookRemovePage mitNotebook page = do
    notebook <- erhalteNotebook mitNotebook
    Gtk.notebookRemovePage notebook page

-- | Setzte die aktuell angezeigte Seite eines 'MitNotebook'
mitNotebookSetCurrentPage :: (MonadIO m, MitNotebook n) => n -> Int32 -> m ()
mitNotebookSetCurrentPage mitNotebook page = do
    notebook <- erhalteNotebook mitNotebook
    Gtk.notebookSetCurrentPage notebook page

erzeugeKlasse ["Widget"] "Paned"

erzeugeKlasse ["Widget"] "ComboBox"

-- * Window
erzeugeKlasse ["Widget"] "Window"

erzeugeKlasse ["Widget", "Window"] "Dialog"

-- * Button
erzeugeKlasse ["Widget"] "Button"

erzeugeKlasse ["Widget", "Button"] "ToggleButton"

erzeugeKlasse ["Widget", "Button", "ToggleButton"] "CheckButton"
