{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
#endif

-- {-# OPTIONS_GHC -ddump-splices #-}
{-|
Description: Erweiterbare Typklassen angelehnt an "GI.Gtk"-Typklassen.

Typklassen aus "GI.Gtk" lassen eine Instanziierung eigener Typen nicht ohne Probleme zu.
Diese Modul stellt via Template Haskell erstellte alternative Mit-Typklassen um dieses Problem zu umgehen.

Außerdem wird eine /Overlappable/-Standard Instanz für "GI.Gtk"-Typen bereitgestellt.
Aufgrund dieser wird in Modulen mit Funktionen, die diese Typklassen verwenden die Verwendung der Spracherweiterung /MonoLocalBinds/ empfohlen.
-}
module Zug.UI.Gtk.Klassen where

#ifdef ZUGKONTROLLEGUI
import Control.Monad.Trans (MonadIO(..))
import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word32)
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
mitWidgetShow w = liftIO . Gtk.widgetShow =<< erhalteWidget w

-- | Verstecke ein 'MitWidget'
mitWidgetHide :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetHide w = liftIO . Gtk.widgetHide =<< erhalteWidget w

-- ** Label
erzeugeKlasse ["Widget"] "Label"

-- ** Entry
erzeugeKlasse ["Widget"] "Entry"

-- ** Range
erzeugeKlasse ["Widget"] "Range"

-- * Container
erzeugeKlasse ["Widget"] "Container"

-- | Füge ein 'MitWidget' zu einem 'MitContainer' hinzu
mitContainerAdd :: (MonadIO m, MitContainer c, MitWidget w) => c -> w -> m ()
mitContainerAdd mitContainer mitWidget = liftIO $ do
    container <- erhalteContainer mitContainer
    widget <- erhalteWidget mitWidget
    Gtk.containerAdd container widget

-- | Entferne ein 'MitWidget' aus einem 'MitContainer'
mitContainerRemove :: (MonadIO m, MitContainer c, MitWidget w) => c -> w -> m ()
mitContainerRemove mitContainer mitWidget = liftIO $ do
    container <- erhalteContainer mitContainer
    widget <- erhalteWidget mitWidget
    Gtk.containerRemove container widget

erzeugeKlasse ["Widget", "Container"] "Box"

-- | Füge ein 'MitWidget' zum Anfang einer 'MitBox' hinzu
mitBoxPackStart :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Bool -> Bool -> Word32 -> m ()
mitBoxPackStart mitBox mitWidget expand fill padding = liftIO $ do
    box <- erhalteBox mitBox
    widget <- erhalteWidget mitWidget
    Gtk.boxPackStart box widget expand fill padding

-- | Füge ein 'MitWidget' zum Ende einer 'MitBox' hinzu
mitBoxPackEnd :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Bool -> Bool -> Word32 -> m ()
mitBoxPackEnd mitBox mitWidget expand fill padding = liftIO $ do
    box <- erhalteBox mitBox
    widget <- erhalteWidget mitWidget
    Gtk.boxPackEnd box widget expand fill padding

erzeugeKlasse ["Widget", "Container"] "Grid"

-- | Füge ein 'MitWidget' zu einem 'MitGrid' hinzu
mitGridAttach
    :: (MonadIO m, MitGrid g, MitWidget w) => g -> w -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
mitGridAttach mitGrid mitWidget left top width height = liftIO $ do
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
mitGridAttachNextTo mitGrid mitWidget maybeSibling position width height = liftIO $ do
    grid <- erhalteGrid mitGrid
    widget <- erhalteWidget mitWidget
    maybeSiblingWidget <- case maybeSibling of
        (Just sibling) -> Just <$> erhalteWidget sibling
        Nothing -> pure Nothing
    Gtk.gridAttachNextTo grid widget maybeSiblingWidget position width height

erzeugeKlasse ["Widget", "Container"] "Fixed"

-- | Füge ein 'MitWidget' an den spezifizierten Koordinaten zu einem 'MitFixed' hinzu
mitFixedPut :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Int32 -> Int32 -> m ()
mitFixedPut mitFixed mitWidget x y = liftIO $ do
    fixed <- erhalteFixed mitFixed
    widget <- erhalteWidget mitWidget
    Gtk.fixedPut fixed widget x y

-- | Bewege ein 'MitWidget' zu den spezifizierten Koordinaten ein einem 'MitFixed'
mitFixedMove :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Int32 -> Int32 -> m ()
mitFixedMove mitFixed mitWidget x y = liftIO $ do
    fixed <- erhalteFixed mitFixed
    widget <- erhalteWidget mitWidget
    Gtk.fixedMove fixed widget x y

erzeugeKlasse ["Widget", "Container"] "Notebook"

-- | Füge eine neue Seite am Ende eines 'MitNotebook' hinzu
mitNotebookAppendPage :: (MonadIO m, MitNotebook n, MitWidget w) => n -> w -> Text -> m Int32
mitNotebookAppendPage mitNotebook mitWidget name = liftIO $ do
    notebook <- erhalteNotebook mitNotebook
    widget <- erhalteWidget mitWidget
    label <- Gtk.labelNew $ Just name
    Gtk.notebookAppendPage notebook widget $ Just label

-- | Füge eine neue Seite am Anfang eines 'MitNotebook' hinzu
mitNotebookPrependPage :: (MonadIO m, MitNotebook n, MitWidget w) => n -> w -> Text -> m Int32
mitNotebookPrependPage mitNotebook mitWidget name = liftIO $ do
    notebook <- erhalteNotebook mitNotebook
    widget <- erhalteWidget mitWidget
    label <- Gtk.labelNew $ Just name
    Gtk.notebookPrependPage notebook widget $ Just label

-- | Entferne eine Seite aus einem 'MitNotebook' hinzu
mitNotebookRemovePage :: (MonadIO m, MitNotebook n) => n -> Int32 -> m ()
mitNotebookRemovePage mitNotebook page = liftIO $ do
    notebook <- erhalteNotebook mitNotebook
    Gtk.notebookRemovePage notebook page

-- | Setzte die aktuell angezeigte Seite eines 'MitNotebook'
mitNotebookSetCurrentPage :: (MonadIO m, MitNotebook n) => n -> Int32 -> m ()
mitNotebookSetCurrentPage mitNotebook page = liftIO $ do
    notebook <- erhalteNotebook mitNotebook
    Gtk.notebookSetCurrentPage notebook page

erzeugeKlasse ["Widget", "Container"] "Paned"

erzeugeKlasse ["Widget", "Container"] "ComboBox"

-- * Window
erzeugeKlasse ["Widget", "Container"] "Window"

erzeugeKlasse ["Widget", "Container", "Window"] "Dialog"

-- * Button
erzeugeKlasse ["Widget", "Container"] "Button"

erzeugeKlasse ["Widget", "Container", "Button"] "ToggleButton"

erzeugeKlasse ["Widget", "Container", "Button", "ToggleButton"] "CheckButton"

erzeugeKlasse ["Widget", "Container", "Button", "ToggleButton", "CheckButton"] "RadioButton"
#endif
--
