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

{-|
Description: Erweiterbare Typklassen angelehnt an "Graphics.UI.Gtk"-Typklassen

Typklassen aus "Graphics.UI.Gtk" lassen eine Instanziierung eigener Typen nicht ohne Probleme zu.
Diese Modul stellt via Template Haskell erstellte alternative Mit-Typklassen um dieses Problem zu umgehen.

Außerdem wird eine /Overlappable/-Standard Instanz für "Graphics.UI.Gtk"-Typen bereitgestellt.
Aufgrund dieser wird in Modulen mit Funktionen, die diese Typklassen verwenden die Verwendung der Spracherweiterung /MonoLocalBinds/ empfohlen.
-}
module Zug.UI.Gtk.Klassen where

#ifdef ZUGKONTROLLEGUI
import qualified Control.Lens as Lens
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk

-- Abhängigkeiten von anderen Modulen
import Zug.Enums (Zugtyp(..), ZugtypEither(..))
import Zug.UI.Gtk.Klassen.TemplateHaskell (erzeugeKlasse)

-- * Widget
erzeugeKlasse [] "Widget"

instance (MitWidget (a 'Märklin), MitWidget (a 'Lego)) => MitWidget (ZugtypEither a) where
    erhalteWidget :: ZugtypEither a -> Gtk.Widget
    erhalteWidget (ZugtypMärklin a) = erhalteWidget a
    erhalteWidget (ZugtypLego a) = erhalteWidget a

-- | Zeige ein 'MitWidget'
mitWidgetShow :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetShow = liftIO . mitWidget Gtk.widgetShow

-- | Verstecke ein 'MitWidget'
mitWidgetHide :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetHide = liftIO . mitWidget Gtk.widgetHide

-- ** Label
erzeugeKlasse [''MitWidget] "Label"

-- ** Entry
erzeugeKlasse [''MitWidget] "Entry"

-- ** Range
erzeugeKlasse [''MitWidget] "Range"

-- * Container
erzeugeKlasse [''MitWidget] "Container"

-- | Füge ein 'MitWidget' zu einem 'MitContainer' hinzu
mitContainerAdd :: (MonadIO m, MitContainer c, MitWidget w) => c -> w -> m ()
mitContainerAdd container widget =
    liftIO $ Gtk.containerAdd (erhalteContainer container) (erhalteWidget widget)

-- | Entferne ein 'MitWidget' aus einem 'MitContainer'
mitContainerRemove :: (MonadIO m, MitContainer c, MitWidget w) => c -> w -> m ()
mitContainerRemove container widget =
    liftIO $ Gtk.containerRemove (erhalteContainer container) (erhalteWidget widget)

erzeugeKlasse [''MitContainer] "Box"

-- | Füge ein 'MitWidget' zum Anfang einer 'MitBox' hinzu
mitBoxPackStart :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Gtk.Packing -> Int -> m ()
mitBoxPackStart box widget packing padding =
    liftIO $ Gtk.boxPackStart (erhalteBox box) (erhalteWidget widget) packing padding

-- | Füge ein 'MitWidget' zum Ende einer 'MitBox' hinzu
mitBoxPackEnd :: (MonadIO m, MitBox b, MitWidget w) => b -> w -> Gtk.Packing -> Int -> m ()
mitBoxPackEnd box widget packing padding =
    liftIO $ Gtk.boxPackEnd (erhalteBox box) (erhalteWidget widget) packing padding

erzeugeKlasse [''MitContainer] "Grid"

-- | Füge ein 'MitWidget' zu einem 'MitGrid' hinzu
mitGridAttach :: (MonadIO m, MitGrid g, MitWidget w) => g -> w -> Int -> Int -> Int -> Int -> m ()
mitGridAttach grid widget left top width height =
    liftIO $ Gtk.gridAttach (erhalteGrid grid) (erhalteWidget widget) left top width height

-- | Füge ein 'MitWidget' zu einem 'MitGrid' direkt neben einem 'MitWidget' hinzu
mitGridAttachNextTo
    :: (MonadIO m, MitGrid g, MitWidget w, MitWidget sibling)
    => g
    -> w
    -> Maybe sibling
    -> Gtk.PositionType
    -> Int
    -> Int
    -> m ()
mitGridAttachNextTo grid widget sibling position width height =
    liftIO
    $ Gtk.gridAttachNextTo
        (erhalteGrid grid)
        (erhalteWidget widget)
        (sibling >>= pure . erhalteWidget)
        position
        width
        height

erzeugeKlasse [''MitContainer] "Fixed"

-- | Füge ein 'MitWidget' an den spezifizierten Koordinaten zu einem 'MitFixed' hinzu
mitFixedPut :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Int -> Int -> m ()
mitFixedPut fixed widget x y =
    liftIO $ Gtk.fixedPut (erhalteFixed fixed) (erhalteWidget widget) (x, y)

-- | Bewege ein 'MitWidget' zu den spezifizierten Koordinaten ein einem 'MitFixed'
mitFixedMove :: (MonadIO m, MitFixed f, MitWidget w) => f -> w -> Int -> Int -> m ()
mitFixedMove fixed widget x y =
    liftIO $ Gtk.fixedMove (erhalteFixed fixed) (erhalteWidget widget) (x, y)

erzeugeKlasse [''MitContainer] "Notebook"

-- | Füge eine neue Seite am Ende eines 'MitNotebook' hinzu
mitNotebookAppendPage :: (MonadIO m, MitNotebook n, MitWidget w) => n -> w -> Text -> m Int
mitNotebookAppendPage notebook widget =
    liftIO . Gtk.notebookAppendPage (erhalteNotebook notebook) (erhalteWidget widget)

-- | Füge eine neue Seite am Anfang eines 'MitNotebook' hinzu
mitNotebookPrependPage :: (MonadIO m, MitNotebook n, MitWidget w) => n -> w -> Text -> m Int
mitNotebookPrependPage notebook widget =
    liftIO . Gtk.notebookPrependPage (erhalteNotebook notebook) (erhalteWidget widget)

-- | Entferne eine Seite aus einem 'MitNotebook' hinzu
mitNotebookRemovePage :: (MonadIO m, MitNotebook n) => n -> Int -> m ()
mitNotebookRemovePage notebook = liftIO . mitNotebook Gtk.notebookRemovePage notebook

-- | Setzte die aktuell angezeigte Seite eines 'MitNotebook'
mitNotebookSetCurrentPage :: (MonadIO m, MitNotebook n) => n -> Int -> m ()
mitNotebookSetCurrentPage notebook = liftIO . mitNotebook Gtk.notebookSetCurrentPage notebook

erzeugeKlasse [''MitContainer] "Paned"

erzeugeKlasse [''MitContainer] "ComboBox"

-- * Window
erzeugeKlasse [''MitContainer] "Window"

erzeugeKlasse [''MitWindow] "Dialog"

-- * Button
erzeugeKlasse [''MitContainer] "Button"

erzeugeKlasse [''MitButton] "ToggleButton"

erzeugeKlasse [''MitToggleButton] "CheckButton"

erzeugeKlasse [''MitCheckButton] "RadioButton"
#endif





