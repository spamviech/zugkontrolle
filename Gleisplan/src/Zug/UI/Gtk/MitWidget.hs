{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Description: Erweiterbare Typklassen angelehnt an "GI.Gtk"-Typklassen.

Typklassen aus "GI.Gtk" lassen eine Instanziierung eigener Typen nicht ohne Probleme zu.
Diese Modul stellt via Template Haskell erstellte alternative Mit-Typklassen um dieses Problem zu umgehen.

Außerdem wird eine /Overlappable/-Standard Instanz für "GI.Gtk"-Typen bereitgestellt.
Aufgrund dieser wird in Modulen mit Funktionen, die diese Typklassen verwenden die Verwendung der Spracherweiterung /MonoLocalBinds/ empfohlen.
-}
module Zug.UI.Gtk.MitWidget (MitWidget(erhalteWidget), mitWidgetShow, mitWidgetHide) where

import Control.Monad.Trans (MonadIO())
import qualified GI.Gtk as Gtk

import Zug.Enums (Zugtyp(..), ZugtypEither(..))

class MitWidget w where
    erhalteWidget :: (MonadIO m) => w -> m Gtk.Widget

instance {-# OVERLAPPABLE #-} (Gtk.IsWidget w) => MitWidget w where
    erhalteWidget :: (MonadIO m) => w -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget

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
