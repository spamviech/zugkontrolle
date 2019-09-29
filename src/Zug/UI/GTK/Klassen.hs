{-# LANGUAGE InstanceSigs #-}
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
module Zug.UI.GTK.Klassen () where
#else
module Zug.UI.GTK.Klassen (
    -- * Widget
    MitWidget(..), mitWidget, mitWidgetShow, mitWidgetHide,
    -- * Container
    MitContainer(..), mitContainer, mitContainerAdd, mitContainerRemove,
    -- ** Box
    MitBox(..), mitBox,
    -- ** Grid
    MitGrid(..), mitGrid, mitGridAttach, mitGridAttachNextTo,
    -- ** Fixed
    MitFixed(..), mitFixed, mitFixedPut, mitFixedMove,
    -- * Window
    MitWindow(..), mitWindow,
    -- ** Dialog
    MitDialog(..), mitDialog) where

import Control.Monad.Trans (MonadIO(..))
import Graphics.UI.Gtk
-- Abhängigkeiten von anderen Modulen
import Zug.UI.GTK.Klassen.TemplateHaskell (erzeugeKlasse)

erzeugeKlasse [] "Widget"

-- | Show the 'Widget' contained in a Type
mitWidgetShow :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetShow = liftIO . mitWidget widgetShow

-- | Hide the 'Widget' contained in a Type
mitWidgetHide :: (MonadIO m, MitWidget w) => w -> m ()
mitWidgetHide = liftIO . mitWidget widgetHide

erzeugeKlasse [''MitWidget] "Container"

-- | Add a 'WidgetClass' to the 'Container' contained in a Type
mitContainerAdd :: (MonadIO m, MitContainer hasC, WidgetClass isW) => hasC -> isW -> m ()
mitContainerAdd hasC isW = liftIO $ mitContainer (flip containerAdd isW) hasC

-- | Remove a 'WidgetClass' from the 'Container' contained in a Type
mitContainerRemove :: (MonadIO m, MitContainer hasC, WidgetClass isW) => hasC -> isW -> m ()
mitContainerRemove hasC isW = liftIO $ mitContainer (flip containerRemove isW) hasC

erzeugeKlasse [''MitContainer] "Box"

erzeugeKlasse [''MitContainer] "Grid"

-- | Add a 'WidgetClass' to the 'Grid' contained in a Type
mitGridAttach :: (MonadIO m, MitGrid hasG, WidgetClass isW) => hasG -> isW -> Int -> Int -> Int -> Int -> m ()
mitGridAttach hasG isW left top width height
    = liftIO $ mitGrid (\grid -> gridAttach grid isW left top width height) hasG

-- | Add a 'WidgetClass' next to the specified 'WidgetClass' in the 'Grid'contained in a Type
mitGridAttachNextTo :: (MonadIO m, MitGrid hasG, WidgetClass isW, WidgetClass sibling)
    => hasG -> isW -> Maybe sibling -> PositionType -> Int -> Int -> m ()
mitGridAttachNextTo hasG isW sibling position width height
    = liftIO $ mitGrid (\grid -> gridAttachNextTo grid isW sibling position width height) hasG

erzeugeKlasse [''MitContainer] "Fixed"

-- | Add a 'WidgetClass' to the specified Coordinates in the 'Fixed' contained in a Type
mitFixedPut :: (MonadIO m, MitFixed hasF, WidgetClass isW) => hasF -> isW -> Int -> Int -> m ()
mitFixedPut hasF isW x y = liftIO $ mitFixed (\fixed -> fixedPut fixed isW (x, y)) hasF

-- | Move a child 'WidgetClass' to the specified Coordinates in the 'Fixed' contained in a Type
mitFixedMove :: (MonadIO m, MitFixed hasF, WidgetClass isW) => hasF -> isW -> Int -> Int -> m ()
mitFixedMove hasF isW x y = liftIO $ mitFixed (\fixed -> fixedMove fixed isW (x, y)) hasF

erzeugeKlasse [''MitContainer] "Window"

erzeugeKlasse [''MitWindow] "Dialog"
#endif