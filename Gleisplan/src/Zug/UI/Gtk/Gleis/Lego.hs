{-# LANGUAGE DataKinds #-}

{-|
Description: This modules defines all Lego (9V) rails I have access to.
-}
module Zug.UI.Gtk.Gleis.Lego
  ( -- * Gerade
    legoGerade
  , legoGeradeNew
    -- * Convenience Re-Exports
  , Gleis
  , gleisNew
  , WeichenRichtungAllgemein(Links, Rechts)
  , Zugtyp(Lego)
    -- ** Anpassen der Größe
  , gleisScale
  , gleisSetWidth
  , gleisSetHeight
  , gleisRotate
  ) where

import Control.Monad.Trans (MonadIO())

import Zug.UI.Gtk.Gleis.Widget (Gleis, gleisNew, Zugtyp(Lego), GleisDefinition(..), WeichenArt(..)
                              , WeichenRichtungAllgemein(..), alsDreiweg, WeichenRichtung(..)
                              , gleisScale, gleisSetWidth, gleisSetHeight, gleisRotate)

-- https://blaulicht-schiene.jimdofree.com/projekte/lego-daten/
{-
 Der Maßstab liegt zwischen 1:35 - 1:49.
Spurbreite
    Innen: ca. 3,81 cm (38,1 mm)
    Außen: ca. 4,20 cm (42,0 mm)
Gerade (Straight)
    Länge: ca. 17 Noppen / 13,6 cm
    Breite: ca. 8 Noppen / 6,4 cm
    Höhe: ca. 1 1/3 Noppen (1 Stein-Höhe) / 1,0 cm
Kurve (Curve)
    Länge Außen: ca. 17 1/2 Noppen (14 1/3 Legosteine hoch) / 13,7 cm
    Länge Innen: ca. 15 1/6 Noppen (12 2/3 Legosteine hoch) / 12,1 cm
    Breite: 8 Noppen / 6,4 cm
Kreis-Aufbau mit PF-Kurven
Ein Kreis benötigt 16 Lego-PF-Kurven.
    Kreis-Durchmesser Außen: ca. 88 Noppen / 70 cm
    Kreis-Durchmesser Innen: ca. 72 Noppen / 57 cm
    Kreis-Radius (halber Kreis, Außen bis Kreismittelpunkt): ca. 44 Noppen / 35 cm 
-}
{-
Lego Spurweite: 38mm
-}
legoGerade :: GleisDefinition 'Lego
legoGerade = Gerade { länge = 13.6 }

legoGeradeNew :: (MonadIO m) => m (Gleis 'Lego)
legoGeradeNew = gleisNew legoGerade
