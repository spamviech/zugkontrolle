{-# LANGUAGE DataKinds #-}

{-|
Description: This modules defines all Lego (9V) rails I have access to.
-}
module Zug.UI.Gtk.Gleis.Lego
  ( -- * Gleise
    legoGerade
  , legoKurve
  , legoWeiche
  , legoKreuzung
    -- * Convenience Re-Exports
  , WeichenRichtungAllgemein(Links, Rechts)
  , Zugtyp(Lego)
  ) where

import Zug.Enums (Zugtyp(Lego))
import Zug.UI.Gtk.Gleis.Widget (GleisDefinition(..), WeichenArt(..), WeichenRichtungAllgemein(..)
                              , KreuzungsArt(OhneKurve), alsDreiweg, WeichenRichtung(..))

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
legoLänge :: Double
legoLänge = 12.8

legoRadius :: Double
legoRadius = 320

legoWinkel :: Double
legoWinkel = 22.5

legoGerade :: GleisDefinition 'Lego
legoGerade = Gerade { länge = legoLänge }

legoKurve :: GleisDefinition 'Lego
legoKurve = Kurve { radius = legoRadius, winkel = legoWinkel }

-- TODO stimmt nicht genau, eigentlich eine leichte S-Kurve
{-
Die Geometrie der LEGO Eisenbahnweichen hat sich mit Einführung des 9 V Systems verändert.
Das Parallelgleis nach der Weiche ist nun 8 Noppen vom Hauptgleis entfernt.
Beim 4,5 V/12V System führte das Parallelgleis direkt am Hauptgleis entlang.
-------------------------------------------------------
1 Noppe ist rund 0,8 cm (genauer: 0,79675... cm)
1,00 cm sind rund 1,25 Noppen (genauer: 1,255...) 
-------------------------------------------------------
Nach 1 Gerade/Kurve sind Haupt- und Parallelgleis auf der selben Höhe
-}
legoWeiche :: WeichenRichtungAllgemein 'WeicheZweiweg -> GleisDefinition 'Lego
legoWeiche lr =
    Weiche
    { länge = legoLänge
    , radius = legoRadius
    , winkel = legoWinkel
    , richtung = Normal $ alsDreiweg lr
    }

legoKreuzung :: GleisDefinition 'Lego
legoKreuzung =
    Kreuzung { länge = legoLänge, radius = 6.4, winkel = 90, kreuzungsArt = OhneKurve }
