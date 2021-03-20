{-# LANGUAGE DataKinds #-}

{-|
Description: This modules defines all Märklin rails I have access to.
-}
module Zug.UI.Gtk.Gleis.Maerklin
  ( -- * Gerade
    märklinGerade5106
  , märklinGerade5107
  , märklinGerade5129
  , märklinGerade5108
  , märklinGerade5109
  , märklinGerade5110
  , märklinGerade5210
  , märklinGerade5208
    -- * Kurve
  , märklinKurve5120
  , märklinKurve5100
  , märklinKurve5101
  , märklinKurve5102
  , märklinKurve5200
  , märklinKurve5206
  , märklinKurve5201
  , märklinKurve5205
    -- * Weiche
  , märklinWeiche5117
  , märklinWeiche5137
  , märklinWeiche5202
    -- * Dreiwegeweiche
  , märklinDreiwegWeiche5214
    -- * Kurven-Weiche
  , märklinKurvenWeiche5140
    -- * Kreuzung
  , märklinKreuzung5128
  , märklinKreuzung5207
    -- * Convenience Re-Exports
  , WeichenRichtungGerade(Links, Rechts)
  , WeichenRichtungGebogen(GLinks, GRechts)
  , Zugtyp(Märklin)
  ) where

import Zug.Enums (Zugtyp(Märklin))
import Zug.UI.Gtk.Gleis.Widget
       (GleisDefinition(..), WeichenRichtungGerade(..), WeichenRichtungGebogen(..)
      , KreuzungsArt(MitKurve), WeichenRichtung(..))

märklinRIndustrie :: Double
märklinRIndustrie = 286

märklinR1 :: Double
märklinR1 = 360

märklinR2 :: Double
märklinR2 = 437.4

{-
H0 Spurweite: 16.5mm
Gerade
    5106: L180mm
    5107: L90mm
    5129: L70mm
    5108: L45mm
    5109: L33.5mm
    5110: L22.5mm
    5210: L16mm
    5208: L8mm
-}
märklinGerade5106 :: GleisDefinition 'Märklin
märklinGerade5106 = Gerade { länge = 180 }

märklinGerade5107 :: GleisDefinition 'Märklin
märklinGerade5107 = Gerade { länge = 90 }

märklinGerade5129 :: GleisDefinition 'Märklin
märklinGerade5129 = Gerade { länge = 70 }

märklinGerade5108 :: GleisDefinition 'Märklin
märklinGerade5108 = Gerade { länge = 45 }

märklinGerade5109 :: GleisDefinition 'Märklin
märklinGerade5109 = Gerade { länge = 33.5 }

märklinGerade5110 :: GleisDefinition 'Märklin
märklinGerade5110 = Gerade { länge = 22.5 }

märklinGerade5210 :: GleisDefinition 'Märklin
märklinGerade5210 = Gerade { länge = 16 }

märklinGerade5208 :: GleisDefinition 'Märklin
märklinGerade5208 = Gerade { länge = 8 }

{-
Kurve
    5120: 45°, R286mm
    5100: 30°, R360mm
    5101: 15°, R360mm
    5102: 7.5°, R360mm
    5200: 30°, R437.4mm
    5206: 24.28°, R437.4mm
    5201: 15°, R437.4mm
    5205: 5.72°, R437.4mm
-}
märklinKurve5120 :: GleisDefinition 'Märklin
märklinKurve5120 = Kurve märklinRIndustrie 45

märklinKurve5100 :: GleisDefinition 'Märklin
märklinKurve5100 = Kurve märklinR1 30

märklinKurve5101 :: GleisDefinition 'Märklin
märklinKurve5101 = Kurve märklinR1 15

märklinKurve5102 :: GleisDefinition 'Märklin
märklinKurve5102 = Kurve märklinR1 7.5

märklinKurve5200 :: GleisDefinition 'Märklin
märklinKurve5200 = Kurve märklinR2 30

märklinKurve5206 :: GleisDefinition 'Märklin
märklinKurve5206 = Kurve märklinR2 24.28

märklinKurve5201 :: GleisDefinition 'Märklin
märklinKurve5201 = Kurve märklinR2 15

märklinKurve5205 :: GleisDefinition 'Märklin
märklinKurve5205 = Kurve märklinR2 5.72

{-
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
    5214 (3-Weg): L180mm, 24,28°, R437.4mm
-}
märklinWeiche5117 :: WeichenRichtungGerade -> GleisDefinition 'Märklin
märklinWeiche5117
    lr = Weiche { länge = 180, radius = märklinR1, winkel = 30, richtung = Normal lr }

märklinWeiche5137 :: WeichenRichtungGerade -> GleisDefinition 'Märklin
märklinWeiche5137
    lr = Weiche { länge = 180, radius = märklinR1, winkel = 22.5, richtung = Normal lr }

märklinWeiche5202 :: WeichenRichtungGerade -> GleisDefinition 'Märklin
märklinWeiche5202
    lr = Weiche { länge = 180, radius = märklinR1, winkel = 24.28, richtung = Normal lr }

märklinDreiwegWeiche5214 :: GleisDefinition 'Märklin
märklinDreiwegWeiche5214 =
    Weiche { länge = 180, radius = märklinR1, winkel = 24.28, richtung = Normal Dreiwege }

{-
Kurven-Weiche
    5140 L/R: 30°, Rin360mm, Rout360mm @ 77.4mm (Gerade vor Bogen)
-}
märklinKurvenWeiche5140 :: WeichenRichtungGebogen -> GleisDefinition 'Märklin
märklinKurvenWeiche5140
    lr = Weiche { länge = 77.4, radius = märklinR1, winkel = 30, richtung = Gebogen lr }

{-
Kreuzung
    5128: L193mm, 30°, R360mm
    5207: L180mm, 24.28°, R437.4mm
-}
märklinKreuzung5128 :: GleisDefinition 'Märklin
märklinKreuzung5128 =
    Kreuzung { länge = 193, radius = märklinR1, winkel = 30, kreuzungsArt = MitKurve }

-- Länge/Winkel passt nicht!
-- https://www.stummiforum.de/viewtopic.php?t=29741#p309938
märklinKreuzung5207 :: GleisDefinition 'Märklin
märklinKreuzung5207 =
    Kreuzung { länge = 180, radius = märklinR2, winkel = 23.254, kreuzungsArt = MitKurve }
-- TODO
{-
Prellbock:
    7190: 70mm
Kupplungsgleis:
    5112 U: 90mm
-}
