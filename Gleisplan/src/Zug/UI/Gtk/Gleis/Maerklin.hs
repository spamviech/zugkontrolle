{-# LANGUAGE DataKinds #-}

{-|
Description: This modules defines all Märklin rails I have access to.
-}
module Zug.UI.Gtk.Gleis.Maerklin
  ( -- * Gerade
    -- ** Definitionen
    märklinGerade5106
  , märklinGerade5107
  , märklinGerade5129
  , märklinGerade5108
  , märklinGerade5109
  , märklinGerade5110
  , märklinGerade5210
  , märklinGerade5208
    -- ** Konstruktoren
  , märklinGerade5106New
  , märklinGerade5107New
  , märklinGerade5129New
  , märklinGerade5108New
  , märklinGerade5109New
  , märklinGerade5110New
  , märklinGerade5210New
  , märklinGerade5208New
    -- * Kurve
    -- ** Definitionen
  , märklinKurve5120
  , märklinKurve5100
  , märklinKurve5101
  , märklinKurve5102
  , märklinKurve5200
  , märklinKurve5206
  , märklinKurve5201
  , märklinKurve5205
    -- ** Konstruktoren
  , märklinKurve5120New
  , märklinKurve5100New
  , märklinKurve5101New
  , märklinKurve5102New
  , märklinKurve5200New
  , märklinKurve5206New
  , märklinKurve5201New
  , märklinKurve5205New
    -- * Weiche
    -- ** Definitionen
  , märklinWeiche5117
  , märklinWeiche5137
  , märklinWeiche5202
    -- ** Konstruktoren
  , märklinWeicheRechts5117New
  , märklinWeicheLinks5117New
  , märklinWeicheRechts5137New
  , märklinWeicheLinks5137New
  , märklinWeicheRechts5202New
  , märklinWeicheLinks5202New
    -- * Dreiwegeweiche
    -- ** Definitionen
  , märklinDreiwegWeiche5214
    -- ** Konstruktoren
  , märklinDreiwegWeiche5214New
    -- * Kurven-Weiche
    -- ** Definitionen
  , märklinKurvenWeiche5140
    -- ** Konstruktoren
  , märklinKurvenWeicheRechts5140New
  , märklinKurvenWeicheLinks5140New
    -- * Kreuzung
    -- ** Definitionen
  , märklinKreuzung5128
  , märklinKreuzung5207
    -- ** Konstruktoren
  , märklinKreuzung5128New
  , märklinKreuzung5207New
    -- * Convenience Re-Exports
  , Gleis
  , unsafeGleisNew
  , WeichenRichtungAllgemein(Links, Rechts)
  , Zugtyp(Märklin)
  ) where

import Control.Monad.Trans (MonadIO())

import Zug.Enums (Zugtyp(Märklin))
import Zug.UI.Gtk.Gleis.Kurve (KurveErgebnis(KurveErgebnis))
import Zug.UI.Gtk.Gleis.Widget
       (Gleis, gleisNew, GleisDefinition(..), WeichenArt(..), WeichenRichtungAllgemein(..)
      , KreuzungsArt(MitKurve), alsDreiweg, WeichenRichtung(..))

unsafeGleisNew :: (MonadIO m) => GleisDefinition 'Märklin -> m (Gleis 'Märklin)
unsafeGleisNew definition =
    let (KurveErgebnis gleis) = gleisNew definition
    in gleis

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

märklinGerade5106New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5106New = unsafeGleisNew märklinGerade5106

märklinGerade5107 :: GleisDefinition 'Märklin
märklinGerade5107 = Gerade { länge = 90 }

märklinGerade5107New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5107New = unsafeGleisNew märklinGerade5107

märklinGerade5129 :: GleisDefinition 'Märklin
märklinGerade5129 = Gerade { länge = 70 }

märklinGerade5129New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5129New = unsafeGleisNew märklinGerade5129

märklinGerade5108 :: GleisDefinition 'Märklin
märklinGerade5108 = Gerade { länge = 45 }

märklinGerade5108New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5108New = unsafeGleisNew märklinGerade5108

märklinGerade5109 :: GleisDefinition 'Märklin
märklinGerade5109 = Gerade { länge = 33.5 }

märklinGerade5109New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5109New = unsafeGleisNew märklinGerade5109

märklinGerade5110 :: GleisDefinition 'Märklin
märklinGerade5110 = Gerade { länge = 22.5 }

märklinGerade5110New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5110New = unsafeGleisNew märklinGerade5110

märklinGerade5210 :: GleisDefinition 'Märklin
märklinGerade5210 = Gerade { länge = 16 }

märklinGerade5210New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5210New = unsafeGleisNew märklinGerade5210

märklinGerade5208 :: GleisDefinition 'Märklin
märklinGerade5208 = Gerade { länge = 8 }

märklinGerade5208New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5208New = unsafeGleisNew märklinGerade5208

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

märklinKurve5120New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5120New = unsafeGleisNew märklinKurve5120

märklinKurve5100 :: GleisDefinition 'Märklin
märklinKurve5100 = Kurve märklinR1 30

märklinKurve5100New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5100New = unsafeGleisNew märklinKurve5100

märklinKurve5101 :: GleisDefinition 'Märklin
märklinKurve5101 = Kurve märklinR1 15

märklinKurve5101New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5101New = unsafeGleisNew märklinKurve5101

märklinKurve5102 :: GleisDefinition 'Märklin
märklinKurve5102 = Kurve märklinR1 7.5

märklinKurve5102New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5102New = unsafeGleisNew märklinKurve5102

märklinKurve5200 :: GleisDefinition 'Märklin
märklinKurve5200 = Kurve märklinR2 30

märklinKurve5200New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5200New = unsafeGleisNew märklinKurve5200

märklinKurve5206 :: GleisDefinition 'Märklin
märklinKurve5206 = Kurve märklinR2 24.28

märklinKurve5206New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5206New = unsafeGleisNew märklinKurve5206

märklinKurve5201 :: GleisDefinition 'Märklin
märklinKurve5201 = Kurve märklinR2 15

märklinKurve5201New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5201New = unsafeGleisNew märklinKurve5201

märklinKurve5205 :: GleisDefinition 'Märklin
märklinKurve5205 = Kurve märklinR2 5.72

märklinKurve5205New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5205New = unsafeGleisNew märklinKurve5205

{-
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
    5214 (3-Weg): L180mm, 24,28°, R437.4mm
-}
märklinWeiche5117 :: WeichenRichtungAllgemein 'WeicheZweiweg -> GleisDefinition 'Märklin
märklinWeiche5117 lr =
    Weiche { länge = 180, radius = märklinR1, winkel = 30, richtung = Normal $ alsDreiweg lr }

märklinWeicheRechts5117New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheRechts5117New = unsafeGleisNew $ märklinWeiche5117 Rechts

märklinWeicheLinks5117New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheLinks5117New = unsafeGleisNew $ märklinWeiche5117 Links

märklinWeiche5137 :: WeichenRichtungAllgemein 'WeicheZweiweg -> GleisDefinition 'Märklin
märklinWeiche5137 lr =
    Weiche { länge = 180, radius = märklinR1, winkel = 22.5, richtung = Normal $ alsDreiweg lr }

märklinWeicheRechts5137New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheRechts5137New = unsafeGleisNew $ märklinWeiche5137 Rechts

märklinWeicheLinks5137New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheLinks5137New = unsafeGleisNew $ märklinWeiche5137 Links

märklinWeiche5202 :: WeichenRichtungAllgemein 'WeicheZweiweg -> GleisDefinition 'Märklin
märklinWeiche5202 lr =
    Weiche { länge = 180, radius = märklinR1, winkel = 24.28, richtung = Normal $ alsDreiweg lr }

märklinWeicheRechts5202New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheRechts5202New = unsafeGleisNew $ märklinWeiche5202 Rechts

märklinWeicheLinks5202New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheLinks5202New = unsafeGleisNew $ märklinWeiche5202 Links

märklinDreiwegWeiche5214 :: GleisDefinition 'Märklin
märklinDreiwegWeiche5214 =
    Weiche { länge = 180, radius = märklinR1, winkel = 24.28, richtung = Normal Dreiwege }

märklinDreiwegWeiche5214New :: (MonadIO m) => m (Gleis 'Märklin)
märklinDreiwegWeiche5214New = unsafeGleisNew märklinDreiwegWeiche5214

{-
Kurven-Weiche
    5140 L/R: 30°, Rin360mm, Rout360mm @ 77.4mm (Gerade vor Bogen)
-}
märklinKurvenWeiche5140 :: WeichenRichtungAllgemein 'WeicheZweiweg -> GleisDefinition 'Märklin
märklinKurvenWeiche5140
    lr = Weiche { länge = 77.4, radius = märklinR1, winkel = 30, richtung = Gebogen lr }

märklinKurvenWeicheRechts5140New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurvenWeicheRechts5140New = unsafeGleisNew $ märklinKurvenWeiche5140 Rechts

märklinKurvenWeicheLinks5140New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurvenWeicheLinks5140New = unsafeGleisNew $ märklinKurvenWeiche5140 Links

{-
Kreuzung
    5128: L193mm, 30°, R360mm
    5207: L180mm, 24.28°, R437.4mm
-}
märklinKreuzung5128 :: GleisDefinition 'Märklin
märklinKreuzung5128 =
    Kreuzung { länge = 193, radius = märklinR1, winkel = 30, kreuzungsArt = MitKurve }

märklinKreuzung5128New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKreuzung5128New = unsafeGleisNew märklinKreuzung5128

--TODO Länge/Winkel passt nicht!
-- https://www.stummiforum.de/viewtopic.php?t=29741#p309938
märklinKreuzung5207 :: GleisDefinition 'Märklin
märklinKreuzung5207 =
    Kreuzung { länge = 180, radius = märklinR2, winkel = 23.254, kreuzungsArt = MitKurve }

märklinKreuzung5207New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKreuzung5207New = unsafeGleisNew märklinKreuzung5207
-- TODO
{-
Prellbock:
    7190: 70mm
Kupplungsgleis:
    5112 U: 90mm
-}
