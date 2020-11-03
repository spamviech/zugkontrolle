{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
#endif

module Zug.UI.Gtk.Gleise
  (
#ifdef ZUGKONTROLLEGUI
    -- * Gleis Widgets
    Gleis()
    -- ** Anpassen der Größe
  , gleisScale
  , gleisSetWidth
  , gleisSetHeight
  , gleisRotate
    -- * Konstruktoren
  , geradeNew
  , kurveNew
  , gleisAnzeigeNew
    -- ** Märklin H0 (M-Gleise)
    -- *** Gerade
  , märklinGerade5106New
  , märklinGerade5107New
  , märklinGerade5129New
  , märklinGerade5108New
  , märklinGerade5109New
  , märklinGerade5110New
  , märklinGerade5210New
  , märklinGerade5208New
    -- *** Kurve
  , märklinKurve5120New
  , märklinKurve5100New
  , märklinKurve5101New
  , märklinKurve5102New
  , märklinKurve5200New
  , märklinKurve5206New
  , märklinKurve5201New
  , märklinKurve5205New
    -- *** Weiche
  , märklinWeicheRechts5117New
  , märklinWeicheLinks5117New
  , märklinWeicheRechts5137New
  , märklinWeicheLinks5137New
  , märklinWeicheRechts5202New
  , märklinWeicheLinks5202New
  , märklinKurvenWeicheRechts5140New
  , märklinKurvenWeicheLinks5140New
    -- ** Lego (9V Gleise)
  , legoGeradeNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (foldM, when)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Int (Int32)
import Data.Text (Text)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import GI.Cairo.Render.Matrix (Matrix(Matrix))
import qualified GI.Gtk as Gtk

import Zug.Enums (Zugtyp(..))
import Zug.UI.Gtk.Hilfsfunktionen (fixedPutWidgetNew)
import Zug.UI.Gtk.Klassen (MitWidget(..))

-- | 'Gtk.Widget' von einem Gleis.
-- Die Größe wird nur über 'gleisScale', 'gleisSetWidth' und 'gleisSetHeight' verändert.
data Gleis (z :: Zugtyp) =
    Gleis
    { drawingArea :: Gtk.DrawingArea
    , width :: Int32
    , height :: Int32
    , tvarScale :: TVar Double
    , tvarAngle :: TVar Double
    }

instance MitWidget (Gleis z) where
    erhalteWidget :: (MonadIO m) => Gleis z -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . drawingArea

gleisAdjustSizeRequest :: (MonadIO m) => Gleis z -> m ()
gleisAdjustSizeRequest Gleis {drawingArea, width, height, tvarScale, tvarAngle} = do
    (scale, angle) <- liftIO $ (,) <$> readTVarIO tvarScale <*> readTVarIO tvarAngle
    let newWidth = scale * (fromIntegral width)
        newHeight = scale * (fromIntegral height)
        adjustedWidth = ceiling $ abs (newWidth * cos angle) + abs (newHeight * sin angle)
        adjustedHeight = ceiling $ abs (newHeight * cos angle) + abs (newWidth * sin angle)
    Gtk.widgetSetSizeRequest drawingArea adjustedWidth adjustedHeight

-- | Skaliere das 'Gleis' mit dem angegebenen Faktor.
gleisScale :: (MonadIO m) => Gleis z -> Double -> m ()
gleisScale gleis@Gleis {tvarScale} scale = do
    liftIO $ atomically $ writeTVar tvarScale scale
    gleisAdjustSizeRequest gleis

-- | Ändere die Breite des 'Gleis'es zum angegebenen Wert.
-- Die Höhe wird bei konstantem Längenverhältnis angepasst.
gleisSetWidth :: (MonadIO m) => Gleis z -> Int32 -> m ()
gleisSetWidth gleis@Gleis {width} newWidth =
    gleisScale gleis $ fromIntegral newWidth / fromIntegral width

-- | Ändere die Höhe des 'Gleis'es zum angegebenen Wert.
-- Die Breite wird bei konstantem Längenverhältnis angepasst.
gleisSetHeight :: (MonadIO m) => Gleis z -> Int32 -> m ()
gleisSetHeight gleis@Gleis {height} newHeight =
    gleisScale gleis $ fromIntegral newHeight / fromIntegral height

-- | Rotation um den angegebenen /winkel/ im Gradmaß.
-- Die Rotation ist im Uhrzeigersinn (siehe 'Cairo.rotate').
gleisRotate :: (MonadIO m) => Gleis z -> Double -> m ()
gleisRotate gleis@Gleis {tvarAngle} angle = do
    liftIO $ atomically $ writeTVar tvarAngle angle
    gleisAdjustSizeRequest gleis

-- | Create a new 'Gtk.DrawingArea' with a fixed size set up with the specified 'Cairo.Render' /draw/ path.
--
-- 'Cairo.setLineWidth' 1 is called before the /draw/ action is executed.
-- After the action 'Cairo.stroke' is executed.
gleisNew :: (MonadIO m)
         => (Gleis z -> Int32)
         -> (Gleis z -> Int32)
         -> (Gleis z -> Cairo.Render ())
         -> m (Gleis z)
gleisNew widthFn heightFn draw = do
    drawingArea <- Gtk.drawingAreaNew
    (tvarScale, tvarAngle) <- liftIO $ (,) <$> newTVarIO 1 <*> newTVarIO 0
    let gleis = Gleis { drawingArea, width, height, tvarScale, tvarAngle }
        width = widthFn gleis
        height = heightFn gleis
    Gtk.widgetSetHexpand drawingArea False
    Gtk.widgetSetHalign drawingArea Gtk.AlignStart
    Gtk.widgetSetVexpand drawingArea False
    Gtk.widgetSetValign drawingArea Gtk.AlignStart
    gleisScale gleis 1
    Gtk.onWidgetDraw drawingArea $ Cairo.renderWithContext $ do
        (scale, angle) <- liftIO $ (,) <$> readTVarIO tvarScale <*> readTVarIO tvarAngle
        -- debugging
        newWidth <- Gtk.widgetGetAllocatedWidth drawingArea
        newHeight <- Gtk.widgetGetAllocatedHeight drawingArea
        -- let scale =
        --         min
        --             (fromIntegral newWidth / fromIntegral width)
        --             (fromIntegral newHeight / fromIntegral height)
        -- liftIO
        --     $ putStrLn
        --     $ show newWidth ++ ", " ++ show newHeight ++ " | " ++ show scale ++ ", " ++ show angle
        -- end debugging
        Cairo.save
        let halfWidth = (0.5 * fromIntegral newWidth)
            halfHeight = (0.5 * fromIntegral newHeight)
        Cairo.translate halfWidth halfHeight
        Cairo.rotate angle
        Cairo.scale scale scale
        Cairo.translate (-0.5 * fromIntegral width) (-0.5 * fromIntegral height)
        Cairo.setLineWidth 1
        Cairo.newPath
        draw gleis
        Cairo.stroke
        Cairo.restore
        pure True
    pure gleis

class Spurweite z where
    spurweite :: Gleis z -> Double

instance Spurweite 'Märklin where
    spurweite :: Gleis 'Märklin -> Double
    spurweite = const 16.5

instance Spurweite 'Lego where
    spurweite :: Gleis 'Lego -> Double
    spurweite = const 38

abstand :: (Spurweite z) => Gleis z -> Double
abstand gleis = spurweite gleis / 3

beschränkung :: (Spurweite z) => Gleis z -> Double
beschränkung gleis = spurweite gleis + 2 * abstand gleis

-- Märklin verwendet mittleren Kurvenradius
-- http://www.modellbau-wiki.de/wiki/Gleisradius
radiusBegrenzung :: (Spurweite z) => Double -> Gleis z -> Double
radiusBegrenzung radius gleis = radius + 0.5 * spurweite gleis + abstand gleis

widthKurve :: (Spurweite z) => Double -> Double -> Gleis z -> Int32
widthKurve radius winkelBogenmaß gleis
    | winkelBogenmaß < 0.5 * pi = ceiling $ radiusBegrenzung radius gleis * sin winkelBogenmaß
    | otherwise = error "Nur Kurven mit Winkel < pi/2 (90°) sind unterstützt."

heightKurve :: (Spurweite z) => Double -> Double -> Gleis z -> Int32
heightKurve radius winkelBogenmaß gleis
    | winkelBogenmaß < 0.5 * pi =
        ceiling
        $ radiusBegrenzung radius gleis * (1 - cos winkelBogenmaß)
        + beschränkung gleis * cos winkelBogenmaß
    | otherwise = error "Nur Kurven mit Winkel < pi/2 (90°) sind unterstützt."

widthWeiche :: (Spurweite z) => Double -> Double -> Double -> Gleis z -> Int32
widthWeiche länge radius winkelBogenmaß gleis =
    max (ceiling länge) $ widthKurve radius winkelBogenmaß gleis

heightWeiche :: (Spurweite z) => Double -> Double -> Gleis z -> Int32
heightWeiche radius winkelBogenmaß gleis =
    max (ceiling $ beschränkung gleis) $ heightKurve radius winkelBogenmaß gleis

-- | Erzeuge eine neues gerades 'Gleis' der angegebenen Länge.
geradeNew :: (MonadIO m, Spurweite z) => Double -> m (Gleis z)
geradeNew länge =
    gleisNew (const $ ceiling länge) (ceiling . beschränkung) $ zeichneGerade länge

-- | Pfad zum Zeichnen einer Geraden der angegebenen Länge.
zeichneGerade :: (Spurweite z) => Double -> Gleis z -> Cairo.Render ()
zeichneGerade länge gleis = do
    -- Beschränkungen
    Cairo.moveTo 0 0
    Cairo.lineTo 0 $ beschränkung gleis
    Cairo.moveTo länge 0
    Cairo.lineTo länge $ beschränkung gleis
    -- Gleis
    Cairo.moveTo 0 gleisOben
    Cairo.lineTo länge gleisOben
    Cairo.moveTo 0 gleisUnten
    Cairo.lineTo länge gleisUnten
    where
        gleisOben :: Double
        gleisOben = abstand gleis

        gleisUnten :: Double
        gleisUnten = beschränkung gleis - abstand gleis

-- | Erzeuge eine neue Kurve mit angegebenen Radius und Winkel im Gradmaß.
kurveNew :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> m (Gleis z)
kurveNew radius winkel =
    gleisNew (widthKurve radius winkelBogenmaß) (heightKurve radius winkelBogenmaß)
    $ zeichneKurve radius winkelBogenmaß True
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Pfad zum Zeichnen einer Kurve mit angegebenen Kurvenradius und Winkel im Bogenmaß.
zeichneKurve :: (Spurweite z) => Double -> Double -> Bool -> Gleis z -> Cairo.Render ()
zeichneKurve radius winkel anfangsBeschränkung gleis = do
    -- Beschränkungen
    when anfangsBeschränkung $ do
        Cairo.moveTo 0 0
        Cairo.lineTo 0 $ beschränkung gleis
    Cairo.moveTo begrenzungX0 begrenzungY0
    Cairo.lineTo begrenzungX1 begrenzungY1
    Cairo.stroke
    -- Gleis
    Cairo.arc 0 bogenZentrumY radiusAußen anfangsWinkel (anfangsWinkel + winkel)
    Cairo.stroke
    Cairo.arc 0 bogenZentrumY radiusInnen anfangsWinkel (anfangsWinkel + winkel)
    where
        begrenzungX0 :: Double
        begrenzungX0 = radiusBegrenzungAußen * sin winkel

        begrenzungY0 :: Double
        begrenzungY0 = radiusBegrenzungAußen * (1 - cos winkel)

        begrenzungX1 :: Double
        begrenzungX1 = begrenzungX0 - beschränkung gleis * sin winkel

        begrenzungY1 :: Double
        begrenzungY1 = begrenzungY0 + beschränkung gleis * cos winkel

        bogenZentrumY :: Double
        bogenZentrumY = abstand gleis + radiusAußen

        anfangsWinkel :: Double
        anfangsWinkel = 3 * pi / 2

        radiusInnen :: Double
        radiusInnen = radius - 0.5 * spurweite gleis

        radiusAußen :: Double
        radiusAußen = radius + 0.5 * spurweite gleis

        radiusBegrenzungAußen :: Double
        radiusBegrenzungAußen = radiusAußen + abstand gleis

weicheRechtsNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
weicheRechtsNew länge radius winkel =
    gleisNew (widthWeiche länge radius winkelBogenmaß) (heightWeiche radius winkelBogenmaß)
    $ zeichneWeicheRechts länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Pfad zum Zeichnen einer Weiche mit angegebener Länge und Rechts-Kurve mit Kurvenradius und Winkel im Bogenmaß.
zeichneWeicheRechts :: (Spurweite z) => Double -> Double -> Double -> Gleis z -> Cairo.Render ()
zeichneWeicheRechts länge radius winkel gleis = do
    zeichneGerade länge gleis
    Cairo.stroke
    zeichneKurve radius winkel False gleis

weicheLinksNew :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
weicheLinksNew länge radius winkel =
    gleisNew (widthWeiche länge radius winkelBogenmaß) (heightWeiche radius winkelBogenmaß)
    $ \gleis -> do
        Cairo.translate (halfWidth gleis) (halfHeight gleis)
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth gleis) (-halfHeight gleis)
        zeichneWeicheRechts länge radius winkelBogenmaß gleis
    where
        halfWidth :: Gleis z -> Double
        halfWidth gleis = 0.5 * fromIntegral (widthWeiche länge radius winkelBogenmaß gleis)

        halfHeight :: Gleis z -> Double
        halfHeight gleis = 0.5 * fromIntegral (heightWeiche radius winkelBogenmaß gleis)

        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

widthKurvenWeiche :: (Spurweite z) => Double -> Double -> Double -> Gleis z -> Int32
widthKurvenWeiche länge radius winkelBogenmaß gleis =
    ceiling länge + widthKurve radius winkelBogenmaß gleis

heightKurvenWeiche :: (Spurweite z) => Double -> Double -> Gleis z -> Int32
heightKurvenWeiche radius winkelBogenmaß gleis =
    max (ceiling $ beschränkung gleis) $ heightKurve radius winkelBogenmaß gleis

kurvenWeicheRechtsNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
kurvenWeicheRechtsNew länge radius winkel =
    gleisNew
        (widthKurvenWeiche länge radius winkelBogenmaß)
        (heightKurvenWeiche radius winkelBogenmaß)
    $ zeichneKurvenWeicheRechts länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Pfad zum Zeichnen einer Kurven-Weiche mit angegebener Länge und Rechts-Kurve mit Kurvenradius und Winkel im Bogenmaß.
--
-- Beide Kurven haben den gleichen Radius und Winkel, die äußere Kurve beginnt erst nach /länge/.
zeichneKurvenWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Gleis z -> Cairo.Render ()
zeichneKurvenWeicheRechts länge radius winkel gleis = do
    zeichneKurve radius winkel True gleis
    Cairo.stroke
    -- Gleis
    Cairo.moveTo 0 gleisOben
    Cairo.lineTo länge gleisOben
    Cairo.moveTo 0 gleisUnten
    Cairo.lineTo länge gleisUnten
    Cairo.stroke
    Cairo.translate länge 0
    zeichneKurve radius winkel False gleis
    where
        gleisOben :: Double
        gleisOben = abstand gleis

        gleisUnten :: Double
        gleisUnten = beschränkung gleis - abstand gleis

kurvenWeicheLinksNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
kurvenWeicheLinksNew länge radius winkel =
    gleisNew
        (widthKurvenWeiche länge radius winkelBogenmaß)
        (heightKurvenWeiche radius winkelBogenmaß)
    $ \gleis -> do
        Cairo.translate (halfWidth gleis) (halfHeight gleis)
        Cairo.transform $ Matrix 1 0 0 (-1) 0 0
        Cairo.translate (-halfWidth gleis) (-halfHeight gleis)
        zeichneKurvenWeicheRechts länge radius winkelBogenmaß gleis
    where
        halfWidth :: Gleis z -> Double
        halfWidth gleis =
            0.5 * fromIntegral (widthKurvenWeiche länge radius winkelBogenmaß gleis)

        halfHeight :: Gleis z -> Double
        halfHeight gleis = 0.5 * fromIntegral (heightKurvenWeiche radius winkelBogenmaß gleis)

        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Notwendige Größen zur Charakterisierung eines 'Gleis'es.
--
-- Alle Längenangaben sind in mm (= Pixel mit scale 1).
-- Winkel sind im Bogenmaß (z.B. 90° ist rechter Winkel).
data GleisDefinition (z :: Zugtyp)
    = Gerade { länge :: Double }
    | Kurve { radius :: Double, winkel :: Double }
    | Weiche { länge :: Double, radius :: Double, winkel :: Double, richtung :: WeichenRichtung }
    | Kreuzung { länge :: Double, radius :: Double, winkel :: Double }

data WeichenArt
    = GeradeWeiche
    | GebogeneWeiche

data WeichenRichtungAllgemein (a :: WeichenArt) where
    Links :: WeichenRichtungAllgemein a
    Rechts :: WeichenRichtungAllgemein a
    Dreiwege :: WeichenRichtungAllgemein 'GeradeWeiche

data WeichenRichtung
    = Normal { geradeRichtung :: WeichenRichtungAllgemein 'GeradeWeiche }
    | Gebogen { gebogeneRichtung :: WeichenRichtungAllgemein 'GebogeneWeiche }

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
märklinGerade5106New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5106New = geradeNew 180

märklinGerade5107New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5107New = geradeNew 90

märklinGerade5129New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5129New = geradeNew 70

märklinGerade5108New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5108New = geradeNew 45

märklinGerade5109New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5109New = geradeNew 33.5

märklinGerade5110New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5110New = geradeNew 22.5

märklinGerade5210New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5210New = geradeNew 16

märklinGerade5208New :: (MonadIO m) => m (Gleis 'Märklin)
märklinGerade5208New = geradeNew 8

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
märklinRIndustrie :: Double
märklinRIndustrie = 286

märklinR1 :: Double
märklinR1 = 360

märklinR2 :: Double
märklinR2 = 437.4

märklinKurve5120New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5120New = kurveNew märklinRIndustrie 45

märklinKurve5100New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5100New = kurveNew märklinR1 30

märklinKurve5101New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5101New = kurveNew märklinR1 15

märklinKurve5102New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5102New = kurveNew märklinR1 7.5

märklinKurve5200New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5200New = kurveNew märklinR2 30

märklinKurve5206New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5206New = kurveNew märklinR2 24.28

märklinKurve5201New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5201New = kurveNew märklinR2 15

märklinKurve5205New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurve5205New = kurveNew märklinR2 5.72

{-
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
    5214 (3-Weg): L180mm, 24,28°, R437.4mm
-}
-- TODO Kurvenradien bei Weichen?
märklinWeicheRechts5117New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheRechts5117New = weicheRechtsNew 180 märklinR1 30

märklinWeicheLinks5117New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheLinks5117New = weicheLinksNew 180 märklinR1 30

märklinWeicheRechts5137New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheRechts5137New = weicheRechtsNew 180 märklinR1 22.5

märklinWeicheLinks5137New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheLinks5137New = weicheLinksNew 180 märklinR1 22.5

märklinWeicheRechts5202New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheRechts5202New = weicheRechtsNew 180 märklinR2 24.28

märklinWeicheLinks5202New :: (MonadIO m) => m (Gleis 'Märklin)
märklinWeicheLinks5202New = weicheLinksNew 180 märklinR2 24.28

märklinDreiwegWeiche5214New :: (MonadIO m) => m (Gleis 'Märklin)
märklinDreiwegWeiche5214New = error "Dreiwege-Weiche 5214" --TODO

{-
Kurven-Weiche
    5140 L/R: 30°, Rin360mm, Rout360mm @ 77.4mm (Gerade vor Bogen)
-}
märklinKurvenWeicheRechts5140New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurvenWeicheRechts5140New = kurvenWeicheRechtsNew 77.4 märklinR1 30

märklinKurvenWeicheLinks5140New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKurvenWeicheLinks5140New = kurvenWeicheLinksNew 77.4 märklinR1 30

{-
Kreuzung
    5128: L193mm, 30°
    5207: L180mm, 24.28°, R437.4mm
-}
märklinKreuzung5128New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKreuzung5128New = error "Kreuzung 5128" --TODO

märklinKreuzung5207New :: (MonadIO m) => m (Gleis 'Märklin)
märklinKreuzung5207New = error "Kreuzung 5207" --TODO

{-
Prellbock:
    7190: 70mm
Kupplungsgleis:
    5112 U: 90mm
-}
-- Beispiel-Anzeige
gleisAnzeigeNew :: (MonadIO m) => m Gtk.Fixed
gleisAnzeigeNew = do
    fixed <- Gtk.fixedNew
    (width, height) <- foldM
        (putWithHeight fixed)
        (0, padding)
        [ ("5106:  ", märklinGerade5106New)
        , ("5107: ", märklinGerade5107New)
        , ("5129: ", märklinGerade5129New)
        , ("5108: ", märklinGerade5108New)
        , ("5109: ", märklinGerade5109New)
        , ("5110: ", märklinGerade5110New)
        , ("5210: ", märklinGerade5210New)
        , ("5208: ", märklinGerade5208New)
        , ("5120: ", märklinKurve5120New)
        , ("5100: ", märklinKurve5100New)
        , ("5101 :", märklinKurve5101New)
        , ("5102 :", märklinKurve5102New)
        , ("5200: ", märklinKurve5200New)
        , ("5206: ", märklinKurve5206New)
        , ("5201: ", märklinKurve5201New)
        , ("5205: ", märklinKurve5205New)
        , ("5117R:", märklinWeicheRechts5117New)
        , ("5117L:", märklinWeicheLinks5117New)
        , ("5137R:", märklinWeicheRechts5137New)
        , ("5137L:", märklinWeicheLinks5137New)
        , ("5202R:", märklinWeicheRechts5202New)
        , ("5202L:", märklinWeicheLinks5202New)
        , ("5140R:", märklinKurvenWeicheRechts5140New)
        , ("5140L:", märklinKurvenWeicheLinks5140New)]
    Gtk.widgetSetSizeRequest fixed (2 * padding + width) (2 * padding + height)
    pure fixed
    where
        padding :: Int32
        padding = 5

        putWithHeight :: (MonadIO m)
                      => Gtk.Fixed
                      -> (Int32, Int32)
                      -> (Text, m (Gleis 'Märklin))
                      -> m (Int32, Int32)
        putWithHeight fixed (maxWidth, y) (text, konstruktor) = do
            label <- fixedPutWidgetNew fixed padding y $ Gtk.labelNew $ Just text
            (_widthMin, widthLabel) <- Gtk.widgetGetPreferredWidth label
            let x = 2 * padding + widthLabel
            Gleis {width, height} <- fixedPutWidgetNew fixed x y konstruktor
            pure (max (x + width) maxWidth, y + height + padding)

{-
Lego Spurweite: 38mm
-}
legoGeradeNew :: (MonadIO m) => m (Gleis 'Lego)
legoGeradeNew = geradeNew $ error "Geraden-Länge"
#endif
