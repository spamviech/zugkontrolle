{-# LANGUAGE CPP #-}
#ifdef ZUGKONTROLLEGUI
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
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
    -- ** Märklin H0 (M-Gleise)
    -- *** Gerade
  , märklin5106New
    -- *** Kurve
  , märklin5100New
    -- *** Weiche
    -- ** Lego (9V Gleise)
  , legoGeradeNew
#endif
  ) where

#ifdef ZUGKONTROLLEGUI
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Int (Int32)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Gtk as Gtk

import Zug.Enums (Zugtyp(..))
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

geradeHeight :: (Spurweite z) => Gleis z -> Double
geradeHeight gleis = spurweite gleis + 2 * abstand gleis

-- | Erzeuge eine neues gerades 'Gleis' der angegebenen Länge.
geradeNew :: (MonadIO m, Spurweite z) => (forall n. Num n => n) -> m (Gleis z)
geradeNew länge = gleisNew (const länge) (ceiling . geradeHeight) $ zeichneGerade länge

-- | Pfad zum Zeichnen einer Geraden der angegebenen Länge.
zeichneGerade :: (Spurweite z) => Double -> Gleis z -> Cairo.Render ()
zeichneGerade länge gleis = do
    -- Beschränkungen
    Cairo.moveTo 0 0
    Cairo.lineTo 0 $ geradeHeight gleis
    Cairo.moveTo länge 0
    Cairo.lineTo länge $ geradeHeight gleis
    -- Gleis
    Cairo.moveTo 0 gleisOben
    Cairo.lineTo länge gleisOben
    Cairo.moveTo 0 gleisUnten
    Cairo.lineTo länge gleisUnten
    where
        gleisOben :: Double
        gleisOben = abstand gleis

        gleisUnten :: Double
        gleisUnten = geradeHeight gleis - abstand gleis

-- | Erzeuge eine neue Kurve mit angegebenen Radius und Winkel im Gradmaß.
kurveNew :: (MonadIO m, Spurweite z) => (forall n. Num n => n) -> (forall n. Num n => n) -> m (Gleis z)
kurveNew radius winkel = gleisNew (const width) (const height) $ zeichneKurve radius $ pi * winkel / 180 
    where
        -- TODO für äußeren Kurvenradius
        width :: Int32
        width = 200 -- _
        height :: Int32
        height = 180 -- _

-- | Pfad zum Zeichnen einer Kurve mit angegebenen Kurvenradius und Winkel im Bogenmaß.
zeichneKurve :: (Spurweite z) => Double -> Double -> Gleis z -> Cairo.Render ()
zeichneKurve radius winkel gleis = do
    -- Beschränkungen
    Cairo.moveTo 0 0
    Cairo.lineTo 0 $ geradeHeight gleis
    -- TODO zweite Beschränkung
    -- Gleis
    Cairo.arc 0 bogenZentrumY radiusAußen anfangsWinkel (anfangsWinkel + winkel)
    Cairo.stroke
    Cairo.arc 0 bogenZentrumY radiusInnen anfangsWinkel (anfangsWinkel + winkel)
    where
        bogenZentrumY :: Double
        bogenZentrumY = abstand gleis + radiusAußen

        anfangsWinkel :: Double
        anfangsWinkel = 3 * pi / 2

        radiusInnen :: Double
        radiusInnen = radius

        radiusAußen :: Double
        radiusAußen = radius + spurweite gleis
-- TODO ist es mittlerer, innerer, äußerer Kurvenradius?

{-
H0 Spurweite: 16.5mm
Gerade (5106): L180mm
Kurve (5120): 45°, R286mm
Kurve (5100): 30°, R360mm
Kurve (5200): 30°, R437.4mm
Kurve (5206): 24.28°, R427.4mm
Weiche (5202 L/R): L180mm, 24,28°, 427.4mm
Weiche (5140 L/R): 30°, Rin360mm, Rout77.4mm
Kreuzung (5128): L193mm, 30°
Kreuzung (5207): L180mm, 24,28°, R427.4mm
-}
märklin5106New :: (MonadIO m) => m (Gleis 'Märklin)
märklin5106New = geradeNew 180

märklin5100New :: (MonadIO m) => m (Gleis 'Märklin)
märklin5100New = kurveNew 360 30

{-
Lego Spurweite: 38mm
-}
legoGeradeNew :: (MonadIO m) => m (Gleis 'Lego)
legoGeradeNew = geradeNew $ error "Geraden-Länge"
#endif
