{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

{-
ideas for rewrite with gtk4
    start from scratch (UI), so legacy-based mistakes might vanish
look at Gtk.renderLine (cairo-render/-connector should still work)
Use `GLib.addIdle GLib.PRIORITY_DEFAULT $ gtkAction` to call Gtk from a forked thread
    (sync e.g. with TMVar for a blocking version)
    https://github.com/haskell-gi/haskell-gi/wiki/Using-threads-in-Gdk-and-Gtk--programs
Gtk.Application has to be used instead of Gtk.main
    https://hackage.haskell.org/package/gi-gtk-4.0.2/docs/GI-Gtk-Objects-Application.html
    startup/activate-signals are in gi-gio
    https://hackage.haskell.org/package/gi-gio-2.0.27/docs/GI-Gio-Objects-Application.html#v:applicationSetResourceBasePath
AspectFrame doesn't draw a frame around the child, so might be useful here
    doesn't pass scaling information to DrawingArea
    usefulness questionable anyway due to rotation requirement
Assistant should work again
-}
module Zug.UI.Gtk.Gleis
  ( -- * Gleis Widgets
    Gleis()
  , Zugtyp(..)
  , MitWidget(..)
    -- ** Definition
  , GleisDefinition(..)
  , WeichenArt(..)
  , WeichenRichtungAllgemein(..)
  , alsDreiweg
  , WeichenRichtung(..)
    -- ** Anker-Punkte
  , AnchorName(..)
  , AnchorPoint(..)
    -- * Konstruktor
  , gleisNew
    -- ** Anpassen der Größe
  , gleisScale
  , gleisSetWidth
  , gleisSetHeight
  , gleisRotate
  ) where

import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (when, void)
import Control.Monad.RWS.Strict
       (RWST(), MonadReader(ask), MonadState(state), MonadWriter(tell), execRWST)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import Data.Bool (bool)
import Data.HashMap.Strict (HashMap())
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable())
import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import GI.Cairo.Render.Matrix (Matrix(Matrix))
import qualified GI.Gtk as Gtk
import Numeric.Natural (Natural)

-- import Zug.Enums (Zugtyp(..))
-- import Zug.UI.Gtk.Hilfsfunktionen (fixedPutWidgetNew)
-- import Zug.UI.Gtk.Klassen (MitWidget(..))
class MitWidget w where
    erhalteWidget :: (MonadIO m) => w -> m Gtk.Widget

data Zugtyp
    = Märklin
    | Lego

-- | 'Gtk.Widget' von einem Gleis.
-- Die Größe wird nur über 'gleisScale', 'gleisSetWidth' und 'gleisSetHeight' verändert.
data Gleis (z :: Zugtyp) =
    Gleis
    { drawingArea :: Gtk.DrawingArea
    , width :: Int32
    , height :: Int32
    , tvarScale :: TVar Double
    , tvarAngle :: TVar Double
    , tvarAnchorPoints :: TVar AnchorPointMap
    , tvarConnectedAnchors :: TVar [AnchorName]
    }

instance MitWidget (Gleis z) where
    erhalteWidget :: (MonadIO m) => Gleis z -> m Gtk.Widget
    erhalteWidget = Gtk.toWidget . drawingArea

-- | Speichern aller AnchorPoints
type AnchorPointMap = HashMap AnchorName AnchorPoint

-- | Monad Transformer zum definieren der AnchorPoints
type AnchorPointT m a = RWST Text AnchorPointMap Natural m a

-- | Namen um ausgezeichnete Punkte eines 'Gleis'es anzusprechen.
newtype AnchorName = AnchorName { anchor :: Text }
    deriving (Show, Eq, Hashable)

-- | Position und ausgehender Vektor eines AnchorPoint.
data AnchorPoint =
    AnchorPoint { anchorX :: Double, anchorY :: Double, anchorVX :: Double, anchorVY :: Double }

-- | Erstelle eine AnchorPoint an der aktuellen Stelle mit Gleisfortführung in Richtung (/vx/, /vy/),
-- angegeben in user space.
--
-- The /AnchorPoint/ is stored in device space, i.e. after a call to 'Cairo.userToDevice'.
-- This is necessary, so arbitrary transformations are still possible.
--
-- Der /anchorName/ wird aus einem konstanten Teil und einer Zahl zusammengesetzt.
makeAnchorPoint :: Double -> Double -> AnchorPointT Cairo.Render ()
makeAnchorPoint vx vy = do
    (anchorX, anchorY) <- lift $ Cairo.getCurrentPoint >>= uncurry Cairo.userToDevice
    (anchorVX, anchorVY) <- lift $ Cairo.userToDeviceDistance vx vy
    anchorName <- fmap AnchorName
        $ (<>) <$> ask <*> (Text.pack . show <$> state (\n -> (n, succ n)))
    tell $ HashMap.singleton anchorName AnchorPoint { anchorX, anchorY, anchorVX, anchorVY }

gleisAdjustSizeRequest :: (MonadIO m) => Gleis z -> m ()
gleisAdjustSizeRequest Gleis {drawingArea, width, height, tvarScale, tvarAngle} = do
    (scale, angle) <- liftIO $ (,) <$> readTVarIO tvarScale <*> readTVarIO tvarAngle
    let newWidth = scale * fromIntegral width
        newHeight = scale * fromIntegral height
        adjustedWidth = ceiling $ abs (newWidth * cos angle) + abs (newHeight * sin angle)
        adjustedHeight = ceiling $ abs (newHeight * cos angle) + abs (newWidth * sin angle)
    Gtk.drawingAreaSetContentHeight drawingArea adjustedHeight
    Gtk.drawingAreaSetContentWidth drawingArea adjustedWidth

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
createGleisWidget :: (MonadIO m)
                  => (Proxy z -> Int32)
                  -> (Proxy z -> Int32)
                  -> Text
                  -> (Proxy z -> AnchorPointT Cairo.Render ())
                  -> m (Gleis z)
createGleisWidget widthFn heightFn anchorBaseName draw = do
    (tvarScale, tvarAngle, tvarAnchorPoints, tvarConnectedAnchors) <- liftIO
        $ (,,,) <$> newTVarIO 1 <*> newTVarIO 0 <*> newTVarIO HashMap.empty <*> newTVarIO []
    let width = widthFn Proxy
        height = heightFn Proxy
    drawingArea <- Gtk.drawingAreaNew
    Gtk.widgetSetHexpand drawingArea False
    Gtk.widgetSetHalign drawingArea Gtk.AlignStart
    Gtk.widgetSetVexpand drawingArea False
    Gtk.widgetSetValign drawingArea Gtk.AlignStart
    let gleis =
            Gleis
            { drawingArea
            , width
            , height
            , tvarScale
            , tvarAngle
            , tvarAnchorPoints
            , tvarConnectedAnchors
            }
    gleisScale gleis 1
    Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ \_drawingArea context newWidth newHeight
        -> void $ flip Cairo.renderWithContext context $ do
            (scale, angle) <- liftIO $ (,) <$> readTVarIO tvarScale <*> readTVarIO tvarAngle
            Cairo.save
            let halfWidth = 0.5 * fromIntegral newWidth
                halfHeight = 0.5 * fromIntegral newHeight
            Cairo.translate halfWidth halfHeight
            Cairo.rotate angle
            Cairo.scale scale scale
            Cairo.translate (-0.5 * fromIntegral width) (-0.5 * fromIntegral height)
            Cairo.setLineWidth 1
            Cairo.newPath
            (_num, deviceAnchorPoints) <- execRWST (draw Proxy) anchorBaseName 0
            Cairo.stroke
            Cairo.restore
            -- container coordinates
            Cairo.save
            Cairo.setLineWidth 1
            userAnchorPoints <- flip HashMap.traverseWithKey deviceAnchorPoints
                $ \anchorName AnchorPoint
                {anchorX = deviceX, anchorY = deviceY, anchorVX = deviceVX, anchorVY = deviceVY}
                -> do
                    (anchorX, anchorY) <- Cairo.deviceToUser deviceX deviceY
                    (anchorVX, anchorVY) <- Cairo.deviceToUserDistance deviceVX deviceVY
                    -- show anchors
                    let len = anchorVX * anchorVX + anchorVY * anchorVY
                    Cairo.moveTo anchorX anchorY
                    (r, g, b) <- bool (0, 0, 1) (0, 1, 0) . elem anchorName
                        <$> liftIO (readTVarIO tvarConnectedAnchors)
                    Cairo.setSourceRGB r g b
                    Cairo.relLineTo (-5 * anchorVX / len) (-5 * anchorVY / len)
                    Cairo.stroke
                    pure AnchorPoint { anchorX, anchorY, anchorVX, anchorVY }
            Cairo.restore
            liftIO $ atomically $ writeTVar tvarAnchorPoints userAnchorPoints
            pure True
    pure gleis

class Spurweite (z :: Zugtyp) where
    spurweite :: Proxy z -> Double

instance Spurweite 'Märklin where
    spurweite :: Proxy 'Märklin -> Double
    spurweite = const 16.5

instance Spurweite 'Lego where
    spurweite :: Proxy 'Lego -> Double
    spurweite = const 38

abstand :: (Spurweite z) => Proxy z -> Double
abstand gleis = spurweite gleis / 3

beschränkung :: (Spurweite z) => Proxy z -> Double
beschränkung gleis = spurweite gleis + 2 * abstand gleis

-- Märklin verwendet mittleren Kurvenradius
-- http://www.modellbau-wiki.de/wiki/Gleisradius
radiusBegrenzung :: (Spurweite z) => Double -> Proxy z -> Double
radiusBegrenzung radius proxy = radius + 0.5 * spurweite proxy + abstand proxy

widthKurve :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
widthKurve radius winkelBogenmaß proxy
    | winkelBogenmaß < 0.5 * pi = ceiling $ radiusBegrenzung radius proxy * sin winkelBogenmaß
    | otherwise = error "Nur Kurven mit Winkel < pi/2 (90°) sind unterstützt."

heightKurve :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightKurve radius winkelBogenmaß proxy
    | winkelBogenmaß < 0.5 * pi =
        ceiling
        $ radiusBegrenzung radius proxy * (1 - cos winkelBogenmaß)
        + beschränkung proxy * cos winkelBogenmaß
    | otherwise = error "Nur Kurven mit Winkel < pi/2 (90°) sind unterstützt."

widthWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthWeiche länge radius winkelBogenmaß proxy =
    max (ceiling länge) $ widthKurve radius winkelBogenmaß proxy

heightWeiche :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightWeiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) $ heightKurve radius winkelBogenmaß proxy

-- | Erzeuge eine neues gerades 'Gleis' der angegebenen Länge.
geradeNew :: (MonadIO m, Spurweite z) => Double -> m (Gleis z)
geradeNew länge =
    createGleisWidget (const $ ceiling länge) (ceiling . beschränkung) "Gerade"
    $ zeichneGerade länge

-- | Pfad zum Zeichnen einer Geraden der angegebenen Länge.
zeichneGerade :: (Spurweite z) => Double -> Proxy z -> AnchorPointT Cairo.Render ()
zeichneGerade länge proxy = do
    lift $ do
        -- Beschränkungen
        Cairo.moveTo 0 0
        Cairo.lineTo 0 $ 0.5 * beschränkung proxy
    makeAnchorPoint (-1) 0
    lift $ do
        Cairo.lineTo 0 $ beschränkung proxy
        Cairo.moveTo länge 0
        Cairo.lineTo länge $ 0.5 * beschränkung proxy
    makeAnchorPoint 1 0
    lift $ Cairo.lineTo länge $ beschränkung proxy
    lift $ do
        -- Gleis
        Cairo.moveTo 0 gleisOben
        Cairo.lineTo länge gleisOben
        Cairo.moveTo 0 gleisUnten
        Cairo.lineTo länge gleisUnten
    where
        gleisOben :: Double
        gleisOben = abstand proxy

        gleisUnten :: Double
        gleisUnten = beschränkung proxy - abstand proxy

-- | Erzeuge eine neue Kurve mit angegebenen Radius und Winkel im Gradmaß.
kurveNew :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> m (Gleis z)
kurveNew radius winkel =
    createGleisWidget
        (widthKurve radius winkelBogenmaß)
        (heightKurve radius winkelBogenmaß)
        "Kurve"
    $ zeichneKurve radius winkelBogenmaß True
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Pfad zum Zeichnen einer Kurve mit angegebenen Kurvenradius und Winkel im Bogenmaß.
zeichneKurve
    :: (Spurweite z) => Double -> Double -> Bool -> Proxy z -> AnchorPointT Cairo.Render ()
zeichneKurve radius winkel anfangsBeschränkung proxy = do
    -- Beschränkungen
    when anfangsBeschränkung $ do
        lift $ do
            Cairo.moveTo 0 0
            Cairo.lineTo 0 $ 0.5 * beschränkung proxy
        makeAnchorPoint (-1) 0
        lift $ Cairo.lineTo 0 $ beschränkung proxy
    lift $ do
        Cairo.moveTo begrenzungX0 begrenzungY0
        Cairo.lineTo begrenzungX1 begrenzungY1
    makeAnchorPoint (cos winkel) (sin winkel)
    lift $ do
        Cairo.lineTo begrenzungX2 begrenzungY2
        Cairo.stroke
    lift $ do
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
        begrenzungX1 = begrenzungX0 - 0.5 * beschränkung proxy * sin winkel

        begrenzungX2 :: Double
        begrenzungX2 = begrenzungX0 - beschränkung proxy * sin winkel

        begrenzungY1 :: Double
        begrenzungY1 = begrenzungY0 + 0.5 * beschränkung proxy * cos winkel

        begrenzungY2 :: Double
        begrenzungY2 = begrenzungY0 + beschränkung proxy * cos winkel

        bogenZentrumY :: Double
        bogenZentrumY = abstand proxy + radiusAußen

        anfangsWinkel :: Double
        anfangsWinkel = 3 * pi / 2

        radiusInnen :: Double
        radiusInnen = radius - 0.5 * spurweite proxy

        radiusAußen :: Double
        radiusAußen = radius + 0.5 * spurweite proxy

        radiusBegrenzungAußen :: Double
        radiusBegrenzungAußen = radiusAußen + abstand proxy

weicheRechtsNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
weicheRechtsNew länge radius winkel =
    createGleisWidget
        (widthWeiche länge radius winkelBogenmaß)
        (heightWeiche radius winkelBogenmaß)
        "WeicheRechts"
    $ zeichneWeicheRechts länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Pfad zum Zeichnen einer Weiche mit angegebener Länge und Rechts-Kurve mit Kurvenradius und Winkel im Bogenmaß.
zeichneWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointT Cairo.Render ()
zeichneWeicheRechts länge radius winkel proxy = do
    zeichneGerade länge proxy
    lift Cairo.stroke
    zeichneKurve radius winkel False proxy

weicheLinksNew :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
weicheLinksNew länge radius winkel =
    createGleisWidget
        (widthWeiche länge radius winkelBogenmaß)
        (heightWeiche radius winkelBogenmaß)
        "WeicheLinks"
    $ \proxy -> do
        lift $ do
            Cairo.translate (halfWidth proxy) (halfHeight proxy)
            Cairo.transform $ Matrix 1 0 0 (-1) 0 0
            Cairo.translate (-halfWidth proxy) (-halfHeight proxy)
        zeichneWeicheRechts länge radius winkelBogenmaß proxy
    where
        halfWidth :: Proxy z -> Double
        halfWidth proxy = 0.5 * fromIntegral (widthWeiche länge radius winkelBogenmaß proxy)

        halfHeight :: Proxy z -> Double
        halfHeight proxy = 0.5 * fromIntegral (heightWeiche radius winkelBogenmaß proxy)

        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

widthKurvenWeiche :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> Int32
widthKurvenWeiche länge radius winkelBogenmaß proxy =
    ceiling länge + widthKurve radius winkelBogenmaß proxy

heightKurvenWeiche :: (Spurweite z) => Double -> Double -> Proxy z -> Int32
heightKurvenWeiche radius winkelBogenmaß proxy =
    max (ceiling $ beschränkung proxy) $ heightKurve radius winkelBogenmaß proxy

kurvenWeicheRechtsNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
kurvenWeicheRechtsNew länge radius winkel =
    createGleisWidget
        (widthKurvenWeiche länge radius winkelBogenmaß)
        (heightKurvenWeiche radius winkelBogenmaß)
        "KurvenWeicheRechts"
    $ zeichneKurvenWeicheRechts länge radius winkelBogenmaß
    where
        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Pfad zum Zeichnen einer Kurven-Weiche mit angegebener Länge und Rechts-Kurve mit Kurvenradius und Winkel im Bogenmaß.
--
-- Beide Kurven haben den gleichen Radius und Winkel, die äußere Kurve beginnt erst nach /länge/.
zeichneKurvenWeicheRechts
    :: (Spurweite z) => Double -> Double -> Double -> Proxy z -> AnchorPointT Cairo.Render ()
zeichneKurvenWeicheRechts länge radius winkel proxy = do
    zeichneKurve radius winkel True proxy
    lift $ do
        Cairo.stroke
        -- Gerade vor äußerer Kurve
        Cairo.moveTo 0 gleisOben
        Cairo.lineTo länge gleisOben
        Cairo.moveTo 0 gleisUnten
        Cairo.lineTo länge gleisUnten
        Cairo.stroke
        Cairo.translate länge 0
    zeichneKurve radius winkel False proxy
    where
        gleisOben :: Double
        gleisOben = abstand proxy

        gleisUnten :: Double
        gleisUnten = beschränkung proxy - abstand proxy

kurvenWeicheLinksNew
    :: forall m z. (MonadIO m, Spurweite z) => Double -> Double -> Double -> m (Gleis z)
kurvenWeicheLinksNew länge radius winkel =
    createGleisWidget
        (widthKurvenWeiche länge radius winkelBogenmaß)
        (heightKurvenWeiche radius winkelBogenmaß)
        "KurvenWeicheLinks"
    $ \proxy -> do
        lift $ do
            Cairo.translate (halfWidth proxy) (halfHeight proxy)
            Cairo.transform $ Matrix 1 0 0 (-1) 0 0
            Cairo.translate (-halfWidth proxy) (-halfHeight proxy)
        zeichneKurvenWeicheRechts länge radius winkelBogenmaß proxy
    where
        halfWidth :: Proxy z -> Double
        halfWidth proxy =
            0.5 * fromIntegral (widthKurvenWeiche länge radius winkelBogenmaß proxy)

        halfHeight :: Proxy z -> Double
        halfHeight proxy = 0.5 * fromIntegral (heightKurvenWeiche radius winkelBogenmaß proxy)

        winkelBogenmaß :: Double
        winkelBogenmaß = pi * winkel / 180

-- | Erstelle ein neues 'Gleis'.
gleisNew :: (MonadIO m, Spurweite z) => GleisDefinition z -> m (Gleis z)
gleisNew Gerade {länge} = geradeNew länge
gleisNew Kurve {radius, winkel} = kurveNew radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Normal {geradeRichtung = Links}} =
    weicheLinksNew länge radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Normal {geradeRichtung = Rechts}} =
    weicheRechtsNew länge radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Normal {geradeRichtung = Dreiwege}} =
    error $ "Dreiwegeweiche: " ++ show länge ++ ", " ++ show radius ++ ", " ++ show winkel  --TODO
gleisNew Weiche {länge, radius, winkel, richtung = Gebogen {gebogeneRichtung = Links}} =
    kurvenWeicheLinksNew länge radius winkel
gleisNew Weiche {länge, radius, winkel, richtung = Gebogen {gebogeneRichtung = Rechts}} =
    kurvenWeicheRechtsNew länge radius winkel
gleisNew Kreuzung {länge, radius, winkel} =
    error $ "Kreuzung: " ++ show länge ++ ", " ++ show radius ++ ", " ++ show winkel --TODO

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
    = WeicheZweiweg
    | WeicheDreiweg

data WeichenRichtungAllgemein (a :: WeichenArt) where
    Links :: WeichenRichtungAllgemein a
    Rechts :: WeichenRichtungAllgemein a
    Dreiwege :: WeichenRichtungAllgemein 'WeicheDreiweg

alsDreiweg :: WeichenRichtungAllgemein a -> WeichenRichtungAllgemein 'WeicheDreiweg
alsDreiweg Links = Links
alsDreiweg Rechts = Rechts
alsDreiweg Dreiwege = Dreiwege

data WeichenRichtung
    = Normal { geradeRichtung :: WeichenRichtungAllgemein 'WeicheDreiweg }
    | Gebogen { gebogeneRichtung :: WeichenRichtungAllgemein 'WeicheZweiweg }
