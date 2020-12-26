{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Zug.UI.Gtk.Gleis.Demonstration (gleisDemonstrationNew) where

import Control.Monad (foldM_)
import Control.Monad.Trans (MonadIO())
import Data.Int (Int32)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import Zug.UI.Gtk.Gleis.Maerklin
       (märklinKurvenWeicheLinks5140New, märklinKurvenWeicheRechts5140New
      , märklinDreiwegWeiche5214New, märklinWeicheLinks5202New, märklinWeicheRechts5202New
      , märklinWeicheLinks5137New, märklinWeicheRechts5137New, märklinWeicheLinks5117New
      , märklinWeicheRechts5117New, märklinKurve5205New, märklinKurve5201New
      , märklinKurve5206New, märklinKurve5200New, märklinKurve5102New, märklinKurve5101New
      , märklinKurve5100New, märklinKurve5120New, märklinGerade5208New, märklinGerade5210New
      , märklinGerade5110New, märklinGerade5109New, märklinGerade5108New, märklinGerade5129New
      , märklinGerade5106New, märklinGerade5107New, märklinKreuzung5128New
      , märklinKreuzung5207New, Zugtyp(Märklin))
import Zug.UI.Gtk.Gleis.Widget (GleisAnzeige, gleisAnzeigeNew, gleisPut, gleisGetSize, Gleis
                              , gleisAnzeigePutLabel, Position(..), gleisAnzeigeScale)
import Zug.UI.Gtk.Klassen (MitWidget(erhalteWidget))

-- Beispiel-Anzeige
gleisDemonstrationNew :: (MonadIO m) => m (GleisAnzeige 'Märklin)
gleisDemonstrationNew = do
    gleisAnzeige <- gleisAnzeigeNew
    foldM_
        (putWithHeight gleisAnzeige)
        (0, 0)
        [ ("5106: ", märklinGerade5106New)
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
        , ("5214: ", märklinDreiwegWeiche5214New)
        , ("5140R:", märklinKurvenWeicheRechts5140New)
        , ("5140L:", märklinKurvenWeicheLinks5140New)
        , ("5128: ", märklinKreuzung5128New)
        , ("5207: ", märklinKreuzung5207New)]
    gleisAnzeigeScale gleisAnzeige 1.4
    widget <- erhalteWidget gleisAnzeige
    Gtk.widgetSetMarginTop widget padding
    Gtk.widgetSetMarginBottom widget padding
    Gtk.widgetSetMarginStart widget padding
    Gtk.widgetSetMarginEnd widget padding
    pure gleisAnzeige
    where
        padding :: Int32
        padding = 5

        putWithHeight :: (MonadIO m)
                      => GleisAnzeige 'Märklin
                      -> (Int32, Int32)
                      -> (Text, m (Gleis 'Märklin))
                      -> m (Int32, Int32)
        putWithHeight gleisAnzeige (maxWidth, y) (text, konstruktor) = do
            label <- Gtk.labelNew $ Just text
            gleisAnzeigePutLabel gleisAnzeige label Position { x = 0, y = fromIntegral y }
            (_reqMinLabel, reqMaxLabel) <- Gtk.widgetGetPreferredSize label
            widthLabel <- Gtk.getRequisitionWidth reqMaxLabel
            let x = padding + widthLabel
            gleis <- konstruktor
            gleisPut gleisAnzeige gleis Position { x = fromIntegral x, y = fromIntegral y } 0
            (width, height) <- gleisGetSize gleis
            pure (max (x + width) maxWidth, y + height + padding)
