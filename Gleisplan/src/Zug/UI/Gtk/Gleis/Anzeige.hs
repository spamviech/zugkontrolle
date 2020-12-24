{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Zug.UI.Gtk.Gleis.Anzeige (gleisAnzeigeNew) where

import Control.Monad (foldM)
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
      , Gleis, Zugtyp(Märklin), märklinGerade5106New, märklinGerade5107New)
import Zug.UI.Gtk.Gleis.Widget (MitWidget(erhalteWidget))

-- Beispiel-Anzeige
gleisAnzeigeNew :: (MonadIO m) => m Gtk.Fixed
gleisAnzeigeNew = do
    fixed <- Gtk.fixedNew
    (width, height) <- foldM
        (putWithHeight fixed)
        (0, padding)
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
            label <- Gtk.labelNew $ Just text
            Gtk.fixedPut fixed label (fromIntegral padding) $ fromIntegral y
            -- required for Gtk3, otherwise size-calculation doesn't work
            Gtk.widgetShow label
            (_reqMinLabel, reqMaxLabel) <- Gtk.widgetGetPreferredSize label
            widthLabel <- Gtk.getRequisitionWidth reqMaxLabel
            let x = 2 * padding + widthLabel
            gleis <- konstruktor
            widget <- erhalteWidget gleis
            Gtk.fixedPut fixed widget (fromIntegral x) $ fromIntegral y
            (_reqMin, reqMax) <- Gtk.widgetGetPreferredSize widget
            width <- Gtk.getRequisitionWidth reqMax
            height <- Gtk.getRequisitionHeight reqMax
            pure (max (x + width) maxWidth, y + height + padding)
