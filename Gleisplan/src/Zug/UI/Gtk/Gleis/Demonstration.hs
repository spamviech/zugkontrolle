{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Zug.UI.Gtk.Gleis.Demonstration (gleisDemonstrationNew) where

import Control.Monad (foldM_)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Monad.Trans.Except (runExceptT, catchE)
import Data.Int (Int32)
import Data.Text (Text)

import Zug.UI.Gtk.Gleis.Maerklin
       (WeichenRichtungAllgemein(Links, Rechts), Zugtyp(Märklin), märklinKurvenWeiche5140
      , märklinDreiwegWeiche5214, märklinWeiche5202, märklinWeiche5137, märklinWeiche5117
      , märklinKurve5205, märklinKurve5201, märklinKurve5206, märklinKurve5200
      , märklinKurve5102, märklinKurve5101, märklinKurve5100, märklinKurve5120
      , märklinGerade5208, märklinGerade5210, märklinGerade5110, märklinGerade5109
      , märklinGerade5108, märklinGerade5129, märklinGerade5106, märklinGerade5107
      , märklinKreuzung5128, märklinKreuzung5207)
import Zug.UI.Gtk.Gleis.Widget
       (GleisAnzeige, GleisAnzeigeConfig(..), Position(..), GleisDefinition, getWidth, getHeight
      , gleisAnzeigeNew, gleisAnzeigeConfig, gleisPut, textPut, gleisAnzeigeSave, gleisAnzeigeLoad)

-- Beispiel-Anzeige
gleisDemonstrationNew :: (MonadIO m) => m (GleisAnzeige 'Märklin)
gleisDemonstrationNew = do
    gleisAnzeige <- gleisAnzeigeNew
    runExceptT $ catchE (gleisAnzeigeLoad gleisAnzeige saveFile) $ \exception -> do
        liftIO $ print exception
        gleisAnzeigeConfig gleisAnzeige GleisAnzeigeConfig { x = 0, y = 0, scale = 1 }
        foldM_
            (putWithHeight gleisAnzeige)
            (0, 0)
            [ ("5106: ", märklinGerade5106)
            , ("5107: ", märklinGerade5107)
            , ("5129: ", märklinGerade5129)
            , ("5108: ", märklinGerade5108)
            , ("5109: ", märklinGerade5109)
            , ("5110: ", märklinGerade5110)
            , ("5210: ", märklinGerade5210)
            , ("5208: ", märklinGerade5208)
            , ("5120: ", märklinKurve5120)
            , ("5100: ", märklinKurve5100)
            , ("5101: ", märklinKurve5101)
            , ("5102: ", märklinKurve5102)
            , ("5200: ", märklinKurve5200)
            , ("5206: ", märklinKurve5206)
            , ("5201: ", märklinKurve5201)
            , ("5205: ", märklinKurve5205)
            , ("5117R:", märklinWeiche5117 Rechts)
            , ("5117L:", märklinWeiche5117 Links)
            , ("5137R:", märklinWeiche5137 Rechts)
            , ("5137L:", märklinWeiche5137 Links)
            , ("5202R:", märklinWeiche5202 Rechts)
            , ("5202L:", märklinWeiche5202 Links)
            , ("5214: ", märklinDreiwegWeiche5214)
            , ("5140R:", märklinKurvenWeiche5140 Rechts)
            , ("5140L:", märklinKurvenWeiche5140 Links)
            , ("5128: ", märklinKreuzung5128)
            , ("5207: ", märklinKreuzung5207)]
        gleisAnzeigeSave gleisAnzeige "demonstration.gleisplan"
    pure gleisAnzeige
    where
        saveFile :: FilePath
        saveFile = "demonstration.gleisplan"

        padding :: Int32
        padding = 5

        putWithHeight :: (MonadIO m)
                      => GleisAnzeige 'Märklin
                      -> (Int32, Int32)
                      -> (Text, GleisDefinition 'Märklin)
                      -> m (Int32, Int32)
        putWithHeight gleisAnzeige (maxWidth, y) (text, definition) = do
            textPut gleisAnzeige text Position { x = 0, y = fromIntegral y, winkel = 0 }
            -- TODO estimate for now
            let widthLabel = 50
            -- widthLabel <- getTextWidth reqMaxLabel
            let x = padding + widthLabel
            gleisPut
                gleisAnzeige
                definition
                Position { x = fromIntegral x, y = fromIntegral y, winkel = 0 }
            pure (max (x + getWidth definition) maxWidth, y + getHeight definition + padding)
