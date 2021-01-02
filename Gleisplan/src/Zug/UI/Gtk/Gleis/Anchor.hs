{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Zug.UI.Gtk.Gleis.Anchor
  ( AnchorName(..)
  , AnchorPoint(..)
  , AnchorPointRTree
  , AnchorPointMap
  , mbbSearch
  , mbbPoint
  , withAnchorName
  ) where

import Data.HashMap.Strict (HashMap())
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable())
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty())
import Data.RTree (RTree)
import qualified Data.RTree as RTree
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic())
import qualified GI.Gtk as Gtk
import Numeric.Natural (Natural)

-- | AnchorPoints einer 'GleisAnzeige', sortiert nach 'Position'.
type AnchorPointRTree = RTree (NonEmpty Gtk.DrawingArea)

-- | AnchorPoints eines 'Gleis'es mit jeweiliger Bezeichnung
type AnchorPointMap = HashMap AnchorName AnchorPoint

-- | Namen um ausgezeichnete Punkte eines 'Gleis'es anzusprechen.
newtype AnchorName = AnchorName { anchor :: Text }
    deriving (Show, Eq, Hashable)

-- | Position und ausgehender Vektor eines AnchorPoint.
data AnchorPoint =
    AnchorPoint { anchorX :: Double, anchorY :: Double, anchorVX :: Double, anchorVY :: Double }
    deriving (Eq, Show, Generic)

instance Hashable AnchorPoint

mbbSearch :: Double -> Double -> RTree.MBB
mbbSearch x y = RTree.mbb (x - epsilon) (y - epsilon) (x + epsilon) (y + epsilon)
    where
        epsilon :: Double
        epsilon = 0.5

mbbPoint :: Double -> Double -> RTree.MBB
mbbPoint x y = RTree.mbb x y x y

withAnchorName :: Text -> [AnchorPoint] -> AnchorPointMap
withAnchorName anchorBaseName = snd . foldl' foldFn (0, HashMap.empty)
    where
        foldFn :: (Natural, AnchorPointMap) -> AnchorPoint -> (Natural, AnchorPointMap)
        foldFn (n, acc) anchorPoint =
            ( succ n
            , HashMap.insert
                  AnchorName { anchor = anchorBaseName <> Text.pack (show n) }
                  anchorPoint
                  acc
            )
