{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Zug.UI.Gtk.StreckenObjekt.KOWidgets (KOWidgets, kontaktPackNew) where

instance Kategorie KOWidgets where
    kategorie :: KategorieText KOWidgets
    kategorie = KategorieText Language.kontakte

data KOWidgets = KOWidgets { ko :: Kontakt }
    deriving (Eq)

kontaktPackNew :: (MonadIO m) => Kontakt -> m KOWidgets
kontaktPackNew = _undefined --TODO

instance Aeson.ToJSON KOWidgets where
    toJSON :: KOWidgets -> Aeson.Value
    toJSON = _undefined --TODO