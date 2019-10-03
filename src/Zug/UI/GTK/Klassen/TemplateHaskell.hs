{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

{-|
Description: Erzeuge Typklassen angelehnt an "Graphics.UI.Gtk"-Typklassen
-}
module Zug.UI.Gtk.Klassen.TemplateHaskell (erzeugeKlasse) where

import Data.Char (toLower)
import Data.Maybe (fromJust)
import Control.Monad (unless)
import Language.Haskell.TH

-- | Erzeuge Klasse /Mit<name>/, sowie eine default-Implementierung über /DefaultSignatures/.
erzeugeKlasse :: [Name] -> String -> Q [Dec]
erzeugeKlasse abhängigkeiten name = do
    istTyp nameMitPräfixGtk >>= flip unless (reportError $ '"' : nameMitPräfixGtk ++ "\" ist kein bekannter Name.")
    typName <- fromJust <$> lookupTypeName nameMitPräfixGtk
    variablenName <- newName "widget"
    mitFunktionSignatur <- erzeugeMitFunktionSignatur
    mitFunktionDeklaration <- erzeugeMitFunktionDeklaration
    getterSignatur <- erzeugeGetterSignatur typName
    pure $ [
        ClassD (context variablenName) klassenName [PlainTV $ variablenName] funDeps $ deklarationen variablenName typName,
        instanzDeklaration variablenName,
        mitFunktionSignatur,
        mitFunktionDeklaration,
        getterSignatur,
        getterDeklaration]
    where
        istTyp :: String -> Q Bool
        istTyp name = lookupTypeName name >>= \case
            Nothing
                -> pure False
            (Just typName)
                -> reify typName >>= pure . \case
                    (TyConI _dec)
                        -> True
                    _info
                        -> False
        namePräfixGtk :: String
        namePräfixGtk = "Gtk."
        nameMitPräfixGtk :: String
        nameMitPräfixGtk = namePräfixGtk ++ name
        klassenName :: Name
        klassenName = mkName $ "Mit" ++ name
        context :: Name -> Cxt
        context variablenName = flip AppT (VarT variablenName) . ConT <$> abhängigkeiten
        funDeps :: [FunDep]
        funDeps = []
        deklarationen :: Name -> Name -> [Dec]
        deklarationen variablenName typName
            = [funktionSignatur, defaultSignatur, defaultImplementierung] <*> [variablenName] <*> [typName]
        funktionName :: Name
        funktionName = mkName $ "erhalte" ++ name
        funktionTyp :: Name -> Name -> Type
        funktionTyp variablenName typName = AppT (AppT ArrowT $ VarT variablenName) $ ConT typName
        funktionSignatur :: Name -> Name -> Dec
        funktionSignatur variablenName typName = SigD funktionName $ funktionTyp variablenName typName
        klassenNameGtk :: Name
        klassenNameGtk = mkName $ namePräfixGtk ++ name ++ "Class"
        defaultSignatur :: Name -> Name -> Dec
        defaultSignatur variablenName typName
            = DefaultSigD funktionName $ ForallT [] [AppT (ConT klassenNameGtk) $ VarT variablenName] $
                funktionTyp variablenName typName
        defaultNameGtk :: Name
        defaultNameGtk = mkName $ namePräfixGtk ++ "to" ++ name
        defaultImplementierung :: Name -> Name -> Dec
        defaultImplementierung _variablenName _typName
            = ValD (VarP funktionName) (NormalB $ VarE defaultNameGtk) []
        instanzDeklaration :: Name -> Dec
        instanzDeklaration variablenName
            = InstanceD (Just Overlappable) [AppT (ConT klassenNameGtk) $ VarT variablenName] (AppT (ConT klassenName) $ VarT variablenName) []
        mitFunktionName :: Name
        mitFunktionName = mkName $ "mit" ++ name
        erzeugeMitFunktionSignatur :: Q Dec
        erzeugeMitFunktionSignatur = do
            m <- newName "m"
            a <- newName "a"
            isW <- newName "isW"
            variablenName <- newName "w"
            pure
                $ SigD mitFunktionName
                    $ ForallT
                        [PlainTV m, PlainTV a, PlainTV variablenName]
                        [AppT (ConT klassenName) $ VarT variablenName]
                        $ AppT
                            (AppT ArrowT (ForallT [PlainTV isW] [AppT (ConT klassenNameGtk) (VarT isW)] $ AppT (AppT ArrowT $ VarT isW) $ AppT (VarT m) (VarT a)))
                            $ AppT
                                (AppT ArrowT (VarT variablenName))
                                $ AppT (VarT m) (VarT a)
        erzeugeMitFunktionDeklaration :: Q Dec
        erzeugeMitFunktionDeklaration = do
            fun <- newName "fun"
            pure $ FunD mitFunktionName [Clause [VarP fun] (NormalB $ UInfixE (VarE fun) (VarE $ mkName ".") (VarE funktionName)) []]
        firstToLower :: String -> String
        firstToLower    []      = []
        firstToLower    (h : t) = toLower h : t
        getterName :: Name
        getterName = mkName $ firstToLower name
        erzeugeGetterSignatur :: Name -> Q Dec
        erzeugeGetterSignatur typName = do
            variablenName <- newName "s"
            pure
                $ SigD getterName
                    $ ForallT
                        [PlainTV variablenName]
                        [AppT (ConT klassenName) $ VarT variablenName]
                        $ AppT (AppT (ConT $ mkName "Lens.Getter") (VarT variablenName)) (ConT typName)
        getterDeklaration :: Dec
        getterDeklaration = ValD (VarP getterName) (NormalB $ AppE (VarE $ mkName "Lens.to") (VarE funktionName)) []