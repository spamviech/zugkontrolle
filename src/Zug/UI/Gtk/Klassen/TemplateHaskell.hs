{-# LANGUAGE LambdaCase #-}

{-|
Description: Erzeuge Typklassen angelehnt an "GI.Gtk"-Typklassen
-}
module Zug.UI.Gtk.Klassen.TemplateHaskell (erzeugeKlasse) where

import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import qualified Language.Haskell.TH as TH

abortWithError :: String -> TH.Q a
abortWithError msg = TH.reportError msg >> undefined

-- | Erzeuge Klasse /Mit<name>/, sowie eine default-Implementierung über /DefaultSignatures/.
erzeugeKlasse :: [TH.Name] -> String -> TH.Q [TH.Dec]
erzeugeKlasse abhängigkeiten name = do
    typName <- TH.lookupTypeName nameMitPräfixGtk >>= \case
        (Just typName) -> TH.reify typName >>= \case
            (TH.TyConI _dec) -> pure typName
            _otherwise -> abortWithError $ '"' : nameMitPräfixGtk ++ "\" ist kein Typ."
        Nothing -> abortWithError $ '"' : nameMitPräfixGtk ++ "\" ist kein bekannter Name."
    variablenName <- TH.newName "widget"
    instanzDeklarationen <- erzeugeInstanzDeklarationen
    eitherInstanzDeklaration <- erzeugeEitherInstanzDeklaration
    mitFunktionSignatur <- erzeugeMitFunktionSignatur
    mitFunktionDeklaration <- erzeugeMitFunktionDeklaration
    getterSignatur <- erzeugeGetterSignatur typName
    pure
        $ [ TH.ClassD
                (context variablenName)
                klassenName
                [TH.PlainTV variablenName]
                funDeps
                $deklarationen
                variablenName
                typName
          , eitherInstanzDeklaration
          , mitFunktionSignatur
          , mitFunktionDeklaration
          , getterSignatur
          , getterDeklaration]
        ++ instanzDeklarationen
    where
        namePräfixGtk :: String
        namePräfixGtk = "Gtk."

        nameMitPräfixGtk :: String
        nameMitPräfixGtk = namePräfixGtk ++ name

        klassenName :: TH.Name
        klassenName = TH.mkName $ "Mit" ++ name

        context :: TH.Name -> TH.Cxt
        context variablenName = flip TH.AppT (TH.VarT variablenName) . TH.ConT <$> abhängigkeiten

        funDeps :: [TH.FunDep]
        funDeps = []

        deklarationen :: TH.Name -> TH.Name -> [TH.Dec]
        deklarationen variablenName typName =
            [funktionSignatur, defaultSignatur, defaultImplementierung]
            <*> [variablenName]
            <*> [typName]

        funktionName :: TH.Name
        funktionName = TH.mkName $ "erhalte" ++ name

        funktionTyp :: TH.Name -> TH.Name -> TH.Type
        funktionTyp variablenName typName =
            TH.AppT (TH.AppT TH.ArrowT $ TH.VarT variablenName) $ TH.ConT typName

        funktionSignatur :: TH.Name -> TH.Name -> TH.Dec
        funktionSignatur variablenName typName =
            TH.SigD funktionName $ funktionTyp variablenName typName

        klassenNameGtk :: TH.Name
        klassenNameGtk = TH.mkName $ namePräfixGtk ++ "Is" ++ name

        defaultSignatur :: TH.Name -> TH.Name -> TH.Dec
        defaultSignatur variablenName typName =
            TH.DefaultSigD funktionName
            $ TH.ForallT [] [TH.AppT (TH.ConT klassenNameGtk) $ TH.VarT variablenName]
            $ funktionTyp variablenName typName

        defaultNameGtk :: TH.Name
        defaultNameGtk = TH.mkName $ namePräfixGtk ++ "to" ++ name

        defaultImplementierung :: TH.Name -> TH.Name -> TH.Dec
        defaultImplementierung _variablenName _typName =
            TH.ValD (TH.VarP funktionName) (TH.NormalB $ TH.VarE defaultNameGtk) []

        erzeugeInstanzDeklarationen :: TH.Q [TH.Dec]
        erzeugeInstanzDeklarationen =
            mapMaybe erzeugeInstanzDeklaration
            <$> TH.reifyInstances klassenNameGtk [TH.VarT $ TH.mkName "a"]

        erzeugeInstanzDeklaration :: TH.InstanceDec -> Maybe TH.Dec
        erzeugeInstanzDeklaration
            (TH.InstanceD _maybeOverlap _cxt (TH.AppT (TH.ConT _klassenNameGtk) ty) _decs) =
            Just $ TH.InstanceD Nothing [] (TH.AppT (TH.ConT klassenName) ty) []
        erzeugeInstanzDeklaration _instanceDec = Nothing

        erzeugeEitherInstanzDeklaration :: TH.Q TH.Dec
        erzeugeEitherInstanzDeklaration = do
            aName <- TH.newName "a"
            bName <- TH.newName "b"
            valName <- TH.newName "value"
            pure
                $ TH.InstanceD
                    (Just TH.Overlappable)
                    [ TH.AppT (TH.ConT klassenName) $ TH.VarT aName
                    , TH.AppT (TH.ConT klassenName) $ TH.VarT bName]
                    (TH.AppT (TH.ConT klassenName)
                     $ TH.ParensT
                     $ TH.AppT (TH.AppT (TH.ConT $ TH.mkName "Either") $ TH.VarT aName)
                     $ TH.VarT bName)
                    [ TH.FunD
                          funktionName
                          [ TH.Clause
                                [TH.ConP (TH.mkName "Left") [TH.VarP valName]]
                                (TH.NormalB $ TH.AppE (TH.VarE funktionName) $ TH.VarE valName)
                                []
                          , TH.Clause
                                [TH.ConP (TH.mkName "Right") [TH.VarP valName]]
                                (TH.NormalB $ TH.AppE (TH.VarE funktionName) $ TH.VarE valName)
                                []]]

        mitFunktionName :: TH.Name
        mitFunktionName = TH.mkName $ "mit" ++ name

        erzeugeMitFunktionSignatur :: TH.Q TH.Dec
        erzeugeMitFunktionSignatur = do
            b <- TH.newName "b"
            isW <- TH.newName "isW"
            variablenName <- TH.newName "w"
            pure
                $ TH.SigD mitFunktionName
                $ TH.ForallT
                    [TH.PlainTV b, TH.PlainTV variablenName]
                    [TH.AppT (TH.ConT klassenName) $ TH.VarT variablenName]
                $ TH.AppT
                    (TH.AppT TH.ArrowT
                     $ TH.ForallT [TH.PlainTV isW] [TH.AppT (TH.ConT klassenNameGtk) (TH.VarT isW)]
                     $ TH.AppT (TH.AppT TH.ArrowT $ TH.VarT isW) (TH.VarT b))
                $ TH.AppT (TH.AppT TH.ArrowT (TH.VarT variablenName)) (TH.VarT b)

        erzeugeMitFunktionDeklaration :: TH.Q TH.Dec
        erzeugeMitFunktionDeklaration = do
            fun <- TH.newName "fun"
            pure
                $ TH.FunD
                    mitFunktionName
                    [ TH.Clause
                          [TH.VarP fun]
                          (TH.NormalB
                           $ TH.UInfixE
                               (TH.VarE fun)
                               (TH.VarE $ TH.mkName ".")
                               (TH.VarE funktionName))
                          []]

        firstToLower :: String -> String
        firstToLower [] = []
        firstToLower (h:t) = toLower h : t

        getterName :: TH.Name
        getterName = TH.mkName $ firstToLower name

        erzeugeGetterSignatur :: TH.Name -> TH.Q TH.Dec
        erzeugeGetterSignatur typName = do
            variablenName <- TH.newName "s"
            pure
                $ TH.SigD getterName
                $ TH.ForallT
                    [TH.PlainTV variablenName]
                    [TH.AppT (TH.ConT klassenName) $ TH.VarT variablenName]
                $ TH.AppT
                    (TH.AppT (TH.ConT $ TH.mkName "Lens.Getter") (TH.VarT variablenName))
                    (TH.ConT typName)

        getterDeklaration :: TH.Dec
        getterDeklaration =
            TH.ValD
                (TH.VarP getterName)
                (TH.NormalB $ TH.AppE (TH.VarE $ TH.mkName "Lens.to") (TH.VarE funktionName))
                []
