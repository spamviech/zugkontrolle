{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Description: Erzeuge Typklassen angelehnt an "GI.Gtk"-Typklassen.
-}
module Zug.UI.Gtk.Klassen.TemplateHaskell (erzeugeKlasse) where

import Control.Monad.Trans (MonadIO())
-- import Data.Maybe (mapMaybe)
import qualified Language.Haskell.TH as TH

abortWithError :: String -> TH.Q a
abortWithError msg = TH.reportError msg >> error "after TH.reportError"

-- | Erzeuge Klasse /Mit<name>/, sowie eine default-Implementierung über /DefaultSignatures/.
erzeugeKlasse :: [String] -> String -> TH.Q [TH.Dec]
erzeugeKlasse abhängigkeiten name = do
    typName <- TH.lookupTypeName nameMitPräfixGtk >>= \case
        (Just typName) -> TH.reify typName >>= \case
            (TH.TyConI _dec) -> pure typName
            _otherwise -> abortWithError $ '"' : nameMitPräfixGtk ++ "\" ist kein Typ."
        Nothing -> abortWithError $ '"' : nameMitPräfixGtk ++ "\" ist kein bekannter Name."
    variablenName <- TH.newName "widget"
    instanzDeklarationen <- erzeugeInstanzDeklarationen
    eitherInstanzDeklaration <- erzeugeEitherInstanzDeklaration
    decls <- deklarationen variablenName typName
    pure
        $ [ TH.ClassD (context variablenName) klassenName [TH.PlainTV variablenName] funDeps decls
          , eitherInstanzDeklaration]
        ++ instanzDeklarationen
    where
        namePräfixGtk :: String
        namePräfixGtk = "Gtk."

        nameMitPräfixGtk :: String
        nameMitPräfixGtk = namePräfixGtk ++ name

        klassenName :: TH.Name
        klassenName = alsKlassenName name

        alsKlassenName :: String -> TH.Name
        alsKlassenName = TH.mkName . ("Mit" ++)

        context :: TH.Name -> TH.Cxt
        context variablenName =
            flip TH.AppT (TH.VarT variablenName) . TH.ConT . alsKlassenName <$> abhängigkeiten

        funDeps :: [TH.FunDep]
        funDeps = []

        deklarationen :: TH.Name -> TH.Name -> TH.Q [TH.Dec]
        deklarationen variablenName typName =
            sequence
            $ [funktionSignatur, defaultSignatur, curry $ pure . uncurry defaultImplementierung]
            <*> [variablenName]
            <*> [typName]

        funktionName :: TH.Name
        funktionName = TH.mkName $ "erhalte" ++ name

        funktionTyp :: TH.Name -> TH.Name -> TH.Q TH.Type
        funktionTyp variablenName typName = do
            mName <- TH.newName "m"
            pure
                $ TH.ForallT [] [TH.AppT (TH.ConT ''MonadIO) $ TH.VarT mName]
                $ TH.AppT (TH.AppT TH.ArrowT $ TH.VarT variablenName)
                $ TH.AppT (TH.VarT mName)
                $ TH.ConT typName

        funktionSignatur :: TH.Name -> TH.Name -> TH.Q TH.Dec
        funktionSignatur variablenName typName =
            TH.SigD funktionName <$> funktionTyp variablenName typName

        klassenNameGtk :: TH.Name
        klassenNameGtk = alsKlassenNameGtk name

        alsKlassenNameGtk :: String -> TH.Name
        alsKlassenNameGtk = TH.mkName . ((namePräfixGtk ++ "Is") ++)

        defaultSignatur :: TH.Name -> TH.Name -> TH.Q TH.Dec
        defaultSignatur variablenName typName = do
            TH.DefaultSigD funktionName
                . TH.ForallT [] [TH.AppT (TH.ConT klassenNameGtk) $ TH.VarT variablenName]
                <$> funktionTyp variablenName typName

        defaultNameGtk :: TH.Name
        defaultNameGtk = TH.mkName $ namePräfixGtk ++ "to" ++ name

        defaultImplementierung :: TH.Name -> TH.Name -> TH.Dec
        defaultImplementierung _variablenName _typName =
            TH.ValD (TH.VarP funktionName) (TH.NormalB $ TH.VarE defaultNameGtk) []

        erzeugeInstanzDeklarationen :: TH.Q [TH.Dec]
        erzeugeInstanzDeklarationen = do
            oName <- TH.newName "o"
            let oType = TH.VarT oName
            pure
                [ TH.InstanceD
                      (Just TH.Overlappable)
                      (TH.AppT (TH.ConT klassenNameGtk) oType
                       : (flip TH.AppT oType . TH.ConT . alsKlassenNameGtk <$> abhängigkeiten))
                      (TH.AppT (TH.ConT klassenName) oType)
                      []]

        -- erzeugeInstanzDeklarationen = do
        --     instances <- TH.reifyInstances klassenNameGtk [TH.VarT $ TH.mkName "a"]
        --     TH.reportError $ show instances
        --     mapMaybe erzeugeInstanzDeklaration
        --         <$> TH.reifyInstances klassenNameGtk [TH.VarT $ TH.mkName "a"]
        -- erzeugeInstanzDeklaration :: TH.InstanceDec -> Maybe TH.Dec
        -- erzeugeInstanzDeklaration
        --     (TH.InstanceD _maybeOverlap _cxt (TH.AppT (TH.ConT _klassenNameGtk) ty) _decs) =
        --     Just $ TH.InstanceD Nothing [] (TH.AppT (TH.ConT klassenName) ty) []
        -- erzeugeInstanzDeklaration _instanceDec = Nothing
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
