{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Template-Haskell Funktionen um die Sprachauswahl zu automatisieren.
-}
module Zug.Language.TemplateHaskell (Sprache(..), erzeugeFunktion) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Language.Haskell.TH as TH
-- Abhängigkeit von anderen Modulen
import Zug.Language.Operatoren (Sprache(..), alleSprachen)

modulName :: Sprache -> String
modulName   Deutsch     = "Zug.Language.DE"
modulName   Englisch    = "Zug.Language.EN"

sprachName :: Sprache -> TH.Name
sprachName  Deutsch     = 'Deutsch
sprachName  Englisch    = 'Englisch

erzeugeFunktion :: String -> TH.Q [TH.Dec]
erzeugeFunktion bezeichner
    = TH.lookupValueName (qualifizierterName Deutsch) >>= \case
        Nothing
            -> do
                TH.reportWarning $
                    "\"" ++ bezeichner ++ "\" kein bekannter Wert in \"" ++ modulName Deutsch ++ "\". " ++
                    "Erzeuge stattdessen eine Dummy-Definition."
                pure fallback
        (Just nameDeutsch)
            -> TH.reify nameDeutsch >>= \case
                (TH.VarI _name typDeutsch _maybeDec)
                    -> do
                        sprachKlauseln <- mapM testeUndErzeugeKlausel alleSprachen
                        let klauseln = sprachKlauseln ++ [Just $ wildcardKlausel nameDeutsch]
                        -- Verwende nur valide Klauseln
                        pure [
                            signatur name typDeutsch,
                            TH.FunD name $ catMaybes klauseln]
                info
                    -> do
                        TH.reportWarning $
                            "\"" ++ bezeichner ++
                            "\" keine Variable in \"" ++ modulName Deutsch ++ "\" " ++
                            "(" ++ show info ++ "). " ++
                            "Erzeuge stattdessen eine Dummy-Definition."
                        pure fallback
    where
        name :: TH.Name
        name = TH.mkName bezeichner
        qualifizierterName :: Sprache -> String
        qualifizierterName sprache = modulName sprache ++ '.' : bezeichner
        signatur :: TH.Name -> TH.Type -> TH.Dec
        signatur
            name
            (TH.ForallT tyVars cxt typSprache)
                = TH.SigD name $ TH.ForallT tyVars cxt $ TH.AppT (TH.AppT TH.ArrowT $ TH.ConT ''Sprache) typSprache
        signatur
            name
            typSprache
                = TH.SigD name $ TH.AppT (TH.AppT TH.ArrowT $ TH.ConT ''Sprache) typSprache
        testeUndErzeugeKlausel :: Sprache -> TH.Q (Maybe TH.Clause)
        testeUndErzeugeKlausel sprache
            = TH.lookupValueName (qualifizierterName sprache) >>= \case
                Nothing
                    -- Erzeuge keine Deklaration für diese Sprache und werfe Warnung
                    -> do
                        TH.reportWarning $ "\"" ++ bezeichner ++ "\" kein bekannter Wert in \"" ++ modulName sprache ++ "\". Erzeuge stattdessen eine Dummy-Definition."
                        pure Nothing
                Just qualifizierterSprachName
                    -- Erzeuge Deklarationen
                    -> pure $ Just $
                        TH.Clause [TH.ConP (sprachName sprache) []] (TH.NormalB $ TH.VarE qualifizierterSprachName) []
        wildcardKlausel :: TH.Name -> TH.Clause
        wildcardKlausel nameDeutsch = TH.Clause [TH.WildP] (TH.NormalB $ TH.VarE nameDeutsch) []
        fallback :: [TH.Dec]
        fallback = [
            TH.SigD name $ TH.AppT (TH.AppT TH.ArrowT $ TH.ConT ''Sprache) $ TH.ConT ''Text,
            TH.ValD
                (TH.VarP name)
                (TH.NormalB $ TH.AppE (TH.VarE 'const) $ TH.LitE $ TH.StringL $ '"' : bezeichner ++ "\"")
                []]