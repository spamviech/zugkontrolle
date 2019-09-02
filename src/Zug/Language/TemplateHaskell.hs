{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Template-Haskell Funktionen um die Sprachauswahl zu automatisieren.
-}
module Zug.Language.TemplateHaskell (Sprache(..), erzeugeDeklaration, erzeugeFunktionDeklaration) where

import Data.Maybe (catMaybes)
import Language.Haskell.TH (Q, Dec(..), Type(..), Name, Cxt, Clause(..), Pat(..), Body(..), Exp(..), Lit(..),
                            mkName, newName, lookupValueName, reportWarning)
-- Abhängigkeit von anderen Modulen
import Zug.Options (Sprache(..), alleSprachen)

modulName :: Sprache -> String
modulName   Deutsch     = "Zug.Language.DE"
modulName   Englisch    = "Zug.Language.EN"

sprachName :: Sprache -> Name
sprachName  Deutsch     = 'Deutsch
sprachName  Englisch    = 'Englisch

-- | Definiere einen Wert abhängig von der gewählten 'Sprache'.  
-- Falls der Wert in der gewählten 'Sprache' nicht exisitert, versuche ihn in 'Deutsch' (aus dem Modul 'Zug.Language.DE') zu definieren.  
-- Falls der Wert dort ebenfalls nicht existiert wird der Variablenname in Anführungszeichen verwendet.
-- 
-- Es wird erwartet, dass sich die gewählte Sprache in der Variable /gewählteSprache/ befindet.  
-- Außerdem muss das Module "Zug.Language.DE", sowie das Modul mit der gewünschten Sprache importiert werden.
erzeugeDeklaration :: String -> Q [Dec]
erzeugeDeklaration bezeichner = do
    fnDec <- funktionDeklaration
    pure [funktionSignatur, fnDec, wertSignatur, wertDeklaration]
        where
            wertSignatur :: Dec
            wertSignatur = SigD wertName fullType
            funktionSignatur :: Dec
            funktionSignatur = SigD funktionName fullFunctionType
            wertName :: Name
            wertName = mkName bezeichner
            funktionName :: Name
            funktionName = mkName $ bezeichner ++ "S"
            typeVar :: Type
            typeVar = VarT $ mkName "s"
            fullType :: Type
            fullType = ForallT [] constraints typeVar
            functionType :: Type
            functionType = AppT (AppT ArrowT (ConT $ mkName "Sprache")) typeVar
            fullFunctionType :: Type
            fullFunctionType = ForallT [] constraints functionType
            constraints :: Cxt
            constraints = [AppT (ConT $ mkName "Semigroup") typeVar, AppT (ConT $ mkName "IsString") typeVar]
            wertDeklaration :: Dec
            wertDeklaration = ValD (VarP wertName) (NormalB $ AppE (VarE funktionName) (VarE sprachVariable)) []
            sprachVariable :: Name
            sprachVariable = mkName "gewählteSprache"
            funktionDeklaration :: Q Dec
            funktionDeklaration = do
                sprachKlauseln <- mapM testeUndErzeugeKlausel alleSprachen
                -- Erzeuge Wildcard-Pattern für Deutsche Variante (falls vorhanden), ansonsten verwende den Variablen-Namen in Anführungszeichen
                wildcardKlausel <- do
                    -- Überprüfe, ob der Name existiert
                    lookupValueName (qualifizierterName Deutsch) >>= \case
                        Nothing
                            -- Erzeuge Dummy-Deklaration und werfe Warnung
                            -> do
                                reportWarning $ "\"" ++ bezeichner ++ "\" kein bekannter Wert in \"" ++ modulName Deutsch ++ "\". Erzeuge stattdessen eine Dummy-Definition."
                                pure $ [Just $ fallback]
                        Just deutscherName
                            -- Erzeuge Deklaration
                            -> pure [Just $ Clause [WildP] (NormalB $ VarE deutscherName) []]
                let klauseln = sprachKlauseln ++ wildcardKlausel
                -- Verwende nur valide Klauseln
                pure $ FunD funktionName $ catMaybes klauseln
            testeUndErzeugeKlausel :: Sprache -> Q (Maybe Clause)
            testeUndErzeugeKlausel sprache = do
                -- Überprüfe, ob der Name existiert
                lookupValueName (qualifizierterName sprache) >>= \case
                    Nothing
                        -- Erzeuge keine Deklaration für diese Sprache
                        -> pure Nothing
                    Just qualifizierterSprachName
                        -- Erzeuge Deklarationen
                        -> pure $ Just $ Clause [ConP (sprachName sprache) []] (NormalB $ VarE qualifizierterSprachName) []
            fallback :: Clause
            fallback = Clause [WildP] (NormalB $ LitE $ StringL $ '"' : bezeichner ++ "\"") []
            qualifizierterName :: Sprache -> String
            qualifizierterName sprache = modulName sprache ++ '.' : bezeichner

-- | Definiere einen Funktion abhängig von der gewählten 'Sprache'.  
-- Falls die Funktion in der gewählten 'Sprache' nicht exisitert, versuche ihn in 'Deutsch' (aus dem Modul 'Zug.Language.DE') zu definieren.  
-- Falls die Funktion dort ebenfalls nicht existiert werden Anführungszeichen vor und nach der Eingabe hinzugefügt.
-- 
-- Es wird erwartet, dass sich die gewählte Sprache in der Variable /gewählteSprache/ befindet.  
-- Außerdem muss das Module "Zug.Language.DE", sowie das Modul mit der gewünschten Sprache importiert werden.
erzeugeFunktionDeklaration :: String -> Q [Dec]
erzeugeFunktionDeklaration bezeichner = do
    fnDec <- funktionDeklaration
    pure [funktionSignatur, fnDec, wertSignatur, wertDeklaration]
        where
            wertSignatur :: Dec
            wertSignatur = SigD wertName fullType
            funktionSignatur :: Dec
            funktionSignatur = SigD funktionName fullFunctionType
            wertName :: Name
            wertName = mkName bezeichner
            funktionName :: Name
            funktionName = mkName $ bezeichner ++ "S"
            typeVar :: Type
            typeVar = VarT $ mkName "s"
            typeVarFunktion :: Type
            typeVarFunktion = AppT (AppT ArrowT typeVar) typeVar
            fullType :: Type
            fullType = ForallT [] constraints typeVarFunktion
            functionType :: Type
            functionType = AppT (AppT ArrowT (ConT $ mkName "Sprache")) typeVarFunktion
            fullFunctionType :: Type
            fullFunctionType = ForallT [] constraints functionType
            constraints :: Cxt
            constraints = [AppT (ConT $ mkName "Semigroup") typeVar, AppT (ConT $ mkName "IsString") typeVar]
            wertDeklaration :: Dec
            wertDeklaration = ValD (VarP wertName) (NormalB $ AppE (VarE funktionName) (VarE sprachVariable)) []
            sprachVariable :: Name
            sprachVariable = mkName "gewählteSprache"
            funktionDeklaration :: Q Dec
            funktionDeklaration = do
                sprachKlauseln <- mapM testeUndErzeugeKlausel alleSprachen
                -- Erzeuge Wildcard-Pattern für Deutsche Variante (falls vorhanden), ansonsten verwende den Variablen-Namen in Anführungszeichen
                wildcardKlausel <- do
                    -- Überprüfe, ob der Name existiert
                    lookupValueName (qualifizierterName Deutsch) >>= \case
                        Nothing
                        -- Erzeuge Dummy-Deklaration und werfe Warnung
                            -> do
                                reportWarning $ "\"" ++ bezeichner ++ "\" kein bekannter Wert in \"" ++ modulName Deutsch ++ "\". Erzeuge stattdessen eine Dummy-Definition."
                                erzeugeFallback >>= pure . (:[]) . Just
                        Just qualifizierterSprachName
                            -- Erzeuge Deklaration
                            -> pure [Just $ Clause [WildP] (NormalB $ VarE qualifizierterSprachName) []]
                let klauseln = sprachKlauseln ++ wildcardKlausel
                -- Verwende nur valide Klauseln
                pure $ FunD funktionName $ catMaybes klauseln
            erzeugeFallback :: Q Clause
            erzeugeFallback = do
                varName <- newName "eingabe"
                pure $ Clause [WildP] (NormalB $ LamE [VarP varName] $ UInfixE (LitE $ StringL "\"") (VarE $ mkName "<>") $ UInfixE (VarE varName) (VarE $ mkName "<>") (LitE $ StringL "\"")) []
            testeUndErzeugeKlausel :: Sprache -> Q (Maybe Clause)
            testeUndErzeugeKlausel sprache = do
                -- Überprüfe, ob der Name existiert
                lookupValueName (qualifizierterName sprache) >>= \case
                    Nothing
                        -- Erzeuge keine Deklaration für diese Sprache
                        -> pure Nothing
                    Just qualifizierterSprachName
                    -- Erzeuge Deklarationen
                        -> pure $ Just $ Clause [ConP (sprachName sprache) []] (NormalB $ VarE qualifizierterSprachName) []
            qualifizierterName :: Sprache -> String
            qualifizierterName sprache = modulName sprache ++ '.' : bezeichner