--{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Description : Template-Haskell Funktionen um die Sprachauswahl zu automatisieren.
-}
module Zug.Language.TemplateHaskell (Sprache(..), erzeugeDeklaration, erzeugeFunktionDeklaration) where

import Control.Monad (foldM)
import Data.Maybe (catMaybes)
import Language.Haskell.TH
-- Abhängigkeit von anderen Modulen
import Zug.Options

modulName :: Sprache -> String
modulName (Deutsch)     = "Zug.Language.DE"
modulName (Englisch)    = "Zug.Language.EN"

sprachName :: Sprache -> Name
sprachName  (Deutsch)   = mkName "Deutsch"
sprachName  (Englisch)  = mkName "Englisch"

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
                wildcardKlausel <- recover (pure $ [Just $ Clause [WildP] (NormalB $ LitE $ StringL $ '"' : bezeichner ++ "\"") []]) $ do
                    -- Überprüfe, ob der Name existiert
                    reify $ qualifizierterName Deutsch
                    -- Erzeuge Deklaration
                    pure [Just $ Clause [WildP] (NormalB $ VarE $ qualifizierterName Deutsch) []]
                let klauseln = sprachKlauseln ++ wildcardKlausel
                -- Verwende nur valide Klauseln
                pure $ FunD funktionName $ catMaybes klauseln
            testeUndErzeugeKlausel :: Sprache -> Q (Maybe Clause)
            testeUndErzeugeKlausel sprache = do
                recover (pure Nothing) $ do
                    -- Überprüfe, ob der Name existiert
                    reify $ qualifizierterName sprache
                    -- Erzeuge Deklarationen
                    pure $ Just $ Clause [ConP (sprachName sprache) []] (NormalB $ VarE $ qualifizierterName sprache) []
            qualifizierterName :: Sprache -> Name
            qualifizierterName sprache = mkName $ modulName sprache ++ '.':bezeichner

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
                wildcardKlausel <- recover (erzeugeFallback >>= pure . (: []) . Just) $ do
                    -- Überprüfe, ob der Name existiert
                    reify $ qualifizierterName Deutsch
                    -- Erzeuge Deklaration
                    pure [Just $ Clause [WildP] (NormalB $ VarE $ qualifizierterName Deutsch) []]
                let klauseln = sprachKlauseln ++ wildcardKlausel
                -- Verwende nur valide Klauseln
                pure $ FunD funktionName $ catMaybes klauseln
            erzeugeFallback :: Q Clause
            erzeugeFallback = do
                varName <- newName "eingabe"
                pure $ Clause [WildP] (NormalB $ LamE [VarP varName] $ UInfixE (LitE $ StringL "\"") (VarE $ mkName "<>") $ UInfixE (VarE varName) (VarE $ mkName "<>") (LitE $ StringL "\"")) []
            testeUndErzeugeKlausel :: Sprache -> Q (Maybe Clause)
            testeUndErzeugeKlausel sprache = do
                recover (pure Nothing) $ do
                    -- Überprüfe, ob der Name existiert
                    reify $ qualifizierterName sprache
                    -- Erzeuge Deklarationen
                    pure $ Just $ Clause [ConP (sprachName sprache) []] (NormalB $ VarE $ qualifizierterName sprache) []
            qualifizierterName :: Sprache -> Name
            qualifizierterName sprache = mkName $ modulName sprache ++ '.':bezeichner