{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Description: TemplateHaskell-deriving von Ord für GADTs.
-}
module Zug.DeriveOrd (deriveOrd) where

import Control.Monad (forM, foldM)
import Data.Either (partitionEithers)
import qualified Language.Haskell.TH as TH

-- | Automatische deriving von 'Ord' schlägt bei GADTs mit mehreren (Phantom)-Typen schlägt fehl.
-- Diese TemplateHaskell-Funktion bietet einen Ersatz, wobei ein Vergleich nur bei
-- kompatiblen Typen erfolgt.
deriveOrd :: TH.Name -> TH.Q [TH.Dec]
deriveOrd name = TH.reify name >>= \case
    (TH.TyConI tyDec) -> do
        (tyVarBndrs, cons) <- case tyDec of
            (TH.DataD _cxt _name tyVarBndrs Nothing cons _deriveClauses) -> pure (tyVarBndrs, cons)
            (TH.DataD _cxt _name _tyVarBndrs (Just kind) _cons _deriveClauses)
                -> fail $ "Nicht unterstützter Kind: " ++ show kind
            (TH.NewtypeD _cxt _name tyVarBndrs Nothing con _deriveClauses)
                -> pure (tyVarBndrs, [con])
            (TH.NewtypeD _cxt _name _tyVarBndrs (Just kind) _con _deriveClauses)
                -> fail $ "Nicht unterstützter Kind: " ++ show kind
            _tyDec -> fail $ '"' : show name ++ "\" hat unerwartete TyConI: " ++ show tyDec
        ty <- makeType name tyVarBndrs
        case partitionEithers $ map conInfo cons of
            ([], []) -> pure [TH.InstanceD Nothing [] ty []]
            (conInfos, []) -> do
                clauses <- compareClauses conInfos
                pure [TH.InstanceD Nothing [] ty $ [TH.FunD 'compare clauses]]
            (_conInfos, fails) -> fail $ show fails
    info -> fail $ '"' : show name ++ "\" ist kein Typ: " ++ show info
    where
        makeType :: TH.Name -> [TH.TyVarBndr] -> TH.Q TH.Type
        makeType name tyVarBndrs = do
            (ty, names) <- foldM applyBndr (TH.ConT name, []) tyVarBndrs
            pure $ TH.ForallT (map TH.PlainTV names) [] $ TH.AppT (TH.ConT ''Ord) ty

        applyBndr :: (TH.Type, [TH.Name]) -> TH.TyVarBndr -> TH.Q (TH.Type, [TH.Name])
        applyBndr (ty, names) tyVarBndr = do
            n <- TH.newName $ show $ bndrName tyVarBndr
            pure (TH.AppT ty (TH.VarT n), n : names)

        bndrName :: TH.TyVarBndr -> TH.Name
        bndrName (TH.PlainTV name) = name
        bndrName (TH.KindedTV name _kind) = name

        conInfo :: TH.Con -> Either (TH.Name, [TH.Type], Maybe TH.Type) String
        conInfo (TH.NormalC name vars) = Left (name, map snd vars, Nothing)
        conInfo (TH.RecC name vars) = Left (name, map third vars, Nothing)
        conInfo (TH.InfixC (_bangL, varL) name (_bangR, varR)) = Left (name, [varL, varR], Nothing)
        conInfo (TH.ForallC _tyVarBndr _cxt con) = conInfo con
        conInfo (TH.GadtC [name] vars ty) = Left (name, map snd vars, Just ty)
        conInfo (TH.RecGadtC [name] vars ty) = Left (name, map third vars, Just ty)
        conInfo con = Right $ "Gadt-Konstruktor mit mehr als einem Namen: " ++ show con

        third :: (a, b, c) -> c
        third (_a, _b, c) = c

        compareClauses :: [(TH.Name, [TH.Type], Maybe TH.Type)] -> TH.Q [TH.Clause]
        compareClauses [] = pure []
        compareClauses (h@(conName, vars, _maybeType):t) = do
            varClause <- compareVars conName vars
            tailClauses <- compareClauses t
            pure $ varClause : foldMap (compareCons h) t ++ tailClauses

        compareVars :: TH.Name -> [a] -> TH.Q TH.Clause
        compareVars conName vars = do
            varNames0 <- forM vars $ const $ TH.newName "a"
            varNames1 <- forM vars $ const $ TH.newName "b"
            exp <- compareVarExp $ zip varNames0 varNames1
            pure
                $ TH.Clause
                    [ TH.ConP conName $ map TH.VarP varNames0
                    , TH.ConP conName $ map TH.VarP varNames1]
                    (TH.NormalB exp)
                    []

        compareVarExp :: [(TH.Name, TH.Name)] -> TH.Q TH.Exp
        compareVarExp [] = pure $ TH.ConE 'EQ
        compareVarExp ((n0, n1):t) = do
            tExp <- compareVarExp t
            varName <- TH.newName "ordering"
            pure
                $ TH.CaseE
                    (TH.AppE (TH.AppE (TH.VarE 'compare) $ TH.VarE n0) $ TH.VarE n1)
                    [ TH.Match (TH.ConP 'EQ []) (TH.NormalB tExp) []
                    , TH.Match (TH.VarP varName) (TH.NormalB $ TH.VarE varName) []]

        compareCons :: (TH.Name, [TH.Type], Maybe TH.Type)
                    -> (TH.Name, [TH.Type], Maybe TH.Type)
                    -> [TH.Clause]
        compareCons (conName0, vars0, maybeType0) (conName1, vars1, maybeType1)
            | typesCompatible maybeType0 maybeType1 =
                [ TH.Clause
                      [ TH.ConP conName0 $ map (const $ TH.WildP) vars0
                      , TH.ConP conName1 $ map (const $ TH.WildP) vars1]
                      (TH.NormalB $ TH.ConE 'GT)
                      []
                , TH.Clause
                      [ TH.ConP conName1 $ map (const $ TH.WildP) vars1
                      , TH.ConP conName0 $ map (const $ TH.WildP) vars0]
                      (TH.NormalB $ TH.ConE 'LT)
                      []]
            | otherwise = []
            where
                typesCompatible :: Maybe TH.Type -> Maybe TH.Type -> Bool
                typesCompatible (Just (TH.VarT _n)) (Just _gadtType1) = True
                typesCompatible (Just _gadtType0) (Just (TH.VarT _n)) = True
                typesCompatible (Just (TH.AppT f0 a0)) (Just (TH.AppT f1 a1)) =
                    typesCompatible (Just f0) (Just f1) && typesCompatible (Just a0) (Just a1)
                typesCompatible (Just gadtType0) (Just gadtType1) = gadtType0 == gadtType1
                typesCompatible _ty0 _ty1 = True

