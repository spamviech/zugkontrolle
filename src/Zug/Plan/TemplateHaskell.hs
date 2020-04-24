{-# LANGUAGE TemplateHaskellQuotes #-}

module Zug.Plan.TemplateHaskell (aktionBahngeschwindigkeitCxtType) where

import qualified Language.Haskell.TH as TH

import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..))

aktionBahngeschwindigkeitCxtType :: (TH.Cxt, TH.Type)
aktionBahngeschwindigkeitCxtType = (cxt, ty)
    where
        bgName :: TH.Name
        bgName = TH.mkName "bg"

        gName :: TH.Name
        gName = TH.mkName "g"

        zName :: TH.Name
        zName = TH.mkName "z"

        ordInstance :: TH.Type -> TH.Pred
        ordInstance = TH.AppT $ TH.ConT ''Ord

        varCxt :: TH.Pred
        varCxt = ordInstance $ TH.AppT (TH.AppT (TH.VarT bgName) $ TH.VarT gName) $ TH.VarT zName

        pwmCxt :: TH.Pred
        pwmCxt =
            ordInstance $ TH.AppT (TH.AppT (TH.VarT bgName) $ TH.PromotedT 'Pwm) $ TH.VarT zName

        konstanteSpannungCxt :: TH.Pred
        konstanteSpannungCxt =
            ordInstance
            $ TH.AppT (TH.AppT (TH.VarT bgName) $ TH.PromotedT 'KonstanteSpannung)
            $ TH.VarT zName

        märklinCxt :: TH.Pred
        märklinCxt =
            ordInstance
            $ TH.AppT (TH.AppT (TH.VarT bgName) $ TH.VarT gName)
            $ TH.PromotedT 'Märklin

        legoCxt :: TH.Pred
        legoCxt =
            ordInstance $ TH.AppT (TH.AppT (TH.VarT bgName) $ TH.VarT gName) $ TH.PromotedT 'Lego

        cxt :: TH.Cxt
        cxt = [varCxt, pwmCxt, konstanteSpannungCxt, märklinCxt, legoCxt]

        ty :: TH.Type
        ty =
            TH.AppT
                (TH.AppT
                     (TH.AppT (TH.ConT $ TH.mkName "AktionBahngeschwindigkeit") $ TH.VarT bgName)
                 $ TH.VarT gName)
            $ TH.VarT zName