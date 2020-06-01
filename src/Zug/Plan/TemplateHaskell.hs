{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Description: Template-Haskell-Werte für Verwendung in 'Zug.Derive.Ord.deriveOrd'.
-}
module Zug.Plan.TemplateHaskell
  ( aktionBahngeschwindigkeitCxtType
  , aktionAllgemeinCxtType
  , planAllgemeinCxtType
  ) where

import qualified Language.Haskell.TH as TH

import Zug.Enums (Zugtyp(..), GeschwindigkeitVariante(..))

bgName :: TH.Name
bgName = TH.mkName "bg"

stName :: TH.Name
stName = TH.mkName "st"

weName :: TH.Name
weName = TH.mkName "we"

kuName :: TH.Name
kuName = TH.mkName "ku"

koName :: TH.Name
koName = TH.mkName "ko"

wsName :: TH.Name
wsName = TH.mkName "ws"

gName :: TH.Name
gName = TH.mkName "g"

zName :: TH.Name
zName = TH.mkName "z"

ordInstance :: TH.Type -> TH.Pred
ordInstance = TH.AppT $ TH.ConT ''Ord

-- | Kontext und Typ für Ord-Instanz von 'Zug.Plan.AktionBahngeschwindigkeit'.
aktionBahngeschwindigkeitCxtType :: (TH.Cxt, TH.Type)
aktionBahngeschwindigkeitCxtType = (cxt, ty)
    where
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

aktionVarCxt :: TH.Cxt
aktionVarCxt = map (\name -> ordInstance $ TH.VarT name) [stName, kuName, koName]

aktionZugtypTypen :: [TH.Type]
aktionZugtypTypen =
    [ TH.AppT (TH.VarT bgName) $ TH.PromotedT 'Pwm
    , TH.AppT (TH.VarT bgName) $ TH.PromotedT 'KonstanteSpannung
    , TH.VarT weName
    , TH.VarT wsName]

aktionMärklinCxt :: TH.Cxt
aktionMärklinCxt =
    map (\ty -> ordInstance $ TH.AppT ty $ TH.PromotedT 'Märklin) aktionZugtypTypen

aktionLegoCxt :: TH.Cxt
aktionLegoCxt = map (\ty -> ordInstance $ TH.AppT ty $ TH.PromotedT 'Lego) aktionZugtypTypen

aktionCxt :: TH.Cxt
aktionCxt = aktionMärklinCxt <> aktionLegoCxt <> aktionVarCxt

aktionType :: String -> TH.Type
aktionType name =
    TH.AppT
        (TH.AppT
             (TH.AppT
                  (TH.AppT
                       (TH.AppT (TH.AppT (TH.ConT $ TH.mkName name) $ TH.VarT bgName)
                        $ TH.VarT stName)
                   $ TH.VarT weName)
              $ TH.VarT kuName)
         $ TH.VarT koName)
    $ TH.VarT wsName

-- | Kontext und Typ für Ord-Instanz von 'Zug.Plan.AktionAllgemein'.
aktionAllgemeinCxtType :: (TH.Cxt, TH.Type)
aktionAllgemeinCxtType = (aktionCxt, aktionType "AktionAllgemein")

-- | Kontext und Typ für Ord-Instanz von 'Zug.Plan.PlanAllgemein'.
planAllgemeinCxtType :: (TH.Cxt, TH.Type)
planAllgemeinCxtType = (aktionCxt, aktionType "PlanAllgemein")
