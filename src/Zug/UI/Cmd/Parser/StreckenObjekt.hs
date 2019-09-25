
{-|
Description: Parsen von 'StreckenObjekt'en
-}
module Zug.UI.Cmd.Parser.StreckenObjekt (
    AnfrageObjekt(..), AnfrageWegstrecke(..), AnfrageWeiche(..), AnfrageBahngeschwindigkeit(..),
    AnfrageStreckenabschnitt(..), AnfrageKupplung(..)) where

-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (Bahngeschwindigkeit(), Streckenabschnitt(), Weiche(), Kupplung(), Wegstrecke())
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), AnfrageFamilie)
import Zug.UI.Cmd.Parser.Plan (AnfragePlan(), anfragePlanAktualisieren)