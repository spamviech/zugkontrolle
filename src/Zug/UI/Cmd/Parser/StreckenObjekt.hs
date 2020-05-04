{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description: Parsen von 'StreckenObjekt'en.
-}
module Zug.UI.Cmd.Parser.StreckenObjekt
  ( -- * Hilfstypen
    AnfrageZugtyp(..)
  , AnfrageZugtypEither(..)
  , AnfrageZugtypKlasse(..)
    -- * StreckenObjekte
    -- ** Bahngeschwindigkeit
  , AnfrageBahngeschwindigkeit(..)
    -- ** Weiche
  , AnfrageWeiche(..)
    -- ** Streckenabschnitt
  , AnfrageStreckenabschnitt(..)
    -- ** Kupplung
  , AnfrageKupplung(..)
    -- ** Kontakt
  , AnfrageKontakt(..)
    -- ** Wegstrecke
  , AnfrageWegstrecke(..)
    -- ** Objekt
  , AnfrageObjekt(..)
  ) where

import Data.Text (Text)

import Zug.Enums (ZugtypEither(..), GeschwindigkeitEither(..))
import Zug.Language (Anzeige(..), Sprache(), toBefehlsString)
import qualified Zug.Language as Language
import Zug.Objekt (Objekt, ObjektAllgemein(..))
import Zug.UI.Cmd.Lexer (EingabeToken())
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage
       (Anfrage(..), MitAnfrage(..), AnfrageZugtyp(..), AnfrageZugtypKlasse(..)
      , AnfrageZugtypEither(..), AnfrageGeschwindigkeitEither(..), AnfrageFortsetzung(..)
      , wähleZwischenwert, ($<<), anfrageAktualisierenZugtyp)
import Zug.UI.Cmd.Parser.Plan (AnfragePlan(..))
import Zug.UI.Cmd.Parser.StreckenObjekt.Bahngeschwindigkeit (AnfrageBahngeschwindigkeit(..))
import Zug.UI.Cmd.Parser.StreckenObjekt.Kontakt (AnfrageKontakt(..))
import Zug.UI.Cmd.Parser.StreckenObjekt.Kupplung (AnfrageKupplung(..))
import Zug.UI.Cmd.Parser.StreckenObjekt.Streckenabschnitt (AnfrageStreckenabschnitt(..))
import Zug.UI.Cmd.Parser.StreckenObjekt.Wegstrecke (AnfrageWegstrecke(..))
import Zug.UI.Cmd.Parser.StreckenObjekt.Weiche (AnfrageWeiche(..))

-- | Unvollständige Objekte
data AnfrageObjekt
    = AnfrageObjekt
    | AOBahngeschwindigkeit (AnfrageZugtypEither (AnfrageGeschwindigkeitEither AnfrageBahngeschwindigkeit))
    | AOStreckenabschnitt AnfrageStreckenabschnitt
    | AOWeiche (AnfrageZugtypEither AnfrageWeiche)
    | AOKupplung AnfrageKupplung
    | AOKontakt AnfrageKontakt
    | AOWegstrecke (AnfrageZugtypEither AnfrageWegstrecke)
    | AOPlan AnfragePlan
    deriving (Show)

instance Anzeige AnfrageObjekt where
    anzeige :: AnfrageObjekt -> Sprache -> Text
    anzeige AnfrageObjekt = Language.objekt
    anzeige (AOBahngeschwindigkeit aBahngeschwindigkeit) = anzeige aBahngeschwindigkeit
    anzeige (AOStreckenabschnitt aStreckenabschnitt) = anzeige aStreckenabschnitt
    anzeige (AOWeiche aWeiche) = anzeige aWeiche
    anzeige (AOKupplung aKupplung) = anzeige aKupplung
    anzeige (AOKontakt aKontakt) = anzeige aKontakt
    anzeige (AOWegstrecke qWegstrecke) = anzeige qWegstrecke
    anzeige (AOPlan aPlan) = anzeige aPlan

instance Anfrage AnfrageObjekt where
    zeigeAnfrage :: AnfrageObjekt -> Sprache -> Text
    zeigeAnfrage AnfrageObjekt = Language.objekt
    zeigeAnfrage (AOBahngeschwindigkeit aBahngeschwindigkeit) = zeigeAnfrage aBahngeschwindigkeit
    zeigeAnfrage (AOStreckenabschnitt aStreckenabschnitt) = zeigeAnfrage aStreckenabschnitt
    zeigeAnfrage (AOWeiche aWeiche) = zeigeAnfrage aWeiche
    zeigeAnfrage (AOKupplung aKupplung) = zeigeAnfrage aKupplung
    zeigeAnfrage (AOKontakt aKontakt) = zeigeAnfrage aKontakt
    zeigeAnfrage (AOWegstrecke qWegstrecke) = zeigeAnfrage qWegstrecke
    zeigeAnfrage (AOPlan aPlan) = zeigeAnfrage aPlan

    zeigeAnfrageOptionen :: AnfrageObjekt -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageObjekt = Just $ toBefehlsString . Language.befehlTypen
    zeigeAnfrageOptionen (AOBahngeschwindigkeit aBahngeschwindigkeit) =
        zeigeAnfrageOptionen aBahngeschwindigkeit
    zeigeAnfrageOptionen (AOStreckenabschnitt aStreckenabschnitt) =
        zeigeAnfrageOptionen aStreckenabschnitt
    zeigeAnfrageOptionen (AOWeiche aWeiche) = zeigeAnfrageOptionen aWeiche
    zeigeAnfrageOptionen (AOKupplung aKupplung) = zeigeAnfrageOptionen aKupplung
    zeigeAnfrageOptionen ((AOKontakt aKontakt)) = zeigeAnfrageOptionen aKontakt
    zeigeAnfrageOptionen (AOWegstrecke aWegstrecke) = zeigeAnfrageOptionen aWegstrecke
    zeigeAnfrageOptionen (AOPlan aPlan) = zeigeAnfrageOptionen aPlan

instance MitAnfrage Objekt where
    type AnfrageTyp Objekt = AnfrageObjekt

    -- | Eingabe eines Objekts
    anfrageAktualisieren
        :: AnfrageObjekt -> EingabeToken -> AnfrageFortsetzung AnfrageObjekt Objekt
    anfrageAktualisieren AnfrageObjekt token =
        wähleZwischenwert
            token
            [ ( Lexer.Bahngeschwindigkeit
                  , AOBahngeschwindigkeit
                    $ AnfrageNothing
                    $ AnfrageGeschwindigkeitNothing AnfrageBahngeschwindigkeit
                  )
            , (Lexer.Streckenabschnitt, AOStreckenabschnitt AnfrageStreckenabschnitt)
            , (Lexer.Weiche, AOWeiche $ AnfrageNothing AnfrageWeiche)
            , (Lexer.Wegstrecke, AOWegstrecke $ AnfrageNothing AnfrageWegstreckeZugtyp)
            , (Lexer.Kupplung, AOKupplung AnfrageKupplung)
            , (Lexer.Kontakt, AOKontakt AnfrageKontakt)
            , (Lexer.Plan, AOPlan AnfragePlan)]
    anfrageAktualisieren
        (AOBahngeschwindigkeit
             (AnfrageNothing (AnfrageGeschwindigkeitNothing AnfrageBahngeschwindigkeit)))
        token = (id, AOBahngeschwindigkeit) $<< anfrageAktualisierenZugtyp token
    anfrageAktualisieren (AOBahngeschwindigkeit (AnfrageNothing _)) token =
        (id, AOBahngeschwindigkeit) $<< anfrageAktualisierenZugtyp token
    anfrageAktualisieren
        (AOBahngeschwindigkeit
             (AnfrageMärklin (AnfrageGeschwindigkeitNothing AMärklinBahngeschwindigkeit)))
        token =
        wähleZwischenwert
            token
            [ ( Lexer.Pwm
                  , AOBahngeschwindigkeit
                    $ AnfrageMärklin
                    $ AnfrageGeschwindigkeitPwm ABahngeschwindigkeitPwmMärklin
                  )
            , ( Lexer.KonstanteSpannung
                  , AOBahngeschwindigkeit
                    $ AnfrageMärklin
                    $ AnfrageGeschwindigkeitKonstanteSpannung
                        AMärklinBahngeschwindigkeitKonstanteSpannung
                  )]
    anfrageAktualisieren
        (AOBahngeschwindigkeit (AnfrageMärklin (AnfrageGeschwindigkeitPwm aBahngeschwindigkeit)))
        token =
        ( AFErgebnis . OBahngeschwindigkeit . ZugtypMärklin . GeschwindigkeitPwm
        , AOBahngeschwindigkeit . AnfrageMärklin . AnfrageGeschwindigkeitPwm
        )
        $<< anfrageAktualisieren aBahngeschwindigkeit token
    anfrageAktualisieren
        (AOBahngeschwindigkeit
             (AnfrageMärklin (AnfrageGeschwindigkeitKonstanteSpannung aBahngeschwindigkeit)))
        token =
        ( AFErgebnis . OBahngeschwindigkeit . ZugtypMärklin . GeschwindigkeitKonstanteSpannung
        , AOBahngeschwindigkeit . AnfrageMärklin . AnfrageGeschwindigkeitKonstanteSpannung
        )
        $<< anfrageAktualisieren aBahngeschwindigkeit token
    anfrageAktualisieren
        (AOBahngeschwindigkeit (AnfrageLego (AnfrageGeschwindigkeitNothing _aBahngeschwindigkeit)))
        token =
        wähleZwischenwert
            token
            [ ( Lexer.Pwm
                  , AOBahngeschwindigkeit
                    $ AnfrageLego
                    $ AnfrageGeschwindigkeitPwm ALegoBahngeschwindigkeit
                  )]
    anfrageAktualisieren
        (AOBahngeschwindigkeit (AnfrageLego (AnfrageGeschwindigkeitPwm aBahngeschwindigkeit)))
        token =
        ( AFErgebnis . OBahngeschwindigkeit . ZugtypLego . GeschwindigkeitPwm
        , AOBahngeschwindigkeit . AnfrageLego . AnfrageGeschwindigkeitPwm
        )
        $<< anfrageAktualisieren aBahngeschwindigkeit token
    anfrageAktualisieren
        (AOBahngeschwindigkeit
             (AnfrageLego (AnfrageGeschwindigkeitKonstanteSpannung aBahngeschwindigkeit)))
        token =
        ( AFErgebnis . OBahngeschwindigkeit . ZugtypLego . GeschwindigkeitKonstanteSpannung
        , AOBahngeschwindigkeit . AnfrageLego . AnfrageGeschwindigkeitKonstanteSpannung
        )
        $<< anfrageAktualisieren aBahngeschwindigkeit token
    anfrageAktualisieren (AOStreckenabschnitt aStreckenabschnitt) token =
        (AFErgebnis . OStreckenabschnitt, AOStreckenabschnitt)
        $<< anfrageAktualisieren aStreckenabschnitt token
    anfrageAktualisieren (AOWeiche (AnfrageNothing AnfrageWeiche)) token =
        (id, AOWeiche) $<< anfrageAktualisierenZugtyp token
    anfrageAktualisieren (AOWeiche (AnfrageMärklin aWeiche)) token =
        (AFErgebnis . OWeiche . ZugtypMärklin, AOWeiche . AnfrageMärklin)
        $<< anfrageAktualisieren aWeiche token
    anfrageAktualisieren (AOWeiche (AnfrageLego aWeiche)) token =
        (AFErgebnis . OWeiche . ZugtypLego, AOWeiche . AnfrageLego)
        $<< anfrageAktualisieren aWeiche token
    anfrageAktualisieren (AOKupplung aKupplung) token =
        (AFErgebnis . OKupplung, AOKupplung) $<< anfrageAktualisieren aKupplung token
    anfrageAktualisieren (AOKontakt aKontakt) token =
        (AFErgebnis . OKontakt, AOKontakt) $<< anfrageAktualisieren aKontakt token
    anfrageAktualisieren (AOWegstrecke (AnfrageNothing _aWegstrecke)) token =
        (id, AOWegstrecke) $<< anfrageAktualisierenZugtyp token
    anfrageAktualisieren (AOWegstrecke (AnfrageMärklin aWegstrecke)) token =
        (AFErgebnis . OWegstrecke . ZugtypMärklin, AOWegstrecke . AnfrageMärklin)
        $<< anfrageAktualisieren aWegstrecke token
    anfrageAktualisieren (AOWegstrecke (AnfrageLego aWegstrecke)) token =
        (AFErgebnis . OWegstrecke . ZugtypLego, AOWegstrecke . AnfrageLego)
        $<< anfrageAktualisieren aWegstrecke token
    anfrageAktualisieren (AOPlan aPlan) token =
        (AFErgebnis . OPlan, AOPlan) $<< anfrageAktualisieren aPlan token