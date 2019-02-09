{-# LANGUAGE NamedFieldPuns, LambdaCase, OverloadedStrings, InstanceSigs #-}

{-|
Description : Verarbeiten von Token.

In diesem Modul werden sämtliche Umwandlungen vorgenommen, für die nicht die IO-Monade benötigt wird.
-}
module Zug.UI.Cmd.Parser (
                        -- * Auswerten einer Text-Eingabe
                        parser, statusQueryObjekt,
                        -- * Ergebnis-Typen
                        QErgebnis(..), QBefehl(..), BefehlSofort(..), QObjektIOStatus(..),
                        -- ** Unvollständige StreckenObjekte
                        Query(..), getQueryFailedDefault, showQuery, showQueryFailed, unbekanntShowText,
                        QPlan(..), QAktion(..), QAktionWegstrecke(..), QAktionWeiche(..), QAktionBahngeschwindigkeit(..), QAktionStreckenabschnitt(..), QAktionKupplung(..),
                        QObjekt(..), QWegstrecke(..), QWeiche(..), QBahngeschwindigkeit(..), QStreckenabschnitt(..), QKupplung(..)) where

-- Bibliotheken
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text (Text, unpack)
import Numeric.Natural
-- Abhängigkeiten von anderen Modulen
import Zug.SEQueue
import Zug.Klassen
import Zug.Anbindung
import Zug.Plan
import qualified Zug.Language as Language
import Zug.Language ((<^>), (<=>), (<->), (<|>), (<:>), showText, fehlerText, toBefehlsString)
import Zug.UI.Base
import Zug.UI.Befehl
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Lexer (AllgemeinesEingabeToken(..), EingabeToken(..), Token())

-- * Auswerten einer Text-Eingabe
-- | Erhalte ein im Status existierendes Objekt
statusQueryObjekt :: QObjektIOStatus -> MStatus (Either QObjektIOStatus Objekt)
statusQueryObjekt   query@(QOIOSUnbekannt _eingabe0)            = pure $ Left query
statusQueryObjekt   query@(QOIOSPlan eingabe)                   = statusQueryObjektAux query eingabe getPläne OPlan
statusQueryObjekt   query@(QOIOSWegstrecke eingabe)             = statusQueryObjektAux query eingabe getWegstrecken OWegstrecke
statusQueryObjekt   query@(QOIOSWeiche eingabe)                 = statusQueryObjektAux query eingabe getWeichen OWeiche
statusQueryObjekt   query@(QOIOSBahngeschwindigkeit eingabe)    = statusQueryObjektAux query eingabe getBahngeschwindigkeiten OBahngeschwindigkeit
statusQueryObjekt   query@(QOIOSStreckenabschnitt eingabe)      = statusQueryObjektAux query eingabe getStreckenabschnitte OStreckenabschnitt
statusQueryObjekt   query@(QOIOSKupplung eingabe)               = statusQueryObjektAux query eingabe getKupplungen OKupplung

-- | Hilfsfunktion
statusQueryObjektAux :: (StreckenObjekt a) => QObjektIOStatus -> EingabeToken -> MStatus [a] -> (a -> Objekt) -> MStatus (Either QObjektIOStatus Objekt)
statusQueryObjektAux    query   eingabe getFromStatus   konstruktor = getFromStatus >>= \objekte -> pure $ case findByNameOrIndex objekte eingabe of
    (Nothing)       -> Left query
    (Just objekt)   -> Right $ konstruktor objekt

-- | Auswerten von Befehlen, so weit es ohne Status-Informationen möglich ist
parser :: QBefehl -> [AllgemeinesEingabeToken] -> ([Befehl], QErgebnis)
parser = parserAux []
    where
        parserAux :: [Befehl] -> QBefehl -> [AllgemeinesEingabeToken] -> ([Befehl], QErgebnis)
        parserAux   acc (QBefehl)   ([])                    = parserErgebnisOk acc
        parserAux   acc query       ([])                    = (reverse acc, QEQBefehl query)
        parserAux   acc _query      (TkBeenden:_t)          = parserErgebnisOk (UI Beenden:acc)
        parserAux   acc _query      (TkAbbrechen:_t)        = parserErgebnisOk (UI Abbrechen:acc)
        parserAux   acc query       ((Tk h):t)              = case queryUpdate query h of
            QEQBefehl qFehler@(QBUnbekannt _qb _b)      -> parserErgebnis acc $ QEQBefehl qFehler
            QEQBefehl qBefehl                           -> parserAux acc qBefehl t
            (QEBefehlSofort eingabe _)                  -> parserErgebnis acc $ QEBefehlSofort eingabe t
            (QEBefehlQuery eingabe konstruktor query _) -> parserErgebnis acc $ QEBefehlQuery eingabe konstruktor query t
            QEBefehl befehl                             -> parserAux (befehl:acc) QBefehl t
        -- | Ergebnis zurückgeben
        parserErgebnis :: [Befehl] -> QErgebnis -> ([Befehl], QErgebnis)
        parserErgebnis  acc query = (reverse acc, query)
        parserErgebnisOk :: [Befehl] -> ([Befehl], QErgebnis)
        parserErgebnisOk    ([])                = parserErgebnis [] $ QEQBefehl QBefehl
        parserErgebnisOk    (befehl:befehle)    = parserErgebnis befehle $ QEBefehl befehl

-- | Auswerten eines Zwischenergebnisses fortsetzen
queryUpdate :: QBefehl -> EingabeToken -> QErgebnis
queryUpdate query@(QBUnbekannt _ _)                         _token                          = QEQBefehl query
queryUpdate (QBefehl)                                       token@(EingabeToken {eingabe})  = wähleBefehl token [
    (Lexer.Beenden              , QEBefehl $ UI Beenden),
    (Lexer.Hinzufügen           , QEQBefehl $ QBHinzufügen QObjekt),
    (Lexer.Entfernen            , QEQBefehl QBEntfernen),
    (Lexer.Speichern            , QEQBefehl QBSpeichern),
    (Lexer.Laden                , QEQBefehl QBLaden),
    (Lexer.Plan                 , QEQBefehl $ QBBefehlQuery QOIOSPlan (Left $ \(OPlan plan) -> QBAktionPlan plan)),
    (Lexer.Wegstrecke           , queryUpdate (QBAktion QAktion) token),
    (Lexer.Weiche               , queryUpdate (QBAktion QAktion) token),
    (Lexer.Bahngeschwindigkeit  , queryUpdate (QBAktion QAktion) token),
    (Lexer.Streckenabschnitt    , queryUpdate (QBAktion QAktion) token),
    (Lexer.Kupplung             , queryUpdate (QBAktion QAktion) token)]
    $ QEQBefehl $ QBUnbekannt QBefehl eingabe
queryUpdate query@(QBHinzufügen qObjekt)                    token                           = case queryObjekt qObjekt token of
    (Left (QOUnbekannt query eingabe))                  -> QEQBefehl $ QBUnbekannt (QBHinzufügen query) eingabe
    (Left (QOIOStatus statusQuery (Right konstruktor))) -> QEBefehlQuery statusQuery (Right $ \objekt -> Hinzufügen $ konstruktor objekt) query []
    (Left (QOIOStatus statusQuery (Left qKonstruktor))) -> QEBefehlQuery statusQuery (Left $ \objekt -> QBHinzufügen $ qKonstruktor objekt) query []
    (Left qObjekt1)                                     -> QEQBefehl $ QBHinzufügen qObjekt1
    (Right objekt)                                      -> QEBefehl $ Hinzufügen objekt
queryUpdate (QBEntfernen)                                   token@(EingabeToken {eingabe})  = case queryObjektExistierend token of
    (Nothing)           -> QEQBefehl $ QBUnbekannt QBEntfernen eingabe
    (Just qKonstruktor) -> QEQBefehl $ QBBefehlQuery qKonstruktor (Right Entfernen)
    where
        -- | Eingabe eines existirendes Objekts
        queryObjektExistierend :: EingabeToken -> Maybe (EingabeToken -> QObjektIOStatus)
        queryObjektExistierend  token@(EingabeToken {}) = wähleBefehl token [
            (Lexer.Plan                  , Just QOIOSPlan),
            (Lexer.Wegstrecke            , Just QOIOSWegstrecke),
            (Lexer.Weiche                , Just QOIOSWeiche),
            (Lexer.Bahngeschwindigkeit   , Just QOIOSBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , Just QOIOSStreckenabschnitt),
            (Lexer.Kupplung              , Just QOIOSKupplung)]
            $ Nothing
queryUpdate (QBSpeichern)                                   (EingabeToken {eingabe})        = QEBefehl $ Speichern $ unpack eingabe
queryUpdate (QBLaden)                                       (EingabeToken {eingabe})        = QEBefehlSofort (BSLaden $ unpack eingabe) []
queryUpdate query@(QBAktionPlan plan@(Plan {plAktionen}))   token@(EingabeToken {eingabe})  = wähleBefehl token [(Lexer.Ausführen, QEBefehl $ Ausführen plan $ \i -> putStrLn $ showText plan <:> showText (toEnum (fromIntegral i) / toEnum (length plAktionen) :: Double))] $ QEQBefehl $ QBUnbekannt query eingabe
queryUpdate query@(QBAktion qAktion)                        token                           = case queryAktion qAktion token of
    (Left (QAUnbekannt query eingabe))                              -> QEQBefehl $ QBUnbekannt (QBAktion query) eingabe
    (Left (QAktionIOStatus qObjektIOStatus (Left qKonstruktor)))    -> QEBefehlQuery qObjektIOStatus (Left $ \objekt -> QBAktion $ qKonstruktor objekt) query []
    (Left (QAktionIOStatus qObjektIOStatus (Right konstruktor)))    -> QEBefehlQuery qObjektIOStatus (Right $ \objekt -> AktionBefehl $ konstruktor objekt) query []
    (Left qAktion)                                                  -> QEQBefehl $ QBAktion qAktion
    (Right aktion)                                                  -> QEBefehl $ AktionBefehl aktion
queryUpdate query@(QBBefehlQuery qKonstruktor eitherF)      token                           = QEBefehlQuery (qKonstruktor token) eitherF query []

-- | Eingabe eines Objekts
queryObjekt :: QObjekt -> EingabeToken -> Either QObjekt Objekt
queryObjekt qFehler@(QOUnbekannt _ _)                       _token                          = Left qFehler
queryObjekt qObjekt@(QOIOStatus _ _)                        _token                          = Left qObjekt
queryObjekt (QObjekt)                                       token@(EingabeToken {eingabe})  = wähleBefehl token [
    (Lexer.Plan                  , Left $ QOPlan QPlan),
    (Lexer.Wegstrecke            , Left $ QOWegstrecke QWegstrecke),
    (Lexer.Weiche                , Left $ QOWeiche QWeiche),
    (Lexer.Bahngeschwindigkeit   , Left $ QOBahngeschwindigkeit QBahngeschwindigkeit),
    (Lexer.Streckenabschnitt     , Left $ QOStreckenabschnitt QStreckenabschnitt),
    (Lexer.Kupplung              , Left $ QOKupplung QKupplung)]
    $ Left $ QOUnbekannt QObjekt eingabe
queryObjekt (QOPlan qPlan)                                  token                           = case queryPlan qPlan token of
    (Left (QPUnbekannt query eingabe1))                         -> Left $ QOUnbekannt (QOPlan query) eingabe1
    (Left (QPlanIOStatus qObjektIOStatus (Right konstruktor)))  -> Left $ QOIOStatus qObjektIOStatus $ Right $ \objekt -> OPlan $ konstruktor objekt
    (Left (QPlanIOStatus qObjektIOStatus (Left qKonstruktor)))  -> Left $ QOIOStatus qObjektIOStatus $ Left $ \objekt -> QOPlan $ qKonstruktor objekt
    (Left qPlan1)                                               -> Left $ QOPlan qPlan1
    (Right plan)                                                -> Right $ OPlan plan
queryObjekt (QOWegstrecke qWegstrecke0)                     token                           = case queryWegstrecke qWegstrecke0 token of
    (Left (QWSUnbekannt query eingabe1))                                    -> Left $ QOUnbekannt (QOWegstrecke query) eingabe1
    (Left (QWegstreckeIOStatus qObjektIOStatus (Right konstruktor)))        -> Left $ QOIOStatus qObjektIOStatus $ Right $ \objekt -> OWegstrecke $ konstruktor objekt
    (Left (QWegstreckeIOStatus qObjektIOStatus (Left qKonstruktor)))        -> Left $ QOIOStatus qObjektIOStatus $ Left $ \objekt -> QOWegstrecke $ qKonstruktor objekt
    (Left qWegstrecke1)                                                     -> Left $ QOWegstrecke qWegstrecke1
    (Right wegstrecke)                                                      -> Right $ OWegstrecke wegstrecke
queryObjekt (QOWeiche qWeiche)                              token                           = case queryWeiche qWeiche token of
    (Left (QWUnbekannt query eingabe1)) -> Left $ QOUnbekannt (QOWeiche query) eingabe1
    (Left qWeiche1)                     -> Left $ QOWeiche qWeiche1
    (Right weiche)                      -> Right $ OWeiche weiche
queryObjekt (QOBahngeschwindigkeit qBahngeschwindigkeit)    token                           = case queryBahngeschwindigkeit qBahngeschwindigkeit token of
    (Left (QBGUnbekannt query eingabe1))    -> Left $ QOUnbekannt (QOBahngeschwindigkeit query) eingabe1
    (Left qBahngeschwindigkeit1)            -> Left $ QOBahngeschwindigkeit qBahngeschwindigkeit1
    (Right bahngeschwindigkeit)             -> Right $ OBahngeschwindigkeit bahngeschwindigkeit
queryObjekt (QOStreckenabschnitt qStreckenabschnitt)        token                           = case queryStreckenabschnitt qStreckenabschnitt token of
    (Left (QSUnbekannt query eingabe1)) -> Left $ QOUnbekannt (QOStreckenabschnitt query) eingabe1
    (Left qStreckenabschnitt1)          -> Left $ QOStreckenabschnitt qStreckenabschnitt1
    (Right streckenabschnitt)           -> Right $ OStreckenabschnitt streckenabschnitt
queryObjekt (QOKupplung qKupplung)                          token                           = case queryKupplung qKupplung token of
    (Left (QKUnbekannt query eingabe1)) -> Left $ QOUnbekannt (QOKupplung query) eingabe1
    (Left qKupplung1)                   -> Left $ QOKupplung qKupplung1
    (Right kupplung)                    -> Right $ OKupplung kupplung
-- | Eingabe eines Plans
queryPlan :: QPlan -> EingabeToken -> Either QPlan Plan
queryPlan   (QPlan)                                         (EingabeToken {eingabe})            = Left $ QPlanName eingabe
queryPlan   query@(QPlanName name)                          (EingabeToken {eingabe, ganzzahl})  = Left $ case ganzzahl of
    (Nothing)       -> QPUnbekannt query eingabe
    (Just anzahl)   -> QPlanNameAnzahl name anzahl seEmpty QAktion
queryPlan   (QPlanNameAnzahl name anzahl acc qAktion)       token                               = case queryAktion qAktion token of
    (Left (QAUnbekannt qAktion1 eingabe))                           -> Left $ QPUnbekannt (QPlanNameAnzahl name anzahl acc qAktion1) eingabe
    (Left (QAktionIOStatus qObjektIOStatus (Left qKonstruktor)))    -> Left $ QPlanIOStatus qObjektIOStatus $ Left $ \objekt -> QPlanNameAnzahl name anzahl acc $ qKonstruktor objekt
    (Left (QAktionIOStatus qObjektIOStatus (Right konstruktor)))    -> Left $ QPlanIOStatus qObjektIOStatus $ if anzahl > 1 then Left $ \objekt -> QPlanNameAnzahl name anzahl (append (konstruktor objekt) acc) QAktion else Right $ \objekt -> Plan {plName=name, plAktionen=toList $ append (konstruktor objekt) acc}
    (Left qAktion1)                                                 -> Left $ QPlanNameAnzahl name anzahl acc qAktion1
    (Right aktion)  | anzahl > 1                                    -> Left $ QPlanNameAnzahl name (pred anzahl) (append aktion acc) QAktion
                    | otherwise                                     -> Right $ Plan {plName=name, plAktionen=toList $ append aktion acc}
queryPlan   (QPlanBefehlQuery qKonstruktor eitherF)         token                               = Left $ QPlanIOStatus (qKonstruktor token) eitherF
queryPlan   query                                           _token                              = Left query
-- | Eingabe einer Aktion
queryAktion :: QAktion -> EingabeToken -> Either QAktion Aktion
queryAktion (QAktion)                                   token                               = Left $ case queryAktionElement token of
    (QAEUnbekannt eingabe)      -> QAUnbekannt QAktion eingabe
    (QAEWarten)                 -> QAWarten
    (QAEWegstrecke)             -> QAktionBefehlQuery QOIOSWegstrecke $ Left $ \(OWegstrecke wegstrecke) -> QAWegstrecke $ QAktionWegstrecke wegstrecke
    (QAEWeiche)                 -> QAktionBefehlQuery QOIOSWeiche $ Left $ \(OWeiche weiche) -> QAWeiche $ QAktionWeiche weiche
    (QAEBahngeschwindigkeit)    -> QAktionBefehlQuery QOIOSBahngeschwindigkeit $ Left $ \(OBahngeschwindigkeit bahngeschwindigkeit) -> QABahngeschwindigkeit $ QAktionBahngeschwindigkeit bahngeschwindigkeit
    (QAEStreckenabschnitt)      -> QAktionBefehlQuery QOIOSStreckenabschnitt $ Left $ \(OStreckenabschnitt streckenabschnitt) -> QAStreckenabschnitt $ QAktionStreckenabschnitt streckenabschnitt
    (QAEKupplung)               -> QAktionBefehlQuery QOIOSKupplung $ Left $ \(OKupplung kupplung) -> QAKupplung $ QAktionKupplung kupplung
    where
        queryAktionElement :: EingabeToken -> QAktionElement
        queryAktionElement  token@(EingabeToken {eingabe})  = wähleBefehl token [
            (Lexer.Warten               , QAEWarten),
            (Lexer.Wegstrecke           , QAEWegstrecke),
            (Lexer.Weiche               , QAEWeiche),
            (Lexer.Bahngeschwindigkeit  , QAEBahngeschwindigkeit),
            (Lexer.Streckenabschnitt    , QAEStreckenabschnitt),
            (Lexer.Kupplung             , QAEKupplung)]
            $ QAEUnbekannt eingabe
queryAktion (QAWarten)                                  (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ QAUnbekannt QAWarten eingabe
    (Just zeit) -> Right $ Warten zeit
queryAktion (QAktionBefehlQuery qKonstruktor eitherF)   token                               = Left $ QAktionIOStatus (qKonstruktor token) eitherF
queryAktion (QAWegstrecke qAktion)                      token                               = case queryAktionWegstrecke qAktion token of
    (Left (QAWSUnbekannt query eingabe))    -> Left $ QAUnbekannt (QAWegstrecke query) eingabe
    (Left qAktionWegstrecke)                -> Left $ QAWegstrecke qAktionWegstrecke
    (Right aktionWegstrecke)                -> Right $ AWegstrecke aktionWegstrecke
queryAktion (QAWeiche qAktion)                          token                               = case queryAktionWeiche qAktion token of
    (Left (QAWUnbekannt query eingabe)) -> Left $ QAUnbekannt (QAWeiche query) eingabe
    (Left qAktionWeiche)                -> Left $ QAWeiche qAktionWeiche
    (Right aktionWeiche)                -> Right $ AWeiche aktionWeiche
queryAktion (QABahngeschwindigkeit qAktion)             token                               = case queryAktionBahngeschwindigkeit qAktion token of
    (Left (QABGUnbekannt query eingabe))    -> Left $ QAUnbekannt (QABahngeschwindigkeit query) eingabe
    (Left qAktionBahngeschwindigkeit)       -> Left $ QABahngeschwindigkeit qAktionBahngeschwindigkeit
    (Right aktionBahngeschwindigkeit)       -> Right $ ABahngeschwindigkeit aktionBahngeschwindigkeit
queryAktion (QAStreckenabschnitt qAktion)               token                               = case queryAktionStreckenabschnitt qAktion token of
    (Left (QASUnbekannt query eingabe)) -> Left $ QAUnbekannt (QAStreckenabschnitt query) eingabe
    (Left qAktionStreckenabschnitt)     -> Left $ QAStreckenabschnitt qAktionStreckenabschnitt
    (Right aktionStreckenabschnitt)     -> Right $ AStreckenabschnitt aktionStreckenabschnitt
queryAktion (QAKupplung qAktion)                        token                               = case queryAktionKupplung qAktion token of
    (Left (QAKUnbekannt query eingabe)) -> Left $ QAUnbekannt (QAKupplung query) eingabe
    (Left qAktionKupplung)              -> Left $ QAKupplung qAktionKupplung
    (Right aktionKupplung)              -> Right $ AKupplung aktionKupplung
queryAktion query                                       _token                              = Left query
-- | Eingabe einer Wegstrecken-Aktion
queryAktionWegstrecke :: (WegstreckeKlasse w) => QAktionWegstrecke qw w -> EingabeToken -> Either (QAktionWegstrecke qw w) (AktionWegstrecke w)
queryAktionWegstrecke   query@(QAWSUnbekannt _ _)               _token  = Left query
queryAktionWegstrecke   query@(QAktionWegstrecke wegstrecke)    token@(EingabeToken {eingabe})  = wähleBefehl token [
    (Lexer.Einstellen       , Right $ Einstellen wegstrecke),
    (Lexer.Geschwindigkeit  , Left $ QAWSBahngeschwindigkeit $ QABGGeschwindigkeit wegstrecke),
    (Lexer.Umdrehen         , Left $ QAWSBahngeschwindigkeit $ QABGUmdrehen wegstrecke),
    (Lexer.Strom            , Left $ QAWSStreckenabschnitt $ QASStrom wegstrecke),
    (Lexer.Kuppeln          , Right $ AWSKupplung $ Kuppeln wegstrecke)]
    $ Left $ QAWSUnbekannt query eingabe
queryAktionWegstrecke   (QAWSBahngeschwindigkeit qAktion0)      token                           = case queryAktionBahngeschwindigkeit qAktion0 token of
    (Left (QABGUnbekannt query eingabe))    -> Left $ QAWSUnbekannt (QAWSBahngeschwindigkeit query) eingabe
    (Left qAktion1)                         -> Left $ QAWSBahngeschwindigkeit qAktion1
    (Right aktion)                          -> Right $ AWSBahngeschwindigkeit aktion
queryAktionWegstrecke   (QAWSStreckenabschnitt qAktion0)        token                           = case queryAktionStreckenabschnitt qAktion0 token of
    (Left (QASUnbekannt query eingabe)) -> Left $ QAWSUnbekannt (QAWSStreckenabschnitt query) eingabe
    (Left qAktion1)                     -> Left $ QAWSStreckenabschnitt qAktion1
    (Right aktion)                      -> Right $ AWSStreckenabschnitt aktion
queryAktionWegstrecke   (QAWSKupplung qAktion0)                 token                           = case queryAktionKupplung qAktion0 token of
    (Left (QAKUnbekannt query eingabe)) -> Left $ QAWSUnbekannt (QAWSKupplung query) eingabe
    (Left qAktion1)                     -> Left $ QAWSKupplung qAktion1
    (Right aktion)                      -> Right $ AWSKupplung aktion
-- | Eingabe einer Weichen-Aktion
queryAktionWeiche :: (Show qw, Show w, WeicheKlasse w) => QAktionWeiche qw w -> EingabeToken -> Either (QAktionWeiche qw w) (AktionWeiche w)
queryAktionWeiche   query@(QAktionWeiche weiche)    token@(EingabeToken {eingabe})  = wähleBefehl token [(Lexer.Stellen  , Left $ QAWStellen weiche)] $ Left $ QAWUnbekannt query eingabe
queryAktionWeiche   query@(QAWStellen _weiche)      token@(EingabeToken {eingabe})  = case wähleRichtung token of
    (Nothing)       -> Left $ QAWUnbekannt query eingabe
    (Just richtung) -> mitRichtung query richtung
        where
            mitRichtung :: (Show qw, Show w, WeicheKlasse w) => QAktionWeiche qw w -> Richtung -> Either (QAktionWeiche qw w) (AktionWeiche w)
            mitRichtung  query@(QAWStellen weiche)  richtung
                | hatRichtung weiche richtung                   = Right $ Stellen weiche richtung
                | otherwise                                     = Left $ QAWUnbekannt query eingabe
            mitRichtung query                       _richtung   = error $ "mitRichtung mit unbekannter query aufgerufen: " ++ show query
queryAktionWeiche   query                           _token                                          = Left query
-- | Eingabe einer Bahngeschwindigkeit-Aktion
queryAktionBahngeschwindigkeit :: (BahngeschwindigkeitKlasse b) => QAktionBahngeschwindigkeit qb b -> EingabeToken -> Either (QAktionBahngeschwindigkeit qb b) (AktionBahngeschwindigkeit b)
queryAktionBahngeschwindigkeit  query@(QAktionBahngeschwindigkeit bahngeschwindigkeit)  token@(EingabeToken {eingabe})      = wähleBefehl token [
    (Lexer.Geschwindigkeit  , Left $ QABGGeschwindigkeit bahngeschwindigkeit),
    (Lexer.Umdrehen         , if zugtyp bahngeschwindigkeit == Märklin then Right $ Umdrehen bahngeschwindigkeit Nothing else Left $ QABGUmdrehen bahngeschwindigkeit)]
    $ Left $ QABGUnbekannt query eingabe
queryAktionBahngeschwindigkeit  query@(QABGGeschwindigkeit bahngeschwindigkeit)         (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ QABGUnbekannt query eingabe
    (Just wert) -> Right $ Geschwindigkeit bahngeschwindigkeit wert
queryAktionBahngeschwindigkeit  query@(QABGUmdrehen bahngeschwindigkeit)                token@(EingabeToken {eingabe})      = wähleBefehl token [
    (Lexer.Vorwärts , Right $ Umdrehen bahngeschwindigkeit $ Just Vorwärts),
    (Lexer.Rückwärts , Right $ Umdrehen bahngeschwindigkeit $ Just Rückwärts)]
    $ Left $ QABGUnbekannt query eingabe
queryAktionBahngeschwindigkeit  query                                                   _token                                  = Left $ query
-- | Eingabe einer Streckenabschnitt-Aktion
queryAktionStreckenabschnitt :: (StreckenabschnittKlasse s) => QAktionStreckenabschnitt qs s -> EingabeToken -> Either (QAktionStreckenabschnitt qs s) (AktionStreckenabschnitt s)
queryAktionStreckenabschnitt    query@(QAktionStreckenabschnitt streckenabschnitt)  token@(EingabeToken {eingabe})  = wähleBefehl token [(Lexer.Strom, Left $ QASStrom streckenabschnitt)] $ Left $ QASUnbekannt query eingabe
queryAktionStreckenabschnitt    query@(QASStrom streckenabschnitt)                  token@(EingabeToken {eingabe})  = wähleBefehl token [
    (Lexer.An   , Right $ Strom streckenabschnitt True),
    (Lexer.Aus  , Right $ Strom streckenabschnitt False)]
    $ Left $ QASUnbekannt query eingabe
queryAktionStreckenabschnitt    query                                               _token                          = Left $ query
-- | Eingabe einer Kupplung-Aktion
queryAktionKupplung :: (KupplungKlasse k) => QAktionKupplung qk k -> EingabeToken -> Either (QAktionKupplung qk k) (AktionKupplung k)
queryAktionKupplung query@(QAktionKupplung kupplung)    token@(EingabeToken {eingabe})  = wähleBefehl token [(Lexer.Kuppeln, Right $ Kuppeln kupplung)] $ Left $ QAKUnbekannt query eingabe
queryAktionKupplung query                               _token                          = Left $ query
-- | Eingabe einer Wegstrecke
queryWegstrecke :: QWegstrecke -> EingabeToken -> Either QWegstrecke Wegstrecke
queryWegstrecke (QWegstrecke)                                                                                                   (EingabeToken {eingabe})            = Left $ QWegstreckeName eingabe
queryWegstrecke query@(QWegstreckeName name)                                                                                    (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)       -> Left $ QWSUnbekannt query eingabe
    (Just anzahl)   -> Left $ QWegstreckeNameAnzahl (Wegstrecke {wsName=name, wsBahngeschwindigkeiten=[], wsStreckenabschnitte=[], wsWeichenRichtungen=[], wsKupplungen=[]}) anzahl
queryWegstrecke query@(QWegstreckeNameAnzahl acc@(Wegstrecke {wsBahngeschwindigkeiten, wsStreckenabschnitte, wsKupplungen}) anzahl)   token                               = Left $ case queryWegstreckenElement token of
    (QWEWeiche)                 -> QWegstreckeBefehlQuery QOIOSWeiche $ Left $ qWeicheAnhängen
    (QWEBahngeschwindigkeit)    -> QWegstreckeBefehlQuery QOIOSBahngeschwindigkeit eitherObjektAnhängen
    (QWEStreckenabschnitt)      -> QWegstreckeBefehlQuery QOIOSStreckenabschnitt eitherObjektAnhängen
    (QWEKupplung)               -> QWegstreckeBefehlQuery QOIOSKupplung eitherObjektAnhängen
    (QWEUnbekannt eingabe)      -> QWSUnbekannt query eingabe
    where
        queryWegstreckenElement :: EingabeToken -> QWegstreckenElement
        queryWegstreckenElement token@(EingabeToken {eingabe})  = wähleBefehl token [
            (Lexer.Weiche                , QWEWeiche),
            (Lexer.Bahngeschwindigkeit   , QWEBahngeschwindigkeit),
            (Lexer.Streckenabschnitt     , QWEStreckenabschnitt),
            (Lexer.Kupplung              , QWEKupplung)]
            $ QWEUnbekannt eingabe
        eitherObjektAnhängen :: Either (Objekt -> QWegstrecke) (Objekt -> Wegstrecke)
        eitherObjektAnhängen = if anzahl > 1 then Left qObjektAnhängen else Right objektAnhängen
        objektAnhängen :: Objekt -> Wegstrecke
        objektAnhängen  (OBahngeschwindigkeit bahngeschwindigkeit)  = acc {wsBahngeschwindigkeiten=bahngeschwindigkeit:wsBahngeschwindigkeiten}
        objektAnhängen  (OStreckenabschnitt streckenabschnitt)      = acc {wsStreckenabschnitte=streckenabschnitt:wsStreckenabschnitte}
        objektAnhängen  (OKupplung kupplung)                        = acc {wsKupplungen=kupplung:wsKupplungen}
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        objektAnhängen  _                                           = acc
        qObjektAnhängen :: Objekt -> QWegstrecke
        qObjektAnhängen objekt = QWegstreckeNameAnzahl (objektAnhängen objekt) $ anzahl - 1
        qWeicheAnhängen :: Objekt -> QWegstrecke
        qWeicheAnhängen (OWeiche weiche)    = QWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche
        -- Ignoriere invalide Eingaben; Sollte nie aufgerufen werden
        qWeicheAnhängen _                   = query
queryWegstrecke (QWegstreckeBefehlQuery qKonstruktor eitherF)                                                                   token                               = Left $ QWegstreckeIOStatus (qKonstruktor token) eitherF
queryWegstrecke query@(QWegstreckeNameAnzahlWeicheRichtung acc@(Wegstrecke {wsWeichenRichtungen}) anzahl weiche)                token@(EingabeToken {eingabe})      = case wähleRichtung token of
    (Nothing)       -> Left $ QWSUnbekannt query eingabe
    (Just richtung) -> eitherWeicheRichtungAnhängen richtung
        where
            eitherWeicheRichtungAnhängen :: Richtung -> Either QWegstrecke Wegstrecke
            eitherWeicheRichtungAnhängen richtung = if anzahl > 1 then Left $ qWeicheRichtungAnhängen richtung else Right $ weicheRichtungAnhängen richtung
            qWeicheRichtungAnhängen :: Richtung -> QWegstrecke
            qWeicheRichtungAnhängen richtung = QWegstreckeNameAnzahl (weicheRichtungAnhängen richtung) $ anzahl - 1
            weicheRichtungAnhängen :: Richtung -> Wegstrecke
            weicheRichtungAnhängen richtung = acc {wsWeichenRichtungen=(weiche, richtung):wsWeichenRichtungen}
queryWegstrecke query                                                                           _token                              = Left query
-- | Eingabe einer Weiche
queryWeiche :: QWeiche -> EingabeToken -> Either QWeiche Weiche
queryWeiche (QWeiche)                                                           token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.Märklin  , QMärklinWeiche),
    (Lexer.Lego     , QLegoWeiche)]
    $ QWUnbekannt QWeiche eingabe
queryWeiche (QLegoWeiche)                                                       (EingabeToken {eingabe})            = Left $ QLegoWeicheName eingabe
queryWeiche query@(QLegoWeicheName name)                                        token@(EingabeToken {eingabe})      = Left $ case wähleRichtung token of
    (Nothing)           -> QWUnbekannt query eingabe
    (Just richtung1)    -> QLegoWeicheNameRichtung1 name richtung1
queryWeiche query@(QLegoWeicheNameRichtung1 name richtung1)                     token@(EingabeToken {eingabe})      = Left $ case wähleRichtung token of
    (Nothing)           -> QWUnbekannt query eingabe
    (Just richtung2)    -> QLegoWeicheNameRichtungen name richtung1 richtung2
queryWeiche query@(QLegoWeicheNameRichtungen name richtung1 richtung2)          (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ QWUnbekannt query eingabe
    (Just pin)  -> Right $ LegoWeiche {weName=name, richtungsPin=toPin pin, richtungen=(richtung1,richtung2)}
queryWeiche (QMärklinWeiche)                                                    (EingabeToken {eingabe})            = Left $ QMärklinWeicheName eingabe
queryWeiche query@(QMärklinWeicheName name)                                     (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)       -> Left $ QWUnbekannt query eingabe
    (Just anzahl)   -> Left $ QMärklinWeicheNameAnzahl name anzahl []
queryWeiche query@(QMärklinWeicheNameAnzahl name anzahl acc)                    token@(EingabeToken {eingabe})      = Left $ case wähleRichtung token of
    (Nothing)       -> QWUnbekannt query eingabe
    (Just richtung) -> QMärklinWeicheNameAnzahlRichtung name anzahl acc richtung
queryWeiche query@(QMärklinWeicheNameAnzahlRichtung name anzahl acc richtung)   (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)           -> Left $ QWUnbekannt query eingabe
    (Just pin)
        | anzahl > 1    -> Left $ QMärklinWeicheNameAnzahl name (anzahl - 1) $ (richtung, toPin pin):acc
        | otherwise     -> Right $ MärklinWeiche {weName=name, richtungsPins=(richtung, toPin pin):|acc}
queryWeiche query@(QWUnbekannt _ _)                                             _token                              = Left query
-- | Eingabe einer Bahngeschwindigkeit
queryBahngeschwindigkeit :: QBahngeschwindigkeit -> EingabeToken -> Either QBahngeschwindigkeit Bahngeschwindigkeit
queryBahngeschwindigkeit    (QBahngeschwindigkeit)                                                          token@(EingabeToken {eingabe})      = Left $ wähleBefehl token [
    (Lexer.Märklin  , QMärklinBahngeschwindigkeit),
    (Lexer.Lego     , QLegoBahngeschwindigkeit)]
    $ QBGUnbekannt QBahngeschwindigkeit eingabe
queryBahngeschwindigkeit    (QLegoBahngeschwindigkeit)                                                      (EingabeToken {eingabe})            = Left $ QLegoBahngeschwindigkeitName eingabe
queryBahngeschwindigkeit    query@(QLegoBahngeschwindigkeitName name)                                       (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ QBGUnbekannt query eingabe
    (Just pin)  -> Left $ QLegoBahngeschwindigkeitNameGeschwindigkeit name $ toPin pin
queryBahngeschwindigkeit    query@(QLegoBahngeschwindigkeitNameGeschwindigkeit name geschwindigkeitsPin)  (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ QBGUnbekannt query eingabe
    (Just pin)  -> Right $ LegoBahngeschwindigkeit {bgName=name, geschwindigkeitsPin, fahrtrichtungsPin=toPin pin}
queryBahngeschwindigkeit    (QMärklinBahngeschwindigkeit)                                                   (EingabeToken {eingabe})            = Left $ QMärklinBahngeschwindigkeitName eingabe
queryBahngeschwindigkeit    query@(QMärklinBahngeschwindigkeitName name)                                    (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ QBGUnbekannt query eingabe
    (Just pin)  -> Right $ MärklinBahngeschwindigkeit {bgName=name, geschwindigkeitsPin=toPin pin}
queryBahngeschwindigkeit    query@(QBGUnbekannt _ _)                                                        _token                              = Left query
-- | Eingabe eines Streckenabschnitts
queryStreckenabschnitt :: QStreckenabschnitt -> EingabeToken -> Either QStreckenabschnitt Streckenabschnitt
queryStreckenabschnitt  (QStreckenabschnitt)                (EingabeToken {eingabe})            = Left $ QStreckenabschnittName eingabe
queryStreckenabschnitt  query@(QStreckenabschnittName name) (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ QSUnbekannt query eingabe
    (Just pin)  -> Right $ Streckenabschnitt {stName=name, stromPin=toPin pin}
queryStreckenabschnitt  query@(QSUnbekannt _ _)             _token                              = Left query
-- | Eingabe einer Kupplung
queryKupplung :: QKupplung -> EingabeToken -> Either QKupplung Kupplung
queryKupplung   (QKupplung)                 (EingabeToken {eingabe})            = Left $ QKupplungName eingabe
queryKupplung   query@(QKupplungName name)  (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Nothing)   -> Left $ QKUnbekannt query eingabe
    (Just pin)  -> Right $ Kupplung {kuName=name, kupplungsPin=toPin pin}
queryKupplung   query@(QKUnbekannt _ _)     _token                       = Left query

-- * Klasse für unvollständige Befehle
class Query q where
    getQuery :: (IsString s, Semigroup s) => q -> s
    getQueryFailed :: (IsString s, Semigroup s) => q -> s -> s
    getQueryFailed = getQueryFailedDefault
    getQueryOptions :: (IsString s, Semigroup s) => q -> Maybe s
    getQueryOptions _query = Nothing
    {-# MINIMAL getQuery #-}

getQueryFailedDefault :: (Query q, IsString s, Semigroup s) => q -> s -> s
getQueryFailedDefault q eingabe = Language.unbekannt (getQuery q) <=> eingabe

showQuery :: (Show q, Query q, IsString s, Semigroup s) => q -> s
showQuery q = showText q <^> getQuery q

showQueryFailed :: (Show q, Query q, IsString s, Semigroup s) => q -> s -> s
showQueryFailed q eingabe = showText q <^> getQueryFailed q eingabe

-- | Rückgabe-Typen
data QErgebnis  = QEBefehl          Befehl
                | QEBefehlSofort    BefehlSofort    [AllgemeinesEingabeToken]
                | QEBefehlQuery     QObjektIOStatus (Either (Objekt -> QBefehl) (Objekt -> Befehl))    QBefehl  [AllgemeinesEingabeToken]
                | QEQBefehl         QBefehl

-- | Befehle, die sofort ausgführt werden müssen
data BefehlSofort   = BSLaden           FilePath

-- | Unvollständiger Befehl, für den zum Auswerten die IOStatus-Monade benötigt wird
data QObjektIOStatus    = QOIOSUnbekannt            Text
                        | QOIOSPlan                 EingabeToken
                        | QOIOSWegstrecke           EingabeToken
                        | QOIOSWeiche               EingabeToken
                        | QOIOSBahngeschwindigkeit  EingabeToken
                        | QOIOSStreckenabschnitt    EingabeToken
                        | QOIOSKupplung             EingabeToken

instance Show QObjektIOStatus where
    show :: QObjektIOStatus -> String
    show    query@(QOIOSUnbekannt eingabe)      = unpack $ getQueryFailed query eingabe
    show    (QOIOSPlan _token)                  = Language.plan
    show    (QOIOSWegstrecke _token)            = Language.wegstrecke
    show    (QOIOSWeiche _token)                = Language.weiche
    show    (QOIOSBahngeschwindigkeit _token)   = Language.bahngeschwindigkeit
    show    (QOIOSStreckenabschnitt _token)     = Language.streckenabschnitt
    show    (QOIOSKupplung _token)              = Language.kupplung
instance Query QObjektIOStatus where
    getQuery :: (IsString s, Semigroup s) => QObjektIOStatus -> s
    getQuery    (QOIOSUnbekannt _eingabe)           = Language.objekt
    getQuery    (QOIOSPlan _token)                  = Language.indexOderName Language.plan
    getQuery    (QOIOSWegstrecke _token)            = Language.indexOderName Language.wegstrecke
    getQuery    (QOIOSWeiche _token)                = Language.indexOderName Language.weiche
    getQuery    (QOIOSBahngeschwindigkeit _token)   = Language.indexOderName Language.bahngeschwindigkeit
    getQuery    (QOIOSStreckenabschnitt _token)     = Language.indexOderName Language.streckenabschnitt
    getQuery    (QOIOSKupplung _token)              = Language.indexOderName Language.kupplung

-- | Unvollständige Befehle
data QBefehl    = QBefehl
                | QBUnbekannt   QBefehl                             Text
                | QBHinzufügen  QObjekt
                | QBEntfernen
                | QBSpeichern
                | QBLaden
                | QBAktionPlan  Plan
                | QBAktion      QAktion
                | QBBefehlQuery (EingabeToken -> QObjektIOStatus)   (Either (Objekt -> QBefehl) (Objekt -> Befehl))

instance Show QBefehl where
    show :: QBefehl -> String
    show    (QBefehl)                               = Language.befehl
    show    (QBUnbekannt query eingabe)             = unpack $ unbekanntShowText query eingabe
    show    (QBHinzufügen qObjekt)                  = Language.hinzufügen <^> showText qObjekt
    show    (QBEntfernen)                           = Language.entfernen
    show    (QBSpeichern)                           = Language.speichern
    show    (QBLaden)                               = Language.laden
    show    (QBAktionPlan plan)                     = Language.aktion <^> showText plan
    show    (QBAktion qAktion)                      = showText qAktion
    show    (QBBefehlQuery qKonstruktor _eitherF)   = showText $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
instance Query QBefehl where
    getQuery :: (IsString s, Semigroup s) => QBefehl -> s
    getQuery    (QBefehl)                               = Language.befehl
    getQuery    (QBUnbekannt query _eingabe)            = getQuery query
    getQuery    (QBHinzufügen qObjekt)                  = getQuery qObjekt
    getQuery    (QBEntfernen)                           = Language.objekt
    getQuery    (QBSpeichern)                           = Language.dateiname
    getQuery    (QBLaden)                               = Language.dateiname
    getQuery    (QBAktionPlan _plan)                    = Language.aktion
    getQuery    (QBAktion qAktion)                      = getQuery qAktion
    getQuery    (QBBefehlQuery qKonstruktor _eitherF)   = getQuery $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    getQueryOptions :: (IsString s, Semigroup s) => QBefehl -> Maybe s
    getQueryOptions (QBUnbekannt query _eingabe)            = getQueryOptions query
    getQueryOptions (QBHinzufügen qObjekt)                  = getQueryOptions qObjekt
    getQueryOptions (QBAktionPlan _plan)                    = Just $ toBefehlsString Language.aktionPlan
    getQueryOptions (QBAktion qAktion)                      = getQueryOptions qAktion
    getQueryOptions (QBBefehlQuery qKonstruktor _eitherF)   = getQueryOptions $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    getQueryOptions _query  = Nothing

-- | Unvollständige Objekte
data QObjekt    = QObjekt
                | QOUnbekannt           QObjekt                 Text
                | QOPlan                QPlan
                | QOWegstrecke          QWegstrecke
                | QOWeiche              QWeiche
                | QOBahngeschwindigkeit QBahngeschwindigkeit
                | QOStreckenabschnitt   QStreckenabschnitt
                | QOKupplung            QKupplung
                | QOIOStatus            QObjektIOStatus                     (Either (Objekt -> QObjekt) (Objekt -> Objekt))

instance Show QObjekt where
    show :: QObjekt -> String
    show    (QOUnbekannt qObjekt eingabe)                   = unpack $ unbekanntShowText qObjekt eingabe
    show    (QObjekt)                                       = Language.objekt
    show    (QOPlan qPlan)                                  = showText qPlan
    show    (QOWegstrecke qWegstrecke)                      = showText qWegstrecke
    show    (QOWeiche qWeiche)                              = showText qWeiche
    show    (QOBahngeschwindigkeit qBahngeschwindigkeit)    = showText qBahngeschwindigkeit
    show    (QOStreckenabschnitt qStreckenabschnitt)        = showText qStreckenabschnitt
    show    (QOKupplung qKupplung)                          = showText qKupplung
    show    (QOIOStatus qObjektIOStatus _eitherKonstruktor) = showText qObjektIOStatus
instance Query QObjekt where
    getQuery :: (IsString s, Semigroup s) => QObjekt -> s
    getQuery    (QOUnbekannt qObjekt _eingabe)                  = getQuery qObjekt
    getQuery    (QObjekt)                                       = Language.objekt
    getQuery    (QOPlan qPlan)                                  = getQuery qPlan
    getQuery    (QOWegstrecke qWegstrecke)                      = getQuery qWegstrecke
    getQuery    (QOWeiche qWeiche)                              = getQuery qWeiche
    getQuery    (QOBahngeschwindigkeit qBahngeschwindigkeit)    = getQuery qBahngeschwindigkeit
    getQuery    (QOStreckenabschnitt qStreckenabschnitt)        = getQuery qStreckenabschnitt
    getQuery    (QOKupplung qKupplung)                          = getQuery qKupplung
    getQuery    (QOIOStatus qObjektIOStatus _eitherKonstruktor) = getQuery qObjektIOStatus
    getQueryOptions :: (IsString s, Semigroup s) => QObjekt -> Maybe s
    getQueryOptions (QOUnbekannt qObjekt _eingabe)                  = getQueryOptions qObjekt
    getQueryOptions (QObjekt)                                       = Just $ toBefehlsString Language.befehlTypen
    getQueryOptions (QOPlan qPlan)                                  = getQueryOptions qPlan
    getQueryOptions (QOWegstrecke qWegstrecke)                      = getQueryOptions qWegstrecke
    getQueryOptions (QOWeiche qWeiche)                              = getQueryOptions qWeiche
    getQueryOptions (QOBahngeschwindigkeit qBahngeschwindigkeit)    = getQueryOptions qBahngeschwindigkeit
    getQueryOptions (QOStreckenabschnitt qStreckenabschnitt)        = getQueryOptions qStreckenabschnitt
    getQueryOptions (QOKupplung qKupplung)                          = getQueryOptions qKupplung
    getQueryOptions (QOIOStatus qObjektIOStatus _eitherKonstruktor) = getQueryOptions qObjektIOStatus

data QAktionElement = QAEUnbekannt              Text
                    | QAEWarten
                    | QAEWegstrecke
                    | QAEWeiche
                    | QAEBahngeschwindigkeit
                    | QAEStreckenabschnitt
                    | QAEKupplung

data QWegstreckenElement    = QWEUnbekannt              Text
                            | QWEWeiche
                            | QWEBahngeschwindigkeit
                            | QWEStreckenabschnitt
                            | QWEKupplung

-- | Unvollständiger Plan
data QPlan  = QPlan
            | QPUnbekannt       QPlan                               Text
            | QPlanName         Text
            | QPlanNameAnzahl   Text                                Natural                                     (SEQueue Aktion)    QAktion
            | QPlanIOStatus     QObjektIOStatus                     (Either (Objekt -> QPlan) (Objekt -> Plan))
            | QPlanBefehlQuery  (EingabeToken -> QObjektIOStatus)   (Either (Objekt -> QPlan) (Objekt -> Plan))

instance Show QPlan where
    show :: QPlan -> String
    show    (QPUnbekannt qPlan eingabe)                         = unpack $ unbekanntShowText qPlan eingabe
    show    (QPlan)                                             = Language.plan
    show    (QPlanName name)                                    = unpack $ Language.plan <^> Language.name <=> name
    show    (QPlanNameAnzahl name anzahl acc qAktion)           = unpack $ Language.plan <^> Language.name <=> name <^> Language.anzahl Language.aktionen <=> showText anzahl <^> showText acc <^> showText qAktion
    show    (QPlanIOStatus qObjektIOStatus _eitherKonstruktor)  = Language.plan <^> showText qObjektIOStatus
    show    (QPlanBefehlQuery qKonstruktor _eitherF)            = Language.plan <^> Language.aktion <-> Language.objekt <^> showText (qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing})
instance Query QPlan where
    getQuery :: (IsString s, Semigroup s) => QPlan -> s
    getQuery    (QPUnbekannt qPlan _eingabe)                        = getQuery qPlan
    getQuery    (QPlan)                                             = Language.name
    getQuery    (QPlanName _name)                                   = Language.anzahl Language.aktionen
    getQuery    (QPlanNameAnzahl _name _anzahl _acc qAktion)        = getQuery qAktion
    getQuery    (QPlanIOStatus qObjektIOStatus _eitherKonstruktor)  = getQuery qObjektIOStatus
    getQuery    (QPlanBefehlQuery qKonstruktor _eitherF)            = getQuery $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    getQueryFailed :: (IsString s, Semigroup s) => QPlan -> s -> s
    getQueryFailed  q@(QPlanName _name) eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q                   eingabe = getQueryFailedDefault q eingabe
    getQueryOptions :: (IsString s, Semigroup s) => QPlan -> Maybe s
    getQueryOptions (QPUnbekannt qPlan _eingabe)                        = getQueryOptions qPlan
    getQueryOptions (QPlan)                                             = Nothing
    getQueryOptions (QPlanName _name)                                   = Nothing
    getQueryOptions (QPlanNameAnzahl _name _anzahl _acc qAktion)        = getQueryOptions qAktion
    getQueryOptions (QPlanIOStatus qObjektIOStatus _eitherKonstruktor)  = getQueryOptions qObjektIOStatus
    getQueryOptions (QPlanBefehlQuery qKonstruktor _eitherF)            = getQueryOptions $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}

data QAktion    = QAktion
                | QAUnbekannt           QAktion                                                                 Text
                | QAWarten
                | QAWegstrecke          (QAktionWegstrecke QWegstrecke Wegstrecke)
                | QAWeiche              (QAktionWeiche QWeiche Weiche)
                | QABahngeschwindigkeit (QAktionBahngeschwindigkeit QBahngeschwindigkeit Bahngeschwindigkeit)
                | QAStreckenabschnitt   (QAktionStreckenabschnitt QStreckenabschnitt Streckenabschnitt)
                | QAKupplung            (QAktionKupplung QKupplung Kupplung)
                | QAktionIOStatus       QObjektIOStatus                                                         (Either (Objekt -> QAktion) (Objekt -> Aktion))
                | QAktionBefehlQuery    (EingabeToken -> QObjektIOStatus)                                       (Either (Objekt -> QAktion) (Objekt -> Aktion))

instance Show QAktion where
    show :: QAktion -> String
    show    (QAUnbekannt qAktion eingabe)                           = unpack $ unbekanntShowText qAktion eingabe
    show    (QAktion)                                               = unpack $ Language.aktion
    show    (QAWarten)                                              = unpack $ Language.aktion <^> Language.warten
    show    (QAWegstrecke qAktionWegstrecke)                        = Language.aktion <^> showText qAktionWegstrecke
    show    (QAWeiche qAktionWeiche)                                = Language.aktion <^> showText qAktionWeiche
    show    (QABahngeschwindigkeit qAktionBahngeschwindigkeit)      = Language.aktion <^> showText qAktionBahngeschwindigkeit
    show    (QAStreckenabschnitt qAktionStreckenabschnitt)          = Language.aktion <^> showText qAktionStreckenabschnitt
    show    (QAKupplung qAktionKupplung)                            = Language.aktion <^> showText qAktionKupplung
    show    (QAktionIOStatus qObjektIOStatus _eitherKonstruktor)    = Language.aktion <^> showText qObjektIOStatus
    show    (QAktionBefehlQuery qKonstruktor _eitherF)              = Language.aktion <-> Language.objekt <^> showText (qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing})
instance Query QAktion where
    getQuery :: (IsString s, Semigroup s) => QAktion -> s
    getQuery    (QAUnbekannt qAktion _eingabe)                          = getQuery qAktion
    getQuery    (QAktion)                                               = Language.aktion
    getQuery    (QAWarten)                                              = Language.zeit
    getQuery    (QAWegstrecke qAktionWegstrecke)                        = getQuery qAktionWegstrecke
    getQuery    (QAWeiche qAktionWeiche)                                = getQuery qAktionWeiche
    getQuery    (QABahngeschwindigkeit qAktionBahngeschwindigkeit)      = getQuery qAktionBahngeschwindigkeit
    getQuery    (QAStreckenabschnitt qAktionStreckenabschnitt)          = getQuery qAktionStreckenabschnitt
    getQuery    (QAKupplung qAktionKupplung)                            = getQuery qAktionKupplung
    getQuery    (QAktionIOStatus qObjektIOStatus _eitherKonstruktor)    = getQuery qObjektIOStatus
    getQuery    (QAktionBefehlQuery qKonstruktor _eitherF)              = getQuery $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    getQueryFailed :: (IsString s, Semigroup s) => QAktion -> s -> s
    getQueryFailed  q@(QAWarten)    eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q               eingabe = getQueryFailedDefault q eingabe
    getQueryOptions :: (IsString s, Semigroup s) => QAktion -> Maybe s
    getQueryOptions (QAUnbekannt qAktion _eingabe)                          = getQueryOptions qAktion
    getQueryOptions (QAktion)                                               = Just $ toBefehlsString Language.aktionGruppen
    getQueryOptions (QAWarten)                                              = Nothing
    getQueryOptions (QAWegstrecke qAktionWegstrecke)                        = getQueryOptions qAktionWegstrecke
    getQueryOptions (QAWeiche qAktionWeiche)                                = getQueryOptions qAktionWeiche
    getQueryOptions (QABahngeschwindigkeit qAktionBahngeschwindigkeit)      = getQueryOptions qAktionBahngeschwindigkeit
    getQueryOptions (QAStreckenabschnitt qAktionStreckenabschnitt)          = getQueryOptions qAktionStreckenabschnitt
    getQueryOptions (QAKupplung qAktionKupplung)                            = getQueryOptions qAktionKupplung
    getQueryOptions (QAktionIOStatus qObjektIOStatus _eitherKonstruktor)    = getQueryOptions qObjektIOStatus
    getQueryOptions (QAktionBefehlQuery qKonstruktor _eitherF)              = getQueryOptions $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}

data QAktionWegstrecke qw w = QAktionWegstrecke         w
                            | QAWSUnbekannt             (QAktionWegstrecke qw w)            Text
                            | QAWSBahngeschwindigkeit   (QAktionBahngeschwindigkeit qw w)
                            | QAWSStreckenabschnitt     (QAktionStreckenabschnitt qw w)
                            | QAWSKupplung              (QAktionKupplung qw w)

instance (Show qw, Show w) => Show (QAktionWegstrecke qw w) where
    show :: QAktionWegstrecke qw w -> String
    show    (QAktionWegstrecke wegstrecke)      = Language.wegstrecke <=> showText wegstrecke
    show    (QAWSUnbekannt qAktion eingabe)     = unpack $ unbekanntShowText qAktion eingabe
    show    (QAWSBahngeschwindigkeit qAktion)   = showText qAktion
    show    (QAWSStreckenabschnitt qAktion)     = showText qAktion
    show    (QAWSKupplung qAktion)              = showText qAktion
instance Query (QAktionWegstrecke qw w) where
    getQuery :: (IsString s, Semigroup s) => QAktionWegstrecke qw w -> s
    getQuery    (QAktionWegstrecke _wegstrecke)     = Language.aktion
    getQuery    (QAWSUnbekannt qAktion _eingabe)    = getQuery qAktion
    getQuery    (QAWSBahngeschwindigkeit qAktion)   = getQuery qAktion
    getQuery    (QAWSStreckenabschnitt qAktion)     = getQuery qAktion
    getQuery    (QAWSKupplung qAktion)              = getQuery qAktion
    getQueryOptions :: (IsString s, Semigroup s) => QAktionWegstrecke qw w -> Maybe s
    getQueryOptions (QAktionWegstrecke _wegstrecke)     = Just $ toBefehlsString Language.aktionWegstrecke
    getQueryOptions (QAWSUnbekannt qAktion _eingabe)    = getQueryOptions qAktion
    getQueryOptions (QAWSBahngeschwindigkeit qAktion)   = getQueryOptions qAktion
    getQueryOptions (QAWSStreckenabschnitt qAktion)     = getQueryOptions qAktion
    getQueryOptions (QAWSKupplung qAktion)              = getQueryOptions qAktion

data QAktionWeiche qw w = QAktionWeiche         w
                        | QAWUnbekannt          (QAktionWeiche qw w)    Text
                        | QAWStellen            w

instance (Show qw, Show w) => Show (QAktionWeiche qw w) where
    show :: QAktionWeiche qw w -> String
    show    (QAktionWeiche weiche)          = Language.weiche <=> showText weiche
    show    (QAWUnbekannt qAktion eingabe)  = unpack $ unbekanntShowText qAktion eingabe
    show    (QAWStellen weiche)             = Language.weiche <=> showText weiche <^> Language.stellen
instance Query (QAktionWeiche qw w) where
    getQuery :: (IsString s, Semigroup s) => QAktionWeiche qw w -> s
    getQuery    (QAktionWeiche _weiche)         = Language.aktion
    getQuery    (QAWUnbekannt qAktion _eingabe) = getQuery qAktion
    getQuery    (QAWStellen _weiche)            = Language.richtung
    {-
    getQueryFailed :: (IsString s, Semigroup s) => (QAktionWeiche qw w) -> s -> s
    getQueryFailed  q@(QAWStellen _weiche)  eingabe = getQueryFailedDefault q eingabe <^> Language.richtungErwartet
    getQueryFailed  q                       eingabe = getQueryFailedDefault q eingabe
    -}
    getQueryOptions :: (IsString s, Semigroup s) => QAktionWeiche qw w -> Maybe s
    getQueryOptions (QAktionWeiche _weiche)         = Just $ toBefehlsString Language.aktionWeiche
    getQueryOptions (QAWUnbekannt qAktion _eingabe) = getQueryOptions qAktion
    getQueryOptions (QAWStellen _weiche)            = Just $ toBefehlsString $ NE.toList $ fmap showText unterstützteRichtungen

data QAktionBahngeschwindigkeit qb b    = QAktionBahngeschwindigkeit            b
                                        | QABGUnbekannt                         (QAktionBahngeschwindigkeit qb b)   Text
                                        | QABGGeschwindigkeit                   b
                                        | QABGUmdrehen                          b

instance (Show qb, Show b) => Show (QAktionBahngeschwindigkeit qb b) where
    show :: QAktionBahngeschwindigkeit qb b -> String
    show    (QAktionBahngeschwindigkeit bahngeschwindigkeit)    = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit
    show    (QABGUnbekannt qAktion eingabe)                     = unpack $ unbekanntShowText qAktion eingabe
    show    (QABGGeschwindigkeit bahngeschwindigkeit)           = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit <^> Language.geschwindigkeit
    show    (QABGUmdrehen bahngeschwindigkeit)                  = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit <^> Language.umdrehen
instance Query (QAktionBahngeschwindigkeit qb b) where
    getQuery :: (IsString s, Semigroup s) => QAktionBahngeschwindigkeit qb b -> s
    getQuery    (QAktionBahngeschwindigkeit _bahngeschwindigkeit)   = Language.aktion
    getQuery    (QABGUnbekannt qAktion _eingabe)                    = getQuery qAktion
    getQuery    (QABGGeschwindigkeit _bahngeschwindigkeit)          = Language.geschwindigkeit
    getQuery    (QABGUmdrehen _bahngeschwindigkeit)                 = Language.fahrtrichtung
    getQueryFailed :: (IsString s, Semigroup s) => QAktionBahngeschwindigkeit qb b -> s -> s
    getQueryFailed  q@(QABGGeschwindigkeit _bahngeschwindigkeit)    eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    -- getQueryFailed  q@(QABGUmdrehen _bahngeschwindigkeit)          eingabe = getQueryFailedDefault q eingabe <^> Language.fahrtrichtungErwartet
    getQueryFailed q                                                eingabe = getQueryFailedDefault q eingabe
    getQueryOptions :: (IsString s, Semigroup s) => QAktionBahngeschwindigkeit bg b -> Maybe s
    getQueryOptions (QAktionBahngeschwindigkeit _bahngeschwindigkeit)   = Just $ toBefehlsString Language.aktionBahngeschwindigkeit
    getQueryOptions (QABGUnbekannt qAktion _eingabe)                    = getQueryOptions qAktion
    getQueryOptions (QABGGeschwindigkeit _bahngeschwindigkeit)          = Nothing
    getQueryOptions (QABGUmdrehen _bahngeschwindigkeit)                 = Just $ toBefehlsString $ map showText $ NE.toList unterstützteFahrtrichtungen

data QAktionStreckenabschnitt qs s  = QAktionStreckenabschnitt          s
                                    | QASUnbekannt                      (QAktionStreckenabschnitt qs s) Text
                                    | QASStrom                          s

instance (Show qs, Show s) => Show (QAktionStreckenabschnitt qs s) where
    show :: QAktionStreckenabschnitt qs s -> String
    show    (QAktionStreckenabschnitt streckenabschnitt)    = Language.streckenabschnitt <=> showText streckenabschnitt
    show    (QASUnbekannt qAktion eingabe)                  = unpack $ unbekanntShowText qAktion eingabe
    show    (QASStrom streckenabschnitt)                    = Language.streckenabschnitt <=> showText streckenabschnitt <^> Language.strom
instance Query (QAktionStreckenabschnitt qst st) where
    getQuery :: (IsString s, Semigroup s) => QAktionStreckenabschnitt qst st -> s
    getQuery    (QAktionStreckenabschnitt _streckenabschnitt)   = Language.aktion
    getQuery    (QASUnbekannt qAktion _eingabe)                 = getQuery qAktion
    getQuery    (QASStrom _streckenabschnitt)                   = Language.an <|> Language.aus
    getQueryOptions :: (IsString s, Semigroup s) => QAktionStreckenabschnitt qst st -> Maybe s
    getQueryOptions (QAktionStreckenabschnitt _streckenabschnitt)   = Just $ toBefehlsString Language.aktionStreckenabschnitt
    getQueryOptions (QASUnbekannt qAktion _eingabe)                 = getQueryOptions qAktion
    getQueryOptions (QASStrom _streckenabschnitt)                   = Just $ toBefehlsString [Language.an, Language.aus]

data QAktionKupplung qk k   = QAktionKupplung           k
                            | QAKUnbekannt              (QAktionKupplung qk k)  Text

instance (Show qk, Show k) => Show (QAktionKupplung qk k) where
    show :: QAktionKupplung qk k -> String
    show    (QAktionKupplung kupplung)      = Language.kupplung <=> showText kupplung
    show    (QAKUnbekannt qAktion eingabe)  = unpack $ unbekanntShowText qAktion eingabe
instance Query (QAktionKupplung qk k) where
    getQuery :: (IsString s, Semigroup s) => QAktionKupplung qk k -> s
    getQuery    (QAktionKupplung _kupplung)     = Language.aktion
    getQuery    (QAKUnbekannt qAktion _eingabe) = getQuery qAktion
    getQueryOptions :: (IsString s, Semigroup s) => QAktionKupplung qk k -> Maybe s
    getQueryOptions (QAktionKupplung _kupplung)     = Just $ toBefehlsString Language.aktionKupplung
    getQueryOptions (QAKUnbekannt qAktion _eingabe) = getQueryOptions qAktion

-- | Unvollständige Wegstrecke
data QWegstrecke    = QWegstrecke
                    | QWSUnbekannt                              QWegstrecke                         Text
                    | QWegstreckeName                           Text
                    | QWegstreckeNameAnzahl                     Wegstrecke                          Natural
                    | QWegstreckeNameAnzahlWeicheRichtung       Wegstrecke                          Natural                                                 Weiche
                    | QWegstreckeIOStatus                       QObjektIOStatus                     (Either (Objekt -> QWegstrecke) (Objekt -> Wegstrecke))
                    | QWegstreckeBefehlQuery                    (EingabeToken -> QObjektIOStatus)   (Either (Objekt -> QWegstrecke) (Objekt -> Wegstrecke))

instance Show QWegstrecke where
    show :: QWegstrecke -> String
    show    (QWSUnbekannt qWegstrecke eingabe)                              = unpack $ unbekanntShowText qWegstrecke eingabe
    show    (QWegstrecke)                                                   = unpack $ Language.wegstrecke
    show    (QWegstreckeName name)                                          = unpack $ Language.wegstrecke <^> Language.name <=> name
    show    (QWegstreckeNameAnzahl acc anzahl)                              = unpack $ Language.wegstrecke <^> showText acc <^> Language.anzahl Language.wegstreckenElemente <=> showText anzahl
    show    (QWegstreckeNameAnzahlWeicheRichtung acc anzahl weiche)         = unpack $ Language.wegstrecke <^> showText acc <^> Language.anzahl Language.wegstreckenElemente <=> showText anzahl <^> showText weiche
    show    (QWegstreckeIOStatus qObjektIOStatus _eitherKonstruktor)        = Language.wegstrecke <^> showText qObjektIOStatus
    show    (QWegstreckeBefehlQuery qKonstruktor _eitherF)                  = Language.wegstreckenElement <^> showText (qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing})
instance Query QWegstrecke where
    getQuery :: (IsString s, Semigroup s) => QWegstrecke -> s
    getQuery    (QWSUnbekannt qWegstrecke _eingabe)                         = getQuery qWegstrecke
    getQuery    (QWegstrecke)                                               = Language.name
    getQuery    (QWegstreckeName _name)                                     = Language.anzahl Language.wegstreckenElemente
    getQuery    (QWegstreckeNameAnzahl _acc _anzahl)                        = Language.wegstreckenElement
    getQuery    (QWegstreckeNameAnzahlWeicheRichtung _acc _anzahl _weiche)  = Language.richtung
    getQuery    (QWegstreckeIOStatus qObjektIOStatus _eitherKonstruktor)    = getQuery qObjektIOStatus
    getQuery    (QWegstreckeBefehlQuery qKonstruktor _eitherF)              = getQuery $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    getQueryOptions :: (IsString s, Semigroup s) => QWegstrecke -> Maybe s
    getQueryOptions (QWSUnbekannt qWegstrecke _eingabe)                         = getQueryOptions qWegstrecke
    getQueryOptions (QWegstreckeNameAnzahl _acc _anzahl)                        = Just $ toBefehlsString Language.befehlWegstreckenElemente
    getQueryOptions (QWegstreckeNameAnzahlWeicheRichtung _acc _anzahl _weiche)  = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    getQueryOptions (QWegstreckeIOStatus qObjektIOStatus _eitherKonstruktor)    = getQueryOptions qObjektIOStatus
    getQueryOptions (QWegstreckeBefehlQuery qKonstruktor _eitherF)              = getQueryOptions $ qKonstruktor $ EingabeToken {eingabe="", möglichkeiten=[], ganzzahl=Nothing}
    getQueryOptions _query                                                      = Nothing

data QWeiche    = QWeiche
                | QWUnbekannt                       QWeiche Text
                | QLegoWeiche
                | QLegoWeicheName                   Text
                | QLegoWeicheNameRichtung1          Text  Richtung
                | QLegoWeicheNameRichtungen         Text  Richtung  Richtung
                | QMärklinWeiche
                | QMärklinWeicheName                Text
                | QMärklinWeicheNameAnzahl          Text  Natural   [(Richtung, Pin)]
                | QMärklinWeicheNameAnzahlRichtung  Text  Natural   [(Richtung, Pin)]   Richtung

instance Show QWeiche where
    show :: QWeiche -> String
    show    (QWeiche)                                                       = Language.weiche
    show    (QWUnbekannt query eingabe)                                     = unpack $ unbekanntShowText query eingabe
    show    (QLegoWeiche)                                                   = Language.lego <-> Language.weiche
    show    (QLegoWeicheName name)                                          = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name
    show    (QLegoWeicheNameRichtung1 name richtung1)                       = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> showText richtung1
    show    (QLegoWeicheNameRichtungen name richtung1 richtung2)            = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> showText richtung1 <^> showText richtung2
    show    (QMärklinWeiche)                                                = Language.märklin <-> Language.weiche
    show    (QMärklinWeicheName name)                                       = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name
    show    (QMärklinWeicheNameAnzahl name anzahl acc)                      = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> Language.erwartet Language.richtungen <=> showText anzahl <^> showText acc
    show    (QMärklinWeicheNameAnzahlRichtung name anzahl acc richtung)     = unpack $ Language.lego <-> Language.weiche <^> Language.name <=> name <^> Language.erwartet Language.richtungen <=> showText anzahl <^> showText acc <> Language.richtung <=> showText richtung
instance Query QWeiche where
    getQuery :: (IsString s, Semigroup s) => QWeiche -> s
    getQuery    (QWeiche)                                                       = Language.zugtyp
    getQuery    (QWUnbekannt query _eingabe)                                    = getQuery query
    getQuery    (QLegoWeiche)                                                   = Language.name
    getQuery    (QLegoWeicheName _name)                                         = Language.richtung
    getQuery    (QLegoWeicheNameRichtung1 _name _richtung1)                     = Language.richtung
    getQuery    (QLegoWeicheNameRichtungen _name _richtung1 _richtung2)         = Language.pin
    getQuery    (QMärklinWeiche)                                                = Language.name
    getQuery    (QMärklinWeicheName _name)                                      = Language.anzahl Language.richtungen
    getQuery    (QMärklinWeicheNameAnzahl _name _anzahl _acc)                   = Language.richtung
    getQuery    (QMärklinWeicheNameAnzahlRichtung _name _anzahl _acc _richtung) = Language.pin
    getQueryFailed :: (IsString s, Semigroup s) => QWeiche -> s -> s
    getQueryFailed  q@(QLegoWeicheNameRichtungen _name _richtung1 _richtung2)           eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q@(QMärklinWeicheName _name)                                        eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q@(QMärklinWeicheNameAnzahlRichtung _name _anzahl _acc _richtung)   eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q                                                                   eingabe = getQueryFailedDefault q eingabe
    getQueryOptions :: (IsString s, Semigroup s) => QWeiche -> Maybe s
    getQueryOptions (QWeiche)                                       = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    getQueryOptions (QWUnbekannt query _eingabe)                    = getQueryOptions query
    getQueryOptions (QLegoWeicheName _name)                         = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    getQueryOptions (QLegoWeicheNameRichtung1 _name _richtung1)     = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    getQueryOptions (QMärklinWeicheNameAnzahl _name _anzahl _acc)   = Just $ toBefehlsString $ map showText $ NE.toList unterstützteRichtungen
    getQueryOptions _query                                          = Nothing

data QBahngeschwindigkeit   = QBahngeschwindigkeit
                            | QBGUnbekannt                                   QBahngeschwindigkeit    Text
                            | QLegoBahngeschwindigkeit
                            | QLegoBahngeschwindigkeitName                  Text
                            | QLegoBahngeschwindigkeitNameGeschwindigkeit   Text                  Pin
                            | QMärklinBahngeschwindigkeit
                            | QMärklinBahngeschwindigkeitName               Text

instance Show QBahngeschwindigkeit where
    show :: QBahngeschwindigkeit -> String
    show    (QBahngeschwindigkeit)                                      = Language.bahngeschwindigkeit
    show    (QBGUnbekannt query eingabe)                                = unpack $ unbekanntShowText query eingabe
    show    (QLegoBahngeschwindigkeit)                                  = Language.lego <-> Language.bahngeschwindigkeit
    show    (QLegoBahngeschwindigkeitName name)                         = unpack $ Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name
    show    (QLegoBahngeschwindigkeitNameGeschwindigkeit name pin)      = unpack $ Language.lego <-> Language.bahngeschwindigkeit <^> Language.name <=> name <^> Language.pin <=> showText pin
    show    (QMärklinBahngeschwindigkeit)                               = Language.märklin <-> Language.bahngeschwindigkeit
    show    (QMärklinBahngeschwindigkeitName name)                      = unpack $ Language.märklin <-> Language.bahngeschwindigkeit <^> Language.name <=> name
instance Query QBahngeschwindigkeit where
    getQuery :: (IsString s, Semigroup s) => QBahngeschwindigkeit -> s
    getQuery    (QBahngeschwindigkeit)                                      = Language.zugtyp
    getQuery    (QBGUnbekannt query _eingabe)                               = getQuery query
    getQuery    (QLegoBahngeschwindigkeit)                                  = Language.name
    getQuery    (QLegoBahngeschwindigkeitName _name)                        = Language.pin
    getQuery    (QLegoBahngeschwindigkeitNameGeschwindigkeit _name _pin)    = Language.pin
    getQuery    (QMärklinBahngeschwindigkeit)                               = Language.name
    getQuery    (QMärklinBahngeschwindigkeitName _name)                     = Language.pin
    getQueryFailed :: (IsString s, Semigroup s) => QBahngeschwindigkeit -> s -> s
    getQueryFailed  q@(QLegoBahngeschwindigkeitName _name)                      eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q@(QLegoBahngeschwindigkeitNameGeschwindigkeit _name _pin)  eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q@(QMärklinBahngeschwindigkeitName _name)                   eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q                                                           eingabe = getQueryFailedDefault q eingabe
    getQueryOptions :: (IsString s, Semigroup s) => QBahngeschwindigkeit -> Maybe s
    getQueryOptions (QBahngeschwindigkeit)          = Just $ toBefehlsString $ map showText $ NE.toList unterstützteZugtypen
    getQueryOptions (QBGUnbekannt query _eingabe)   = getQueryOptions query
    getQueryOptions _query                          = Nothing

data QStreckenabschnitt = QStreckenabschnitt
                        | QSUnbekannt               QStreckenabschnitt  Text
                        | QStreckenabschnittName    Text

instance Show QStreckenabschnitt where
    show :: QStreckenabschnitt -> String
    show    (QStreckenabschnitt)            = Language.streckenabschnitt
    show    (QSUnbekannt query eingabe)     = unpack $ unbekanntShowText query eingabe
    show    (QStreckenabschnittName name)   = unpack $ Language.streckenabschnitt <^> Language.name <=> name 
instance Query QStreckenabschnitt where
    getQuery :: (IsString s, Semigroup s) => QStreckenabschnitt -> s
    getQuery    (QStreckenabschnitt)            = Language.name
    getQuery    (QSUnbekannt query _eingabe)    = getQuery query
    getQuery    (QStreckenabschnittName _name)  = Language.pin
    getQueryFailed :: (IsString s, Semigroup s) => QStreckenabschnitt -> s -> s
    getQueryFailed  q@(QStreckenabschnittName _name)    eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q                                   eingabe = getQueryFailedDefault q eingabe
    getQueryOptions :: (IsString s, Semigroup s) => QStreckenabschnitt -> Maybe s
    getQueryOptions (QSUnbekannt query _eingabe)    = getQueryOptions query
    getQueryOptions _query                          = Nothing

data QKupplung  = QKupplung
                | QKUnbekannt   QKupplung   Text
                | QKupplungName Text

instance Show QKupplung where
    show :: QKupplung -> String
    show    (QKupplung)                 = Language.kupplung
    show    (QKUnbekannt query eingabe) = unpack $ unbekanntShowText query eingabe
    show    (QKupplungName name)        = unpack $ Language.kupplung <^> Language.name <=> name 
instance Query QKupplung where
    getQuery :: (IsString s, Semigroup s) => QKupplung -> s
    getQuery    (QKupplung)                     = Language.name
    getQuery    (QKUnbekannt query _eingabe)    = getQuery query
    getQuery    (QKupplungName _name)           = Language.pin
    getQueryFailed :: (IsString s, Semigroup s) => QKupplung -> s -> s
    getQueryFailed  q@(QKupplungName _name) eingabe = getQueryFailedDefault q eingabe <^> Language.integerErwartet
    getQueryFailed  q                       eingabe = getQueryFailedDefault q eingabe
    getQueryOptions :: (IsString s, Semigroup s) => QKupplung -> Maybe s
    getQueryOptions (QKUnbekannt query _eingabe)    = getQueryOptions query
    getQueryOptions _query                          = Nothing

-- * Hilfs-Befehle
wähleBefehl :: EingabeToken -> [(Token, a)] -> a -> a
wähleBefehl _eingabe                                ([])                    ersatz  = ersatz
wähleBefehl eingabe@(EingabeToken {möglichkeiten})  ((befehl,ergebnis):t)   ersatz
    | elem befehl möglichkeiten                                                     = ergebnis
    | otherwise                                                                     = wähleBefehl eingabe t ersatz

wähleRichtung :: EingabeToken -> Maybe Richtung
wähleRichtung token = wähleBefehl token [
    (Lexer.Gerade   , Just Gerade),
    (Lexer.Kurve    , Just Kurve),
    (Lexer.Links    , Just Links),
    (Lexer.Rechts   , Just Rechts)]
    $ Nothing

-- ** Text-Hilfsfunktionen
--  | Fehlerhafte Eingabe melden
unbekanntShowText :: (Show q, Query q, IsString s, Semigroup s) => q -> s -> s
unbekanntShowText q eingabe = fehlerText $ showQueryFailed q eingabe

-- | Element einer Liste anhand des Index oder Namens finden
findByNameOrIndex :: (StreckenObjekt a) => [a] -> EingabeToken -> Maybe a
findByNameOrIndex   liste   (EingabeToken {eingabe, ganzzahl})  = case ganzzahl of
    (Just index)    | index >= 0, längerAls liste index     -> Just $ liste !! fromIntegral index
    _maybeIndex                                             -> listToMaybe $ filter ((== eingabe) . getName) liste

-- | Prüft, ob eine Liste mindestens von der Länge i ist, ohne die komplette Länge zu berechnen
längerAls :: [a] -> Natural -> Bool
längerAls   ([])    i   = (i < 0)
längerAls   _liste  0   = True
längerAls   (_h:t)  i   = längerAls t $ i - 1