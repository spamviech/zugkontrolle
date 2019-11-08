{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description: Parsen von 'Plan' und 'Aktion'
-}
module Zug.UI.Cmd.Parser.Plan (
    AnfragePlan(..), AnfrageAktion(..),
    AnfrageAktionWegstrecke(..), AktionWegstreckeZugtyp(..),
    AnfrageAktionBahngeschwindigkeit(..), AktionBahngeschwindigkeitZugtyp(..),
    AnfrageAktionStreckenabschnitt(..), AnfrageAktionWeiche(..),
    AnfrageAktionKupplung(..)) where

import Data.Foldable (Foldable(..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup())
import Data.String (IsString())
import Data.Text (Text, unpack)
import Numeric.Natural (Natural)
-- Abhängigkeit von anderen Modulen
import Zug.Anbindung (Wegstrecke(..), WegstreckeKlasse(..), Weiche(..), WeicheKlasse(..),
                    Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..),
                    Streckenabschnitt(..), StreckenabschnittKlasse(..),
                    Kupplung(..), KupplungKlasse(..), Wartezeit(..))
import Zug.Klassen (Zugtyp(..), ZugtypEither(..), Richtung(..), unterstützteRichtungen, Strom(..),
                    Fahrtrichtung(..), unterstützteFahrtrichtungen)
import Zug.Language ((<^>), (<=>), (<->), (<|>), showText, toBefehlsString)
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan (Plan(..), Aktion(..), AktionStreckenabschnitt(..), AktionWegstrecke(..),
                AktionBahngeschwindigkeit(..), AktionWeiche(..), AktionKupplung(..))
import Zug.Warteschlange (Warteschlange, leer, anhängen, zeigeLetztes, Anzeige(..))
import Zug.UI.Cmd.Lexer (EingabeToken(..), leeresToken)
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, MitAnfrage(..),
                                StatusAnfrageObjekt(..), wähleBefehl, wähleRichtung,
                                AnfrageFortsetzung(..), ($<<), wähleZwischenwert, wähleErgebnis)

-- | Unvollständige 'Aktion' einer 'Bahngeschwindigkeit'
data AnfrageAktionBahngeschwindigkeit b (z :: Zugtyp) where
    AnfrageAktionBahngeschwindigkeit
        :: (b z)                                    -- ^ Bahngeschwindigkeit
            -> AnfrageAktionBahngeschwindigkeit b z
    AABGGeschwindigkeit
        :: (b z)                                    -- ^ Bahngeschwindigkeit
            -> AnfrageAktionBahngeschwindigkeit b z
    AABGFahrtrichtungEinstellen
        :: (b 'Lego)                                -- ^ Bahngeschwindigkeit
            -> AnfrageAktionBahngeschwindigkeit b 'Lego

instance (Show (AnfrageTyp (b z)), Show (b z)) => Show (AnfrageAktionBahngeschwindigkeit b z) where
    show :: AnfrageAktionBahngeschwindigkeit b z -> String
    show
        (AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
            = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit
    show
        (AABGGeschwindigkeit bahngeschwindigkeit)
            = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit <^> Language.geschwindigkeit
    show
        (AABGFahrtrichtungEinstellen bahngeschwindigkeit)
            = Language.bahngeschwindigkeit <=> showText bahngeschwindigkeit <^> Language.fahrtrichtungEinstellen
instance Anfrage (AnfrageAktionBahngeschwindigkeit b z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit b z -> s
    zeigeAnfrage
        (AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit)
            = Language.aktion
    zeigeAnfrage
        (AABGGeschwindigkeit _bahngeschwindigkeit)
            = Language.geschwindigkeit
    zeigeAnfrage
        (AABGFahrtrichtungEinstellen _bahngeschwindigkeit)
            = Language.fahrtrichtung
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit b z -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(AABGGeschwindigkeit _bahngeschwindigkeit)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionBahngeschwindigkeit b z -> Maybe s
    zeigeAnfrageOptionen
        (AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit)
            = Just $ toBefehlsString Language.aktionBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AABGGeschwindigkeit _bahngeschwindigkeit)
            = Nothing
    zeigeAnfrageOptionen
        (AABGFahrtrichtungEinstellen _bahngeschwindigkeit)
            = Just $ toBefehlsString $ map showText $ NE.toList unterstützteFahrtrichtungen

class AktionBahngeschwindigkeitZugtyp (z :: Zugtyp) where
    wähleAktionBahngeschwindigkeit ::
        AnfrageAktionBahngeschwindigkeit bg z -> EingabeToken
            -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit bg z) (AktionBahngeschwindigkeit bg z)

instance AktionBahngeschwindigkeitZugtyp 'Märklin where
    wähleAktionBahngeschwindigkeit ::
        AnfrageAktionBahngeschwindigkeit bg 'Märklin -> EingabeToken
            -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit bg 'Märklin) (AktionBahngeschwindigkeit bg 'Märklin)
    wähleAktionBahngeschwindigkeit
        anfrage@(AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Geschwindigkeit  , AFZwischenwert $ AABGGeschwindigkeit bahngeschwindigkeit),
                (Lexer.Umdrehen         , AFErgebnis $ Umdrehen bahngeschwindigkeit)]
                $ AFFehler anfrage eingabe
    wähleAktionBahngeschwindigkeit
        anfrage
        EingabeToken {eingabe}
            = AFFehler anfrage eingabe

instance AktionBahngeschwindigkeitZugtyp 'Lego where
    wähleAktionBahngeschwindigkeit ::
        AnfrageAktionBahngeschwindigkeit bg 'Lego -> EingabeToken
            -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit bg 'Lego) (AktionBahngeschwindigkeit bg 'Lego)
    wähleAktionBahngeschwindigkeit
        anfrage@(AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
        token
            = wähleZwischenwert anfrage token [
                (Lexer.Geschwindigkeit          , AABGGeschwindigkeit bahngeschwindigkeit),
                (Lexer.FahrtrichtungEinstellen  , AABGFahrtrichtungEinstellen bahngeschwindigkeit)]
    wähleAktionBahngeschwindigkeit
        anfrage
        EingabeToken {eingabe}
            = AFFehler anfrage eingabe

instance (BahngeschwindigkeitKlasse b, AktionBahngeschwindigkeitZugtyp z)
        => MitAnfrage (AktionBahngeschwindigkeit b z) where
    type AnfrageTyp (AktionBahngeschwindigkeit b z) = AnfrageAktionBahngeschwindigkeit b z
    -- | Eingabe einer Bahngeschwindigkeit-Aktion
    anfrageAktualisieren :: 
        AnfrageAktionBahngeschwindigkeit b z -> EingabeToken
            -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit b z) (AktionBahngeschwindigkeit b z)
    anfrageAktualisieren
        anfrage@(AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit)
        token
            = wähleAktionBahngeschwindigkeit anfrage token
    anfrageAktualisieren
        anfrage@(AABGGeschwindigkeit bahngeschwindigkeit)
        EingabeToken {eingabe, ganzzahl}
            = case ganzzahl of
                Nothing
                    -> AFFehler anfrage eingabe
                (Just wert)
                    -> AFErgebnis $ Geschwindigkeit bahngeschwindigkeit wert
    anfrageAktualisieren
        anfrage@(AABGFahrtrichtungEinstellen bahngeschwindigkeit)
        token
            = wähleErgebnis anfrage token [
                (Lexer.Vorwärts     , FahrtrichtungEinstellen bahngeschwindigkeit Vorwärts),
                (Lexer.Rückwärts    , FahrtrichtungEinstellen bahngeschwindigkeit Rückwärts)]

-- | Unvollständige 'Aktion' eines 'Streckenabschnitt's
data AnfrageAktionStreckenabschnitt s
    = AnfrageAktionStreckenabschnitt
        s           -- ^ Streckenabschnitt
    | AASTStrom
        s           -- ^ Streckenabschnitt

instance (Show (AnfrageTyp s), Show s) => Show (AnfrageAktionStreckenabschnitt s) where
    show :: AnfrageAktionStreckenabschnitt s -> String
    show
        (AnfrageAktionStreckenabschnitt streckenabschnitt)
            = Language.streckenabschnitt <=> showText streckenabschnitt
    show
        (AASTStrom streckenabschnitt)
            = Language.streckenabschnitt <=> showText streckenabschnitt <^> Language.strom
instance Anfrage (AnfrageAktionStreckenabschnitt st) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionStreckenabschnitt st -> s
    zeigeAnfrage
        (AnfrageAktionStreckenabschnitt _streckenabschnitt)
            = Language.aktion
    zeigeAnfrage
        (AASTStrom _streckenabschnitt)
            = Language.fließend <|> Language.gesperrt
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionStreckenabschnitt st -> Maybe s
    zeigeAnfrageOptionen
        (AnfrageAktionStreckenabschnitt _streckenabschnitt)
            = Just $ toBefehlsString Language.aktionStreckenabschnitt
    zeigeAnfrageOptionen
        (AASTStrom _streckenabschnitt)
            = Just $ toBefehlsString [Language.an, Language.aus]

instance (StreckenabschnittKlasse s) => MitAnfrage (AktionStreckenabschnitt s) where
    type AnfrageTyp (AktionStreckenabschnitt s) = AnfrageAktionStreckenabschnitt s
    -- | Eingabe einer Streckenabschnitt-Aktion
    anfrageAktualisieren :: AnfrageAktionStreckenabschnitt s -> EingabeToken
            -> AnfrageFortsetzung (AnfrageAktionStreckenabschnitt s) (AktionStreckenabschnitt s)
    anfrageAktualisieren
        anfrage@(AnfrageAktionStreckenabschnitt streckenabschnitt)
        token
            = wähleZwischenwert anfrage token [(Lexer.Strom, AASTStrom streckenabschnitt)]
    anfrageAktualisieren
        anfrage@(AASTStrom streckenabschnitt)
        token
            = wähleErgebnis anfrage token [
                (Lexer.Fließend , Strom streckenabschnitt Fließend),
                (Lexer.An       , Strom streckenabschnitt Fließend),
                (Lexer.Gesperrt , Strom streckenabschnitt Gesperrt),
                (Lexer.Aus      , Strom streckenabschnitt Gesperrt)]

-- | Unvollständige 'Aktion' einer 'Weiche'
data AnfrageAktionWeiche w
    = AnfrageAktionWeiche
        w           -- ^ Weiche
    | AAWStellen
        w           -- ^ Weiche

instance (Show w) => Show (AnfrageAktionWeiche w) where
    show :: AnfrageAktionWeiche w -> String
    show
        (AnfrageAktionWeiche weiche)
            = Language.weiche <=> showText weiche
    show
        (AAWStellen weiche)
            = Language.weiche <=> showText weiche <^> Language.stellen
instance Anfrage (AnfrageAktionWeiche w) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionWeiche w -> s
    zeigeAnfrage
        (AnfrageAktionWeiche _weiche)
            = Language.aktion
    zeigeAnfrage
        (AAWStellen _weiche)
            = Language.richtung
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionWeiche w -> Maybe s
    zeigeAnfrageOptionen
        (AnfrageAktionWeiche _weiche)
            = Just $ toBefehlsString Language.aktionWeiche
    zeigeAnfrageOptionen
        (AAWStellen _weiche)
            = Just $ toBefehlsString $ NE.toList $ fmap showText unterstützteRichtungen

instance (Show w, WeicheKlasse w) => MitAnfrage (AktionWeiche w) where
    type AnfrageTyp (AktionWeiche w) = AnfrageAktionWeiche w
    -- | Eingabe einer Weichen-Aktion
    anfrageAktualisieren ::
        AnfrageAktionWeiche w -> EingabeToken -> AnfrageFortsetzung (AnfrageAktionWeiche w) (AktionWeiche w)
    anfrageAktualisieren
        anfrage@(AnfrageAktionWeiche weiche)
        token
            = wähleZwischenwert anfrage token [(Lexer.Stellen, AAWStellen weiche)]
    anfrageAktualisieren
        anfrage@(AAWStellen _weiche)
        token@EingabeToken {eingabe}
            = case wähleRichtung token of
                Nothing
                    -> AFFehler anfrage eingabe
                (Just richtung)
                    -> mitRichtung anfrage richtung
        where
            mitRichtung :: (Show w, WeicheKlasse w)
                => AnfrageAktionWeiche w -> Richtung -> AnfrageFortsetzung (AnfrageAktionWeiche w) (AktionWeiche w)
            mitRichtung
                anfrage@(AAWStellen weiche)
                richtung
                    | hatRichtung weiche richtung
                        = AFErgebnis $ Stellen weiche richtung
                    | otherwise
                        = AFFehler anfrage eingabe
            mitRichtung
                anfrage
                _richtung
                    = error $ "mitRichtung mit unerwarteter Anfrage aufgerufen: " ++ show anfrage

-- | Unvollständige 'Aktion' einer 'Kupplung'
data AnfrageAktionKupplung k
    = AnfrageAktionKupplung
        k   -- ^ Kupplung

instance (Show (AnfrageTyp k), Show k) => Show (AnfrageAktionKupplung k) where
    show :: AnfrageAktionKupplung k -> String
    show
        (AnfrageAktionKupplung kupplung)
            = Language.kupplung <=> showText kupplung
instance Anfrage (AnfrageAktionKupplung k) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionKupplung k -> s
    zeigeAnfrage
        (AnfrageAktionKupplung _kupplung)
            = Language.aktion
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionKupplung k -> Maybe s
    zeigeAnfrageOptionen
        (AnfrageAktionKupplung _kupplung)
            = Just $ toBefehlsString Language.aktionKupplung

instance (KupplungKlasse k) => MitAnfrage (AktionKupplung k) where
    type AnfrageTyp (AktionKupplung k) = AnfrageAktionKupplung k
    -- | Eingabe einer Kupplung-Aktion
    anfrageAktualisieren ::
        AnfrageAktionKupplung k -> EingabeToken -> AnfrageFortsetzung (AnfrageAktionKupplung k) (AktionKupplung k)
    anfrageAktualisieren
        anfrage@(AnfrageAktionKupplung kupplung)
        token
            = wähleErgebnis anfrage token [(Lexer.Kuppeln, Kuppeln kupplung)]

-- | Unvollständige 'Aktion' einer 'Wegstrecke'
data AnfrageAktionWegstrecke w (z :: Zugtyp)
    = AnfrageAktionWegstrecke
        (w z)                               -- ^ Wegstrecke
    | AAWSBahngeschwindigkeit
        (AnfrageAktionBahngeschwindigkeit w z)
    | AAWSStreckenabschnitt
        (AnfrageAktionStreckenabschnitt (w z))
    | AAWSKupplung
        (AnfrageAktionKupplung (w z))

instance (Show (AnfrageTyp (w z)), Show (w z)) => Show (AnfrageAktionWegstrecke w z) where
    show :: AnfrageAktionWegstrecke w z -> String
    show    (AnfrageAktionWegstrecke wegstrecke)      = Language.wegstrecke <=> showText wegstrecke
    show    (AAWSBahngeschwindigkeit anfrageAktion)   = showText anfrageAktion
    show    (AAWSStreckenabschnitt anfrageAktion)     = showText anfrageAktion
    show    (AAWSKupplung anfrageAktion)              = showText anfrageAktion
instance Anfrage (AnfrageAktionWegstrecke w z) where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktionWegstrecke w z -> s
    zeigeAnfrage    (AnfrageAktionWegstrecke _wegstrecke)     = Language.aktion
    zeigeAnfrage    (AAWSBahngeschwindigkeit anfrageAktion)   = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWSStreckenabschnitt anfrageAktion)     = zeigeAnfrage anfrageAktion
    zeigeAnfrage    (AAWSKupplung anfrageAktion)              = zeigeAnfrage anfrageAktion
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktionWegstrecke w z -> Maybe s
    zeigeAnfrageOptionen (AnfrageAktionWegstrecke _wegstrecke)     = Just $ toBefehlsString Language.aktionWegstrecke
    zeigeAnfrageOptionen (AAWSBahngeschwindigkeit anfrageAktion)   = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSStreckenabschnitt anfrageAktion)     = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSKupplung anfrageAktion)              = zeigeAnfrageOptionen anfrageAktion

class AktionWegstreckeZugtyp (z :: Zugtyp) where
    wähleAktionWegstrecke ::
        AnfrageAktionWegstrecke ws z -> EingabeToken
            -> AnfrageFortsetzung (AnfrageAktionWegstrecke ws z) (AktionWegstrecke ws z)

instance AktionWegstreckeZugtyp 'Märklin where
    wähleAktionWegstrecke ::
        AnfrageAktionWegstrecke ws 'Märklin -> EingabeToken
            -> AnfrageFortsetzung (AnfrageAktionWegstrecke ws 'Märklin) (AktionWegstrecke ws 'Märklin)
    wähleAktionWegstrecke
        anfrage@(AnfrageAktionWegstrecke wegstrecke)
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Einstellen       , AFErgebnis $ Einstellen wegstrecke),
                (Lexer.Geschwindigkeit  , AFZwischenwert $ AAWSBahngeschwindigkeit $ AABGGeschwindigkeit wegstrecke),
                (Lexer.Umdrehen         , AFErgebnis $ AWSBahngeschwindigkeit $ Umdrehen wegstrecke),
                (Lexer.Strom            , AFZwischenwert $ AAWSStreckenabschnitt $ AASTStrom wegstrecke),
                (Lexer.Kuppeln          , AFErgebnis $ AWSKupplung $ Kuppeln wegstrecke)]
                $ AFFehler anfrage eingabe
    wähleAktionWegstrecke
        anfrage
        EingabeToken {eingabe}
            = AFFehler anfrage eingabe

instance AktionWegstreckeZugtyp 'Lego where
    wähleAktionWegstrecke ::
        AnfrageAktionWegstrecke ws 'Lego ->
        EingabeToken ->
            AnfrageFortsetzung (AnfrageAktionWegstrecke ws 'Lego) (AktionWegstrecke ws 'Lego)
    wähleAktionWegstrecke
        anfrage@(AnfrageAktionWegstrecke wegstrecke)
        token@EingabeToken {eingabe}
            = wähleBefehl token [
                (Lexer.Einstellen               , AFErgebnis $ Einstellen wegstrecke),
                (Lexer.Geschwindigkeit          ,
                    AFZwischenwert $ AAWSBahngeschwindigkeit $ AABGGeschwindigkeit wegstrecke),
                (Lexer.FahrtrichtungEinstellen  ,
                    AFZwischenwert $ AAWSBahngeschwindigkeit $ AABGFahrtrichtungEinstellen wegstrecke),
                (Lexer.Strom                    , AFZwischenwert $ AAWSStreckenabschnitt $ AASTStrom wegstrecke),
                (Lexer.Kuppeln                  , AFErgebnis $ AWSKupplung $ Kuppeln wegstrecke)]
                $ AFFehler anfrage eingabe
    wähleAktionWegstrecke
        anfrage
        EingabeToken {eingabe}
            = AFFehler anfrage eingabe

instance (AktionWegstreckeZugtyp z, WegstreckeKlasse (w z),
    BahngeschwindigkeitKlasse w, AktionBahngeschwindigkeitZugtyp z)
        => MitAnfrage (AktionWegstrecke w z) where
    type AnfrageTyp (AktionWegstrecke w z) = AnfrageAktionWegstrecke w z
    -- | Eingabe einer Wegstrecken-Aktion
    anfrageAktualisieren ::
        AnfrageAktionWegstrecke w z ->
        EingabeToken ->
            AnfrageFortsetzung (AnfrageAktionWegstrecke w z) (AktionWegstrecke w z)
    anfrageAktualisieren
        anfrage@(AnfrageAktionWegstrecke _wegstrecke)
        token
            = wähleAktionWegstrecke anfrage token
    anfrageAktualisieren
        (AAWSBahngeschwindigkeit aAktion0)
        token
            = (AFErgebnis . AWSBahngeschwindigkeit, AAWSBahngeschwindigkeit) $<< anfrageAktualisieren aAktion0 token
    anfrageAktualisieren
        (AAWSStreckenabschnitt aAktion0)
        token
            = (AFErgebnis . AWSStreckenabschnitt, AAWSStreckenabschnitt) $<< anfrageAktualisieren aAktion0 token
    anfrageAktualisieren
        (AAWSKupplung aAktion0)
        token
            = (AFErgebnis . AWSKupplung, AAWSKupplung) $<< anfrageAktualisieren aAktion0 token

-- | Unvollständige 'Aktion'
data AnfrageAktion
    = AnfrageAktion
    | AARückgängig
    | AAWarten
    | AAWegstreckeMärklin
        (AnfrageAktionWegstrecke Wegstrecke 'Märklin)
    | AAWegstreckeLego
        (AnfrageAktionWegstrecke Wegstrecke 'Lego)
    | AAWeiche
        (AnfrageAktionWeiche (ZugtypEither Weiche))
    | AABahngeschwindigkeitMärklin
        (AnfrageAktionBahngeschwindigkeit Bahngeschwindigkeit 'Märklin)
    | AABahngeschwindigkeitLego
        (AnfrageAktionBahngeschwindigkeit Bahngeschwindigkeit 'Lego)
    | AAStreckenabschnitt
        (AnfrageAktionStreckenabschnitt Streckenabschnitt)
    | AAKupplung
        (AnfrageAktionKupplung Kupplung)
    | AAKlassifizierung
        (EingabeToken -> StatusAnfrageObjekt)
        (Either (Objekt -> AnfrageAktion) (Objekt -> Aktion))

instance (Show (AnfrageTyp (Wegstrecke 'Märklin)), Show (AnfrageTyp (Wegstrecke 'Lego)),
    Show (AnfrageTyp (Bahngeschwindigkeit 'Märklin)), Show (AnfrageTyp (Bahngeschwindigkeit 'Lego)),
    Show (AnfrageTyp Streckenabschnitt), Show (AnfrageTyp Kupplung))
        => Show AnfrageAktion where
    show :: AnfrageAktion -> String
    show
        AnfrageAktion
            = Language.aktion
    show
        AARückgängig
            = Language.rückgängig
    show
        AAWarten
            = Language.aktion <^> Language.warten
    show
        (AAWegstreckeMärklin aAktionWegstrecke)
            = Language.aktion <^> showText aAktionWegstrecke
    show
        (AAWegstreckeLego aAktionWegstrecke)
            = Language.aktion <^> showText aAktionWegstrecke
    show
        (AAWeiche aAktionWeiche)
            = Language.aktion <^> showText aAktionWeiche
    show
        (AABahngeschwindigkeitMärklin aAktionBahngeschwindigkeit)
            = Language.aktion <^> showText aAktionBahngeschwindigkeit
    show
        (AABahngeschwindigkeitLego aAktionBahngeschwindigkeit)
            = Language.aktion <^> showText aAktionBahngeschwindigkeit
    show
        (AAStreckenabschnitt aAktionStreckenabschnitt)
            = Language.aktion <^> showText aAktionStreckenabschnitt
    show
        (AAKupplung aAktionKupplung)
            = Language.aktion <^> showText aAktionKupplung
    show
        (AAKlassifizierung anfrageKonstruktor _eitherF)
            = Language.aktion <-> Language.objekt <^> showText (anfrageKonstruktor leeresToken)

instance Anfrage AnfrageAktion where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfrageAktion -> s
    zeigeAnfrage
        AnfrageAktion
            = Language.aktion
    zeigeAnfrage
        AARückgängig
            = Language.aktion
    zeigeAnfrage
        AAWarten
            = Language.zeit
    zeigeAnfrage
        (AAWegstreckeMärklin aAktionWegstrecke)
            = zeigeAnfrage aAktionWegstrecke
    zeigeAnfrage
        (AAWegstreckeLego aAktionWegstrecke)
            = zeigeAnfrage aAktionWegstrecke
    zeigeAnfrage
        (AAWeiche aAktionWeiche)
            = zeigeAnfrage aAktionWeiche
    zeigeAnfrage
        (AABahngeschwindigkeitMärklin aAktionBahngeschwindigkeit)
            = zeigeAnfrage aAktionBahngeschwindigkeit
    zeigeAnfrage
        (AABahngeschwindigkeitLego aAktionBahngeschwindigkeit)
            = zeigeAnfrage aAktionBahngeschwindigkeit
    zeigeAnfrage
        (AAStreckenabschnitt aAktionStreckenabschnitt)
            = zeigeAnfrage aAktionStreckenabschnitt
    zeigeAnfrage
        (AAKupplung aAktionKupplung)
            = zeigeAnfrage aAktionKupplung
    zeigeAnfrage
        (AAKlassifizierung anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfrageAktion -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@AAWarten
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfrageAktion -> Maybe s
    zeigeAnfrageOptionen
        AnfrageAktion
            = Just $ toBefehlsString Language.aktionGruppen
    zeigeAnfrageOptionen
        AARückgängig
            = Nothing
    zeigeAnfrageOptionen
        AAWarten
            = Nothing
    zeigeAnfrageOptionen
        (AAWegstreckeMärklin aAktionWegstrecke)
            = zeigeAnfrageOptionen aAktionWegstrecke
    zeigeAnfrageOptionen
        (AAWegstreckeLego aAktionWegstrecke)
            = zeigeAnfrageOptionen aAktionWegstrecke
    zeigeAnfrageOptionen
        (AAWeiche aAktionWeiche)
            = zeigeAnfrageOptionen aAktionWeiche
    zeigeAnfrageOptionen
        (AABahngeschwindigkeitMärklin aAktionBahngeschwindigkeit)
            = zeigeAnfrageOptionen aAktionBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AABahngeschwindigkeitLego aAktionBahngeschwindigkeit)
            = zeigeAnfrageOptionen aAktionBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AAStreckenabschnitt aAktionStreckenabschnitt)
            = zeigeAnfrageOptionen aAktionStreckenabschnitt
    zeigeAnfrageOptionen
        (AAKupplung aAktionKupplung)
            = zeigeAnfrageOptionen aAktionKupplung
    zeigeAnfrageOptionen
        (AAKlassifizierung anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken

-- | 'Aktion'-Klassifizierungen
data AnfrageAktionElement
    = AAEUnbekannt
        Text
    | AAERückgängig
    | AAEWarten
    | AAEAusführen
    | AAEWegstrecke
    | AAEWeiche
    | AAEBahngeschwindigkeit
    | AAEStreckenabschnitt
    | AAEKupplung

instance MitAnfrage Aktion where
    type AnfrageTyp Aktion = AnfrageAktion
    -- | Eingabe einer 'Aktion'
    anfrageAktualisieren :: AnfrageAktion -> EingabeToken -> AnfrageFortsetzung AnfrageAktion Aktion
    anfrageAktualisieren
        AnfrageAktion
        token
            = case anfrageAktionElement token of
                (AAEUnbekannt eingabe)
                    -> AFFehler AnfrageAktion eingabe
                AAERückgängig
                    -> AFZwischenwert AARückgängig
                AAEWarten
                    -> AFZwischenwert AAWarten
                AAEAusführen
                    -> AFZwischenwert $ AAKlassifizierung SAOPlan $ Right $ \(OPlan plan) -> AktionAusführen plan
                AAEWegstrecke
                    -> AFZwischenwert $ AAKlassifizierung SAOWegstrecke $ Left erhalteWegstreckeAktion
                    where
                        erhalteWegstreckeAktion :: Objekt -> AnfrageAktion
                        erhalteWegstreckeAktion
                            (OWegstrecke (ZugtypMärklin wegstrecke))
                                = AAWegstreckeMärklin $ AnfrageAktionWegstrecke wegstrecke
                        erhalteWegstreckeAktion
                            (OWegstrecke (ZugtypLego wegstrecke))
                                = AAWegstreckeLego $ AnfrageAktionWegstrecke wegstrecke
                        erhalteWegstreckeAktion
                            _objekt
                                = AnfrageAktion
                AAEWeiche
                    -> AFZwischenwert $ AAKlassifizierung SAOWeiche $ Left $
                        \(OWeiche weiche) -> AAWeiche $ AnfrageAktionWeiche weiche
                AAEBahngeschwindigkeit
                    -> AFZwischenwert $ AAKlassifizierung SAOBahngeschwindigkeit $
                        Left erhalteBahngeschwindigkeitAktion
                    where
                        erhalteBahngeschwindigkeitAktion :: Objekt -> AnfrageAktion
                        erhalteBahngeschwindigkeitAktion
                            (OBahngeschwindigkeit (ZugtypMärklin bahngeschwindigkeit))
                                = AABahngeschwindigkeitMärklin $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
                        erhalteBahngeschwindigkeitAktion
                            (OBahngeschwindigkeit (ZugtypLego bahngeschwindigkeit))
                                = AABahngeschwindigkeitLego $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
                        erhalteBahngeschwindigkeitAktion
                            _objekt
                                = AnfrageAktion
                AAEStreckenabschnitt
                    -> AFZwischenwert $ AAKlassifizierung SAOStreckenabschnitt $ Left $
                        \(OStreckenabschnitt streckenabschnitt)
                            -> AAStreckenabschnitt $ AnfrageAktionStreckenabschnitt streckenabschnitt
                AAEKupplung
                    -> AFZwischenwert $ AAKlassifizierung SAOKupplung $ Left $
                        \(OKupplung kupplung) -> AAKupplung $ AnfrageAktionKupplung kupplung
        where
            anfrageAktionElement :: EingabeToken -> AnfrageAktionElement
            anfrageAktionElement token@EingabeToken {eingabe} = wähleBefehl token [
                (Lexer.Rückgängig           , AAERückgängig),
                (Lexer.Warten               , AAEWarten),
                (Lexer.Wegstrecke           , AAEWegstrecke),
                (Lexer.Weiche               , AAEWeiche),
                (Lexer.Bahngeschwindigkeit  , AAEBahngeschwindigkeit),
                (Lexer.Streckenabschnitt    , AAEStreckenabschnitt),
                (Lexer.Kupplung             , AAEKupplung)]
                $ AAEUnbekannt eingabe
    anfrageAktualisieren
        _anfrage
        EingabeToken {möglichkeiten}
            | elem Lexer.Rückgängig möglichkeiten
                = AFZwischenwert AnfrageAktion
    anfrageAktualisieren
        AAWarten
        EingabeToken {eingabe, ganzzahl}
            = case ganzzahl of
                Nothing
                    -> AFFehler AAWarten eingabe
                (Just zeit)
                    -> AFErgebnis $ Warten $ MikroSekunden zeit
    anfrageAktualisieren
        (AAKlassifizierung anfrageKonstruktor (Left zwischenwertKonstruktor))
        token
            = AFStatusAnfrage (anfrageKonstruktor token) $ AFZwischenwert . zwischenwertKonstruktor
    anfrageAktualisieren
        (AAKlassifizierung anfrageKonstruktor (Right konstruktor))
        token
            = AFStatusAnfrage (anfrageKonstruktor token) $ AFErgebnis . konstruktor
    anfrageAktualisieren
        (AAWegstreckeMärklin anfrageAktion)
        token
            = (AFErgebnis . AWegstreckeMärklin, AAWegstreckeMärklin) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren
        (AAWegstreckeLego anfrageAktion)
        token
            = (AFErgebnis . AWegstreckeLego, AAWegstreckeLego) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren
        (AAWeiche anfrageAktion)
        token
            = (AFErgebnis . AWeiche, AAWeiche) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren
        (AABahngeschwindigkeitMärklin anfrageAktion)
        token
            = (AFErgebnis . ABahngeschwindigkeitMärklin, AABahngeschwindigkeitMärklin) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren
        (AABahngeschwindigkeitLego anfrageAktion)
        token
            = (AFErgebnis . ABahngeschwindigkeitLego, AABahngeschwindigkeitLego) $<<
                anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren
        (AAStreckenabschnitt anfrageAktion)
        token
            = (AFErgebnis . AStreckenabschnitt, AAStreckenabschnitt) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren
        (AAKupplung anfrageAktion)
        token
            = (AFErgebnis . AKupplung, AAKupplung) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren
        anfrage@AARückgängig
        _token
            = AFZwischenwert anfrage

-- | Unvollständiger 'Plan'
data AnfragePlan
    = AnfragePlan
    | APlanName
        Text                    -- ^ Name
    | APlanNameAnzahl
        Text                    -- ^ Name
        Natural                 -- ^ Verbleibende Aktionen
        (Warteschlange Aktion)  -- ^ Bekannte Aktionen
        AnfrageAktion           -- ^ Nächste Aktion
    | APlanNameAktionen
        Text                    -- ^ Name
        (Warteschlange Aktion)  -- ^ Aktionen
    | APlanKlassifizierung
        (EingabeToken -> StatusAnfrageObjekt)
        (Either (Objekt -> AnfragePlan) (Objekt -> Plan))

instance (Show (AnfrageTyp (Wegstrecke 'Märklin)), Show (AnfrageTyp (Wegstrecke 'Lego)),
    Show (AnfrageTyp (Bahngeschwindigkeit 'Märklin)), Show (AnfrageTyp (Bahngeschwindigkeit 'Lego)),
    Show (AnfrageTyp Streckenabschnitt), Show (AnfrageTyp Kupplung))
        => Show AnfragePlan where
    show :: AnfragePlan -> String
    show
        AnfragePlan
            = Language.plan
    show
        (APlanName name)
            = unpack $ Language.plan <^> Language.name <=> name
    show
        (APlanNameAnzahl name anzahl acc anfrageAktion)
            = unpack $ Language.plan
                <^> Language.name <=> name
                <^> Language.anzahl Language.aktionen <=> showText anzahl
                <^> showText acc
                <^> showText anfrageAktion
    show
        (APlanNameAktionen name aktionen)
            = unpack $ Language.plan
                <^> Language.name <=> name
                <^> showText aktionen
    show
        (APlanKlassifizierung anfrageKonstruktor _eitherF)
            = Language.plan
                <^> Language.aktion <-> Language.objekt
                <^> showText (anfrageKonstruktor leeresToken)
instance Anfrage AnfragePlan where
    zeigeAnfrage :: (IsString s, Semigroup s) => AnfragePlan -> s
    zeigeAnfrage
        AnfragePlan
            = Language.name
    zeigeAnfrage
        (APlanName _name)
            = Language.anzahl Language.aktionen
    zeigeAnfrage
        (APlanNameAnzahl _name _anzahl _acc anfrageAktion)
            = zeigeAnfrage anfrageAktion
    zeigeAnfrage
        (APlanNameAktionen _name _aktionen)
            = Language.ausführModus
    zeigeAnfrage
        (APlanKlassifizierung anfrageKonstruktor _eitherF)
            = zeigeAnfrage $ anfrageKonstruktor leeresToken
    zeigeAnfrageFehlgeschlagen :: (IsString s, Semigroup s) => AnfragePlan -> s -> s
    zeigeAnfrageFehlgeschlagen
        anfrage@(APlanName _name)
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen
        anfrage
        eingabe
            = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe
    zeigeAnfrageOptionen :: (IsString s, Semigroup s) => AnfragePlan -> Maybe s
    zeigeAnfrageOptionen
        AnfragePlan
            = Nothing
    zeigeAnfrageOptionen
        (APlanName _name)
            = Nothing
    zeigeAnfrageOptionen
        (APlanNameAnzahl _name _anzahl _acc anfrageAktion)
            = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen
        (APlanNameAktionen _name _aktionen)
            = Just $ toBefehlsString [Language.einfachAusführung, Language.dauerschleife]
    zeigeAnfrageOptionen
        (APlanKlassifizierung anfrageKonstruktor _eitherF)
            = zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken

instance MitAnfrage Plan where
    type AnfrageTyp Plan = AnfragePlan
    -- | Eingabe eines Plans
    anfrageAktualisieren :: AnfragePlan -> EingabeToken -> AnfrageFortsetzung AnfragePlan Plan
    anfrageAktualisieren
        AnfragePlan
        EingabeToken {eingabe}
            = AFZwischenwert $ APlanName eingabe
    anfrageAktualisieren
        anfrage@(APlanName name)
        EingabeToken {eingabe, ganzzahl}
            = case ganzzahl of
                Nothing         -> AFFehler anfrage eingabe
                (Just anzahl)   -> AFZwischenwert $ APlanNameAnzahl name anzahl leer AnfrageAktion
    anfrageAktualisieren
        (APlanNameAnzahl name anzahl acc anfrageAktion)
        token
            = (aktionVerwenden, anfrageAktionVerwenden) $<< anfrageAktualisieren anfrageAktion token
            where
                löscheLetztes :: Warteschlange a -> Warteschlange a
                löscheLetztes warteschlange = case zeigeLetztes warteschlange of
                    Leer
                        -> warteschlange
                    (Gefüllt _l p)
                        -> p
                anfrageAktionVerwenden :: AnfrageAktion -> AnfragePlan
                anfrageAktionVerwenden
                    AARückgängig
                        = APlanNameAnzahl name (succ anzahl) (löscheLetztes acc) AnfrageAktion
                anfrageAktionVerwenden
                    aAktion
                        = APlanNameAnzahl name anzahl acc aAktion
                aktionVerwenden :: Aktion -> AnfrageFortsetzung AnfragePlan Plan
                aktionVerwenden aktion
                    | anzahl > 1
                        = AFZwischenwert $ APlanNameAnzahl name (pred anzahl) (anhängen aktion acc) AnfrageAktion
                    | otherwise
                        = AFZwischenwert $ APlanNameAktionen name $ anhängen aktion acc
    anfrageAktualisieren
        (APlanKlassifizierung anfrageKonstruktor (Left zwischenwertKonstruktor))
        token
            = AFStatusAnfrage (anfrageKonstruktor token) $ AFZwischenwert . zwischenwertKonstruktor
    anfrageAktualisieren
        (APlanKlassifizierung anfrageKonstruktor (Right konstruktor))
        token
            = AFStatusAnfrage (anfrageKonstruktor token) $ AFErgebnis . konstruktor
    anfrageAktualisieren
        anfrage@(APlanNameAktionen plName aktionen)
        token
            = wähleErgebnis anfrage token [
                (Lexer.EinfachAusführen , Plan {plName, plAktionen = toList $ aktionen}),
                (Lexer.Dauerschleife    , dauerschleife)]
            where
                dauerschleife :: Plan
                dauerschleife = Plan {plName, plAktionen = toList $ anhängen (AktionAusführen dauerschleife) aktionen}