{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Description: Parsen von 'Plan' und 'Aktion'
-}
module Zug.UI.Cmd.Parser.Plan
  ( AnfragePlan(..)
  , AnfrageAktion(..)
  , AnfrageAktionWegstrecke(..)
  , AktionWegstreckeZugtyp(..)
  , AnfrageAktionBahngeschwindigkeit(..)
  , AktionBahngeschwindigkeitZugtyp(..)
  , AnfrageAktionStreckenabschnitt(..)
  , AnfrageAktionWeiche(..)
  , AnfrageAktionKupplung(..)
  ) where

import Data.Foldable (Foldable(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Zug.Anbindung
       (Wegstrecke(..), WegstreckeKlasse(..), Weiche(..), WeicheKlasse(..), Bahngeschwindigkeit(..)
      , BahngeschwindigkeitKlasse(..), Streckenabschnitt(..), StreckenabschnittKlasse(..)
      , Kupplung(..), KupplungKlasse(..), Kontakt(..), KontaktKlasse(..), Wartezeit(..))
import Zug.Enums
       (Zugtyp(..), ZugtypEither(..), GeschwindigkeitVariante(..), GeschwindigkeitEither(..)
      , GeschwindigkeitPhantom(..), Richtung(..), unterstützteRichtungen, Strom(..)
      , Fahrtrichtung(..), unterstützteFahrtrichtungen)
import Zug.Language (Anzeige(..), Sprache(..), (<^>), (<=>), (<->), (<|>), toBefehlsString)
import qualified Zug.Language as Language
import Zug.Objekt (ObjektAllgemein(..), Objekt)
import Zug.Plan
       (Plan(..), Aktion(..), AktionStreckenabschnitt(..), AktionWegstrecke(..)
      , AktionBahngeschwindigkeit(..), AktionWeiche(..), AktionKupplung(..), AktionKontakt(..))
import Zug.UI.Cmd.Lexer (EingabeToken(..), leeresToken)
import qualified Zug.UI.Cmd.Lexer as Lexer
import Zug.UI.Cmd.Parser.Anfrage (Anfrage(..), zeigeAnfrageFehlgeschlagenStandard, MitAnfrage(..)
                                , StatusAnfrageObjekt(..), wähleBefehl, wähleRichtung
                                , AnfrageFortsetzung(..), ($<<), wähleZwischenwert, wähleErgebnis)
import Zug.Warteschlange (Warteschlange)
import qualified Zug.Warteschlange as Warteschlange

-- | Unvollständige 'Aktion' einer 'Bahngeschwindigkeit'.
data AnfrageAktionBahngeschwindigkeit b (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
    AnfrageAktionBahngeschwindigkeit :: b g z -> AnfrageAktionBahngeschwindigkeit b g z
    AABGGeschwindigkeit :: b 'Pwm z -> AnfrageAktionBahngeschwindigkeit b 'Pwm z
    AABGFahrstrom
        :: b 'KonstanteSpannung z -> AnfrageAktionBahngeschwindigkeit b 'KonstanteSpannung z
    AABGFahrtrichtungEinstellen :: b g 'Lego -> AnfrageAktionBahngeschwindigkeit b g 'Lego

deriving instance (Eq (b g z)) => Eq (AnfrageAktionBahngeschwindigkeit b g z)

deriving instance (Show (b g z)) => Show (AnfrageAktionBahngeschwindigkeit b g z)

instance (Anzeige (b g z)) => Anzeige (AnfrageAktionBahngeschwindigkeit b g z) where
    anzeige :: AnfrageAktionBahngeschwindigkeit b g z -> Sprache -> Text
    anzeige (AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit) =
        Language.bahngeschwindigkeit <=> bahngeschwindigkeit
    anzeige (AABGGeschwindigkeit bahngeschwindigkeit) =
        Language.bahngeschwindigkeit <=> bahngeschwindigkeit <^> Language.geschwindigkeit
    anzeige (AABGFahrstrom bahngeschwindigkeit) =
        Language.bahngeschwindigkeit <=> bahngeschwindigkeit <^> Language.fahrstrom
    anzeige (AABGFahrtrichtungEinstellen bahngeschwindigkeit) =
        Language.bahngeschwindigkeit <=> bahngeschwindigkeit <^> Language.fahrtrichtungEinstellen

instance Anfrage (AnfrageAktionBahngeschwindigkeit b g z) where
    zeigeAnfrage :: AnfrageAktionBahngeschwindigkeit b g z -> Sprache -> Text
    zeigeAnfrage (AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit) = Language.aktion
    zeigeAnfrage (AABGGeschwindigkeit _bahngeschwindigkeit) = Language.geschwindigkeit
    zeigeAnfrage (AABGFahrstrom _bahngeschwindigkeit) = Language.fahrstrom
    zeigeAnfrage (AABGFahrtrichtungEinstellen _bahngeschwindigkeit) = Language.fahrtrichtung

    zeigeAnfrageFehlgeschlagen :: AnfrageAktionBahngeschwindigkeit b g z -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@(AABGGeschwindigkeit _bahngeschwindigkeit) eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage@(AABGFahrstrom _bahngeschwindigkeit) eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageAktionBahngeschwindigkeit b g z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen (AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit) =
        Just $ toBefehlsString . Language.aktionBahngeschwindigkeit
    zeigeAnfrageOptionen (AABGGeschwindigkeit _bahngeschwindigkeit) = Nothing
    zeigeAnfrageOptionen (AABGFahrstrom _bahngeschwindigkeit) = Nothing
    zeigeAnfrageOptionen (AABGFahrtrichtungEinstellen _bahngeschwindigkeit) =
        Just $ toBefehlsString . (\sprache -> map (`anzeige` sprache)
                                  $ NonEmpty.toList unterstützteFahrtrichtungen)

-- | Auswahl der 'Bahngeschwindigkeit' für eine 'AktionBahngeschwindigkeit'
-- (beliebiger 'Zugtyp' & 'GeschwindigkeitVariante').
class AktionBahngeschwindigkeitZugtyp (g :: GeschwindigkeitVariante) (z :: Zugtyp) where
    wähleAktionBahngeschwindigkeit
        :: AnfrageAktionBahngeschwindigkeit bg g z
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit bg g z) (AktionBahngeschwindigkeit bg g z)

instance AktionBahngeschwindigkeitZugtyp 'Pwm 'Märklin where
    wähleAktionBahngeschwindigkeit
        :: AnfrageAktionBahngeschwindigkeit bg 'Pwm 'Märklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit bg 'Pwm 'Märklin) (AktionBahngeschwindigkeit bg 'Pwm 'Märklin)
    wähleAktionBahngeschwindigkeit
        (AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
        token@EingabeToken {eingabe} =
        wähleBefehl
            token
            [ (Lexer.Geschwindigkeit, AFZwischenwert $ AABGGeschwindigkeit bahngeschwindigkeit)
            , (Lexer.Umdrehen, AFErgebnis $ Umdrehen bahngeschwindigkeit)]
        $ AFFehler eingabe
    wähleAktionBahngeschwindigkeit _anfrage EingabeToken {eingabe} = AFFehler eingabe

instance AktionBahngeschwindigkeitZugtyp 'KonstanteSpannung 'Märklin where
    wähleAktionBahngeschwindigkeit
        :: AnfrageAktionBahngeschwindigkeit bg 'KonstanteSpannung 'Märklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit bg 'KonstanteSpannung 'Märklin) (AktionBahngeschwindigkeit bg 'KonstanteSpannung 'Märklin)
    wähleAktionBahngeschwindigkeit
        (AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
        token@EingabeToken {eingabe} =
        wähleBefehl
            token
            [ (Lexer.Fahrstrom, AFZwischenwert $ AABGFahrstrom bahngeschwindigkeit)
            , (Lexer.Umdrehen, AFErgebnis $ Umdrehen bahngeschwindigkeit)]
        $ AFFehler eingabe
    wähleAktionBahngeschwindigkeit _anfrage EingabeToken {eingabe} = AFFehler eingabe

instance AktionBahngeschwindigkeitZugtyp 'Pwm 'Lego where
    wähleAktionBahngeschwindigkeit
        :: AnfrageAktionBahngeschwindigkeit bg 'Pwm 'Lego
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit bg 'Pwm 'Lego) (AktionBahngeschwindigkeit bg 'Pwm 'Lego)
    wähleAktionBahngeschwindigkeit (AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit) token =
        wähleZwischenwert
            token
            [ (Lexer.Geschwindigkeit, AABGGeschwindigkeit bahngeschwindigkeit)
            , (Lexer.FahrtrichtungEinstellen, AABGFahrtrichtungEinstellen bahngeschwindigkeit)]
    wähleAktionBahngeschwindigkeit _anfrage EingabeToken {eingabe} = AFFehler eingabe

instance AktionBahngeschwindigkeitZugtyp 'KonstanteSpannung 'Lego where
    wähleAktionBahngeschwindigkeit
        :: AnfrageAktionBahngeschwindigkeit bg 'KonstanteSpannung 'Lego
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit bg 'KonstanteSpannung 'Lego) (AktionBahngeschwindigkeit bg 'KonstanteSpannung 'Lego)
    wähleAktionBahngeschwindigkeit
        (AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit)
        token@EingabeToken {} =
        wähleZwischenwert
            token
            [ (Lexer.Fahrstrom, AABGFahrstrom bahngeschwindigkeit)
            , (Lexer.FahrtrichtungEinstellen, AABGFahrtrichtungEinstellen bahngeschwindigkeit)]
    wähleAktionBahngeschwindigkeit _anfrage EingabeToken {eingabe} = AFFehler eingabe

instance (BahngeschwindigkeitKlasse b, AktionBahngeschwindigkeitZugtyp g z)
    => MitAnfrage (AktionBahngeschwindigkeit b g z) where
    type AnfrageTyp (AktionBahngeschwindigkeit b g z) = AnfrageAktionBahngeschwindigkeit b g z

    -- | Eingabe einer Bahngeschwindigkeit-Aktion
    anfrageAktualisieren
        :: AnfrageAktionBahngeschwindigkeit b g z
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionBahngeschwindigkeit b g z) (AktionBahngeschwindigkeit b g z)
    anfrageAktualisieren anfrage@(AnfrageAktionBahngeschwindigkeit _bahngeschwindigkeit) token =
        wähleAktionBahngeschwindigkeit anfrage token
    anfrageAktualisieren
        (AABGGeschwindigkeit bahngeschwindigkeit)
        EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just wert) -> AFErgebnis
            $ Geschwindigkeit bahngeschwindigkeit
            $ fromIntegral
            $ min (fromIntegral (maxBound :: Word8)) wert
    anfrageAktualisieren (AABGFahrstrom bahngeschwindigkeit) EingabeToken {eingabe, ganzzahl} =
        case ganzzahl of
            Nothing -> AFFehler eingabe
            (Just wert) -> AFErgebnis
                $ Fahrstrom bahngeschwindigkeit
                $ fromIntegral
                $ min (fromIntegral (maxBound :: Word8)) wert
    anfrageAktualisieren (AABGFahrtrichtungEinstellen bahngeschwindigkeit) token =
        wähleErgebnis
            token
            [ (Lexer.Vorwärts, FahrtrichtungEinstellen bahngeschwindigkeit Vorwärts)
            , (Lexer.Rückwärts, FahrtrichtungEinstellen bahngeschwindigkeit Rückwärts)]

-- | Unvollständige 'Aktion' eines 'Streckenabschnitt's.
data AnfrageAktionStreckenabschnitt s
    = AnfrageAktionStreckenabschnitt s
    | AASTStrom s
    deriving (Eq, Show)

instance (Anzeige s) => Anzeige (AnfrageAktionStreckenabschnitt s) where
    anzeige :: AnfrageAktionStreckenabschnitt s -> Sprache -> Text
    anzeige (AnfrageAktionStreckenabschnitt streckenabschnitt) =
        Language.streckenabschnitt <=> streckenabschnitt
    anzeige (AASTStrom streckenabschnitt) =
        Language.streckenabschnitt <=> streckenabschnitt <^> Language.strom

instance Anfrage (AnfrageAktionStreckenabschnitt st) where
    zeigeAnfrage :: AnfrageAktionStreckenabschnitt st -> Sprache -> Text
    zeigeAnfrage (AnfrageAktionStreckenabschnitt _streckenabschnitt) = Language.aktion
    zeigeAnfrage (AASTStrom _streckenabschnitt) = Language.fließend <|> Language.gesperrt

    zeigeAnfrageFehlgeschlagen :: AnfrageAktionStreckenabschnitt st -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@(AASTStrom _streckenabschnitt) eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.stromErwartet
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageAktionStreckenabschnitt st -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen (AnfrageAktionStreckenabschnitt _streckenabschnitt) =
        Just $ toBefehlsString . Language.aktionStreckenabschnitt
    zeigeAnfrageOptionen (AASTStrom _streckenabschnitt) = Just $ \sprache -> toBefehlsString
        $ map ($ sprache) [Language.an, Language.aus]

instance (StreckenabschnittKlasse s) => MitAnfrage (AktionStreckenabschnitt s) where
    type AnfrageTyp (AktionStreckenabschnitt s) = AnfrageAktionStreckenabschnitt s

    -- | Eingabe einer Streckenabschnitt-Aktion
    anfrageAktualisieren
        :: AnfrageAktionStreckenabschnitt s
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionStreckenabschnitt s) (AktionStreckenabschnitt s)
    anfrageAktualisieren (AnfrageAktionStreckenabschnitt streckenabschnitt) token =
        wähleZwischenwert token [(Lexer.Strom, AASTStrom streckenabschnitt)]
    anfrageAktualisieren (AASTStrom streckenabschnitt) token =
        wähleErgebnis
            token
            [ (Lexer.Fließend, Strom streckenabschnitt Fließend)
            , (Lexer.An, Strom streckenabschnitt Fließend)
            , (Lexer.Gesperrt, Strom streckenabschnitt Gesperrt)
            , (Lexer.Aus, Strom streckenabschnitt Gesperrt)]

-- | Unvollständige 'Aktion' einer 'Weiche'.
data AnfrageAktionWeiche w
    = AnfrageAktionWeiche w
    | AAWStellen w
    deriving (Eq, Show)

instance (Anzeige w) => Anzeige (AnfrageAktionWeiche w) where
    anzeige :: AnfrageAktionWeiche w -> Sprache -> Text
    anzeige (AnfrageAktionWeiche weiche) = Language.weiche <=> weiche
    anzeige (AAWStellen weiche) = Language.weiche <=> weiche <^> Language.stellen

instance Anfrage (AnfrageAktionWeiche w) where
    zeigeAnfrage :: AnfrageAktionWeiche w -> Sprache -> Text
    zeigeAnfrage (AnfrageAktionWeiche _weiche) = Language.aktion
    zeigeAnfrage (AAWStellen _weiche) = Language.richtung

    zeigeAnfrageOptionen :: AnfrageAktionWeiche w -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen (AnfrageAktionWeiche _weiche) =
        Just $ toBefehlsString . Language.aktionWeiche
    zeigeAnfrageOptionen (AAWStellen _weiche) =
        Just $ toBefehlsString . (\sprache -> map (`anzeige` sprache)
                                  $ NonEmpty.toList unterstützteRichtungen)

instance (Show w, WeicheKlasse w) => MitAnfrage (AktionWeiche w) where
    type AnfrageTyp (AktionWeiche w) = AnfrageAktionWeiche w

    -- | Eingabe einer Weichen-Aktion
    anfrageAktualisieren :: AnfrageAktionWeiche w
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageAktionWeiche w) (AktionWeiche w)
    anfrageAktualisieren (AnfrageAktionWeiche weiche) token =
        wähleZwischenwert token [(Lexer.Stellen, AAWStellen weiche)]
    anfrageAktualisieren anfrage@(AAWStellen _weiche) token@EingabeToken {eingabe} =
        case wähleRichtung token of
            Nothing -> AFFehler eingabe
            (Just richtung) -> mitRichtung anfrage richtung
        where
            mitRichtung :: (Show w, WeicheKlasse w)
                        => AnfrageAktionWeiche w
                        -> Richtung
                        -> AnfrageFortsetzung (AnfrageAktionWeiche w) (AktionWeiche w)
            mitRichtung (AAWStellen weiche) richtung
                | hatRichtung weiche richtung = AFErgebnis $ Stellen weiche richtung
                | otherwise = AFFehler eingabe
            mitRichtung anfrage _richtung =
                error $ "mitRichtung mit unerwarteter Anfrage aufgerufen: " ++ show anfrage

-- | Unvollständige 'Aktion' einer 'Kupplung'.
newtype AnfrageAktionKupplung k = AnfrageAktionKupplung k   -- ^ Kupplung
    deriving (Show, Eq)

instance (Anzeige k) => Anzeige (AnfrageAktionKupplung k) where
    anzeige :: AnfrageAktionKupplung k -> Sprache -> Text
    anzeige (AnfrageAktionKupplung kupplung) = Language.kupplung <=> kupplung

instance Anfrage (AnfrageAktionKupplung k) where
    zeigeAnfrage :: AnfrageAktionKupplung k -> Sprache -> Text
    zeigeAnfrage (AnfrageAktionKupplung _kupplung) = Language.aktion

    zeigeAnfrageOptionen :: AnfrageAktionKupplung k -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen (AnfrageAktionKupplung _kupplung) =
        Just $ toBefehlsString . Language.aktionKupplung

instance (KupplungKlasse k) => MitAnfrage (AktionKupplung k) where
    type AnfrageTyp (AktionKupplung k) = AnfrageAktionKupplung k

    -- | Eingabe einer Kupplung-Aktion
    anfrageAktualisieren :: AnfrageAktionKupplung k
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageAktionKupplung k) (AktionKupplung k)
    anfrageAktualisieren (AnfrageAktionKupplung kupplung) token =
        wähleErgebnis token [(Lexer.Kuppeln, Kuppeln kupplung)]

-- | Unvollständige 'Aktion' eines 'Kontakt's.
newtype AnfrageAktionKontakt k = AnfrageAktionKontakt k   -- ^ Kontakt
    deriving (Show, Eq)

instance (Anzeige k) => Anzeige (AnfrageAktionKontakt k) where
    anzeige :: AnfrageAktionKontakt k -> Sprache -> Text
    anzeige (AnfrageAktionKontakt kontakt) = Language.kontakt <=> kontakt

instance Anfrage (AnfrageAktionKontakt k) where
    zeigeAnfrage :: AnfrageAktionKontakt k -> Sprache -> Text
    zeigeAnfrage (AnfrageAktionKontakt _kontakt) = Language.aktion

    zeigeAnfrageOptionen :: AnfrageAktionKontakt k -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen (AnfrageAktionKontakt _kontakt) =
        Just $ toBefehlsString . Language.aktionKontakt

instance (KontaktKlasse k) => MitAnfrage (AktionKontakt k) where
    type AnfrageTyp (AktionKontakt k) = AnfrageAktionKontakt k

    -- | Eingabe einer Kupplung-Aktion
    anfrageAktualisieren :: AnfrageAktionKontakt k
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageAktionKontakt k) (AktionKontakt k)
    anfrageAktualisieren (AnfrageAktionKontakt kontakt) token =
        wähleErgebnis token [(Lexer.Warten, WartenAuf kontakt)]

-- | Unvollständige 'Aktion' einer 'Wegstrecke'
data AnfrageAktionWegstrecke w (z :: Zugtyp)
    = AnfrageAktionWegstrecke (w z)
    | AAWSBahngeschwindigkeit (GeschwindigkeitEither (AnfrageAktionBahngeschwindigkeit (GeschwindigkeitPhantom w)) z)
    | AAWSStreckenabschnitt (AnfrageAktionStreckenabschnitt (w z))
    | AAWSKupplung (AnfrageAktionKupplung (w z))
    deriving (Eq, Show)

instance (Anzeige (w z)) => Anzeige (AnfrageAktionWegstrecke w z) where
    anzeige :: AnfrageAktionWegstrecke w z -> Sprache -> Text
    anzeige (AnfrageAktionWegstrecke wegstrecke) = Language.wegstrecke <=> wegstrecke
    anzeige (AAWSBahngeschwindigkeit anfrageAktion) = anzeige anfrageAktion
    anzeige (AAWSStreckenabschnitt anfrageAktion) = anzeige anfrageAktion
    anzeige (AAWSKupplung anfrageAktion) = anzeige anfrageAktion

instance Anfrage (AnfrageAktionWegstrecke w z) where
    zeigeAnfrage :: AnfrageAktionWegstrecke w z -> Sprache -> Text
    zeigeAnfrage (AnfrageAktionWegstrecke _wegstrecke) = Language.aktion
    zeigeAnfrage (AAWSBahngeschwindigkeit anfrageAktion) = zeigeAnfrage anfrageAktion
    zeigeAnfrage (AAWSStreckenabschnitt anfrageAktion) = zeigeAnfrage anfrageAktion
    zeigeAnfrage (AAWSKupplung anfrageAktion) = zeigeAnfrage anfrageAktion

    zeigeAnfrageOptionen :: AnfrageAktionWegstrecke w z -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen (AnfrageAktionWegstrecke _wegstrecke) =
        Just $ toBefehlsString . Language.aktionWegstrecke
    zeigeAnfrageOptionen (AAWSBahngeschwindigkeit anfrageAktion) =
        zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSStreckenabschnitt anfrageAktion) = zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (AAWSKupplung anfrageAktion) = zeigeAnfrageOptionen anfrageAktion

-- | Auswahl der 'Wegstrecke' für eine 'AktionWegstrecke' (beliebiger 'Zugtyp').
class AktionWegstreckeZugtyp (z :: Zugtyp) where
    wähleAktionWegstrecke
        :: AnfrageAktionWegstrecke ws z
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionWegstrecke ws z) (AktionWegstrecke ws z)

instance AktionWegstreckeZugtyp 'Märklin where
    wähleAktionWegstrecke
        :: AnfrageAktionWegstrecke ws 'Märklin
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionWegstrecke ws 'Märklin) (AktionWegstrecke ws 'Märklin)
    wähleAktionWegstrecke (AnfrageAktionWegstrecke wegstrecke) token@EingabeToken {eingabe} =
        wähleBefehl
            token
            [ (Lexer.Einstellen, AFErgebnis $ Einstellen wegstrecke)
            , ( Lexer.Geschwindigkeit
                  , AFZwischenwert
                    $ AAWSBahngeschwindigkeit
                    $ GeschwindigkeitPwm
                    $ AABGGeschwindigkeit
                    $ GeschwindigkeitPhantom wegstrecke
                  )
            , ( Lexer.Fahrstrom
                  , AFZwischenwert
                    $ AAWSBahngeschwindigkeit
                    $ GeschwindigkeitKonstanteSpannung
                    $ AABGFahrstrom
                    $ GeschwindigkeitPhantom wegstrecke
                  )
            , ( Lexer.Umdrehen
                  , AFErgebnis
                    $ AWSBahngeschwindigkeit
                    $ GeschwindigkeitPwm
                    $ Umdrehen
                    $ GeschwindigkeitPhantom wegstrecke
                  )
            , (Lexer.Strom, AFZwischenwert $ AAWSStreckenabschnitt $ AASTStrom wegstrecke)
            , (Lexer.Kuppeln, AFErgebnis $ AWSKupplung $ Kuppeln wegstrecke)]
        $ AFFehler eingabe
    wähleAktionWegstrecke _anfrage EingabeToken {eingabe} = AFFehler eingabe

instance AktionWegstreckeZugtyp 'Lego where
    wähleAktionWegstrecke
        :: AnfrageAktionWegstrecke ws 'Lego
        -> EingabeToken
        -> AnfrageFortsetzung (AnfrageAktionWegstrecke ws 'Lego) (AktionWegstrecke ws 'Lego)
    wähleAktionWegstrecke (AnfrageAktionWegstrecke wegstrecke) token@EingabeToken {eingabe} =
        wähleBefehl
            token
            [ (Lexer.Einstellen, AFErgebnis $ Einstellen wegstrecke)
            , ( Lexer.Geschwindigkeit
                  , AFZwischenwert
                    $ AAWSBahngeschwindigkeit
                    $ GeschwindigkeitPwm
                    $ AABGGeschwindigkeit
                    $ GeschwindigkeitPhantom wegstrecke
                  )
            , ( Lexer.Fahrstrom
                  , AFZwischenwert
                    $ AAWSBahngeschwindigkeit
                    $ GeschwindigkeitKonstanteSpannung
                    $ AABGFahrstrom (GeschwindigkeitPhantom wegstrecke)
                  )
            , ( Lexer.FahrtrichtungEinstellen
                  , AFZwischenwert
                    $ AAWSBahngeschwindigkeit
                    $ GeschwindigkeitPwm
                    $ AABGFahrtrichtungEinstellen
                    $ GeschwindigkeitPhantom wegstrecke
                  )
            , (Lexer.Strom, AFZwischenwert $ AAWSStreckenabschnitt $ AASTStrom wegstrecke)
            , (Lexer.Kuppeln, AFErgebnis $ AWSKupplung $ Kuppeln wegstrecke)]
        $ AFFehler eingabe
    wähleAktionWegstrecke _anfrage EingabeToken {eingabe} = AFFehler eingabe

instance ( AktionWegstreckeZugtyp z
         , WegstreckeKlasse (w z)
         , BahngeschwindigkeitKlasse (GeschwindigkeitPhantom w)
         , AktionBahngeschwindigkeitZugtyp 'Pwm z
         , AktionBahngeschwindigkeitZugtyp 'KonstanteSpannung z
         ) => MitAnfrage (AktionWegstrecke w z) where
    type AnfrageTyp (AktionWegstrecke w z) = AnfrageAktionWegstrecke w z

    -- | Eingabe einer Wegstrecken-Aktion
    anfrageAktualisieren :: AnfrageAktionWegstrecke w z
                         -> EingabeToken
                         -> AnfrageFortsetzung (AnfrageAktionWegstrecke w z) (AktionWegstrecke w z)
    anfrageAktualisieren anfrage@(AnfrageAktionWegstrecke _wegstrecke) token =
        wähleAktionWegstrecke anfrage token
    anfrageAktualisieren (AAWSBahngeschwindigkeit (GeschwindigkeitPwm aAktion0)) token =
        ( AFErgebnis . AWSBahngeschwindigkeit . GeschwindigkeitPwm
        , AAWSBahngeschwindigkeit . GeschwindigkeitPwm
        )
        $<< anfrageAktualisieren aAktion0 token
    anfrageAktualisieren
        (AAWSBahngeschwindigkeit (GeschwindigkeitKonstanteSpannung aAktion0))
        token =
        ( AFErgebnis . AWSBahngeschwindigkeit . GeschwindigkeitKonstanteSpannung
        , AAWSBahngeschwindigkeit . GeschwindigkeitKonstanteSpannung
        )
        $<< anfrageAktualisieren aAktion0 token
    anfrageAktualisieren (AAWSStreckenabschnitt aAktion0) token =
        (AFErgebnis . AWSStreckenabschnitt, AAWSStreckenabschnitt)
        $<< anfrageAktualisieren aAktion0 token
    anfrageAktualisieren (AAWSKupplung aAktion0) token =
        (AFErgebnis . AWSKupplung, AAWSKupplung) $<< anfrageAktualisieren aAktion0 token

-- | Unvollständige 'Aktion'
data AnfrageAktion
    = AnfrageAktion
    | AARückgängig
    | AAWarten
    | AAWegstreckeMärklin (AnfrageAktionWegstrecke Wegstrecke 'Märklin)
    | AAWegstreckeLego (AnfrageAktionWegstrecke Wegstrecke 'Lego)
    | AAWeiche (AnfrageAktionWeiche (ZugtypEither Weiche))
    | AABahngeschwindigkeitMärklinPwm (AnfrageAktionBahngeschwindigkeit Bahngeschwindigkeit 'Pwm 'Märklin)
    | AABahngeschwindigkeitMärklinKonstanteSpannung (AnfrageAktionBahngeschwindigkeit Bahngeschwindigkeit 'KonstanteSpannung 'Märklin)
    | AABahngeschwindigkeitLegoPwm (AnfrageAktionBahngeschwindigkeit Bahngeschwindigkeit 'Pwm 'Lego)
    | AABahngeschwindigkeitLegoKonstanteSpannung (AnfrageAktionBahngeschwindigkeit Bahngeschwindigkeit 'KonstanteSpannung 'Lego)
    | AAStreckenabschnitt (AnfrageAktionStreckenabschnitt Streckenabschnitt)
    | AAKupplung (AnfrageAktionKupplung Kupplung)
    | AAKontakt (AnfrageAktionKontakt Kontakt)
    | AAStatusAnfrage (EingabeToken -> StatusAnfrageObjekt)
                      (Either (Objekt -> AnfrageAktion) (Objekt -> Aktion))

instance Anzeige AnfrageAktion where
    anzeige :: AnfrageAktion -> Sprache -> Text
    anzeige AnfrageAktion = Language.aktion
    anzeige AARückgängig = Language.rückgängig
    anzeige AAWarten = Language.aktion <^> Language.warten
    anzeige (AAWegstreckeMärklin aAktionWegstrecke) = Language.aktion <^> aAktionWegstrecke
    anzeige (AAWegstreckeLego aAktionWegstrecke) = Language.aktion <^> aAktionWegstrecke
    anzeige (AAWeiche aAktionWeiche) = Language.aktion <^> aAktionWeiche
    anzeige (AABahngeschwindigkeitMärklinPwm aAktionBahngeschwindigkeit) =
        Language.aktion <^> aAktionBahngeschwindigkeit
    anzeige (AABahngeschwindigkeitMärklinKonstanteSpannung aAktionBahngeschwindigkeit) =
        Language.aktion <^> aAktionBahngeschwindigkeit
    anzeige (AABahngeschwindigkeitLegoPwm aAktionBahngeschwindigkeit) =
        Language.aktion <^> aAktionBahngeschwindigkeit
    anzeige (AABahngeschwindigkeitLegoKonstanteSpannung aAktionBahngeschwindigkeit) =
        Language.aktion <^> aAktionBahngeschwindigkeit
    anzeige (AAStreckenabschnitt aAktionStreckenabschnitt) =
        Language.aktion <^> aAktionStreckenabschnitt
    anzeige (AAKupplung aAktionKupplung) = Language.aktion <^> aAktionKupplung
    anzeige (AAKontakt aAktionKontakt) = Language.aktion <^> aAktionKontakt
    anzeige (AAStatusAnfrage anfrageKonstruktor _eitherF) =
        Language.aktion <-> Language.objekt <^> anfrageKonstruktor leeresToken

instance Anfrage AnfrageAktion where
    zeigeAnfrage :: AnfrageAktion -> Sprache -> Text
    zeigeAnfrage AnfrageAktion = Language.aktion
    zeigeAnfrage AARückgängig = Language.aktion
    zeigeAnfrage AAWarten = Language.zeit
    zeigeAnfrage (AAWegstreckeMärklin aAktionWegstrecke) = zeigeAnfrage aAktionWegstrecke
    zeigeAnfrage (AAWegstreckeLego aAktionWegstrecke) = zeigeAnfrage aAktionWegstrecke
    zeigeAnfrage (AAWeiche aAktionWeiche) = zeigeAnfrage aAktionWeiche
    zeigeAnfrage (AABahngeschwindigkeitMärklinPwm aAktionBahngeschwindigkeit) =
        zeigeAnfrage aAktionBahngeschwindigkeit
    zeigeAnfrage (AABahngeschwindigkeitMärklinKonstanteSpannung aAktionBahngeschwindigkeit) =
        zeigeAnfrage aAktionBahngeschwindigkeit
    zeigeAnfrage (AABahngeschwindigkeitLegoPwm aAktionBahngeschwindigkeit) =
        zeigeAnfrage aAktionBahngeschwindigkeit
    zeigeAnfrage (AABahngeschwindigkeitLegoKonstanteSpannung aAktionBahngeschwindigkeit) =
        zeigeAnfrage aAktionBahngeschwindigkeit
    zeigeAnfrage (AAStreckenabschnitt aAktionStreckenabschnitt) =
        zeigeAnfrage aAktionStreckenabschnitt
    zeigeAnfrage (AAKupplung aAktionKupplung) = zeigeAnfrage aAktionKupplung
    zeigeAnfrage (AAKontakt aAktionKontakt) = zeigeAnfrage aAktionKontakt
    zeigeAnfrage (AAStatusAnfrage anfrageKonstruktor _eitherF) =
        zeigeAnfrage $ anfrageKonstruktor leeresToken

    zeigeAnfrageFehlgeschlagen :: AnfrageAktion -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@AAWarten eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfrageAktion -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfrageAktion = Just $ toBefehlsString . Language.aktionGruppen
    zeigeAnfrageOptionen AARückgängig = Nothing
    zeigeAnfrageOptionen AAWarten = Nothing
    zeigeAnfrageOptionen (AAWegstreckeMärklin aAktionWegstrecke) =
        zeigeAnfrageOptionen aAktionWegstrecke
    zeigeAnfrageOptionen (AAWegstreckeLego aAktionWegstrecke) =
        zeigeAnfrageOptionen aAktionWegstrecke
    zeigeAnfrageOptionen (AAWeiche aAktionWeiche) = zeigeAnfrageOptionen aAktionWeiche
    zeigeAnfrageOptionen (AABahngeschwindigkeitMärklinPwm aAktionBahngeschwindigkeit) =
        zeigeAnfrageOptionen aAktionBahngeschwindigkeit
    zeigeAnfrageOptionen
        (AABahngeschwindigkeitMärklinKonstanteSpannung aAktionBahngeschwindigkeit) =
        zeigeAnfrageOptionen aAktionBahngeschwindigkeit
    zeigeAnfrageOptionen (AABahngeschwindigkeitLegoPwm aAktionBahngeschwindigkeit) =
        zeigeAnfrageOptionen aAktionBahngeschwindigkeit
    zeigeAnfrageOptionen (AABahngeschwindigkeitLegoKonstanteSpannung aAktionBahngeschwindigkeit) =
        zeigeAnfrageOptionen aAktionBahngeschwindigkeit
    zeigeAnfrageOptionen (AAStreckenabschnitt aAktionStreckenabschnitt) =
        zeigeAnfrageOptionen aAktionStreckenabschnitt
    zeigeAnfrageOptionen (AAKupplung aAktionKupplung) = zeigeAnfrageOptionen aAktionKupplung
    zeigeAnfrageOptionen (AAKontakt aAktionKontakt) = zeigeAnfrageOptionen aAktionKontakt
    zeigeAnfrageOptionen (AAStatusAnfrage anfrageKonstruktor _eitherF) =
        zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken

-- | 'Aktion'-Klassifizierungen
data AnfrageAktionElement
    = AAEUnbekannt Text
    | AAERückgängig
    | AAEWarten
    | AAEAusführen
    | AAEWegstrecke
    | AAEWeiche
    | AAEBahngeschwindigkeit
    | AAEStreckenabschnitt
    | AAEKupplung
    | AAEKontakt

instance MitAnfrage Aktion where
    type AnfrageTyp Aktion = AnfrageAktion

    -- | Eingabe einer 'Aktion'
    anfrageAktualisieren
        :: AnfrageAktion -> EingabeToken -> AnfrageFortsetzung AnfrageAktion Aktion
    anfrageAktualisieren AnfrageAktion token = case anfrageAktionElement token of
        (AAEUnbekannt eingabe) -> AFFehler eingabe
        AAERückgängig -> AFZwischenwert AARückgängig
        AAEWarten -> AFZwischenwert AAWarten
        AAEAusführen -> AFZwischenwert $ AAStatusAnfrage SAOPlan $ Right $ \(OPlan plan)
            -> AktionAusführen plan
        AAEWegstrecke
         -> AFZwischenwert $ AAStatusAnfrage SAOWegstrecke $ Left erhalteWegstreckeAktion
            where
                erhalteWegstreckeAktion :: Objekt -> AnfrageAktion
                erhalteWegstreckeAktion (OWegstrecke (ZugtypMärklin wegstrecke)) =
                    AAWegstreckeMärklin $ AnfrageAktionWegstrecke wegstrecke
                erhalteWegstreckeAktion (OWegstrecke (ZugtypLego wegstrecke)) =
                    AAWegstreckeLego $ AnfrageAktionWegstrecke wegstrecke
                erhalteWegstreckeAktion _objekt = AnfrageAktion
        AAEWeiche -> AFZwischenwert $ AAStatusAnfrage SAOWeiche $ Left $ \(OWeiche weiche)
            -> AAWeiche $ AnfrageAktionWeiche weiche
        AAEBahngeschwindigkeit -> AFZwischenwert
            $ AAStatusAnfrage SAOBahngeschwindigkeit
            $ Left erhalteBahngeschwindigkeitAktion
            where
                erhalteBahngeschwindigkeitAktion :: Objekt -> AnfrageAktion
                erhalteBahngeschwindigkeitAktion
                    (OBahngeschwindigkeit (ZugtypMärklin (GeschwindigkeitPwm bahngeschwindigkeit))) =
                    AABahngeschwindigkeitMärklinPwm
                    $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
                erhalteBahngeschwindigkeitAktion
                    (OBahngeschwindigkeit
                         (ZugtypMärklin (GeschwindigkeitKonstanteSpannung bahngeschwindigkeit))) =
                    AABahngeschwindigkeitMärklinKonstanteSpannung
                    $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
                erhalteBahngeschwindigkeitAktion
                    (OBahngeschwindigkeit (ZugtypLego (GeschwindigkeitPwm bahngeschwindigkeit))) =
                    AABahngeschwindigkeitLegoPwm
                    $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
                erhalteBahngeschwindigkeitAktion
                    (OBahngeschwindigkeit
                         (ZugtypLego (GeschwindigkeitKonstanteSpannung bahngeschwindigkeit))) =
                    AABahngeschwindigkeitLegoKonstanteSpannung
                    $ AnfrageAktionBahngeschwindigkeit bahngeschwindigkeit
                erhalteBahngeschwindigkeitAktion _objekt = AnfrageAktion
        AAEStreckenabschnitt -> AFZwischenwert
            $ AAStatusAnfrage SAOStreckenabschnitt
            $ Left
            $ \(OStreckenabschnitt streckenabschnitt)
            -> AAStreckenabschnitt $ AnfrageAktionStreckenabschnitt streckenabschnitt
        AAEKupplung -> AFZwischenwert $ AAStatusAnfrage SAOKupplung $ Left $ \(OKupplung kupplung)
            -> AAKupplung $ AnfrageAktionKupplung kupplung
        AAEKontakt -> AFZwischenwert $ AAStatusAnfrage SAOKontakt $ Left $ \(OKontakt kontakt)
            -> AAKontakt $ AnfrageAktionKontakt kontakt
        where
            anfrageAktionElement :: EingabeToken -> AnfrageAktionElement
            anfrageAktionElement token@EingabeToken {eingabe} =
                wähleBefehl
                    token
                    [ (Lexer.Rückgängig, AAERückgängig)
                    , (Lexer.Warten, AAEWarten)
                    , (Lexer.Wegstrecke, AAEWegstrecke)
                    , (Lexer.Weiche, AAEWeiche)
                    , (Lexer.Bahngeschwindigkeit, AAEBahngeschwindigkeit)
                    , (Lexer.Streckenabschnitt, AAEStreckenabschnitt)
                    , (Lexer.Kupplung, AAEKupplung)
                    , (Lexer.Kontakt, AAEKontakt)]
                $ AAEUnbekannt eingabe
    anfrageAktualisieren _anfrage EingabeToken {möglichkeiten}
        | Lexer.Rückgängig `elem` möglichkeiten = AFZwischenwert AnfrageAktion
    anfrageAktualisieren AAWarten EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just zeit) -> AFErgebnis $ Warten $ MikroSekunden zeit
    anfrageAktualisieren (AAStatusAnfrage anfrageKonstruktor (Left zwischenwertKonstruktor)) token =
        AFStatusAnfrage (anfrageKonstruktor token) $ AFZwischenwert . zwischenwertKonstruktor
    anfrageAktualisieren (AAStatusAnfrage anfrageKonstruktor (Right konstruktor)) token =
        AFStatusAnfrage (anfrageKonstruktor token) $ AFErgebnis . konstruktor
    anfrageAktualisieren (AAWegstreckeMärklin anfrageAktion) token =
        (AFErgebnis . AWegstreckeMärklin, AAWegstreckeMärklin)
        $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AAWegstreckeLego anfrageAktion) token =
        (AFErgebnis . AWegstreckeLego, AAWegstreckeLego)
        $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AAWeiche anfrageAktion) token =
        (AFErgebnis . AWeiche, AAWeiche) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AABahngeschwindigkeitMärklinPwm anfrageAktion) token =
        (AFErgebnis . ABahngeschwindigkeitMärklinPwm, AABahngeschwindigkeitMärklinPwm)
        $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AABahngeschwindigkeitMärklinKonstanteSpannung anfrageAktion) token =
        ( AFErgebnis . ABahngeschwindigkeitMärklinKonstanteSpannung
        , AABahngeschwindigkeitMärklinKonstanteSpannung
        )
        $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AABahngeschwindigkeitLegoPwm anfrageAktion) token =
        (AFErgebnis . ABahngeschwindigkeitLegoPwm, AABahngeschwindigkeitLegoPwm)
        $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AABahngeschwindigkeitLegoKonstanteSpannung anfrageAktion) token =
        ( AFErgebnis . ABahngeschwindigkeitLegoKonstanteSpannung
        , AABahngeschwindigkeitLegoKonstanteSpannung
        )
        $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AAStreckenabschnitt anfrageAktion) token =
        (AFErgebnis . AStreckenabschnitt, AAStreckenabschnitt)
        $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AAKupplung anfrageAktion) token =
        (AFErgebnis . AKupplung, AAKupplung) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren (AAKontakt anfrageAktion) token =
        (AFErgebnis . AKontakt, AAKontakt) $<< anfrageAktualisieren anfrageAktion token
    anfrageAktualisieren anfrage@AARückgängig _token = AFZwischenwert anfrage

-- | Unvollständiger 'Plan'
data AnfragePlan
    = AnfragePlan
    | APlanName { aplName :: Text }
    | APlanNameAnzahl
          { aplName :: Text
          , aplCount :: Natural
          , aplAkkumulator :: (Warteschlange Aktion)
          , aplNächste :: AnfrageAktion
          }
    | APlanNameAktionen { aplName :: Text, aplAkkumulator :: (Warteschlange Aktion) }
    | APlanStatusAnfrage
          { aplStatusAnfrage :: (EingabeToken -> StatusAnfrageObjekt)
          , aplAuswertung :: (Either (Objekt -> AnfragePlan) (Objekt -> Plan))
          }

instance Show AnfragePlan where
    show :: AnfragePlan -> String
    show = Text.unpack . flip anzeige Deutsch

instance Anzeige AnfragePlan where
    anzeige :: AnfragePlan -> Sprache -> Text
    anzeige AnfragePlan = Language.plan
    anzeige (APlanName name) = Language.plan <^> Language.name <=> name
    anzeige (APlanNameAnzahl name anzahl acc anfrageAktion) =
        Language.plan
        <^> Language.name
        <=> name
        <^> (\sprache -> Language.anzahl sprache (Language.aktionen sprache))
        <=> anzahl <^> acc <^> anfrageAktion
    anzeige (APlanNameAktionen name aktionen) =
        Language.plan <^> Language.name <=> name <^> aktionen
    anzeige (APlanStatusAnfrage anfrageKonstruktor _eitherF) =
        Language.plan <^> Language.aktion <-> Language.objekt <^> anfrageKonstruktor leeresToken

instance Anfrage AnfragePlan where
    zeigeAnfrage :: AnfragePlan -> Sprache -> Text
    zeigeAnfrage AnfragePlan = Language.name
    zeigeAnfrage (APlanName _name) =
        \sprache -> Language.anzahl sprache $ Language.aktionen sprache
    zeigeAnfrage (APlanNameAnzahl _name _anzahl _acc anfrageAktion) = zeigeAnfrage anfrageAktion
    zeigeAnfrage (APlanNameAktionen _name _aktionen) = Language.ausführModus
    zeigeAnfrage (APlanStatusAnfrage anfrageKonstruktor _eitherF) =
        zeigeAnfrage $ anfrageKonstruktor leeresToken

    zeigeAnfrageFehlgeschlagen :: AnfragePlan -> Text -> Sprache -> Text
    zeigeAnfrageFehlgeschlagen anfrage@(APlanName _name) eingabe =
        zeigeAnfrageFehlgeschlagenStandard anfrage eingabe <^> Language.integerErwartet
    zeigeAnfrageFehlgeschlagen anfrage eingabe = zeigeAnfrageFehlgeschlagenStandard anfrage eingabe

    zeigeAnfrageOptionen :: AnfragePlan -> Maybe (Sprache -> Text)
    zeigeAnfrageOptionen AnfragePlan = Nothing
    zeigeAnfrageOptionen (APlanName _name) = Nothing
    zeigeAnfrageOptionen (APlanNameAnzahl _name _anzahl _acc anfrageAktion) =
        zeigeAnfrageOptionen anfrageAktion
    zeigeAnfrageOptionen (APlanNameAktionen _name _aktionen) = Just $ toBefehlsString . \sprache
        -> map ($ sprache) [Language.einfachAusführung, Language.dauerschleife]
    zeigeAnfrageOptionen (APlanStatusAnfrage anfrageKonstruktor _eitherF) =
        zeigeAnfrageOptionen $ anfrageKonstruktor leeresToken

instance MitAnfrage Plan where
    type AnfrageTyp Plan = AnfragePlan

    -- | Eingabe eines Plans
    anfrageAktualisieren :: AnfragePlan -> EingabeToken -> AnfrageFortsetzung AnfragePlan Plan
    anfrageAktualisieren AnfragePlan EingabeToken {eingabe} = AFZwischenwert $ APlanName eingabe
    anfrageAktualisieren (APlanName name) EingabeToken {eingabe, ganzzahl} = case ganzzahl of
        Nothing -> AFFehler eingabe
        (Just 0) -> AFFehler eingabe
        (Just anzahl)
            -> AFZwischenwert $ APlanNameAnzahl name anzahl Warteschlange.leer AnfrageAktion
    anfrageAktualisieren (APlanNameAnzahl name anzahl acc anfrageAktion) token =
        (aktionVerwenden, anfrageAktionVerwenden) $<< anfrageAktualisieren anfrageAktion token
        where
            löscheLetztes :: Warteschlange a -> Warteschlange a
            löscheLetztes warteschlange = case Warteschlange.zeigeLetztes warteschlange of
                Warteschlange.Leer -> warteschlange
                (Warteschlange.Gefüllt _l p) -> p

            anfrageAktionVerwenden :: AnfrageAktion -> AnfragePlan
            anfrageAktionVerwenden AARückgängig =
                APlanNameAnzahl name (succ anzahl) (löscheLetztes acc) AnfrageAktion
            anfrageAktionVerwenden aAktion = APlanNameAnzahl name anzahl acc aAktion

            aktionVerwenden :: Aktion -> AnfrageFortsetzung AnfragePlan Plan
            aktionVerwenden aktion
                | anzahl > 1 =
                    AFZwischenwert
                    $ APlanNameAnzahl
                        name
                        (pred anzahl)
                        (Warteschlange.anhängen aktion acc)
                        AnfrageAktion
                | otherwise =
                    AFZwischenwert $ APlanNameAktionen name $ Warteschlange.anhängen aktion acc
    anfrageAktualisieren
        (APlanStatusAnfrage anfrageKonstruktor (Left zwischenwertKonstruktor))
        token =
        AFStatusAnfrage (anfrageKonstruktor token) $ AFZwischenwert . zwischenwertKonstruktor
    anfrageAktualisieren (APlanStatusAnfrage anfrageKonstruktor (Right konstruktor)) token =
        AFStatusAnfrage (anfrageKonstruktor token) $ AFErgebnis . konstruktor
    anfrageAktualisieren (APlanNameAktionen plName aktionen) token =
        wähleErgebnis
            token
            [ ( Lexer.EinfachAusführen
                  , Plan { plName, plAktionen = NonEmpty.fromList $ toList aktionen }
                  )
            , (Lexer.Dauerschleife, dauerschleife)]
        where
            dauerschleife :: Plan
            dauerschleife =
                Plan
                { plName
                , plAktionen = NonEmpty.fromList
                      $ toList
                      $ Warteschlange.anhängen (AktionAusführen dauerschleife) aktionen
                }