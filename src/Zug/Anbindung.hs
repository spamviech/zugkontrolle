{-|
Description : Low-Level-Definition der unterstützen Aktionen auf Anschluss-Ebene.
-}
module Zug.Anbindung
  ( -- * Anschluss-Repräsentation
    Anschluss(..)
  , AnschlussEither(..)
  , MitInterruptPin(..)
  , InterruptPinBenötigt(..)
  , PCF8574Klasse(..)
  , PCF8574Port(..)
  , PCF8574(..)
  , PCF8574Variant(..)
  , AnschlussKlasse(..)
  , pcf8574Gruppieren
  , pcf8574MultiPortWrite
  , beiÄnderung
  , pwmSetzeWert
  , pwmServo
  , erhaltePwmWertVoll
  , erhaltePwmWertReduziert
  , pwmEingabeMaximal
  , Pin(..)
  , Value(..)
  , alleValues
  , PwmValueUnmodifiziert
  , IntEdge(..)
    -- ** Notwendige Hintergrund-Informationen
  , PwmMap
  , pwmMapEmpty
  , MitPwmMap(..)
  , PwmReader(..)
  , I2CMap
  , i2cMapEmpty
  , MitI2CMap(..)
  , I2CReader(..)
  , InterruptMap
  , interruptMapEmpty
  , MitInterruptMap(..)
  , InterruptReader(..)
    -- ** Hilfsfunktionen
  , pwmMöglich
  , clockMöglich
    -- * Strecken-Objekte
  , StreckenObjekt(..)
  , StreckenAtom(..)
    -- ** Bahngeschwindigkeiten
  , Bahngeschwindigkeit(..)
  , BahngeschwindigkeitKlasse(..)
  , BahngeschwindigkeitContainer(..)
  , verwendetPwm
    -- ** Streckenabschnitte
  , Streckenabschnitt(..)
  , StreckenabschnittKlasse(..)
  , StreckenabschnittContainer(..)
    -- ** Weichen
  , Weiche(..)
  , WeicheKlasse(..)
  , WeicheContainer(..)
    -- ** Kupplungen
  , Kupplung(..)
  , KupplungKlasse(..)
  , KupplungContainer(..)
    -- ** Kontakt
  , Kontakt(..)
  , KontaktKlasse(..)
  , KontaktContainer(..)
    -- ** Wegstrecken
  , Wegstrecke(..)
  , WegstreckeKlasse(..)
    -- * Wartezeit
  , warte
  , Wartezeit(..)
  , addition
  , differenz
  , multiplizieren
  , dividieren
  ) where

import Zug.Anbindung.Anschluss
       (Anschluss(..), AnschlussKlasse(..), AnschlussEither(..), MitInterruptPin(..)
      , InterruptPinBenötigt(..), PCF8574Klasse(..), Pin(..), PCF8574Port(..), PCF8574(..)
      , PCF8574Variant(..), anschlussWrite, Value(..), alleValues, I2CMap, i2cMapEmpty
      , MitI2CMap(..), I2CReader(..), pcf8574Gruppieren, pcf8574MultiPortWrite, beiÄnderung
      , InterruptMap, interruptMapEmpty, MitInterruptMap(..), InterruptReader(..), IntEdge(..))
import Zug.Anbindung.Bahngeschwindigkeit
       (Bahngeschwindigkeit(..), BahngeschwindigkeitKlasse(..), verwendetPwm)
import Zug.Anbindung.Container (BahngeschwindigkeitContainer(..), StreckenabschnittContainer(..)
                              , WeicheContainer(..), KupplungContainer(..), KontaktContainer(..))
import Zug.Anbindung.Klassen (StreckenObjekt(..), StreckenAtom(..))
import Zug.Anbindung.Kontakt (Kontakt(..), KontaktKlasse(..))
import Zug.Anbindung.Kupplung (Kupplung(..), KupplungKlasse(..))
import Zug.Anbindung.Pwm (pwmSetzeWert, pwmServo, erhaltePwmWertVoll, erhaltePwmWertReduziert
                        , pwmMöglich, clockMöglich, PwmValueUnmodifiziert, pwmEingabeMaximal)
import Zug.Anbindung.SoftwarePWM (PwmMap, pwmMapEmpty, MitPwmMap(..), PwmReader(..))
import Zug.Anbindung.Streckenabschnitt (Streckenabschnitt(..), StreckenabschnittKlasse(..))
import Zug.Anbindung.Wartezeit
       (warte, Wartezeit(..), addition, differenz, multiplizieren, dividieren)
import Zug.Anbindung.Wegstrecke (Wegstrecke(..), WegstreckeKlasse(..))
import Zug.Anbindung.Weiche (Weiche(..), WeicheKlasse(..))